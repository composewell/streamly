-- |
-- Module      : Streamly.Test.FileSystem.Event
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Main (main) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.IO.Class (MonadIO)
#if !defined(CABAL_OS_WINDOWS)
import Data.Char (ord)
#endif
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Directory
    ( createDirectoryIfMissing
    , createDirectoryLink
    , removeFile
    , removeDirectory
    , removePathForcibly
    , renameDirectory
    , renamePath
    )
import System.FilePath ((</>))
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.IO.Temp (withSystemTempDirectory)
#if !defined(CABAL_OS_WINDOWS)
import System.IO.Unsafe (unsafePerformIO)
#endif
import Streamly.Internal.Data.Array.Foreign (Array)

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.FileSystem.Event as CommonEvent
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform"
#endif

import Test.Hspec

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8' . S.fromList

utf8ToString :: Array Word8 -> String
utf8ToString = runIdentity . S.toList . U.decodeUtf8' . Array.toStream

timeout :: IO String
timeout = threadDelay 5000000 >> return "Timeout"

fseventDir :: String
fseventDir = "fsevent_dir"

eoTask :: String
eoTask = "EOTask"

-- XXX Make the getRelPath type same on windows and other platforms
eventPredicate :: Event.Event -> Bool
eventPredicate ev =
    if (utf8ToString $ Event.getRelPath ev) == eoTask
    then False
    else True

-------------------------------------------------------------------------------
-- Event matching utilities
-------------------------------------------------------------------------------
#if defined(CABAL_OS_LINUX)
removeTrailingSlash :: Array Word8 -> Array Word8
removeTrailingSlash path =
    if Array.length path == 0
    then path
    else
        let mx = Array.getIndex path (Array.length path - 1)
         in case mx of
            Nothing -> error "removeTrailingSlash: Bug: Invalid index"
            Just x ->
                if x == fromIntegral (ord '/')
                -- XXX need array slicing
                then unsafePerformIO
                        $ Array.fromStreamN (Array.length path - 1)
                        $ Array.toStream path
                else path
#endif

-- XXX Return a tuple (path, flags) instead of appending flags to path. And
-- then check the flags using an event mask.

showEventShort :: Event.Event -> String
#if defined(CABAL_OS_WINDOWS)
-- | Convert an 'Event' record to a short representation for unit test.
showEventShort ev@Event.Event{..} =
    (utf8ToString $ Event.getRelPath ev) ++ "_" ++ show eventFlags
#elif defined(CABAL_OS_LINUX)
showEventShort ev@Event.Event{..} =
    utf8ToString (removeTrailingSlash $ Event.getRelPath ev)
        ++ "_" ++ show eventFlags
        ++ showev Event.isDir "Dir"

    where showev f str = if f ev then "_" ++ str else ""
#else
#error "Unsupported OS"
#endif

rootPathRemovedEventCount :: Int
#if defined(CABAL_OS_WINDOWS)
rootPathRemovedEventCount = 3
#else
rootPathRemovedEventCount = 10
eventListWithFixLenSymLink :: ToEventList
eventListWithFixLenSymLink = S.toList . S.take 1
#endif

-------------------------------------------------------------------------------
-- Event Watcher
-------------------------------------------------------------------------------

type EventChecker = FilePath -> MVar () -> [String] -> IO String
type EventWatch = NonEmpty (Array Word8) -> S.SerialT IO Event.Event
type ToEventList = S.SerialT IO Event.Event -> IO [Event.Event]

eventListWithFixLen :: ToEventList
eventListWithFixLen = S.toList . S.take rootPathRemovedEventCount

eventListWithEOtask :: ToEventList
eventListWithEOtask = S.parse (PR.takeWhile eventPredicate FL.toList)

checkEvents :: ToEventList -> EventWatch -> EventChecker
checkEvents toEL ew rootPath m matchList = do
    let args = [rootPath]
    paths <- mapM toUtf8 args
    putStrLn ("Watch started !!!! on Path " ++ rootPath)
    events <- toEL
        $ S.before (putMVar m ())
        $ ew (NonEmpty.fromList paths)
    let eventStr =  map showEventShort events
    let baseSet = Set.fromList matchList
        resultSet = Set.fromList eventStr
    if baseSet `Set.isSubsetOf` resultSet
    then
        return "PASS"
    else do
        putStrLn $ "baseSet " ++ show matchList
        putStrLn $ "resultSet " ++ show eventStr
        return "Mismatch"

-------------------------------------------------------------------------------
-- FS Event Generators
-------------------------------------------------------------------------------

checker :: S.IsStream t =>
      EventChecker -> FilePath -> MVar () -> [String] -> t IO String
checker ec rootPath synch matchList =
    S.fromEffect (ec rootPath synch matchList)
    `S.parallelFst`
    S.fromEffect timeout

-------------------------------------------------------------------------------
-- Test Drivers
-------------------------------------------------------------------------------

driver ::
       EventChecker
    -> ( String
       , FilePath -> IO ()
       , FilePath -> IO ()
       , [String]
       , Bool
       )
    -> SpecWith ()
driver ec (desc, pre, ops, events, sym) = it desc $ runTest `shouldReturn` "PASS"

    where

    runTest = do
        sync <- newEmptyMVar
        withSystemTempDirectory fseventDir $ \fp ->
            if sym
            then do
                  createDirectoryLink fp (fp  ++ "SymLink")
                  pre (fp  ++ "SymLink")
                  let eventStream = checker ec (fp  ++ "SymLink") sync events
                      fsOps = S.fromEffect $ runFSOps (fp  ++ "SymLink") sync
                  fmap fromJust $ S.head $ eventStream `S.parallelFst` fsOps
            else do
                  pre fp
                  let eventStream = checker ec fp sync events
                      fsOps = S.fromEffect $ runFSOps fp sync
                  fmap fromJust $ S.head $ eventStream `S.parallelFst` fsOps

    runFSOps fp sync = do
        _ <- takeMVar sync
        threadDelay 200000
        ops fp
        -- 'EOTask Created dir' event gets out of order so need to wait here
        threadDelay 200000 -- Why this delay?
        createDirectoryIfMissing True (fp </> eoTask)
        threadDelay 10000000
        error "fs ops timed out"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

testDesc, testDescRootDir ::
    [ ( String                       -- test description
      , FilePath -> IO ()            -- pre test operation
      , FilePath -> IO ()            -- file system actions
      , [String]                     -- expected events
      , Bool )                       -- SymLink
    ]
testDesc =
    [
      ( "Create a single directory"
      , const (return ())
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1Single_1" ]
#elif defined(CABAL_OS_LINUX)
      , ["dir1Single_1073742080_Dir"]
#endif
      , False
      )
    , ( "Remove a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp -> removeDirectory (fp </> "dir1Single")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1Single_2" ]
#elif defined(CABAL_OS_LINUX)
      , [
        "dir1Single_1073742336_Dir"
        ]
#endif
      , False
      )
    , ( "Rename a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp ->
            let spath = fp </> "dir1Single"
                tpath = fp </> "dir1SingleRenamed"
            in renameDirectory spath tpath
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1Single_4"
        , "dir1SingleRenamed_5"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1Single_1073741888_Dir"
        , "dir1SingleRenamed_1073741952_Dir"
        ]
#endif
      , False
      )
    , ( "Create a nested directory"
      , const (return ())
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1_1"
        , "dir1\\dir2_1"
        , "dir1\\dir2\\dir3_1"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1_1073742080_Dir"
        ]
#endif
      , False
      )
    , ( "Rename a nested directory"
      , \fp -> createDirectoryIfMissing True
                (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let spath = fp </> "dir1" </> "dir2" </> "dir3"
                tpath = fp </> "dir1" </> "dir2" </> "dir3Renamed"
            in renameDirectory spath tpath
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1\\dir2_3"
        , "dir1\\dir2\\dir3_4"
        , "dir1\\dir2\\dir3Renamed_5"
        , "dir1\\dir2_3"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1/dir2/dir3_1073741888_Dir"
        , "dir1/dir2/dir3Renamed_1073741952_Dir"
        ]
#endif
      , False
      )
    , ( "Create a file in root Dir"
      , const (return ())
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
#if defined(CABAL_OS_WINDOWS)
      , [ "FileCreated.txt_1"
        , "FileCreated.txt_3"
        , "FileCreated.txt_3"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "FileCreated.txt_256"
        , "FileCreated.txt_2"
        ]
#endif
      , False
      )
    , ( "Remove a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp -> removeFile (fp </> "FileCreated.txt")
#if defined(CABAL_OS_WINDOWS)
      , [ "FileCreated.txt_2" ]
#elif defined(CABAL_OS_LINUX)
      , [ "FileCreated.txt_512" ]
#endif
      , False
      )
    , ( "Rename a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp ->
            let spath = (fp </> "FileCreated.txt")
                tpath = (fp </> "FileRenamed.txt")
            in renamePath spath tpath
#if defined(CABAL_OS_WINDOWS)
      , [ "FileCreated.txt_4"
        , "FileRenamed.txt_5"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "FileCreated.txt_64"
        , "FileRenamed.txt_128"
        ]
#endif
      , False
      )
    , ( "Create a file in a nested Dir"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let p = fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt"
            in writeFile p "Test Data"
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1\\dir2\\dir3\\FileCreated.txt_1"
        , "dir1\\dir2\\dir3\\FileCreated.txt_3"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1/dir2/dir3/FileCreated.txt_256"
        , "dir1/dir2/dir3/FileCreated.txt_2"
        ]
#endif
      , False
      )
    , ( "Remove a file in a nested Dir"
      , \fp ->
            let nestedDir = fp </> "dir1" </> "dir2" </> "dir3"
                fpath = nestedDir </> "FileCreated.txt"
            in do
                createDirectoryIfMissing True nestedDir
                writeFile fpath "Test Data"
      , \fp ->
            let p = fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt"
            in removeFile p
#if defined(CABAL_OS_WINDOWS)
      , ["dir1\\dir2\\dir3\\FileCreated.txt_2"]
#elif defined(CABAL_OS_LINUX)
      , ["dir1/dir2/dir3/FileCreated.txt_512"]
#endif
      , False
      )
    , ( "Rename a file in a nested Dir"
      , \fp ->
            let nestedDir = fp </> "dir1" </> "dir2" </> "dir3"
                fpath = nestedDir </> "FileCreated.txt"
            in do
                createDirectoryIfMissing True nestedDir
                writeFile fpath "Test Data"
      , \fp ->
            let s = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt")
                t = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileRenamed.txt")
            in renamePath s t
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1\\dir2\\dir3_3"
        , "dir1\\dir2\\dir3\\FileCreated.txt_4"
        , "dir1\\dir2\\dir3\\FileRenamed.txt_5"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1/dir2/dir3/FileCreated.txt_64"
        , "dir1/dir2/dir3/FileRenamed.txt_128"
        ]
#endif
      , False
      )
    , ( "Remove the nested directory"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp -> removePathForcibly (fp </> "dir1")
#if defined(CABAL_OS_WINDOWS)
      , [ "dir1_3"
        , "dir1\\dir2_3"
        , "dir1\\dir2\\dir3_2"
        , "dir1\\dir2_2","dir1_2"
        ]
#elif defined(CABAL_OS_LINUX)
      , [ "dir1_1073741828_Dir"
        , "dir1_1073741828_Dir"
        , "dir1/dir2_1073741828_Dir"
        , "dir1/dir2_1073741828_Dir"
        , "dir1/dir2/dir3_1073741828_Dir"
        , "dir1/dir2/dir3_1073741828_Dir"
        , "dir1/dir2/dir3_1024"
        , "dir1/dir2/dir3_1073742336_Dir"
        , "dir1/dir2_1024"
        , "dir1/dir2_1073742336_Dir"
        , "dir1_1024"
        , "dir1_1073742336_Dir"
        ]
#endif
      , False
      )
    ]

testDescRootDir =
    [ ( "Remove the root directory"
    , \fp ->
          createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
    , \fp -> removePathForcibly fp
#if defined(CABAL_OS_WINDOWS)
    , ["dir1_2", "dir1_3", "dir1\\dir2_2"]
#elif defined(CABAL_OS_LINUX)
    , [ "_1073741828_Dir"
      , "dir1_1073741828_Dir"
      , "dir1_1073741828_Dir"
      , "dir1/dir2_1073741828_Dir"
      , "dir1/dir2_1073741828_Dir"
      , "dir1/dir2_1024"
      , "dir1/dir2_1073742336_Dir"
      , "dir1_1024"
      , "dir1_1073742336_Dir"
      , "_1024"
    ]
#endif
      , False
     )
   ]

#if defined(CABAL_OS_LINUX)
testDescRootDirSymLink ::
    [ ( String                       -- test description
      , FilePath -> IO ()            -- pre test operation
      , FilePath -> IO ()            -- file system actions
      , [String]                     -- expected events
      , Bool )                       -- SymLink
    ]
testDescRootDirSymLink =
      [ ( "Remove the root directory as SymLink"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
      , \fp -> removePathForcibly fp
      , ["_1073741828_Dir"]
      , True
       )
     ]
#endif

moduleName :: String
moduleName = "FileSystem.Event"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
#if defined(CABAL_OS_LINUX)
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask Event.watchRecursive)
      testDesc
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask CommonEvent.watchRecursive)
      (map (\(a, b, c, d, _) -> (a, b, c, d, True)) testDesc)
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithFixLenSymLink CommonEvent.watchRecursive)
      testDescRootDirSymLink
#endif
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask CommonEvent.watchRecursive)
      testDesc
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithFixLen CommonEvent.watchRecursive)
      testDescRootDir
