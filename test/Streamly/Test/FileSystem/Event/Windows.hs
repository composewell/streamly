-- |
-- Module      : Streamly.Test.FileSystem.Event.Windows
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Windows (testCommonEvents) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.IO.Class (MonadIO)

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
import System.IO.Temp (withSystemTempDirectory)

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

import qualified Streamly.Internal.FileSystem.Event.Windows as Event


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


-- XXX Return a tuple (path, flags) instead of appending flags to path. And
-- then check the flags using an event mask.

showEventShort :: Event.Event -> String
-- | Convert an 'Event' record to a short representation for unit test.
showEventShort ev@Event.Event{..} =
    (utf8ToString $ Event.getRelPath ev) ++ "_" ++ show eventFlags

-------------------------------------------------------------------------------
-- Event Watcher
-------------------------------------------------------------------------------

type EventChecker = FilePath -> MVar () -> [String] -> IO String
type EventWatch = NonEmpty (Array Word8) -> S.SerialT IO Event.Event
type ToEventList = S.SerialT IO Event.Event -> IO [Event.Event]

eventListWithFixLen :: Int -> ToEventList
eventListWithFixLen count = S.toList . S.take count

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
                        runTest0 (fp  ++ "SymLink") sync
                  else runTest0 fp sync

    runTest0 fp sync = do
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
      , [ "dir1Single_1" ]
      , False
      )
    , ( "Remove a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp -> removeDirectory (fp </> "dir1Single")
      , [ "dir1Single_2" ]
      , False
      )
    , ( "Rename a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp ->
            let spath = fp </> "dir1Single"
                tpath = fp </> "dir1SingleRenamed"
            in renameDirectory spath tpath
      , [ "dir1Single_4"
        , "dir1SingleRenamed_5"
        ]
      , False
      )
    , ( "Create a nested directory"
      , const (return ())
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , [ "dir1_1"
        , "dir1\\dir2_1"
        , "dir1\\dir2\\dir3_1"
        ]
      , False
      )
    , ( "Rename a nested directory"
      , \fp -> createDirectoryIfMissing True
                (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let spath = fp </> "dir1" </> "dir2" </> "dir3"
                tpath = fp </> "dir1" </> "dir2" </> "dir3Renamed"
            in renameDirectory spath tpath
      , [ "dir1\\dir2_3"
        , "dir1\\dir2\\dir3_4"
        , "dir1\\dir2\\dir3Renamed_5"
        , "dir1\\dir2_3"
        ]
      , False
      )
    , ( "Create a file in root Dir"
      , const (return ())
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , [ "FileCreated.txt_1"
        , "FileCreated.txt_3"
        , "FileCreated.txt_3"
        ]
      , False
      )
    , ( "Remove a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp -> removeFile (fp </> "FileCreated.txt")
      , [ "FileCreated.txt_2" ]
      , False
      )
    , ( "Rename a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp ->
            let spath = (fp </> "FileCreated.txt")
                tpath = (fp </> "FileRenamed.txt")
            in renamePath spath tpath
      , [ "FileCreated.txt_4"
        , "FileRenamed.txt_5"
        ]
      , False
      )
    , ( "Create a file in a nested Dir"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let p = fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt"
            in writeFile p "Test Data"

      , [ "dir1\\dir2\\dir3\\FileCreated.txt_1"
        , "dir1\\dir2\\dir3\\FileCreated.txt_3"
        ]

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
      , ["dir1\\dir2\\dir3\\FileCreated.txt_2"]
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
      , [ "dir1\\dir2\\dir3_3"
        , "dir1\\dir2\\dir3\\FileCreated.txt_4"
        , "dir1\\dir2\\dir3\\FileRenamed.txt_5"
        ]
      , False
      )
    , ( "Remove the nested directory"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp -> removePathForcibly (fp </> "dir1")
      , [ "dir1_3"
        , "dir1\\dir2_3"
        , "dir1\\dir2\\dir3_2"
        , "dir1\\dir2_2","dir1_2"
        ]
      , False
      )
    ]

-- Since removel of root path as Symlink hangs for ever, testDescRootDir
-- is provided seperately.
testDescRootDir =
    [ ( "Remove the root directory"
    , \fp ->
          createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
    , \fp -> removePathForcibly fp
    , ["dir1_2", "dir1_3", "dir1\\dir2_2"]
      , False
     )
   ]

moduleName :: String
moduleName = "FileSystem.Event"

testCommonEvents :: IO ()
testCommonEvents = do  
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask Event.watchRecursive)
      testDesc
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents (eventListWithFixLen 3) Event.watchRecursive)
      testDescRootDir
    -- Run test cases for SymLink
    -- Remove the root directory as SymLink is not allowed, it hangs and Timeout.
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask Event.watchRecursive)
      (map (\(a, b, c, d, _) -> (a, b, c, d, True)) testDesc)
