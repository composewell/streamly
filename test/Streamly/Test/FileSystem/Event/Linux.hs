-- |
-- Module      : Streamly.Test.FileSystem.Event.Linux
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Linux (testAllEvents, testCommonEvents) where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (ord)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
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
import System.IO.Unsafe (unsafePerformIO)
import Streamly.Internal.Data.Array.Foreign (Array)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Internal.FileSystem.Event.Linux as Event

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


-- XXX Return a tuple (path, flags) instead of appending flags to path. And
-- then check the flags using an event mask.

showEventShort :: Event.Event -> String
showEventShort ev@Event.Event{..} =
    utf8ToString (removeTrailingSlash $ Event.getRelPath ev)
        ++ "_" ++ show eventFlags
        -- ++ showev Event.isDir "DirX"
        ++ showev Event.isEventsLost "Overflow"

        ++ showev Event.isRootUnwatched "RootUnwatched"
        ++ showev Event.isRootDeleted "RootDeleted"
        ++ showev Event.isRootMoved "RootMoved"
        ++ showev Event.isRootUnmounted "RootUnmounted"

        ++ showev Event.isMetadataChanged "MetadataChanged"

        ++ showev Event.isAccessed "Accessed"
        ++ showev Event.isOpened "Opened"
        ++ showev Event.isWriteClosed "WriteClosed"
        ++ showev Event.isNonWriteClosed "NonWriteClosed"

        ++ showev Event.isCreated "Created"
        ++ showev Event.isDeleted "Deleted"
        ++ showev Event.isModified "Modified"
        ++ showev Event.isMovedFrom "MovedFrom"
        ++ showev Event.isMovedTo "MovedTo"
        ++ showev Event.isDir "Dir"

    where showev f str = if f ev then "_" ++ str else ""

type EventChecker = FilePath -> MVar () -> [String] -> IO String
type EventWatch = NonEmpty (Array Word8) -> S.SerialT IO Event.Event
type ToEventList = S.SerialT IO Event.Event -> IO [Event.Event]




-------------------------------------------------------------------------------
-- Event Watcher
-------------------------------------------------------------------------------

eventListWithFixLen :: Int -> ToEventList
eventListWithFixLen count = S.toList . S.take count --51/20

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

testDesc,
 testDescRemoveRootDir,
 testDescRemoveRootDirSymLink,
 testDescMoveRootDir,
 testDescCommon,
 testDescMoveRootDirCommon,
 testDescRemoveRootDirCommon,
 testDescRemoveRootDirSymLinkCommon ::
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
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1Single_1073742080_Created_Dir"
        , "dir1Single_1073741856_Opened_Dir"
        , "dir1Single_1073741825_Accessed_Dir"
        , "dir1Single_1073741840_NonWriteClosed_Dir"
        ]
      , False
      )
    , ( "Remove a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp -> removeDirectory (fp </> "dir1Single")
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1Single_1073741856_Opened_Dir"
        , "dir1Single_1073741825_Accessed_Dir"
        , "dir1Single_1073741840_NonWriteClosed_Dir"
        , "dir1Single_1024_RootDeleted"
        , "dir1Single_32768_RootUnwatched"
        , "dir1Single_1073742336_Deleted_Dir"
        ]
      , False
      )
    , ( "Rename a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp ->
            let spath = fp </> "dir1Single"
                tpath = fp </> "dir1SingleRenamed"
            in renameDirectory spath tpath
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1Single_1073741856_Opened_Dir"
        , "dir1Single_1073741825_Accessed_Dir"
        , "dir1Single_1073741840_NonWriteClosed_Dir"
        , "dir1Single_1073741888_MovedFrom_Dir"
        , "dir1SingleRenamed_1073741952_MovedTo_Dir"
        , "dir1Single_2048_RootMoved"
        ]
      , False
      )
    , ( "Create a nested directory"
      , const (return ())
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073742080_Created_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3_1073741856_Opened_Dir"
        , "dir1/dir2/dir3_1073741825_Accessed_Dir"
        , "dir1/dir2/dir3_1073741840_NonWriteClosed_Dir"
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
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3_1073741856_Opened_Dir"
        , "dir1/dir2/dir3_1073741825_Accessed_Dir"
        , "dir1/dir2/dir3_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3_1073741888_MovedFrom_Dir"
        , "dir1/dir2/dir3Renamed_1073741952_MovedTo_Dir"
        , "dir1/dir2/dir3_2048_RootMoved"
        ]
      , False
      )
    , ( "Create a file in root Dir"
      , const (return ())
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "FileCreated.txt_256_Created"
        , "FileCreated.txt_32_Opened"
        , "FileCreated.txt_2_Modified"
        , "FileCreated.txt_8_WriteClosed"
        ]
      , False
      )
    , ( "Remove a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp -> removeFile (fp </> "FileCreated.txt")
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "FileCreated.txt_512_Deleted"
        ]
      , False
      )
    , ( "Rename a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp ->
            let spath = (fp </> "FileCreated.txt")
                tpath = (fp </> "FileRenamed.txt")
            in renamePath spath tpath
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "FileCreated.txt_64_MovedFrom"
        , "FileRenamed.txt_128_MovedTo"
        ]
      , False
      )
    , ( "Create a file in a nested Dir"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let p = fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt"
            in writeFile p "Test Data"
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3_1073741856_Opened_Dir"
        , "dir1/dir2/dir3_1073741825_Accessed_Dir"
        , "dir1/dir2/dir3_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3/FileCreated.txt_256_Created"
        , "dir1/dir2/dir3/FileCreated.txt_32_Opened"
        , "dir1/dir2/dir3/FileCreated.txt_2_Modified"
        , "dir1/dir2/dir3/FileCreated.txt_8_WriteClosed"
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
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3_1073741856_Opened_Dir"
        , "dir1/dir2/dir3_1073741825_Accessed_Dir"
        , "dir1/dir2/dir3_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3/FileCreated.txt_512_Deleted"
        ]
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
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3_1073741856_Opened_Dir"
        , "dir1/dir2/dir3_1073741825_Accessed_Dir"
        , "dir1/dir2/dir3_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3/FileCreated.txt_64_MovedFrom"
        , "dir1/dir2/dir3/FileRenamed.txt_128_MovedTo"
        ]
      , False
      )
    , ( "Remove the nested directory"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp -> removePathForcibly (fp </> "dir1")

      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2/dir3_1073741856_Opened_Dir"
        , "dir1/dir2/dir3_1073741825_Accessed_Dir"
        , "dir1/dir2/dir3_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741828_MetadataChanged_Dir"
        , "dir1/dir2_1073741828_MetadataChanged_Dir"
        , "dir1/dir2/dir3_1073741828_MetadataChanged_Dir"
        , "dir1/dir2/dir3_1024_RootDeleted"
        , "dir1/dir2/dir3_32768_RootUnwatched"
        , "dir1/dir2/dir3_1073742336_Deleted_Dir"
        , "dir1/dir2_1024_RootDeleted"
        , "dir1/dir2_32768_RootUnwatched"
        , "dir1/dir2_1073742336_Deleted_Dir"
        , "dir1_1024_RootDeleted"
        , "dir1_32768_RootUnwatched"
        , "dir1_1073742336_Deleted_Dir"
        ]
      , False
      )
    ]

testDescRemoveRootDir =
    [ ( "Remove the root directory"
    , \fp ->
          createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
    , \fp -> removePathForcibly fp
    ,   [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "_1073741828_MetadataChanged_Dir"
        , "dir1_1073741828_MetadataChanged_Dir"
        , "dir1/dir2_1073741828_MetadataChanged_Dir"
        , "dir1/dir2_1024_RootDeleted"
        , "dir1/dir2_32768_RootUnwatched"
        , "dir1/dir2_1073742336_Deleted_Dir"
        , "dir1_1024_RootDeleted"
        , "dir1_32768_RootUnwatched"
        , "dir1_1073742336_Deleted_Dir"
        , "_1024_RootDeleted"
        ]
    , False
    )
   ]

testDescMoveRootDir =
    [
     ( "Moved the root directory"
    , \fp ->
          createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
    , \fp -> renameDirectory (fp </> "dir1" </> "dir2") (fp </> "dir1" </> "dir2_Moved")
    ,   [ "_1073741856_Opened_Dir"
        ,"_1073741825_Accessed_Dir"
        ,"_1073741840_NonWriteClosed_Dir"
        ,"dir1_1073741856_Opened_Dir"
        ,"dir1_1073741825_Accessed_Dir"
        ,"dir1_1073741840_NonWriteClosed_Dir"
        ,"dir1/dir2_1073741856_Opened_Dir"
        ,"dir1/dir2_1073741825_Accessed_Dir"
        ,"dir1/dir2_1073741840_NonWriteClosed_Dir"
        ,"dir1/dir2_1073741888_MovedFrom_Dir"
        ,"dir1/dir2_Moved_1073741952_MovedTo_Dir"
        ,"dir1/dir2_2048_RootMoved"
        ]
    , False
     )
   ]

testDescRemoveRootDirSymLink =
      [ ( "Remove the root directory as SymLink"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
      , \fp -> removePathForcibly fp
      , [ "_1073741856_Opened_Dir"
        , "_1073741825_Accessed_Dir"
        , "_1073741840_NonWriteClosed_Dir"
        , "dir1_1073741856_Opened_Dir"
        , "dir1_1073741825_Accessed_Dir"
        , "dir1_1073741840_NonWriteClosed_Dir"
        , "dir1/dir2_1073741856_Opened_Dir"
        , "dir1/dir2_1073741825_Accessed_Dir"
        , "dir1/dir2_1073741840_NonWriteClosed_Dir"
        , "_1073741828_MetadataChanged_Dir"
        ]
      , True
       )
     ]

-------------------------Test Descriptor for common Event Types ---------------

testDescCommon =
    [
      ( "Create a single directory"
      , const (return ())
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , ["dir1Single_1073742080_Created_Dir"]
      , False
      )
    , ( "Remove a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp -> removeDirectory (fp </> "dir1Single")
      , [ "dir1Single_1024_RootDeleted"
        , "dir1Single_32768_RootUnwatched"
        , "dir1Single_1073742336_Deleted_Dir"
        ]
      , False
      )
    , ( "Rename a single directory"
      , \fp -> createDirectoryIfMissing True (fp </> "dir1Single")
      , \fp ->
            let spath = fp </> "dir1Single"
                tpath = fp </> "dir1SingleRenamed"
            in renameDirectory spath tpath
      , [ "dir1Single_1073741888_MovedFrom_Dir"
        , "dir1SingleRenamed_1073741952_MovedTo_Dir"
        , "dir1Single_2048_RootMoved"
        ]
      , False
      )
    , ( "Create a nested directory"
      , const (return ())
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , ["dir1_1073742080_Created_Dir"]
      , False
      )
    , ( "Rename a nested directory"
      , \fp -> createDirectoryIfMissing True
                (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let spath = fp </> "dir1" </> "dir2" </> "dir3"
                tpath = fp </> "dir1" </> "dir2" </> "dir3Renamed"
            in renameDirectory spath tpath
      , [ "dir1/dir2/dir3_1073741888_MovedFrom_Dir"
        , "dir1/dir2/dir3Renamed_1073741952_MovedTo_Dir"
        , "dir1/dir2/dir3_2048_RootMoved"
        ]
      , False
      )
    , ( "Create a file in root Dir"
      , const (return ())
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , [ "FileCreated.txt_256_Created"
        , "FileCreated.txt_2_Modified"
        ]
      , False
      )
    , ( "Remove a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp -> removeFile (fp </> "FileCreated.txt")
      , ["FileCreated.txt_512_Deleted"]
      , False
      )
    , ( "Rename a file in root Dir"
      , \fp -> writeFile (fp </> "FileCreated.txt") "Test Data"
      , \fp ->
            let spath = (fp </> "FileCreated.txt")
                tpath = (fp </> "FileRenamed.txt")
            in renamePath spath tpath
      , [ "FileCreated.txt_64_MovedFrom"
        , "FileRenamed.txt_128_MovedTo"
        ]
      , False
      )
    , ( "Create a file in a nested Dir"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp ->
            let p = fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt"
            in writeFile p "Test Data"
      , [ "dir1/dir2/dir3/FileCreated.txt_256_Created"
        , "dir1/dir2/dir3/FileCreated.txt_2_Modified"
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
      , ["dir1/dir2/dir3/FileCreated.txt_512_Deleted"]
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
      , [ "dir1/dir2/dir3/FileCreated.txt_64_MovedFrom"
        , "dir1/dir2/dir3/FileRenamed.txt_128_MovedTo"
        ]
      , False
      )
    , ( "Remove the nested directory"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
      , \fp -> removePathForcibly (fp </> "dir1")

      , [ "dir1_1073741828_MetadataChanged_Dir"
        , "dir1/dir2_1073741828_MetadataChanged_Dir"
        , "dir1/dir2/dir3_1073741828_MetadataChanged_Dir"
        , "dir1/dir2/dir3_1024_RootDeleted"
        , "dir1/dir2/dir3_32768_RootUnwatched"
        , "dir1/dir2/dir3_1073742336_Deleted_Dir"
        , "dir1/dir2_1024_RootDeleted"
        , "dir1/dir2_32768_RootUnwatched"
        , "dir1/dir2_1073742336_Deleted_Dir"
        , "dir1_1024_RootDeleted"
        , "dir1_32768_RootUnwatched"
        , "dir1_1073742336_Deleted_Dir"
        ]
      , False
      )
    ]

testDescRemoveRootDirCommon =
    [ ( "Remove the root directory"
    , \fp ->
          createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
    , \fp -> removePathForcibly fp
    ,   [ "_1073741828_MetadataChanged_Dir"
        , "dir1_1073741828_MetadataChanged_Dir"
        , "dir1/dir2_1073741828_MetadataChanged_Dir"
        , "dir1/dir2_1024_RootDeleted"
        , "dir1/dir2_32768_RootUnwatched"
        , "dir1/dir2_1073742336_Deleted_Dir"
        , "dir1_1024_RootDeleted"
        , "dir1_32768_RootUnwatched"
        , "dir1_1073742336_Deleted_Dir"
        , "_1024_RootDeleted"
        ]
    , False
    )
   ]

testDescMoveRootDirCommon =
    [
     ( "Moved the root directory"
    , \fp ->
          createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
    , \fp ->
        renameDirectory
        (fp </> "dir1" </> "dir2")
        (fp </> "dir1" </> "dir2_Moved")
    ,   [ "dir1/dir2_1073741888_MovedFrom_Dir"
        , "dir1/dir2_Moved_1073741952_MovedTo_Dir"
        , "dir1/dir2_2048_RootMoved"
        ]
    , False
     )
   ]

testDescRemoveRootDirSymLinkCommon =
      [ ( "Remove the root directory as SymLink"
      , \fp ->
            createDirectoryIfMissing True (fp </> "dir1" </> "dir2")
      , \fp -> removePathForcibly fp
      , ["_1073741828_MetadataChanged_Dir"]
      , True
       )
     ]
-------------------------------------------------------------------------------
moduleName :: String
moduleName = "FileSystem.Event.Linux"

setAllEvents :: Event.Config -> Event.Config
setAllEvents cfg = (  Event.setAccessed Event.On
                    . Event.setOpened Event.On
                    . Event.setWriteClosed Event.On
                    . Event.setNonWriteClosed Event.On
                   )cfg

allEvents :: NonEmpty (Array Word8) -> S.SerialT IO Event.Event
allEvents = Event.watchWith setAllEvents

-- Test cases for Linux platform
--
testAllEvents :: IO ()
testAllEvents = do
  -- Test cases for regular path
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask allEvents)
      testDesc

  -- Test cases for SymLink path    
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask allEvents)
      (map (\(a, b, c, d, _) -> (a, b, c, d, True)) testDesc)

  -- Test cases for moving root path    
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask allEvents)
      testDescMoveRootDir

  -- Test cases for moving root path as SymLink      
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask allEvents)
      (map (\(a, b, c, d, _) -> (a, b, c, d, True)) testDescMoveRootDir)

  -- Test cases for removing root path     
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents (eventListWithFixLen 51) allEvents)
      testDescRemoveRootDir

  -- Test cases for removing root path as Symlink     
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents (eventListWithFixLen 20) allEvents)
      testDescRemoveRootDirSymLink

-- Test cases for Events which are common across the platforms
--
testCommonEvents :: IO ()
testCommonEvents = do
  -- Test cases for regular path
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask $ Event.watchRecursive)
      testDescCommon

  -- Test cases for SymLink path    
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask Event.watchRecursive)
      (map (\(a, b, c, d, _) -> (a, b, c, d, True)) testDescCommon)

  -- Test cases for moving root path   
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask Event.watchRecursive)
      testDescMoveRootDirCommon

  -- Test cases for moving root path as SymLink    
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents eventListWithEOtask Event.watchRecursive)
      (map (\(a, b, c, d, _) -> (a, b, c, d, True)) testDescMoveRootDirCommon)

  -- Test cases for removing root path   
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents (eventListWithFixLen 12) Event.watchRecursive)
      testDescRemoveRootDirCommon

  -- Test cases for removing root path as Symlink    
    hspec
      $ describe moduleName
      $ mapM_
      (driver $ checkEvents (eventListWithFixLen 1) Event.watchRecursive)
      testDescRemoveRootDirSymLinkCommon
