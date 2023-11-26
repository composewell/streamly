{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.FileSystem.Event.Common
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Test.FileSystem.Event.Common
    ( TestDesc

    -- * Running tests
    , runTests
    , WatchRootType (..)

    -- * Event predicates
    , dirEvent
    , fileEvent

    -- * Tests
    , dirCreate
    , dirCreateWithParent
    , dirDelete
    , dirMove
    , rootDirMove

    , fileCreate
    , fileCreateWithParent
    , fileDelete
    , fileModify
    , fileMove
    , rootFileMove

    , commonTests
    , commonRecTests
    )

where

import Debug.Trace (trace)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import System.Directory
    ( createDirectory
    , createDirectoryIfMissing
    , createDirectoryLink
    , removeFile
    , removePathForcibly
    , renameDirectory
    , renamePath
    )
import System.FilePath ((</>), takeDirectory)
import System.IO
    ( BufferMode(..), hSetBuffering, stdout, IOMode (WriteMode), openFile
    , hClose)
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Unicode.Stream as Unicode

#if defined(FILESYSTEM_EVENT_LINUX)
import Streamly.Internal.FileSystem.Event.Linux (Event)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(FILESYSTEM_EVENT_DARWIN)
import Streamly.Internal.FileSystem.Event.Darwin (Event)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(FILESYSTEM_EVENT_WINDOWS)
import Streamly.Internal.FileSystem.Event.Windows (Event)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
import Streamly.Internal.FileSystem.Event (Event)
import qualified Streamly.Internal.FileSystem.Event as Event
#endif

import Test.Hspec

-------------------------------------------------------------------------------
-- Check generated events
-------------------------------------------------------------------------------

type EventChecker =
       FilePath  -- watch root
    -> FilePath  -- watch target, target dir if root is a symlink
    -> MVar ()   -- mvar to sync file system ops and the watch
    -> [(String, Event -> Bool)] -- expected events
    -> IO ()
type EventWatcher = NonEmpty (Array Word8) -> Stream.Stream IO Event.Event

eventMatches :: Event -> (String, Event -> Bool) -> Bool
eventMatches ev (expectedPath, f) =
    trace ("paths: " ++ show evPath ++ " " ++ show expectedPath)
        (evPath == expectedPath && f ev)

    where

    utf8ToString :: Array Word8 -> String
    utf8ToString =
        runIdentity . Stream.toList . Unicode.decodeUtf8' . Array.read

    evPath = utf8ToString (Event.getAbsPath ev)

toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8' . Stream.fromList

checkEvents :: EventWatcher -> EventChecker
checkEvents watcher rootPath targetPath mvar matchList = do
    putStrLn ("Watching on root [" <> rootPath
             <> "] for [" <> targetPath <> "]")

    let matchList1 = fmap (first (targetPath </>)) matchList
        finder xs ev = filter (not . eventMatches ev) xs

    paths <- mapM toUtf8 [rootPath]
    watcher (NonEmpty.fromList paths)
        & Stream.before (putMVar mvar ())
        & Stream.trace (putStrLn . Event.showEvent)
        & Stream.scanl' finder matchList1
        & Stream.takeWhile (not . null)
        & Stream.drain

-------------------------------------------------------------------------------
-- Run tests
-------------------------------------------------------------------------------

type TestDesc =
    ( String            -- test description
    , FilePath -> IO () -- pre test operation, arg is path of the watch root.
    , FilePath -> IO () -- file system action, arg is path of the watch root.
    , [(String, Event -> Bool)] -- expected events (Absolute Path, predicate)
    )

-- | Whether watch root is a symlink and if so then what's the behavior
data WatchRootType =
      DirType             -- watch root is a directory
    | FileType            -- watch root is a file
    | SymLinkResolvedPath -- symlink to a directory,
                          -- Event contains path via the resolved dir
    | SymLinkOrigPath     -- symlink to a directory,
                          -- Event contains path via the original symlink
    deriving Show

driver :: EventChecker -> WatchRootType -> TestDesc -> SpecWith ()
driver checker symlinkStyle (desc, pre, ops, expected) =
    it desc $ runOneTest `shouldReturn` ()

    where

    fseventDir :: String
    fseventDir = "fsevent_dir"

    runOneTest = do
            sync <- newEmptyMVar
            withSystemTempDirectory fseventDir $ \fp -> do
                let root = fp </> "watch-root"
                target <-
                    case symlinkStyle of
                        DirType -> do
                            createDirectory root
                            return root
                        FileType -> do
                            openFile root WriteMode >>= hClose
                            return root
                        SymLinkResolvedPath -> do
                            let tgt = fp </> "watch-root-real"
                            createDirectory tgt
                            createDirectoryLink tgt root
                            return tgt
                        SymLinkOrigPath -> do
                            let tgt = fp </> "watch-root-real"
                            createDirectory tgt
                            createDirectoryLink tgt root
                            return root

                -- XXX On macOS we seem to get the watch root create events
                -- even though they occur before the watch is started.  Even if
                -- we add a delay here.
                startWatchAndCheck root target sync

    startWatchAndCheck root target sync = do
            pre root
            -- XXX On macOS the events from pre ops also seem to be bundled
            -- with the events occurred after the watch is started.
            let check = checker root target sync expected
                fsOps = Stream.fromEffect $ runFSOps root sync
            Stream.drain
                $ Stream.parListEagerFst [Stream.fromEffect check, fsOps]

    runFSOps fp sync = do
        -- We put the MVar before the event watcher starts to run but that does
        -- not ensure that the event watcher has actually started. So we need a
        -- delay as well. Do we?
        takeMVar sync >> threadDelay 200000
        ops fp
        threadDelay 10000000
        error "Time out occurred before event watcher could terminate"

-------------------------------------------------------------------------------
-- Test descriptions
-------------------------------------------------------------------------------

dirEvent, fileEvent :: (Event -> Bool) -> Event -> Bool

#if defined(FILESYSTEM_EVENT_DARWIN)
dirEvent f ev = Event.isDir ev && f ev
fileEvent f ev = Event.isFile ev && f ev
#elif defined(FILESYSTEM_EVENT_LINUX)
dirEvent f ev = Event.isDir ev && f ev
fileEvent f ev = not (Event.isDir ev) && f ev
#else
dirEvent = id
fileEvent = id
#endif

-- XXX Tests for root as a regular file instead of dir?
-- XXX Add symlink and hardlink tests
-- XXX Add exception condition tests

-------------------------------------------------------------------------------
-- Dir tests
-------------------------------------------------------------------------------

createParent :: FilePath -> FilePath -> IO ()
createParent file parent = do
    createDirectoryIfMissing True (parent </> takeDirectory file)

createDirWithParent :: FilePath -> FilePath -> IO ()
createDirWithParent dir parent =
    when (not (null dir)) $ createDirectoryIfMissing True (parent </> dir)

createDir :: FilePath -> FilePath -> IO ()
createDir dir parent =
    when (not (null dir)) $ createDirectory (parent </> dir)

-- | Create the dir along with its parent dir during the test. This is
-- especially useful to detect a race in Linux case where the watch for a
-- directory is added in event processing, so if the the child directory is
-- created before the watch was installed then we may miss some events.
dirCreateWithParent ::
    String -> (String -> [([Char], Event -> Bool)]) -> TestDesc
dirCreateWithParent dir events =
    ( "dir created (" ++ dir ++ ")"
    , const (return ())
    , createDirWithParent dir
    , events dir
    )

dirCreate :: String -> (String -> [([Char], Event -> Bool)]) -> TestDesc
dirCreate dir events =
    ( "dir created (" ++ dir ++ ")"
    , createParent dir
    , createDir dir
    , events dir
    )

dirDelete :: String -> (String -> [([Char], Event -> Bool)]) -> TestDesc
dirDelete dir events =
      ( "dir deleted (" ++ dir ++ ")"
      , createDirWithParent dir
      , \fp -> removePathForcibly (fp </> dir)
      , events dir
      )

dirMove ::
       String
    -> String
    -> (String -> String -> [(String, Event -> Bool)])
    -> TestDesc
dirMove dir1 dir2 events =
      ( "dir moved (" ++ dir1 ++ " " ++ dir2 ++ ")"
      , createDirWithParent dir1
      , \fp -> renameDirectory (fp </> dir1) (fp </> dir2)
      , events dir1 dir2
      )

rootDirMove ::
       String
    -> (String -> [(String, Event -> Bool)])
    -> TestDesc
rootDirMove suffix events =
      ( "root dir moved" ++ "(" ++ suffix ++ ")"
      , const (return ())
      , \fp -> renameDirectory fp (fp <> suffix)
      , events ""
      )

-------------------------------------------------------------------------------
-- File tests
-------------------------------------------------------------------------------

createFileWithParent :: FilePath -> FilePath -> IO ()
createFileWithParent file parent = do
    when (not (null file)) $
        createDirectoryIfMissing True (parent </> takeDirectory file)
    openFile (parent </> file) WriteMode >>= hClose

createFile :: FilePath -> FilePath -> IO ()
createFile file parent =
    openFile (parent </> file) WriteMode >>= hClose

fileCreate :: String -> (String -> [(String, Event -> Bool)]) -> TestDesc
fileCreate file1 events =
    ( "File created (" ++ file1 ++ ")"
    , createParent file1
    , createFile file1
    , (file1, fileEvent Event.isCreated) : events file1
    )

-- | See comments in dirCreateWithParent
fileCreateWithParent ::
    String -> (String -> [(String, Event -> Bool)]) -> TestDesc
fileCreateWithParent file1 events =
    ( "File created (" ++ file1 ++ ")"
    , const (return ())
    , createFileWithParent  file1
    , (file1, fileEvent Event.isCreated) : events file1
    )

fileDelete :: String -> (String -> [(String, Event -> Bool)]) -> TestDesc
fileDelete file1 events =
    ( "File deleted (" ++ file1 ++ ")"
    , createFileWithParent file1
    , \fp -> removeFile (fp </> file1)
    , events file1
    )

fileModify :: String -> (String -> [(String, Event -> Bool)]) -> TestDesc
fileModify file1 events =
    ( "File modified (" ++ file1 ++ ")"
    , createFileWithParent file1
    , \fp -> writeFile (fp </> file1) "Test Data"
    , (file1, fileEvent Event.isModified) : events file1
    )

fileMove ::
       String
    -> String
    -> (String -> String -> [(String, Event -> Bool)])
    -> TestDesc
fileMove file1 file2 events =
    ( "File moved (" ++ file1 ++ " " ++ file2 ++ ")"
    , createFileWithParent file1
    , \fp -> renamePath (fp </> file1) (fp </> file2)
    ,     (file1, fileEvent Event.isMoved)
        : (file2, fileEvent Event.isMoved)
        : events file1 file2
    )

rootFileMove :: String -> (String -> [(String, Event -> Bool)]) -> TestDesc
rootFileMove suffix events =
    ( "File moved (" ++ suffix ++ ")"
    , const (return ())
    , \fp -> renamePath fp (fp <> suffix)
    , events ""
    )

-------------------------------------------------------------------------------
-- Common test bundles
-------------------------------------------------------------------------------

withParent :: [Char] -> [Char] -> [Char]
withParent parent file = if null parent then file else parent </> file

testsWithParent :: String -> [TestDesc]
testsWithParent p =
    [ dirCreate
        (withParent p "dir") (\dir -> [(dir, dirEvent Event.isCreated)])
    , dirDelete
        (withParent p "dir") (\dir -> [(dir, dirEvent Event.isDeleted)])
    , dirMove
        (withParent p "dir1")
        (withParent p "dir2")
        (\dir1 dir2 ->
            [ (dir1, dirEvent Event.isMoved)
            , (dir2, dirEvent Event.isMoved)
            ]
        )
    , fileCreate (withParent p "file1") (const [])
    , fileDelete
        (withParent p "file1")
        (\file -> [(file, fileEvent Event.isDeleted)])
    , fileModify (withParent p "file1") (const [])
    , fileMove (withParent p "file1") (withParent p "file2") (\_ _ -> [])
    ]

commonTests :: [TestDesc]
commonTests = testsWithParent ""

commonRecTests :: [TestDesc]
commonRecTests = testsWithParent "subdir"

-------------------------------------------------------------------------------
-- Running tests
-------------------------------------------------------------------------------

runTests ::
       String
    -> String
    -> EventWatcher
    -> WatchRootType
    -> [TestDesc]
    -> IO ()
runTests modName watchType watcher rootType tests = do
    hSetBuffering stdout NoBuffering
    hspec
        $ describe modName
        $ describe watchType
        $ do
            let checker = checkEvents watcher
            describe ("Root type " ++ show rootType)
                    $ mapM_ (driver checker rootType) tests
