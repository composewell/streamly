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
import Data.Maybe (fromJust)
import Data.Word (Word8)
import System.Directory
    ( createDirectoryIfMissing
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
import Streamly.Internal.Data.Array.Storable.Foreign (Array)

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Data.Array.Storable.Foreign as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as S
#if defined(CABAL_OS_DARWIN)
import qualified Streamly.Internal.FileSystem.Event.Darwin as Event
#elif defined(CABAL_OS_LINUX)
import qualified Streamly.Internal.FileSystem.Event.Linux as Event
#elif defined(CABAL_OS_WINDOWS)
import qualified Streamly.Internal.FileSystem.Event.Windows as Event
#else
#error "FS Events not supported on this platform
#endif

#if !defined(CABAL_OS_WINDOWS)
import Data.Functor.Identity (runIdentity)
import qualified Streamly.Internal.Unicode.Stream as U
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
toUtf8 :: MonadIO m => String -> m (Array Word8)
toUtf8 = Array.fromStream . Unicode.encodeUtf8' . S.fromList

#if !defined(CABAL_OS_WINDOWS)
utf8ToString :: Array Word8 -> String
utf8ToString = runIdentity . S.toList . U.decodeUtf8' . Array.toStream
#endif

timeout :: IO String
timeout = threadDelay 5000000 >> return "Timeout"

fseventDir :: String
fseventDir = "fsevent_dir"

-- XXX Make the getRelPath type same on windows and other platforms
eventPredicate :: Event.Event -> Bool
eventPredicate ev =
#if defined(CABAL_OS_WINDOWS)
    if (Event.getRelPath ev) == "EOTask"
#else
    if (utf8ToString $ Event.getRelPath ev) == "EOTask"
#endif
    then False
    else True

-------------------------------------------------------------------------------
-- Event lists to be matched with
-------------------------------------------------------------------------------

-- XXX Use a tuple (path, flags) instead of a string with flags

#if defined(CABAL_OS_WINDOWS)

singleDirCreateEvents :: [String]
singleDirCreateEvents =
    [ "dir1Single_1" ]

singleDirRemoveEvents :: [String]
singleDirRemoveEvents =
    [ "dir1Single_2" ]

singleDirRenameEvents :: [String]
singleDirRenameEvents =
    [ "dir1Single_4"
    , "dir1SingleRenamed_5"
    ]

nestedDirCreateEvents :: [String]
nestedDirCreateEvents =
    [ "dir1_1"
    , "dir1\\dir2_1"
    , "dir1\\dir2\\dir3_1"
    ]

nestedDirRemoveEvents :: [String]
nestedDirRemoveEvents =
    [ "dir1_3"
    , "dir1\\dir2_3"
    , "dir1\\dir2\\dir3_2"
    , "dir1\\dir2_2","dir1_2"
    ]

nestedDirRenameEvents :: [String]
nestedDirRenameEvents =
    [ "dir1\\dir2_3"
    , "dir1\\dir2\\dir3_4"
    , "dir1\\dir2\\dir3Renamed_5"
    , "dir1\\dir2_3"
    ]

createFileRootDirEvents :: [String]
createFileRootDirEvents =
    [ "FileCreated.txt_1"
    , "FileCreated.txt_3"
    , "FileCreated.txt_3"
    ]

removeFileRootDirEvents :: [String]
removeFileRootDirEvents =
    [ "FileCreated.txt_2" ]

renameFileRootDirEvents :: [String]
renameFileRootDirEvents =
    [ "FileCreated.txt_4"
    , "FileRenamed.txt_5"
    ]

createFileNestedDirEvents :: [String]
createFileNestedDirEvents =
    [ "dir1\\dir2\\dir3\\FileCreated.txt_1"
    , "dir1\\dir2\\dir3\\FileCreated.txt_3"
    ]

removeFileNestedDirEvents :: [String]
removeFileNestedDirEvents =
    ["dir1\\dir2\\dir3\\FileCreated.txt_2"]

renameFileNestedDirEvents :: [String]
renameFileNestedDirEvents =
    [ "dir1\\dir2\\dir3_3"
    , "dir1\\dir2\\dir3\\FileCreated.txt_4"
    , "dir1\\dir2\\dir3\\FileRenamed.txt_5"
    ]

-- | Convert an 'Event' record to a short representation for unit test.
showEventShort :: Event.Event -> String
showEventShort ev@Event.Event{..} = Event.getRelPath ev ++ "_" ++ show eventFlags

#else

singleDirCreateEvents :: [String]
singleDirCreateEvents =
    [ "dir1Single_1073742080_Dir"
    , "dir1Single_1073741856_Dir"
    , "dir1Single_1073741825_Dir"
    , "dir1Single_1073741840_Dir"
    ]

singleDirRemoveEvents :: [String]
singleDirRemoveEvents =
    [ "dir1Single_1024"
    , "dir1Single_32768"
    ]

singleDirRenameEvents :: [String]
singleDirRenameEvents =
    [ "dir1Single_1073741888_Dir"
    , "dir1SingleRenamed_1073741952_Dir"
    ]

nestedDirCreateEvents :: [String]
nestedDirCreateEvents =
    [ "dir1_1073742080_Dir"
    , "dir1_1073741856_Dir"
    , "dir1_1073741825_Dir"
    , "dir1_1073741840_Dir"
    ]

nestedDirRemoveEvents :: [String]
nestedDirRemoveEvents =
    [ "dir1/dir2/dir3_1073742336_Dir"
    , "dir1/dir2_1073742336_Dir"
    , "dir1_1073742336_Dir"
    ]

nestedDirRenameEvents :: [String]
nestedDirRenameEvents =
    [ "dir1/dir2/dir3_1073741888_Dir"
    , "dir1/dir2/dir3Renamed_1073741952_Dir"
    ]

createFileRootDirEvents :: [String]
createFileRootDirEvents =
    [ "FileCreated.txt_256"
    , "FileCreated.txt_32"
    , "FileCreated.txt_2"
    ]

removeFileRootDirEvents :: [String]
removeFileRootDirEvents =
    ["FileCreated.txt_512"]

renameFileRootDirEvents :: [String]
renameFileRootDirEvents =
    [ "FileCreated.txt_64"
    , "FileRenamed.txt_128"
    ]

createFileNestedDirEvents :: [String]
createFileNestedDirEvents =
    [ "dir1/dir2/dir3/FileCreated.txt_256"
    , "dir1/dir2/dir3/FileCreated.txt_32"
    , "dir1/dir2/dir3/FileCreated.txt_2"
    , "dir1/dir2/dir3/FileCreated.txt_8"
    ]

removeFileNestedDirEvents :: [String]
removeFileNestedDirEvents =
    ["dir1/dir2/dir3/FileCreated.txt_512"]

renameFileNestedDirEvents :: [String]
renameFileNestedDirEvents =
    [ "dir1/dir2/dir3/FileCreated.txt_64"
    , "dir1/dir2/dir3/FileRenamed.txt_128"
    ]

removeTrailingSlash :: Array Word8 -> Array Word8
removeTrailingSlash path =
    if Array.length path == 0
    then path
    else
        let mx = Array.readIndex path (Array.length path - 1)
         in case mx of
            Nothing -> error "removeTrailingSlash: Bug: Invalid index"
            Just x ->
                if x == fromIntegral (ord '/')
                -- XXX need array slicing
                then unsafePerformIO
                        $ Array.fromStreamN (Array.length path - 1)
                        $ Array.toStream path
                else path

showEventShort :: Event.Event -> String
showEventShort ev@Event.Event{..} =
    (utf8ToString $ removeTrailingSlash $ Event.getRelPath ev)
        ++ "_" ++ show eventFlags
        ++ showev Event.isDir "Dir"

    where showev f str = if f ev then "_" ++ str else ""

#endif

-------------------------------------------------------------------------------
-- Event Watcher
-------------------------------------------------------------------------------

checkEvents :: FilePath -> MVar () -> [String] -> IO String
checkEvents rootPath m matchList = do
    let args = [rootPath]
    paths <- mapM toUtf8 args
    putStrLn ("Watch started !!!! on Path " ++ rootPath)
    events <- S.parse (PR.takeWhile eventPredicate FL.toList)
        $ S.before (putMVar m ())
        $ Event.watchTrees (NonEmpty.fromList paths)
    let eventStr =  map showEventShort events
    let baseSet = Set.fromList matchList
        resultSet = Set.fromList eventStr
    if (baseSet `Set.isSubsetOf` resultSet)
    then
        return "PASS"
    else do
        putStrLn $ "baseSet " ++ show matchList
        putStrLn $ "resultSet " ++ show eventStr
        return "Mismatch"

-------------------------------------------------------------------------------
-- FS Event Generators
-------------------------------------------------------------------------------

-- XXX Factor out common code from all these functions. The specific operation
-- can be passed to a common function.

fsOpsCreateSingleDir :: FilePath -> MVar () -> IO ()
fsOpsCreateSingleDir fp m = do
    takeMVar m
    putStrLn ("Create Single Directory !!!!!!! on " ++ fp)
    threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "dir1Single")
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRemoveSingleDir :: FilePath -> MVar () -> IO ()
fsOpsRemoveSingleDir fp m = do
    takeMVar m
    putStrLn ("Remove Single Directory !!!!!!! on " ++ fp)
    threadDelay 200000
        >> removeDirectory (fp </> "dir1Single")
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRenameSingleDir :: FilePath -> MVar () -> IO ()
fsOpsRenameSingleDir fp m = do
    takeMVar m
    putStrLn ("Rename a Single Directory !!!!!!! on " ++ fp)
    threadDelay 200000
        >> renameDirectory (fp </> "dir1Single") (fp </> "dir1SingleRenamed")
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsCreateNestedDir :: FilePath -> MVar () -> IO ()
fsOpsCreateNestedDir fp m = do
    takeMVar m
    putStrLn ("Create Nested Directory !!!!!!!!!!!!! on " ++ fp)
    threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRemoveNestedDir :: FilePath -> MVar () -> IO ()
fsOpsRemoveNestedDir fp m = do
    takeMVar m
    putStrLn ("Remove Nested Directory !!!!!!!!!!!!! on " ++ fp)
    threadDelay 200000
        >> removePathForcibly (fp </> "dir1")
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRenameNestedDir :: FilePath -> MVar () -> IO ()
fsOpsRenameNestedDir fp m = do
    takeMVar m
    putStrLn ("Rename Nested Directory !!!!!!!!!!!!! on " ++ fp)
    threadDelay 200000
        >> renameDirectory
            (fp </> "dir1" </> "dir2" </> "dir3")
            (fp </> "dir1" </> "dir2" </> "dir3Renamed")
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsCreateFileInRootDir :: FilePath -> MVar () -> IO ()
fsOpsCreateFileInRootDir fp m = do
    takeMVar m
    let tpath = (fp </> "FileCreated.txt")
    putStrLn ("create a File  on " ++ fp)
    threadDelay 200000
        >> writeFile tpath "Test Data"
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRemoveFileInRootDir :: FilePath -> MVar () -> IO ()
fsOpsRemoveFileInRootDir fp m = do
    takeMVar m
    let tpath = (fp </> "FileCreated.txt")
    putStrLn ("Remove a File  on " ++ fp)
    threadDelay 200000
        >> removeFile tpath
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRenameFileInRootDir :: FilePath -> MVar () -> IO ()
fsOpsRenameFileInRootDir fp m = do
    takeMVar m
    let spath = (fp </> "FileCreated.txt")
        tpath = (fp </> "FileRenamed.txt")
    putStrLn ("Rename a File  on " ++ fp)
    threadDelay 200000
        >> renamePath spath tpath
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsCreateFileInNestedDir :: FilePath -> MVar () -> IO ()
fsOpsCreateFileInNestedDir fp m = do
    takeMVar m
    let tpath = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt")
    putStrLn ("create a File  on nested dir " ++ fp)
    threadDelay 200000
        >> writeFile tpath "Test Data"
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRemoveFileInNestedDir :: FilePath -> MVar () -> IO ()
fsOpsRemoveFileInNestedDir fp m = do
    takeMVar m
    let tpath = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt")
    putStrLn ("Remove a File  on nested dir " ++ fp)
    threadDelay 200000
        >> threadDelay 200000 >> removeFile tpath
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

fsOpsRenameFileInNestedDir :: FilePath -> MVar () -> IO ()
fsOpsRenameFileInNestedDir fp m = do
    takeMVar m
    let spath = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt")
        tpath = (fp </> "dir1" </> "dir2" </> "dir3" </> "FileRenamed.txt")
    putStrLn ("Rename a File  on nested dir " ++ fp)
    threadDelay 200000
        >> renamePath spath tpath
        >> threadDelay 200000
        >> createDirectoryIfMissing True (fp </> "EOTask")

checker :: S.IsStream t =>
                FilePath -> MVar () -> [String] -> t IO String
checker rootPath synch matchList =
    S.yieldM (checkEvents rootPath synch matchList)
    `S.parallelFst`
    S.yieldM timeout

driverInit :: IO (MVar ())
driverInit = do
    hSetBuffering stdout NoBuffering
    pre <- newEmptyMVar
    return pre

-------------------------------------------------------------------------------
-- Test Drivers
-------------------------------------------------------------------------------

-- XXX Factor out common code from all these. Pass a specific fsops function to
-- a common functions.

driverCreateSingleDir :: IO String
driverCreateSingleDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        res <- S.head
            $ (checker fp sync singleDirCreateEvents)
            `S.parallelFst`
            S.yieldM  -- ^ this message should follow checker
            (fsOpsCreateSingleDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRemoveSingleDir :: IO String
driverRemoveSingleDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        createDirectoryIfMissing True (fp </> "dir1Single")
        res <- S.head
            $ (checker fp sync singleDirRemoveEvents)
            `S.parallelFst`
            S.yieldM  -- ^ this message should follow checker
            (fsOpsRemoveSingleDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRenameSingleDir :: IO String
driverRenameSingleDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        createDirectoryIfMissing True (fp </> "dir1Single")
        res <- S.head
            $ (checker fp sync singleDirRenameEvents)
            `S.parallelFst`
            S.yieldM  -- ^ this message should follow checker
            (fsOpsRenameSingleDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverCreateNestedDir :: IO String
driverCreateNestedDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        res <- S.head
            $ (checker fp sync nestedDirCreateEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsCreateNestedDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRemoveNestedDir :: IO String
driverRemoveNestedDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
        res <- S.head
            $ (checker fp sync nestedDirRemoveEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsRemoveNestedDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRenameNestedDir :: IO String
driverRenameNestedDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
        res <- S.head
            $ (checker fp sync nestedDirRenameEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsRenameNestedDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverCreateFileInRootDir :: IO String
driverCreateFileInRootDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        res <- S.head
            $ (checker fp sync createFileRootDirEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsCreateFileInRootDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRemoveFileInRootDir :: IO String
driverRemoveFileInRootDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        writeFile (fp </> "FileCreated.txt") "Test Data"
        res <- S.head
            $ (checker fp sync removeFileRootDirEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsRemoveFileInRootDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRenameFileInRootDir :: IO String
driverRenameFileInRootDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        writeFile (fp </> "FileCreated.txt") "Test Data"
        res <- S.head
            $ (checker fp sync renameFileRootDirEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsRenameFileInRootDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverCreateFileInNestedDir :: IO String
driverCreateFileInNestedDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
        res <- S.head
            $ (checker fp sync createFileNestedDirEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsCreateFileInNestedDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRemoveFileInNestedDir :: IO String
driverRemoveFileInNestedDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
            >> writeFile (fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt") "Test Data"
        res <- S.head
            $ (checker fp sync removeFileNestedDirEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsRemoveFileInNestedDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

driverRenameFileInNestedDir :: IO String
driverRenameFileInNestedDir = do
    sync <- driverInit
    withSystemTempDirectory fseventDir $ \fp -> do
        createDirectoryIfMissing True (fp </> "dir1" </> "dir2" </> "dir3")
            >> writeFile (fp </> "dir1" </> "dir2" </> "dir3" </> "FileCreated.txt") "Test Data"
        res <- S.head
            $ (checker fp sync renameFileNestedDirEvents)
            `S.parallelFst`
            S.yieldM
            (fsOpsRenameFileInNestedDir fp sync
                >> threadDelay 10000000
                >> return "fOps Done")
        return $ fromJust res

-------------------------------------------------------------------------------
-- Test Cases
-------------------------------------------------------------------------------

-- XXX These can either be directly inlined instead of creating a wrapper
-- function or the wrapper could be common passing the function being checked.

testCreateSingleDir :: Expectation
testCreateSingleDir = driverCreateSingleDir `shouldReturn` "PASS"

testRemoveSingleDir :: Expectation
testRemoveSingleDir = driverRemoveSingleDir `shouldReturn` "PASS"

testRenameSingleDir :: Expectation
testRenameSingleDir = driverRenameSingleDir `shouldReturn` "PASS"

testCreateNestedDir :: Expectation
testCreateNestedDir = driverCreateNestedDir `shouldReturn` "PASS"

testRemoveNestedDir :: Expectation
testRemoveNestedDir = driverRemoveNestedDir `shouldReturn` "PASS"

testRenameNestedDir :: Expectation
testRenameNestedDir = driverRenameNestedDir `shouldReturn` "PASS"

testCreateFileInRootDir :: Expectation
testCreateFileInRootDir = driverCreateFileInRootDir `shouldReturn` "PASS"

testRemoveFileInRootDir :: Expectation
testRemoveFileInRootDir = driverRemoveFileInRootDir `shouldReturn` "PASS"

testRenameFileInRootDir :: Expectation
testRenameFileInRootDir = driverRenameFileInRootDir `shouldReturn` "PASS"

testCreateFileInNestedDir :: Expectation
testCreateFileInNestedDir = driverCreateFileInNestedDir `shouldReturn` "PASS"

testRemoveFileInNestedDir :: Expectation
testRemoveFileInNestedDir = driverRemoveFileInNestedDir `shouldReturn` "PASS"

testRenameFileInNestedDir :: Expectation
testRenameFileInNestedDir = driverRenameFileInNestedDir `shouldReturn` "PASS"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    prop "Create a single directory" testCreateSingleDir
    prop "Remove a single directory" testRemoveSingleDir
    prop "Rename a single directory" testRenameSingleDir
    prop "Create a nested directory" testCreateNestedDir
    prop "Remove a nested directory" testRemoveNestedDir
    prop "Rename a nested directory" testRenameNestedDir
    prop "Create a file in root Dir" testCreateFileInRootDir
    prop "Remove a file in root Dir" testRemoveFileInRootDir
    prop "Rename a file in root Dir" testRenameFileInRootDir
    prop "Create a file in a nested Dir" testCreateFileInNestedDir
    prop "Remove a file in a nested Dir" testRemoveFileInNestedDir
    prop "Rename a file in a nested Dir" testRenameFileInNestedDir
