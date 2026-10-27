-- |
-- Module      : Streamly.Test.FileSystem.ReadDir
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reading directories on all platforms. The DirIO test-suite is not built on
-- Windows.

module Streamly.Test.FileSystem.ReadDir (main) where

import Control.Monad (forM_, replicateM_)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.DirIO as Dir
import qualified Streamly.FileSystem.Path as Path

moduleName :: String
moduleName = "FileSystem.ReadDir"

listDir :: FilePath -> IO [String]
listDir dir = do
    p <- Path.fromString dir
    fmap (map Path.toString) $ Stream.toList $ Dir.read p

-- On Windows, openDirStream terminated the UTF-16 search pattern passed to
-- FindFirstFileW with a single NUL byte, the second byte was uninitialized
-- memory. Reading an existing directory then failed at random with "does
-- not exist". Directory names of many lengths are read many times so that
-- the memory after the pattern varies.
readManyDirs :: IO ()
readManyDirs =
    withSystemTempDirectory "readdir-test" $ \root -> do
        let dirs = [root </> replicate n 'd' | n <- [1 .. 64]]
        forM_ dirs $ \d -> do
            createDirectory d
            writeFile (d </> "f") ""
        replicateM_ 20 $ forM_ dirs $ \d -> listDir d `shouldReturn` ["f"]

main :: IO ()
main =
    hspec $ describe moduleName $
        it "reads directories with names of many lengths" readManyDirs
