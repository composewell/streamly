-- |
-- Module      : Streamly.Test.FileSystem.PathIO
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Tests for "Streamly.Internal.FileSystem.PathIO".

module Streamly.Test.FileSystem.PathIO (main) where

import Control.Exception (bracket, try)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec as H

import qualified Streamly.Internal.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.PathIO as PathIO
import qualified System.Directory as Directory

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Run an action and restore the working directory afterwards.
preservingCwd :: IO a -> IO a
preservingCwd act =
    bracket
        Directory.getCurrentDirectory
        Directory.setCurrentDirectory
        (const act)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

testGetCurrentDirectory :: Expectation
testGetCurrentDirectory = do
    cwd <- Path.toString <$> PathIO.getCurrentDirectory
    Directory.getCurrentDirectory `shouldReturn` cwd

testSetCurrentDirectory :: Expectation
testSetCurrentDirectory =
    withSystemTempDirectory "pathio" $ \dir -> preservingCwd $ do
        PathIO.setCurrentDirectory (Path.fromString_ dir)
        -- The temporary directory may be reached through a symlink, the
        -- working directory is its resolved form.
        expected <- Directory.canonicalizePath dir
        cwd <- Path.toString <$> PathIO.getCurrentDirectory
        cwd `shouldBe` expected
        Directory.getCurrentDirectory `shouldReturn` expected

testSetCurrentDirectoryMissing :: Expectation
testSetCurrentDirectoryMissing =
    withSystemTempDirectory "pathio" $ \dir -> preservingCwd $ do
        let missing = dir </> "missing"
        r <- try $ PathIO.setCurrentDirectory (Path.fromString_ missing)
        case r of
            Left e -> e `shouldSatisfy` isDoesNotExistError
            Right () -> expectationFailure "setCurrentDirectory succeeded"

testMakeAbsoluteRelative :: Expectation
testMakeAbsoluteRelative = do
    cwd <- Directory.getCurrentDirectory
    p <- PathIO.makeAbsolute (Path.fromString_ "a/b")
    Path.toString p `shouldBe` cwd </> "a/b"

testMakeAbsoluteAbsolute :: Expectation
testMakeAbsoluteAbsolute = do
    p <- PathIO.makeAbsolute (Path.fromString_ "/a/b")
    Path.toString p `shouldBe` "/a/b"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.PathIO"

-- The tests change the working directory of the process, so they must not
-- run in parallel.
main :: IO ()
main = hspec $ describe moduleName $ do
    it "getCurrentDirectory" testGetCurrentDirectory
    it "setCurrentDirectory" testSetCurrentDirectory
    it "setCurrentDirectory missing" testSetCurrentDirectoryMissing
    it "makeAbsolute relative" testMakeAbsoluteRelative
    it "makeAbsolute absolute" testMakeAbsoluteAbsolute
