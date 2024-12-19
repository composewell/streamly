-- |
-- Module      : Streamly.Test.FileSystem.Path
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Test.FileSystem.Path (main) where

import qualified System.FilePath as FilePath
import qualified Streamly.Internal.FileSystem.Path as Path

import Test.Hspec as H

moduleName :: String
moduleName = "FileSystem.Path"

testNormalize :: String -> Spec
testNormalize inp =
    it ("normalize: " ++ show inp) $ do
        p <- Path.fromString inp
        let expected = FilePath.normalise inp
            got = Path.toString (Path.normalize p)
        got `shouldBe` expected

main :: IO ()
main =
    hspec $
    H.parallel $
      describe moduleName $ do
        describe "normalize" $ do
            -- Primarily for Windows
            testNormalize "C:\\"
            testNormalize "C:"
            testNormalize "\\\\?\\c:\\"
            testNormalize "c:\\file/bob\\"
            testNormalize "c:\\"
            testNormalize "c:\\\\\\\\"
            testNormalize "C:.\\"
            testNormalize "\\\\server\\test"
            testNormalize "//server/test"
            testNormalize "c:/file"
            testNormalize "/file"
            testNormalize "\\"
            -- Primarily for Posix
            testNormalize "/./"
            testNormalize "/file/\\test////"
            testNormalize "/file/./test"
            testNormalize "/test/file/../bob/fred/"
            testNormalize "../bob/fred/"
            testNormalize "/a/../c"
            testNormalize "./bob/fred/"
            testNormalize "."
            testNormalize "./"
            testNormalize "./."
            testNormalize "/./"
            testNormalize "/"
            testNormalize "bob/fred/."
            testNormalize "//home"
