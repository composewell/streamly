-- |
-- Module      : Streamly.Test.Syscall.Posix.File
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Syscall.Posix.File (main) where

import Control.Exception (bracket, bracket_)
import Data.Bits ((.&.), (.|.), complement)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (allocaBytes)
import Numeric (showOct)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Internals
    (c_stat, c_umask, o_WRONLY, sizeof_stat, st_mode)
import System.Posix.Types (CMode)
import Test.Hspec (describe, hspec, it, shouldBe)

import qualified Streamly.Internal.FileSystem.PosixPath as Path
import qualified Streamly.Internal.Syscall.Posix.File as File

-- | Permission bits of a file.
fileMode :: FilePath -> IO CMode
fileMode file =
    allocaBytes sizeof_stat $ \p ->
        withCString file $ \cstr -> do
            _ <- c_stat cstr p
            (.&. 0o7777) <$> st_mode p

-- | Create a file with the given create mode and return the permission bits
-- of the created file. The umask is cleared for the duration.
createWithMode :: File.FileMode -> IO CMode
createWithMode mode =
    withSystemTempDirectory "streamly-test" $ \dir -> do
        let file = dir </> "file"
        path <- Path.fromString file
        bracket_ (c_umask 0) (c_umask 0o022) $
            bracket
                (File.openAt
                    Nothing path (File.OpenFlags o_WRONLY) (Just mode))
                File.close
                (const (return ()))
        fileMode file

type ModeFn = File.FileMode -> File.FileMode

modeBits :: [(String, ModeFn, ModeFn, CMode)]
modeBits =
    [ ("Ur", File.setUr, File.clrUr, 0o400)
    , ("Uw", File.setUw, File.clrUw, 0o200)
    , ("Ux", File.setUx, File.clrUx, 0o100)
    , ("Gr", File.setGr, File.clrGr, 0o040)
    , ("Gw", File.setGw, File.clrGw, 0o020)
    , ("Gx", File.setGx, File.clrGx, 0o010)
    , ("Or", File.setOr, File.clrOr, 0o004)
    , ("Ow", File.setOw, File.clrOw, 0o002)
    , ("Ox", File.setOx, File.clrOx, 0o001)
    , ("Sticky", File.setSticky, File.clrSticky, 0o1000)
    ]

showMode :: CMode -> String
showMode m = showOct m ""

main :: IO ()
main = hspec $ describe "Syscall.Posix.File" $ do
    it "defaultCreateMode" $ do
        m <- createWithMode File.defaultCreateMode
        showMode m `shouldBe` showMode 0o666
    mapM_ testBit modeBits

    where

    testBit (name, set, clr, bit) = do
        it ("set" ++ name) $ do
            m <- createWithMode (set File.defaultCreateMode)
            showMode m `shouldBe` showMode (0o666 .|. bit)
        it ("clr" ++ name) $ do
            m <- createWithMode (clr File.defaultCreateMode)
            showMode m `shouldBe` showMode (0o666 .&. complement bit)
