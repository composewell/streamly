-- |
-- Module      : Streamly.Test.Data.CString
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.CString (main) where

import Control.Monad (forM, forM_)
import Data.Word (Word8)
import Foreign.Marshal.Array (withArray)
import Test.Hspec (describe, hspec, it, shouldBe)

import qualified Streamly.Internal.Data.CString as CString
import qualified Streamly.Internal.Data.MutByteArray as MBA

-- | A MutByteArray of the given size holding the CString "a", the bytes after
-- the terminating null byte are 0xff.
newCString :: Int -> IO MBA.MutByteArray
newCString size = do
    arr <- MBA.new size
    forM_ [0 .. size - 1] $ \i -> MBA.pokeAt i arr (0xff :: Word8)
    MBA.pokeAt 0 arr (0x61 :: Word8)
    MBA.pokeAt 1 arr (0 :: Word8)
    return arr

peekBytes :: MBA.MutByteArray -> Int -> IO [Word8]
peekBytes arr n = forM [0 .. n - 1] $ \i -> MBA.peekAt i arr

-- | splicePtrN appends exactly n bytes and terminates the result with a null
-- byte, the byte after the n bytes at the source is not copied.
testSplicePtrN :: IO ()
testSplicePtrN = do
    dst <- newCString 8
    withArray [0x62, 0x63, 0x64 :: Word8] $ \src -> do
        n <- CString.splicePtrN dst src 2
        n `shouldBe` 3
        bytes <- peekBytes dst 5
        bytes `shouldBe` [0x61, 0x62, 0x63, 0, 0xff]
        len <- CString.length dst
        len `shouldBe` 3

testSpliceCString :: IO ()
testSpliceCString = do
    dst <- newCString 8
    src <- MBA.new' 3
    forM_ (zip [0 ..] [0x62, 0x63, 0 :: Word8]) $ \(i, x) -> MBA.pokeAt i src x
    MBA.unsafeAsPtr src $ \p -> do
        n <- CString.spliceCString dst p
        n `shouldBe` 3
    bytes <- peekBytes dst 5
    bytes `shouldBe` [0x61, 0x62, 0x63, 0, 0xff]

main :: IO ()
main = hspec $ describe "Data.CString" $ do
    it "splicePtrN" testSplicePtrN
    it "spliceCString" testSpliceCString
