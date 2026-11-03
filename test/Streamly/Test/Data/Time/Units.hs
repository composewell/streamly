-- |
-- Module      : Streamly.Test.Data.Time.Units
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Time.Units (main) where

import Control.Exception (evaluate)
import System.Timeout (timeout)
import Test.Hspec (describe, hspec, it, shouldBe, shouldReturn)

import Streamly.Internal.Data.Time.Units (NanoSecond64(..), showNanoSecond64)

testShowNanoSecond64 :: IO ()
testShowNanoSecond64 = do
    showNanoSecond64 (NanoSecond64 999) `shouldBe` "999.0 ns"
    showNanoSecond64 (NanoSecond64 1500000) `shouldBe` "1.500 ms"
    showNanoSecond64 (NanoSecond64 (-1500000)) `shouldBe` "-1.500 ms"

-- | Negating minBound overflows, showing it must still terminate.
testShowMinBound :: IO ()
testShowMinBound = do
    let str = showNanoSecond64 minBound
    timeout 1000000 (evaluate (length str) >> return (take 1 str))
        `shouldReturn` Just "-"

main :: IO ()
main = hspec $ describe "Data.Time.Units" $ do
    it "showNanoSecond64" testShowNanoSecond64
    it "showNanoSecond64 minBound" testShowMinBound
