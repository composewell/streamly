-- |
-- Module      : Streamly.Test.Data.Ring.Unboxed
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Ring.Unboxed (main) where

import Control.Monad (void)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Ring as Ring

import Prelude as P
import qualified Data.Foldable as P

import Test.Hspec as H

unsafeEqArrayN :: [Int] -> [Int] -> Int -> Int -> Bool -> IO ()
unsafeEqArrayN lstArr lstRing startR nelem expected = do
    let arr = Array.fromList lstArr
    (ring, rh) <- Ring.new (length lstRing)
    void $ P.foldlM (Ring.unsafeInsert ring) rh lstRing
    Ring.unsafeEqArrayN ring (Ring.moveBy startR ring rh) arr nelem
        `shouldBe` expected

unsafeEqArray :: [Int] -> [Int] -> Int -> Bool -> IO ()
unsafeEqArray lstArr lstRing startR expected = do
    let arr = Array.fromList lstArr
    (ring, rh) <- Ring.new (length lstRing)
    void $ P.foldlM (Ring.unsafeInsert ring) rh lstRing
    Ring.unsafeEqArray ring (Ring.moveBy startR ring rh) arr
        `shouldBe` expected

moduleName :: String
moduleName = "Data.Ring.Unboxed"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        describe "Eq" $ do
            let lstArr = [0..99]
                lstRing = [50..99] ++ [0..49]
            it "unsafeEqArrayN True (n < len)"
                   $ unsafeEqArrayN lstArr lstRing 50 75 True
            it "unsafeEqArrayN True (n > len)"
                   $ unsafeEqArrayN lstArr lstRing 50 200 True
            it "unsafeEqArrayN False"
                   $ unsafeEqArrayN lstArr lstRing 10 75 False
            it "unsafeEqArray True" $ unsafeEqArray lstArr lstRing 50 True
            it "unsafeEqArray False" $ unsafeEqArray lstArr lstRing 20 False
