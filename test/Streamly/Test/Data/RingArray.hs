-- |
-- Module      : Streamly.Test.Data.RingArray
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.RingArray (main) where

import Streamly.Test.Common (performGCSweep)

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.RingArray as RingArray

import Prelude as P
import Test.Hspec as H

eqArrayN :: [Int] -> [Int] -> Int -> Int -> Bool -> IO ()
eqArrayN lstArr lstRing startR nBytes expected = do
    let arr = Array.fromList lstArr
    marr <- MutArray.fromList lstRing
    let ring =
            maybe (error "cast failed") id $ RingArray.castMutArrayWith startR marr
    performGCSweep 4 100000
    res <- RingArray.eqArrayN ring arr nBytes
    res `shouldBe` expected

eqArray :: [Int] -> [Int] -> Int -> Bool -> IO ()
eqArray lstArr lstRing startR expected = do
    let arr = Array.fromList lstArr
    marr <- MutArray.fromList lstRing
    let ring =
            maybe (error "cast failed") id $ RingArray.castMutArrayWith startR marr
    performGCSweep 4 100000
    res <- RingArray.eqArray ring arr
    res `shouldBe` expected

moduleName :: String
moduleName = "Data.RingArray"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        describe "Eq" $ do
            let lstArr = [0..99]
                lstRing = [50..99] ++ [0..49]
            it "eqArrayN True (n < len)"
                   $ eqArrayN lstArr lstRing 50 75 True
            it "eqArrayN True (n > len)"
                   $ eqArrayN lstArr lstRing 50 200 True
            it "eqArrayN False"
                   $ eqArrayN lstArr lstRing 10 75 False
            it "eqArray True" $ eqArray lstArr lstRing 50 True
            it "eqArray False" $ eqArray lstArr lstRing 20 False
