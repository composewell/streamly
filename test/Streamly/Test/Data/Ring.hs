-- |
-- Module      : Streamly.Test.Data.Ring
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Ring (main) where

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Ring as Ring

import Prelude as P

import Test.Hspec as H

eqArrayN :: [Int] -> [Int] -> Int -> Int -> Bool -> IO ()
eqArrayN lstArr lstRing startR nelem expected = do
    let arr = Array.fromList lstArr
    marr <- MutArray.fromList lstRing
    let ring =
            maybe (error "cast failed") id $ Ring.castMutArrayWith startR marr
    Ring.eqArrayN ring arr nelem `shouldReturn` expected

eqArray :: [Int] -> [Int] -> Int -> Bool -> IO ()
eqArray lstArr lstRing startR expected = do
    let arr = Array.fromList lstArr
    marr <- MutArray.fromList lstRing
    let ring =
            maybe (error "cast failed") id $ Ring.castMutArrayWith startR marr
    Ring.eqArray ring arr `shouldReturn` expected

moduleName :: String
moduleName = "Data.Ring"

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
