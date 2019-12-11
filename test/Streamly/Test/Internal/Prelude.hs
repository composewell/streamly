{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Main (main) where

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Test.Hspec as H

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as SI

import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
import Data.Int (Int64)

tenPow8 :: Int64
tenPow8 = 10^(8 :: Int)

tenPow7 :: Int64
tenPow7 = 10^(7 :: Int)

testTakeByTime :: Property
testTakeByTime =
    forAll (choose (1, 10)) $ \(n :: Int64) -> monadicIO $ do
        let lim = NanoSecond64 $ n * tenPow8
        let graceTime = NanoSecond64 $ 8 * tenPow7
        t <- run $ getTime Monotonic
        t'' <- run $ S.last $ SI.takeByTime lim $ S.repeatM (getTime Monotonic)
        case t'' of
          Nothing -> assert True
          Just t' -> assert $ diffAbsTime64 t' t <= toRelTime64 (lim + graceTime)

testDropByTime :: Property
testDropByTime =
    forAll (choose (1, 10)) $ \(n :: Int64) -> monadicIO $ do
        let lim = NanoSecond64 $ n * tenPow8
        t <- run $ getTime Monotonic
        _ <- run $ S.drain $ S.take 1 $ SI.dropByTime lim $ S.repeat ()
        t' <- run $ getTime Monotonic
        assert $ diffAbsTime64 t' t >= toRelTime64 lim

main :: IO ()
main =
    hspec $
    describe "Time combinators" $ do
        prop "takeByTime" testTakeByTime
        prop "dropByTime" testDropByTime

