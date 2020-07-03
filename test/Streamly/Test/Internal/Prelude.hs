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

import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Test.Hspec as H
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, choose)
import Test.QuickCheck.Monadic (monadicIO, pick)

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Prelude as SI

import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)
import Data.Int (Int64)

max_length :: Int
max_length = 1000

tenPow8 :: Int64
tenPow8 = 10^(8 :: Int)

tenPow7 :: Int64
tenPow7 = 10^(7 :: Int)

takeDropTime :: NanoSecond64
takeDropTime = NanoSecond64 $ 5 * tenPow8

checkTakeDropTime :: (Maybe AbsTime, Maybe AbsTime) -> IO Bool
checkTakeDropTime (mt0, mt1) = do
    let graceTime = NanoSecond64 $ 8 * tenPow7
    case mt0 of
        Nothing -> return True
        Just t0 ->
            case mt1 of
                Nothing -> return True
                Just t1 -> do
                    let tMax = toRelTime64 (takeDropTime + graceTime)
                    let tMin = toRelTime64 (takeDropTime - graceTime)
                    let t = diffAbsTime64 t1 t0
                    let r = t >= tMin && t <= tMax
                    when (not r) $ putStrLn $
                        "t = " ++ show t ++
                        " tMin = " ++ show tMin ++
                        " tMax = " ++ show tMax
                    return r

testTakeByTime :: IO Bool
testTakeByTime = do
    r <-
          S.fold ((,) <$> FL.head <*> FL.last)
        $ SI.takeByTime takeDropTime
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime r

testDropByTime :: IO Bool
testDropByTime = do
    t0 <- getTime Monotonic
    mt1 <-
          S.head
        $ SI.dropByTime takeDropTime
        $ S.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime (Just t0, mt1)

unfold :: Property
unfold = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.enumerateFromToIntegral b
    ls <- S.toList $ S.unfold unf a
    return $ ls == [a..b]

unfold0 :: Property
unfold0 = monadicIO $ do
    a <- pick $ choose (0, max_length `div` 2)
    b <- pick $ choose (0, max_length)
    let unf = UF.supply (UF.enumerateFromToIntegral b) a
    ls <- S.toList $ SI.unfold0 unf
    return $ ls == [a..b]

main :: IO ()
main =
    hspec $ do
        describe "Filtering" $ do
            it "takeByTime" (testTakeByTime `shouldReturn` True)
            it "dropByTime" (testDropByTime `shouldReturn` True)
        describe "From Generators" $ do
            prop "unfold" unfold
            prop "unfold0" unfold0
