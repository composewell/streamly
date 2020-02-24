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

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Prelude as SI

import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)
import Data.Int (Int64)

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, choose)
import Test.QuickCheck.Monadic (monadicIO, assert)

import Streamly



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

newtype WholeInt = WholeInt Int deriving (Eq)

instance Bounded WholeInt where
    minBound = WholeInt 0
    maxBound = WholeInt maxBound

{-# INLINE diff #-}
diff :: WholeInt -> WholeInt -> Int
diff (WholeInt x) (WholeInt y) = x - y

srcOrd :: Monad m => Int -> Int -> SerialT m WholeInt
srcOrd value' i = S.unfoldr step (i, 0 :: Int)
  where
    step (a, b)
        | c > value' = Nothing
        | otherwise =
            Just
                ( WholeInt c
                , ( if a + i == 0
                        then i
                        else a - 2
                  , b + 1))
      where
        c = a + b

testReassembleBy :: Property
testReassembleBy =
    forAll (choose (1, 5)) $ \i ->
        forAll (choose (10, 20)) $ \v ->
            monadicIO $ do
                l1 <- S.toList $ SI.reassembleBy (i + 1) diff $ srcOrd (v * (i + 1)) i
                let l2 = map WholeInt [0 .. (v * (i + 1) - 1)]
                assert $ l1 == l2

main :: IO ()
main =
    hspec $ do
        describe "Filtering" $ do
            it "takeByTime" (testTakeByTime `shouldReturn` True)
            it "dropByTime" (testDropByTime `shouldReturn` True)
        describe "Reordering" $ do
            prop "reassembleBy" testReassembleBy
