-- XXX We are using head/tail at one place
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- |
-- Module      : Streamly.Test.Data.Stream.Prelude.Time
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Prelude.Time (main) where

import Control.Concurrent ( threadDelay )
import Control.Monad ( when)
#ifdef DEVBUILD
import Data.Int (Int64)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)

import qualified Streamly.Internal.Data.Fold as Fold
#endif
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Stream

import Test.Hspec as H

#ifdef DEVBUILD
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

testTakeInterval :: IO Bool
testTakeInterval = do
    r <-
         Stream.fold (Fold.tee Fold.one Fold.latest)
        $ Stream.takeInterval (fromIntegral takeDropTime * 10**(-9))
        $ Stream.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime r

testDropInterval :: IO Bool
testDropInterval = do
    t0 <- getTime Monotonic
    mt1 <-
         Stream.fold Fold.one
        $ Stream.dropInterval (fromIntegral takeDropTime * 10**(-9))
        $ Stream.repeatM (threadDelay 1000 >> getTime Monotonic)
    checkTakeDropTime (Just t0, mt1)
#endif

-- | A stream of n elements, each one generated after a delay of the given
-- number of milliseconds.
delayedStream :: Int -> Int -> Stream.Stream IO Int
delayedStream ms n =
    Stream.mapM (\x -> threadDelay (ms * 1000) >> return x)
        $ Stream.fromList [1..n]

-- The bounds are wide so that scheduling delays do not fail the tests.

-- | 40 elements 50 ms apart, about 6 of them are generated in 0.3 s.
testTakeIntervalElems :: IO ()
testTakeIntervalElems = do
    xs <- Stream.toList $ Stream.takeInterval 0.3 $ delayedStream 50 40
    when (null xs || length xs >= 20)
        $ error $ "takeInterval: unexpected number of elements " ++ show xs
    xs `shouldBe` [1 .. length xs]

-- | 10 elements 100 ms apart, about 3 of them are generated in 0.3 s.
testDropIntervalElems :: IO ()
testDropIntervalElems = do
    xs <- Stream.toList $ Stream.dropInterval 0.3 $ delayedStream 100 10
    when (null xs || length xs >= 10)
        $ error $ "dropInterval: unexpected number of elements " ++ show xs
    xs `shouldBe` [11 - length xs .. 10]

-- | Two bursts, 1 and 2, then 3 and 4 after a gap of 1 s.
twoBursts :: Stream.Stream IO Int
twoBursts =
    Stream.mapM (\x -> when (x == 3) (threadDelay 1000000) >> return x)
        $ Stream.fromList [1, 2, 3, 4]

testSampleBurst :: IO ()
testSampleBurst = do
    let xs = Stream.fromList [1, 2, 3 :: Int]
    Stream.toList (Stream.sampleBurstEnd 0.5 xs) `shouldReturn` [3]
    Stream.toList (Stream.sampleBurstStart 0.5 xs) `shouldReturn` [1]
    Stream.toList (Stream.sampleBurstEnd 0.5 twoBursts) `shouldReturn` [2, 4]
    Stream.toList (Stream.sampleBurstStart 0.5 twoBursts) `shouldReturn` [1, 3]
    Stream.toList (Stream.sampleBurstEnd 0.5 (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

moduleName :: String
moduleName = "Data.Stream.Time"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
#ifdef DEVBUILD
        describe "Filtering" $ do
            it "takeInterval" (testTakeInterval `shouldReturn` True)
#ifdef INCLUDE_FLAKY_TESTS
            it "dropInterval" (testDropInterval `shouldReturn` True)
#endif
#endif
        it "takeInterval elements" testTakeIntervalElems
        it "dropInterval elements" testDropIntervalElems
        it "sampleBurstEnd/sampleBurstStart" testSampleBurst

