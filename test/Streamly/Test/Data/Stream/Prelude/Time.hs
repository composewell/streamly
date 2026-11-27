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
import System.Timeout (timeout)
#ifdef DEVBUILD
import Data.Int (Int64)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), toRelTime64, diffAbsTime64)
#endif

import qualified Streamly.Internal.Data.Fold as Fold
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

-- The tests below do not measure time, they only use intervals that are either
-- much longer or much shorter than the time taken by the stream.

longInterval :: Double
longInterval = 100

shortInterval :: Double
shortInterval = 0.1

-- | An endless stream of consecutive numbers, one every millisecond.
slowEnumeration :: Stream.Stream IO Int
slowEnumeration =
    Stream.mapM (\x -> threadDelay 1000 >> return x) (Stream.enumerateFrom 1)

takeIntervalAll :: IO [Int]
takeIntervalAll =
    Stream.fold Fold.toList
        $ Stream.takeInterval longInterval
        $ Stream.fromList [1..10]

dropIntervalAll :: IO [Int]
dropIntervalAll =
    Stream.fold Fold.toList
        $ Stream.dropInterval longInterval
        $ Stream.fromList [1..10]

-- Zero, negative and NaN intervals.
takeIntervalOf :: Double -> IO [Int]
takeIntervalOf d =
    Stream.fold Fold.toList
        $ Stream.takeInterval d
        $ Stream.fromList [1..10]

dropIntervalOf :: Double -> IO [Int]
dropIntervalOf d =
    Stream.fold Fold.toList
        $ Stream.dropInterval d
        $ Stream.fromList [1..10]

nan :: Double
nan = 0 / 0

-- The stream blocks for a long time after its first element, takeInterval
-- must end it at the end of the interval nevertheless.
takeIntervalBlocked :: IO (Maybe [Int])
takeIntervalBlocked =
    timeout 10000000
        $ Stream.fold Fold.toList
        $ Stream.takeInterval shortInterval
        $ Stream.mapM (\x -> when (x > 1) (threadDelay 100000000) >> return x)
        $ Stream.fromList [1..10]

-- An interval that overflows an Int64 when converted to nanoseconds.
dropIntervalHuge :: Double -> IO [Int]
dropIntervalHuge d =
    Stream.fold Fold.toList
        $ Stream.dropInterval d
        $ Stream.fromList [1..10]

takeInterval_Of :: Double -> IO [Int]
takeInterval_Of d =
    Stream.fold Fold.toList
        $ Stream.takeInterval_ d
        $ Stream.fromList [1..10]

takeInterval_Prefix :: IO Bool
takeInterval_Prefix = do
    xs <-
        Stream.fold Fold.toList
            $ Stream.takeInterval_ shortInterval slowEnumeration
    return $ not (null xs) && xs == [1..length xs]

-- The second element is generated after the end of the interval, by an action
-- started before it. takeInterval_ does not interrupt the action, and yields
-- the element. No action is started after the end of the interval.
takeInterval_Blocked :: IO [Int]
takeInterval_Blocked =
    Stream.fold Fold.toList
        $ Stream.takeInterval_ shortInterval
        $ Stream.mapM (\x -> when (x > 1) (threadDelay 300000) >> return x)
        $ Stream.fromList [1..10]

-- The loop does not allocate, so the timer thread of takeInterval_ runs only if
-- the loop yields the CPU. A loop that never yields cannot be interrupted, so
-- the stream is finite: if the timer thread does not run, the loop ends by
-- itself after several seconds, having taken all the elements. The number of
-- elements must fit in an Int of 32 bits, for the JavaScript backend.
takeInterval_Spin :: IO Bool
takeInterval_Spin = do
    let total = 2000000000 :: Int
    n <-
        Stream.fold Fold.length
            $ Stream.takeInterval_ shortInterval
            $ Stream.enumerateFromTo 1 total
    return $ n < total

-- The stream is endless, so this returns only if takeInterval ends it. The
-- elements taken must be the beginning of the stream.
takeIntervalPrefix :: IO Bool
takeIntervalPrefix = do
    xs <-
        Stream.fold Fold.toList
            $ Stream.takeInterval shortInterval slowEnumeration
    return $ not (null xs) && xs == [1..length xs]

-- The elements that remain must be a suffix of the stream, starting after the
-- first element.
dropIntervalSuffix :: IO Bool
dropIntervalSuffix = do
    xs <-
        Stream.fold Fold.toList
            $ Stream.take 10
            $ Stream.dropInterval shortInterval slowEnumeration
    return $ case xs of
        x : _ -> x > 1 && xs == [x .. x + 9]
        [] -> False

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
        describe "Trimming" $ do
            it "takeInterval takes all the elements within the interval"
                (takeIntervalAll `shouldReturn` [1..10])
            it "takeInterval ends the stream after the interval"
                (takeIntervalPrefix `shouldReturn` True)
            it "dropInterval drops all the elements within the interval"
                (dropIntervalAll `shouldReturn` [])
            it "dropInterval yields the elements after the interval"
                (dropIntervalSuffix `shouldReturn` True)
            it "takeInterval takes nothing if the interval is zero"
                (takeIntervalOf 0 `shouldReturn` [])
            it "takeInterval takes nothing if the interval is negative"
                (takeIntervalOf (-1) `shouldReturn` [])
            it "takeInterval takes nothing if the interval is NaN"
                (takeIntervalOf nan `shouldReturn` [])
            it "takeInterval ends a blocked stream after the interval"
                (takeIntervalBlocked `shouldReturn` Just [1])
            it "takeInterval_ takes all the elements within the interval"
                (takeInterval_Of longInterval `shouldReturn` [1..10])
            it "takeInterval_ takes all the elements if the interval is infinite"
                (takeInterval_Of (1/0) `shouldReturn` [1..10])
            it "takeInterval_ ends the stream after the interval"
                (takeInterval_Prefix `shouldReturn` True)
            it "takeInterval_ takes nothing if the interval is zero"
                (takeInterval_Of 0 `shouldReturn` [])
            it "takeInterval_ takes nothing if the interval is negative"
                (takeInterval_Of (-1) `shouldReturn` [])
            it "takeInterval_ takes nothing if the interval is NaN"
                (takeInterval_Of nan `shouldReturn` [])
            it "takeInterval_ yields the element of an action in progress"
                (takeInterval_Blocked `shouldReturn` [1,2])
            it "takeInterval_ ends a loop that never yields the CPU"
                (takeInterval_Spin `shouldReturn` True)
            it "dropInterval drops nothing if the interval is zero"
                (dropIntervalOf 0 `shouldReturn` [1..10])
            it "dropInterval drops nothing if the interval is negative"
                (dropIntervalOf (-1) `shouldReturn` [1..10])
            it "dropInterval drops nothing if the interval is NaN"
                (dropIntervalOf nan `shouldReturn` [1..10])
            it "dropInterval drops everything if the interval is 1e10 seconds"
                (dropIntervalHuge 1e10 `shouldReturn` [])
            it "dropInterval drops everything if the interval is 1e300 seconds"
                (dropIntervalHuge 1e300 `shouldReturn` [])
            it "dropInterval drops everything if the interval is infinite"
                (dropIntervalHuge (1/0) `shouldReturn` [])

