{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Prelude.MaxRate
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Prelude.Rate (main) where

import qualified Streamly.Prelude as S

import Streamly.Prelude
    ( fromAhead, fromAsync, fromWAsync, avgRate, maxBuffer, maxThreads, rate,
      SerialT, IsStream )
import Streamly.Internal.Data.Time.Clock (getTime, Clock(..))
import Streamly.Internal.Data.Time.Units
    (NanoSecond64, diffAbsTime64, fromRelTime64)

import Control.Concurrent
import Control.Monad
import System.Random
import Test.Hspec

durationShouldBe :: (Double, Double) -> IO () -> Expectation
durationShouldBe d@(tMin, tMax) action = do
        t0 <- getTime Monotonic
        action
        t1 <- getTime Monotonic
        let diff = fromRelTime64 (diffAbsTime64 t1 t0) :: NanoSecond64
        let t = fromIntegral diff / 1e9
        putStrLn $ "Expected: " <> show d <> " Took: " <> show t
        (t <= tMax && t >= tMin) `shouldBe` True

toMicroSecs :: Num a => a -> a
toMicroSecs x = x * 10^(6 :: Int)

measureRate' :: IsStream t
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Int -- buffers
    -> Int -- threads
    -> Either Double Int  -- either rate or count of actions
    -> Int
    -> (Double, Double)
    -> (Double, Double)
    -> Spec
measureRate' desc t buffers threads rval consumerDelay producerDelay expectedRange = do

    let threadAction =
            case rval of
                Left r -> S.take (round $ 10 * r) . S.repeatM
                Right n -> S.replicateM n

        rateDesc = case rval of
            Left r ->  " rate: " <> show r
            Right n -> " count: " <> show n

    it (desc <> rateDesc
             <> " buffers: " <> show buffers
             <> " threads: " <> show threads
             <> ", consumer latency: " <> show consumerDelay
             <> ", producer latency: " <> show producerDelay)
        $ durationShouldBe expectedRange $
            S.drain
                $ (if consumerDelay > 0
                  then S.mapM $ \x ->
                            threadDelay (toMicroSecs consumerDelay) >> return x
                  else id)
                $ t
                $ maxBuffer  buffers
                $ maxThreads threads
                $ (case rval of {Left r -> avgRate r; Right _ -> rate Nothing})
                $ threadAction $ do
                    let (t1, t2) = producerDelay
                    r <- if t1 == t2
                         then return $ round $ toMicroSecs t1
                         else randomRIO ( round $ toMicroSecs t1
                                        , round $ toMicroSecs t2)
                    when (r > 0) $ -- do
                        -- t1 <- getTime Monotonic
                        threadDelay r
                        -- t2 <- getTime Monotonic
                        -- let delta = fromIntegral (toNanoSecs (t2 - t1)) / 1000000000
                        -- putStrLn $ "delay took: " <> show delta
                        -- when (delta > 2) $ do
                        --     putStrLn $ "delay took high: " <> show delta
                    return 1

measureRateVariable :: IsStream t
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Double
    -> Int
    -> (Double, Double)
    -> (Double, Double)
    -> Spec
measureRateVariable desc t rval consumerDelay producerDelay dur =
    measureRate' desc t (-1) (-1) (Left rval)
                 consumerDelay producerDelay dur

measureRate :: IsStream t
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Double
    -> Int
    -> Int
    -> (Double, Double)
    -> Spec
measureRate desc t rval consumerDelay producerDelay dur =
    let d = fromIntegral producerDelay
    in measureRateVariable desc t rval consumerDelay (d, d) dur

measureThreads :: IsStream t
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Int -- threads
    -> Int -- count of actions
    -> Spec
measureThreads desc t threads count = do
    let expectedTime =
            if threads < 0
            then 1.0
            else fromIntegral count / fromIntegral threads
        duration = (expectedTime * 0.9, expectedTime * 1.1)
    measureRate' desc t (-1) threads (Right count) 0 (1,1) duration

measureBuffers :: IsStream t
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Int -- buffers
    -> Int -- count of actions
    -> Spec
measureBuffers desc t buffers count = do
    let expectedTime =
            if buffers < 0
            then 1.0
            else fromIntegral count / fromIntegral buffers
        duration = (expectedTime * 0.9, expectedTime * 1.1)
    measureRate' desc t buffers (-1) (Right count) 0 (1,1) duration

moduleName :: String
moduleName = "Prelude.Rate"

main :: IO ()
main = hspec $ do
  describe moduleName $ do

    describe "maxBuffers" $ do
        measureBuffers "asyncly" fromAsync (-1) 5
        -- XXX this test fails due to a known issue
        -- measureBuffers "maxBuffers" fromAsync 1 5
        measureBuffers "asyncly" fromAsync 5 5

    describe "maxThreads" $ do
        measureThreads "asyncly" fromAsync (-1) 5
        measureThreads "asyncly" fromAsync 1 5
        measureThreads "asyncly" fromAsync 5 5

        measureThreads "aheadly" fromAhead (-1) 5
        measureThreads "aheadly" fromAhead 1 5
        measureThreads "aheadly" fromAhead 5 5

    let range = (8,12)

    -- Note that because after the last yield we don't wait, the last period
    -- will be effectively shorter. This becomes significant when the rates are
    -- lower (1 or lower). For rate 1 we lose 1 second in the end and for rate
    -- 10 0.1 second.
    let rates = [1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "asyncly no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "asyncly" fromAsync r 0 0 range)

    -- XXX try staggering the dispatches to achieve higher rates
    -- Producer delay causes a lot of threads to be created, consuming large
    -- amounts of memory at higher rates.
    let rates = [1, 10, 100
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                1000, 10000, 25000
#endif
                ]
     in describe "asyncly no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "asyncly" fromAsync r 0 1 range)

    -- At lower rates (1/10) this is likely to vary quite a bit depending on
    -- the spread of random producer latencies generated.
    let rates = [1, 10, 100
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 1000, 10000, 25000
#endif
                ]
     in describe "asyncly, no consumer delay and variable producer delay" $
            forM_ rates $ \r ->
                measureRateVariable "asyncly" fromAsync r 0 (0.1, 3) range

    let rates = [1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "fromWAsync, no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "fromWAsync" fromWAsync r 0 0 range)

    let rates = [1, 10, 100, 1000
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 10000, 25000
#endif
                ]
     in describe "fromWAsync, no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "fromWAsync" fromWAsync r 0 1 range)

    let rates = [1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "aheadly, no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "aheadly" fromAhead r 0 0 range)

    -- XXX after the change to stop workers when the heap is clearing
    -- thi does not work well at a 25000 ops per second, need to fix.
    let rates = [1, 10, 100, 1000
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 10000, 12500
#endif
                ]
     in describe "aheadly, no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "aheadly" fromAhead r 0 1 range)

    describe "asyncly, some consumer delay and 1 sec producer delay" $ do
        -- ideally it should take 10 x 1 + 1 seconds
        forM_ [1] (\r -> measureRate "asyncly" fromAsync r 1 1 (11, 16))
        -- ideally it should take 10 x 2 + 1 seconds
        forM_ [1] (\r -> measureRate "asyncly" fromAsync r 2 1 (21, 23))
        -- ideally it should take 10 x 3 + 1 seconds
        forM_ [1] (\r -> measureRate "asyncly" fromAsync r 3 1 (31, 33))

    describe "aheadly, some consumer delay and 1 sec producer delay" $ do
        forM_ [1] (\r -> measureRate "aheadly" fromAhead r 1 1 (11, 16))
        forM_ [1] (\r -> measureRate "aheadly" fromAhead r 2 1 (21, 23))
        forM_ [1] (\r -> measureRate "aheadly" fromAhead r 3 1 (31, 33))
