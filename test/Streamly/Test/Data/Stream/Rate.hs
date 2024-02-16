-- |
-- Module      : Streamly.Test.Data.Stream.Rate
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Rate (main) where

import Data.Int (Int64)
import Streamly.Data.Stream.Prelude (Config)
import Streamly.Internal.Data.Time.Clock (getTime, Clock(..))
import Streamly.Internal.Data.Time.Units
    (NanoSecond64, diffAbsTime64, fromRelTime64)
import System.Mem (performMajorGC)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Stream

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

measureRate' ::
       String
    -> (Config -> Config)
    -> Int -- buffers
    -> Int -- threads
    -> Either Double Int  -- either rate or count of actions
    -> Int
    -> (Double, Double)
    -> (Double, Double)
    -> Spec
measureRate'
    desc modifier buffers threads rval consumerDelay producerDelay expectedRange = do
    it (desc <> rateDesc
             <> " buffers: " <> show buffers
             <> " threads: " <> show threads
             <> ", consumer latency: " <> show consumerDelay
             <> ", producer latency: " <> show producerDelay)
        runTest

    where

    -- Keep a minimum of 2 for the very low rate cases, otherwise the
    -- timing would be 0 because we will finish as soon as the first result
    -- arrives.
    yieldCount :: Int
    yieldCount = case rval of
        Left r -> max 2 (round (10 * r))
        Right n -> max 2 n

    rateDesc = (case rval of
        Left r ->  ", rate: " <> show r <> ", count: " <> show yieldCount
        Right n -> ", count: " <> show n) <> ","

    cfg (_n :: Maybe Int64) =
          modifier
        . Stream.maxBuffer buffers
        -- . Stream.inspect True
        . Stream.maxThreads threads
        . case rval of
            Left r -> Stream.avgRate r
            Right _ -> Stream.rate Nothing
        -- XXX it comes out less than expected for ordered streams at high
        -- rates, need to fix.
        -- . Stream.maxYields (Just (fromIntegral yieldCount))

    threadAction f =
        case rval of
            Left _ ->
                  Stream.take yieldCount
                $ Stream.parMapM (cfg (Just (fromIntegral yieldCount))) f
                $ Stream.enumerateFrom (1 :: Int)
            Right _ ->
                Stream.parReplicateM
                    (cfg (Just (fromIntegral yieldCount))) yieldCount (f 1)

    runTest = do
        durationShouldBe expectedRange $ do
            res <- Stream.fold Fold.length
                $ (if consumerDelay > 0
                  then Stream.mapM $ \x ->
                            threadDelay (toMicroSecs consumerDelay) >> return x
                  else id)
                $ threadAction $ \_idx -> do
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
                    -- putStrLn $ "Done: " ++ show idx
                    return (1 :: Int)
            when (res /= yieldCount) $
                error $ "expected yield count: " ++ show yieldCount
                    ++ " actual: " ++ show res

        -- To ensure that when we use "inspect" option on the channel, GC
        -- occurs and cleans up the channel to print the debug info.
        performMajorGC

measureRateVariable ::
       String
    -> (Config -> Config)
    -> Double
    -> Int
    -> (Double, Double)
    -> (Double, Double)
    -> Spec
measureRateVariable desc modifier rval =
    measureRate' desc modifier (-1) (-1) (Left rval)

measureRate ::
       String
    -> (Config -> Config)
    -> Double
    -> Int
    -> Int
    -> (Double, Double)
    -> Spec
measureRate desc modifier rval consumerDelay producerDelay dur =
    let d = fromIntegral producerDelay
    in measureRateVariable desc modifier rval consumerDelay (d, d) dur

measureThreads ::
       String
    -> (Config -> Config)
    -> Int -- threads
    -> Int -- count of actions
    -> Spec
measureThreads desc modifier threads count = do
    let expectedTime =
            if threads < 0
            then 1.0
            else fromIntegral count / fromIntegral threads
        duration = (expectedTime * 0.9, expectedTime * 1.1)
    measureRate' desc modifier (-1) threads (Right count) 0 (1,1) duration

measureBuffers ::
       String
    -> (Config -> Config)
    -> Int -- buffers
    -> Int -- count of actions
    -> Spec
measureBuffers desc modifier buffers count = do
    let expectedTime =
            if buffers < 0
            then 1.0
            else fromIntegral count / fromIntegral buffers
        duration = (expectedTime * 0.9, expectedTime * 1.1)
    measureRate' desc modifier buffers (-1) (Right count) 0 (1,1) duration

moduleName :: String
moduleName = "Data.Stream.Rate"

main :: IO ()
main = hspec $ do
  describe moduleName $ do

    describe "maxBuffers" $ do
        measureBuffers "async" id (-1) 5
        -- XXX this test fails due to a known issue
        -- measureBuffers "maxBuffers" id 1 5
        measureBuffers "async" id 5 5

    describe "maxThreads" $ do
        measureThreads "async" id (-1) 5
        measureThreads "async" id 1 5
        measureThreads "async" id 5 5

        measureThreads "ordered" (Stream.ordered True) (-1) 5
        measureThreads "ordered" (Stream.ordered True) 1 5
        measureThreads "ordered" (Stream.ordered True) 5 5

        measureThreads "interleaved" (Stream.interleaved True) (-1) 5
        measureThreads "interleaved" (Stream.interleaved True) 1 5
        measureThreads "interleaved" (Stream.interleaved True) 5 5

    describe "max rate possible (count / time)" $ do
        measureRate "async" (Stream.rate Nothing) 1000000 0 0 (0, 1e9)

    let range = (8,12)

    -- Note that because after the last yield we don't wait, the last period
    -- will be effectively shorter. This becomes significant when the rates are
    -- lower (1 or lower). For rate 1 we lose 1 second in the end and for rate
    -- 10 0.1 second.
    let rates = [0.1, 1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "async no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "async" id r 0 0 range)

    -- XXX try staggering the dispatches to achieve higher rates
    -- Producer delay causes a lot of threads to be created, consuming large
    -- amounts of memory at higher rates.
    let rates = [0.1, 1, 10, 100
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                1000, 10000, 25000
#endif
                ]
     in describe "async no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "async" id r 0 1 range)

    -- At lower rates (1/10) this is likely to vary quite a bit depending on
    -- the spread of random producer latencies generated.
    let rates = [1, 10, 100
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 1000, 10000, 25000
#endif
                ]
     in describe "async, no consumer delay and variable producer delay" $
            forM_ rates $ \r ->
                measureRateVariable "async" id r 0 (0.1, 3) range

    let rates = [0.1, 1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "interleaved, no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "interleaved" (Stream.interleaved True) r 0 0 range)

    let rates = [0.1, 1, 10, 100, 1000
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 10000, 25000
#endif
                ]
     in describe "interleaved, no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "interleaved" (Stream.interleaved True) r 0 1 range)

    let rates = [0.1, 1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "ordered, no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "ordered" (Stream.ordered True) r 0 0 range)

    -- XXX after the change to stop workers when the heap is clearing
    -- thi does not work well at a 25000 ops per second, need to fix.
    let rates = [0.1, 1, 10, 100, 1000
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 10000, 12500
#endif
                ]
     in describe "ordered, no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "ordered" (Stream.ordered True) r 0 1 range)

    describe "async, some consumer delay and 1 sec producer delay" $ do
        -- ideally it should take 10 x 1 + 1 seconds
        forM_ [1] (\r -> measureRate "async" id r 1 1 (11, 16))
        -- ideally it should take 10 x 2 + 1 seconds
        forM_ [1] (\r -> measureRate "async" id r 2 1 (21, 23))
        -- ideally it should take 10 x 3 + 1 seconds
        forM_ [1] (\r -> measureRate "async" id r 3 1 (31, 33))

    describe "ordered, some consumer delay and 1 sec producer delay" $ do
        forM_ [1] (\r -> measureRate "ordered" (Stream.ordered True) r 1 1 (11, 16))
        forM_ [1] (\r -> measureRate "ordered" (Stream.ordered True) r 2 1 (21, 23))
        forM_ [1] (\r -> measureRate "ordered" (Stream.ordered True) r 3 1 (31, 33))
