-- |
-- Module      : Streamly.Test.Data.Stream.Rate
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Rate (main) where

import Streamly.Data.Stream.Prelude (Stream, Config)
import Streamly.Internal.Data.Time.Clock (getTime, Clock(..))
import Streamly.Internal.Data.Time.Units
    (NanoSecond64, diffAbsTime64, fromRelTime64)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream

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

    let
        cfg =
              Stream.maxBuffer buffers
            . Stream.maxThreads threads
            . case rval of
                Left r -> Stream.avgRate r
                Right _ -> Stream.rate Nothing
            . modifier

        threadAction =
            case rval of
                Left r ->
                      Stream.take (round $ 10 * r)
                    . Stream.parRepeatM cfg
                Right n ->
                    Stream.parReplicateM cfg n

        rateDesc = case rval of
            Left r ->  " rate: " <> show r
            Right n -> " count: " <> show n

    it (desc <> rateDesc
             <> " buffers: " <> show buffers
             <> " threads: " <> show threads
             <> ", consumer latency: " <> show consumerDelay
             <> ", producer latency: " <> show producerDelay)
        $ durationShouldBe expectedRange $
            Stream.fold Fold.drain
                $ (if consumerDelay > 0
                  then Stream.mapM $ \x ->
                            threadDelay (toMicroSecs consumerDelay) >> return x
                  else id)
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
                    return (1 :: Int)

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
            forM_ rates (\r -> measureRate "async" id r 0 0 range)

    -- XXX try staggering the dispatches to achieve higher rates
    -- Producer delay causes a lot of threads to be created, consuming large
    -- amounts of memory at higher rates.
    let rates = [1, 10, 100
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                1000, 10000, 25000
#endif
                ]
     in describe "asyncly no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "async" id r 0 1 range)

    -- At lower rates (1/10) this is likely to vary quite a bit depending on
    -- the spread of random producer latencies generated.
    let rates = [1, 10, 100
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 1000, 10000, 25000
#endif
                ]
     in describe "asyncly, no consumer delay and variable producer delay" $
            forM_ rates $ \r ->
                measureRateVariable "async" id r 0 (0.1, 3) range

    let rates = [1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "interleaved, no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "interleaved" (Stream.interleaved True) r 0 0 range)

    let rates = [1, 10, 100, 1000
#if !defined(__GHCJS__) && defined USE_LARGE_MEMORY
                , 10000, 25000
#endif
                ]
     in describe "interleaved, no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "interleaved" (Stream.interleaved True) r 0 1 range)

    let rates = [1, 10, 100, 1000, 10000
#ifndef __GHCJS__
                , 100000, 1000000
#endif
                ]
     in describe "ordered, no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "ordered" (Stream.ordered True) r 0 0 range)

    -- XXX after the change to stop workers when the heap is clearing
    -- thi does not work well at a 25000 ops per second, need to fix.
    let rates = [1, 10, 100, 1000
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
