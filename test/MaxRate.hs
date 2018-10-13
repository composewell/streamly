{-# LANGUAGE FlexibleContexts #-}

import Streamly
import qualified Streamly.Prelude as S
import Control.Concurrent
import Control.Monad
import System.Clock
import Test.Hspec
import System.Random

durationShouldBe :: (Double, Double) -> IO () -> Expectation
durationShouldBe d@(tMin, tMax) action = do
        t0 <- getTime Monotonic
        action
        t1 <- getTime Monotonic
        let t = fromIntegral (toNanoSecs (t1 - t0)) / 1e9
            -- tMax = fromNanoSecs (round $ d*10^9*1.2)
            -- tMin = fromNanoSecs (round $ d*10^9*0.8)
        putStrLn $ "Expected: " <> show d <> " Took: " <> show t
        (t <= tMax && t >= tMin) `shouldBe` True

toMicroSecs :: Num a => a -> a
toMicroSecs x = x * 10^(6 :: Int)

measureRate' :: IsStream t
    => String
    -> (t IO Int -> SerialT IO Int)
    -> Double
    -> Int
    -> (Double, Double)
    -> (Double, Double)
    -> Spec
measureRate' desc t rval consumerDelay producerDelay dur =
    it (desc <> " rate: " <> show rval
             <> ", consumer latency: " <> show consumerDelay
             <> ", producer latency: " <> show producerDelay)
    $ durationShouldBe dur $
        runStream
            $ (if consumerDelay > 0
              then S.mapM $ \x ->
                        threadDelay (toMicroSecs consumerDelay) >> return x
              else id)
            $ t
            $ maxBuffer  (-1)
            $ maxThreads (-1)
            $ avgRate rval
            $ S.take  (round $ rval * 10)
            $ S.repeatM $ do
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
    in measureRate' desc t rval consumerDelay (d, d) dur

main :: IO ()
main = hspec $ do
    let range = (8,12)

    -- Note that because after the last yield we don't wait, the last period
    -- will be effectively shorter. This becomes significant when the rates are
    -- lower (1 or lower). For rate 1 we lose 1 second in the end and for rate
    -- 10 0.1 second.
    let rates = [1, 10, 100, 1000, 10000, 100000, 1000000]
     in describe "asyncly no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "asyncly" asyncly r 0 0 range)

    -- XXX try staggering the dispatches to achieve higher rates
    let rates = [1, 10, 100, 1000, 10000, 25000]
     in describe "asyncly no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "asyncly" asyncly r 0 1 range)

    -- At lower rates (1/10) this is likely to vary quite a bit depending on
    -- the spread of random producer latencies generated.
    let rates = [1, 10, 100, 1000, 10000, 25000]
     in describe "asyncly no consumer delay and variable producer delay" $
            forM_ rates $ \r ->
                measureRate' "asyncly" asyncly r 0 (0.1, 3) range

    let rates = [1, 10, 100, 1000, 10000, 100000, 1000000]
     in describe "wAsyncly no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "wAsyncly" wAsyncly r 0 0 range)

    let rates = [1, 10, 100, 1000, 10000, 25000]
     in describe "wAsyncly no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "wAsyncly" wAsyncly r 0 1 range)

    let rates = [1, 10, 100, 1000, 10000, 100000, 1000000]
     in describe "aheadly no consumer delay no producer delay" $
            forM_ rates (\r -> measureRate "aheadly" aheadly r 0 0 range)

    -- XXX after the change to stop workers when the heap is clearing
    -- thi does not work well at a 25000 ops per second, need to fix.
    let rates = [1, 10, 100, 1000, 10000, 12500]
     in describe "aheadly no consumer delay and 1 sec producer delay" $
            forM_ rates (\r -> measureRate "aheadly" aheadly r 0 1 range)

    describe "asyncly with 1 sec producer delay and some consumer delay" $ do
        -- ideally it should take 10 x 1 + 1 seconds
        forM_ [1] (\r -> measureRate "asyncly" asyncly r 1 1 (11, 16))
        -- ideally it should take 10 x 2 + 1 seconds
        forM_ [1] (\r -> measureRate "asyncly" asyncly r 2 1 (21, 23))
        -- ideally it should take 10 x 3 + 1 seconds
        forM_ [1] (\r -> measureRate "asyncly" asyncly r 3 1 (31, 33))

    describe "aheadly with 1 sec producer delay and some consumer delay" $ do
        forM_ [1] (\r -> measureRate "aheadly" aheadly r 1 1 (11, 16))
        forM_ [1] (\r -> measureRate "aheadly" aheadly r 2 1 (21, 23))
        forM_ [1] (\r -> measureRate "aheadly" aheadly r 3 1 (31, 33))
