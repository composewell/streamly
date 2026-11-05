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
import Data.List (sort)
import Streamly.Internal.Data.Fold (Fold(..), Step(..))
import Streamly.Internal.Data.Time.Units
       (AbsTime, MilliSecond64(..), toAbsTime)
#ifdef DEVBUILD
import Data.Int (Int64)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (NanoSecond64(..), toRelTime64, diffAbsTime64)
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

-------------------------------------------------------------------------------
-- classifySessionsBy
-------------------------------------------------------------------------------

-- | An event at the given time in milliseconds.
at :: Int -> k -> a -> (AbsTime, (k, a))
at ms k a = (toAbsTime (MilliSecond64 (fromIntegral ms)), (k, a))

-- | A fold whose @extract@ returns "extract" and @final@ returns "final".
extractOrFinal :: Monad m => Fold m a String
extractOrFinal =
    Fold
        (\_ _ -> return $ Partial ())
        (return $ Partial ())
        (\_ -> return "extract")
        (\_ -> return "final")

-- | Sessions ejected by the ejection predicate are finished with @final@.
testClassifyEjectFinal :: IO ()
testClassifyEjectFinal = do
    -- At most one session, the second key ejects the first session.
    xs <- Stream.toList
        $ Stream.classifySessionsBy
            100 False (return . (>= 1)) 100 extractOrFinal
        $ Stream.fromList [at 0 (1 :: Int) 'a', at 0 2 'b']
    xs `shouldBe` [(1, "final"), (2, "final")]

-- | Sessions ejected on timeout are finished with @final@.
testClassifyTimeoutFinal :: IO ()
testClassifyTimeoutFinal = do
    -- The session of key 1 times out during the 500 ms delay.
    xs <- Stream.toList
        $ Stream.classifySessionsBy
            0.01 False (const (return False)) 0.1 extractOrFinal
        $ Stream.mapM
            (\e@(_, (k, _)) -> when (k == 2) (threadDelay 500000) >> return e)
        $ Stream.fromList [at 0 (1 :: Int) 'a', at 0 2 'b']
    xs `shouldBe` [(1, "final"), (2, "final")]

-- | A session restarted after its fold finished times out a timeout after
-- its own first event, not after the first event of the finished session.
testClassifyRestartExpiry :: IO ()
testClassifyRestartExpiry = do
    -- tick 0.1 s, timeout 2 s. The fold finishes at 0 ms, the session is
    -- restarted at 1500 ms and expires at 3500 ms. The 1 s delay advances
    -- the clock by about 10 ticks, to about 2500 ms, past the 2000 ms
    -- expiry of the first session but before the expiry of the second one.
    xs <- Stream.toList
        $ Stream.classifySessionsBy
            0.1 False (const (return False)) 2 (Fold.take 2 Fold.toList)
        $ Stream.mapM
            (\e@(_, (_, v)) -> when (v == 'd') (threadDelay 1000000)
                >> return e)
        $ Stream.fromList
            [ at 0 (1 :: Int) 'a', at 0 1 'b', at 1500 1 'c', at 1500 1 'd']
    xs `shouldBe` [(1, "ab"), (1, "cd")]

-- | A session restarted after its fold finished is counted in the session
-- count passed to the ejection predicate.
testClassifyRestartCount :: IO ()
testClassifyRestartCount = do
    -- At most one session. The restarted session of key 1 must be ejected
    -- when the session of key 2 starts.
    xs <- Stream.toList
        $ Stream.classifySessionsBy
            100 False (return . (>= 1)) 100 (Fold.take 2 Fold.toList)
        $ Stream.fromList
            [ at 0 (1 :: Int) 'a', at 0 1 'b', at 0 1 'c', at 0 2 'd'
            , at 0 1 'e']
    sort xs `shouldBe` [(1, "ab"), (1, "c"), (1, "e"), (2, "d")]

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
        describe "classifySessionsBy" $ do
            it "ejected session uses final" testClassifyEjectFinal
            it "timed out session uses final" testClassifyTimeoutFinal
            it "restarted session expiry" testClassifyRestartExpiry
            it "restarted session count" testClassifyRestartCount

