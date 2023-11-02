{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty.Bench
import Streamly.Prelude as S
import System.Random (randomRIO)

-- Note that we should also compare the cpuTime especially when threaded
-- runtime is used with this benchmark because thread scheduling is not
-- predictable and can add non-deterministic delay to the total time measured.
--
-- Also, the worker dispatch depends on the worker dispatch latency which is
-- set to fixed 200 us. We need to keep that in mind when designing tests.

moduleName :: String
moduleName = "Prelude.Adaptive"

value :: Int
value = 1000

{-# INLINE source #-}
source :: IsStream t => (Int, Int) -> t IO Int
source range = S.replicateM value $ do
    r <- randomRIO range
    when (r /= 0) $ liftIO $ threadDelay r
    return r

{-# INLINE run #-}
run :: IsStream t => (Int, Int) -> (Int, Int) -> (t IO Int -> SerialT IO Int) -> IO ()
run srange crange t = S.drain $ S.mapM action (t $ source srange)

    where

    action x = liftIO $ do
        d <- randomRIO crange
        when (d /= 0) $ threadDelay d
        return x

low, medium, high :: Int
low = 10
medium = 20
high = 30

{-# INLINE noDelay #-}
noDelay :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
noDelay = run (0,0) (0,0)

{-# INLINE alwaysConstSlowSerial #-}
alwaysConstSlowSerial :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstSlowSerial = run (0,0) (medium,medium)

{-# INLINE alwaysConstSlow #-}
alwaysConstSlow :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstSlow = run (low,low) (medium,medium)

{-# INLINE alwaysConstFast #-}
alwaysConstFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstFast = run (high,high) (medium,medium)

{-# INLINE alwaysVarSlow #-}
alwaysVarSlow :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysVarSlow = run (low,low) (low,high)

{-# INLINE alwaysVarFast #-}
alwaysVarFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysVarFast = run (high,high) (low,high)

-- XXX add variable producer tests as well

{-# INLINE runVarSometimesFast #-}
runVarSometimesFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
runVarSometimesFast = run (medium,medium) (low,high)

{-# INLINE randomVar #-}
randomVar :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
randomVar = run (low,high) (low,high)

main :: IO ()
main =
  defaultMain [bgroup moduleName allBenchmarks]

  where

  allBenchmarks =
    [
      bgroup "serialConstantSlowConsumer"
      [ bench "serially"    $ nfIO $ alwaysConstSlowSerial fromSerial
      , bench "wSerially"   $ nfIO $ alwaysConstSlowSerial fromWSerial
      ]
    , bgroup "default"
      [ bench "serially"   $ nfIO $ noDelay fromSerial
      , bench "wSerially"  $ nfIO $ noDelay fromWSerial
      , bench "aheadly"    $ nfIO $ noDelay fromAhead
      , bench "asyncly"    $ nfIO $ noDelay fromAsync
      , bench "wAsyncly"   $ nfIO $ noDelay fromWAsync
      , bench "parallely"  $ nfIO $ noDelay fromParallel
      ]
    , bgroup "constantSlowConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysConstSlow fromAhead
      , bench "asyncly"    $ nfIO $ alwaysConstSlow fromAsync
      , bench "wAsyncly"   $ nfIO $ alwaysConstSlow fromWAsync
      , bench "parallely"  $ nfIO $ alwaysConstSlow fromParallel
      ]
   ,  bgroup "constantFastConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysConstFast fromAhead
      , bench "asyncly"    $ nfIO $ alwaysConstFast fromAsync
      , bench "wAsyncly"   $ nfIO $ alwaysConstFast fromWAsync
      , bench "parallely"  $ nfIO $ alwaysConstFast fromParallel
      ]
   ,  bgroup "variableSlowConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysVarSlow fromAhead
      , bench "asyncly"    $ nfIO $ alwaysVarSlow fromAsync
      , bench "wAsyncly"   $ nfIO $ alwaysVarSlow fromWAsync
      , bench "parallely"  $ nfIO $ alwaysVarSlow fromParallel
      ]
   ,  bgroup "variableFastConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysVarFast fromAhead
      , bench "asyncly"    $ nfIO $ alwaysVarFast fromAsync
      , bench "wAsyncly"   $ nfIO $ alwaysVarFast fromWAsync
      , bench "parallely"  $ nfIO $ alwaysVarFast fromParallel
      ]
   ,  bgroup "variableSometimesFastConsumer"
      [ bench "aheadly"    $ nfIO $ runVarSometimesFast fromAhead
      , bench "asyncly"    $ nfIO $ runVarSometimesFast fromAsync
      , bench "wAsyncly"   $ nfIO $ runVarSometimesFast fromWAsync
      , bench "parallely"  $ nfIO $ runVarSometimesFast fromParallel
      ]
   ,  bgroup "variableFullOverlap"
      [ bench "aheadly"    $ nfIO $ randomVar fromAhead
      , bench "asyncly"    $ nfIO $ randomVar fromAsync
      , bench "wAsyncly"   $ nfIO $ randomVar fromWAsync
      , bench "parallely"  $ nfIO $ randomVar fromParallel
      ]
   ]
