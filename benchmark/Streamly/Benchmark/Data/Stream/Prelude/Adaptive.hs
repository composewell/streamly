-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Streamly.Data.Stream.Prelude (Stream, Config)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream

import Test.Tasty.Bench

-- Note that we should also compare the cpuTime especially when threaded
-- runtime is used with this benchmark because thread scheduling is not
-- predictable and can add non-deterministic delay to the total time measured.
--
-- Also, the worker dispatch depends on the worker dispatch latency which is
-- set to fixed 200 us. We need to keep that in mind when designing tests.

moduleName :: String
moduleName = "Data.Stream.Adaptive"

value :: Int
value = 1000

produce :: MonadIO m => (Int, Int) -> m Int
produce range = do
    r <- randomRIO range
    when (r /= 0) $ liftIO $ threadDelay r
    return r

consume :: MonadIO m => (Int, Int) -> Int -> m Int
consume crange x = liftIO $ do
    d <- randomRIO crange
    when (d /= 0) $ threadDelay d
    return x

{-# INLINE serialSrc #-}
serialSrc :: (Int, Int) -> Stream IO Int
serialSrc range = Stream.replicateM value $ produce range

{-# INLINE parSrc #-}
parSrc :: (Config -> Config) -> (Int, Int) -> Stream IO Int
parSrc cfg range = Stream.parReplicateM cfg value $ produce range

{-# INLINE serialRun #-}
serialRun :: (Int, Int) -> (Int, Int) -> IO ()
serialRun srange crange =
    Stream.fold Fold.drain
        $ Stream.mapM (consume crange) (serialSrc srange)

{-# INLINE parRun #-}
parRun :: (Config -> Config) -> (Int, Int) -> (Int, Int) -> IO ()
parRun cfg srange crange =
    Stream.fold Fold.drain
        $ Stream.parMapM cfg (consume crange) (parSrc cfg srange)

low, medium, high :: Int
low = 10
medium = 20
high = 30

{-# INLINE noDelaySerial #-}
noDelaySerial :: IO ()
noDelaySerial = serialRun (0,0) (0,0)

{-# INLINE noDelay #-}
noDelay :: (Config -> Config) -> IO ()
noDelay cfg = parRun cfg (0,0) (0,0)

{-# INLINE alwaysConstSlowSerial #-}
alwaysConstSlowSerial :: IO ()
alwaysConstSlowSerial = serialRun (0,0) (medium,medium)

{-# INLINE alwaysConstSlow #-}
alwaysConstSlow :: (Config -> Config) -> IO ()
alwaysConstSlow cfg = parRun cfg (low,low) (medium,medium)

{-# INLINE alwaysConstFast #-}
alwaysConstFast :: (Config -> Config) -> IO ()
alwaysConstFast cfg = parRun cfg (high,high) (medium,medium)

{-# INLINE alwaysVarSlow #-}
alwaysVarSlow :: (Config -> Config) -> IO ()
alwaysVarSlow cfg = parRun cfg (low,low) (low,high)

{-# INLINE alwaysVarFast #-}
alwaysVarFast :: (Config -> Config) -> IO ()
alwaysVarFast cfg = parRun cfg (high,high) (low,high)

-- XXX add variable producer tests as well

{-# INLINE runVarSometimesFast #-}
runVarSometimesFast :: (Config -> Config) -> IO ()
runVarSometimesFast cfg = parRun cfg (medium,medium) (low,high)

{-# INLINE randomVar #-}
randomVar :: (Config -> Config) -> IO ()
randomVar cfg = parRun cfg (low,high) (low,high)

main :: IO ()
main =
  defaultMain [bgroup moduleName allBenchmarks]

  where

  allBenchmarks =
    [
      bgroup "serialConstantSlowConsumer"
      [ bench "serial" $ nfIO alwaysConstSlowSerial
      ]
    , bgroup "default"
      [ bench "serial"      $ nfIO noDelaySerial
      , bench "async"       $ nfIO $ noDelay id
      , bench "ordered"     $ nfIO $ noDelay (Stream.ordered True)
      , bench "interleaved" $ nfIO $ noDelay (Stream.interleaved True)
      , bench "parallel"    $ nfIO $ noDelay (Stream.eager True)
      ]
    , bgroup "constantSlowConsumer"
      [ bench "async"       $ nfIO $ alwaysConstSlow id
      , bench "ordered"     $ nfIO $ alwaysConstSlow (Stream.ordered True)
      , bench "interleaved" $ nfIO $ alwaysConstSlow (Stream.interleaved True)
      , bench "parallel"    $ nfIO $ alwaysConstSlow (Stream.eager True)
      ]
   ,  bgroup "constantFastConsumer"
      [ bench "async"       $ nfIO $ alwaysConstFast id
      , bench "ordered"     $ nfIO $ alwaysConstFast (Stream.ordered True)
      , bench "interleaved" $ nfIO $ alwaysConstFast (Stream.interleaved True)
      , bench "parallel"    $ nfIO $ alwaysConstFast (Stream.eager True)
      ]
   ,  bgroup "variableSlowConsumer"
      [ bench "async"       $ nfIO $ alwaysVarSlow id
      , bench "ordered"     $ nfIO $ alwaysVarSlow (Stream.ordered True)
      , bench "interleaved" $ nfIO $ alwaysVarSlow (Stream.interleaved True)
      , bench "parallel"    $ nfIO $ alwaysVarSlow (Stream.eager True)
      ]
   ,  bgroup "variableFastConsumer"
      [ bench "async"       $ nfIO $ alwaysVarFast id
      , bench "ordered"     $ nfIO $ alwaysVarFast (Stream.ordered True)
      , bench "interleaved" $ nfIO $ alwaysVarFast (Stream.interleaved True)
      , bench "parallel"    $ nfIO $ alwaysVarFast (Stream.eager True)
      ]
   ,  bgroup "variableSometimesFastConsumer"
      [ bench "async"       $ nfIO $ runVarSometimesFast id
      , bench "ordered"     $ nfIO $ runVarSometimesFast (Stream.ordered True)
      , bench "interleaved" $ nfIO $ runVarSometimesFast (Stream.interleaved True)
      , bench "parallel"    $ nfIO $ runVarSometimesFast (Stream.eager True)
      ]
   ,  bgroup "variableFullOverlap"
      [ bench "async"       $ nfIO $ randomVar id
      , bench "ordered"     $ nfIO $ randomVar (Stream.ordered True)
      , bench "interleaved" $ nfIO $ randomVar (Stream.interleaved True)
      , bench "parallel"    $ nfIO $ randomVar (Stream.eager True)
      ]
   ]
