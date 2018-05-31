-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Gauge
import Streamly
import Streamly.Prelude as S
import System.Random (randomRIO)

value :: Int
value = 10000

{-# INLINE source #-}
source :: IsStream t => (Int, Int) -> t IO Int
source range = S.replicateM value $ do
    r <- randomRIO range
    when (r /= 0) $ liftIO $ threadDelay r
    return r

{-# INLINE run #-}
run :: IsStream t => (Int, Int) -> (Int, Int) -> (t IO Int -> SerialT IO Int) -> IO ()
run srange crange t = runStream $ do
    n <- t $ source srange
    d <- liftIO (randomRIO crange)
    when (d /= 0) $ liftIO $ threadDelay d
    return n

{-# INLINE alwaysConstSlowSerial #-}
alwaysConstSlowSerial :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstSlowSerial = run (0,0) (20,20)

{-# INLINE alwaysConstSlow #-}
alwaysConstSlow :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstSlow = run (10,10) (20,20)

{-# INLINE alwaysConstFast #-}
alwaysConstFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysConstFast = run (30,30) (20,20)

{-# INLINE alwaysVarSlow #-}
alwaysVarSlow :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysVarSlow = run (10,10) (10,30)

{-# INLINE alwaysVarFast #-}
alwaysVarFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
alwaysVarFast = run (30,30) (10,30)

-- XXX add variable producer tests as well

{-# INLINE runVarSometimesFast #-}
runVarSometimesFast :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
runVarSometimesFast = run (20,20) (10,30)

{-# INLINE randomVar #-}
randomVar :: IsStream t => (t IO Int -> SerialT IO Int) -> IO ()
randomVar = run (10,30) (10,30)

main :: IO ()
main = do
  defaultMain
    [
      bgroup "serialConstantSlowConsumer"
      [ bench "serially"    $ nfIO $ alwaysConstSlowSerial serially
      , bench "wSerially"   $ nfIO $ alwaysConstSlowSerial wSerially
      ]
    , bgroup "constantSlowConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysConstSlow aheadly
      , bench "asyncly"    $ nfIO $ alwaysConstSlow asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysConstSlow wAsyncly
      , bench "parallely"  $ nfIO $ alwaysConstSlow parallely
      ]
   ,  bgroup "constantFastConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysConstFast aheadly
      , bench "asyncly"    $ nfIO $ alwaysConstFast asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysConstFast wAsyncly
      , bench "parallely"  $ nfIO $ alwaysConstFast parallely
      ]
   ,  bgroup "variableSlowConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysVarSlow aheadly
      , bench "asyncly"    $ nfIO $ alwaysVarSlow asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysVarSlow wAsyncly
      , bench "parallely"  $ nfIO $ alwaysVarSlow parallely
      ]
   ,  bgroup "variableFastConsumer"
      [ bench "aheadly"    $ nfIO $ alwaysVarFast aheadly
      , bench "asyncly"    $ nfIO $ alwaysVarFast asyncly
      , bench "wAsyncly"   $ nfIO $ alwaysVarFast wAsyncly
      , bench "parallely"  $ nfIO $ alwaysVarFast parallely
      ]
   ,  bgroup "variableSometimesFastConsumer"
      [ bench "aheadly"    $ nfIO $ runVarSometimesFast aheadly
      , bench "asyncly"    $ nfIO $ runVarSometimesFast asyncly
      , bench "wAsyncly"   $ nfIO $ runVarSometimesFast wAsyncly
      , bench "parallely"  $ nfIO $ runVarSometimesFast parallely
      ]
   ,  bgroup "variableFullOverlap"
      [ bench "aheadly"    $ nfIO $ randomVar aheadly
      , bench "asyncly"    $ nfIO $ randomVar asyncly
      , bench "wAsyncly"   $ nfIO $ randomVar wAsyncly
      , bench "parallely"  $ nfIO $ randomVar parallely
      ]
   ]
