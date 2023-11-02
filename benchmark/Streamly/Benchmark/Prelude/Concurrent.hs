{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Control.Concurrent
import Control.Monad (when, replicateM)
import Streamly.Prelude
    ( IsStream, SerialT, serial, async, fromAsync, ahead, fromAhead, wAsync
    , fromWAsync, parallel, fromParallel
    )

import Test.Tasty.Bench
import qualified Streamly.Prelude as S

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- | Run @tcount@ number of actions concurrently using the given concurrency
-- style. Each thread produces a single output after a delay of @d@
-- microseconds.
--
{-# INLINE append #-}
append :: IsStream t
    => Int -> Int -> Int -> (t IO Int -> SerialT IO Int) -> IO ()
append buflen tcount d t =
    let work = (\i -> when (d /= 0) (threadDelay d) >> return i)
    in S.drain
        $ t
        $ S.maxBuffer buflen
        $ S.maxThreads (-1)
        $ S.fromFoldableM $ fmap work [1..tcount]

-- | Run @threads@ concurrently, each producing streams of @elems@ elements
-- with a delay of @d@ microseconds between successive elements, and merge
-- their outputs in a single output stream. The individual streams are produced
-- serially but merged using the provided concurrency style.
--
{-# INLINE concated #-}
concated
    :: Int
    -> Int
    -> Int
    -> Int
    -> (forall a. SerialT IO a -> SerialT IO a -> SerialT IO a)
    -> IO ()
concated buflen threads d elems t =
    let work = \i -> S.replicateM i (when (d /= 0) (threadDelay d) >> return i)
    in S.drain
        $ S.adapt
        $ S.maxThreads (-1)
        $ S.maxBuffer buflen
        $ S.concatMapWith t work
        $ S.replicate threads elems

appendGroup :: Int -> Int -> Int -> [Benchmark]
appendGroup buflen threads usec =
    [ -- bench "serial"   $ nfIO $ append buflen threads delay fromSerial
      bench "ahead"    $ nfIO $ append buflen threads usec fromAhead
    , bench "async"    $ nfIO $ append buflen threads usec fromAsync
    , bench "wAsync"   $ nfIO $ append buflen threads usec fromWAsync
    , bench "parallel" $ nfIO $ append buflen threads usec fromParallel
    ]

concatGroup :: Int -> Int -> Int -> Int -> [Benchmark]
concatGroup buflen threads usec n =
    [ bench "serial"   $ nfIO $ concated buflen threads usec n serial
    , bench "ahead"    $ nfIO $ concated buflen threads usec n ahead
    , bench "async"    $ nfIO $ concated buflen threads usec n async
    , bench "wAsync"   $ nfIO $ concated buflen threads usec n wAsync
    , bench "parallel" $ nfIO $ concated buflen threads usec n parallel
    ]

main :: IO ()
main =
#ifdef MIN_VERSION_gauge
  defaultMainWith (defaultConfig
    { timeLimit = Just 0
    , minSamples = Just 1
    , minDuration = 0
    , includeFirstIter = True
    , quickMode = True
    })
#else
    defaultMain
#endif

    [ -- bgroup "append/buf-1-threads-10k-0sec"  (appendGroup 1 10000 0)
    -- , bgroup "append/buf-100-threads-100k-0sec"  (appendGroup 100 100000 0)
      bgroup "stream1x10k/buf10k-threads10k-5sec"  (appendGroup 10000 10000 5000000)
    --  bgroup "concat/buf-1-threads-100k-count-1" (concatGroup 1 100000 0 1)
    --  bgroup "concat/buf-1-threads-1-count-10m" (concatGroup 1 1 0 10000000)
    , bgroup "streams100x500k/buf100-threads100"  (concatGroup 100 100 0 500000)

    , bench "forkIO/threads10k-5sec" $
        let delay = threadDelay 5000000
            count = 10000 :: Int
            list = [1..count]
            work i = delay >> return i
        in nfIO $ do
            ref <- newEmptyMVar
            mapM_ (\i -> forkIO $ work i >>=
                   \j -> putMVar ref j) list
            replicateM 10000 (takeMVar ref)
   ]
