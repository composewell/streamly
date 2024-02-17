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
import Streamly.Data.Stream.Prelude (Config)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream

import Test.Tasty (localOption)
import Test.Tasty.Bench

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- | Run @tcount@ number of actions concurrently using the given concurrency
-- style. Each thread produces a single output after a delay of @d@
-- microseconds.
--
{-# INLINE append #-}
append :: Int -> Int -> Int -> (Config -> Config) -> IO ()
append buflen tcount d modifier =
    let work i = when (d /= 0) (threadDelay d) >> return i
        cfg =
              modifier
            . Stream.maxBuffer buflen
            . Stream.maxThreads (-1)
    in Stream.fold Fold.drain
        $ Stream.parMapM cfg work
        $ Stream.fromList [1..tcount]

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
    -> (Config -> Config)
    -> IO ()
concated buflen threads d elems modifier =
    let work i =
            Stream.replicateM i (when (d /= 0) (threadDelay d) >> return i)
        cfg =
              modifier
            . Stream.maxBuffer buflen
            . Stream.maxThreads (-1)
    in Stream.fold Fold.drain
        $ Stream.parConcatMap cfg work
        $ Stream.replicate threads elems

appendGroup :: Int -> Int -> Int -> [Benchmark]
appendGroup buflen threads usec =
    [ -- bench "serial"   $ nfIO $ append buflen threads delay fromSerial
      bench "async"
        $ nfIO $ append buflen threads usec id
    , bench "ordered"
        $ nfIO $ append buflen threads usec (Stream.ordered True)
    , bench "interleaved"
        $ nfIO $ append buflen threads usec (Stream.interleaved True)
    , bench "parallel"
        $ nfIO $ append buflen threads usec (Stream.eager True)
    ]

concatGroup :: Int -> Int -> Int -> Int -> [Benchmark]
concatGroup buflen threads usec n =
    [ -- bench "serial"   $ nfIO $ concated buflen threads usec n serial
      bench "async"
        $ nfIO $ concated buflen threads usec n id
    , bench "ordered"
        $ nfIO $ concated buflen threads usec n (Stream.ordered True)
    , bench "interleaved"
        $ nfIO $ concated buflen threads usec n (Stream.interleaved True)
    , bench "parallel"
        $ nfIO $ concated buflen threads usec n (Stream.eager True)
    ]

main :: IO ()
main =
    defaultMain $ map (localOption (RelStDev 100000))
    [ -- bgroup "append/buf-1-threads-10k-0sec"  (appendGroup 1 10000 0)
    -- , bgroup "append/buf-100-threads-100k-0sec"  (appendGroup 100 100000 0)
      bgroup "streamly/threads10k-delay5s-singleton(buf10k)"  (appendGroup 10000 10000 5000000)
    --  bgroup "concat/buf-1-threads-100k-count-1" (concatGroup 1 100000 0 1)
    --  bgroup "concat/buf-1-threads-1-count-10m" (concatGroup 1 1 0 10000000)
    , bgroup "streamly/threads100-delay0s-stream500k(buf100)"  (concatGroup 100 100 0 500000)

    , bench "forkIO/threads10k-delay5s-singleton" $
        let delay = threadDelay 5000000
            count = 10000 :: Int
            list = [1..count]
            work i = delay >> return i
        in nfIO $ do
            ref <- newEmptyMVar
            mapM_ (\i -> forkIO (work i >>= \j -> putMVar ref j)) list
            replicateM 10000 (takeMVar ref)
   ]
