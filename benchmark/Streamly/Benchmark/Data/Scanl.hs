-- |
-- Module      : Streamly.Benchmark.Data.Scanl
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity(..))
import System.Random (randomRIO)

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Scanl (Scanl(..))
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Prelude hiding (last, length, all, any, take, unzip, sequence_, filter)

import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE source #-}
source :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE benchScanWith #-}
benchScanWith :: Num a =>
    (Int -> a -> Stream IO a) -> Int -> String -> Scanl IO a b -> Benchmark
benchScanWith src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int)
            >>= Stream.fold FL.drain
                . Stream.postscanl f
                . src len
                . fromIntegral

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Scanl IO Int a -> Benchmark
benchWithPostscan = benchScanWith source

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Scanl"

instance NFData (MutArray a) where
    {-# INLINE rnf #-}
    rnf _ = ()

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ Stream.fold (FL.foldl' (\_ x -> rnf x) ()) xs

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "key-value"
            [
              benchWithPostscan value "demuxIO (1-shot) (64 buckets) [sum 100]"
                  $ Scanl.demuxIO (getKey 64) getScanl
            , benchWithPostscan value "demuxIO (64 buckets) [sum]"
                  $ Scanl.demuxIO (getKey 64) (const (pure (Just Scanl.sum)))
            , benchWithPostscan value "classifyIO (64 buckets) [sum 100]"
                  $ Scanl.classifyIO (getKey 64) (limitedSum 100)
            , benchWithPostscan value "classifyIO (64 buckets) [sum]"
                  $ Scanl.classifyIO (getKey 64) Scanl.sum
            ]
    ]

    where

    limitedSum n = Scanl.take n Scanl.sum

    getKey buckets = (`mod` buckets)

    afterDone action (Scanl.Scanl step i e f) = Scanl.Scanl step1 i e f
        where
        step1 x a = do
            res <- step x a
            case res of
                Scanl.Partial s1 -> pure $ Scanl.Partial s1
                Scanl.Done b -> action >> pure (Scanl.Done b)

    ref = unsafePerformIO $ newIORef Set.empty
    getScanl k = do
        set <- readIORef ref
        if Set.member k set
        then pure Nothing
        else pure
                 $ Just
                 $ afterDone (modifyIORef ref (Set.insert k)) (limitedSum 100)

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
