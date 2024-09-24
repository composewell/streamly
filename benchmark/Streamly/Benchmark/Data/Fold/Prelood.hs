-- |
-- Module      : Streamly.Benchmark.Data.Fold.Prelude
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

module Main (main) where

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import System.Random (randomRIO)

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.IsMap.HashMap ()
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Data.Fold.Prelude as Fold
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE source #-}
source :: Monad m => Int -> Int -> Stream m Int
source = sourceUnfoldrM

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ randomRIO (1,1) >>= f . source value

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# INLINE demuxToHashMap  #-}
demuxToHashMap :: (Monad m, Hashable k
#if __GLASGOW_HASKELL__ == 810
    , Eq k
#endif
    ) =>
    (a -> k) -> (a -> m (Fold m a b)) -> Stream m a -> m (HashMap k b)
demuxToHashMap f g = Stream.fold (Fold.demuxToContainer f g)

{-# INLINE demuxToHashMapIO  #-}
demuxToHashMapIO :: (MonadIO m, Hashable k
#if __GLASGOW_HASKELL__ == 810
    , Eq k
#endif
    ) =>
    (a -> k) -> (a -> m (Fold m a b)) -> Stream m a -> m (HashMap k b)
demuxToHashMapIO f g = Stream.fold (Fold.demuxToContainerIO f g)

{-# INLINE toHashMapIO #-}
toHashMapIO :: (MonadIO m, Num a, Hashable k
#if __GLASGOW_HASKELL__ == 810
    , Eq k
#endif
    ) =>
    (a -> k) -> Stream m a -> m (HashMap k a)
toHashMapIO f = Stream.fold (Fold.toHashMapIO f Fold.sum)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Fold.Prelude"

instance NFData (MutArray a) where
    {-# INLINE rnf #-}
    rnf _ = ()

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ Stream.fold (Fold.foldl' (\_ x -> rnf x) ()) xs

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "key-value"
            [
              benchIOSink value "demuxToHashMap (64 buckets) [sum, length]"
                $ demuxToHashMap (getKey 64) (getFold . getKey 64)
            , benchIOSink value "demuxToHashMapIO (64 buckets) [sum, length]"
                $ demuxToHashMapIO (getKey 64) (getFold . getKey 64)

            -- classify: mutable cells
            , benchIOSink value "toHashMapIO (single bucket) sum"
                $ toHashMapIO (getKey 1)
            , benchIOSink value "toHashMapIO (64 buckets) sum"
                $ toHashMapIO (getKey 64)
            , benchIOSink value "toHashMapIO (max buckets) sum"
                $ toHashMapIO (getKey value)
            ]
    ]

    where

    getKey buckets = (`mod` buckets)

    getFold k =
        return $ case k of
            0 -> Fold.sum
            1 -> Fold.length
            _ -> Fold.length

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks _env value =
        [ bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    let input = source value 1
    let getKey buckets = (`mod` buckets)
    let getFold k =
            return $ case k of
                0 -> Fold.sum
                1 -> Fold.length
                _ -> Fold.length

    -- demuxToMap (getKey 64) (getFold . getKey 64) input
    toIntMapIO (getKey 64) input
    return ()
#endif
