-- |
-- Module      : Scanl.Container
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- Benchmarks for operations exported from Streamly.Internal.Data.Scanl.Container.
module Scanl.Container (benchmarks) where

import Data.IntSet (IntSet)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.STRef (STRef)
import Streamly.Internal.Data.Scanl (Scanl(..), Tuple'Fused)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple', Tuple3')
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Fusion.Plugin.Types
import Streamly.Benchmark.Common
import Test.Tasty.Bench

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE source #-}
source :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream len f = randomRIO (1, 1 :: Int) >>= f . source len

{-# INLINE withPostscanl #-}
withPostscanl :: Int -> Scanl IO Int b -> IO ()
withPostscanl n s = withStream n $ Stream.fold FL.drain . Stream.postscanl s

{-# INLINE benchIO #-}
benchIO :: String -> (Int -> IO ()) -> Int -> Benchmark
benchIO name f value = bench name $ nfIO $ f value

{-# INLINE getKey #-}
getKey :: Int -> Int -> Int
getKey buckets = (`mod` buckets)

{-# INLINE limitedSum #-}
limitedSum :: Int -> Scanl IO Int Int
limitedSum n = Scanl.take n Scanl.sum

{-# NOINLINE ref #-}
ref :: IORef (Set.Set Int)
ref = unsafePerformIO $ newIORef Set.empty

{-# INLINE getScanl #-}
getScanl :: Int -> IO (Maybe (Scanl IO Int Int))
getScanl k = do
    set <- readIORef ref
    if Set.member k set
    then pure Nothing
    else do
        modifyIORef ref (Set.insert k)
        pure $ Just (limitedSum 100)

-------------------------------------------------------------------------------
-- Set operations
-------------------------------------------------------------------------------

{-# ANN toSet (PermitTypes [''Int,''Set,''STRef,''(,)]) #-}
{-# ANN toSet (PermitTypeClasses []) #-}
{-# ANN toSet (MaxCoreSize 1000) #-}
{-# NOINLINE toSet #-}
toSet :: Int -> IO ()
toSet n = withPostscanl n Scanl.toSet

{-# ANN toIntSet (PermitTypes [''Int,''STRef,''IntSet,''(,)]) #-}
{-# ANN toIntSet (PermitTypeClasses []) #-}
{-# ANN toIntSet (MaxCoreSize 1000) #-}
{-# NOINLINE toIntSet #-}
toIntSet :: Int -> IO ()
toIntSet n = withPostscanl n Scanl.toIntSet

{-# ANN countDistinct (PermitTypes [''Int,''STRef,''Set,''(,)]) #-}
{-# ANN countDistinct (PermitTypeClasses []) #-}
{-# ANN countDistinct (MaxCoreSize 1000) #-}
{-# NOINLINE countDistinct #-}
countDistinct :: Int -> IO ()
countDistinct n = withPostscanl n Scanl.countDistinct

{-# ANN countDistinctInt (PermitTypes [''Int,''STRef,''IntSet,''(,)]) #-}
{-# ANN countDistinctInt (PermitTypeClasses []) #-}
{-# ANN countDistinctInt (MaxCoreSize 1000) #-}
{-# NOINLINE countDistinctInt #-}
countDistinctInt :: Int -> IO ()
countDistinctInt n = withPostscanl n Scanl.countDistinctInt

{-# ANN nub (PermitTypes [''Int,''STRef,''Set,''Maybe,''Tuple',''(,)]) #-}
{-# ANN nub (PermitTypeClasses []) #-}
{-# ANN nub (MaxCoreSize 1000) #-}
{-# NOINLINE nub #-}
nub :: Int -> IO ()
nub n = withPostscanl n Scanl.nub

{-# ANN nubInt (PermitTypes [''Int,''STRef,''IntSet,''(,)]) #-}
{-# ANN nubInt (PermitTypeClasses []) #-}
{-# ANN nubInt (MaxCoreSize 1000) #-}
{-# NOINLINE nubInt #-}
nubInt :: Int -> IO ()
nubInt n = withPostscanl n Scanl.nubInt

-------------------------------------------------------------------------------
-- Demultiplexing
-------------------------------------------------------------------------------

{-# ANN demuxIOOneShot (PermitTypes [''Int,''STRef,''IORef,''Map,''Scanl,
   ''FL.Step, ''Scanl.Step,''(,)]) #-}
{-# ANN demuxIOOneShot (PermitTypeClasses []) #-}
{-# ANN demuxIOOneShot (MaxCoreSize 2000) #-}
{-# NOINLINE demuxIOOneShot #-}
demuxIOOneShot :: Int -> IO ()
demuxIOOneShot len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.demuxIO (getKey 64) getScanl)

{-# ANN demuxIOSum (PermitTypes
   [''Int,''STRef,''IORef,''Map,''Scanl,''FL.Step,''Scanl.Step,''(,)]) #-}
{-# ANN demuxIOSum (PermitTypeClasses []) #-}
{-# ANN demuxIOSum (MaxCoreSize 2000) #-}
{-# NOINLINE demuxIOSum #-}
demuxIOSum :: Int -> IO ()
demuxIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxIO (getKey 64) (const (pure (Just Scanl.sum))))

{-# ANN demuxSum (PermitTypes [''Int,''STRef,''IO,''Map,''Scanl,''FL.Step,''Scanl.Step,''(,)]) #-}
{-# ANN demuxSum (PermitTypeClasses []) #-}
{-# ANN demuxSum (MaxCoreSize 2000) #-}
{-# NOINLINE demuxSum #-}
demuxSum :: Int -> IO ()
demuxSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demux (getKey 64) (const (pure (Just Scanl.sum))))

{-# ANN demuxGenericSum (PermitTypes
   [''Int,''STRef,''Map,''Maybe,''Scanl,''FL.Step,''Scanl.Step,''(,)]) #-}
{-# ANN demuxGenericSum (PermitTypeClasses []) #-}
{-# ANN demuxGenericSum (MaxCoreSize 2000) #-}
{-# NOINLINE demuxGenericSum #-}
demuxGenericSum :: Int -> IO ()
demuxGenericSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGeneric (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# ANN demuxGenericIOSum (PermitTypes
   [''Int,''STRef,''Map,''Maybe,''Scanl,''FL.Step,''Scanl.Step,''(,)]) #-}
{-# ANN demuxGenericIOSum (PermitTypeClasses []) #-}
{-# ANN demuxGenericIOSum (MaxCoreSize 2000) #-}
{-# NOINLINE demuxGenericIOSum #-}
demuxGenericIOSum :: Int -> IO ()
demuxGenericIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGenericIO (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

-------------------------------------------------------------------------------
-- Classifying
-------------------------------------------------------------------------------

{-# ANN classifyLimitedSum (PermitTypes
   [''Int,''STRef,''IORef,''Map,''Set,''Tuple',''Tuple'Fused,''(,)]) #-}
{-# ANN classifyLimitedSum (PermitTypeClasses []) #-}
{-# ANN classifyLimitedSum (MaxCoreSize 2000) #-}
{-# NOINLINE classifyLimitedSum #-}
classifyLimitedSum :: Int -> IO ()
classifyLimitedSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) (limitedSum 100))

{-# ANN classifyIOSum (PermitTypes [''Int,''STRef,''IORef,''Map,''Tuple',''(,)]) #-}
{-# ANN classifyIOSum (PermitTypeClasses []) #-}
{-# ANN classifyIOSum (MaxCoreSize 1000) #-}
{-# NOINLINE classifyIOSum #-}
classifyIOSum :: Int -> IO ()
classifyIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) Scanl.sum)

{-# ANN classifySum (PermitTypes [''Int,''STRef,''Map,''Tuple',''(,)]) #-}
{-# ANN classifySum (PermitTypeClasses []) #-}
{-# ANN classifySum (MaxCoreSize 1000) #-}
{-# NOINLINE classifySum #-}
classifySum :: Int -> IO ()
classifySum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classify (getKey 64) Scanl.sum)

{-# ANN classifyGenericSum (PermitTypes [''Int,''STRef,''Map,''Maybe,''Set,''Tuple',''Tuple3',''(,)]) #-}
{-# ANN classifyGenericSum (PermitTypeClasses []) #-}
{-# ANN classifyGenericSum (MaxCoreSize 1500) #-}
{-# NOINLINE classifyGenericSum #-}
classifyGenericSum :: Int -> IO ()
classifyGenericSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.classifyGeneric (getKey 64) Scanl.sum
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# ANN classifyGenericIOSum (PermitTypes [''Int,''STRef,''Map,''Maybe,''Set,''Tuple',''Tuple3',''(,)]) #-}
{-# ANN classifyGenericIOSum (PermitTypeClasses []) #-}
{-# ANN classifyGenericIOSum (MaxCoreSize 1500) #-}
{-# NOINLINE classifyGenericIOSum #-}
classifyGenericIOSum :: Int -> IO ()
classifyGenericIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.classifyGenericIO (getKey 64) Scanl.sum
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    fmap (SpaceO_1,)
        [ benchIO "demuxIO (1-shot) (64 buckets) [sum 100]" demuxIOOneShot value
        , benchIO "demuxIO (64 buckets) [sum]" demuxIOSum value
        , benchIO "classifyIO (64 buckets) [sum 100]" classifyLimitedSum value
        , benchIO "classifyIO (64 buckets) [sum]" classifyIOSum value
        ]
    ++ fmap (HeapO_n,)
        [ benchIO "toSet" toSet value
        , benchIO "toIntSet" toIntSet value
        , benchIO "countDistinct" countDistinct value
        , benchIO "countDistinctInt" countDistinctInt value
        , benchIO "nub" nub value
        , benchIO "nubInt" nubInt value
        , benchIO "demux (64 buckets) [sum]" demuxSum value
        , benchIO "demuxGeneric (64 buckets) [sum]" demuxGenericSum value
        , benchIO "demuxGenericIO (64 buckets) [sum]" demuxGenericIOSum value
        , benchIO "classify (64 buckets) [sum]" classifySum value
        , benchIO "classifyGeneric (64 buckets) [sum]" classifyGenericSum value
        , benchIO "classifyGenericIO (64 buckets) [sum]" classifyGenericIOSum value
        ]
