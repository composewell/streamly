-- |
-- Module      : Scanl.Container
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Benchmarks for operations exported from Streamly.Internal.Data.Scanl.Container.
module Scanl.Container (benchmarks) where

import GHC.Types (SPEC(..))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.STRef (STRef)
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import Streamly.Internal.Data.Scanl (Scanl(..), Step, Tuple'Fused)
import Streamly.Internal.Data.Tuple.Strict (Tuple', Tuple3')
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Fusion.Plugin.Types
import Scanl.Type (benchIO, withStream, withPostscanl)
import Streamly.Benchmark.Common
import Test.Tasty.Bench

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

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

{-# ANN toSet (PermitPatternMatches [''Set,''Int]) #-}
{-# ANN toSet (PermitConstructions [''Set,''()]) #-}
{-# ANN toSet (PermitTypeClasses []) #-}
{-# NOINLINE toSet #-}
toSet :: Int -> Int -> IO ()
toSet n = withPostscanl n Scanl.toSet

{-# ANN toIntSet (PermitPatternMatches [''IntSet,''Int]) #-}
{-# ANN toIntSet (PermitConstructions [''()]) #-}
{-# ANN toIntSet (PermitTypeClasses []) #-}
{-# NOINLINE toIntSet #-}
toIntSet :: Int -> Int -> IO ()
toIntSet n = withPostscanl n Scanl.toIntSet

{-# ANN countDistinct (PermitPatternMatches [''Set,''Int]) #-}
{-# ANN countDistinct (PermitConstructions [''Set,''()]) #-}
{-# ANN countDistinct (PermitTypeClasses []) #-}
{-# NOINLINE countDistinct #-}
countDistinct :: Int -> Int -> IO ()
countDistinct n = withPostscanl n Scanl.countDistinct

{-# ANN countDistinctInt (PermitPatternMatches [''IntSet,''Int]) #-}
{-# ANN countDistinctInt (PermitConstructions [''()]) #-}
{-# ANN countDistinctInt (PermitTypeClasses []) #-}
{-# NOINLINE countDistinctInt #-}
countDistinctInt :: Int -> Int -> IO ()
countDistinctInt n = withPostscanl n Scanl.countDistinctInt

{-# ANN nub (PermitPatternMatches [''Set,''Maybe,''Int,''Tuple']) #-}
{-# ANN nub (PermitConstructions [''Int,''Set,''Maybe,''Tuple',''()]) #-}
{-# ANN nub (PermitTypeClasses []) #-}
{-# NOINLINE nub #-}
nub :: Int -> Int -> IO ()
nub n = withPostscanl n Scanl.nub

{-# ANN nubInt (PermitPatternMatches [''IntSet,''Bool,''Int]) #-}
{-# ANN nubInt (PermitConstructions [''()]) #-}
{-# ANN nubInt (PermitTypeClasses []) #-}
{-# NOINLINE nubInt #-}
nubInt :: Int -> Int -> IO ()
nubInt n = withPostscanl n Scanl.nubInt

-------------------------------------------------------------------------------
-- Demultiplexing
-------------------------------------------------------------------------------

{-# ANN demuxIOOneShot (PermitPatternMatches [''Map,''Set,''Maybe,''STRef,''Bool,''IO,''Int,''FL.Step,''Step,''Scanl,''Tuple'Fused,''Tuple']) #-}
{-# ANN demuxIOOneShot (PermitConstructions [''Int,''Map,''Maybe,''Scanl,''Set,''SrcLoc,''CallStack,''Step,''FL.Step,''STRef,''Tuple',''Tuple'Fused,''(),''Bool]) #-}
{-# ANN demuxIOOneShot (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxIOOneShot #-}
demuxIOOneShot :: Int -> Int -> IO ()
demuxIOOneShot len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.demuxIO (getKey 64) getScanl)

{-# ANN demuxIOSum (PermitPatternMatches [''Map,''STRef,''IO,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demuxIOSum (PermitConstructions [''Int,''Map,''SrcLoc,''CallStack,''Step,''Tuple',''()]) #-}
{-# ANN demuxIOSum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxIOSum #-}
demuxIOSum :: Int -> Int -> IO ()
demuxIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxIO (getKey 64) (const (pure (Just Scanl.sum))))

{-# ANN demuxSum (PermitPatternMatches [''Map,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demuxSum (PermitConstructions [''Int,''Map,''Step,''Tuple',''()]) #-}
{-# ANN demuxSum (PermitTypeClasses []) #-}
{-# NOINLINE demuxSum #-}
demuxSum :: Int -> Int -> IO ()
demuxSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demux (getKey 64) (const (pure (Just Scanl.sum))))

{-# ANN demuxGenericSum (PermitPatternMatches [''Map,''Maybe,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demuxGenericSum (PermitConstructions [''Int,''Map,''Maybe,''Step,''Tuple',''()]) #-}
{-# ANN demuxGenericSum (PermitTypeClasses []) #-}
{-# NOINLINE demuxGenericSum #-}
demuxGenericSum :: Int -> Int -> IO ()
demuxGenericSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGeneric (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# ANN demuxGenericIOSum (PermitPatternMatches [''Map,''Maybe,''STRef,''IO,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demuxGenericIOSum (PermitConstructions [''Int,''Map,''Maybe,''SrcLoc,''CallStack,''Step,''Tuple',''()]) #-}
{-# ANN demuxGenericIOSum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxGenericIOSum #-}
demuxGenericIOSum :: Int -> Int -> IO ()
demuxGenericIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGenericIO (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

-------------------------------------------------------------------------------
-- Classifying
-------------------------------------------------------------------------------

{-# ANN classifyLimitedSum (PermitPatternMatches [''Map,''Set,''STRef,''Int,''Tuple'Fused,''Tuple']) #-}
{-# ANN classifyLimitedSum (PermitConstructions [''Int,''Map,''Set,''()]) #-}
{-# ANN classifyLimitedSum (PermitTypeClasses []) #-}
{-# NOINLINE classifyLimitedSum #-}
classifyLimitedSum :: Int -> Int -> IO ()
classifyLimitedSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) (limitedSum 100))

{-# ANN classifyIOSum (PermitPatternMatches [''Map,''Set,''STRef,''Int,''Tuple']) #-}
{-# ANN classifyIOSum (PermitConstructions [''Int,''Map,''Set,''()]) #-}
{-# ANN classifyIOSum (PermitTypeClasses []) #-}
{-# NOINLINE classifyIOSum #-}
classifyIOSum :: Int -> Int -> IO ()
classifyIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) Scanl.sum)

{-# ANN classifySum (PermitPatternMatches [''Map,''Set,''Int,''Tuple']) #-}
{-# ANN classifySum (PermitConstructions [''Int,''Map,''Set,''()]) #-}
{-# ANN classifySum (PermitTypeClasses []) #-}
{-# NOINLINE classifySum #-}
classifySum :: Int -> Int -> IO ()
classifySum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classify (getKey 64) Scanl.sum)

{-# ANN classifyGenericSum (PermitPatternMatches [''Map,''Set,''Maybe,''Int,''Tuple',''Tuple3']) #-}
{-# ANN classifyGenericSum (PermitConstructions [''Int,''Map,''Maybe,''Set,''Tuple3',''(,),''()]) #-}
{-# ANN classifyGenericSum (PermitTypeClasses []) #-}
{-# NOINLINE classifyGenericSum #-}
classifyGenericSum :: Int -> Int -> IO ()
classifyGenericSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.classifyGeneric (getKey 64) Scanl.sum
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# ANN classifyGenericIOSum (PermitPatternMatches [''Map,''Set,''Maybe,''STRef,''Int,''Tuple',''Tuple3']) #-}
{-# ANN classifyGenericIOSum (PermitConstructions [''Int,''Map,''Maybe,''Set,''Tuple3',''(,),''()]) #-}
{-# ANN classifyGenericIOSum (PermitTypeClasses []) #-}
{-# NOINLINE classifyGenericIOSum #-}
classifyGenericIOSum :: Int -> Int -> IO ()
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
