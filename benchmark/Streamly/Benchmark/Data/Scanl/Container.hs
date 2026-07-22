-- |
-- Module      : Scanl.Container
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Benchmarks for operations exported from
-- Streamly.Internal.Data.Scanl.Container.
module Scanl.Container (benchmarks) where

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
{-# ANN toSet (PermitConstructions [''Set,''(),''Int]) #-}
{-# ANN toSet (PermitTypeClasses []) #-}
{-# NOINLINE toSet #-}
toSet :: Int -> Int -> IO ()
toSet n = withPostscanl n Scanl.toSet

{-# ANN toIntSet (PermitPatternMatches [''IntSet,''Int]) #-}
{-# ANN toIntSet (PermitConstructions [''(),''IntSet]) #-}
{-# ANN toIntSet (PermitTypeClasses []) #-}
{-# NOINLINE toIntSet #-}
toIntSet :: Int -> Int -> IO ()
toIntSet n = withPostscanl n Scanl.toIntSet

{-# ANN countDistinct (PermitPatternMatches [''Set,''Int]) #-}
{-# ANN countDistinct (PermitConstructions [''Set,''(),''Int]) #-}
{-# ANN countDistinct (PermitTypeClasses []) #-}
{-# NOINLINE countDistinct #-}
countDistinct :: Int -> Int -> IO ()
countDistinct n = withPostscanl n Scanl.countDistinct

{-# ANN countDistinctInt (PermitPatternMatches [''IntSet,''Int]) #-}
{-# ANN countDistinctInt (PermitConstructions [''(),''IntSet]) #-}
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
{-# ANN nubInt (PermitConstructions [''(),''IntSet]) #-}
{-# ANN nubInt (PermitTypeClasses []) #-}
{-# NOINLINE nubInt #-}
nubInt :: Int -> Int -> IO ()
nubInt n = withPostscanl n Scanl.nubInt

-------------------------------------------------------------------------------
-- Demultiplexing
-------------------------------------------------------------------------------

{-# ANN demuxIO_Sum100 (PermitPatternMatches
    [''Map,''Set,''Maybe,''STRef,''Bool,''IO,''Int,''FL.Step,''Step
    ,''Scanl,''Tuple'Fused,''Tuple']) #-}
{-# ANN demuxIO_Sum100 (PermitConstructions
    [''Int,''Map,''Maybe,''Scanl,''Set,''SrcLoc,''CallStack,''Step
    ,''FL.Step,''STRef,''Tuple',''Tuple'Fused,''(),''Bool]) #-}
{-# ANN demuxIO_Sum100 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxIO_Sum100 #-}
demuxIO_Sum100 :: Int -> Int -> IO ()
demuxIO_Sum100 len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.demuxIO (getKey 64) getScanl)

{-# ANN demuxIO_Sum (PermitPatternMatches
    [''Map,''STRef,''IO,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demuxIO_Sum (PermitConstructions
    [''Int,''Map,''SrcLoc,''CallStack,''Step,''Tuple',''(),''Scanl
    ,''STRef,''FL.Step]) #-}
{-# ANN demuxIO_Sum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxIO_Sum #-}
demuxIO_Sum :: Int -> Int -> IO ()
demuxIO_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxIO (getKey 64) (const (pure (Just Scanl.sum))))

{-# ANN demux_Sum (PermitPatternMatches
    [''Map,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demux_Sum (PermitConstructions
    [''Int,''Map,''Step,''Tuple',''(),''Scanl,''FL.Step]) #-}
{-# ANN demux_Sum (PermitTypeClasses []) #-}
{-# NOINLINE demux_Sum #-}
demux_Sum :: Int -> Int -> IO ()
demux_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demux (getKey 64) (const (pure (Just Scanl.sum))))

{-# ANN demuxGeneric_Sum (PermitPatternMatches
    [''Map,''Maybe,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demuxGeneric_Sum (PermitConstructions
    [''Int,''Map,''Maybe,''Step,''Tuple',''(),''Scanl,''FL.Step]) #-}
{-# ANN demuxGeneric_Sum (PermitTypeClasses []) #-}
{-# NOINLINE demuxGeneric_Sum #-}
demuxGeneric_Sum :: Int -> Int -> IO ()
demuxGeneric_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGeneric (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# ANN demuxGenericIO_Sum (PermitPatternMatches
    [''Map,''Maybe,''STRef,''IO,''Int,''FL.Step,''Step,''Scanl,''Tuple']) #-}
{-# ANN demuxGenericIO_Sum (PermitConstructions
    [''Int,''Map,''Maybe,''SrcLoc,''CallStack,''Step,''Tuple',''(),''Scanl
    ,''STRef,''FL.Step]) #-}
{-# ANN demuxGenericIO_Sum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE demuxGenericIO_Sum #-}
demuxGenericIO_Sum :: Int -> Int -> IO ()
demuxGenericIO_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGenericIO (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

-------------------------------------------------------------------------------
-- Classifying
-------------------------------------------------------------------------------

{-# ANN classifyIO_Sum100 (PermitPatternMatches
    [''Map,''Set,''STRef,''Int,''Tuple'Fused,''Tuple']) #-}
{-# ANN classifyIO_Sum100 (PermitConstructions
    [''Int,''Map,''Set,''(),''Tuple'Fused,''Tuple',''STRef]) #-}
{-# ANN classifyIO_Sum100 (PermitTypeClasses []) #-}
{-# NOINLINE classifyIO_Sum100 #-}
classifyIO_Sum100 :: Int -> Int -> IO ()
classifyIO_Sum100 len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) (limitedSum 100))

{-# ANN classifyIO_Sum (PermitPatternMatches
    [''Map,''Set,''STRef,''Int,''Tuple']) #-}
{-# ANN classifyIO_Sum (PermitConstructions
    [''Int,''Map,''Set,''(),''Tuple',''STRef]) #-}
{-# ANN classifyIO_Sum (PermitTypeClasses []) #-}
{-# NOINLINE classifyIO_Sum #-}
classifyIO_Sum :: Int -> Int -> IO ()
classifyIO_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) Scanl.sum)

{-# ANN classify_Sum (PermitPatternMatches [''Map,''Set,''Int,''Tuple']) #-}
{-# ANN classify_Sum (PermitConstructions
    [''Int,''Map,''Set,''(),''Tuple']) #-}
{-# ANN classify_Sum (PermitTypeClasses []) #-}
{-# NOINLINE classify_Sum #-}
classify_Sum :: Int -> Int -> IO ()
classify_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classify (getKey 64) Scanl.sum)

{-# ANN classifyGeneric_Sum (PermitPatternMatches
    [''Map,''Set,''Maybe,''Int,''Tuple',''Tuple3']) #-}
{-# ANN classifyGeneric_Sum (PermitConstructions
    [''Int,''Map,''Maybe,''Set,''Tuple3',''(,),''(),''Tuple']) #-}
{-# ANN classifyGeneric_Sum (PermitTypeClasses []) #-}
{-# NOINLINE classifyGeneric_Sum #-}
classifyGeneric_Sum :: Int -> Int -> IO ()
classifyGeneric_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.classifyGeneric (getKey 64) Scanl.sum
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# ANN classifyGenericIO_Sum (PermitPatternMatches
    [''Map,''Set,''Maybe,''STRef,''Int,''Tuple',''Tuple3']) #-}
{-# ANN classifyGenericIO_Sum (PermitConstructions
    [''Int,''Map,''Maybe,''Set,''Tuple3',''(,),''(),''Tuple',''STRef]) #-}
{-# ANN classifyGenericIO_Sum (PermitTypeClasses []) #-}
{-# NOINLINE classifyGenericIO_Sum #-}
classifyGenericIO_Sum :: Int -> Int -> IO ()
classifyGenericIO_Sum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.classifyGenericIO (getKey 64) Scanl.sum
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    fmap (SpaceO_1,)
        [ benchIO "demuxIO_Sum100 (64 buckets)" demuxIO_Sum100 value
        , benchIO "demuxIO_Sum (64 buckets)" demuxIO_Sum value
        , benchIO "classifyIO_Sum100 (64 buckets)" classifyIO_Sum100 value
        , benchIO "classifyIO_Sum (64 buckets)" classifyIO_Sum value
        ]
    ++ fmap (HeapO_n,)
        [ benchIO "toSet" toSet value
        , benchIO "toIntSet" toIntSet value
        , benchIO "countDistinct" countDistinct value
        , benchIO "countDistinctInt" countDistinctInt value
        , benchIO "nub" nub value
        , benchIO "nubInt" nubInt value
        , benchIO "demux_Sum (64 buckets)" demux_Sum value
        , benchIO "demuxGeneric_Sum (64 buckets)" demuxGeneric_Sum value
        , benchIO "demuxGenericIO_Sum (64 buckets)" demuxGenericIO_Sum value
        , benchIO "classify_Sum (64 buckets)" classify_Sum value
        , benchIO "classifyGeneric_Sum (64 buckets)" classifyGeneric_Sum value
        , benchIO "classifyGenericIO_Sum (64 buckets)" classifyGenericIO_Sum
            value
        ]
