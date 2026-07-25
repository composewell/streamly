-- |
-- Module      : Streamly.Benchmark.Data.RingArray
-- Copyright   : (c) 2022 Composewell
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.RingArray as RingArray

import Control.DeepSeq (NFData)
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Unsafe.Coerce (UnsafeEquality)

import Prelude as P

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# ANN eqArrayN (PermitPatternMatches [''UnsafeEquality, ''IO]) #-}
{-# ANN eqArrayN (PermitConstructions [''Int, ''SrcLoc, ''CallStack]) #-}
{-# ANN eqArrayN (PermitTypeClasses [''IP]) #-}
{-# NOINLINE eqArrayN #-}
eqArrayN :: (Int, Array.Array Int, RingArray.RingArray Int) -> IO Bool
eqArrayN (value, arr, ring) = RingArray.eqArrayN ring arr value

{-# ANN eqArray (PermitPatternMatches [''UnsafeEquality, ''IO]) #-}
{-# ANN eqArray (PermitConstructions [''Int, ''SrcLoc, ''CallStack]) #-}
{-# ANN eqArray (PermitTypeClasses [''IP]) #-}
{-# NOINLINE eqArray #-}
eqArray :: (Array.Array Int, RingArray.RingArray Int) -> IO Bool
eqArray (arr, ring) = RingArray.eqArray ring arr

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

o_1_space_serial ::
       Int
    -> Array.Array Int
    -> RingArray.RingArray Int
    -> [(SpaceComplexity, Benchmark)]
o_1_space_serial value arr ring =
    [ (SpaceO_1, benchIO "eqArrayN (whole array)"
          $ eqArrayN (value, arr, ring))
    , (SpaceO_1, benchIO "eqArray" $ eqArray (arr, ring))
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.RingArray"

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks ::
       Int
    -> Array.Array Int
    -> RingArray.RingArray Int
    -> [(SpaceComplexity, Benchmark)]
benchmarks = o_1_space_serial

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        let input = [1 .. value] :: [Int]
        let arr = Array.fromList input
        marr <- MutArray.fromList input
        let ring = maybe (error "cast failed") id (RingArray.castMutArray marr)

        return (arr, ring)

    allBenchmarks (arr, ring) value =
        let allBenches = benchmarks value arr ring
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        ]
