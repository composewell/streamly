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

import Test.Tasty.Bench
import Streamly.Benchmark.Common

import Prelude as P

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

eqArrayN :: (Int, Array.Array Int, RingArray.RingArray Int) -> IO Bool
eqArrayN (value, arr, ring) = RingArray.eqArrayN ring arr value

eqArray :: (Array.Array Int, RingArray.RingArray Int) -> IO Bool
eqArray (arr, ring) = RingArray.eqArray ring arr

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

o_1_space_serial :: Int -> Array.Array Int -> RingArray.RingArray Int -> [Benchmark]
o_1_space_serial value arr ring =
    [ bench "eqArrayN" $ nfIO $ eqArrayN (value, arr, ring)
    , bench "eqArray" $ nfIO $ eqArray (arr, ring)
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.RingArray"

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
        [ bgroup
              (o_1_space_prefix moduleName)
              (o_1_space_serial value arr ring)
        ]
