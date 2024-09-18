-- |
-- Module      : Streamly.Benchmark.Data.Ring
-- Copyright   : (c) 2022 Composewell
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Ring as Ring

import Test.Tasty.Bench
import Streamly.Benchmark.Common

import Prelude as P

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

eqArrayN :: (Int, Array.Array Int, Ring.Ring Int) -> IO Bool
eqArrayN (value, arr, ring) = Ring.eqArrayN ring arr value

eqArray :: (Array.Array Int, Ring.Ring Int) -> IO Bool
eqArray (arr, ring) = Ring.eqArray ring arr

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

o_1_space_serial :: Int -> Array.Array Int -> Ring.Ring Int -> [Benchmark]
o_1_space_serial value arr ring =
    [ bench "eqArrayN" $ nfIO $ eqArrayN (value, arr, ring)
    , bench "eqArray" $ nfIO $ eqArray (arr, ring)
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Ring"

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        let input = [1 .. value] :: [Int]
        let arr = Array.fromList input
        marr <- MutArray.fromList input
        let ring = maybe (error "cast failed") id (Ring.castMutArray marr)

        return (arr, ring)

    allBenchmarks (arr, ring) value =
        [ bgroup
              (o_1_space_prefix moduleName)
              (o_1_space_serial value arr ring)
        ]
