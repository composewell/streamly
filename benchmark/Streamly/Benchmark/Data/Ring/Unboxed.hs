-- |
-- Module      : Streamly.Benchmark.Data.Ring.Unboxed
-- Copyright   : (c) 2022 Composewell
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (void)
import GHC.Ptr (Ptr(..))
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Ring as Ring
import qualified Data.Foldable as P

import Test.Tasty.Bench
import Streamly.Benchmark.Common

import Prelude as P

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

unsafeEqArrayN :: (Int, Array.Array Int, (Ring.Ring Int, Ptr Int)) -> Bool
unsafeEqArrayN (value, arr, (ring, rh)) = Ring.unsafeEqArrayN ring rh arr value

unsafeEqArray :: (Array.Array Int, (Ring.Ring Int, Ptr Int)) -> Bool
unsafeEqArray (arr, (ring, rh)) = Ring.unsafeEqArray ring rh arr

-------------------------------------------------------------------------------
-- Benchmark groups
-------------------------------------------------------------------------------

o_1_space_serial ::
       Int -> Array.Array Int -> (Ring.Ring Int, Ptr Int) -> [Benchmark]
o_1_space_serial value arr (ring, rh) =
    [ bench "unsafeEqArrayN" $ nf unsafeEqArrayN (value, arr, (ring, rh))
    , bench "unsafeEqArray" $ nf unsafeEqArray (arr, (ring, rh))
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Ring.Unboxed"

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        let input = [1 .. value] :: [Int]
        let arr = Array.fromList input
        (ring, rh) <- Ring.new value
        void $ P.foldlM (Ring.unsafeInsert ring) rh input
        return (arr, (ring, rh))

    allBenchmarks (arr, (ring, rh)) value =
        [ bgroup
              (o_1_space_prefix moduleName)
              (o_1_space_serial value arr (ring, rh))
        ]
