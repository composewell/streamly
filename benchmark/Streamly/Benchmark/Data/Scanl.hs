-- |
-- Module      : Streamly.Benchmark.Data.Scanl
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Tasty.Bench (bgroup)
import Streamly.Benchmark.Common

import qualified Scanl.Type as Type
import qualified Scanl.Combinators as Combinators
import qualified Scanl.Container as Container
import qualified Scanl.Window as Window

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Scanl"

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        let allBenches = Type.benchmarks value
                      ++ Combinators.benchmarks value
                      ++ Container.benchmarks value
                      ++ Window.benchmarks value
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_space = get SpaceO_n
            o_n_heap = get HeapO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_space_prefix moduleName) o_n_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        ]
