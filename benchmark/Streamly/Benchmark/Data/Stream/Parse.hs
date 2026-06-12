-- |
-- Module      : Stream.Parse
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Stream.Parse (benchmarks) where

import qualified Stream.Parse.Group as Group
import qualified Stream.Parse.Split as Split
import qualified Stream.Parse.SplitChunks as SplitChunks

import Test.Tasty.Bench (Benchmark)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle (BenchEnv)

benchmarks :: Int -> BenchEnv -> [(SpaceComplexity, Benchmark)]
benchmarks size env =
    Group.benchmarks size
    ++ Split.benchmarks env
    ++ SplitChunks.benchmarks env
