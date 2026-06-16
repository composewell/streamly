-- |
-- Module      : Stream.Parse
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Stream.Parse (benchmarks) where

import qualified Stream.Parse.Group as Group

import Test.Tasty.Bench (Benchmark)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle (BenchEnv)

benchmarks :: Int -> BenchEnv -> [(SpaceComplexity, Benchmark)]
benchmarks size _env =
    Group.benchmarks size
