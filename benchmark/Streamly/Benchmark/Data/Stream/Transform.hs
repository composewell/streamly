-- |
-- Module      : Stream.Transform
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Stream.Transform (benchmarks) where

import qualified Stream.Transform.Basic as Basic
import qualified Stream.Transform.Composed as Composed

import Test.Tasty.Bench (Benchmark)
import Streamly.Benchmark.Common

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size = Basic.benchmarks size ++ Composed.benchmarks size
