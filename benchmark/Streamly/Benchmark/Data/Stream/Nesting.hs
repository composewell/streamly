-- |
-- Module      : Stream.Nesting
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Stream.Nesting (benchmarks) where

import Test.Tasty.Bench (Benchmark)
import Streamly.Benchmark.Common (SpaceComplexity)

import qualified Stream.Nesting.Basic as Basic
import qualified Stream.Nesting.LogicConcat as LogicConcat
import qualified Stream.Nesting.LogicUnfold as LogicUnfold

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
       Basic.benchmarks size
    ++ LogicConcat.benchmarks size
    ++ LogicUnfold.benchmarks size
