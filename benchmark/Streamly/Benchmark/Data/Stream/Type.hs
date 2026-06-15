-- |
-- Module      : Stream.Type
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Stream.Type
    ( benchmarks
    , boundedInts
    , infiniteInts
    , boundedIntsUnfold
    , checkStream
    , checkPair
    , result
    , withPureStream
    , withStream
    , withDrain
    , withDrainPure
    , withRandomInt
    , withRandomIntIO
    , benchIO
    ) where

import Test.Tasty.Bench (Benchmark)
import Streamly.Benchmark.Common (SpaceComplexity)

import Stream.Type.Basic
    ( benchIO
    , withRandomIntIO
    , withDrain
    , withDrainPure
    , withRandomInt
    , withStream
    , withPureStream
    )
import Stream.Type.Logic
    ( boundedInts
    , infiniteInts
    , boundedIntsUnfold
    , checkStream
    , checkPair
    , result
    )

import qualified Stream.Type.Basic as Basic
import qualified Stream.Type.MultiStream as MultiStream
import qualified Stream.Type.Nested as Nested
import qualified Stream.Type.Logic as Logic

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
       Basic.benchmarks size
    ++ MultiStream.benchmarks size
    ++ Nested.benchmarks size
    ++ Logic.benchmarks size
