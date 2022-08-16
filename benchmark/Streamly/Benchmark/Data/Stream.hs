-- |
-- Module      : Data.Stream
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Streamly.Benchmark.Common.Handle (mkHandleBenchEnv)

import qualified Stream.Eliminate as Elimination
import qualified Stream.Exceptions as Exceptions
import qualified Stream.Expand as NestedStream
import qualified Stream.Generate as Generation
import qualified Stream.Lift as Lift
import qualified Stream.Reduce as NestedFold
#ifdef USE_PRELUDE
import qualified Stream.Split as Split
#endif
import qualified Stream.Transform as Transformation

import Streamly.Benchmark.Common

moduleName :: String
moduleName = "Data.Stream"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env size = Prelude.concat
        [ Generation.benchmarks moduleName size
        , Elimination.benchmarks moduleName size
        , Exceptions.benchmarks moduleName env size
#ifdef USE_PRELUDE
        , Split.benchmarks moduleName env
#endif
        , Transformation.benchmarks moduleName size
        , NestedFold.benchmarks moduleName size
        , Lift.benchmarks moduleName size
        , NestedStream.benchmarks moduleName size
        ]