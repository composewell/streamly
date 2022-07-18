-- |
-- Module      : Data.Stream
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Streamly.Benchmark.Common.Handle (mkHandleBenchEnv)

import qualified Stream.Eliminate as Elimination
import qualified Stream.Exception as Exception
import qualified Stream.Generate as Generation
import qualified Stream.Lift as Lift
import qualified Stream.Reduce as Reduce
import qualified Stream.Transformation as Transformation

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

    allBenchmarks env size =
           Generation.benchmarks moduleName size
        ++ Elimination.benchmarks moduleName size
        ++ Exception.benchmarks moduleName env size
        ++ Lift.benchmarks moduleName size
        ++ Reduce.benchmarks moduleName size
        ++ Transformation.benchmarks moduleName size
