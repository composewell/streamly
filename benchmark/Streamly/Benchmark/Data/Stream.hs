-- |
-- Module      : Serial
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Streamly.Benchmark.Common.Handle (mkHandleBenchEnv)

import qualified Stream.Elimination as Elimination
import qualified Stream.Generate as Generation

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

    allBenchmarks _env size =
           Generation.benchmarks moduleName size
        ++ Elimination.benchmarks moduleName size
