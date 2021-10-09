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

import qualified Serial.Elimination as Elimination
import qualified Serial.Exceptions as Exceptions
import qualified Serial.Generation as Generation
import qualified Serial.NestedStream as NestedStream
import qualified Serial.Split as Split
import qualified Serial.Transformation1 as Transformation1
import qualified Serial.NestedFold as NestedFold
import qualified Serial.Lift as Lift

import Streamly.Benchmark.Common

moduleName :: String
moduleName = "Prelude.Serial"

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
        , Split.benchmarks moduleName env
        , Transformation1.benchmarks moduleName size
        , NestedFold.benchmarks moduleName size
        , Lift.benchmarks moduleName size
        , NestedStream.benchmarks moduleName size
        ]
