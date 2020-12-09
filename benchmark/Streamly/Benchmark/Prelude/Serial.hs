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
import qualified Serial.Nested as Nested
import qualified Serial.Split as Split
import qualified Serial.Transformation1 as Transformation1
import qualified Serial.Transformation2 as Transformation2
import qualified Serial.Transformation3 as Transformation3

import Gauge hiding (env)
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
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    env <- mkHandleBenchEnv
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value env)
    where

    allBenchmarks size env = Prelude.concat
        [ Generation.benchmarks moduleName size
        , Elimination.benchmarks moduleName size
        , Exceptions.benchmarks moduleName env
        , Split.benchmarks moduleName env
        , Transformation1.benchmarks moduleName size
        , Transformation2.benchmarks moduleName size
        , Transformation3.benchmarks moduleName size
        , Nested.benchmarks moduleName size
        ]
