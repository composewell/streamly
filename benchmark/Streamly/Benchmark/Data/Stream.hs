-- |
-- Module      : Data.Stream
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

module Main (main) where

import Streamly.Benchmark.Common.Handle (mkHandleBenchEnv)

import qualified Stream.Eliminate as Elimination
import qualified Stream.Exceptions as Exceptions
import qualified Stream.Expand as NestedStream
import qualified Stream.Generate as Generation
import qualified Stream.Lift as Lift
import qualified Stream.Reduce as NestedFold
import qualified Stream.Split as Split
import qualified Stream.Transform as Transformation

import Streamly.Benchmark.Common

#ifdef FUSION_CHECK
import Data.Function ((&))
import Streamly.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Data.Foldable as Foldable
import Stream.Common
#endif

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
#ifndef FUSION_CHECK
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env size = Prelude.concat
        [ Generation.benchmarks moduleName size
        , Elimination.benchmarks moduleName size
        , Exceptions.benchmarks moduleName env size
        , Split.benchmarks moduleName env
        , Transformation.benchmarks moduleName size
        , NestedFold.benchmarks moduleName size
        , Lift.benchmarks moduleName size
        , NestedStream.benchmarks moduleName size
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    let input = sourceUnfoldrM value 1

    -- Stream.foldr (:) [] input >>= print . Prelude.length
    -- Stream.fold (Fold.foldr' (:) []) input >>= print . Prelude.length

    -- Stream.toList input >>= print . Prelude.length
    -- Stream.fold Fold.toList input >>= print . Prelude.length

    -- We can automatically assert that these generate identical core

    -- Stream.drain input >>= print
    -- Stream.fold Fold.drain input >>= print

    -- Stream.last input >>= print
    -- Stream.fold Fold.latest input >>= print

    -- Stream.length input >>= print
    -- Stream.fold Fold.length input >>= print

    -- Stream.maximum input >>= print
    -- Stream.fold Fold.maximum input >>= print

    -- Stream.head input >>= print
    -- Stream.fold Fold.one input >>= print

    -- composeN 4 (Stream.scanl' (+) 0) input
    composeN 4 (Stream.scan (Fold.foldl' (+) 0)) input
    return ()
#endif
