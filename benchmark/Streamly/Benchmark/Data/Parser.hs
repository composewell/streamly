#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  (
    main
  ) where

import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile, dropWhile, span)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle
import Streamly.Benchmark.Data.Parser.Alternative as Alternative
import Streamly.Benchmark.Data.Parser.Applicative as Applicative
import Streamly.Benchmark.Data.Parser.Monad as Monad
import Streamly.Benchmark.Data.Parser.Sequence as Sequence
import Streamly.Benchmark.Data.Parser.Producer as Producer
import Streamly.Benchmark.Data.Parser.Interleave as Interleave
import Streamly.Benchmark.Data.Parser.Groups as Groups

moduleName :: String
moduleName = "Data.Parser"

benchmarkList ::
       Int
    -> BenchEnv
    -> [Array.Array Int]
    -> [(SpaceComplexity, Benchmark)]
benchmarkList value env arrays =
    Alternative.benchmarks value
        ++ Applicative.benchmarks value
        ++ Monad.benchmarks value
        ++ Sequence.benchmarks value
        ++ Sequence.benchmarksFileIO env
        ++ Producer.benchmarks value arrays
        ++ Groups.benchmarks value
        ++ Interleave.benchmarks value

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    env <- mkHandleBenchEnv
    runWithCLIOptsEnv defaultStreamSize alloc (allBenchmarks env)

    where

    alloc value = Stream.fold Fold.toList $ Array.chunksOf 100 $ streamUnfoldrM value 0

    allBenchmarks env arrays value =
        let allBenches = benchmarkList value env arrays
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    -- let input = streamUnfoldrM value 1
    -- manyTill value input
    -- deintercalate value input
    -- deintercalate1 value input
    -- deintercalateAll value input
    -- sepByWords input
    -- sepByAllWords input
    -- sepBy1 input
    -- sepByWords1 input
    takeFramedByEsc_ value (sourceEscapedFrames value 1)
    return ()
#endif
