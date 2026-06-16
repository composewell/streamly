--
-- Module      : CrossModule
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Driver for benchmarks that combine operations from multiple library modules,
-- primarily to test fusion and performance across module boundaries (e.g. file
-- I/O fused with stream folds, chunking and splitting operations).

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

import qualified CrossModule.FileSystem as FileSystem
import qualified CrossModule.Split as Split
import qualified CrossModule.SplitChunks as SplitChunks

moduleName :: String
moduleName = "CrossModule"

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    defaultMain (allBenchmarks env)

    where

    allBenchmarks env =
        let allBenches =
                   FileSystem.benchmarks env
                ++ Split.benchmarks env
                ++ SplitChunks.benchmarks env
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        ]
