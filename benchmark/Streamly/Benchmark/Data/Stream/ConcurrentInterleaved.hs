{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2022 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Stream.AsyncCommon (allBenchmarks)
import Streamly.Benchmark.Common (runWithCLIOpts, defaultStreamSize)

import qualified Streamly.Internal.Data.Stream.Concurrent as Async

moduleName :: String
moduleName = "Data.Stream.ConcurrentInterleaved"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    runWithCLIOpts
        defaultStreamSize (allBenchmarks moduleName Async.interleaved)
