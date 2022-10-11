{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Stream.AsyncCommon (allBenchmarks)
import Streamly.Benchmark.Common (runWithCLIOpts, defaultStreamSize)

import qualified Streamly.Internal.Data.Stream.Concurrent as Async

moduleName :: String
moduleName = "Data.Stream.AsyncEager"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    runWithCLIOpts defaultStreamSize (allBenchmarks moduleName Async.eagerEval)
