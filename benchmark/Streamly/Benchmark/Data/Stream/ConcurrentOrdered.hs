{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Stream.ConcurrentCommon (allBenchmarks)
import Streamly.Benchmark.Common (runWithCLIOpts, defaultStreamSize)

import qualified Streamly.Internal.Data.Stream.Prelude as Async

moduleName :: String
moduleName = "Data.Stream.ConcurrentOrdered"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    runWithCLIOpts
        defaultStreamSize
        (allBenchmarks moduleName False (Async.ordered True))
