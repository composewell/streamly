{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Stream.AsyncCommon (allBenchmarks, interleaveBenchmarks)
import Streamly.Benchmark.Common (runWithCLIOpts, defaultStreamSize)

moduleName :: String
moduleName = "Data.Stream.Concurrent"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    runWithCLIOpts defaultStreamSize
        (\value ->
               allBenchmarks moduleName id value
            <> interleaveBenchmarks moduleName id value
        )
