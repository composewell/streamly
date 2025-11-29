--
-- Module      : Main
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Stream.ConcurrentCommon
import Streamly.Benchmark.Common (runWithCLIOpts, defaultStreamSize)

import qualified Streamly.Internal.Data.Stream.Prelude as Stream

moduleName :: String
moduleName = "Data.Stream.ConcurrentEager"

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    runWithCLIOpts
        defaultStreamSize
        (allBenchmarks
            mkEagerParallel
            unEagerParallel moduleName True (Stream.eager True))
