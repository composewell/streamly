-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq`
        runMode
            (mode cfg)
            cfg
            benches
      -- XXX arbitrarily large rate should be the same as rate Nothing
            (concat
                 [o_1_space_async_avgRate value, o_1_space_ahead_avgRate value])
