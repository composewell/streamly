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
    -- let value2 = round $ sqrt $ (fromIntegral value :: Double)
    value `seq`
        runMode
            (mode cfg)
            cfg
            benches
            (concat [linear value, o_1_space_parallel_outerProductStreams value])
  where
    linear value =
        concat
            [ o_1_space_parallel_generation value
            , o_1_space_parallel_concatFoldable value
            -- , o_n_space_parallel_outerProductStreams
            , o_1_space_parallel_concatMap value
            , o_1_space_parallel_transformation value
            ]
