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
    --  let value2 = round $ sqrt $ (fromIntegral value :: Double)
    value `seq`
        runMode
            (mode cfg)
            cfg
            benches
            (concat
                 [ async value
                 , wAsync value
                 , ahead value
                 , o_1_space_async_zip value
                 ])
  where
    async value =
        concat
            [ o_1_space_async_generation value
            , o_1_space_async_concatFoldable value
            , o_1_space_async_concatMap value
            , o_1_space_async_transformation value
            ]
    wAsync value =
        concat
            [ o_1_space_wAsync_generation value
            , o_1_space_wAsync_concatFoldable value
            , o_1_space_wAsync_concatMap value
            , o_1_space_wAsync_transformation value
            ]
    ahead value =
        concat
            [ o_1_space_ahead_generation value
            , o_1_space_ahead_concatFoldable value
            , o_1_space_ahead_concatMap value
            , o_1_space_ahead_transformation value
            ]
