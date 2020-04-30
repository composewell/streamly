-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude

import Gauge

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    size <- limitStreamSize value
    size `seq` runMode (mode cfg) cfg benches (allBenchmarks size)
  where
    allBenchmarks size =
        [ bgroup "o-n-stack" $ o_n_stack_serial_iterated size
        , bgroup "o-1-space" $
          concat [serial size, wSerial size, zipSerial size]
        , bgroup "o-n-heap" $
          concat [o_n_heap_serial_foldl size, o_n_heap_serial_buffering size]
        , bgroup "o-n-space" $
          concat
              [ o_n_space_serial_toList size -- < 2MB
              , o_n_space_serial_outerProductStreams size
              , o_n_space_wSerial_outerProductStreams size
              , o_n_space_serial size -- < 2MB
              , o_n_space_serial_foldr size
              ]
        ]
    serial size =
        concat
            [ o_1_space_serial_pure size
            , o_1_space_serial_foldable size
            , o_1_space_serial_generation size
            , o_1_space_serial_elimination size
            , o_1_space_serial_foldMultiStream size
            , o_1_space_serial_pipes size
            , o_1_space_serial_pipesX4 size
            , o_1_space_serial_transformer size
            , o_1_space_serial_transformation size
            , o_1_space_serial_transformationX4 size
            , o_1_space_serial_filtering size
            , o_1_space_serial_filteringX4 size
            , o_1_space_serial_joining size
            , o_1_space_serial_concatFoldable size
            , o_1_space_serial_concatSerial size
            , o_1_space_serial_outerProductStreams size
            , o_1_space_serial_mixed size
            , o_1_space_serial_mixedX4 size
            ]
    wSerial size =
        concat
            [ o_1_space_wSerial_transformation size
            , o_1_space_wSerial_concatMap size
            , o_1_space_wSerial_outerProduct size
            ]
    zipSerial size = concat [o_1_space_zipSerial_transformation size]
