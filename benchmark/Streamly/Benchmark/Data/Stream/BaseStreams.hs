-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

import qualified Streamly.Benchmark.Data.Stream.StreamK as K
import qualified Streamly.Benchmark.Data.Stream.StreamD as D
import qualified Streamly.Benchmark.Data.Stream.StreamDK as DK

import Gauge

main :: IO ()
main =
    defaultMain $
    [ bgroup "o-n-stack" o_n_stack
    , bgroup "o-1-space" o_1_space
    , bgroup "o-n-heap" o_n_heap
    , bgroup "o-n-space" o_n_space
    ]
  where
    o_1_space =
        concat [D.o_1_space, K.o_1_space_list, K.o_1_space, DK.o_1_space]
    o_n_heap = K.o_n_heap
    o_n_stack = concat [D.o_n_stack, K.o_n_stack]
    o_n_space = concat [D.o_n_space, K.o_n_space]
