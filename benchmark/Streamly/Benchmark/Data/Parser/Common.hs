-- |
-- Module      : Streamly.Benchmark.Data.Parser.Common
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Common
  (
    benchIOSink
  , benchIOSrc
  ) where

import Control.DeepSeq (NFData(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import Test.Tasty.Bench
import Streamly.Benchmark.Common

-- XXX use benchIOSink1 instead, this is to be removed
-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . streamUnfoldrM value

-- XXX use benchIOSrc1 instead, this is to be removed
{-# INLINE benchIOSrc #-}
benchIOSrc
    :: NFData b
    => (Int -> Int -> Stream IO a)
    -> Int
    -> String
    -> (Stream IO a -> IO b)
    -> Benchmark
benchIOSrc src value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . src value
