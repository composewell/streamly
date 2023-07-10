{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Benchmark.Data.Unbox
-- Copyright   : (c) 2023 Composewell
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.DeepSeq (NFData(..))
import Control.Exception (assert)
import Control.Monad (replicateM_)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import System.Random (randomRIO)

import Streamly.Internal.Data.Unbox
import Streamly.Internal.Data.Unbox.TH

import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data CustomDT1 a b
    = CDT1C1
    | CDT1C2 a
    | CDT1C3 a b
    deriving (Generic, Show, Eq)
type CustomDT1_ = CustomDT1 Int Bool

instance (Unbox a, Unbox b) => Unbox (CustomDT1 a b)

data CustomDT2 a b
    = CDT2C1
    | CDT2C2 a
    | CDT2C3 a b
    deriving (Show, Eq)
type CustomDT2_ = CustomDT2 Int Bool

$(deriveUnbox ''CustomDT2)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

serializeDeserializeTimes :: forall a. (Eq a, Unbox a) => a -> Int -> IO ()
serializeDeserializeTimes val times = do
    arr <- newBytes (sizeOf (Proxy :: Proxy a))
    replicateM_ times $ do
        pokeByteIndex 0 arr val
        val1 <- peekByteIndex 0 arr
        assert (val == val1) (pure ())

benchSink :: NFData b => String -> Int -> (Int -> IO b) -> Benchmark
benchSink name times f = bench name (nfIO (randomRIO (times, times) >>= f))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

allBenchmarks :: Int -> [Benchmark]
allBenchmarks times =
    [ benchSink
          "serializeDeserializeTimes CDT1C1 (Generic)"
          times
          (serializeDeserializeTimes (CDT1C1 :: CustomDT1_))
    , benchSink
          "serializeDeserializeTimes CDT2C1 (TH)"
          times
          (serializeDeserializeTimes (CDT2C1 :: CustomDT2_))
    , benchSink
          "serializeDeserializeTimes CDT1C2 (Generic)"
          times
          (serializeDeserializeTimes ((CDT1C2 (5 :: Int)) :: CustomDT1_))
    , benchSink
          "serializeDeserializeTimes CDT2C2 (TH)"
          times
          (serializeDeserializeTimes ((CDT2C2 (5 :: Int)) :: CustomDT2_))
    , benchSink
          "serializeDeserializeTimes CDT1C3 (Generic)"
          times
          (serializeDeserializeTimes ((CDT1C3 (5 :: Int) True) :: CustomDT1_))
    , benchSink
          "serializeDeserializeTimes CDT2C3 (TH)"
          times
          (serializeDeserializeTimes ((CDT2C3 (5 :: Int) True) :: CustomDT2_))
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks
