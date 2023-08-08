{-# LANGUAGE TemplateHaskell #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

-- |
-- Module      : Streamly.Benchmark.Data.Serialize
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
import GHC.Generics (Generic)
import System.Random (randomRIO)

import Streamly.Internal.Data.Unbox (newBytes)
import Streamly.Internal.Data.Serialize
#define USE_TH
#ifdef USE_TH
import Streamly.Internal.Data.Serialize.TH
#endif

import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data CustomDT1
    = CDT1C1
    | CDT1C2 Int
    | CDT1C3 Int Bool
    deriving (Generic, Show, Eq)

#ifndef USE_TH
instance Serialize CustomDT1
#else
$(deriveSerialize ''CustomDT1)
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE pokeTimes #-}
pokeTimes :: forall a. Serialize a => a -> Int -> IO ()
pokeTimes val times = do
    let n =
            case size :: Size a of
                ConstSize x -> x
                VarSize f -> f val
    arr <- newBytes n
    replicateM_ times $ do
        serialize 0 arr val

{-# INLINE peekTimes #-}
peekTimes :: forall a. Serialize a => a -> Int -> IO ()
peekTimes val times = do
    let n =
            case size :: Size a of
                ConstSize x -> x
                VarSize f -> f val
    arr <- newBytes n
    _ <- serialize 0 arr val
    replicateM_ times $ do
        (_, _ :: a) <- deserialize 0 arr
        return ()

{-# INLINE roundtrip #-}
roundtrip :: forall a. (Eq a, Serialize a) => a -> Int -> IO ()
roundtrip val times = do
    let n =
            case size :: Size a of
                ConstSize x -> x
                VarSize f -> f val
    arr <- newBytes n
    replicateM_ times $ do
        _ <- serialize 0 arr val
        (_, val1) <- deserialize 0 arr
        assert (val == val1) (pure ())

benchSink :: NFData b => String -> Int -> (Int -> IO b) -> Benchmark
benchSink name times f = bench name (nfIO (randomRIO (times, times) >>= f))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

allBenchmarks :: Int -> [Benchmark]
allBenchmarks times =
    [ bgroup "poke"
        [ benchSink "C1" times
            (pokeTimes (CDT1C1 :: CustomDT1))
        , benchSink "C2" times
            (pokeTimes ((CDT1C2 (5 :: Int)) :: CustomDT1))
        , benchSink "C3" times
            (pokeTimes ((CDT1C3 (5 :: Int) True) :: CustomDT1))
        ]
    , bgroup "peek"
        [ benchSink "C1" times
            (peekTimes (CDT1C1 :: CustomDT1))
        , benchSink "C2" times
            (peekTimes ((CDT1C2 (5 :: Int)) :: CustomDT1))
        , benchSink "C3" times
            (peekTimes ((CDT1C3 (5 :: Int) True) :: CustomDT1))
        ]
    , bgroup "roundtrip"
        [ benchSink "C1" times
            (roundtrip (CDT1C1 :: CustomDT1))
        , benchSink "C2" times
            (roundtrip ((CDT1C2 (5 :: Int)) :: CustomDT1))
        , benchSink "C3" times
            (roundtrip ((CDT1C3 (5 :: Int) True) :: CustomDT1))
        ]
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    runWithCLIOpts defaultStreamSize allBenchmarks
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    -- peekTimes ((CDT1C2 (5 :: Int)) :: CustomDT1) value
    roundtrip ((CDT1C2 (5 :: Int)) :: CustomDT1) value
    return ()
#endif
