-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM_)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile, dropWhile)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Unicode.Parser as PRU

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common

runParser :: Int -> (Stream IO Char -> IO a) -> IO ()
runParser count p = do
    let v = "+123456789.123456789e-123"
    let !(arr :: Array.Array Char) = Array.fromListN (length v) v
    let s = Stream.unfold Array.reader arr
    replicateM_ count (p s)

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink :: Int -> String -> (Stream IO Char -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ runParser value f

{-# INLINE doubleParser #-}
doubleParser :: Monad m => Stream m Char -> m (Either ParseError (Int, Int))
doubleParser = Stream.parse PRU.doubleParser

{-# INLINE number #-}
number :: Monad m => Stream m Char -> m (Either ParseError (Integer, Int))
number = Stream.parse PRU.number

{-# INLINE double #-}
double :: Monad m => Stream m Char -> m (Either ParseError Double)
double = Stream.parse PRU.double

{-# INLINE numberDouble #-}
numberDouble :: Monad m => Stream m Char -> m (Either ParseError Double)
numberDouble = Stream.parse p
    where p = uncurry PRU.mkDouble <$> PRU.number

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Unicode.Parser"

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
      benchIOSink value "doubleParser" doubleParser
    , benchIOSink value "number" number
    , benchIOSink value "double (doubleParser)" double
    , benchIOSink value "double (number)" numberDouble
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    runParser value double
    return ()
#endif
