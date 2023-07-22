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

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad (replicateM_)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream.StreamD (Stream)
import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile, dropWhile)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Unicode.Parser as PRU

import Gauge hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

runParser :: Int -> (Stream IO Char -> IO a) -> IO ()
runParser count p = do
    let v = "+123456789.123456789e-123"
    let s = Stream.unfold Unfold.fromList v
    replicateM_ count (p s)

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink :: Int -> String -> (Stream IO Char -> IO b) -> Benchmark
benchIOSink value name f = bench name $ nfIO $ runParser value f

{-# INLINE double #-}
double :: Monad m => Stream m Char -> m (Either ParseError Double)
double = Stream.parse PRU.double

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
      benchIOSink value "double" double
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOptsEnv defaultStreamSize alloc (allBenchmarks env)

    where

    alloc value = Stream.fold Fold.toList $ Stream.chunksOf 100 $ sourceUnfoldrM value 0

    allBenchmarks _ _ value =
        [ bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
