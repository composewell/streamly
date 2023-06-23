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
import System.Random (randomRIO)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream.StreamD (Stream)
import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile, dropWhile)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
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

{-# INLINE sourceUnfoldrMc #-}
sourceUnfoldrMc :: Monad m => Int -> Int -> Stream m Char
sourceUnfoldrMc value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just ('1', cnt + 1))


-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSinkc #-}
benchIOSinkc
    :: NFData b
    => Int -> String -> (Stream IO Char -> IO b) -> Benchmark
benchIOSinkc value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrMc value

{-# INLINE double #-}
double :: Monad m => Stream m Char -> m (Either ParseError Double)
double = Stream.parse PRU.double

{-# INLINE double2 #-}
double2 :: Monad m => Stream m Char -> m (Either ParseError Double)
double2 = Stream.parse PRU.double2

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
      benchIOSinkc value "double" double
    , benchIOSinkc value "double2" double2
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

