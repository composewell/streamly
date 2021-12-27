-- |
-- Module      : Streamly.Benchmark.Data.Mut.Array
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)
import Prelude hiding ()

import qualified Streamly.Prelude  as Stream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array.Stream.Mut.Foreign as MArray

import Gauge hiding (env)
import Streamly.Prelude (SerialT, MonadAsync, IsStream)
import Streamly.Benchmark.Common

#ifdef INSPECTION
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (IsStream t, MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE benchIO #-}
benchIO
    :: NFData b
    => String -> (Int -> t IO a) -> (t IO a -> IO b) -> Benchmark
benchIO name src sink =
    bench name $ nfIO $ randomRIO (1,1) >>= sink . src

{-# INLINE partitionBy #-}
partitionBy :: Int -> SerialT IO (MArray.Array Int) -> IO ()
partitionBy value s = Stream.fold Fold.drain $ do
    a <- s
    MArray.partitionBy ( < value) a

o_n_space_serial_marray ::
    Int -> [MArray.Array Int] -> [Benchmark]
o_n_space_serial_marray bound arrays =
    [ benchIO "partitionBy" (\_ -> Stream.fromList arrays)
        $ partitionBy bound
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Mut.Array"

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value =
        Stream.toList $ MArray.arraysOf value $ sourceUnfoldrM value 0

    allBenchmarks arrays value =
        [ bgroup (o_1_space_prefix moduleName) $
            o_n_space_serial_marray value arrays
        ]
