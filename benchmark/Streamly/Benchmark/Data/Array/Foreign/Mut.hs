-- |
-- Module      : Streamly.Benchmark.Data.Array.Foreign.Mut
-- Copyright   : (c) 2021 Composewell Technologies
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

module Main (main) where

import Control.DeepSeq (NFData(..))
import Streamly.Prelude (MonadAsync, SerialT, IsStream)
import System.Random (randomRIO)

import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Foreign.Mut as MArray

import Gauge hiding (env)
import Streamly.Benchmark.Common

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
    => String -> (Int -> a) -> (a -> IO b) -> Benchmark
benchIO name src sink =
    bench name $ nfIO $ randomRIO (1,1) >>= sink . src

o_1_space_serial_marray ::
    Int -> (MArray.Array Int, Array.Array Int) -> [Benchmark]
o_1_space_serial_marray value ~(array, indices) =
    [ benchIO "partitionBy (< 0)" (const array)
        $ MArray.partitionBy (< 0)
    , benchIO "partitionBy (> 0)" (const array)
        $ MArray.partitionBy (> 0)
    , benchIO "partitionBy (< value/2)" (const array)
        $ MArray.partitionBy (< (value `div` 2))
    , benchIO "partitionBy (> value/2)" (const array)
        $ MArray.partitionBy (> (value `div` 2))
    , benchIO "strip (< value/2 || > value/2)" (const array)
        $ MArray.strip (\x -> x < value `div` 2 || x > value `div` 2)
    , benchIO "strip (> 0)" (const array)
        $ MArray.strip (> 0)
    , benchIO "modifyIndices (+ 1)" (const indices)
        $ Stream.fold (MArray.modifyIndices (\_idx val -> val + 1) array)
        . Stream.unfold Array.read
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Foreign.Mut"

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        marr <- MArray.fromStream (sourceUnfoldrM value 0 :: SerialT IO Int)
        indices <- Array.fromStream (sourceUnfoldrM value 0 :: SerialT IO Int)
        return (marr, indices)

    allBenchmarks array value =
        [ bgroup (o_1_space_prefix moduleName) $
            o_1_space_serial_marray value array
        ]
