-- |
-- Module      : Streamly.Benchmark.Data.MutArray
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

import Control.Monad.IO.Class (MonadIO)
#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif
import Prelude
    ( IO
    , Int
    , Monad(..)
    , Num(..)
    , Eq(..)
    , String
    , ($)
    , (.)
    , (++)
    , filter
    , fmap
    , fst
    , snd
    , undefined
    )
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)
import Streamly.Benchmark.Data.MutArray.Type
    (typeCommonBenchmarks, benchIO, withArray, sourceUnfoldrM)

#if __GLASGOW_HASKELL__ >= 810
type Stream :: Type -> Type
#endif
type Stream = MutArray

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN :: Monad m
    => Int -> (Stream Int -> m (Stream Int)) -> Stream Int -> m (Stream Int)
composeN n f x =
    case n of
        1 -> f x
        2 -> f x >>= f
        3 -> f x >>= f >>= f
        4 -> f x >>= f >>= f >>= f
        _ -> undefined

{-# INLINE onArray #-}
onArray
    :: MonadIO m => Int -> (Stream.Stream m Int -> Stream.Stream m Int)
    -> Stream Int
    -> m (Stream Int)
onArray value f arr =
    Stream.fold (MArray.createOf value) $ f $ Stream.unfold MArray.reader arr

{-# INLINE scanl' #-}
scanl' :: Int -> IO (Stream Int)
scanl' value = withArray value $ composeN 1 $ onArray value $ Stream.scanl (Scanl.scanl' (+) 0)

{-# INLINE scanl'X4 #-}
scanl'X4 :: Int -> IO (Stream Int)
scanl'X4 value = withArray value $ composeN 4 $ onArray value $ Stream.scanl (Scanl.scanl' (+) 0)

{-# INLINE scanl1' #-}
scanl1' :: Int -> IO (Stream Int)
scanl1' value = withArray value $ composeN 1 $ onArray value $ Stream.scanl1' (+)

{-# INLINE scanl1'X4 #-}
scanl1'X4 :: Int -> IO (Stream Int)
scanl1'X4 value = withArray value $ composeN 4 $ onArray value $ Stream.scanl1' (+)

{-# INLINE map #-}
map :: Int -> IO (Stream Int)
map value = withArray value $ composeN 1 $ onArray value $ fmap (+ 1)

{-# INLINE mapX4 #-}
mapX4 :: Int -> IO (Stream Int)
mapX4 value = withArray value $ composeN 4 $ onArray value $ fmap (+ 1)

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.MutArray"

benchmarks ::
    (MutArray Int, Array.Array Int) -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks env value =
    typeCommonBenchmarks env value
      ++ [ (SpaceO_1, benchIO "scanl'" $ scanl' value)
         , (SpaceO_1, benchIO "scanl1'" $ scanl1' value)
         , (SpaceO_1, benchIO "map" $ map value)

         , (SpaceO_1, benchIO "scanl'X4" $ scanl'X4 value)
         , (SpaceO_1, benchIO "scanl1'X4" $ scanl1'X4 value)
         , (SpaceO_1, benchIO "mapX4" $ mapX4 value)
         ]

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        marr <- MArray.fromStream (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
        indices <- Array.fromStream (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
        return (marr, indices)

    allBenchmarks array value =
        let allBenches = benchmarks array value
            get x = fmap snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
