{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Benchmark.Data.MutArray.Type
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

module Streamly.Benchmark.Data.MutArray.Type
    (
      typeCommonBenchmarks
    , benchIO
    , withArray
    , sourceUnfoldrM
    ) where

import Control.DeepSeq (NFData(..))
#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif
import System.Random (randomRIO)
import Prelude

import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)

#if __GLASGOW_HASKELL__ >= 810
type Stream :: Type -> Type
#endif
type Stream = MutArray

instance NFData (MutArray a) where
    {-# INLINE rnf #-}
    rnf _ = ()

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withArray #-}
withArray :: Int -> (Stream Int -> IO b) -> Int -> IO b
withArray value f n = sourceIntFromTo value n >>= f

{-# INLINE withStream #-}
withStream :: Int -> (Stream.Stream IO Int -> IO b) -> Int -> IO b
withStream value f = f . sourceUnfoldrM value

drain :: Monad m => Stream.Stream m a -> m ()
drain = Stream.fold Fold.drain

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> Int -> IO (Stream Int)
sourceUnfoldr value n =
    let step cnt =
            if cnt > n + value
            then Nothing
            else Just (cnt, cnt + 1)
    in Stream.fold (MArray.createOf value) $ Stream.unfoldr step n

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: Int -> Int -> IO (Stream Int)
sourceIntFromTo value n =
    Stream.fold (MArray.createOf value) $ Stream.enumerateFromTo n (n + value)

{-# INLINE sourceFromList #-}
sourceFromList :: Int -> Int -> IO (Stream Int)
sourceFromList value n =
    Stream.fold (MArray.createOf value) $ Stream.fromList [n .. n + value]

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: Int -> Int -> IO (Stream Int)
sourceIntFromToFromList value n =
    MArray.fromListN value [n..n + value]

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: Int -> Int -> IO (Stream Int)
sourceIntFromToFromStream value n =
    Stream.fold MArray.create $ Stream.enumerateFromTo n (n + value)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream.Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE idArr #-}
idArr :: Int -> Int -> IO (Stream Int)
idArr value = withArray value return

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: Int -> Int -> IO ()
unfoldReadDrain value = withArray value $ drain . Stream.unfold MArray.reader

{-# INLINE unfoldReadRevDrain #-}
unfoldReadRevDrain :: Int -> Int -> IO ()
unfoldReadRevDrain value = withArray value $ drain . Stream.unfold MArray.readerRev

{-# INLINE toStreamDRevDrain #-}
toStreamDRevDrain :: Int -> Int -> IO ()
toStreamDRevDrain value = withArray value $ drain . MArray.readRev

{-# INLINE toStreamDDrain #-}
toStreamDDrain :: Int -> Int -> IO ()
toStreamDDrain value = withArray value $ drain . MArray.read

{-# INLINE unfoldFold #-}
unfoldFold :: Int -> Int -> IO Int
unfoldFold value = withArray value $ Stream.fold (Fold.foldl' (+) 0) . Stream.unfold MArray.reader

{-# INLINE writeN #-}
writeN :: Int -> Int -> IO (Stream Int)
writeN value = withStream value (Stream.fold (MArray.createOf value))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

typeCommonBenchmarks ::
    (MutArray Int, Array.Array Int) -> Int -> [(SpaceComplexity, Benchmark)]
typeCommonBenchmarks ~(array, indices) value =
      [ (SpaceO_1, benchIO "partitionBy (< 0)" $ \_ ->
            MArray.partitionBy (< 0) array)
      , (SpaceO_1, benchIO "partitionBy (> 0)" $ \_ ->
            MArray.partitionBy (> 0) array)
      , (SpaceO_1, benchIO "partitionBy (< value/2)" $ \_ ->
            MArray.partitionBy (< (value `div` 2)) array)
      , (SpaceO_1, benchIO "partitionBy (> value/2)" $ \_ ->
            MArray.partitionBy (> (value `div` 2)) array)
      , (SpaceO_1, benchIO "strip (< value/2 || > value/2)" $ \_ ->
            MArray.dropAround (\x -> x < value `div` 2 || x > value `div` 2) array)
      , (SpaceO_1, benchIO "strip (> 0)" $ \_ ->
            MArray.dropAround (> 0) array)
      , (SpaceO_1, benchIO "modifyIndices (+ 1)" $ \_ ->
            Stream.fold (MArray.modifyIndices array (\_idx val -> val + 1))
            $ Stream.unfold Array.reader indices)

      , (SpaceO_1, benchIO "createOf . intFromTo" $ sourceIntFromTo value)
      , (SpaceO_1, benchIO "fromList . intFromTo" $ sourceIntFromToFromList value)
      , (SpaceO_1, benchIO "createOf . unfoldr" $ sourceUnfoldr value)
      , (SpaceO_1, benchIO "createOf . fromList" $ sourceFromList value)
      , (SpaceO_1, benchIO "write . intFromTo" $ sourceIntFromToFromStream value)

      , (SpaceO_1, benchIO "id" $ idArr value)
      , (SpaceO_1, benchIO "foldl'" $ unfoldFold value)
      , (SpaceO_1, benchIO "read" $ unfoldReadDrain value)
      , (SpaceO_1, benchIO "readRev" $ unfoldReadRevDrain value)
      , (SpaceO_1, benchIO "toStream" $ toStreamDDrain value)
      , (SpaceO_1, benchIO "toStreamRev" $ toStreamDRevDrain value)

      , (HeapO_n, benchIO "createOf" $ writeN value)
      ]
