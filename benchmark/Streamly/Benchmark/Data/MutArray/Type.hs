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
import Control.Monad.IO.Class (MonadIO)
#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif
import System.Random (randomRIO)
import Prelude hiding (read)

import Streamly.Data.MutByteArray (MutByteArray, Unbox)
import Streamly.Internal.Data.MutByteArray (PinnedState)
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)
import Fusion.Plugin.Types

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

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: Int -> Int -> IO (Stream Int)
sourceIntFromTo value n =
    Stream.fold (MArray.createOf value) $ Stream.enumerateFromTo n (n + value)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream.Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- sourceIntFromTo is also the helper behind withArray, so it stays INLINE and
-- the benchmark gets its own NOINLINE wrapper.
{-# ANN createOf (PermitPatternMatches
    [''Int,''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''IO]) #-}
{-# ANN createOf (PermitConstructions
    [''Int,''MutArray,''Fold.Tuple'Fused,''MArray.ArrayUnsafe]) #-}
{-# ANN createOf (PermitTypeClasses []) #-}
{-# NOINLINE createOf #-}
createOf :: Int -> Int -> IO (Stream Int)
createOf = sourceIntFromTo

{-# ANN createOf_Unfoldr (PermitPatternMatches [''Int,''IO]) #-}
{-# ANN createOf_Unfoldr (PermitConstructions [''MutArray]) #-}
{-# ANN createOf_Unfoldr (PermitTypeClasses []) #-}
{-# NOINLINE createOf_Unfoldr #-}
createOf_Unfoldr :: Int -> Int -> IO (Stream Int)
createOf_Unfoldr value n =
    let step cnt =
            if cnt > n + value
            then Nothing
            else Just (cnt, cnt + 1)
    in Stream.fold (MArray.createOf value) $ Stream.unfoldr step n

{-# ANN createOf_FromList (PermitPatternMatches
    [''Fold.Tuple'Fused,''[],''Int,''MArray.ArrayUnsafe,''IO]) #-}
{-# ANN createOf_FromList (PermitConstructions
    [''Int,''MutArray,''[],''Fold.Tuple'Fused,''MArray.ArrayUnsafe]) #-}
{-# ANN createOf_FromList (PermitTypeClasses []) #-}
{-# NOINLINE createOf_FromList #-}
createOf_FromList :: Int -> Int -> IO (Stream Int)
createOf_FromList value n =
    Stream.fold (MArray.createOf value) $ Stream.fromList [n .. n + value]

{-# ANN createOf_UnfoldrM (PermitPatternMatches [''Int,''IO]) #-}
{-# ANN createOf_UnfoldrM (PermitConstructions [''MutArray]) #-}
{-# ANN createOf_UnfoldrM (PermitTypeClasses []) #-}
{-# NOINLINE createOf_UnfoldrM #-}
createOf_UnfoldrM :: Int -> Int -> IO (Stream Int)
createOf_UnfoldrM value =
    withStream value (Stream.fold (MArray.createOf value))

{-# ANN fromListN (PermitPatternMatches [''[],''Int,''IO]) #-}
{-# ANN fromListN (PermitConstructions [''MutArray,''[],''Int]) #-}
{-# ANN fromListN (PermitTypeClasses []) #-}
{-# NOINLINE fromListN #-}
fromListN :: Int -> Int -> IO (Stream Int)
fromListN value n = MArray.fromListN value [n..n + value]

{-# ANN create (PermitPatternMatches [''MutArray,''Int]) #-}
{-# ANN create (PermitConstructions [''MutArray, ''PinnedState]) #-}
{-# ANN create (PermitTypeClasses []) #-}
{-# NOINLINE create #-}
create :: Int -> Int -> IO (Stream Int)
create value n =
    Stream.fold MArray.create $ Stream.enumerateFromTo n (n + value)

-------------------------------------------------------------------------------
-- In-place transformation
-------------------------------------------------------------------------------

{-# ANN partitionBy_LT (PermitPatternMatches
    [''MutArray,''Int,''Maybe,''(,)]) #-}
{-# ANN partitionBy_LT (PermitConstructions
    [''Maybe,''(,),''Int,''MutArray]) #-}
{-# ANN partitionBy_LT (PermitTypeClasses []) #-}
{-# NOINLINE partitionBy_LT #-}
partitionBy_LT ::
    Stream Int -> Int -> Int -> IO (Stream Int, Stream Int)
partitionBy_LT array pivot _ = MArray.partitionBy (< pivot) array

{-# ANN partitionBy_GT (PermitPatternMatches
    [''MutArray,''Int,''Maybe,''(,)]) #-}
{-# ANN partitionBy_GT (PermitConstructions
    [''Maybe,''(,),''Int,''MutArray]) #-}
{-# ANN partitionBy_GT (PermitTypeClasses []) #-}
{-# NOINLINE partitionBy_GT #-}
partitionBy_GT ::
    Stream Int -> Int -> Int -> IO (Stream Int, Stream Int)
partitionBy_GT array pivot _ = MArray.partitionBy (> pivot) array

{-# ANN dropAround_GT (PermitPatternMatches [''Int,''MutByteArray]) #-}
{-# ANN dropAround_GT (PermitConstructions [''MutArray]) #-}
{-# ANN dropAround_GT (PermitTypeClasses []) #-}
{-# NOINLINE dropAround_GT #-}
dropAround_GT :: Stream Int -> Int -> Int -> IO (Stream Int)
dropAround_GT array pivot _ = MArray.dropAround (> pivot) array

{-# ANN dropAround_NotEq (PermitPatternMatches [''Int,''MutByteArray]) #-}
{-# ANN dropAround_NotEq (PermitConstructions [''MutArray]) #-}
{-# ANN dropAround_NotEq (PermitTypeClasses []) #-}
{-# NOINLINE dropAround_NotEq #-}
dropAround_NotEq :: Stream Int -> Int -> Int -> IO (Stream Int)
dropAround_NotEq array pivot _ =
    MArray.dropAround (\x -> x < pivot || x > pivot) array

{-# ANN modifyIndices (PermitPatternMatches
    [''Int,''Array.Array,''MutArray,''()]) #-}
{-# ANN modifyIndices (PermitConstructions [''(,),''Int,''()]) #-}
{-# ANN modifyIndices (PermitTypeClasses [''MonadIO,''Unbox]) #-}
{-# NOINLINE modifyIndices #-}
modifyIndices :: Stream Int -> Array.Array Int -> Int -> IO ()
modifyIndices array indices _ =
    Stream.fold (MArray.modifyIndices array (\_idx val -> val + 1))
        $ Stream.unfold Array.reader indices

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# ANN reader (PermitPatternMatches
    [''MutArray,''Int,''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''IO]) #-}
{-# ANN reader (PermitConstructions
    [''(),''Int,''MutArray,''Fold.Tuple'Fused,''MArray.ArrayUnsafe]) #-}
{-# ANN reader (PermitTypeClasses []) #-}
{-# NOINLINE reader #-}
reader :: Int -> Int -> IO ()
reader value = withArray value $ drain . Stream.unfold MArray.reader

{-# ANN readerRev (PermitPatternMatches
    [''MutArray,''Int,''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''IO]) #-}
{-# ANN readerRev (PermitConstructions
    [''(),''Int,''MutArray,''Fold.Tuple'Fused,''MArray.ArrayUnsafe]) #-}
{-# ANN readerRev (PermitTypeClasses []) #-}
{-# NOINLINE readerRev #-}
readerRev :: Int -> Int -> IO ()
readerRev value = withArray value $ drain . Stream.unfold MArray.readerRev

{-# ANN read (PermitPatternMatches
    [''Fold.Tuple'Fused,''Int,''MArray.ArrayUnsafe,''IO]) #-}
{-# ANN read (PermitConstructions
    [''Int,''(),''Fold.Tuple'Fused,''MArray.ArrayUnsafe]) #-}
{-# ANN read (PermitTypeClasses []) #-}
{-# NOINLINE read #-}
read :: Int -> Int -> IO ()
read value = withArray value $ drain . MArray.read

{-# ANN readRev (PermitPatternMatches
    [''Fold.Tuple'Fused,''Int,''MArray.ArrayUnsafe,''IO]) #-}
{-# ANN readRev (PermitConstructions
    [''Int,''(),''Fold.Tuple'Fused,''MArray.ArrayUnsafe]) #-}
{-# ANN readRev (PermitTypeClasses []) #-}
{-# NOINLINE readRev #-}
readRev :: Int -> Int -> IO ()
readRev value = withArray value $ drain . MArray.readRev

{-# ANN foldl'_Reader (PermitPatternMatches
    [''MutArray,''Int,''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''IO]) #-}
{-# ANN foldl'_Reader (PermitConstructions
    [''Int,''MutArray,''Fold.Tuple'Fused,''MArray.ArrayUnsafe]) #-}
{-# ANN foldl'_Reader (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_Reader #-}
foldl'_Reader :: Int -> Int -> IO Int
foldl'_Reader value =
    withArray value
        $ Stream.fold (Fold.foldl' (+) 0) . Stream.unfold MArray.reader

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

typeCommonBenchmarks ::
    (MutArray Int, Array.Array Int) -> Int -> [(SpaceComplexity, Benchmark)]
typeCommonBenchmarks ~(array, indices) value =
    let half = value `div` 2
    in
      [ (SpaceO_1, benchIO "partitionBy_LT (0)" $ partitionBy_LT array 0)
      , (SpaceO_1, benchIO "partitionBy_GT (0)" $ partitionBy_GT array 0)
      , (SpaceO_1, benchIO "partitionBy_LT (value div 2)"
            $ partitionBy_LT array half)
      , (SpaceO_1, benchIO "partitionBy_GT (value div 2)"
            $ partitionBy_GT array half)
      , (SpaceO_1, benchIO "dropAround_NotEq (value div 2)"
            $ dropAround_NotEq array half)
      , (SpaceO_1, benchIO "dropAround_GT (0)" $ dropAround_GT array 0)
      , (SpaceO_1, benchIO "modifyIndices (+ 1)"
            $ modifyIndices array indices)

      , (SpaceO_1, benchIO "createOf (enumerateFromTo)" $ createOf value)
      , (SpaceO_1, benchIO "fromListN (enumerateFromTo)" $ fromListN value)
      , (SpaceO_1, benchIO "createOf_Unfoldr" $ createOf_Unfoldr value)
      , (SpaceO_1, benchIO "createOf_FromList" $ createOf_FromList value)
      , (SpaceO_1, benchIO "create (enumerateFromTo)" $ create value)

      , (SpaceO_1, benchIO "foldl'_Reader" $ foldl'_Reader value)
      , (SpaceO_1, benchIO "reader" $ reader value)
      , (SpaceO_1, benchIO "readerRev" $ readerRev value)
      , (SpaceO_1, benchIO "read" $ read value)
      , (SpaceO_1, benchIO "readRev" $ readRev value)

      , (HeapO_n, benchIO "createOf_UnfoldrM" $ createOf_UnfoldrM value)
      ]
