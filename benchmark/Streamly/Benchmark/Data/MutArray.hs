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
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import GHC.Types (SPEC(..))
import Prelude
    ( IO
    , Int
    , Either
    , Maybe
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
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)
import Fusion.Plugin.Types
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

-- XXX these are not fusing
{-# ANN scanl'_x1 (PermitPatternMatches
    [''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Int,''Stream.EnumToState
    ,''MutArray,''Fold.Step,''IO,''Either,''Fold.Fold,''(,)]) #-}
{-# ANN scanl'_x1 (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''MutArray,''Stream.Step,''Stream.EnumToState
    ,''Fold.Step,''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Either,''(,)
    ,''Stream.Stream,''Fold.Fold]) #-}
{-# ANN scanl'_x1 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE scanl'_x1 #-}
scanl'_x1 :: Int -> Int -> IO (Stream Int)
scanl'_x1 value =
    withArray value
        $ composeN 1 $ onArray value $ Stream.scanl (Scanl.scanl' (+) 0)

{-# ANN scanl'_x4 (PermitPatternMatches
    [''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Int,''Stream.EnumToState
    ,''MutArray,''Fold.Step,''IO,''Either,''Fold.Fold,''(,)]) #-}
{-# ANN scanl'_x4 (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''MutArray,''Stream.Step,''Stream.EnumToState
    ,''Fold.Step,''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Either,''(,)
    ,''Stream.Stream,''Fold.Fold]) #-}
{-# ANN scanl'_x4 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE scanl'_x4 #-}
scanl'_x4 :: Int -> Int -> IO (Stream Int)
scanl'_x4 value =
    withArray value
        $ composeN 4 $ onArray value $ Stream.scanl (Scanl.scanl' (+) 0)

{-# ANN scanl1'_x1 (PermitPatternMatches
    [''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Int,''Stream.EnumToState
    ,''MutArray,''SPEC,''Maybe,''Stream.UnfoldState,''Fold.Step,''IO,''Either
    ,''Fold.Fold,''(,)]) #-}
{-# ANN scanl1'_x1 (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''MutArray,''Stream.Step,''Stream.EnumToState
    ,''Fold.Step,''Fold.Tuple'Fused,''MArray.ArrayUnsafe
    ,''Stream.UnfoldState,''SPEC,''Maybe,''Either,''(,),''Stream.Stream
    ,''Fold.Fold]) #-}
{-# ANN scanl1'_x1 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE scanl1'_x1 #-}
scanl1'_x1 :: Int -> Int -> IO (Stream Int)
scanl1'_x1 value =
    withArray value $ composeN 1 $ onArray value $ Stream.scanl1' (+)

{-# ANN scanl1'_x4 (PermitPatternMatches
    [''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Int,''Stream.EnumToState
    ,''MutArray,''SPEC,''Maybe,''Stream.UnfoldState,''Fold.Step,''IO,''Either
    ,''Fold.Fold,''(,)]) #-}
{-# ANN scanl1'_x4 (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''MutArray,''Stream.Step,''Stream.EnumToState
    ,''Fold.Step,''Fold.Tuple'Fused,''MArray.ArrayUnsafe
    ,''Stream.UnfoldState,''SPEC,''Maybe,''Either,''(,),''Stream.Stream
    ,''Fold.Fold]) #-}
{-# ANN scanl1'_x4 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE scanl1'_x4 #-}
scanl1'_x4 :: Int -> Int -> IO (Stream Int)
scanl1'_x4 value =
    withArray value $ composeN 4 $ onArray value $ Stream.scanl1' (+)

{-# ANN fmap_x1 (PermitPatternMatches
    [''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Int,''Stream.EnumToState
    ,''MutArray,''SPEC,''Stream.UnfoldState,''Fold.Step,''IO,''Either
    ,''Fold.Fold,''(,)]) #-}
{-# ANN fmap_x1 (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''MutArray,''Stream.Step,''Stream.EnumToState
    ,''Fold.Step,''Fold.Tuple'Fused,''MArray.ArrayUnsafe
    ,''Stream.UnfoldState,''SPEC,''Either,''(,),''Stream.Stream
    ,''Fold.Fold]) #-}
{-# ANN fmap_x1 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE fmap_x1 #-}
fmap_x1 :: Int -> Int -> IO (Stream Int)
fmap_x1 value = withArray value $ composeN 1 $ onArray value $ fmap (+ 1)

{-# ANN fmap_x4 (PermitPatternMatches
    [''Fold.Tuple'Fused,''MArray.ArrayUnsafe,''Int,''Stream.EnumToState
    ,''MutArray,''SPEC,''Stream.UnfoldState,''Fold.Step,''IO,''Either
    ,''Fold.Fold,''(,)]) #-}
{-# ANN fmap_x4 (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''MutArray,''Stream.Step,''Stream.EnumToState
    ,''Fold.Step,''Fold.Tuple'Fused,''MArray.ArrayUnsafe
    ,''Stream.UnfoldState,''SPEC,''Either,''(,),''Stream.Stream
    ,''Fold.Fold]) #-}
{-# ANN fmap_x4 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE fmap_x4 #-}
fmap_x4 :: Int -> Int -> IO (Stream Int)
fmap_x4 value = withArray value $ composeN 4 $ onArray value $ fmap (+ 1)

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.MutArray"

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks ::
    (MutArray Int, Array.Array Int) -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks benchEnv value =
    typeCommonBenchmarks benchEnv value
      ++ [ (SpaceO_1, benchIO "scanl'_x1" $ scanl'_x1 value)
         , (SpaceO_1, benchIO "scanl1'_x1" $ scanl1'_x1 value)
         , (SpaceO_1, benchIO "fmap_x1" $ fmap_x1 value)

         , (SpaceO_1, benchIO "scanl'_x4" $ scanl'_x4 value)
         , (SpaceO_1, benchIO "scanl1'_x4" $ scanl1'_x4 value)
         , (SpaceO_1, benchIO "fmap_x4" $ fmap_x4 value)
         ]

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        marr <-
            MArray.fromStream
                (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
        indices <-
            Array.fromStream
                (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
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
