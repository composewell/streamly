-- |
-- Module      : Stream.Nesting.LogicUnfold
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Stream.Nesting.LogicUnfold (benchmarks) where

import Streamly.Internal.Data.Producer (InterleaveState(..), EnumToState(..))
import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Type (benchIO)
import Streamly.Benchmark.Common
import qualified Stream.Type as Type
import Prelude hiding (concatMap, zipWith)
import Fusion.Plugin.Types
import GHC.Stack (SrcLoc, CallStack)
import GHC.Classes (IP)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

{-# INLINE infiniteIntsUnfold #-}
infiniteIntsUnfold :: Monad m => Int -> Int -> Unfold m ((), ()) Int
infiniteIntsUnfold _ _ =
    Unfold.interleave
        (Unfold.supply (0 :: Int) Unfold.enumerateFrom)
        (Unfold.supply (-1) Unfold.enumerateDownFromNum)

{-# INLINE unfoldCrossEqn #-}
unfoldCrossEqn :: Monad m => Int -> Unfold m ((), ()) Int -> m ()
unfoldCrossEqn maxVal input =
    Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.unfold (Unfold.cross input input) (undefined, undefined)

{-# INLINE fairUnfoldCrossEqn #-}
fairUnfoldCrossEqn :: Monad m => Int -> Unfold m ((), ()) Int -> m ()
fairUnfoldCrossEqn maxVal input =
    Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.unfold (Unfold.fairCross input input) (undefined, undefined)

{-# INLINE fairUnfoldEachEqn #-}
fairUnfoldEachEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldEachEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.fairUnfoldEach intu ints

{-# INLINE unfoldSchedEqn #-}
unfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldSchedEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.unfoldSched intu ints

{-# INLINE fairUnfoldSchedEqn #-}
fairUnfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
fairUnfoldSchedEqn maxVal input ints =
    let intu = Unfold.carryInput $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.fairUnfoldSched intu ints

-- GHC 9.14.1 cannot fuse InterleaveState, though 9.10 can
{-# ANN unfoldCrossBounded (PermitPatternMatches [''Maybe,''Int]) #-}
{-# ANN unfoldCrossBounded (PermitConstructions [''Int,''SrcLoc,''CallStack,''Maybe,''(,),''()]) #-}
{-# ANN unfoldCrossBounded (PermitTypeClasses [''IP]) #-}
{-# NOINLINE unfoldCrossBounded #-}
unfoldCrossBounded :: Int -> Int -> IO ()
unfoldCrossBounded maxVal _ = unfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

{-# ANN fairUnfoldCrossBounded (PermitPatternMatches [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldCrossBounded (PermitConstructions [''[],''Int,''SrcLoc,''CallStack,''EnumToState,''Maybe,''InterleaveState,''(,),''()]) #-}
{-# ANN fairUnfoldCrossBounded (PermitTypeClasses [''IP]) #-}
{-# NOINLINE fairUnfoldCrossBounded #-}
fairUnfoldCrossBounded :: Int -> Int -> IO ()
fairUnfoldCrossBounded maxVal _ = fairUnfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

{-# ANN fairUnfoldCrossInfinite (PermitPatternMatches [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldCrossInfinite (PermitConstructions [''Int,''EnumToState,''(,),''[],''SrcLoc,''CallStack,''Maybe,''InterleaveState,''()]) #-}
{-# ANN fairUnfoldCrossInfinite (PermitTypeClasses [''IP]) #-}
{-# NOINLINE fairUnfoldCrossInfinite #-}
fairUnfoldCrossInfinite :: Int -> Int -> IO ()
fairUnfoldCrossInfinite maxVal _ = fairUnfoldCrossEqn maxVal (infiniteIntsUnfold maxVal 0)

{-# ANN fairUnfoldEachBounded (PermitPatternMatches [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldEachBounded (PermitConstructions [''Maybe,''Int,''EnumToState,''InterleaveState,''(,),''[],''()]) #-}
{-# ANN fairUnfoldEachBounded (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldEachBounded #-}
fairUnfoldEachBounded :: Int -> Int -> IO ()
fairUnfoldEachBounded maxVal n =
    fairUnfoldEachEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

{-# ANN fairUnfoldEachInfinite (PermitPatternMatches [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldEachInfinite (PermitConstructions [''Int,''EnumToState,''(,),''Maybe,''InterleaveState,''[],''()]) #-}
{-# ANN fairUnfoldEachInfinite (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldEachInfinite #-}
fairUnfoldEachInfinite :: Int -> Int -> IO ()
fairUnfoldEachInfinite maxVal n =
    fairUnfoldEachEqn maxVal (infiniteIntsUnfold maxVal 0) (Type.infiniteInts maxVal n)

{-# ANN unfoldSchedBounded (PermitPatternMatches [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN unfoldSchedBounded (PermitConstructions [''Maybe,''Int,''EnumToState,''InterleaveState,''[],''(,),''()]) #-}
{-# ANN unfoldSchedBounded (PermitTypeClasses []) #-}
{-# NOINLINE unfoldSchedBounded #-}
unfoldSchedBounded :: Int -> Int -> IO ()
unfoldSchedBounded maxVal n =
    unfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

{-# ANN fairUnfoldSchedBounded (PermitPatternMatches [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldSchedBounded (PermitConstructions [''Maybe,''Int,''EnumToState,''InterleaveState,''(,),''[],''()]) #-}
{-# ANN fairUnfoldSchedBounded (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldSchedBounded #-}
fairUnfoldSchedBounded :: Int -> Int -> IO ()
fairUnfoldSchedBounded maxVal n =
    fairUnfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

{-# ANN fairUnfoldSchedInfinite (PermitPatternMatches [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldSchedInfinite (PermitConstructions [''Int,''EnumToState,''(,),''Maybe,''InterleaveState,''[],''()]) #-}
{-# ANN fairUnfoldSchedInfinite (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldSchedInfinite #-}
fairUnfoldSchedInfinite :: Int -> Int -> IO ()
fairUnfoldSchedInfinite maxVal n =
    fairUnfoldSchedEqn maxVal (infiniteIntsUnfold maxVal 0) (Type.infiniteInts maxVal n)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Solve simultaneous equations by exploring all possibilities
    -- Unfold
      [ (SpaceO_1, benchIO "equations/unfoldCross (bounded)" $ unfoldCrossBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldCross (bounded)" $ fairUnfoldCrossBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldCross (infinite)" $ fairUnfoldCrossInfinite sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldEach (bounded)" $ fairUnfoldEachBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldEach (infinite)" $ fairUnfoldEachInfinite sqrtVal)
      , (SpaceO_1, benchIO "equations/unfoldSched (bounded)" $ unfoldSchedBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldSched (bounded)" $ fairUnfoldSchedBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairUnfoldSched (infinite)" $ fairUnfoldSchedInfinite sqrtVal)
      ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
