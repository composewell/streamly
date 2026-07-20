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
fairUnfoldEachEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int
    -> m ()
fairUnfoldEachEqn maxVal input ints =
    let intu =
            Unfold.carryInput
                $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.fairUnfoldEach intu ints

{-# INLINE unfoldSchedEqn #-}
unfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int
    -> m ()
unfoldSchedEqn maxVal input ints =
    let intu =
            Unfold.carryInput
                $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.unfoldSched intu ints

{-# INLINE fairUnfoldSchedEqn #-}
fairUnfoldSchedEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int
    -> m ()
fairUnfoldSchedEqn maxVal input ints =
    let intu =
            Unfold.carryInput
                $ Unfold.lmap (const (undefined, undefined)) input
     in Type.result
        $ Stream.mapM (Type.checkPair maxVal)
        $ Stream.fairUnfoldSched intu ints

-- GHC 9.14.1 cannot fuse InterleaveState, though 9.10 can
{-# ANN cross_Bounded_Unfold (PermitPatternMatches [''Maybe,''Int]) #-}
{-# ANN cross_Bounded_Unfold (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''Maybe,''(,),''()]) #-}
{-# ANN cross_Bounded_Unfold (PermitTypeClasses [''IP]) #-}
{-# NOINLINE cross_Bounded_Unfold #-}
cross_Bounded_Unfold :: Int -> Int -> IO ()
cross_Bounded_Unfold maxVal _ =
    unfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

{-# ANN fairCross_Bounded_Unfold (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairCross_Bounded_Unfold (PermitConstructions
    [''[],''Int,''SrcLoc,''CallStack,''EnumToState,''Maybe
    ,''InterleaveState,''(,),''()]) #-}
{-# ANN fairCross_Bounded_Unfold (PermitTypeClasses [''IP]) #-}
{-# NOINLINE fairCross_Bounded_Unfold #-}
fairCross_Bounded_Unfold :: Int -> Int -> IO ()
fairCross_Bounded_Unfold maxVal _ =
    fairUnfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

{-# ANN fairCross_Infinite_Unfold (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairCross_Infinite_Unfold (PermitConstructions
    [''Int,''EnumToState,''(,),''[],''SrcLoc,''CallStack,''Maybe
    ,''InterleaveState,''()]) #-}
{-# ANN fairCross_Infinite_Unfold (PermitTypeClasses [''IP]) #-}
{-# NOINLINE fairCross_Infinite_Unfold #-}
fairCross_Infinite_Unfold :: Int -> Int -> IO ()
fairCross_Infinite_Unfold maxVal _ =
    fairUnfoldCrossEqn maxVal (infiniteIntsUnfold maxVal 0)

{-# ANN fairUnfoldEach_Bounded (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldEach_Bounded (PermitConstructions
    [''Maybe,''Int,''EnumToState,''InterleaveState,''(,),''[],''()]) #-}
{-# ANN fairUnfoldEach_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldEach_Bounded #-}
fairUnfoldEach_Bounded :: Int -> Int -> IO ()
fairUnfoldEach_Bounded maxVal n =
    fairUnfoldEachEqn maxVal (Type.boundedIntsUnfold maxVal 0)
        (Type.boundedInts maxVal n)

{-# ANN fairUnfoldEach_Infinite (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldEach_Infinite (PermitConstructions
    [''Int,''EnumToState,''(,),''Maybe,''InterleaveState,''[],''()]) #-}
{-# ANN fairUnfoldEach_Infinite (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldEach_Infinite #-}
fairUnfoldEach_Infinite :: Int -> Int -> IO ()
fairUnfoldEach_Infinite maxVal n =
    fairUnfoldEachEqn maxVal (infiniteIntsUnfold maxVal 0)
        (Type.infiniteInts maxVal n)

{-# ANN unfoldSched_Bounded (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN unfoldSched_Bounded (PermitConstructions
    [''Maybe,''Int,''EnumToState,''InterleaveState,''[],''(,),''()]) #-}
{-# ANN unfoldSched_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE unfoldSched_Bounded #-}
unfoldSched_Bounded :: Int -> Int -> IO ()
unfoldSched_Bounded maxVal n =
    unfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0)
        (Type.boundedInts maxVal n)

{-# ANN fairUnfoldSched_Bounded (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldSched_Bounded (PermitConstructions
    [''Maybe,''Int,''EnumToState,''InterleaveState,''(,),''[],''()]) #-}
{-# ANN fairUnfoldSched_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldSched_Bounded #-}
fairUnfoldSched_Bounded :: Int -> Int -> IO ()
fairUnfoldSched_Bounded maxVal n =
    fairUnfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0)
        (Type.boundedInts maxVal n)

{-# ANN fairUnfoldSched_Infinite (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''EnumToState,''InterleaveState]) #-}
{-# ANN fairUnfoldSched_Infinite (PermitConstructions
    [''Int,''EnumToState,''(,),''Maybe,''InterleaveState,''[],''()]) #-}
{-# ANN fairUnfoldSched_Infinite (PermitTypeClasses []) #-}
{-# NOINLINE fairUnfoldSched_Infinite #-}
fairUnfoldSched_Infinite :: Int -> Int -> IO ()
fairUnfoldSched_Infinite maxVal n =
    fairUnfoldSchedEqn maxVal (infiniteIntsUnfold maxVal 0)
        (Type.infiniteInts maxVal n)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Solve simultaneous equations by exploring all possibilities
    -- Unfold
    -- XXX Move the Unfold benchmarks to the Unfold module
      [ (SpaceO_1, benchIO "cross_Bounded_Unfold (Unfold equations)" $
            cross_Bounded_Unfold sqrtVal)
      , (SpaceO_1, benchIO "fairCross_Bounded_Unfold (Unfold equations)" $
            fairCross_Bounded_Unfold sqrtVal)
      , (SpaceO_1, benchIO "fairCross_Infinite_Unfold (Unfold equations)" $
            fairCross_Infinite_Unfold sqrtVal)
      , (SpaceO_1, benchIO "fairUnfoldEach_Bounded (equations)" $
            fairUnfoldEach_Bounded sqrtVal)
      , (SpaceO_1, benchIO "fairUnfoldEach_Infinite (equations)" $
            fairUnfoldEach_Infinite sqrtVal)
      , (SpaceO_1, benchIO "unfoldSched_Bounded (equations)" $
            unfoldSched_Bounded sqrtVal)
      , (SpaceO_1, benchIO "fairUnfoldSched_Bounded (equations)" $
            fairUnfoldSched_Bounded sqrtVal)
      , (SpaceO_1, benchIO "fairUnfoldSched_Infinite (equations)" $
            fairUnfoldSched_Infinite sqrtVal)
      ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
