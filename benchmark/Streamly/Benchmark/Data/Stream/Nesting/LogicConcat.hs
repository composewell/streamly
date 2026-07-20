-- |
-- Module      : Stream.Nesting.LogicConcat
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Nesting.LogicConcat (benchmarks) where

import Streamly.Data.Stream (Stream)

import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.SVar.Type as SVar

import Test.Tasty.Bench
import Stream.Type (benchIO)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import qualified Stream.Type as Type
import Prelude hiding (concatMap, zipWith)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- In bounded case, the x stream is 0 to maxVal and y stream is -1 to -maxVal.
-- The solution of the equation is x = maxVal y = -maxVal, so in the worst case
-- we get to the solution only after exhausting both the streams.
--
-- In the infinite stream case we terminate after we get to the solution or
-- both streams go beyond maxVal, in this case if one stream is explored more
-- then we might go through more than maxVal x maxVal cases.
--
{-# INLINE checkStreamK #-}
checkStreamK :: Int -> Int -> Int
    -> StreamK.StreamK m (Maybe (Maybe (Int, Int)))
checkStreamK maxVal x y =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then StreamK.fromPure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then StreamK.fromPure (Just Nothing)
        else StreamK.fromPure Nothing

{-# INLINE fairConcatForEqn #-}
fairConcatForEqn :: Monad m => Int -> Stream m Int -> m ()
fairConcatForEqn maxVal input =
    Type.result
        $ Stream.fairConcatFor input $ \x ->
              Stream.fairConcatForM input $ \y -> do
                return $ Type.checkStream maxVal x y

{-# INLINE fairConcatForEqnK #-}
fairConcatForEqnK :: Monad m => Int -> Stream m Int -> m ()
fairConcatForEqnK maxVal input =
    let inputK = StreamK.fromStream input
    in Type.result
        $ StreamK.toStream
        $ StreamK.fairConcatFor inputK $ \x ->
              StreamK.fairConcatForM inputK $ \y -> do
                return $ checkStreamK maxVal x y

{-# INLINE fairSchedForEqn #-}
fairSchedForEqn :: Monad m => Int -> Stream m Int -> m ()
fairSchedForEqn maxVal input =
    Type.result
        $ Stream.fairSchedFor input $ \x ->
              Stream.fairSchedForM input $ \y -> do
                return $ Type.checkStream maxVal x y

_schedForEqn :: Monad m => Int -> Stream m Int -> m ()
_schedForEqn maxVal input =
    Type.result
        $ Stream.schedFor input $ \x ->
              Stream.schedForM input $ \y -> do
                return $ Type.checkStream maxVal x y

{-# ANN fairConcatFor_Bounded (PermitPatternMatches
    [''Maybe,''Bool,''Int,''[],''Producer.InterleaveState
    ,''Stream.EnumToState,''Stream.Step,''Stream.FairUnfoldState,''Stream]) #-}
{-# ANN fairConcatFor_Bounded (PermitConstructions
    [''Producer.InterleaveState,''Stream.EnumToState,''Int,''Maybe
    ,''Stream.Step,''Stream,''(,),''[],''Stream.FairUnfoldState
    ,''SVar.State,''(),''Bool]) #-}
{-# ANN fairConcatFor_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE fairConcatFor_Bounded #-}
fairConcatFor_Bounded :: Int -> Int -> IO ()
fairConcatFor_Bounded maxVal n =
    fairConcatForEqn maxVal (Type.boundedInts maxVal n)

{-# ANN fairConcatForK_Bounded (PermitPatternMatches
    [''Maybe,''Bool,''Int,''[],''Producer.InterleaveState,''SVar.State
    ,''Stream.EnumToState,''Stream.Step]) #-}
{-# ANN fairConcatForK_Bounded (PermitConstructions
    [''Maybe,''[],''Producer.InterleaveState,''Int,''Stream.EnumToState
    ,''SVar.State,''Stream.Step,''(,),''(),''Bool]) #-}
{-# ANN fairConcatForK_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE fairConcatForK_Bounded #-}
fairConcatForK_Bounded :: Int -> Int -> IO ()
fairConcatForK_Bounded maxVal n =
    fairConcatForEqnK maxVal (Type.boundedInts maxVal n)

{-# ANN fairConcatFor_Infinite (PermitPatternMatches
    [''Maybe,''Bool,''Int,''[],''Producer.InterleaveState
    ,''Stream.EnumToState,''Stream.Step,''Stream.FairUnfoldState,''Stream]) #-}
{-# ANN fairConcatFor_Infinite (PermitConstructions
    [''Int,''Producer.InterleaveState,''Stream.EnumToState,''Maybe
    ,''Stream.Step,''Stream,''(,),''[],''Stream.FairUnfoldState
    ,''SVar.State,''(),''Bool]) #-}
{-# ANN fairConcatFor_Infinite (PermitTypeClasses []) #-}
{-# NOINLINE fairConcatFor_Infinite #-}
fairConcatFor_Infinite :: Int -> Int -> IO ()
fairConcatFor_Infinite maxVal n =
    fairConcatForEqn maxVal (Type.infiniteInts maxVal n)

{-# ANN fairSchedFor_Bounded (PermitPatternMatches
    [''Maybe,''Bool,''Int,''[],''Producer.InterleaveState
    ,''Stream.EnumToState,''Stream.Step,''Stream.FairUnfoldState,''Stream]) #-}
{-# ANN fairSchedFor_Bounded (PermitConstructions
    [''Producer.InterleaveState,''Stream.EnumToState,''Int,''Maybe
    ,''Stream.Step,''Stream,''(,),''[],''Stream.FairUnfoldState
    ,''SVar.State,''(),''Bool]) #-}
{-# ANN fairSchedFor_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE fairSchedFor_Bounded #-}
fairSchedFor_Bounded :: Int -> Int -> IO ()
fairSchedFor_Bounded maxVal n =
    fairSchedForEqn maxVal (Type.boundedInts maxVal n)

{-# ANN fairSchedFor_Infinite (PermitPatternMatches
    [''Maybe,''Bool,''Int,''[],''Producer.InterleaveState
    ,''Stream.EnumToState,''Stream.Step,''Stream.FairUnfoldState,''Stream]) #-}
{-# ANN fairSchedFor_Infinite (PermitConstructions
    [''Int,''Producer.InterleaveState,''Stream.EnumToState,''Maybe
    ,''Stream.Step,''Stream,''(,),''[],''Stream.FairUnfoldState
    ,''SVar.State,''(),''Bool]) #-}
{-# ANN fairSchedFor_Infinite (PermitTypeClasses []) #-}
{-# NOINLINE fairSchedFor_Infinite #-}
fairSchedFor_Infinite :: Int -> Int -> IO ()
fairSchedFor_Infinite maxVal n =
    fairSchedForEqn maxVal (Type.infiniteInts maxVal n)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- XXX Move StreamK functions to StreamK module
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Solve simultaneous equations by exploring all possibilities
    -- Concat
      [ (SpaceO_1, benchIO "fairConcatFor_Bounded (equations)" $
            fairConcatFor_Bounded sqrtVal)
      , (SpaceO_1, benchIO "fairConcatForK_Bounded (equations)" $
            fairConcatForK_Bounded sqrtVal)
      , (SpaceO_1, benchIO "fairConcatFor_Infinite (equations)" $
            fairConcatFor_Infinite sqrtVal)
      , (SpaceO_1, benchIO "fairSchedFor_Bounded (equations)" $
            fairSchedFor_Bounded sqrtVal)
      , (SpaceO_1, benchIO "fairSchedFor_Infinite (equations)" $
            fairSchedFor_Infinite sqrtVal)
      ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
