-- |
-- Module      : Stream.Type.Logic
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | Logic programming style benchmarks: solving simultaneous equations by
-- exploring all possibilities using the cross product combinators.
module Stream.Type.Logic
    ( benchmarks
    , boundedInts
    , infiniteInts
    , boundedIntsUnfold
    , checkStream
    , checkPair
    , result
    ) where

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Tasty.Bench
import Stream.Type.Basic (benchIO)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import qualified Streamly.Internal.Data.SVar.Type as SVar
import Prelude hiding (concatMap, mapM, zipWith)

-- search space |x| = 1000, |y| = 1000
{-# INLINE boundedInts #-}
boundedInts :: Monad m => Int -> Int -> Stream m Int
boundedInts n _ =
    Stream.interleave
        (Stream.enumerateFromTo (0 :: Int) n)
        (Stream.enumerateDownFromToNum (-1) (-n))

{-# INLINE infiniteInts #-}
infiniteInts :: Monad m => Int -> Int -> Stream m Int
infiniteInts _ _ =
    Stream.interleave
        (Stream.enumerateFrom (0 :: Int))
        (Stream.enumerateDownFromNum (-1))

{-# INLINE boundedIntsUnfold #-}
boundedIntsUnfold :: Monad m => Int -> Int -> Unfold m ((), ()) Int
boundedIntsUnfold n _ =
    Unfold.interleave
        (Unfold.supply (0 :: Int, n) Unfold.enumerateFromTo)
        (Unfold.supply (-1, -n) Unfold.enumerateDownFromToNum)

{-# INLINE checkStream #-}
checkStream :: Applicative m =>
    Int -> Int -> Int -> Stream m (Maybe (Maybe (Int, Int)))
checkStream maxVal x y =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then Stream.fromPure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then Stream.fromPure (Just Nothing)
        else Stream.fromPure Nothing

{-# INLINE checkPair #-}
checkPair :: Monad m => Int -> (Int, Int) -> m (Maybe (Maybe (Int, Int)))
checkPair maxVal (x, y) =
    let eq1 = x + y == 0
        eq2 = x - y == 2 * maxVal
     in if eq1 && eq2
        then pure (Just (Just (x,y)))
        else if abs x > maxVal && abs y > maxVal
        then pure (Just Nothing)
        else pure Nothing

-- Terminate the stream as soon as we get a Just value
{-# INLINE result #-}
result :: Monad m => Stream m (Maybe a) -> m ()
result = Stream.fold (Fold.take 1 Fold.drain) . Stream.catMaybes

{-# INLINE concatForEqn #-}
concatForEqn :: Monad m => Int -> Stream m Int -> m ()
concatForEqn maxVal input =
    result
        $ Stream.concatFor input $ \x ->
              Stream.concatForM input $ \y -> do
                return $ checkStream maxVal x y

{-# INLINE streamCrossEqn #-}
streamCrossEqn :: Monad m => Int -> Stream m Int -> m ()
streamCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.cross input input

{-# INLINE fairStreamCrossEqn #-}
fairStreamCrossEqn :: Monad m => Int -> Stream m Int -> m ()
fairStreamCrossEqn maxVal input =
    result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.fairCross input input

{-# INLINE unfoldEachEqn #-}
unfoldEachEqn :: Monad m => Int -> Unfold m ((), ()) Int -> Stream m Int -> m ()
unfoldEachEqn maxVal input ints =
    let intu =
            Unfold.carryInput
                $ Unfold.lmap (const (undefined, undefined)) input
     in result
        $ Stream.mapM (checkPair maxVal)
        $ Stream.unfoldEach intu ints

{-# ANN concatFor_Bounded (PermitPatternMatches
    [''Either,''Maybe,''(,),''Bool,''Int,''Producer.InterleaveState
    ,''Stream.EnumToState,''Stream.Step,''Stream]) #-}
{-# ANN concatFor_Bounded (PermitConstructions
    [''Producer.InterleaveState,''Stream.EnumToState,''Either,''Int
    ,''Maybe,''Stream.Step,''Stream,''(,),''SVar.State,''(),''Bool]) #-}
{-# ANN concatFor_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_Bounded #-}
concatFor_Bounded :: Int -> Int -> IO ()
concatFor_Bounded maxVal n =
    concatForEqn maxVal (boundedInts maxVal n)

{-# ANN cross_Bounded (PermitPatternMatches [''Maybe,''Int]) #-}
{-# ANN cross_Bounded (PermitConstructions [''Maybe,''Int,''(,),''()]) #-}
{-# ANN cross_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE cross_Bounded #-}
cross_Bounded :: Int -> Int -> IO ()
cross_Bounded maxVal n =
    streamCrossEqn maxVal (boundedInts maxVal n)

{-# ANN fairCross_Bounded (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''Producer.InterleaveState
    ,''Stream.EnumToState]) #-}
{-# ANN fairCross_Bounded (PermitConstructions
    [''Maybe,''Int,''Stream.EnumToState,''Producer.InterleaveState,''(,)
    ,''[],''()]) #-}
{-# ANN fairCross_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE fairCross_Bounded #-}
fairCross_Bounded :: Int -> Int -> IO ()
fairCross_Bounded maxVal n =
    fairStreamCrossEqn maxVal (boundedInts maxVal n)

{-# ANN fairCross_Infinite (PermitPatternMatches
    [''Maybe,''(,),''Int,''[],''Producer.InterleaveState
    ,''Stream.EnumToState]) #-}
{-# ANN fairCross_Infinite (PermitConstructions
    [''Int,''Maybe,''Producer.InterleaveState,''(,),''[]
    ,''Stream.EnumToState,''()]) #-}
{-# ANN fairCross_Infinite (PermitTypeClasses []) #-}
{-# NOINLINE fairCross_Infinite #-}
fairCross_Infinite :: Int -> Int -> IO ()
fairCross_Infinite maxVal n =
    fairStreamCrossEqn maxVal (infiniteInts maxVal n)

{-# ANN unfoldEach_Bounded (PermitPatternMatches [''Maybe,''Int]) #-}
{-# ANN unfoldEach_Bounded (PermitConstructions [''Maybe,''Int,''(,),''()]) #-}
{-# ANN unfoldEach_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach_Bounded #-}
unfoldEach_Bounded :: Int -> Int -> IO ()
unfoldEach_Bounded maxVal n =
    unfoldEachEqn maxVal (boundedIntsUnfold maxVal 0) (boundedInts maxVal n)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

{-# ANN benchmarks "HLint: ignore" #-}
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Logic Programming
    -- Solve simultaneous equations by exploring all possibilities
    [ (SpaceO_1, benchIO "concatFor_Bounded (equations)" $
          concatFor_Bounded sqrtVal)
    , (SpaceO_1, benchIO "cross_Bounded (equations)" $
          cross_Bounded sqrtVal)
    , (SpaceO_1, benchIO "fairCross_Bounded (equations)" $
          fairCross_Bounded sqrtVal)
    , (SpaceO_1, benchIO "fairCross_Infinite (equations)" $
          fairCross_Infinite sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach_Bounded (equations)" $
          unfoldEach_Bounded sqrtVal)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double) -- double nested loop
