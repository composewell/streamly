-- |
-- Module      : Stream.Nesting.LogicUnfold
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Stream.Nesting.LogicUnfold (benchmarks) where

import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Type (benchIO, withRandomIntIO)
import Streamly.Benchmark.Common
import qualified Stream.Type as Type
import Prelude hiding (concatMap, zipWith)

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

{-# INLINE infiniteIntsUnfold #-}
infiniteIntsUnfold :: Monad m => Int -> Int -> Unfold m ((), ()) Int
infiniteIntsUnfold _ _ =
    Unfold.interleave
        (Unfold.supply (0 :: Int) Unfold.enumerateFrom)
        (Unfold.supply (-1, -2) Unfold.enumerateFromThen)

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

unfoldCrossBounded :: Int -> IO ()
unfoldCrossBounded maxVal = unfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

fairUnfoldCrossBounded :: Int -> IO ()
fairUnfoldCrossBounded maxVal = fairUnfoldCrossEqn maxVal (Type.boundedIntsUnfold maxVal 0)

fairUnfoldCrossInfinite :: Int -> IO ()
fairUnfoldCrossInfinite maxVal = fairUnfoldCrossEqn maxVal (infiniteIntsUnfold maxVal 0)

fairUnfoldEachBounded :: Int -> IO ()
fairUnfoldEachBounded maxVal = withRandomIntIO $ \n ->
    fairUnfoldEachEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

fairUnfoldEachInfinite :: Int -> IO ()
fairUnfoldEachInfinite maxVal = withRandomIntIO $ \n ->
    fairUnfoldEachEqn maxVal (infiniteIntsUnfold maxVal 0) (Type.infiniteInts maxVal n)

unfoldSchedBounded :: Int -> IO ()
unfoldSchedBounded maxVal = withRandomIntIO $ \n ->
    unfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

fairUnfoldSchedBounded :: Int -> IO ()
fairUnfoldSchedBounded maxVal = withRandomIntIO $ \n ->
    fairUnfoldSchedEqn maxVal (Type.boundedIntsUnfold maxVal 0) (Type.boundedInts maxVal n)

fairUnfoldSchedInfinite :: Int -> IO ()
fairUnfoldSchedInfinite maxVal = withRandomIntIO $ \n ->
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
