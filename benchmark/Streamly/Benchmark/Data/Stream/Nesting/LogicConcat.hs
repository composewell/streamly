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

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK

import Test.Tasty.Bench
import Stream.Type (benchIO, withRandomIntIO)
import Streamly.Benchmark.Common
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
checkStreamK :: Int -> Int -> Int -> StreamK.StreamK m (Maybe (Maybe (Int, Int)))
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

fairConcatForBounded :: Int -> IO ()
fairConcatForBounded maxVal = withRandomIntIO $ \n ->
    fairConcatForEqn maxVal (Type.boundedInts maxVal n)

fairConcatForKBounded :: Int -> IO ()
fairConcatForKBounded maxVal = withRandomIntIO $ \n ->
    fairConcatForEqnK maxVal (Type.boundedInts maxVal n)

fairConcatForInfinite :: Int -> IO ()
fairConcatForInfinite maxVal = withRandomIntIO $ \n ->
    fairConcatForEqn maxVal (Type.infiniteInts maxVal n)

fairSchedForBounded :: Int -> IO ()
fairSchedForBounded maxVal = withRandomIntIO $ \n ->
    fairSchedForEqn maxVal (Type.boundedInts maxVal n)

fairSchedForInfinite :: Int -> IO ()
fairSchedForInfinite maxVal = withRandomIntIO $ \n ->
    fairSchedForEqn maxVal (Type.infiniteInts maxVal n)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Solve simultaneous equations by exploring all possibilities
    -- Concat
      [ (SpaceO_1, benchIO "equations/fairConcatFor (bounded)" $ fairConcatForBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairConcatForK (bounded)" $ fairConcatForKBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairConcatFor (infinite)" $ fairConcatForInfinite sqrtVal)
      , (SpaceO_1, benchIO "equations/fairSchedFor (bounded)" $ fairSchedForBounded sqrtVal)
      , (SpaceO_1, benchIO "equations/fairSchedFor (infinite)" $ fairSchedForInfinite sqrtVal)
      ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
