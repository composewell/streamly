-- |
-- Module      : Stream.Lift
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Lift (benchmarks) where

import Control.DeepSeq (NFData(..))
import Control.Monad.State.Strict (StateT, get, put)
import Stream.Common (sourceUnfoldr, sourceUnfoldrM)
import System.Random (randomRIO)
import Streamly.Internal.Data.Stream (Stream)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Prelude hiding (reverse, tail)

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))

import GHC.Types (SPEC(..))
import Test.Inspection
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withRandomIntIO #-}
withRandomIntIO :: (Int -> IO b) -> IO b
withRandomIntIO f = randomRIO (1, 1 :: Int) >>= f

-------------------------------------------------------------------------------
-- Monad transformation (hoisting etc.)
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrState #-}
sourceUnfoldrState :: Common.MonadAsync m =>
    Int -> Int -> Stream (StateT Int m) Int
sourceUnfoldrState value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else do
            s <- get
            put (s + 1)
            return (Just (s, cnt + 1))

{-# INLINE evalStateT #-}
evalStateT :: Common.MonadAsync m => Int -> Int -> Stream m Int
evalStateT value n =
    Stream.evalStateT (return 0) (sourceUnfoldrState value n)

{-# INLINE withState #-}
withState :: Common.MonadAsync m => Int -> Int -> Stream m Int
withState value n =
    Stream.evalStateT
        (return (0 :: Int)) (Stream.liftInner (sourceUnfoldrM value n))

{-# INLINE evalStateTIO #-}
evalStateTIO :: Int -> IO ()
evalStateTIO value = withRandomIntIO $ \n ->
    Stream.fold Fold.drain (evalStateT value n :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'evalStateTIO
inspect $ 'evalStateTIO `hasNoType` ''Step
inspect $ 'evalStateTIO `hasNoType` ''Fold.Step
inspect $ 'evalStateTIO `hasNoType` ''SPEC
#endif

{-# INLINE withStateIO #-}
withStateIO :: Int -> IO ()
withStateIO value = withRandomIntIO $ \n ->
    Stream.fold Fold.drain (withState value n :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'withStateIO
inspect $ 'withStateIO `hasNoType` ''Step
inspect $ 'withStateIO `hasNoType` ''Fold.Step
inspect $ 'withStateIO `hasNoType` ''SPEC
#endif

{-# INLINE generalizeInnerIO #-}
generalizeInnerIO :: Int -> IO Int
generalizeInnerIO value = withRandomIntIO $ \n ->
    Stream.fold Fold.length
        (Stream.generalizeInner (sourceUnfoldr value n) :: Stream IO Int)

o_1_space_hoisting :: Int -> [Benchmark]
o_1_space_hoisting value =
    [ bgroup "hoisting"
        [ benchIO "evalState" $ evalStateTIO value
        , benchIO "withState" $ withStateIO value
        , benchIO "generalizeInner" $ generalizeInnerIO value
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    map (SpaceO_1,) (o_1_space_hoisting size)
