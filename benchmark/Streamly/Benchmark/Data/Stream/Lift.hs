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

import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity(..), runIdentity)
import Stream.Common (sourceUnfoldr, sourceUnfoldrM)
import Stream.Type (benchIO, withPureStream, withRandomIntIO)
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

evalStateTIO :: Int -> IO ()
evalStateTIO value = withRandomIntIO $ \n ->
    Stream.fold Fold.drain (evalStateT value n :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'evalStateTIO
inspect $ 'evalStateTIO `hasNoType` ''Step
inspect $ 'evalStateTIO `hasNoType` ''Fold.Step
inspect $ 'evalStateTIO `hasNoType` ''SPEC
#endif

withStateIO :: Int -> IO ()
withStateIO value = withRandomIntIO $ \n ->
    Stream.fold Fold.drain (withState value n :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'withStateIO
inspect $ 'withStateIO `hasNoType` ''Step
inspect $ 'withStateIO `hasNoType` ''Fold.Step
inspect $ 'withStateIO `hasNoType` ''SPEC
#endif

generalizeInner :: Int -> IO Int
generalizeInner value =
    withPureStream value $
        runIdentity . Stream.fold Fold.length . Stream.generalizeInner

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'generalizeInner
inspect $ 'generalizeInner `hasNoType` ''Step
inspect $ 'generalizeInner `hasNoType` ''Fold.Step
inspect $ 'generalizeInner `hasNoType` ''SPEC
#endif

generalizeInnerIO :: Int -> IO Int
generalizeInnerIO value = withRandomIntIO $ \n ->
    Stream.fold Fold.length
        (Stream.generalizeInner (sourceUnfoldr value n) :: Stream IO Int)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
      [ (SpaceO_1, benchIO "evalState" $ evalStateTIO size)
      , (SpaceO_1, benchIO "withState" $ withStateIO size)
      , (SpaceO_1, benchIO "length . generalizeInner" $ generalizeInner size)
      , (SpaceO_1, benchIO "generalizeInner" $ generalizeInnerIO size)
      ]
