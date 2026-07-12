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

import GHC.Types (SPEC(..))
import Control.Monad.State.Strict (StateT, get, put)
import Data.Functor.Identity (Identity(..), runIdentity)
import Stream.Common (sourceUnfoldr, sourceUnfoldrM)
import Stream.Type (benchIO, withPureStream)
import Streamly.Internal.Data.Stream (Stream, Step(..))

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Prelude hiding (reverse, tail)

#ifdef INSPECTION

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

{-# ANN evalStateTIO (PermitPatternMatches [''Int]) #-}
{-# ANN evalStateTIO (PermitConstructions [''Int,''()]) #-}
{-# ANN evalStateTIO (PermitTypeClasses []) #-}
{-# NOINLINE evalStateTIO #-}
evalStateTIO :: Int -> Int -> IO ()
evalStateTIO value n =
    Stream.fold Fold.drain (evalStateT value n :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'evalStateTIO
inspect $ 'evalStateTIO `hasNoType` ''Step
inspect $ 'evalStateTIO `hasNoType` ''Fold.Step
inspect $ 'evalStateTIO `hasNoType` ''SPEC
#endif

{-# ANN withStateIO (PermitPatternMatches [''Int]) #-}
{-# ANN withStateIO (PermitConstructions [''Int,''()]) #-}
{-# ANN withStateIO (PermitTypeClasses []) #-}
{-# NOINLINE withStateIO #-}
withStateIO :: Int -> Int -> IO ()
withStateIO value n =
    Stream.fold Fold.drain (withState value n :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'withStateIO
inspect $ 'withStateIO `hasNoType` ''Step
inspect $ 'withStateIO `hasNoType` ''Fold.Step
inspect $ 'withStateIO `hasNoType` ''SPEC
#endif

-- NOTE: eta expansion is required to eliminate Step pattern match
{-# ANN generalizeInner (PermitPatternMatches [''Int]) #-}
{-# ANN generalizeInner (PermitConstructions [''Int]) #-}
{-# ANN generalizeInner (PermitTypeClasses []) #-}
{-# NOINLINE generalizeInner #-}
generalizeInner :: Int -> Int -> IO Int
generalizeInner value n =
    (withPureStream value $
        runIdentity . Stream.fold Fold.length . Stream.generalizeInner) n

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'generalizeInner
inspect $ 'generalizeInner `hasNoType` ''Step
inspect $ 'generalizeInner `hasNoType` ''Fold.Step
inspect $ 'generalizeInner `hasNoType` ''SPEC
#endif

{-# ANN generalizeInnerIO (PermitPatternMatches []) #-}
{-# ANN generalizeInnerIO (PermitConstructions [''Int]) #-}
{-# ANN generalizeInnerIO (PermitTypeClasses []) #-}
{-# NOINLINE generalizeInnerIO #-}
generalizeInnerIO :: Int -> Int -> IO Int
generalizeInnerIO value n =
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
