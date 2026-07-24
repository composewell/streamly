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

{-# ANN evalStateT (PermitPatternMatches [''Int]) #-}
{-# ANN evalStateT (PermitConstructions [''Int,''()]) #-}
{-# ANN evalStateT (PermitTypeClasses []) #-}
{-# NOINLINE evalStateT #-}
evalStateT :: Int -> Int -> IO ()
evalStateT value n =
    Stream.fold Fold.drain
        (Stream.evalStateT (return 0) (sourceUnfoldrState value n)
            :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'evalStateT
inspect $ 'evalStateT `hasNoType` ''Step
inspect $ 'evalStateT `hasNoType` ''Fold.Step
inspect $ 'evalStateT `hasNoType` ''SPEC
#endif

{-# ANN liftInner (PermitPatternMatches [''Int]) #-}
{-# ANN liftInner (PermitConstructions [''Int,''()]) #-}
{-# ANN liftInner (PermitTypeClasses []) #-}
{-# NOINLINE liftInner #-}
liftInner :: Int -> Int -> IO ()
liftInner value n =
    Stream.fold Fold.drain
        (Stream.evalStateT
            (return (0 :: Int)) (Stream.liftInner (sourceUnfoldrM value n))
            :: Stream IO Int)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'liftInner
inspect $ 'liftInner `hasNoType` ''Step
inspect $ 'liftInner `hasNoType` ''Fold.Step
inspect $ 'liftInner `hasNoType` ''SPEC
#endif

-- NOTE: eta expansion is required to eliminate Step pattern match
{-# ANN generalizeInner_Identity (PermitPatternMatches []) #-}
{-# ANN generalizeInner_Identity (PermitConstructions [''Int]) #-}
{-# ANN generalizeInner_Identity (PermitTypeClasses []) #-}
{-# NOINLINE generalizeInner_Identity #-}
generalizeInner_Identity :: Int -> Int -> IO Int
generalizeInner_Identity value n =
    (withPureStream value $
        runIdentity . Stream.fold Fold.length . Stream.generalizeInner) n

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'generalizeInner_Identity
inspect $ 'generalizeInner_Identity `hasNoType` ''Step
inspect $ 'generalizeInner_Identity `hasNoType` ''Fold.Step
inspect $ 'generalizeInner_Identity `hasNoType` ''SPEC
#endif

{-# ANN generalizeInner_IO (PermitPatternMatches []) #-}
{-# ANN generalizeInner_IO (PermitConstructions [''Int]) #-}
{-# ANN generalizeInner_IO (PermitTypeClasses []) #-}
{-# NOINLINE generalizeInner_IO #-}
generalizeInner_IO :: Int -> Int -> IO Int
generalizeInner_IO value n =
    Stream.fold Fold.length
        (Stream.generalizeInner (sourceUnfoldr value n) :: Stream IO Int)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
      [ (SpaceO_1, benchIO "evalStateT" $ evalStateT size)
      , (SpaceO_1, benchIO "liftInner" $ liftInner size)
      , (SpaceO_1, benchIO "generalizeInner_Identity" $
            generalizeInner_Identity size)
      , (SpaceO_1, benchIO "generalizeInner_IO" $ generalizeInner_IO size)
      ]
