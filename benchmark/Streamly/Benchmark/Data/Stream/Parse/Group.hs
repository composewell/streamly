-- |
-- Module      : Stream.Parse
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -fforce-recomp #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Parse.Group (benchmarks) where

#ifdef INSPECTION
import Test.Inspection
#endif

import GHC.Types (SPEC(..))
import Data.Monoid (Sum(..))

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as S

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import GHC.Stack (SrcLoc, CallStack)
import GHC.Classes (IP)
import Stream.Type (benchIO, withStream)

-------------------------------------------------------------------------------
-- Grouping transformations
-------------------------------------------------------------------------------

-- XXX use errorWithoutStackTrace to get rid of IP/srcLoc/CallStack
{-# ANN groupsWhile_LT (PermitPatternMatches [''IO,''Int,''S.GroupByState]) #-}
{-# ANN groupsWhile_LT (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''S.GroupByState,''()]) #-}
{-# ANN groupsWhile_LT (PermitTypeClasses [''IP]) #-}
{-# NOINLINE groupsWhile_LT #-}
groupsWhile_LT :: Int -> Int -> IO ()
groupsWhile_LT value =
    withStream value $ Common.drain . S.groupsWhile (<) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsWhile_LT
-- XXX fails but can't find the type in the core printed
-- inspect $ 'groupsWhile_LT `hasNoType` ''S.Step
inspect $ 'groupsWhile_LT `hasNoType` ''FL.Step
inspect $ 'groupsWhile_LT `hasNoType` ''SPEC
#endif

{-# ANN groupsWhile_Eq (PermitPatternMatches [''IO,''Int,''S.GroupByState]) #-}
{-# ANN groupsWhile_Eq (PermitConstructions
    [''Int,''SrcLoc,''CallStack,''S.GroupByState,''()]) #-}
{-# ANN groupsWhile_Eq (PermitTypeClasses [''IP]) #-}
{-# NOINLINE groupsWhile_Eq #-}
groupsWhile_Eq :: Int -> Int -> IO ()
groupsWhile_Eq value =
    withStream value $ Common.drain . S.groupsWhile (==) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsWhile_Eq
-- XXX fails but can't find the type in the core printed
-- inspect $ 'groupsWhile_Eq `hasNoType` ''S.Step
inspect $ 'groupsWhile_Eq `hasNoType` ''FL.Step
inspect $ 'groupsWhile_Eq `hasNoType` ''SPEC
#endif

{-# ANN groupsRollingBy_LT (PermitPatternMatches [''Int,''S.GroupByState]) #-}
{-# ANN groupsRollingBy_LT (PermitConstructions
    [''S.GroupByState,''Int,''()]) #-}
{-# ANN groupsRollingBy_LT (PermitTypeClasses []) #-}
{-# NOINLINE groupsRollingBy_LT #-}
groupsRollingBy_LT :: Int -> Int -> IO ()
groupsRollingBy_LT value =
    withStream value $ Common.drain . S.groupsRollingBy (<) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsRollingBy_LT
inspect $ 'groupsRollingBy_LT `hasNoType` ''S.Step
-- inspect $ 'groupsRollingBy_LT `hasNoType` ''S.GroupByState
inspect $ 'groupsRollingBy_LT `hasNoType` ''FL.Step
inspect $ 'groupsRollingBy_LT `hasNoType` ''SPEC
#endif

{-# ANN groupsRollingBy_Eq (PermitPatternMatches [''Int,''S.GroupByState]) #-}
{-# ANN groupsRollingBy_Eq (PermitConstructions
    [''S.GroupByState,''Int,''()]) #-}
{-# ANN groupsRollingBy_Eq (PermitTypeClasses []) #-}
{-# NOINLINE groupsRollingBy_Eq #-}
groupsRollingBy_Eq :: Int -> Int -> IO ()
groupsRollingBy_Eq value =
    withStream value $ Common.drain . S.groupsRollingBy (==) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsRollingBy_Eq
inspect $ 'groupsRollingBy_Eq `hasNoType` ''S.Step
-- inspect $ 'groupsRollingBy_Eq `hasNoType` ''S.GroupByState
inspect $ 'groupsRollingBy_Eq `hasNoType` ''FL.Step
inspect $ 'groupsRollingBy_Eq `hasNoType` ''SPEC
#endif

{-# ANN foldIterateM (PermitPatternMatches [''Int,''FL.Tuple'Fused]) #-}
{-# ANN foldIterateM (PermitConstructions [''Int,''FL.Tuple'Fused,''()]) #-}
{-# ANN foldIterateM (PermitTypeClasses []) #-}
{-# NOINLINE foldIterateM #-}
foldIterateM :: Int -> Int -> IO ()
foldIterateM value =
    withStream value $
        Common.drain
            . fmap getSum
            . S.foldIterateM
                (return . FL.take 2 . FL.sconcat) (return (Sum 0))
            . fmap Sum

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldIterateM
inspect $ 'foldIterateM `hasNoType` ''S.Step
inspect $ 'foldIterateM `hasNoType` ''S.FIterState
inspect $ 'foldIterateM `hasNoType` ''FL.Step
inspect $ 'foldIterateM `hasNoType` ''SPEC
#endif

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
    -- Buffering operations using heap proportional to group/window sizes.
      [ (SpaceO_1, benchIO "groupsWhile_LT" $ groupsWhile_LT size)
      , (SpaceO_1, benchIO "groupsWhile_Eq" $ groupsWhile_Eq size)
      , (SpaceO_1, benchIO "groupsRollingBy_LT" $ groupsRollingBy_LT size)
      , (SpaceO_1, benchIO "groupsRollingBy_Eq" $ groupsRollingBy_Eq size)

      , (SpaceO_1, benchIO "foldIterateM" $ foldIterateM size)
      ]
