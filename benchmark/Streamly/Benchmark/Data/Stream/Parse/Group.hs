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
{-# ANN groups (PermitPatternMatches [''IO,''Int,''S.GroupByState]) #-}
{-# ANN groups (PermitConstructions [''Int,''SrcLoc,''CallStack,''S.GroupByState,''()]) #-}
{-# ANN groups (PermitTypeClasses [''IP]) #-}
{-# NOINLINE groups #-}
groups :: Int -> Int -> IO ()
groups value = withStream value $ Common.drain . S.groupsWhile (==) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groups
-- XXX fails but can't find the type in the core printed
-- inspect $ 'groups `hasNoType` ''S.Step
inspect $ 'groups `hasNoType` ''FL.Step
inspect $ 'groups `hasNoType` ''SPEC
#endif

{-# ANN groupsWhileLT (PermitPatternMatches [''IO,''Int,''S.GroupByState]) #-}
{-# ANN groupsWhileLT (PermitConstructions [''Int,''SrcLoc,''CallStack,''S.GroupByState,''()]) #-}
{-# ANN groupsWhileLT (PermitTypeClasses [''IP]) #-}
{-# NOINLINE groupsWhileLT #-}
groupsWhileLT :: Int -> Int -> IO ()
groupsWhileLT value = withStream value $ Common.drain . S.groupsWhile (<) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsWhileLT
-- XXX fails but can't find the type in the core printed
-- inspect $ 'groupsWhileLT `hasNoType` ''S.Step
inspect $ 'groupsWhileLT `hasNoType` ''FL.Step
inspect $ 'groupsWhileLT `hasNoType` ''SPEC
#endif

{-# ANN groupsWhileEq (PermitPatternMatches [''IO,''Int,''S.GroupByState]) #-}
{-# ANN groupsWhileEq (PermitConstructions [''Int,''SrcLoc,''CallStack,''S.GroupByState,''()]) #-}
{-# ANN groupsWhileEq (PermitTypeClasses [''IP]) #-}
{-# NOINLINE groupsWhileEq #-}
groupsWhileEq :: Int -> Int -> IO ()
groupsWhileEq value = withStream value $ Common.drain . S.groupsWhile (==) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsWhileEq
-- XXX fails but can't find the type in the core printed
-- inspect $ 'groupsWhileEq `hasNoType` ''S.Step
inspect $ 'groupsWhileEq `hasNoType` ''FL.Step
inspect $ 'groupsWhileEq `hasNoType` ''SPEC
#endif

{-# ANN groupsByRollingLT (PermitPatternMatches [''Int,''S.GroupByState]) #-}
{-# ANN groupsByRollingLT (PermitConstructions [''S.GroupByState,''Int,''()]) #-}
{-# ANN groupsByRollingLT (PermitTypeClasses []) #-}
{-# NOINLINE groupsByRollingLT #-}
groupsByRollingLT :: Int -> Int -> IO ()
groupsByRollingLT value = withStream value $ Common.drain . S.groupsRollingBy (<) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsByRollingLT
inspect $ 'groupsByRollingLT `hasNoType` ''S.Step
-- inspect $ 'groupsByRollingLT `hasNoType` ''S.GroupByState
inspect $ 'groupsByRollingLT `hasNoType` ''FL.Step
inspect $ 'groupsByRollingLT `hasNoType` ''SPEC
#endif

{-# ANN groupsByRollingEq (PermitPatternMatches [''Int,''S.GroupByState]) #-}
{-# ANN groupsByRollingEq (PermitConstructions [''S.GroupByState,''Int,''()]) #-}
{-# ANN groupsByRollingEq (PermitTypeClasses []) #-}
{-# NOINLINE groupsByRollingEq #-}
groupsByRollingEq :: Int -> Int -> IO ()
groupsByRollingEq value = withStream value $ Common.drain . S.groupsRollingBy (==) FL.drain

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsByRollingEq
inspect $ 'groupsByRollingEq `hasNoType` ''S.Step
-- inspect $ 'groupsByRollingEq `hasNoType` ''S.GroupByState
inspect $ 'groupsByRollingEq `hasNoType` ''FL.Step
inspect $ 'groupsByRollingEq `hasNoType` ''SPEC
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

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Buffering operations using heap proportional to group/window sizes.
      [ (SpaceO_1, benchIO "groups" $ groups size)
      , (SpaceO_1, benchIO "groupsWhileLT" $ groupsWhileLT size)
      , (SpaceO_1, benchIO "groupsWhileEq" $ groupsWhileEq size)
      , (SpaceO_1, benchIO "groupsByRollingLT" $ groupsByRollingLT size)
      , (SpaceO_1, benchIO "groupsByRollingEq" $ groupsByRollingEq size)

      , (SpaceO_1, benchIO "foldIterateM" $ foldIterateM size)
      ]
