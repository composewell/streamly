#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- |
-- Module      : Streamly.Benchmark.Data.Parser.Monad
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Monad
  (
    benchmarks
  ) where

import Control.DeepSeq (NFData(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser (ParseError(..), SeqAState)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Fusion.Plugin.Types

#ifdef INSPECTION
import Test.Inspection

import qualified Streamly.Internal.Data.Fold as FL
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> Int -> IO b
withStream value f = f . streamUnfoldrM value

{-# ANN then_MonadInstance_x2 (PermitPatternMatches
    [''[], ''Int, ''SeqAState]) #-}
{-# ANN then_MonadInstance_x2 (PermitConstructions
    [''SeqAState, ''(), ''[], ''Int]) #-}
{-# ANN then_MonadInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE then_MonadInstance_x2 #-}
then_MonadInstance_x2 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x2 value =
    withStream value $
        Stream.parse
            $ do
                PR.dropWhile (<= (value `div` 2))
                PR.dropWhile (<= value)

-- NOTE: these do blocks have no binds, so they go through the Monad
-- instance's (>>), which is defined as (*>) i.e. split_. We only verify that
-- the Fold step type and SPEC are eliminated; we do not check PR.Step,
-- PR.Initial, or S.Step.
#ifdef INSPECTION
inspect $ 'then_MonadInstance_x2 `hasNoType` ''FL.Step
-- inspect $ 'then_MonadInstance_x2 `hasNoType` ''SPEC
#endif

{-# ANN then_MonadInstance_x4 (PermitPatternMatches
    [''[], ''Int, ''SPEC, ''SeqAState]) #-}
{-# ANN then_MonadInstance_x4 (PermitConstructions
    [''SeqAState, ''(), ''[], ''Int]) #-}
{-# ANN then_MonadInstance_x4 (PermitTypeClasses []) #-}
{-# NOINLINE then_MonadInstance_x4 #-}
then_MonadInstance_x4 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x4 value =
    withStream value $
        Stream.parse $ do
            PR.dropWhile (<= (value `div` 4))
            PR.dropWhile (<= (value `div` 2))
            PR.dropWhile (<= (value * 3 `div` 4))
            PR.dropWhile (<= value)

{- HLINT ignore "Evaluate"-}
{-# ANN then_MonadInstance_x8 (PermitPatternMatches
    [''[], ''Int, ''SPEC, ''SeqAState]) #-}
{-# ANN then_MonadInstance_x8 (PermitConstructions
    [''SeqAState, ''(), ''[], ''Int]) #-}
{-# ANN then_MonadInstance_x8 (PermitTypeClasses []) #-}
{-# NOINLINE then_MonadInstance_x8 #-}
then_MonadInstance_x8 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x8 value =
    withStream value $
        Stream.parse $ do
            PR.dropWhile (<= (value * 1 `div` 8))
            PR.dropWhile (<= (value * 2 `div` 8))
            PR.dropWhile (<= (value * 3 `div` 8))
            PR.dropWhile (<= (value * 4 `div` 8))
            PR.dropWhile (<= (value * 5 `div` 8))
            PR.dropWhile (<= (value * 6 `div` 8))
            PR.dropWhile (<= (value * 7 `div` 8))
            PR.dropWhile (<= value)

{-# ANN then_MonadInstance_x16 (PermitPatternMatches
    [''[], ''Int, ''SPEC, ''SeqAState]) #-}
{-# ANN then_MonadInstance_x16 (PermitConstructions
    [''SeqAState, ''(), ''[], ''Int]) #-}
{-# ANN then_MonadInstance_x16 (PermitTypeClasses []) #-}
{-# NOINLINE then_MonadInstance_x16 #-}
then_MonadInstance_x16 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x16 value =
    withStream value $
        Stream.parse $ do
            PR.dropWhile (<= (value * 1 `div` 16))
            PR.dropWhile (<= (value * 2 `div` 16))
            PR.dropWhile (<= (value * 3 `div` 16))
            PR.dropWhile (<= (value * 4 `div` 16))
            PR.dropWhile (<= (value * 5 `div` 16))
            PR.dropWhile (<= (value * 6 `div` 16))
            PR.dropWhile (<= (value * 7 `div` 16))
            PR.dropWhile (<= (value * 8 `div` 16))
            PR.dropWhile (<= (value * 9 `div` 16))
            PR.dropWhile (<= (value * 10 `div` 16))
            PR.dropWhile (<= (value * 11 `div` 16))
            PR.dropWhile (<= (value * 12 `div` 16))
            PR.dropWhile (<= (value * 13 `div` 16))
            PR.dropWhile (<= (value * 14 `div` 16))
            PR.dropWhile (<= (value * 15 `div` 16))
            PR.dropWhile (<= value)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- Monad
      (SpaceO_1, benchIO "then_MonadInstance_x2" $ then_MonadInstance_x2 value)
    , (SpaceO_1, benchIO "then_MonadInstance_x4" $ then_MonadInstance_x4 value)
    , (SpaceO_1, benchIO "then_MonadInstance_x8" $ then_MonadInstance_x8 value)
    -- XXX Takes lot of space when run on a long stream, why?
    , (HeapO_n, benchIO "then_MonadInstance_x16"
          $ then_MonadInstance_x16 value)
    ]
