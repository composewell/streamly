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
-- Module      : Streamly.Benchmark.Data.Parser.Applicative
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Applicative
  (
    benchmarks
  ) where

import Control.DeepSeq (NFData(..))
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Parser
    ( ParseError(..), SeqAState, SeqParseState, Final, Initial, Parser, Step
    , Tuple'Fused
    )
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Prelude hiding (sequence, sequence_, sequenceA)

#ifdef INSPECTION
import Test.Inspection

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as S
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> Int -> IO b
withStream value f = f . streamUnfoldrM value

{-# ANN ap_ApplicativeInstance_x2 (PermitPatternMatches
    [''[], ''(,), ''Int, ''SeqParseState]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitConstructions
    [''SeqParseState, ''(), ''(,), ''[], ''Int]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_x2 #-}
ap_ApplicativeInstance_x2 :: Int -> Int -> IO (Either ParseError ((), ()))
ap_ApplicativeInstance_x2 value =
    withStream value $
        Stream.parse
            ((,)
                <$> PR.dropWhile (<= (value `div` 2))
                <*> PR.dropWhile (<= value)
            )

#ifdef INSPECTION
inspect $ 'ap_ApplicativeInstance_x2 `hasNoType` ''S.Step
inspect $ 'ap_ApplicativeInstance_x2 `hasNoType` ''PR.Step
inspect $ 'ap_ApplicativeInstance_x2 `hasNoType` ''PR.Initial
inspect $ 'ap_ApplicativeInstance_x2 `hasNoType` ''FL.Step
-- inspect $ 'ap_ApplicativeInstance_x2 `hasNoType` ''SPEC
-- inspect $ 'ap_ApplicativeInstance_x2 `hasNoType` ''SeqParseState
#endif

{- HLINT ignore "Evaluate"-}
{-# ANN ap_ApplicativeInstance_x4 (PermitPatternMatches
    [''(), ''[], ''Int, ''SPEC, ''SeqParseState]) #-}
{-# ANN ap_ApplicativeInstance_x4 (PermitConstructions
    [''SeqParseState, ''(), ''[], ''Int]) #-}
{-# ANN ap_ApplicativeInstance_x4 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_x4 #-}
ap_ApplicativeInstance_x4 :: Int -> Int -> IO (Either ParseError ())
ap_ApplicativeInstance_x4 value =
    withStream value $
        Stream.parse
            (      (\() () () () -> ())
                <$> PR.dropWhile (<= (value * 1 `div` 4))
                <*> PR.dropWhile (<= (value * 2 `div` 4))
                <*> PR.dropWhile (<= (value * 3 `div` 4))
                <*> PR.dropWhile (<= value)
            )

{-# ANN ap_ApplicativeInstance_x8 (PermitPatternMatches
    [''(), ''[], ''Int, ''SPEC, ''SeqParseState]) #-}
{-# ANN ap_ApplicativeInstance_x8 (PermitConstructions
    [''SeqParseState, ''(), ''[], ''Int]) #-}
{-# ANN ap_ApplicativeInstance_x8 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_x8 #-}
ap_ApplicativeInstance_x8 :: Int -> Int -> IO (Either ParseError ())
ap_ApplicativeInstance_x8 value =
    withStream value $
        Stream.parse
            (      (\() () () () () () () () -> ())
                <$> PR.dropWhile (<= (value * 1 `div` 8))
                <*> PR.dropWhile (<= (value * 2 `div` 8))
                <*> PR.dropWhile (<= (value * 3 `div` 8))
                <*> PR.dropWhile (<= (value * 4 `div` 8))
                <*> PR.dropWhile (<= (value * 5 `div` 8))
                <*> PR.dropWhile (<= (value * 6 `div` 8))
                <*> PR.dropWhile (<= (value * 7 `div` 8))
                <*> PR.dropWhile (<= value)
            )

{-# ANN discardFst_ApplicativeInstance_x2 (PermitPatternMatches
    [''[], ''Int, ''SeqAState]) #-}
{-# ANN discardFst_ApplicativeInstance_x2 (PermitConstructions
    [''SeqAState, ''(), ''[], ''Int]) #-}
{-# ANN discardFst_ApplicativeInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE discardFst_ApplicativeInstance_x2 #-}
discardFst_ApplicativeInstance_x2 ::
    Int -> Int -> IO (Either ParseError ())
discardFst_ApplicativeInstance_x2 value =
    withStream value $
        Stream.parse
            (  PR.dropWhile (<= (value `div` 2))
            *> PR.dropWhile (<= value)
            )

#ifdef INSPECTION
inspect $ 'discardFst_ApplicativeInstance_x2 `hasNoType` ''S.Step
inspect $ 'discardFst_ApplicativeInstance_x2 `hasNoType` ''PR.Step
inspect $ 'discardFst_ApplicativeInstance_x2 `hasNoType` ''PR.Initial
inspect $ 'discardFst_ApplicativeInstance_x2 `hasNoType` ''FL.Step
-- inspect $ 'discardFst_ApplicativeInstance_x2 `hasNoType` ''SPEC
-- inspect $ 'discardFst_ApplicativeInstance_x2 `hasNoType` ''SeqAState
#endif

{-# ANN discardSnd_ApplicativeInstance_x2 (PermitPatternMatches
    [''[], ''(), ''Int, ''SeqParseState]) #-}
{-# ANN discardSnd_ApplicativeInstance_x2 (PermitConstructions
    [''SeqParseState, ''(), ''[], ''Int]) #-}
{-# ANN discardSnd_ApplicativeInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE discardSnd_ApplicativeInstance_x2 #-}
discardSnd_ApplicativeInstance_x2 ::
    Int -> Int -> IO (Either ParseError ())
discardSnd_ApplicativeInstance_x2 value =
    withStream value $
        Stream.parse
            (  PR.dropWhile (<= (value `div` 2))
            <* PR.dropWhile (<= value)
            )

#ifdef INSPECTION
inspect $ 'discardSnd_ApplicativeInstance_x2 `hasNoType` ''S.Step
inspect $ 'discardSnd_ApplicativeInstance_x2 `hasNoType` ''PR.Step
inspect $ 'discardSnd_ApplicativeInstance_x2 `hasNoType` ''PR.Initial
inspect $ 'discardSnd_ApplicativeInstance_x2 `hasNoType` ''FL.Step
-- inspect $ 'discardSnd_ApplicativeInstance_x2 `hasNoType` ''SPEC
-- inspect $
--     'discardSnd_ApplicativeInstance_x2 `hasNoType` ''SeqParseState
#endif

{-# ANN splitWith_x2 (PermitPatternMatches
    [''[], ''(,), ''Int, ''SeqParseState]) #-}
{-# ANN splitWith_x2 (PermitConstructions
    [''SeqParseState, ''(), ''(,), ''[], ''Int]) #-}
{-# ANN splitWith_x2 (PermitTypeClasses []) #-}
{-# NOINLINE splitWith_x2 #-}
splitWith_x2 :: Int -> Int -> IO (Either ParseError ((), ()))
splitWith_x2 value =
    withStream value $
        Stream.parse
            (PR.splitWith (,)
                (PR.dropWhile (<= (value `div` 2)))
                (PR.dropWhile (<= value))
            )

#ifdef INSPECTION
inspect $ 'splitWith_x2 `hasNoType` ''S.Step
inspect $ 'splitWith_x2 `hasNoType` ''PR.Step
inspect $ 'splitWith_x2 `hasNoType` ''PR.Initial
inspect $ 'splitWith_x2 `hasNoType` ''FL.Step
-- inspect $ 'splitWith_x2 `hasNoType` ''SPEC
-- inspect $ 'splitWith_x2 `hasNoType` ''SeqParseState
#endif

{-# ANN split_ (PermitPatternMatches [''[], ''Int, ''SeqAState]) #-}
{-# ANN split_ (PermitConstructions
    [''SeqAState, ''(), ''[], ''Int]) #-}
{-# ANN split_ (PermitTypeClasses []) #-}
{-# NOINLINE split_ #-}
split_ :: Int -> Int -> IO (Either ParseError ())
split_ value =
    withStream value $
        Stream.parse
            (PR.split_
                (PR.dropWhile (<= (value `div` 2)))
                (PR.dropWhile (<= value))
            )

#ifdef INSPECTION
inspect $ 'split_ `hasNoType` ''S.Step
inspect $ 'split_ `hasNoType` ''PR.Step
inspect $ 'split_ `hasNoType` ''PR.Initial
inspect $ 'split_ `hasNoType` ''FL.Step
-- inspect $ 'split_ `hasNoType` ''SPEC
-- inspect $ 'split_ `hasNoType` ''SeqAState
#endif

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- XXX The timing of this increased 3x after the stepify extract changes.
{-# ANN sequenceA_ (PermitPatternMatches
    [ ''SeqAState, ''Int, ''Parser, ''Initial, ''Step, ''Final
    , ''[], ''IO, ''(,)
    ]) #-}
{-# ANN sequenceA_ (PermitConstructions
    [ ''Final, ''SeqAState, ''(), ''Initial, ''Step, ''Parser
    , ''[], ''Int, ''SrcLoc, ''CallStack, ''Either, ''(,)
    ]) #-}
{-# ANN sequenceA_ (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequenceA_ #-}
sequenceA_ :: Int -> Int -> IO (Either ParseError ())
{- HLINT ignore "Use replicateM_"-}
sequenceA_ value =
    withStream value $
        Stream.parse (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

-- quadratic complexity
{-# ANN sequenceA (PermitPatternMatches
    [ ''[], ''SeqParseState, ''Step, ''Initial, ''Final, ''()
    , ''Int, ''Parser, ''IO, ''(,)
    ]) #-}
{-# ANN sequenceA (PermitConstructions
    [ ''[], ''Int, ''SrcLoc, ''CallStack, ''Parser, ''Step
    , ''SeqParseState, ''Initial, ''Final, ''(), ''(,)
    ]) #-}
{-# ANN sequenceA (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequenceA #-}
sequenceA :: Int -> Int -> IO Int
sequenceA value start = do
    x <- withStream value
            (Stream.parse (TR.sequenceA (replicate value (PR.satisfy (> 0)))))
            start
    return $ length x

-- quadratic complexity
{-# ANN sequence (PermitPatternMatches
    [ ''SeqParseState, ''Step, ''Initial, ''[], ''Final, ''()
    , ''Int, ''Parser, ''IO, ''(,)
    ]) #-}
{-# ANN sequence (PermitConstructions
    [ ''Parser, ''Step, ''SeqParseState, ''[], ''Initial
    , ''Final, ''(), ''Int, ''SrcLoc, ''CallStack, ''(,)
    ]) #-}
{-# ANN sequence (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequence #-}
sequence :: Int -> Int -> IO Int
sequence value start = do
    x <- withStream value
            (Stream.parse (TR.sequence (replicate value (PR.satisfy (> 0)))))
            start
    return $ length x

{-# ANN sequence_ (PermitPatternMatches
    [ ''SeqAState, ''Tuple'Fused, ''Int, ''String, ''Parser
    , ''Initial, ''Step, ''Final, ''[], ''IO, ''(,)
    ]) #-}
{-# ANN sequence_ (PermitConstructions
    [ ''Final, ''(), ''SeqAState, ''Initial, ''Step
    , ''Tuple'Fused, ''Int, ''Parser, ''[], ''SrcLoc, ''CallStack
    , ''Either, ''(,)
    ]) #-}
{-# ANN sequence_ (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequence_ #-}
sequence_ :: Int -> Int -> IO (Either ParseError ())
sequence_ value =
    withStream value $
        Stream.parse
            (foldr f (return ())
                (replicate value (PR.takeBetween 0 1 Fold.drain)))

    where

    {-# INLINE f #-}
    f m k = m >> k

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
    -- Applicative
      (SpaceO_1, benchIO "ap_ApplicativeInstance_x2 (<*>)"
          $ ap_ApplicativeInstance_x2 value)
    , (SpaceO_1, benchIO "ap_ApplicativeInstance_x4 (<*>)"
          $ ap_ApplicativeInstance_x4 value)
    , (SpaceO_1, benchIO "ap_ApplicativeInstance_x8 (<*>)"
          $ ap_ApplicativeInstance_x8 value)
    , (SpaceO_1, benchIO "discardFst_ApplicativeInstance_x2 (*>)"
          $ discardFst_ApplicativeInstance_x2 value)
    , (SpaceO_1, benchIO "discardSnd_ApplicativeInstance_x2 (<*)"
          $ discardSnd_ApplicativeInstance_x2 value)
    , (SpaceO_1, benchIO "splitWith_x2" $ splitWith_x2 value)
    -- non-linear time complexity (parserD)
    , (HeapO_n, benchIO "split_" $ split_ value)

    -- Sequential Collection
    -- Accumulate the results in a list.
    , (SpaceO_n, benchIO "sequenceA (value div 100)"
          $ sequenceA (value `div` 100))
    , (SpaceO_n, benchIO "sequenceA_ (value div 100)"
          $ sequenceA_ (value `div` 100))
    , (SpaceO_n, benchIO "sequence (value div 100)"
          $ sequence (value `div` 100))
    , (SpaceO_n, benchIO "sequence_ (takeBetween 0 1, value div 100)"
          $ sequence_ (value `div` 100))
    ]
