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
-- Module      : Streamly.Benchmark.Data.Parser.Interleave
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Interleave
  (
    benchmarks
  ) where

import Control.DeepSeq (NFData(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Streamly.Benchmark.Common
import Fusion.Plugin.Types

#ifdef INSPECTION
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import GHC.Types (SPEC(..))
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

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# ANN sepBy_Words (PermitPatternMatches [''Int, ''(), ''[]]) #-}
{-# ANN sepBy_Words (PermitConstructions [''(), ''[], ''Int]) #-}
{-# ANN sepBy_Words (PermitTypeClasses []) #-}
{-# NOINLINE sepBy_Words #-}
sepBy_Words :: Int -> Int -> IO (Either ParseError ())
sepBy_Words value = withStream value $ Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

#ifdef INSPECTION
inspect $ 'sepBy_Words `hasNoType` ''S.Step
inspect $ 'sepBy_Words `hasNoType` ''PR.Step
inspect $ 'sepBy_Words `hasNoType` ''PR.Initial
inspect $ 'sepBy_Words `hasNoType` ''FL.Step
inspect $ 'sepBy_Words `hasNoType` ''SPEC
inspect $ 'sepBy_Words `hasNoType` ''PR.SepByState
#endif

{-# ANN sepByAll_Words (PermitPatternMatches [''()]) #-}
{-# ANN sepByAll_Words (PermitConstructions [''Either, ''()]) #-}
{-# ANN sepByAll_Words (PermitTypeClasses []) #-}
{-# NOINLINE sepByAll_Words #-}
sepByAll_Words :: Int -> Int -> IO (Either ParseError ())
sepByAll_Words value = withStream value $ Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepByAll (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

#ifdef INSPECTION
inspect $ 'sepByAll_Words `hasNoType` ''S.Step
inspect $ 'sepByAll_Words `hasNoType` ''PR.Step
inspect $ 'sepByAll_Words `hasNoType` ''PR.Initial
inspect $ 'sepByAll_Words `hasNoType` ''FL.Step
inspect $ 'sepByAll_Words `hasNoType` ''SPEC
inspect $ 'sepByAll_Words `hasNoType` ''PR.DeintercalateAllState
#endif

-- Returning a list to compare with the sepBy1 in ParserK
{-# ANN sepBy1_Satisfy (PermitPatternMatches [''[]]) #-}
{-# ANN sepBy1_Satisfy (PermitConstructions [''Either, ''[], ''Int]) #-}
{-# ANN sepBy1_Satisfy (PermitTypeClasses []) #-}
{-# NOINLINE sepBy1_Satisfy #-}
sepBy1_Satisfy :: Int -> Int -> IO (Either ParseError [Int])
sepBy1_Satisfy value =
    withStream value $
        Stream.parse (PR.sepBy1 (PR.satisfy odd) (PR.satisfy even) Fold.toList)

{-# ANN sepBy1_Words (PermitPatternMatches [''Int, ''(), ''[]]) #-}
{-# ANN sepBy1_Words (PermitConstructions [''(), ''[], ''Int]) #-}
{-# ANN sepBy1_Words (PermitTypeClasses []) #-}
{-# NOINLINE sepBy1_Words #-}
sepBy1_Words :: Int -> Int -> IO (Either ParseError ())
sepBy1_Words value = withStream value $ Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy1 (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

#ifdef INSPECTION
inspect $ 'sepBy1_Words `hasNoType` ''S.Step
inspect $ 'sepBy1_Words `hasNoType` ''PR.Step
inspect $ 'sepBy1_Words `hasNoType` ''PR.Initial
inspect $ 'sepBy1_Words `hasNoType` ''FL.Step
inspect $ 'sepBy1_Words `hasNoType` ''SPEC
inspect $ 'sepBy1_Words `hasNoType` ''PR.SepBy1State
#endif

{-# ANN deintercalate (PermitPatternMatches [''(), ''Int, ''[]]) #-}
{-# ANN deintercalate (PermitConstructions [''(), ''[], ''Int]) #-}
{-# ANN deintercalate (PermitTypeClasses []) #-}
{-# NOINLINE deintercalate #-}
deintercalate :: Int -> Int -> IO (Either ParseError ())
deintercalate value = withStream value $ Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate
            (PR.takeWhile (not . p) Fold.sum)
            (PR.takeWhile p Fold.sum)
            Fold.drain

#ifdef INSPECTION
inspect $ 'deintercalate `hasNoType` ''S.Step
inspect $ 'deintercalate `hasNoType` ''PR.Step
inspect $ 'deintercalate `hasNoType` ''PR.Initial
inspect $ 'deintercalate `hasNoType` ''FL.Step
inspect $ 'deintercalate `hasNoType` ''SPEC
inspect $ 'deintercalate `hasNoType` ''PR.DeintercalateState
#endif

{-# ANN deintercalate1 (PermitPatternMatches [''(), ''Int, ''[]]) #-}
{-# ANN deintercalate1 (PermitConstructions [''(), ''[], ''Int]) #-}
{-# ANN deintercalate1 (PermitTypeClasses []) #-}
{-# NOINLINE deintercalate1 #-}
deintercalate1 :: Int -> Int -> IO (Either ParseError ())
deintercalate1 value = withStream value $ Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate1
            (PR.takeWhile (not . p) Fold.sum)
            (PR.takeWhile p Fold.sum)
            Fold.drain

#ifdef INSPECTION
inspect $ 'deintercalate1 `hasNoType` ''S.Step
inspect $ 'deintercalate1 `hasNoType` ''PR.Step
inspect $ 'deintercalate1 `hasNoType` ''PR.Initial
inspect $ 'deintercalate1 `hasNoType` ''FL.Step
inspect $ 'deintercalate1 `hasNoType` ''SPEC
inspect $ 'deintercalate1 `hasNoType` ''PR.Deintercalate1State
#endif

{-# ANN deintercalateAll (PermitPatternMatches [''()]) #-}
{-# ANN deintercalateAll (PermitConstructions [''Either, ''()]) #-}
{-# ANN deintercalateAll (PermitTypeClasses []) #-}
{-# NOINLINE deintercalateAll #-}
deintercalateAll :: Int -> Int -> IO (Either ParseError ())
deintercalateAll value = withStream value $ Stream.parse (partition even)

    where

    partition p =
        PR.deintercalateAll
            (PR.takeWhile (not . p) Fold.sum)
            (PR.takeWhile p Fold.sum)
            Fold.drain

#ifdef INSPECTION
inspect $ 'deintercalateAll `hasNoType` ''S.Step
inspect $ 'deintercalateAll `hasNoType` ''PR.Step
inspect $ 'deintercalateAll `hasNoType` ''PR.Initial
inspect $ 'deintercalateAll `hasNoType` ''FL.Step
inspect $ 'deintercalateAll `hasNoType` ''SPEC
inspect $ 'deintercalateAll `hasNoType` ''PR.DeintercalateAllState
#endif

#ifdef INSPECTION
{-# ANN manyTill (PermitPatternMatches
    [''[], ''Int, ''(,),''SPEC]) #-}
{-# ANN manyTill (PermitConstructions
    [''[], ''Int, ''SrcLoc, ''CallStack, ''Either, ''(,),''()]) #-}
{-# ANN manyTill (PermitTypeClasses [''IP]) #-}

-- XXX NOINLINE makes the inspection tests fail and INLINE makes non-inspection
-- build fail.
{-# INLINE manyTill #-}
#endif
manyTill :: Int -> Int -> IO (Either ParseError Int)
manyTill value x =
    (withStream value $
        Stream.parse
            (PR.manyTill
                (PR.satisfy (> 0)) (PR.satisfy (== value)) Fold.length)) x

#ifdef INSPECTION
inspect $ 'manyTill `hasNoType` ''S.Step
inspect $ 'manyTill `hasNoType` ''PR.Step
inspect $ 'manyTill `hasNoType` ''PR.Initial
inspect $ 'manyTill `hasNoType` ''FL.Step
inspect $ 'manyTill `hasNoType` ''SPEC
inspect $ 'manyTill `hasNoType` ''PR.ManyTillState
#endif

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
    -- Interleaved Repetition
      (SpaceO_1, benchIO "deintercalate" $ deintercalate value)
    , (SpaceO_1, benchIO "deintercalate1" $ deintercalate1 value)
    , (SpaceO_1, benchIO "deintercalateAll" $ deintercalateAll value)

    -- Accumulates the results in a list.
    , (HeapO_n, benchIO "sepBy1_Satisfy (odd & even, toList)"
          $ sepBy1_Satisfy value)
    , (SpaceO_1, benchIO "sepBy1_Words" $ sepBy1_Words value)
    , (SpaceO_1, benchIO "sepBy_Words" $ sepBy_Words value)
    , (SpaceO_1, benchIO "sepByAll_Words" $ sepByAll_Words value)
    , (SpaceO_1, benchIO "manyTill" $ manyTill value)
    ]
