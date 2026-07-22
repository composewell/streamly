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

-- Compiling parseMany with higher values of spec-constr-recursive hogs a lot
-- of memory and takes too much time. Fusion plugin alleviates the problem
-- though.
{-# OPTIONS_GHC -fspec-constr-recursive=10 #-}

-- |
-- Module      : Streamly.Benchmark.Data.Parser.Sequence
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Sequence
  (
    benchmarks
  ) where

import Control.DeepSeq (NFData(..))
import Data.Monoid (Sum(..))
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
import System.Random (randomRIO)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Prelude hiding (sequence)

#ifdef INSPECTION
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
-- Stream transformation
-------------------------------------------------------------------------------

{-# ANN parseMany (PermitPatternMatches [''Int]) #-}
{-# ANN parseMany (PermitConstructions [''()]) #-}
{-# ANN parseMany (PermitTypeClasses []) #-}
{-# NOINLINE parseMany #-}
parseMany :: Int -> Int -> Int -> IO ()
parseMany n value =
    withStream value $
          Stream.fold Fold.drain
        . fmap getSum
        . Stream.catRights
        . Stream.parseMany (PR.fromFold $ Fold.take n Fold.mconcat)
        . fmap Sum

#ifdef INSPECTION
inspect $ 'parseMany `hasNoType` ''S.Step
inspect $ 'parseMany `hasNoType` ''PR.Step
inspect $ 'parseMany `hasNoType` ''PR.Initial
inspect $ 'parseMany `hasNoType` ''FL.Step
inspect $ 'parseMany `hasNoType` ''SPEC
inspect $ 'parseMany `hasNoType` ''S.FIterState
#endif

{-# ANN parseMany_GroupBy_LT (PermitPatternMatches [''Int, ''[]]) #-}
{-# ANN parseMany_GroupBy_LT (PermitConstructions [''(), ''[], ''Int]) #-}
{-# ANN parseMany_GroupBy_LT (PermitTypeClasses []) #-}
{-# NOINLINE parseMany_GroupBy_LT #-}
parseMany_GroupBy_LT :: Int -> Int -> IO ()
parseMany_GroupBy_LT value =
    withStream value $
        Stream.fold Fold.drain . Stream.parseMany (PR.groupBy (<) Fold.drain)

#ifdef INSPECTION
inspect $ 'parseMany_GroupBy_LT `hasNoType` ''S.Step
inspect $ 'parseMany_GroupBy_LT `hasNoType` ''PR.Step
inspect $ 'parseMany_GroupBy_LT `hasNoType` ''PR.Initial
inspect $ 'parseMany_GroupBy_LT `hasNoType` ''FL.Step
inspect $ 'parseMany_GroupBy_LT `hasNoType` ''SPEC
inspect $ 'parseMany_GroupBy_LT `hasNoType` ''S.FIterState
inspect $ 'parseMany_GroupBy_LT `hasNoType` ''PR.GroupByState
#endif

{-# ANN parseMany_GroupBy_Eq (PermitPatternMatches [''Int, ''[]]) #-}
{-# ANN parseMany_GroupBy_Eq (PermitConstructions [''(), ''[], ''Int]) #-}
{-# ANN parseMany_GroupBy_Eq (PermitTypeClasses []) #-}
{-# NOINLINE parseMany_GroupBy_Eq #-}
parseMany_GroupBy_Eq :: Int -> Int -> IO ()
parseMany_GroupBy_Eq value =
    withStream value $
        Stream.fold Fold.drain . Stream.parseMany (PR.groupBy (==) Fold.drain)

#ifdef INSPECTION
inspect $ 'parseMany_GroupBy_Eq `hasNoType` ''S.Step
inspect $ 'parseMany_GroupBy_Eq `hasNoType` ''PR.Step
inspect $ 'parseMany_GroupBy_Eq `hasNoType` ''PR.Initial
inspect $ 'parseMany_GroupBy_Eq `hasNoType` ''FL.Step
inspect $ 'parseMany_GroupBy_Eq `hasNoType` ''SPEC
inspect $ 'parseMany_GroupBy_Eq `hasNoType` ''S.FIterState
inspect $ 'parseMany_GroupBy_Eq `hasNoType` ''PR.GroupByState
#endif

{-# ANN parseMany_GroupByRolling_Bounded (PermitPatternMatches
    [''Int, ''[]]) #-}
{-# ANN parseMany_GroupByRolling_Bounded (PermitConstructions
    [''(), ''[], ''Int]) #-}
{-# ANN parseMany_GroupByRolling_Bounded (PermitTypeClasses []) #-}
{-# NOINLINE parseMany_GroupByRolling_Bounded #-}
parseMany_GroupByRolling_Bounded :: Int -> Int -> IO ()
parseMany_GroupByRolling_Bounded value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany (PR.groupByRolling (\_ _ -> False) Fold.drain)

#ifdef INSPECTION
inspect $ 'parseMany_GroupByRolling_Bounded `hasNoType` ''S.Step
inspect $ 'parseMany_GroupByRolling_Bounded `hasNoType` ''PR.Step
inspect $ 'parseMany_GroupByRolling_Bounded `hasNoType` ''PR.Initial
inspect $ 'parseMany_GroupByRolling_Bounded `hasNoType` ''FL.Step
inspect $ 'parseMany_GroupByRolling_Bounded `hasNoType` ''SPEC
inspect $ 'parseMany_GroupByRolling_Bounded `hasNoType` ''S.FIterState
inspect $ 'parseMany_GroupByRolling_Bounded `hasNoType` ''PR.GroupByState
#endif

{-# ANN parseMany_GroupByRolling_OneGroup (PermitPatternMatches [''Int]) #-}
{-# ANN parseMany_GroupByRolling_OneGroup (PermitConstructions [''()]) #-}
{-# ANN parseMany_GroupByRolling_OneGroup (PermitTypeClasses []) #-}
{-# NOINLINE parseMany_GroupByRolling_OneGroup #-}
parseMany_GroupByRolling_OneGroup :: Int -> Int -> IO ()
parseMany_GroupByRolling_OneGroup value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany (PR.groupByRolling (\_ _ -> True) Fold.drain)

{-# ANN parseMany_GroupByRollingEither_LT (PermitPatternMatches
    [''Int, ''[]]) #-}
{-# ANN parseMany_GroupByRollingEither_LT (PermitConstructions
    [''(), ''[], ''Int]) #-}
{-# ANN parseMany_GroupByRollingEither_LT (PermitTypeClasses []) #-}
{-# NOINLINE parseMany_GroupByRollingEither_LT #-}
parseMany_GroupByRollingEither_LT :: Int -> Int -> IO ()
parseMany_GroupByRollingEither_LT value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany
            (PR.groupByRollingEither (<) Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'parseMany_GroupByRollingEither_LT `hasNoType` ''S.Step
inspect $ 'parseMany_GroupByRollingEither_LT `hasNoType` ''PR.Step
inspect $ 'parseMany_GroupByRollingEither_LT `hasNoType` ''PR.Initial
inspect $ 'parseMany_GroupByRollingEither_LT `hasNoType` ''FL.Step
inspect $ 'parseMany_GroupByRollingEither_LT `hasNoType` ''SPEC
inspect $ 'parseMany_GroupByRollingEither_LT `hasNoType` ''S.FIterState
inspect $
    'parseMany_GroupByRollingEither_LT `hasNoType` ''PR.GroupByStatePair
#endif

{-# ANN parseMany_GroupByRollingEither_GT (PermitPatternMatches
    [''Int, ''[]]) #-}
{-# ANN parseMany_GroupByRollingEither_GT (PermitConstructions
    [''(), ''[], ''Int]) #-}
{-# ANN parseMany_GroupByRollingEither_GT (PermitTypeClasses []) #-}
{-# NOINLINE parseMany_GroupByRollingEither_GT #-}
parseMany_GroupByRollingEither_GT :: Int -> Int -> IO ()
parseMany_GroupByRollingEither_GT value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany
            (PR.groupByRollingEither (>) Fold.drain Fold.drain)

{-# ANN parseMany_GroupByRollingEither_Alternating
    (PermitPatternMatches [''Int, ''[]]) #-}
{-# ANN parseMany_GroupByRollingEither_Alternating
    (PermitConstructions [''(), ''[], ''Int]) #-}
{-# ANN parseMany_GroupByRollingEither_Alternating
    (PermitTypeClasses []) #-}
{-# NOINLINE parseMany_GroupByRollingEither_Alternating #-}
parseMany_GroupByRollingEither_Alternating :: Int -> Int -> IO ()
parseMany_GroupByRollingEither_Alternating value =
    withStream value $
          Stream.fold Fold.drain
        . Stream.parseMany
            (PR.groupByRollingEither (>) Fold.drain Fold.drain)
        -- Make the input unsorted.
        . fmap (\x -> if even x then x + 2 else x)

{-# ANN sequence (PermitPatternMatches
    [ ''[], ''PR.Step, ''(,,), ''(,), ''Maybe', ''PR.Parser, ''PR.Initial
    , ''PR.Final, ''(), ''IO, ''Int
    ]) #-}
{-# ANN sequence (PermitConstructions
    [ ''[], ''(,), ''Either, ''Int, ''SrcLoc, ''CallStack, ''PR.Final, ''()
    , ''(,,), ''Maybe', ''PR.Parser, ''PR.Initial, ''PR.Step
    ]) #-}
{-# ANN sequence (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequence #-}
sequence :: Int -> Int -> IO (Either ParseError ())
sequence value =
    withStream value
        $ Stream.parse (PR.sequence (Stream.repeat PR.one) Fold.drain)

{-# ANN parseIterate (PermitPatternMatches
    [ ''[], ''Int, ''Fold.Tuple'Fused, ''PR.Step, ''(,), ''Bool
    , ''PR.Initial
    ]) #-}
{-# ANN parseIterate (PermitConstructions
    [ ''Fold.Tuple'Fused, ''PR.Initial, ''Int, ''SrcLoc, ''CallStack, ''[]
    , ''(), ''(,), ''PR.Step
    ]) #-}
{-# ANN parseIterate (PermitTypeClasses [''IP]) #-}
{-# NOINLINE parseIterate #-}
parseIterate :: Int -> Int -> Int -> IO ()
parseIterate n value =
    withStream value $
          Stream.fold Fold.drain
        . fmap getSum
        . Stream.catRights
        . Stream.parseIterate
            (PR.fromFold . Fold.take n . Fold.sconcat)
            (Sum 0)
        . fmap Sum

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
    -- parseMany
      (SpaceO_1, benchIO "parseMany (take 1)" $ parseMany 1 value)
    , (SpaceO_1, benchIO "parseMany (take all)" $ parseMany value value)
    , (SpaceO_1, benchIO "parseMany_GroupBy_LT" $ parseMany_GroupBy_LT value)
    -- requires -fspec-constr-recursive=10
    , (SpaceO_1, benchIO "parseMany_GroupBy_Eq" $ parseMany_GroupBy_Eq value)
    -- requires -fspec-constr-recursive=10
    , (SpaceO_1, benchIO "parseMany_GroupByRolling_Bounded"
          $ parseMany_GroupByRolling_Bounded value)
    , (SpaceO_1, benchIO "parseMany_GroupByRolling_OneGroup"
          $ parseMany_GroupByRolling_OneGroup value)
    , (SpaceO_1, benchIO "parseMany_GroupByRollingEither_LT (all Left)"
          $ parseMany_GroupByRollingEither_LT value)
    , (SpaceO_1, benchIO "parseMany_GroupByRollingEither_GT (all Right)"
          $ parseMany_GroupByRollingEither_GT value)
    -- requires -fspec-constr-recursive=10
    , (SpaceO_1, benchIO "parseMany_GroupByRollingEither_Alternating"
          $ parseMany_GroupByRollingEither_Alternating value)
    , (SpaceO_1, benchIO "sequence (repeat one, drain)" $ sequence value)

    -- parseIterate
    , (SpaceO_1, benchIO "parseIterate (take 1)" $ parseIterate 1 value)
    , (SpaceO_1, benchIO "parseIterate (take all)" $ parseIterate value value)
    ]
