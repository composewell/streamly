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

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as S
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = randomRIO (1,1) >>= f . streamUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE sepByWords #-}
sepByWords :: Int -> IO (Either ParseError ())
sepByWords value = withStream value $ Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

#ifdef INSPECTION
inspect $ 'sepByWords `hasNoType` ''S.Step
inspect $ 'sepByWords `hasNoType` ''PR.Step
inspect $ 'sepByWords `hasNoType` ''PR.Initial
inspect $ 'sepByWords `hasNoType` ''FL.Step
inspect $ 'sepByWords `hasNoType` ''SPEC
inspect $ 'sepByWords `hasNoType` ''PR.SepByState
#endif

{-# INLINE sepByAllWords #-}
sepByAllWords :: Int -> IO (Either ParseError ())
sepByAllWords value = withStream value $ Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepByAll (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

#ifdef INSPECTION
inspect $ 'sepByAllWords `hasNoType` ''S.Step
inspect $ 'sepByAllWords `hasNoType` ''PR.Step
inspect $ 'sepByAllWords `hasNoType` ''PR.Initial
inspect $ 'sepByAllWords `hasNoType` ''FL.Step
inspect $ 'sepByAllWords `hasNoType` ''SPEC
inspect $ 'sepByAllWords `hasNoType` ''PR.DeintercalateAllState
#endif

-- Returning a list to compare with the sepBy1 in ParserK
{-# INLINE sepBy1 #-}
sepBy1 :: Int -> IO (Either ParseError [Int])
sepBy1 value =
    withStream value $
        Stream.parse (PR.sepBy1 (PR.satisfy odd) (PR.satisfy even) Fold.toList)

{-# INLINE sepByWords1 #-}
sepByWords1 :: Int -> IO (Either ParseError ())
sepByWords1 value = withStream value $ Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy1 (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

#ifdef INSPECTION
inspect $ 'sepByWords1 `hasNoType` ''S.Step
inspect $ 'sepByWords1 `hasNoType` ''PR.Step
inspect $ 'sepByWords1 `hasNoType` ''PR.Initial
inspect $ 'sepByWords1 `hasNoType` ''FL.Step
inspect $ 'sepByWords1 `hasNoType` ''SPEC
inspect $ 'sepByWords1 `hasNoType` ''PR.SepBy1State
#endif

{-# INLINE deintercalate #-}
deintercalate :: Int -> IO (Either ParseError ())
deintercalate value = withStream value $ Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

#ifdef INSPECTION
inspect $ 'deintercalate `hasNoType` ''S.Step
inspect $ 'deintercalate `hasNoType` ''PR.Step
inspect $ 'deintercalate `hasNoType` ''PR.Initial
inspect $ 'deintercalate `hasNoType` ''FL.Step
inspect $ 'deintercalate `hasNoType` ''SPEC
inspect $ 'deintercalate `hasNoType` ''PR.DeintercalateState
#endif

{-# INLINE deintercalate1 #-}
deintercalate1 :: Int -> IO (Either ParseError ())
deintercalate1 value = withStream value $ Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate1
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

#ifdef INSPECTION
inspect $ 'deintercalate1 `hasNoType` ''S.Step
inspect $ 'deintercalate1 `hasNoType` ''PR.Step
inspect $ 'deintercalate1 `hasNoType` ''PR.Initial
inspect $ 'deintercalate1 `hasNoType` ''FL.Step
inspect $ 'deintercalate1 `hasNoType` ''SPEC
inspect $ 'deintercalate1 `hasNoType` ''PR.Deintercalate1State
#endif

{-# INLINE deintercalateAll #-}
deintercalateAll :: Int -> IO (Either ParseError ())
deintercalateAll value = withStream value $ Stream.parse (partition even)

    where

    partition p =
        PR.deintercalateAll
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

#ifdef INSPECTION
inspect $ 'deintercalateAll `hasNoType` ''S.Step
inspect $ 'deintercalateAll `hasNoType` ''PR.Step
inspect $ 'deintercalateAll `hasNoType` ''PR.Initial
inspect $ 'deintercalateAll `hasNoType` ''FL.Step
inspect $ 'deintercalateAll `hasNoType` ''SPEC
inspect $ 'deintercalateAll `hasNoType` ''PR.DeintercalateAllState
#endif

{-# INLINE manyTill #-}
manyTill :: Int -> IO (Either ParseError Int)
manyTill value =
    withStream value $
        Stream.parse (PR.manyTill (PR.satisfy (> 0)) (PR.satisfy (== value)) Fold.length)

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

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- Interleaved Repetition
      (SpaceO_1, benchIO "deintercalate" $ deintercalate value)
    , (SpaceO_1, benchIO "deintercalate1" $ deintercalate1 value)
    , (SpaceO_1, benchIO "deintercalateAll" $ deintercalateAll value)

    -- Accumulates the results in a list.
    , (HeapO_n, benchIO "sepBy1" $ sepBy1 value)
    , (SpaceO_1, benchIO "sepBy1 (words)" $ sepByWords1 value)
    , (SpaceO_1, benchIO "sepBy (words)" $ sepByWords value)
    , (SpaceO_1, benchIO "sepByAll (words)" $ sepByAllWords value)
    , (SpaceO_1, benchIO "manyTill" $ manyTill value)
    ]
