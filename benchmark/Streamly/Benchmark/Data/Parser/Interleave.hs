#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
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
import Test.Tasty.Bench (Benchmark)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.Parser.Common

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE sepByWords #-}
sepByWords :: Monad m => Stream m Int -> m (Either ParseError ())
sepByWords = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

{-# INLINE sepByAllWords #-}
sepByAllWords :: Monad m => Stream m Int -> m (Either ParseError ())
sepByAllWords = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepByAll (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

-- Returning a list to compare with the sepBy1 in ParserK
{-# INLINE sepBy1 #-}
sepBy1 :: Monad m => Stream m Int -> m (Either ParseError [Int])
sepBy1 xs = do
    Stream.parse (PR.sepBy1 (PR.satisfy odd) (PR.satisfy even) Fold.toList) xs

{-# INLINE sepByWords1 #-}
sepByWords1 :: Monad m => Stream m Int -> m (Either ParseError ())
sepByWords1 = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy1 (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

{-# INLINE deintercalate #-}
deintercalate :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
deintercalate _ = Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

{-# INLINE deintercalate1 #-}
deintercalate1 :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
deintercalate1 _ = Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate1
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

{-# INLINE deintercalateAll #-}
deintercalateAll :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
deintercalateAll _ = Stream.parse (partition even)

    where

    partition p =
        PR.deintercalateAll
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

{-# INLINE manyTill #-}
manyTill :: Monad m => Int -> Stream m Int -> m (Either ParseError Int)
manyTill value =
    Stream.parse (PR.manyTill (PR.satisfy (> 0)) (PR.satisfy (== value)) Fold.length)

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
      (SpaceO_1, benchIOSink value "deintercalate" $ deintercalate value)
    , (SpaceO_1, benchIOSink value "deintercalate1" $ deintercalate1 value)
    , (SpaceO_1, benchIOSink value "deintercalateAll" $ deintercalateAll value)

    -- Accumulates the results in a list.
    , (HeapO_n, benchIOSink value "sepBy1" sepBy1)
    , (SpaceO_1, benchIOSink value "sepBy1 (words)" sepByWords1)
    , (SpaceO_1, benchIOSink value "sepBy (words)" sepByWords)
    , (SpaceO_1, benchIOSink value "sepByAll (words)" sepByAllWords)
    , (SpaceO_1, benchIOSink value "manyTill" $ manyTill value)
    ]
