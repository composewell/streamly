#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
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
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import Test.Tasty.Bench (Benchmark)

import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.Parser.Common
import Prelude hiding (sequence, sequence_, sequenceA)

{-# INLINE splitAp2 #-}
splitAp2 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ((), ()))
splitAp2 value =
    Stream.parse
        ((,)
            <$> PR.dropWhile (<= (value `div` 2))
            <*> PR.dropWhile (<= value)
        )

{-# INLINE splitAp4 #-}
splitAp4 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitAp4 value =
    Stream.parse
        (      (\() () () () -> ())
            <$> PR.dropWhile (<= (value * 1 `div` 4))
            <*> PR.dropWhile (<= (value * 2 `div` 4))
            <*> PR.dropWhile (<= (value * 3 `div` 4))
            <*> PR.dropWhile (<= value)
        )

{-# INLINE splitAp8 #-}
splitAp8 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitAp8 value =
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

{-# INLINE splitApBefore #-}
splitApBefore :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitApBefore value =
    Stream.parse
        (  PR.dropWhile (<= (value `div` 2))
        *> PR.dropWhile (<= value)
        )

{-# INLINE splitApAfter #-}
splitApAfter :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitApAfter value =
    Stream.parse
        (  PR.dropWhile (<= (value `div` 2))
        <* PR.dropWhile (<= value)
        )

{-# INLINE splitWith2 #-}
splitWith2 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ((), ()))
splitWith2 value =
    Stream.parse
        (PR.splitWith (,)
            (PR.dropWhile (<= (value `div` 2)))
            (PR.dropWhile (<= value))
        )

{-# INLINE split_ #-}
split_ :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
split_ value =
    Stream.parse
        (PR.split_
            (PR.dropWhile (<= (value `div` 2)))
            (PR.dropWhile (<= value))
        )

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- XXX The timing of this increased 3x after the stepify extract changes.
{-# INLINE sequenceA_ #-}
sequenceA_ :: Monad m => Int -> Int -> m (Either ParseError ())
sequenceA_ value =
    Stream.parse (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))
        . streamUnfoldrM value

-- quadratic complexity
{-# INLINE sequenceA #-}
sequenceA :: Monad m => Int -> Int -> m Int
sequenceA value n = do
    x <- Stream.parse (TR.sequenceA (replicate value (PR.satisfy (> 0))))
            (streamUnfoldrM value n)
    return $ length x

-- quadratic complexity
{-# INLINE sequence #-}
sequence :: Monad m => Int -> Int -> m Int
sequence value n = do
    x <- Stream.parse (TR.sequence (replicate value (PR.satisfy (> 0))))
            (streamUnfoldrM value n)
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: Monad m => Int -> Int -> m (Either ParseError ())
sequence_ value n =
    Stream.parse (foldr f (return ()) (replicate value (PR.takeBetween 0 1 Fold.drain)))
            (streamUnfoldrM value n)

    where

    {-# INLINE f #-}
    f m k = m >>= (\_ -> k)

{-# INLINE concatSequence #-}
concatSequence :: Monad m => Stream m Int -> m (Either ParseError ())
concatSequence =
    Stream.parse $ PR.sequence (Stream.repeat PR.one) Fold.drain

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- Applicative
      (SpaceO_1, benchIOSink value "splitAp2" $ splitAp2 value)
    , (SpaceO_1, benchIOSink value "splitAp4" $ splitAp4 value)
    , (SpaceO_1, benchIOSink value "splitAp8" $ splitAp8 value)
    , (SpaceO_1, benchIOSink value "splitApBefore" $ splitApBefore value)
    , (SpaceO_1, benchIOSink value "splitApAfter" $ splitApAfter value)
    , (SpaceO_1, benchIOSink value "splitWith2" $ splitWith2 value)
    -- non-linear time complexity (parserD)
    , (HeapO_n, benchIOSink value "split_" $ split_ value)

    -- Sequential Collection
    -- Accumulate the results in a list.
    , (SpaceO_n, benchIOSink1 "sequenceA/100" $ sequenceA (value `div` 100))
    , (SpaceO_n, benchIOSink1 "sequenceA_/100" $ sequenceA_ (value `div` 100))
    , (SpaceO_n, benchIOSink1 "sequence/100" $ sequence (value `div` 100))
    , (SpaceO_n, benchIOSink1 "sequence_/100" $ sequence_ (value `div` 100))
    , (SpaceO_1, benchIOSink value "concatSequence" concatSequence)
    ]
