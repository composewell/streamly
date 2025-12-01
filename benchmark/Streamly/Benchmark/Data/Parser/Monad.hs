#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
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
import Data.Monoid (Sum(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import Test.Tasty.Bench (Benchmark)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.Parser.Common

{-# INLINE monad #-}
monad :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad value =
    Stream.parse
        $ do
            PR.dropWhile (<= (value `div` 2))
            PR.dropWhile (<= value)

{-# INLINE monad4 #-}
monad4 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad4 value =
    Stream.parse $ do
        PR.dropWhile (<= (value `div` 4))
        PR.dropWhile (<= (value `div` 2))
        PR.dropWhile (<= (value * 3 `div` 4))
        PR.dropWhile (<= value)

{-# INLINE monad8 #-}
monad8 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad8 value =
    Stream.parse $ do
        PR.dropWhile (<= (value * 1 `div` 8))
        PR.dropWhile (<= (value * 2 `div` 8))
        PR.dropWhile (<= (value * 3 `div` 8))
        PR.dropWhile (<= (value * 4 `div` 8))
        PR.dropWhile (<= (value * 5 `div` 8))
        PR.dropWhile (<= (value * 6 `div` 8))
        PR.dropWhile (<= (value * 7 `div` 8))
        PR.dropWhile (<= value)

{-# INLINE monad16 #-}
monad16 :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
monad16 value =
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

{-# INLINE parseIterate #-}
parseIterate :: Monad m => Int -> Stream m Int -> m ()
parseIterate n =
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

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- Monad
      (SpaceO_1, benchIOSink value "monad2" $ monad value)
    , (SpaceO_1, benchIOSink value "monad4" $ monad4 value)
    , (SpaceO_1, benchIOSink value "monad8" $ monad8 value)
    -- XXX Takes lot of space when run on a long stream, why?
    , (HeapO_n, benchIOSink value "monad16" $ monad16 value)

    -- parseIterate
    , (SpaceO_1, benchIOSink value "parseIterate (take 1)" (parseIterate 1))
    , (SpaceO_1, benchIOSink value "parseIterate (take all)" (parseIterate value))
    ]
