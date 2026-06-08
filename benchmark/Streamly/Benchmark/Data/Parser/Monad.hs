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
import Data.Monoid (Sum(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common

#ifdef INSPECTION
import Test.Inspection

import qualified Streamly.Internal.Data.Fold as FL
#endif

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = randomRIO (1,1) >>= f . streamUnfoldrM value

{-# INLINE monad #-}
monad :: Int -> IO (Either ParseError ())
monad value =
    withStream value $
        Stream.parse
            $ do
                PR.dropWhile (<= (value `div` 2))
                PR.dropWhile (<= value)

-- NOTE: monad parsers use concatMap (>>=) which introduces ConcatParseState.
-- ConcatParseState has an existential type in its second constructor, which
-- prevents full fusion. We only verify that the Fold step type and SPEC are
-- eliminated; we do not check PR.Step, PR.Initial, or S.Step.
#ifdef INSPECTION
inspect $ 'monad `hasNoType` ''FL.Step
-- inspect $ 'monad `hasNoType` ''SPEC
#endif

{-# INLINE monad4 #-}
monad4 :: Int -> IO (Either ParseError ())
monad4 value =
    withStream value $
        Stream.parse $ do
            PR.dropWhile (<= (value `div` 4))
            PR.dropWhile (<= (value `div` 2))
            PR.dropWhile (<= (value * 3 `div` 4))
            PR.dropWhile (<= value)

{- HLINT ignore "Evaluate"-}
{-# INLINE monad8 #-}
monad8 :: Int -> IO (Either ParseError ())
monad8 value =
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

{-# INLINE monad16 #-}
monad16 :: Int -> IO (Either ParseError ())
monad16 value =
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

{-# INLINE parseIterate #-}
parseIterate :: Int -> Int -> IO ()
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

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    [
    -- Monad
      (SpaceO_1, benchIO "monad2" $ monad value)
    , (SpaceO_1, benchIO "monad4" $ monad4 value)
    , (SpaceO_1, benchIO "monad8" $ monad8 value)
    -- XXX Takes lot of space when run on a long stream, why?
    , (HeapO_n, benchIO "monad16" $ monad16 value)

    -- parseIterate
    , (SpaceO_1, benchIO "parseIterate (take 1)" $ parseIterate 1 value)
    , (SpaceO_1, benchIO "parseIterate (take all)" $ parseIterate value value)
    ]
