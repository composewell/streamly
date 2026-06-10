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
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream

import Streamly.Benchmark.Common
import Prelude hiding (sequence, sequence_, sequenceA)

#ifdef INSPECTION
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

{-# INLINE splitAp2 #-}
splitAp2 :: Int -> IO (Either ParseError ((), ()))
splitAp2 value =
    withStream value $
        Stream.parse
            ((,)
                <$> PR.dropWhile (<= (value `div` 2))
                <*> PR.dropWhile (<= value)
            )

#ifdef INSPECTION
inspect $ 'splitAp2 `hasNoType` ''S.Step
inspect $ 'splitAp2 `hasNoType` ''PR.Step
inspect $ 'splitAp2 `hasNoType` ''PR.Initial
inspect $ 'splitAp2 `hasNoType` ''FL.Step
-- inspect $ 'splitAp2 `hasNoType` ''SPEC
-- inspect $ 'splitAp2 `hasNoType` ''PR.SeqParseState
#endif

{- HLINT ignore "Evaluate"-}
{-# INLINE splitAp4 #-}
splitAp4 :: Int -> IO (Either ParseError ())
splitAp4 value =
    withStream value $
        Stream.parse
            (      (\() () () () -> ())
                <$> PR.dropWhile (<= (value * 1 `div` 4))
                <*> PR.dropWhile (<= (value * 2 `div` 4))
                <*> PR.dropWhile (<= (value * 3 `div` 4))
                <*> PR.dropWhile (<= value)
            )

{-# INLINE splitAp8 #-}
splitAp8 :: Int -> IO (Either ParseError ())
splitAp8 value =
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

{-# INLINE splitApBefore #-}
splitApBefore :: Int -> IO (Either ParseError ())
splitApBefore value =
    withStream value $
        Stream.parse
            (  PR.dropWhile (<= (value `div` 2))
            *> PR.dropWhile (<= value)
            )

#ifdef INSPECTION
inspect $ 'splitApBefore `hasNoType` ''S.Step
inspect $ 'splitApBefore `hasNoType` ''PR.Step
inspect $ 'splitApBefore `hasNoType` ''PR.Initial
inspect $ 'splitApBefore `hasNoType` ''FL.Step
-- inspect $ 'splitApBefore `hasNoType` ''SPEC
-- inspect $ 'splitApBefore `hasNoType` ''PR.SeqAState
#endif

{-# INLINE splitApAfter #-}
splitApAfter :: Int -> IO (Either ParseError ())
splitApAfter value =
    withStream value $
        Stream.parse
            (  PR.dropWhile (<= (value `div` 2))
            <* PR.dropWhile (<= value)
            )

#ifdef INSPECTION
inspect $ 'splitApAfter `hasNoType` ''S.Step
inspect $ 'splitApAfter `hasNoType` ''PR.Step
inspect $ 'splitApAfter `hasNoType` ''PR.Initial
inspect $ 'splitApAfter `hasNoType` ''FL.Step
-- inspect $ 'splitApAfter `hasNoType` ''SPEC
-- inspect $ 'splitApAfter `hasNoType` ''PR.SeqParseState
#endif

{-# INLINE splitWith2 #-}
splitWith2 :: Int -> IO (Either ParseError ((), ()))
splitWith2 value =
    withStream value $
        Stream.parse
            (PR.splitWith (,)
                (PR.dropWhile (<= (value `div` 2)))
                (PR.dropWhile (<= value))
            )

#ifdef INSPECTION
inspect $ 'splitWith2 `hasNoType` ''S.Step
inspect $ 'splitWith2 `hasNoType` ''PR.Step
inspect $ 'splitWith2 `hasNoType` ''PR.Initial
inspect $ 'splitWith2 `hasNoType` ''FL.Step
-- inspect $ 'splitWith2 `hasNoType` ''SPEC
-- inspect $ 'splitWith2 `hasNoType` ''PR.SeqParseState
#endif

{-# INLINE split_ #-}
split_ :: Int -> IO (Either ParseError ())
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
-- inspect $ 'split_ `hasNoType` ''PR.SeqAState
#endif

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- XXX The timing of this increased 3x after the stepify extract changes.
{-# INLINE sequenceA_ #-}
sequenceA_ :: Int -> IO (Either ParseError ())
{- HLINT ignore "Use replicateM_"-}
sequenceA_ value =
    withStream value $
        Stream.parse (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

-- quadratic complexity
{-# INLINE sequenceA #-}
sequenceA :: Int -> IO Int
sequenceA value = do
    x <- withStream value $
            Stream.parse (TR.sequenceA (replicate value (PR.satisfy (> 0))))
    return $ length x

-- quadratic complexity
{-# INLINE sequence #-}
sequence :: Int -> IO Int
sequence value = do
    x <- withStream value $
            Stream.parse (TR.sequence (replicate value (PR.satisfy (> 0))))
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: Int -> IO (Either ParseError ())
sequence_ value =
    withStream value $
        Stream.parse
            (foldr f (return ()) (replicate value (PR.takeBetween 0 1 Fold.drain)))

    where

    {-# INLINE f #-}
    f m k = m >> k

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
      (SpaceO_1, benchIO "splitAp2" $ splitAp2 value)
    , (SpaceO_1, benchIO "splitAp4" $ splitAp4 value)
    , (SpaceO_1, benchIO "splitAp8" $ splitAp8 value)
    , (SpaceO_1, benchIO "splitApBefore" $ splitApBefore value)
    , (SpaceO_1, benchIO "splitApAfter" $ splitApAfter value)
    , (SpaceO_1, benchIO "splitWith2" $ splitWith2 value)
    -- non-linear time complexity (parserD)
    , (HeapO_n, benchIO "split_" $ split_ value)

    -- Sequential Collection
    -- Accumulate the results in a list.
    , (SpaceO_n, benchIO "sequenceA/100" $ sequenceA (value `div` 100))
    , (SpaceO_n, benchIO "sequenceA_/100" $ sequenceA_ (value `div` 100))
    , (SpaceO_n, benchIO "sequence/100" $ sequence (value `div` 100))
    , (SpaceO_n, benchIO "sequence_/100" $ sequence_ (value `div` 100))
    ]
