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
-- Module      : Streamly.Benchmark.Data.Parser.Groups
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Streamly.Benchmark.Data.Parser.Groups
  (
    benchmarks
  ) where

import Control.DeepSeq (NFData(..))
import Data.Functor (($>))
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Either.Strict (Either'(..))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..))
import Streamly.Internal.Data.Parser
    (ParseError(..), GroupByState, SeqParseState, WordByState)
import Streamly.Internal.Data.Stream (Stream, Step)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Prelude hiding (takeWhile, dropWhile, span)

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

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Generates something like this: { { \{ \{ } }.  The stream consists of
-- three parts, the first part is contains a sequence of `{`. The second part
-- contains a sequence pf escaped values `\{`. The third part contains a
-- sequence of `}`.
{-# INLINE sourceEscapedFrames #-}
sourceEscapedFrames ::
    Monad m
    => Int
    -> Int
    -> Stream m Char
sourceEscapedFrames value = Stream.unfoldrM step

    where

    bs = '\\'
    cbOpen = '{'
    cbClose = '}'
    value1 = value `div` 4

    step cnt
        | cnt > 4 * value1 = return Nothing
        | cnt <= value1 = return $ Just (cbOpen, cnt + 1)
        | cnt > 3 * value1 = return $ Just (cbClose, cnt + 1)
        | otherwise =
            return
                $ Just
                $ if (cnt - value1) `mod` 2 == 1
                  then (bs, cnt + 1)
                  else (cbOpen, cnt + 1)

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# ANN takeBetween (PermitPatternMatches [''[], ''String, ''IO]) #-}
{-# ANN takeBetween (PermitConstructions
    [''[], ''Either, ''(), ''Int, ''SrcLoc, ''CallStack]) #-}
{-# ANN takeBetween (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeBetween #-}
takeBetween :: Int -> Int -> IO (Either ParseError ())
takeBetween value =
    withStream value $ Stream.parse (PR.takeBetween 0 value Fold.drain)

#ifdef INSPECTION
inspect $ 'takeBetween `hasNoType` ''S.Step
inspect $ 'takeBetween `hasNoType` ''PR.Step
-- inspect $ 'takeBetween `hasNoType` ''PR.Initial
inspect $ 'takeBetween `hasNoType` ''FL.Step
inspect $ 'takeBetween `hasNoType` ''SPEC
-- inspect $ 'takeBetween `hasNoType` ''PR.Tuple'Fused
#endif

{-# ANN takeEQ (PermitPatternMatches [''String, ''[]]) #-}
{-# ANN takeEQ (PermitConstructions [''Either, ''(), ''[]]) #-}
{-# ANN takeEQ (PermitTypeClasses []) #-}
{-# NOINLINE takeEQ #-}
takeEQ :: Int -> Int -> IO (Either ParseError ())
takeEQ value = withStream value $ Stream.parse (PR.takeEQ value Fold.drain)

#ifdef INSPECTION
inspect $ 'takeEQ `hasNoType` ''S.Step
inspect $ 'takeEQ `hasNoType` ''PR.Step
inspect $ 'takeEQ `hasNoType` ''PR.Initial
inspect $ 'takeEQ `hasNoType` ''FL.Step
inspect $ 'takeEQ `hasNoType` ''SPEC
inspect $ 'takeEQ `hasNoType` ''PR.Tuple'Fused
#endif

{-# ANN takeGE (PermitPatternMatches [''String, ''[]]) #-}
{-# ANN takeGE (PermitConstructions
    [''Step, ''Stream, ''(), ''Either, ''[], ''Int]) #-}
{-# ANN takeGE (PermitTypeClasses []) #-}
{-# NOINLINE takeGE #-}
takeGE :: Int -> Int -> IO (Either ParseError ())
takeGE value = withStream value $ Stream.parse (PR.takeGE value Fold.drain)

#ifdef INSPECTION
-- inspect $ 'takeGE `hasNoType` ''S.Step
inspect $ 'takeGE `hasNoType` ''PR.Step
inspect $ 'takeGE `hasNoType` ''PR.Initial
inspect $ 'takeGE `hasNoType` ''FL.Step
inspect $ 'takeGE `hasNoType` ''SPEC
inspect $ 'takeGE `hasNoType` ''PR.TakeGEState
#endif

{-# ANN dropWhile (PermitPatternMatches []) #-}
{-# ANN dropWhile (PermitConstructions [''Either, ''()]) #-}
{-# ANN dropWhile (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile #-}
dropWhile :: Int -> Int -> IO (Either ParseError ())
dropWhile value = withStream value $ Stream.parse (PR.dropWhile (<= value))

#ifdef INSPECTION
inspect $ 'dropWhile `hasNoType` ''S.Step
inspect $ 'dropWhile `hasNoType` ''PR.Step
inspect $ 'dropWhile `hasNoType` ''PR.Initial
inspect $ 'dropWhile `hasNoType` ''FL.Step
inspect $ 'dropWhile `hasNoType` ''SPEC
#endif

{-# ANN takeBeginBy (PermitPatternMatches [''[], ''Int, ''Either']) #-}
{-# ANN takeBeginBy (PermitConstructions [''Either, ''(), ''Either']) #-}
{-# ANN takeBeginBy (PermitTypeClasses []) #-}
{-# NOINLINE takeBeginBy #-}
takeBeginBy :: Int -> Int -> IO (Either ParseError ())
takeBeginBy value n =
    Stream.parse (PR.takeBeginBy (== value) Fold.drain) stream

    where

    stream = value `Stream.cons` streamUnfoldrM value n

#ifdef INSPECTION
inspect $ 'takeBeginBy `hasNoType` ''S.Step
inspect $ 'takeBeginBy `hasNoType` ''PR.Step
inspect $ 'takeBeginBy `hasNoType` ''PR.Initial
inspect $ 'takeBeginBy `hasNoType` ''FL.Step
inspect $ 'takeBeginBy `hasNoType` ''SPEC
#endif

{-# ANN takeFramedByEsc_ (PermitPatternMatches
    [''Maybe, ''(,), ''Char, ''Int]) #-}
{-# ANN takeFramedByEsc_ (PermitConstructions
    [''Either, ''Char, ''(), ''Maybe, ''(,), ''Int]) #-}
{-# ANN takeFramedByEsc_ (PermitTypeClasses []) #-}
{-# NOINLINE takeFramedByEsc_ #-}
takeFramedByEsc_ :: Int -> Int -> IO (Either ParseError ())
takeFramedByEsc_ value n =
    Stream.parse parser (sourceEscapedFrames value n)

    where

    isEsc = (== '\\')
    isBegin = (== '{')
    isEnd = (== '}')

    parser = PR.takeFramedByEsc_ isEsc isBegin isEnd Fold.drain

{-# ANN listEqBy (PermitPatternMatches [''[], ''Int, ''Maybe', ''String]) #-}
{-# ANN listEqBy (PermitConstructions [''Either, ''[], ''Int, ''Maybe']) #-}
{-# ANN listEqBy (PermitTypeClasses []) #-}
{-# NOINLINE listEqBy #-}
listEqBy :: Int -> Int -> IO (Either ParseError [Int])
listEqBy value =
    withStream value $ Stream.parse (PR.listEqBy (==) [1 .. value])

{-# ANN streamEqBy (PermitPatternMatches [''String]) #-}
{-# ANN streamEqBy (PermitConstructions [''Either, ''(), ''[]]) #-}
{-# ANN streamEqBy (PermitTypeClasses []) #-}
{-# NOINLINE streamEqBy #-}
streamEqBy :: Int -> Int -> IO (Either ParseError ())
streamEqBy value =
    withStream value
        $ Stream.parse
            (PR.streamEqBy (==) (Stream.enumerateFromTo 1 value))

{-# ANN takeWhile (PermitPatternMatches []) #-}
{-# ANN takeWhile (PermitConstructions [''Either, ''()]) #-}
{-# ANN takeWhile (PermitTypeClasses []) #-}
{-# NOINLINE takeWhile #-}
takeWhile :: Int -> Int -> IO (Either ParseError ())
takeWhile value =
    withStream value $ Stream.parse (PR.takeWhile (<= value) Fold.drain)

#ifdef INSPECTION
inspect $ 'takeWhile `hasNoType` ''S.Step
inspect $ 'takeWhile `hasNoType` ''PR.Step
inspect $ 'takeWhile `hasNoType` ''PR.Initial
inspect $ 'takeWhile `hasNoType` ''FL.Step
inspect $ 'takeWhile `hasNoType` ''SPEC
#endif

{-# ANN takeWhileP (PermitPatternMatches []) #-}
{-# ANN takeWhileP (PermitConstructions [''Either, ''()]) #-}
{-# ANN takeWhileP (PermitTypeClasses []) #-}
{-# NOINLINE takeWhileP #-}
takeWhileP :: Int -> Int -> IO (Either ParseError ())
takeWhileP value =
    withStream value $
        Stream.parse
            (PR.takeWhileP (<= value)
                (PR.takeWhile (<= value - 1) Fold.drain))

#ifdef INSPECTION
-- inspect $ 'takeWhileP `hasNoType` ''S.Step
-- inspect $ 'takeWhileP `hasNoType` ''PR.Step
inspect $ 'takeWhileP `hasNoType` ''PR.Initial
inspect $ 'takeWhileP `hasNoType` ''FL.Step
inspect $ 'takeWhileP `hasNoType` ''SPEC
#endif

{-# ANN takeP (PermitPatternMatches []) #-}
{-# ANN takeP (PermitConstructions [''Either, ''()]) #-}
{-# ANN takeP (PermitTypeClasses []) #-}
{-# NOINLINE takeP #-}
takeP :: Int -> Int -> IO (Either ParseError ())
takeP value =
    withStream value
        $ Stream.parse (PR.takeP value (PR.fromFold Fold.drain))

{-# ANN groupBy (PermitPatternMatches []) #-}
{-# ANN groupBy (PermitConstructions [''()]) #-}
{-# ANN groupBy (PermitTypeClasses []) #-}
{-# NOINLINE groupBy #-}
groupBy :: Int -> Int -> IO (Either ParseError ())
groupBy value = withStream value $ Stream.parse (PR.groupBy (<=) Fold.drain)

#ifdef INSPECTION
inspect $ 'groupBy `hasNoType` ''S.Step
inspect $ 'groupBy `hasNoType` ''PR.Step
inspect $ 'groupBy `hasNoType` ''PR.Initial
inspect $ 'groupBy `hasNoType` ''FL.Step
inspect $ 'groupBy `hasNoType` ''SPEC
inspect $ 'groupBy `hasNoType` ''GroupByState
#endif

{-# ANN groupByRolling (PermitPatternMatches []) #-}
{-# ANN groupByRolling (PermitConstructions [''()]) #-}
{-# ANN groupByRolling (PermitTypeClasses []) #-}
{-# NOINLINE groupByRolling #-}
groupByRolling :: Int -> Int -> IO (Either ParseError ())
groupByRolling value =
    withStream value $ Stream.parse (PR.groupByRolling (<=) Fold.drain)

#ifdef INSPECTION
inspect $ 'groupByRolling `hasNoType` ''S.Step
inspect $ 'groupByRolling `hasNoType` ''PR.Step
inspect $ 'groupByRolling `hasNoType` ''PR.Initial
inspect $ 'groupByRolling `hasNoType` ''FL.Step
inspect $ 'groupByRolling `hasNoType` ''SPEC
inspect $ 'groupByRolling `hasNoType` ''GroupByState
#endif

{-# ANN wordBy (PermitPatternMatches
    [''(), ''[], ''Int, ''WordByState]) #-}
{-# ANN wordBy (PermitConstructions [''(), ''WordByState]) #-}
{-# ANN wordBy (PermitTypeClasses []) #-}
{-# NOINLINE wordBy #-}
wordBy :: Int -> Int -> IO (Either ParseError ())
wordBy value =
    withStream value $ Stream.parse (PR.wordBy (>= value) Fold.drain)

#ifdef INSPECTION
inspect $ 'wordBy `hasNoType` ''S.Step
inspect $ 'wordBy `hasNoType` ''PR.Step
inspect $ 'wordBy `hasNoType` ''PR.Initial
inspect $ 'wordBy `hasNoType` ''FL.Step
inspect $ 'wordBy `hasNoType` ''SPEC
#endif

{-# ANN takeEndBy_ (PermitPatternMatches []) #-}
{-# ANN takeEndBy_ (PermitConstructions [''Either, ''()]) #-}
{-# ANN takeEndBy_ (PermitTypeClasses []) #-}
{-# NOINLINE takeEndBy_ #-}
takeEndBy_ :: Int -> Int -> IO (Either ParseError ())
takeEndBy_ value =
    withStream value
        $ Stream.parse
            (PR.takeEndBy_ (>= value) (PR.fromFold Fold.drain))

#ifdef INSPECTION
inspect $ 'takeEndBy_ `hasNoType` ''S.Step
inspect $ 'takeEndBy_ `hasNoType` ''PR.Step
inspect $ 'takeEndBy_ `hasNoType` ''PR.Initial
inspect $ 'takeEndBy_ `hasNoType` ''FL.Step
inspect $ 'takeEndBy_ `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Spanning
-------------------------------------------------------------------------------

{-# ANN span (PermitPatternMatches
    [''[], ''(,), ''Int, ''SPEC, ''SeqParseState]) #-}
{-# ANN span (PermitConstructions
    [''SeqParseState, ''(), ''[], ''(,)]) #-}
{-# ANN span (PermitTypeClasses []) #-}
{-# NOINLINE span #-}
span :: Int -> Int -> IO (Either ParseError ((), ()))
span value =
    withStream value
        $ Stream.parse
            (PR.span (<= (value `div` 2)) Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'span `hasNoType` ''S.Step
inspect $ 'span `hasNoType` ''PR.Step
inspect $ 'span `hasNoType` ''PR.Initial
inspect $ 'span `hasNoType` ''FL.Step
-- inspect $ 'span `hasNoType` ''SPEC
-- inspect $ 'span `hasNoType` ''SeqParseState
#endif

{-# ANN spanBy (PermitPatternMatches
    [''[], ''(,), ''Int, ''SPEC, ''SeqParseState, ''GroupByState]) #-}
{-# ANN spanBy (PermitConstructions
    [''GroupByState, ''(), ''SeqParseState, ''[], ''(,), ''Int]) #-}
{-# ANN spanBy (PermitTypeClasses []) #-}
{-# NOINLINE spanBy #-}
spanBy :: Int -> Int -> IO (Either ParseError ((), ()))
spanBy value =
    withStream value $
        Stream.parse
            (PR.spanBy
                (\_ i -> i <= (value `div` 2)) Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'spanBy `hasNoType` ''S.Step
inspect $ 'spanBy `hasNoType` ''PR.Step
inspect $ 'spanBy `hasNoType` ''PR.Initial
inspect $ 'spanBy `hasNoType` ''FL.Step
-- inspect $ 'spanBy `hasNoType` ''SPEC
-- inspect $ 'spanBy `hasNoType` ''SeqParseState
-- inspect $ 'spanBy `hasNoType` ''GroupByState
#endif

{-# ANN spanByRolling (PermitPatternMatches
    [''[], ''(,), ''Int, ''SPEC, ''SeqParseState, ''GroupByState]) #-}
{-# ANN spanByRolling (PermitConstructions
    [''GroupByState, ''(), ''SeqParseState, ''[], ''(,), ''Int]) #-}
{-# ANN spanByRolling (PermitTypeClasses []) #-}
{-# NOINLINE spanByRolling #-}
spanByRolling :: Int -> Int -> IO (Either ParseError ((), ()))
spanByRolling value =
    withStream value $
        Stream.parse
            (PR.spanByRolling
                (\_ i -> i <= value `div` 2) Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'spanByRolling `hasNoType` ''S.Step
inspect $ 'spanByRolling `hasNoType` ''PR.Step
inspect $ 'spanByRolling `hasNoType` ''PR.Initial
inspect $ 'spanByRolling `hasNoType` ''FL.Step
-- inspect $ 'spanByRolling `hasNoType` ''SPEC
-- inspect $ 'spanByRolling `hasNoType` ''SeqParseState
-- inspect $ 'spanByRolling `hasNoType` ''GroupByState
#endif

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

{-# ANN lookAhead (PermitPatternMatches [''String]) #-}
{-# ANN lookAhead (PermitConstructions [''Either, ''(), ''[]]) #-}
{-# ANN lookAhead (PermitTypeClasses []) #-}
{-# NOINLINE lookAhead #-}
lookAhead :: Int -> Int -> IO (Either ParseError ())
lookAhead value =
    withStream value $
        Stream.parse (PR.lookAhead (PR.takeWhile (<= value) Fold.drain) $> ())

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
    -- lookahead sequence length
    -- lookahead benchmark holds the entire input till end
      (HeapO_n, benchIO "lookAhead" $ lookAhead value)

    -- take sequence by length
    , (SpaceO_1, benchIO "takeBetween" $ takeBetween value)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (HeapO_n, benchIO "takeEQ" $ takeEQ value)
    , (HeapO_n, benchIO "takeGE" $ takeGE value)
    , (SpaceO_1, benchIO "takeP" $ takeP value)

    -- Match exact sequence
    -- O_n because of the list accumulation
    , (HeapO_n, benchIO "listEqBy" $ listEqBy value)
    , (SpaceO_1, benchIO "streamEqBy" $ streamEqBy value)

    -- sequence matching a predicate
    , (SpaceO_1, benchIO "takeWhile" $ takeWhile value)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "takeWhileP" $ takeWhileP value)
    , (SpaceO_1, benchIO "dropWhile" $ dropWhile value)

    -- sequence begin/end by known elements
    , (SpaceO_1, benchIO "takeEndBy_" $ takeEndBy_ value)
    , (SpaceO_1, benchIO "takeBeginBy" $ takeBeginBy value)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "wordBy" $ wordBy value)

    -- Group sequence by
    , (SpaceO_1, benchIO "groupBy" $ groupBy value)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "groupByRolling" $ groupByRolling value)

    -- Framing
    -- o-n-heap because of backtracking
    , (HeapO_n, benchIO "takeFramedByEsc_" $ takeFramedByEsc_ value)

    -- Spanning
    , (SpaceO_1, benchIO "span" $ span value)
    , (SpaceO_1, benchIO "spanBy" $ spanBy value)
    , (SpaceO_1, benchIO "spanByRolling" $ spanByRolling value)
    ]
