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
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Test.Tasty.Bench (Benchmark, bench, nfIO)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Prelude hiding (takeWhile, dropWhile, span)

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

{-# INLINE takeBetween #-}
takeBetween :: Int -> IO (Either ParseError ())
takeBetween value = withStream value $ Stream.parse (PR.takeBetween 0 value Fold.drain)

#ifdef INSPECTION
inspect $ 'takeBetween `hasNoType` ''S.Step
inspect $ 'takeBetween `hasNoType` ''PR.Step
-- inspect $ 'takeBetween `hasNoType` ''PR.Initial
inspect $ 'takeBetween `hasNoType` ''FL.Step
inspect $ 'takeBetween `hasNoType` ''SPEC
-- inspect $ 'takeBetween `hasNoType` ''PR.Tuple'Fused
#endif

{-# INLINE takeEQ #-}
takeEQ :: Int -> IO (Either ParseError ())
takeEQ value = withStream value $ Stream.parse (PR.takeEQ value Fold.drain)

#ifdef INSPECTION
inspect $ 'takeEQ `hasNoType` ''S.Step
inspect $ 'takeEQ `hasNoType` ''PR.Step
inspect $ 'takeEQ `hasNoType` ''PR.Initial
inspect $ 'takeEQ `hasNoType` ''FL.Step
inspect $ 'takeEQ `hasNoType` ''SPEC
inspect $ 'takeEQ `hasNoType` ''PR.Tuple'Fused
#endif

{-# INLINE takeGE #-}
takeGE :: Int -> IO (Either ParseError ())
takeGE value = withStream value $ Stream.parse (PR.takeGE value Fold.drain)

#ifdef INSPECTION
-- inspect $ 'takeGE `hasNoType` ''S.Step
inspect $ 'takeGE `hasNoType` ''PR.Step
inspect $ 'takeGE `hasNoType` ''PR.Initial
inspect $ 'takeGE `hasNoType` ''FL.Step
inspect $ 'takeGE `hasNoType` ''SPEC
inspect $ 'takeGE `hasNoType` ''PR.TakeGEState
#endif

{-# INLINE dropWhile #-}
dropWhile :: Int -> IO (Either ParseError ())
dropWhile value = withStream value $ Stream.parse (PR.dropWhile (<= value))

#ifdef INSPECTION
inspect $ 'dropWhile `hasNoType` ''S.Step
inspect $ 'dropWhile `hasNoType` ''PR.Step
inspect $ 'dropWhile `hasNoType` ''PR.Initial
inspect $ 'dropWhile `hasNoType` ''FL.Step
inspect $ 'dropWhile `hasNoType` ''SPEC
#endif

{-# INLINE takeBeginBy #-}
takeBeginBy :: Int -> IO (Either ParseError ())
takeBeginBy value = do
    n <- randomRIO (1, 1)
    let stream = value `Stream.cons` streamUnfoldrM value n
    Stream.parse (PR.takeBeginBy (== value) Fold.drain) stream

#ifdef INSPECTION
inspect $ 'takeBeginBy `hasNoType` ''S.Step
inspect $ 'takeBeginBy `hasNoType` ''PR.Step
inspect $ 'takeBeginBy `hasNoType` ''PR.Initial
inspect $ 'takeBeginBy `hasNoType` ''FL.Step
inspect $ 'takeBeginBy `hasNoType` ''SPEC
#endif

{-# INLINE takeFramedByEsc_ #-}
takeFramedByEsc_ :: Int -> IO (Either ParseError ())
takeFramedByEsc_ value = do
    n <- randomRIO (1, 1)
    Stream.parse parser (sourceEscapedFrames value n)

    where

    isEsc = (== '\\')
    isBegin = (== '{')
    isEnd = (== '}')

    parser = PR.takeFramedByEsc_ isEsc isBegin isEnd Fold.drain

{-# INLINE listEqBy #-}
listEqBy :: Int -> IO (Either ParseError [Int])
listEqBy value = withStream value $ Stream.parse (PR.listEqBy (==) [1 .. value])

{-# INLINE streamEqBy #-}
streamEqBy :: Int -> IO (Either ParseError ())
streamEqBy value =
    withStream value $ Stream.parse (PR.streamEqBy (==) (Stream.enumerateFromTo 1 value))

{-# INLINE takeWhile #-}
takeWhile :: Int -> IO (Either ParseError ())
takeWhile value = withStream value $ Stream.parse (PR.takeWhile (<= value) Fold.drain)

#ifdef INSPECTION
inspect $ 'takeWhile `hasNoType` ''S.Step
inspect $ 'takeWhile `hasNoType` ''PR.Step
inspect $ 'takeWhile `hasNoType` ''PR.Initial
inspect $ 'takeWhile `hasNoType` ''FL.Step
inspect $ 'takeWhile `hasNoType` ''SPEC
#endif

{-# INLINE takeWhileP #-}
takeWhileP :: Int -> IO (Either ParseError ())
takeWhileP value =
    withStream value $
        Stream.parse (PR.takeWhileP (<= value) (PR.takeWhile (<= value - 1) Fold.drain))

#ifdef INSPECTION
-- inspect $ 'takeWhileP `hasNoType` ''S.Step
-- inspect $ 'takeWhileP `hasNoType` ''PR.Step
inspect $ 'takeWhileP `hasNoType` ''PR.Initial
inspect $ 'takeWhileP `hasNoType` ''FL.Step
inspect $ 'takeWhileP `hasNoType` ''SPEC
#endif

{-# INLINE takeP #-}
takeP :: Int -> IO (Either ParseError ())
takeP value = withStream value $ Stream.parse (PR.takeP value (PR.fromFold Fold.drain))

{-# INLINE groupBy #-}
groupBy :: Int -> IO (Either ParseError ())
groupBy value = withStream value $ Stream.parse (PR.groupBy (<=) Fold.drain)

#ifdef INSPECTION
inspect $ 'groupBy `hasNoType` ''S.Step
inspect $ 'groupBy `hasNoType` ''PR.Step
inspect $ 'groupBy `hasNoType` ''PR.Initial
inspect $ 'groupBy `hasNoType` ''FL.Step
inspect $ 'groupBy `hasNoType` ''SPEC
inspect $ 'groupBy `hasNoType` ''PR.GroupByState
#endif

{-# INLINE groupByRolling #-}
groupByRolling :: Int -> IO (Either ParseError ())
groupByRolling value = withStream value $ Stream.parse (PR.groupByRolling (<=) Fold.drain)

#ifdef INSPECTION
inspect $ 'groupByRolling `hasNoType` ''S.Step
inspect $ 'groupByRolling `hasNoType` ''PR.Step
inspect $ 'groupByRolling `hasNoType` ''PR.Initial
inspect $ 'groupByRolling `hasNoType` ''FL.Step
inspect $ 'groupByRolling `hasNoType` ''SPEC
inspect $ 'groupByRolling `hasNoType` ''PR.GroupByState
#endif

{-# INLINE wordBy #-}
wordBy :: Int -> IO (Either ParseError ())
wordBy value = withStream value $ Stream.parse (PR.wordBy (>= value) Fold.drain)

#ifdef INSPECTION
inspect $ 'wordBy `hasNoType` ''S.Step
inspect $ 'wordBy `hasNoType` ''PR.Step
inspect $ 'wordBy `hasNoType` ''PR.Initial
inspect $ 'wordBy `hasNoType` ''FL.Step
inspect $ 'wordBy `hasNoType` ''SPEC
#endif

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Int -> IO (Either ParseError ())
takeEndBy_ value =
    withStream value $ Stream.parse (PR.takeEndBy_ (>= value) (PR.fromFold Fold.drain))

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

{-# INLINE span #-}
span :: Int -> IO (Either ParseError ((), ()))
span value =
    withStream value $ Stream.parse (PR.span (<= (value `div` 2)) Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'span `hasNoType` ''S.Step
inspect $ 'span `hasNoType` ''PR.Step
inspect $ 'span `hasNoType` ''PR.Initial
inspect $ 'span `hasNoType` ''FL.Step
-- inspect $ 'span `hasNoType` ''SPEC
-- inspect $ 'span `hasNoType` ''PR.SeqParseState
#endif

{-# INLINE spanBy #-}
spanBy :: Int -> IO (Either ParseError ((), ()))
spanBy value =
    withStream value $
        Stream.parse (PR.spanBy (\_ i -> i <= (value `div` 2)) Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'spanBy `hasNoType` ''S.Step
inspect $ 'spanBy `hasNoType` ''PR.Step
inspect $ 'spanBy `hasNoType` ''PR.Initial
inspect $ 'spanBy `hasNoType` ''FL.Step
-- inspect $ 'spanBy `hasNoType` ''SPEC
-- inspect $ 'spanBy `hasNoType` ''PR.SeqParseState
-- inspect $ 'spanBy `hasNoType` ''PR.GroupByState
#endif

{-# INLINE spanByRolling #-}
spanByRolling :: Int -> IO (Either ParseError ((), ()))
spanByRolling value =
    withStream value $
        Stream.parse (PR.spanByRolling (\_ i -> i <= value `div` 2) Fold.drain Fold.drain)

#ifdef INSPECTION
inspect $ 'spanByRolling `hasNoType` ''S.Step
inspect $ 'spanByRolling `hasNoType` ''PR.Step
inspect $ 'spanByRolling `hasNoType` ''PR.Initial
inspect $ 'spanByRolling `hasNoType` ''FL.Step
-- inspect $ 'spanByRolling `hasNoType` ''SPEC
-- inspect $ 'spanByRolling `hasNoType` ''PR.SeqParseState
-- inspect $ 'spanByRolling `hasNoType` ''PR.GroupByState
#endif

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

{-# INLINE lookAhead #-}
lookAhead :: Int -> IO (Either ParseError ())
lookAhead value =
    withStream value $
        Stream.parse (PR.lookAhead (PR.takeWhile (<= value) Fold.drain) $> ())

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

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
