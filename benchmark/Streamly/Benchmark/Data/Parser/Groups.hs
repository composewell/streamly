#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
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
import Test.Tasty.Bench (Benchmark)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.Parser.Common
import Prelude hiding (takeWhile, dropWhile, span)

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
takeBetween :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeBetween value = Stream.parse (PR.takeBetween 0 value Fold.drain)

{-# INLINE takeEQ #-}
takeEQ :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeEQ value = Stream.parse (PR.takeEQ value Fold.drain)

{-# INLINE takeGE #-}
takeGE :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeGE value = Stream.parse (PR.takeGE value Fold.drain)

{-# INLINE dropWhile #-}
dropWhile :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
dropWhile value = Stream.parse (PR.dropWhile (<= value))

{-# INLINE takeBeginBy #-}
takeBeginBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeBeginBy value stream = do
    let stream2 = value `Stream.cons` stream
    Stream.parse (PR.takeBeginBy (== value) Fold.drain) stream2

takeFramedByEsc_ :: Monad m => Int -> Stream m Char -> m (Either ParseError ())
takeFramedByEsc_ _ = Stream.parse parser

    where

    isEsc = (== '\\')
    isBegin = (== '{')
    isEnd = (== '}')

    parser = PR.takeFramedByEsc_ isEsc isBegin isEnd Fold.drain

{-# INLINE listEqBy #-}
listEqBy :: Int -> Stream IO Int -> IO (Either ParseError [Int])
listEqBy len = Stream.parse (PR.listEqBy (==) [1 .. len])

{-# INLINE streamEqBy #-}
streamEqBy :: Int -> Stream IO Int -> IO (Either ParseError ())
streamEqBy len = Stream.parse (PR.streamEqBy (==) (Stream.enumerateFromTo 1 len))

{-# INLINE takeWhile #-}
takeWhile :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeWhile value = Stream.parse (PR.takeWhile (<= value) Fold.drain)

takeWhileP :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeWhileP value =
    Stream.parse (PR.takeWhileP (<= value) (PR.takeWhile (<= value - 1) Fold.drain))

{-# INLINE takeP #-}
takeP :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeP value = Stream.parse (PR.takeP value (PR.fromFold Fold.drain))

{-# INLINE groupBy #-}
groupBy :: Monad m => Stream m Int -> m (Either ParseError ())
groupBy = Stream.parse (PR.groupBy (<=) Fold.drain)

{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => Stream m Int -> m (Either ParseError ())
groupByRolling = Stream.parse (PR.groupByRolling (<=) Fold.drain)

{-# INLINE wordBy #-}
wordBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
wordBy value = Stream.parse (PR.wordBy (>= value) Fold.drain)

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
takeEndBy_ value = Stream.parse (PR.takeEndBy_ (>= value) (PR.fromFold Fold.drain))

-------------------------------------------------------------------------------
-- Spanning
-------------------------------------------------------------------------------

{-# INLINE span #-}
span :: Monad m => Int -> Stream m Int -> m (Either ParseError ((), ()))
span value = Stream.parse (PR.span (<= (value `div` 2)) Fold.drain Fold.drain)

{-# INLINE spanBy #-}
spanBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ((), ()))
spanBy value =
    Stream.parse (PR.spanBy (\_ i -> i <= (value `div` 2)) Fold.drain Fold.drain)

{-# INLINE spanByRolling #-}
spanByRolling :: Monad m => Int -> Stream m Int -> m (Either ParseError ((), ()))
spanByRolling value =
    Stream.parse (PR.spanByRolling (\_ i -> i <= value `div` 2) Fold.drain Fold.drain)

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

{-# INLINE lookAhead #-}
lookAhead :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
lookAhead value =
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
      (HeapO_n, benchIOSink value "lookAhead" $ lookAhead value)

    -- take sequence by length
    , (SpaceO_1, benchIOSink value "takeBetween" $ takeBetween value)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (HeapO_n, benchIOSink value "takeEQ" $ takeEQ value)
    , (HeapO_n, benchIOSink value "takeGE" $ takeGE value)
    , (SpaceO_1, benchIOSink value "takeP" $ takeP value)

    -- Match exact sequence
    -- O_n because of the list accumulation
    , (HeapO_n, benchIOSink value "listEqBy" (listEqBy value))
    , (SpaceO_1, benchIOSink value "streamEqBy" (streamEqBy value))

    -- sequence matching a predicate
    , (SpaceO_1, benchIOSink value "takeWhile" $ takeWhile value)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIOSink value "takeWhileP" $ takeWhileP value)
    , (SpaceO_1, benchIOSink value "dropWhile" $ dropWhile value)

    -- sequence begin/end by known elements
    , (SpaceO_1, benchIOSink value "takeEndBy_" $ takeEndBy_ value)
    , (SpaceO_1, benchIOSink value "takeBeginBy" $ takeBeginBy value)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIOSink value "wordBy" $ wordBy value)

    -- Group sequence by
    , (SpaceO_1, benchIOSink value "groupBy" groupBy)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIOSink value "groupByRolling" groupByRolling)

    -- Framing
    -- o-n-heap because of backtracking
    , (HeapO_n, benchIOSrc sourceEscapedFrames value "takeFramedByEsc_"
        $ takeFramedByEsc_ value)

    -- Spanning
    , (SpaceO_1, benchIOSink value "span" $ span value)
    , (SpaceO_1, benchIOSink value "spanBy" $ spanBy value)
    , (SpaceO_1, benchIOSink value "spanByRolling" $ spanByRolling value)
    ]
