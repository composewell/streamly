-- |
-- Module      : Streamly.Internal.Unicode.Array.Char
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Array.Char
    (
    -- * Streams of Strings
      lines
    , words
    , unlines
    , unwords
    )
where

import Control.Monad.IO.Class (MonadIO)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Array.Unboxed (Array)

import qualified Streamly.Data.Array.Unboxed as A
import qualified Streamly.Internal.Unicode.Stream as S

import Prelude hiding (String, lines, words, unlines, unwords)

-- $setup
-- >>> :m
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (String, lines, words, unlines, unwords)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Unicode.Array.Char as Unicode

-- | Break a string up into a stream of strings at newline characters.
-- The resulting strings do not contain newlines.
--
-- > lines = S.lines A.write
--
-- >>> Stream.toList $ Unicode.lines $ Stream.fromList "lines\nthis\nstring\n\n\n"
-- ["lines","this","string","",""]
--
{-# INLINE lines #-}
lines :: MonadIO m => Stream m Char -> Stream m (Array Char)
lines = S.lines A.write

-- | Break a string up into a stream of strings, which were delimited
-- by characters representing white space.
--
-- > words = S.words A.write
--
-- >>> Stream.toList $ Unicode.words $ Stream.fromList "A  newline\nis considered white space?"
-- ["A","newline","is","considered","white","space?"]
--
{-# INLINE words #-}
words :: MonadIO m => Stream m Char -> Stream m (Array Char)
words = S.words A.write

-- | Flattens the stream of @Array Char@, after appending a terminating
-- newline to each string.
--
-- 'unlines' is an inverse operation to 'lines'.
--
-- >>> Stream.toList $ Unicode.unlines $ Stream.fromList ["lines", "this", "string"]
-- "lines\nthis\nstring\n"
--
-- > unlines = S.unlines A.read
--
-- Note that, in general
--
-- > unlines . lines /= id
{-# INLINE unlines #-}
unlines :: MonadIO m => Stream m (Array Char) -> Stream m Char
unlines = S.unlines A.reader

-- | Flattens the stream of @Array Char@, after appending a separating
-- space to each string.
--
-- 'unwords' is an inverse operation to 'words'.
--
-- >>> Stream.toList $ Unicode.unwords $ Stream.fromList ["unwords", "this", "string"]
-- "unwords this string"
--
-- > unwords = S.unwords A.read
--
-- Note that, in general
--
-- > unwords . words /= id
{-# INLINE unwords #-}
unwords :: MonadIO m => Stream m (Array Char) -> Stream m Char
unwords = S.unwords A.reader
