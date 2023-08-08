-- |
-- Module      : Streamly.Internal.Unicode.Array
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- XXX Remove this or move this to Unicode.Utf32 making "Array Char" as a
-- newtype wrapper for Utf32. Is this any better than the [Char] (String) type?
-- This provides random access and the length of the string in O(1). Also,
-- better append performance.
--
module Streamly.Internal.Unicode.Array
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Streams of Strings
      lines
    , words
    , unlines
    , unwords
    )
where

import Control.Monad.IO.Class (MonadIO)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Array (Array)

import qualified Streamly.Data.Array as A
import qualified Streamly.Internal.Unicode.Stream as S

import Prelude hiding (String, lines, words, unlines, unwords)

-- $setup
-- >>> :m
-- >>> :set -XOverloadedStrings
-- >>> import Prelude hiding (String, lines, words, unlines, unwords)
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Unicode.Array as Unicode

-- | Break a string up into a stream of strings at newline characters.
-- The resulting strings do not contain newlines.
--
-- > lines = S.lines A.write
--
-- >>> Stream.fold Fold.toList $ Unicode.lines $ Stream.fromList "lines\nthis\nstring\n\n\n"
-- [fromList "lines",fromList "this",fromList "string",fromList "",fromList ""]
--
{-# INLINE lines #-}
lines :: MonadIO m => Stream m Char -> Stream m (Array Char)
lines = S.lines A.write

-- | Break a string up into a stream of strings, which were delimited
-- by characters representing white space.
--
-- > words = S.words A.write
--
-- >>> Stream.fold Fold.toList $ Unicode.words $ Stream.fromList "A  newline\nis considered white space?"
-- [fromList "A",fromList "newline",fromList "is",fromList "considered",fromList "white",fromList "space?"]
--
{-# INLINE words #-}
words :: MonadIO m => Stream m Char -> Stream m (Array Char)
words = S.words A.write

-- | Flattens the stream of @Array Char@, after appending a terminating
-- newline to each string.
--
-- 'unlines' is an inverse operation to 'lines'.
--
-- >>> Stream.fold Fold.toList $ Unicode.unlines $ Stream.fromList ["lines", "this", "string"]
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
-- >>> Stream.fold Fold.toList $ Unicode.unwords $ Stream.fromList ["unwords", "this", "string"]
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
