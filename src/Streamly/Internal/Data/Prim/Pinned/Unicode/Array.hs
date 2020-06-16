{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Streamly.Internal.Data.Prim.Pinned.Unicode.Array
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Prim.Pinned.Unicode.Array
    (
    -- * Streams of Strings
      lines
    , words
    , unlines
    , unwords
    )
where

import Control.Monad.Primitive (PrimMonad)
import Streamly (IsStream, MonadAsync)
import Prelude hiding (String, lines, words, unlines, unwords)
import Streamly.Internal.Data.Prim.Pinned.Array (Array)

import qualified Streamly.Internal.Data.Unicode.Stream as S
import qualified Streamly.Internal.Data.Prim.Pinned.Array as A

-- | Break a string up into a stream of strings at newline characters.
-- The resulting strings do not contain newlines.
--
-- > lines = S.lines A.write
--
-- >>> S.toList $ lines $ S.fromList "lines\nthis\nstring\n\n\n"
-- ["lines","this","string","",""]
--
{-# INLINE lines #-}
lines :: (PrimMonad m, IsStream t) => t m Char -> t m (Array Char)
lines = S.lines A.write

-- | Break a string up into a stream of strings, which were delimited
-- by characters representing white space.
--
-- > words = S.words A.write
--
-- >>> S.toList $ words $ S.fromList "A  newline\nis considered white space?"
-- ["A", "newline", "is", "considered", "white", "space?"]
--
{-# INLINE words #-}
words :: (PrimMonad m, IsStream t) => t m Char -> t m (Array Char)
words = S.words A.write

-- | Flattens the stream of @Array Char@, after appending a terminating
-- newline to each string.
--
-- 'unlines' is an inverse operation to 'lines'.
--
-- >>> S.toList $ unlines $ S.fromList ["lines", "this", "string"]
-- "lines\nthis\nstring\n"
--
-- > unlines = S.unlines A.read
--
-- Note that, in general
--
-- > unlines . lines /= id
{-# INLINE unlines #-}
unlines :: (MonadAsync m, PrimMonad m, IsStream t) => t m (Array Char) -> t m Char
unlines = S.unlines A.read

-- | Flattens the stream of @Array Char@, after appending a separating
-- space to each string.
--
-- 'unwords' is an inverse operation to 'words'.
--
-- >>> S.toList $ unwords $ S.fromList ["unwords", "this", "string"]
-- "unwords this string"
--
-- > unwords = S.unwords A.read
--
-- Note that, in general
--
-- > unwords . words /= id
{-# INLINE unwords #-}
unwords :: (MonadAsync m, PrimMonad m, IsStream t) => t m (Array Char) -> t m Char
unwords = S.unwords A.read
