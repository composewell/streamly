-- |
-- Module      : Streamly.Internal.Unicode.Array.Prim.Pinned
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Array.Prim.Pinned
    (
    -- * Streams of Strings
      lines
    , words
    , unlines
    , unwords
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Prelude (MonadAsync)
import Prelude hiding (String, lines, words, unlines, unwords)
import Streamly.Internal.Data.Stream.IsStream (IsStream)
import Streamly.Internal.Data.Array.Prim.Pinned (Array)

import qualified Streamly.Internal.Unicode.Stream as S
import qualified Streamly.Internal.Data.Array.Prim.Pinned as A

-- $setup
-- >>> :m
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Unicode.Array.Prim.Pinned as Unicode

-- | Break a string up into a stream of strings at newline characters.
-- The resulting strings do not contain newlines.
--
-- prop> lines = S.lines A.write
--
-- >>> Stream.toList $ Unicode.lines $ Stream.fromList "lines\nthis\nstring\n\n\n"
-- [fromListN 5 "lines",fromListN 4 "this",fromListN 6 "string",fromListN 0 "",fromListN 0 ""]
--
{-# INLINE lines #-}
lines :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
lines = S.lines A.write

-- | Break a string up into a stream of strings, which were delimited
-- by characters representing white space.
--
-- prop> words = S.words A.write
--
-- >>> Stream.toList $ Unicode.words $ Stream.fromList "A  newline\nis considered white space?"
-- [fromListN 1 "A",fromListN 7 "newline",fromListN 2 "is",fromListN 10 "considered",fromListN 5 "white",fromListN 6 "space?"]
--
{-# INLINE words #-}
words :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
words = S.words A.write

-- | Flattens the stream of @Array Char@, after appending a terminating
-- newline to each string.
--
-- 'unlines' is an inverse operation to 'lines'.
--
-- >>> Stream.toList $ Unicode.unlines $ Stream.fromList ["lines", "this", "string"]
-- "lines\nthis\nstring\n"
--
-- prop> unlines = S.unlines A.read
--
-- Note that, in general
--
-- prop> unlines . lines /= id
{-# INLINE unlines #-}
unlines :: (MonadAsync m, IsStream t) => t m (Array Char) -> t m Char
unlines = S.unlines A.read

-- | Flattens the stream of @Array Char@, after appending a separating
-- space to each string.
--
-- 'unwords' is an inverse operation to 'words'.
--
-- >>> Stream.toList $ Unicode.unwords $ Stream.fromList ["unwords", "this", "string"]
-- "unwords this string"
--
-- prop> unwords = S.unwords A.read
--
-- Note that, in general
--
-- prop> unwords . words /= id
{-# INLINE unwords #-}
unwords :: (MonadAsync m, IsStream t) => t m (Array Char) -> t m Char
unwords = S.unwords A.read
