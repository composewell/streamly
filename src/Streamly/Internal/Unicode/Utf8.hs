-- |
-- Module      : Streamly.Internal.Unicode.Utf8
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module mimics the API of the text package. Some documentation snippets
-- may have been taken from the text package.
module Streamly.Internal.Unicode.Utf8
    (

      module Streamly.Internal.Unicode.Utf8.Type
    , module Streamly.Internal.Unicode.Utf8.Transform
    , module Streamly.Internal.Unicode.Utf8.Eliminate
    , module Streamly.Internal.Unicode.Utf8.Generate
    , module Streamly.Internal.Unicode.Utf8.Reduce

    -- * Folds

    -- ** Special folds
    , concat
    , concatMap

    -- * Substrings

    -- ** Breaking into lines and words
    , lines
    --, lines'
    , words
    , unlines
    , unwords

    -- * Zipping
    , zip
    , zipWith

    -- -* Ordered
    -- , sort

    -- -- * Low level operations
    -- , copy
    -- , unpackCString#

    )
where

#include "inline.hs"

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Char (isSpace)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.List as List
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Streamly.Internal.Unicode.Utf8.Type
import Streamly.Internal.Unicode.Utf8.Transform
import Streamly.Internal.Unicode.Utf8.Eliminate
import Streamly.Internal.Unicode.Utf8.Generate
import Streamly.Internal.Unicode.Utf8.Reduce

import Prelude hiding
    ( concat
    , concatMap
    , foldr
    , lines
    , null
    , unlines
    , unwords
    , words
    , zip
    , zipWith
    )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Internal.Unicode.Utf8 as Utf8

--------------------------------------------------------------------------------
-- Special folds
--------------------------------------------------------------------------------

-- XXX We should write these APIs generalized on Array a and then just use those
-- for the Utf8 type. The generalized APIs would be more useful, they can go in
-- the Array module itself and can be used generally for arrays, you won't need
-- to transform arrays into stream and then back for such common operations.
-- | Concatenate a list of 'Utf8's.
--
-- /Time complexity:/ O(n)
{-# INLINE concat #-}
concat :: [Utf8] -> Utf8
concat ts =
    case Prelude.filter (not . null) ts of
        [] -> empty
        [t] -> t
        xs -> Prelude.foldl1 append xs

-- | Map a function over a 'Utf8' that results in a 'Utf8', and
-- concatenate the results.
--
-- /Time complexity:/ O(n)
{-# INLINE concatMap #-}
concatMap :: (Char -> Utf8) -> Utf8 -> Utf8
concatMap f = concat . foldr ((:) . f) []

--------------------------------------------------------------------------------
-- Zipping
--------------------------------------------------------------------------------

-- | 'zip' takes two 'Utf8's and returns a list of
-- corresponding pairs of bytes. If one input 'Utf8' is short,
-- excess elements of the longer 'Utf8' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
--
-- /Time complexity:/ O(n)
{-# INLINE zip #-}
zip :: Utf8 -> Utf8 -> [(Char,Char)]
zip a b =
    unsafePerformIO
        $ Stream.toList $ Stream.zipWith (,) (toStream a) (toStream b)

-- | 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
{-# INLINE zipWith #-}
zipWith :: (Char -> Char -> Char) -> Utf8 -> Utf8 -> Utf8
zipWith f a b = fromStream (Stream.zipWith f (toStream a) (toStream b))

-- | Breaks a 'Utf8' up into a list of words, delimited by 'Char's
-- representing white space.
--
-- /Time complexity:/ O(n)
{-# INLINE words #-}
words :: Utf8 -> [Utf8]
words = split isSpace

-- | Breaks a 'Utf8' up into a list of 'Utf8's at
-- newline 'Char's. The resulting strings do not contain newlines.
--
-- /Time complexity:/ O(n)
{-# INLINE lines #-}
lines :: Utf8 -> [Utf8]
lines = split (== '\n')

-- | Joins lines, after appending a terminating newline to
-- each.
--
-- /Time complexity:/ O(n)
{-# INLINE unlines #-}
unlines :: [Utf8] -> Utf8
unlines = concat . List.map (`snoc` '\n')

-- | Joins words using single space characters.
--
-- /Time complexity:/ O(n)
{-# INLINE unwords #-}
unwords :: [Utf8] -> Utf8
unwords = intercalate (singleton ' ')
