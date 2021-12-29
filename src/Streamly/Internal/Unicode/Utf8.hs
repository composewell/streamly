-- |
-- Module      : Streamly.Internal.Unicode.Utf8
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
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
import qualified Prelude
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Streamly.Internal.Unicode.Utf8.Type
import Streamly.Internal.Unicode.Utf8.Transform
import Streamly.Internal.Unicode.Utf8.Eliminate
import Streamly.Internal.Unicode.Utf8.Generate
import Streamly.Internal.Unicode.Utf8.Reduce

import Prelude hiding
    ( all
    , any
    , break
    , concat
    , concatMap
    , drop
    , dropWhile
    , elem
    , filter
    , foldl
    , foldl1
    , foldr
    , foldr1
    , head
    , init
    , last
    , length
    , lines
    , map
    , maximum
    , minimum
    , null
    , read
    , replicate
    , reverse
    , scanl
    , scanl1
    , scanr
    , scanr1
    , span
    , splitAt
    , tail
    , take
    , takeWhile
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

{-# INLINE concat #-}
-- | /O(n)/ Concatenate a list of 'Utf8's.
concat :: [Utf8] -> Utf8
concat ts =
    case Prelude.filter (not . null) ts of
        [] -> empty
        [t] -> t
        xs -> Prelude.foldl1 append xs

{-# INLINE concatMap #-}
-- | /O(n)/ Map a function over a 'Utf8' that results in a 'Utf8', and
-- concatenate the results.
concatMap :: (Char -> Utf8) -> Utf8 -> Utf8
concatMap f = concat . foldr ((:) . f) []

--------------------------------------------------------------------------------
-- Zipping
--------------------------------------------------------------------------------

-- | /O(n)/ 'zip' takes two 'Utf8's and returns a list of
-- corresponding pairs of bytes. If one input 'Utf8' is short,
-- excess elements of the longer 'Utf8' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
{-# INLINE zip #-}
zip :: Utf8 -> Utf8 -> [(Char,Char)]
zip a b = unsafePerformIO $ Stream.toList $ Stream.zipWith (,) (stream a) (stream b)

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
{-# INLINE zipWith #-}
zipWith :: (Char -> Char -> Char) -> Utf8 -> Utf8 -> Utf8
zipWith f a b = unstream (Stream.zipWith f (stream a) (stream b))

-- | /O(n)/ Breaks a 'Utf8' up into a list of words, delimited by 'Char's
-- representing white space.
{-# INLINE words #-}
words :: Utf8 -> [Utf8]
words = split isSpace

-- | /O(n)/ Breaks a 'Utf8' up into a list of 'Utf8's at
-- newline 'Char's. The resulting strings do not contain newlines.
{-# INLINE lines #-}
lines :: Utf8 -> [Utf8]
lines = split (== '\n')

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
{-# INLINE unlines #-}
unlines :: [Utf8] -> Utf8
unlines = concat . List.map (`snoc` '\n')

-- | /O(n)/ Joins words using single space characters.
{-# INLINE unwords #-}
unwords :: [Utf8] -> Utf8
unwords = intercalate (singleton ' ')
