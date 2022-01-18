-- |
-- Module      : Streamly.Internal.Unicode.Utf8.Eliminate
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Utf8.Eliminate
    (
    -- * Folds
      fold
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

    -- ** Special folds
    , any
    , all
    , maximum
    , minimum

    -- * Predicates

    -- XXX Comment our further stuff to help with the GHCJS compilation
    -- , isPrefixOf
    -- , isSuffixOf
    -- XXX isInfixOf takes too much memory to compile
    -- , isInfixOf

    -- ** View patterns

    -- XXX Comment our further stuff to help with the GHCJS compilation
    -- , stripPrefix
    -- , stripSuffix
    , commonPrefixes

    -- * Searching
    , find
    , elem

    -- * Indexing
    -- $index
    , index
    , findIndex
    , countChar
    , count
    )
where

#include "inline.hs"

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream.IsStream.Lift (hoist)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Streamly.Internal.Unicode.Utf8.Type

import Prelude hiding
    ( all
    , any
    , elem
    , foldl
    , foldl1
    , foldr
    , foldr1
    , maximum
    , minimum
    )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Internal.Unicode.Utf8 as Utf8

--------------------------------------------------------------------------------
-- Reducing Streams (folds)
--------------------------------------------------------------------------------

{-# INLINE fold #-}
fold :: MonadIO m => Fold m Char b -> Utf8 -> m b
fold f = Stream.fold f . hoist liftIO . toStream

-- | 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Utf8',
--
-- /Time complexity:/ O(n)
-- reduces the 'Utf8' using the binary operator, from left to right.
--
{-# INLINE foldl #-}
foldl :: (a -> Char -> a) -> a -> Utf8 -> a
foldl = undefined

-- | A strict version of 'foldl'.
--
-- /Time complexity:/ O(n)
{-# INLINE foldl' #-}
foldl' :: (a -> Char -> a) -> a -> Utf8 -> a
foldl' f z t = unsafePerformIO $ Stream.foldl' f z (toStream t)

-- | A variant of 'foldl' that has no starting value argument. Returns
-- 'Nothing' if applied to an empty 'Utf8'.
--
-- /Time complexity:/ O(n)
{-# INLINE foldl1 #-}
foldl1 :: (Char -> Char -> Char) -> Utf8 -> Char
foldl1 = undefined

-- | A strict version of 'foldl1'.
--
-- /Time complexity:/ O(n)
{-# INLINE foldl1' #-}
foldl1' :: (Char -> Char -> Char) -> Utf8 -> Maybe Char
foldl1' f t = unsafePerformIO $ Stream.foldl1' f (toStream t)

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Utf8',
-- reduces the 'Utf8' using the binary operator, from right to left.
--
-- /Time complexity:/ O(n)
{-# INLINE foldr #-}
foldr :: (Char -> a -> a) -> a -> Utf8 -> a
foldr f z t = unsafePerformIO $ Stream.foldr f z (toStream t)

-- | A variant of 'foldr' that has no starting value argument. Returns
-- 'Nothing' if applied to an empty 'Utf8'.
--
-- /Time complexity:/ O(n)
{-# INLINE foldr1 #-}
foldr1 :: (Char -> Char -> Char) -> Utf8 -> Maybe Char
foldr1 = undefined

--------------------------------------------------------------------------------
-- Special folds
--------------------------------------------------------------------------------

-- | 'any' @p@ @t@ determines whether any character in the
-- 'Utf8' @t@ satisfies the predicate @p@.
--
-- /Time complexity:/ O(n)
{-# INLINE any #-}
any :: (Char -> Bool) -> Utf8 -> Bool
any p t = unsafePerformIO $ Stream.any p (toStream t)

-- | 'all' @p@ @t@ determines whether all characters in the
-- 'Utf8' @t@ satisfy the predicate @p@.
--
-- /Time complexity:/ O(n)
{-# INLINE all #-}
all :: (Char -> Bool) -> Utf8 -> Bool
all p t = unsafePerformIO $ Stream.all p (toStream t)

-- | 'maximum' returns the maximum value from a 'Utf8', or 'Nothing' if
-- empty.
--
-- /Time complexity:/ O(n)
{-# INLINE maximum #-}
maximum :: Utf8 -> Maybe Char
maximum t = unsafePerformIO $ Stream.maximum (toStream t)

-- | 'minimum' returns the minimum value from a 'Utf8', or 'Nothing' if
-- empty.
--
-- /Time complexity:/ O(n)
{-# INLINE minimum #-}
minimum :: Utf8 -> Maybe Char
minimum t = unsafePerformIO $ Stream.minimum (toStream t)

--------------------------------------------------------------------------------
-- Indexing 'Utf8's
--------------------------------------------------------------------------------

-- $index
--
-- If you think of a 'Utf8' value as an array of 'Char' values (which
-- it is not), you run the risk of writing inefficient code.
--
-- An idiom that is common in some languages is to find the numeric
-- offset of a character or substring, then use that number to split
-- or trim the searched string.  With a 'Utf8' value, this approach
-- would require two /O(n)/ operations: one to perform the search, and
-- one to operate from wherever the search ended.
--
-- For example, suppose you have a string that you want to split on
-- the substring @\"::\"@, such as @\"foo::bar::quux\"@. Instead of
-- searching for the index of @\"::\"@ and taking the substrings
-- before and after that index, you would instead use @breakOnAll \"::\"@.

-- | 'Utf8' index (subscript) operator, starting from 0.
--
-- /Time complexity:/ O(n)
{-# INLINE index #-}
index :: Utf8 -> Int -> Maybe Char
index t n = unsafePerformIO $ (Stream.!!) (toStream t) n

-- | The 'findIndex' function takes a predicate and a 'Utf8'
-- and returns the index of the first element in the 'Utf8' satisfying
-- the predicate.
--
-- /Time complexity:/ O(n)
{-# INLINE findIndex #-}
findIndex :: (Char -> Bool) -> Utf8 -> Maybe Int
findIndex p t = unsafePerformIO $ Stream.findIndex p (toStream t)

-- | The 'count' function returns the number of times the
-- query string appears in the given 'Utf8'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- /Time complexity:/ O(n+m)
{-# INLINE_NORMAL count #-}
count :: Utf8 -> Utf8 -> Int
count = undefined

-- | The 'countChar' function returns the number of times the
-- query element appears in the given 'Utf8'.
--
-- /Time complexity:/ O(n)
{-# INLINE countChar #-}
countChar :: Char -> Utf8 -> Int
countChar c t =
    unsafePerformIO $ Stream.length $ Stream.filter (== c) (toStream t)

--------------------------------------------------------------------------------
-- Searching
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Searching with a predicate
--------------------------------------------------------------------------------

-- | The 'elem' function takes a character and a 'Utf8', and
-- returns 'True' if the element is found in the given 'Utf8', or
-- 'False' otherwise.
--
-- /Time complexity:/ O(n)
{-# INLINE elem #-}
elem :: Char -> Utf8 -> Bool
elem c = any (== c)

-- | The 'find' function takes a predicate and a 'Utf8', and
-- returns the first element matching the predicate, or 'Nothing' if
-- there is no such element.
--
-- /Time complexity:/ O(n)
{-# INLINE find #-}
find :: (Char -> Bool) -> Utf8 -> Maybe Char
find p t = unsafePerformIO $ Stream.find p (toStream t)

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

-- | The 'isPrefixOf' function takes two 'Utf8's and returns
-- 'True' iff the first is a prefix of the second.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL _isPrefixOf #-}
_isPrefixOf :: Utf8 -> Utf8 -> Bool
_isPrefixOf a b =
    Array.byteLength (toArray a) <= Array.byteLength (toArray b)
        && unsafePerformIO (Stream.isPrefixOf (toStream a) (toStream b))

-- | The 'isSuffixOf' function takes two 'Utf8's and returns
-- 'True' iff the first is a suffix of the second.
--
-- /Time complexity:/ O(n)
{-# INLINE _isSuffixOf #-}
_isSuffixOf :: Utf8 -> Utf8 -> Bool
_isSuffixOf a b = unsafePerformIO (Stream.isSuffixOf (toStream a) (toStream b))

-- XXX This specific API uses a lot of memory to compile
-- XXX Use domain specific knowledge to implement it efficiently!
-- | The 'isInfixOf' function takes two 'Utf8's and returns
-- 'True' iff the first is contained, wholly and intact, anywhere
-- within the second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- /Time complexity:/ O(n+m)
{-# INLINE_NORMAL _isInfixOf #-}
_isInfixOf :: Utf8 -> Utf8 -> Bool
_isInfixOf a b = unsafePerformIO (Stream.isInfixOf (toStream a) (toStream b))

--------------------------------------------------------------------------------
-- View patterns
--------------------------------------------------------------------------------

-- XXX Change >> to >>> once exposed
-- | Return the suffix of the second string if its prefix
-- matches the entire first string.
--
-- Examples:
--
-- >> stripPrefix "foo" "foobar"
-- Just "bar"
--
-- >> stripPrefix ""    "baz"
-- Just "baz"
--
-- >> stripPrefix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Streamly.Internal.Unicode.Utf8 as Utf8
-- >
-- > fnordLength :: Utf8 -> Int
-- > fnordLength (stripPrefix "fnord" -> Just suf) = Utf8.length suf
-- > fnordLength _                                 = -1
--
-- /Time complexity:/ O(n)
_stripPrefix :: Utf8 -> Utf8 -> Maybe Utf8
_stripPrefix p t =
    fmap fromStream $ unsafePerformIO $ Stream.stripPrefix (toStream p) (toStream t)

-- XXX Change >> to >>> after implementation
-- | Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match.
--
-- If the strings do not have a common prefix or either one is empty,
-- this function returns 'Nothing'.
--
-- Examples:
--
-- >> commonPrefixes "foobar" "fooquux"
-- Just ("foo","bar","quux")
--
-- >> commonPrefixes "veeble" "fetzer"
-- Nothing
--
-- >> commonPrefixes "" "baz"
-- Nothing
--
-- /Time complexity:/ O(n)
commonPrefixes :: Utf8 -> Utf8 -> Maybe (Utf8,Utf8,Utf8)
commonPrefixes = undefined

-- XXX Change >> to >>> once exposed
-- | Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- >> stripSuffix "bar" "foobar"
-- Just "foo"
--
-- >> stripSuffix ""    "baz"
-- Just "baz"
--
-- >> stripSuffix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Streamly.Internal.Unicode.Utf8 as Utf8
-- >
-- > quuxLength :: Utf8 -> Int
-- > quuxLength (stripSuffix "quux" -> Just pre) = Utf8.length pre
-- > quuxLength _                                = -1
--
-- /Time complexity:/ O(n)
_stripSuffix :: Utf8 -> Utf8 -> Maybe Utf8
_stripSuffix p t =
    fmap fromStream
        $ unsafePerformIO $ Stream.stripSuffix (toStream p) (toStream t)
