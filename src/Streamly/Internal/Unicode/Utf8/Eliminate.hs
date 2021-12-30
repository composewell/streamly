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
    , isPrefixOf
    , isSuffixOf
    -- XXX isInfixOf takes too much memory to compile
    -- , isInfixOf

    -- ** View patterns
    , stripPrefix
    , stripSuffix
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
-- Reducing Streams (folds)
--------------------------------------------------------------------------------

{-# INLINE fold #-}
fold :: MonadIO m => Fold m Char b -> Utf8 -> m b
fold f = Stream.fold f . hoist liftIO . stream

{-# INLINE foldl #-}
-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Utf8',
-- reduces the 'Utf8' using the binary operator, from left to right.
--
foldl :: (a -> Char -> a) -> a -> Utf8 -> a
foldl = undefined

{-# INLINE foldl' #-}
-- | /O(n)/ A strict version of 'foldl'.
foldl' :: (a -> Char -> a) -> a -> Utf8 -> a
foldl' f z t = unsafePerformIO $ Stream.foldl' f z (stream t)

{-# INLINE foldl1 #-}
-- | /O(n)/ A variant of 'foldl' that has no starting value argument. Returns
-- 'Nothing' if applied to an empty 'Utf8'.
foldl1 :: (Char -> Char -> Char) -> Utf8 -> Char
foldl1 = undefined

{-# INLINE foldl1' #-}
-- | /O(n)/ A strict version of 'foldl1'.
foldl1' :: (Char -> Char -> Char) -> Utf8 -> Maybe Char
foldl1' f t = unsafePerformIO $ Stream.foldl1' f (stream t)

{-# INLINE foldr #-}
-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Utf8',
-- reduces the 'Utf8' using the binary operator, from right to left.
--
foldr :: (Char -> a -> a) -> a -> Utf8 -> a
foldr f z t = unsafePerformIO $ Stream.foldr f z (stream t)

{-# INLINE foldr1 #-}
-- | /O(n)/ A variant of 'foldr' that has no starting value argument. Returns
-- 'Nothing' if applied to an empty 'Utf8'.
foldr1 :: (Char -> Char -> Char) -> Utf8 -> Maybe Char
foldr1 = undefined

--------------------------------------------------------------------------------
-- Special folds
--------------------------------------------------------------------------------

{-# INLINE any #-}
-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'Utf8' @t@ satisfies the predicate @p@.
any :: (Char -> Bool) -> Utf8 -> Bool
any p t = unsafePerformIO $ Stream.any p (stream t)

{-# INLINE all #-}
-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'Utf8' @t@ satisfy the predicate @p@.
all :: (Char -> Bool) -> Utf8 -> Bool
all p t = unsafePerformIO $ Stream.all p (stream t)

{-# INLINE maximum #-}
-- | /O(n)/ 'maximum' returns the maximum value from a 'Utf8', or 'Nothing' if
-- empty.
maximum :: Utf8 -> Maybe Char
maximum t = unsafePerformIO $ Stream.maximum (stream t)

{-# INLINE minimum #-}
-- | /O(n)/ 'minimum' returns the minimum value from a 'Utf8', or 'Nothing' if
-- empty.
minimum :: Utf8 -> Maybe Char
minimum t = unsafePerformIO $ Stream.minimum (stream t)

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

-- | /O(n)/ 'Utf8' index (subscript) operator, starting from 0.
{-# INLINE index #-}
index :: Utf8 -> Int -> Maybe Char
index t n = unsafePerformIO $ (Stream.!!) (stream t) n

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'Utf8'
-- and returns the index of the first element in the 'Utf8' satisfying
-- the predicate.
{-# INLINE findIndex #-}
findIndex :: (Char -> Bool) -> Utf8 -> Maybe Int
findIndex p t = unsafePerformIO $ Stream.findIndex p (stream t)

-- | /O(n+m)/ The 'count' function returns the number of times the
-- query string appears in the given 'Utf8'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
{-# INLINE_NORMAL count #-}
count :: Utf8 -> Utf8 -> Int
count = undefined

-- | /O(n)/ The 'countChar' function returns the number of times the
-- query element appears in the given 'Utf8'.
{-# INLINE countChar #-}
countChar :: Char -> Utf8 -> Int
countChar c t =
    unsafePerformIO $ Stream.length $ Stream.filter (== c) (stream t)


--------------------------------------------------------------------------------
-- Searching
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Searching with a predicate
--------------------------------------------------------------------------------

-- | /O(n)/ The 'elem' function takes a character and a 'Utf8', and
-- returns 'True' if the element is found in the given 'Utf8', or
-- 'False' otherwise.
{-# INLINE elem #-}
elem :: Char -> Utf8 -> Bool
elem c = any (== c)

-- | /O(n)/ The 'find' function takes a predicate and a 'Utf8', and
-- returns the first element matching the predicate, or 'Nothing' if
-- there is no such element.
{-# INLINE find #-}
find :: (Char -> Bool) -> Utf8 -> Maybe Char
find p t = unsafePerformIO $ Stream.find p (stream t)

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

-- | /O(n)/ The 'isPrefixOf' function takes two 'Utf8's and returns
-- 'True' iff the first is a prefix of the second.
{-# INLINE_NORMAL isPrefixOf #-}
isPrefixOf :: Utf8 -> Utf8 -> Bool
isPrefixOf a b =
    Array.byteLength (toArray a) <= Array.byteLength (toArray b)
        && unsafePerformIO (Stream.isPrefixOf (stream a) (stream b))

-- | /O(n)/ The 'isSuffixOf' function takes two 'Utf8's and returns
-- 'True' iff the first is a suffix of the second.
{-# INLINE isSuffixOf #-}
isSuffixOf :: Utf8 -> Utf8 -> Bool
isSuffixOf a b = unsafePerformIO (Stream.isSuffixOf (stream a) (stream b))

-- XXX This specific API uses a lot of memory to compile
-- XXX Use domain specific knowledge to implement it efficiently!
-- | /O(n+m)/ The 'isInfixOf' function takes two 'Utf8's and returns
-- 'True' iff the first is contained, wholly and intact, anywhere
-- within the second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
{-# INLINE_NORMAL _isInfixOf #-}
_isInfixOf :: Utf8 -> Utf8 -> Bool
_isInfixOf a b = unsafePerformIO (Stream.isInfixOf (stream a) (stream b))

--------------------------------------------------------------------------------
-- View patterns
--------------------------------------------------------------------------------

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripPrefix "foo" "foobar"
-- Just "bar"
--
-- >>> stripPrefix ""    "baz"
-- Just "baz"
--
-- >>> stripPrefix "foo" "quux"
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
stripPrefix :: Utf8 -> Utf8 -> Maybe Utf8
stripPrefix p t =
    fmap unstream $ unsafePerformIO $ Stream.stripPrefix (stream p) (stream t)

-- XXX Change >> to >>> after implementation
-- | /O(n)/ Find the longest non-empty common prefix of two strings
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
commonPrefixes :: Utf8 -> Utf8 -> Maybe (Utf8,Utf8,Utf8)
commonPrefixes = undefined

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripSuffix "bar" "foobar"
-- Just "foo"
--
-- >>> stripSuffix ""    "baz"
-- Just "baz"
--
-- >>> stripSuffix "foo" "quux"
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
stripSuffix :: Utf8 -> Utf8 -> Maybe Utf8
stripSuffix p t =
    fmap unstream $ unsafePerformIO $ Stream.stripSuffix (stream p) (stream t)
