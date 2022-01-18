-- |
-- Module      : Streamly.Internal.Unicode.Utf8.Reduce
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Utf8.Reduce
    (
    -- * Substrings

    -- ** Breaking strings
      splitAt
    , breakOn
    , breakOnEnd
    , break
    , span
    , group
    , groupBy
    , inits
    , tails

    -- ** Breaking into many substrings
    -- $split
    , splitOn
    , split
    , chunksOf

    -- * Searching
    , breakOnAll
    , partition
    )
where

#include "inline.hs"

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Streamly.Internal.Unicode.Utf8.Type
import Streamly.Internal.Unicode.Utf8.Transform (filter)

import Prelude hiding (break, filter, span, splitAt)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Internal.Unicode.Utf8 as Utf8

--------------------------------------------------------------------------------
-- Substrings
--------------------------------------------------------------------------------

-- | 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
--
-- /Time complexity:/ O(n)
splitAt :: Int -> Utf8 -> (Utf8, Utf8)
splitAt = undefined

-- | 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the list.
--
-- >> Utf8.span (=='0') "000AB"
-- ("000","AB")
--
-- /Time complexity:/ O(n)
{-# INLINE span #-}
span :: (Char -> Bool) -> Utf8 -> (Utf8, Utf8)
span = undefined

-- | 'break' is like 'span', but the prefix returned is
-- over elements that fail the predicate @p@.
--
-- >> Utf8.break (=='c') "180cm"
-- ("180","cm")
--
-- /Time complexity:/ O(n)
{-# INLINE break #-}
break :: (Char -> Bool) -> Utf8 -> (Utf8, Utf8)
break p = span (not . p)

-- | Group characters in a string according to a predicate.
--
-- /Time complexity:/ O(n)
groupBy :: (Char -> Char -> Bool) -> Utf8 -> [Utf8]
groupBy p = unsafePerformIO . Stream.toList . Stream.groupsBy p write . toStream

{-
-- | Returns the /array/ index (in units of 'Word16') at which a
-- character may be found.  This is /not/ the same as the logical
-- index returned by e.g. 'findIndex'.
findAIndexOrEnd :: (Char -> Bool) -> Utf8 -> Int
findAIndexOrEnd q t@(Utf8 _arr _off len) = go 0
    where go !i | i >= len || q c       = i
                | otherwise             = go (i+d)
                where Iter c d          = iter t i
-}

-- | Group characters in a string by equality.
--
-- /Time complexity:/ O(n)
group :: Utf8 -> [Utf8]
group = groupBy (==)

-- | Return all initial segments of the given 'Utf8', shortest
-- first.
--
-- /Time complexity:/ O(n)
inits :: Utf8 -> [Utf8]
inits = undefined

-- | Return all final segments of the given 'Utf8', longest
-- first.
--
-- /Time complexity:/ O(n)
tails :: Utf8 -> [Utf8]
tails = undefined

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Utf8's that
-- are slices of the original.

-- | Break a 'Utf8' into pieces separated by the first 'Utf8'
-- argument (which cannot be empty), consuming the delimiter. An empty
-- delimiter is invalid, and will cause an error to be raised.
--
-- Examples:
--
-- >> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >> splitOn "x"    "x"
-- ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
--
-- (Note: the string @s@ to split on above cannot be empty.)
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- /Time complexity:/ O(m+n)
{-# INLINE_NORMAL splitOn #-}
splitOn :: Utf8
        -- ^ String to split on. If this string is empty, an error
        -- will occur.
        -> Utf8
        -- ^ Input text.
        -> [Utf8]
splitOn = undefined
{-
splitOn pat src =
    unsafePerformIO
        $ Stream.toList $ Stream.splitOnSeq (toArray pat) write (stream src)
-}

-- | Splits a 'Utf8' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- >>> split (=='a') "aabbaca"
-- ["","","bb","c",""]
--
-- >>> split (=='a') ""
-- [""]
--
-- /Time complexity:/ O(n)
{-# INLINE split #-}
split :: (Char -> Bool) -> Utf8 -> [Utf8]
split p t =
    unsafePerformIO $ Stream.toList $ Stream.splitOn p write (toStream t)

-- | Splits a 'Utf8' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- >>> chunksOf 3 "foobarbaz"
-- ["foo","bar","baz"]
--
-- >>> chunksOf 4 "haskell.org"
-- ["hask","ell.","org"]
--
-- /Time complexity:/ O(n)
{-# INLINE chunksOf #-}
chunksOf :: Int -> Utf8 -> [Utf8]
chunksOf k t =
    unsafePerformIO $ Stream.toList $ Stream.chunksOf k write (toStream t)

-- | Find the first instance of @needle@ (which must be
-- non-'null') in @haystack@.  The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- >> breakOn "::" "a::b::c"
-- ("a","::b::c")
--
-- >> breakOn "/" "foobar"
-- ("foobar","")
--
-- Laws:
--
-- > append prefix match == haystack
-- >   where (prefix, match) = breakOn needle haystack
--
-- If you need to break a string by a substring repeatedly (e.g. you
-- want to break on every instance of a substring), use 'breakOnAll'
-- instead, as it has lower startup overhead.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- /Time complexity:/ O(n+m)
{-# INLINE breakOn #-}
breakOn :: Utf8 -> Utf8 -> (Utf8, Utf8)
breakOn = undefined

-- XXX Change >> to >>> after implementation
-- | Similar to 'breakOn', but searches from the end of the
-- string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- >> breakOnEnd "::" "a::b::c"
-- ("a::b::","c")
--
-- /Time complexity:/ O(n+m)
{-# INLINE breakOnEnd #-}
breakOnEnd :: Utf8 -> Utf8 -> (Utf8, Utf8)
breakOnEnd = undefined

--------------------------------------------------------------------------------
-- Searching
--------------------------------------------------------------------------------

-- | The 'partition' function takes a predicate and a 'Utf8',
-- and returns the pair of 'Utf8's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
--
-- /Time complexity:/ O(n)
{-# INLINE partition #-}
partition :: (Char -> Bool) -> Utf8 -> (Utf8, Utf8)
partition p t = (filter p t, filter (not . p) t)

-- XXX Change >> to >>> after implementation
-- | Find all non-overlapping instances of @needle@ in
-- @haystack@.  Each element of the returned list consists of a pair:
--
-- * The entire string prior to the /k/th match (i.e. the prefix)
--
-- * The /k/th match, followed by the remainder of the string
--
-- Examples:
--
-- >> breakOnAll "::" ""
-- []
--
-- >> breakOnAll "/" "a/b/c/"
-- [("a","/b/c/"),("a/b","/c/"),("a/b/c","/")]
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- The @needle@ parameter may not be empty.
--
-- /Time complexity:/ O(n+m)
{-# INLINE breakOnAll #-}
breakOnAll :: Utf8              -- ^ @needle@ to search for
           -> Utf8              -- ^ @haystack@ in which to search
           -> [(Utf8, Utf8)]
breakOnAll = undefined
