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
    -- * Type
      Utf8
    , toArray

    -- * Creation and elimination
    , empty
    , singleton
    , stream
    , unstream
    , pack
    , unpack

    -- * Basic interface
    , cons
    , snoc
    , append
    , uncons
    , unsnoc
    , head
    , last
    , tail
    , init
    , null

    , isSingleton
    , length
    , compareLength

    -- * Transformations
    , map
    , intercalate
    , intersperse
    , transpose
    , reverse
    , replace

    -- ** Case conversion
    -- $case
    , toCaseFold
    , toLower
    , toTitle
    , toUpper

    -- ** Justification
    , justifyLeft
    , justifyRight
    , center

    -- * Folds
    , fold
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1

    -- ** Special folds
    , concat
    , concatMap
    , any
    , all
    , maximum
    , minimum

    -- * Construction

    -- ** Scans
    , scanl
    , scanl1
    , scanl'
    , scanl1'
    , scanr
    , scanr1

    -- ** Accumulating maps
    , mapAccumL
    , mapAccumR

    -- ** Generation and unfolding
    , replicateChar
    , replicate
    , unfoldr
    , unfoldrN

    -- * Substrings

    -- ** Breaking strings
    , take
    , takeEnd
    , drop
    , dropEnd
    , takeWhile
    , takeWhileEnd
    , dropWhile
    , dropWhileEnd
    , dropAround
    , strip
    , stripStart
    , stripEnd
    , splitAt
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

    -- ** Breaking into lines and words
    , lines
    --, lines'
    , words
    , unlines
    , unwords

    -- * Predicates
    , isPrefixOf
    , isSuffixOf
    , isInfixOf

    -- ** View patterns
    , stripPrefix
    , stripSuffix
    , commonPrefixes

    -- * Searching
    , filter
    , breakOnAll
    , find
    , elem
    , partition

    -- , findSubstring

    -- * Indexing
    -- $index
    , index
    , findIndex
    , countChar
    , count

    -- * Zipping
    , zip
    , zipWith

    -- * Folds
    , write

    -- * Unfolds
    , read

    -- -* Ordered
    -- , sort

    -- -- * Low level operations
    -- , copy
    -- , unpackCString#

    -- , module Streamly.Internal.Unicode.Utf8.Transform
    -- , module Streamly.Internal.Unicode.Utf8.Transform.Type
    )
where

#include "inline.hs"

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isSpace)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream.IsStream.Lift (hoist)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.List as List
import qualified Prelude
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Streamly.Internal.Unicode.Utf8.Type
import Streamly.Internal.Unicode.Utf8.Transform

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
-- Building 'Utf8's
--------------------------------------------------------------------------------

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Utf8', passing an accumulating
-- parameter from left to right, and returns a final 'Utf8'.  Performs
-- replacement on invalid scalar values.
{-# INLINE mapAccumL #-}
mapAccumL :: (a -> Char -> (a,Char)) -> a -> Utf8 -> (a, Utf8)
mapAccumL = undefined

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'Utf8', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'Utf8'.
-- Performs replacement on invalid scalar values.
{-# INLINE mapAccumR #-}
mapAccumR :: (a -> Char -> (a,Char)) -> a -> Utf8 -> (a, Utf8)
mapAccumR = undefined

--------------------------------------------------------------------------------
-- Generating and unfolding 'Utf8's
--------------------------------------------------------------------------------

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'Utf8' consisting of the input
-- @t@ repeated @n@ times.
{-# INLINE replicate #-}
replicate :: Int -> Utf8 -> Utf8
replicate = undefined

-- | /O(n)/ 'replicateChar' @n@ @c@ is a 'Utf8' of length @n@ with @c@ the
-- value of every element.
{-# INLINE replicateChar #-}
replicateChar :: Int -> Char -> Utf8
replicateChar n c = unstream (Stream.replicate n c)

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Utf8' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Utf8', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production.
-- Performs replacement on invalid scalar values.
{-# INLINE unfoldr #-}
unfoldr :: (a -> Maybe (Char, a)) -> a -> Utf8
unfoldr f s = unstream (Stream.unfoldr f s)

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Utf8' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
-- Performs replacement on invalid scalar values.
{-# INLINE unfoldrN #-}
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Utf8
unfoldrN = undefined

--------------------------------------------------------------------------------
-- Substrings
--------------------------------------------------------------------------------

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: Int -> Utf8 -> (Utf8, Utf8)
splitAt = undefined

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the list.
--
-- >>> Utf8.span (=='0') "000AB"
-- ("000","AB")
{-# INLINE span #-}
span :: (Char -> Bool) -> Utf8 -> (Utf8, Utf8)
span = undefined

-- | /O(n)/ 'break' is like 'span', but the prefix returned is
-- over elements that fail the predicate @p@.
--
-- >>> Utf8.break (=='c') "180cm"
-- ("180","cm")
{-# INLINE break #-}
break :: (Char -> Bool) -> Utf8 -> (Utf8, Utf8)
break p = span (not . p)

-- | /O(n)/ Group characters in a string according to a predicate.
groupBy :: (Char -> Char -> Bool) -> Utf8 -> [Utf8]
groupBy p = unsafePerformIO . Stream.toList . Stream.groupsBy p write . stream

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

-- | /O(n)/ Group characters in a string by equality.
group :: Utf8 -> [Utf8]
group = groupBy (==)

-- | /O(n)/ Return all initial segments of the given 'Utf8', shortest
-- first.
inits :: Utf8 -> [Utf8]
inits = undefined

-- | /O(n)/ Return all final segments of the given 'Utf8', longest
-- first.
tails :: Utf8 -> [Utf8]
tails = undefined

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Utf8's that
-- are slices of the original.

-- | /O(m+n)/ Break a 'Utf8' into pieces separated by the first 'Utf8'
-- argument (which cannot be empty), consuming the delimiter. An empty
-- delimiter is invalid, and will cause an error to be raised.
--
-- Examples:
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >>> splitOn "x"    "x"
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

-- | /O(n)/ Splits a 'Utf8' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- >>> split (=='a') "aabbaca"
-- ["","","bb","c",""]
--
-- >>> split (=='a') ""
-- [""]
{-# INLINE split #-}
split :: (Char -> Bool) -> Utf8 -> [Utf8]
split p t =
    unsafePerformIO $ Stream.toList $ Stream.splitOn p write (stream t)

-- | /O(n)/ Splits a 'Utf8' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- >>> chunksOf 3 "foobarbaz"
-- ["foo","bar","baz"]
--
-- >>> chunksOf 4 "haskell.org"
-- ["hask","ell.","org"]
{-# INLINE chunksOf #-}
chunksOf :: Int -> Utf8 -> [Utf8]
chunksOf k t =
    unsafePerformIO $ Stream.toList $ Stream.chunksOf k write (stream t)

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

-- | /O(n)/ The 'partition' function takes a predicate and a 'Utf8',
-- and returns the pair of 'Utf8's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
{-# INLINE partition #-}
partition :: (Char -> Bool) -> Utf8 -> (Utf8, Utf8)
partition p t = (filter p t, filter (not . p) t)

-- | /O(n+m)/ Find the first instance of @needle@ (which must be
-- non-'null') in @haystack@.  The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- >>> breakOn "::" "a::b::c"
-- ("a","::b::c")
--
-- >>> breakOn "/" "foobar"
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
{-# INLINE breakOn #-}
breakOn :: Utf8 -> Utf8 -> (Utf8, Utf8)
breakOn = undefined

-- | /O(n+m)/ Similar to 'breakOn', but searches from the end of the
-- string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- >>> breakOnEnd "::" "a::b::c"
-- ("a::b::","c")
{-# INLINE breakOnEnd #-}
breakOnEnd :: Utf8 -> Utf8 -> (Utf8, Utf8)
breakOnEnd = undefined

-- | /O(n+m)/ Find all non-overlapping instances of @needle@ in
-- @haystack@.  Each element of the returned list consists of a pair:
--
-- * The entire string prior to the /k/th match (i.e. the prefix)
--
-- * The /k/th match, followed by the remainder of the string
--
-- Examples:
--
-- >>> breakOnAll "::" ""
-- []
--
-- >>> breakOnAll "/" "a/b/c/"
-- [("a","/b/c/"),("a/b","/c/"),("a/b/c","/")]
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- The @needle@ parameter may not be empty.
{-# INLINE breakOnAll #-}
breakOnAll :: Utf8              -- ^ @needle@ to search for
           -> Utf8              -- ^ @haystack@ in which to search
           -> [(Utf8, Utf8)]
breakOnAll = undefined

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
