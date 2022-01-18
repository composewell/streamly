-- |
-- Module      : Streamly.Internal.Unicode.Utf8.Transform
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Utf8.Transform
    (
    -- * Transformations
      map
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

    -- * Construction

    -- ** Scans
    , scanl
    , scanl1
    , scanl'
    , scanl1'
    , scanr
    , scanr1

    -- * Searching
    , filter

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
    )
where

#include "inline.hs"

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Char (isSpace)

import qualified Data.List as List
import qualified Prelude
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Streamly.Internal.Unicode.Utf8.Type

import Prelude hiding
    ( drop
    , dropWhile
    , filter
    , map
    , reverse
    , scanl
    , scanl1
    , scanr
    , scanr1
    , take
    , takeWhile
    )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Internal.Unicode.Utf8 as Utf8

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- | 'map' @f@ @t@ is the 'Utf8' obtained by applying @f@ to
-- each element of @t@.
--
-- Example:
--
-- >>> let message = Utf8.pack "I am not angry. Not at all."
-- >>> Utf8.map (\c -> if c == '.' then '!' else c) message
-- "I am not angry! Not at all!"
--
-- Performs replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
{-# INLINE map #-}
map :: (Char -> Char) -> Utf8 -> Utf8
map f = fromStream . Stream.map f . toStream

-- XXX Change >> to >>> after implementation
-- | The 'intercalate' function takes a 'Utf8' and a list of
-- 'Utf8's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Example:
--
-- >> Utf8.intercalate "NI!" ["We", "seek", "the", "Holy", "Grail"]
-- "WeNI!seekNI!theNI!HolyNI!Grail"
--
-- /Time complexity:/ O(n)
{-# INLINE intercalate #-}
intercalate :: Utf8 -> [Utf8] -> Utf8
intercalate = undefined

-- XXX Change >> to >>> after implementation
-- | The 'intersperse' function takes a character and places it
-- between the characters of a 'Utf8'.
--
-- Example:
--
-- >> Utf8.intersperse '.' "SHIELD"
-- "S.H.I.E.L.D"
--
-- Performs replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
{-# INLINE intersperse #-}
intersperse :: Char -> Utf8 -> Utf8
intersperse = undefined

-- XXX Change >> to >>> after implementation
-- | Reverse the characters of a string.
--
-- Example:
--
-- >> Utf8.reverse "desrever"
-- "reversed"
--
--
-- /Time complexity:/ O(n)
{-# INLINE reverse #-}
reverse :: Utf8 -> Utf8
reverse = fromStream . Stream.reverse . toStream

-- XXX Change >> to >>> after implementation
-- | Replace every non-overlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
--
-- This function behaves as though it was defined as follows:
--
-- @
-- replace needle replacement haystack =
--   'intercalate' replacement ('splitOn' needle haystack)
-- @
--
-- As this suggests, each occurrence is replaced exactly once.  So if
-- @needle@ occurs in @replacement@, that occurrence will /not/ itself
-- be replaced recursively:
--
-- >> replace "oo" "foo" "oo"
-- "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- >> replace "ofo" "bar" "ofofo"
-- "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- /Time complexity:/ O(m+n)
{-# INLINE replace #-}
replace :: Utf8
        -- ^ @needle@ to search for.  If this string is empty, an
        -- error will occur.
        -> Utf8
        -- ^ @replacement@ to replace @needle@ with.
        -> Utf8
        -- ^ @haystack@ in which to search.
        -> Utf8
replace = undefined

--------------------------------------------------------------------------------
-- Case conversions (folds)
--------------------------------------------------------------------------------

-- | Convert a string to folded case.
--
-- This function is mainly useful for performing caseless (also known
-- as case insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature \"&#xfb13;\" (men now, U+FB13) is case
-- folded to the sequence \"&#x574;\" (men, U+0574) followed by
-- \"&#x576;\" (now, U+0576), while the Greek \"&#xb5;\" (micro sign,
-- U+00B5) is case folded to \"&#x3bc;\" (small letter mu, U+03BC)
-- instead of itself.
--
-- /Time complexity:/ O(n)
{-# INLINE toCaseFold #-}
toCaseFold :: Utf8 -> Utf8
toCaseFold = undefined

-- | Convert a string to lower case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, \"&#x130;\" (Latin capital letter I with dot above,
-- U+0130) maps to the sequence \"i\" (Latin small letter i, U+0069)
-- followed by \" &#x307;\" (combining dot above, U+0307).
--
-- /Time complexity:/ O(n)
{-# INLINE toLower #-}
toLower :: Utf8 -> Utf8
toLower = undefined

-- | Convert a string to upper case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the German \"&#xdf;\" (eszett, U+00DF) maps to the
-- two-letter sequence \"SS\".
--
-- /Time complexity:/ O(n)
{-# INLINE toUpper #-}
toUpper :: Utf8 -> Utf8
toUpper = undefined

-- | Convert a 'Utf8' to title case, using simple case
-- conversion.
--
-- The first letter of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
--
-- /Time complexity:/ O(n)
{-# INLINE toTitle #-}
toTitle :: Utf8 -> Utf8
toTitle = undefined

-- XXX Change >> to >>> after implementation
-- | Left-justify a string to the given length, using the
-- specified fill character on the right.
-- Performs replacement on invalid scalar values.
--
-- Examples:
--
-- >> justifyLeft 7 'x' "foo"
-- "fooxxxx"
--
-- >> justifyLeft 3 'x' "foobar"
-- "foobar"
--
-- /Time complexity:/ O(n)
{-# INLINE justifyLeft #-}
justifyLeft :: Int -> Char -> Utf8 -> Utf8
justifyLeft = undefined

-- | Right-justify a string to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- >> justifyRight 7 'x' "bar"
-- "xxxxbar"
--
-- >> justifyRight 3 'x' "foobar"
-- "foobar"
--
-- /Time complexity:/ O(n)
{-# INLINE justifyRight #-}
justifyRight :: Int -> Char -> Utf8 -> Utf8
justifyRight = undefined

-- XXX Change >> to >>> after implementation
-- | Center a string to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- >> center 8 'x' "HS"
-- "xxxHSxxx"
--
-- /Time complexity:/ O(n)
{-# INLINE center #-}
center :: Int -> Char -> Utf8 -> Utf8
center = undefined

-- | The 'transpose' function transposes the rows and columns
-- of its 'Utf8' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
--
-- Examples:
--
-- >>> transpose ["green","orange"]
-- ["go","rr","ea","en","ng","e"]
--
-- >>> transpose ["blue","red"]
-- ["br","le","ud","e"]
--
-- /Time complexity:/ O(n)
{-# INLINE transpose #-}
transpose :: [Utf8] -> [Utf8]
transpose = Prelude.map pack . List.transpose . Prelude.map unpack

--------------------------------------------------------------------------------
-- Building 'Utf8's
--------------------------------------------------------------------------------

-- | 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
--
-- /Time complexity:/ O(n)
{-# INLINE scanl #-}
scanl :: (Char -> Char -> Char) -> Char -> Utf8 -> Utf8
scanl f z t = fromStream (Stream.scanl' f z (toStream t))

-- | 'scanl1' is a variant of 'scanl' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
-- /Time complexity:/ O(n)
{-# INLINE scanl1 #-}
scanl1 :: (Char -> Char -> Char) -> Utf8 -> Utf8
scanl1 f t = fromStream (Stream.scanl1' f (toStream t))

-- | 'scanl'' is similar to 'foldl'', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl' f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl' f z xs) == foldl f z xs.
--
-- /Time complexity:/ O(n)
{-# INLINE scanl' #-}
scanl' :: (Char -> Char -> Char) -> Char -> Utf8 -> Utf8
scanl' f z t = fromStream (Stream.scanl' f z (toStream t))

-- | 'scanl1'' is a variant of 'scanl'' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1' f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
-- /Time complexity:/ O(n)
{-# INLINE scanl1' #-}
scanl1' :: (Char -> Char -> Char) -> Utf8 -> Utf8
scanl1' f t = fromStream (Stream.scanl1' f (toStream t))

-- | 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
--
-- /Time complexity:/ O(n)
{-# INLINE scanr #-}
scanr :: (Char -> Char -> Char) -> Char -> Utf8 -> Utf8
scanr = undefined

-- | 'scanr1' is a variant of 'scanr' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
{-# INLINE scanr1 #-}
scanr1 :: (Char -> Char -> Char) -> Utf8 -> Utf8
scanr1 = undefined

--------------------------------------------------------------------------------
-- Searching with a predicate
--------------------------------------------------------------------------------

-- | 'filter', applied to a predicate and a 'Utf8',
-- returns a 'Utf8' containing those characters that satisfy the
-- predicate.
--
-- /Time complexity:/ O(n)
{-# INLINE filter #-}
filter :: (Char -> Bool) -> Utf8 -> Utf8
filter p t = fromStream (Stream.filter p (toStream t))

--------------------------------------------------------------------------------
-- Substrings
--------------------------------------------------------------------------------

-- | 'take' @n@, applied to a 'Utf8', returns the prefix of the
-- 'Utf8' of length @n@, or the 'Utf8' itself if @n@ is greater than
-- the length of the Utf8.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL take #-}
take :: Int -> Utf8 -> Utf8
take n t = fromStream (Stream.take n (toStream t))

-- | 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- >> takeEnd 3 "foobar"
-- "bar"
--
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL takeEnd #-}
takeEnd :: Int -> Utf8 -> Utf8
takeEnd = undefined

-- | 'drop' @n@, applied to a 'Utf8', returns the suffix of the
-- 'Utf8' after the first @n@ characters, or the empty 'Utf8' if @n@
-- is greater than the length of the 'Utf8'.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL drop #-}
drop :: Int -> Utf8 -> Utf8
drop n t = fromStream (Stream.drop n (toStream t))

-- | 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@.
--
-- Examples:
--
-- >> dropEnd 3 "foobar"
-- "foo"
--
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL dropEnd #-}
dropEnd :: Int -> Utf8 -> Utf8
dropEnd = undefined

-- | 'takeWhile', applied to a predicate @p@ and a 'Utf8',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL takeWhile #-}
takeWhile :: (Char -> Bool) -> Utf8 -> Utf8
takeWhile p t = fromStream (Stream.takeWhile p (toStream t))

-- | 'takeWhileEnd', applied to a predicate @p@ and a 'Utf8',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- >> takeWhileEnd (=='o') "foo"
-- "oo"
--
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL takeWhileEnd #-}
takeWhileEnd :: (Char -> Bool) -> Utf8 -> Utf8
takeWhileEnd = undefined

-- | 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL dropWhile #-}
dropWhile :: (Char -> Bool) -> Utf8 -> Utf8
dropWhile p t = fromStream (Stream.dropWhile p (toStream t))

-- | 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- >> dropWhileEnd (=='.') "foo..."
-- "foo"
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL dropWhileEnd #-}
dropWhileEnd :: (Char -> Bool) -> Utf8 -> Utf8
dropWhileEnd = undefined

-- | 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL dropAround #-}
dropAround :: (Char -> Bool) -> Utf8 -> Utf8
dropAround p = dropWhile p . dropWhileEnd p

-- | Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
--
-- /Time complexity:/ O(n)
{-# INLINE stripStart #-}
stripStart :: Utf8 -> Utf8
stripStart = dropWhile isSpace

-- | Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL stripEnd #-}
stripEnd :: Utf8 -> Utf8
stripEnd = dropWhileEnd isSpace

-- | Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL strip #-}
strip :: Utf8 -> Utf8
strip = dropAround isSpace
