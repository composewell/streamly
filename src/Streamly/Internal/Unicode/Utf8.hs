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
    )
where

#include "inline.hs"

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Internal.Unicode.Utf8 as Utf8

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isSpace)
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Foreign.Type (Array)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream.IsStream (SerialT)
import Streamly.Internal.Data.Stream.IsStream.Lift (hoist)
import Streamly.Internal.Data.Unfold (Unfold)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.List as List
import qualified Prelude as Prelude
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Unicode.Stream as Unicode

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

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

-- | A space efficient, packed, unboxed Unicode container.
newtype Utf8 =
    Utf8 (Array Word8)
    deriving (NFData)

empty :: Utf8
empty = Utf8 Array.nil

singleton :: Char -> Utf8
singleton x = pack [x]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINE stream #-}
stream :: Utf8 -> SerialT IO Char
stream = Unicode.decodeUtf8' . Array.toStream . toArray

{-# INLINE unstream #-}
unstream :: SerialT IO Char -> Utf8
unstream =
    Utf8 . unsafePerformIO . Stream.fold Array.write . Unicode.encodeUtf8'

{-# INLINE second #-}
second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

{-
-- | Apply a function to the first element of an optional pair.
{-# INLINE firstf #-}
firstf :: (a -> c) -> Maybe (a,b) -> Maybe (c,b)
firstf f (Just (a, b)) = Just (f a, b)
firstf _  Nothing      = Nothing
-}

--------------------------------------------------------------------------------
-- Conversion to/from 'Utf8'
--------------------------------------------------------------------------------

{-# INLINE toArray #-}
toArray :: Utf8 -> Array Word8
toArray (Utf8 arr) = arr

-- | /O(n)/ Convert a 'String' into a 'Utf8'. Performs
-- replacement on invalid scalar values.
{-# INLINE_NORMAL pack #-}
pack :: String -> Utf8
pack = unstream . Stream.fromList

-- | /O(n)/ Convert a 'Utf8' into a 'String'.
{-# INLINE_NORMAL unpack #-}
unpack :: Utf8 -> String
unpack = unsafePerformIO . Stream.toList . stream

--------------------------------------------------------------------------------
-- Streamly style APIs
--------------------------------------------------------------------------------

{-# INLINE write #-}
write :: Fold m Char Utf8
write = undefined

{-# INLINE read #-}
read :: Unfold m Utf8 Char
read = undefined

--------------------------------------------------------------------------------
-- Basic functions
--------------------------------------------------------------------------------

-- | /O(n)/ Adds a character to the front of a 'Utf8'. This function is more
-- costly than its 'List' counterpart because it requires copying a new array.
-- Performs replacement on invalid scalar values.
{-# INLINE cons #-}
cons :: Char -> Utf8 -> Utf8
cons c = unstream . Stream.cons c . stream

-- | /O(n)/ Adds a character to the end of a 'Utf8'.  This copies the entire
-- array in the process, unless fused.   Performs replacement
-- on invalid scalar values.
{-# INLINE snoc #-}
snoc :: Utf8 -> Char -> Utf8
snoc = undefined

-- | /O(n)/ Appends one 'Utf8' to the other by copying both of them into a new
-- 'Utf8'.
{-# NOINLINE append #-}
append :: Utf8 -> Utf8 -> Utf8
append (Utf8 a) (Utf8 b) = Utf8 $ unsafePerformIO $ Array.splice a b

-- | /O(1)/ Returns the first character of a 'Utf8', or 'Nothing' if empty.
--
{-# INLINE head #-}
head :: Utf8 -> Maybe Char
head = unsafePerformIO . Stream.head . stream

-- | /O(1)/ Returns the first character and rest of a 'Utf8', or 'Nothing' if
-- empty.
{-# INLINE_NORMAL uncons #-}
uncons :: Utf8 -> Maybe (Char, Utf8)
uncons = fmap (second unstream) . unsafePerformIO . Stream.uncons . stream

-- | /O(1)/ Returns the last character of a 'Utf8', or 'Nothing' if empty.
--
{-# INLINE_NORMAL last #-}
last :: Utf8 -> Char
last = undefined

-- | /O(1)/ Returns all characters after the head of a 'Utf8', or 'Nothing' if
-- empty.
{-# INLINE_NORMAL tail #-}
tail :: Utf8 -> Maybe Utf8
tail = fmap unstream . unsafePerformIO . Stream.tail . stream

-- | /O(1)/ Returns all but the last character of a 'Utf8', or 'Nothing' if
-- empty.
{-# INLINE_NORMAL init #-}
init :: Utf8 -> Maybe Utf8
init = fmap unstream . unsafePerformIO . Stream.init . stream

-- | /O(1)/ Returns all but the last character and the last character of a
-- 'Utf8', or 'Nothing' if empty.
{-# INLINE unsnoc #-}
unsnoc :: Utf8 -> Maybe (Utf8, Char)
unsnoc = undefined

-- | /O(1)/ Tests whether a 'Utf8' is empty or not.
{-# INLINE null #-}
null :: Utf8 -> Bool
null = Array.null . toArray

-- | /O(1)/ Tests whether a 'Utf8' contains exactly one character.
{-# INLINE isSingleton #-}
isSingleton :: Utf8 -> Bool
isSingleton = undefined

-- | /O(n)/ Returns the number of characters in a 'Utf8'.
{-# INLINE length #-}
length :: Utf8 -> Int
length = unsafePerformIO . Stream.length . stream


-- | /O(n)/ Compare the count of characters in a 'Utf8' to a number.
--
-- This function gives the same answer as comparing against the result of
-- 'length', but can short circuit if the count of characters is greater than
-- the number, and hence be more efficient.
{-# INLINE_NORMAL compareLength #-}
compareLength :: Utf8 -> Int -> Ordering
compareLength = undefined

--------------------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------------------

-- | /O(n)/ 'map' @f@ @t@ is the 'Utf8' obtained by applying @f@ to
-- each element of @t@.
--
-- Example:
--
-- >>> let message = pack "I am not angry. Not at all."
-- >>> Utf8.map (\c -> if c == '.' then '!' else c) message
-- "I am not angry! Not at all!"
--
--  Performs replacement on invalid scalar values.
{-# INLINE map #-}
map :: (Char -> Char) -> Utf8 -> Utf8
map f = unstream . Stream.map f . stream

-- | /O(n)/ The 'intercalate' function takes a 'Utf8' and a list of
-- 'Utf8's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Example:
--
-- >>> Utf8.intercalate "NI!" ["We", "seek", "the", "Holy", "Grail"]
-- "WeNI!seekNI!theNI!HolyNI!Grail"
{-# INLINE intercalate #-}
intercalate :: Utf8 -> [Utf8] -> Utf8
intercalate = undefined

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Utf8'.
--
-- Example:
--
-- >>> Utf8.intersperse '.' "SHIELD"
-- "S.H.I.E.L.D"
--
--  Performs replacement on invalid scalar values.
{-# INLINE intersperse #-}
intersperse :: Char -> Utf8 -> Utf8
intersperse = undefined

-- | /O(n)/ Reverse the characters of a string.
--
-- Example:
--
-- >>> Utf8.reverse "desrever"
-- "reversed"
--
{-# INLINE reverse #-}
reverse :: Utf8 -> Utf8
reverse = unstream . Stream.reverse . stream


-- | /O(m+n)/ Replace every non-overlapping occurrence of @needle@ in
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
-- >>> replace "oo" "foo" "oo"
-- "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- >>> replace "ofo" "bar" "ofofo"
-- "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
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

-- | /O(n)/ Convert a string to folded case.
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
{-# INLINE toCaseFold #-}
toCaseFold :: Utf8 -> Utf8
toCaseFold = undefined

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, \"&#x130;\" (Latin capital letter I with dot above,
-- U+0130) maps to the sequence \"i\" (Latin small letter i, U+0069)
-- followed by \" &#x307;\" (combining dot above, U+0307).
{-# INLINE toLower #-}
toLower :: Utf8 -> Utf8
toLower = undefined

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the German \"&#xdf;\" (eszett, U+00DF) maps to the
-- two-letter sequence \"SS\".
{-# INLINE toUpper #-}
toUpper :: Utf8 -> Utf8
toUpper = undefined

-- | /O(n)/ Convert a 'Utf8' to title case, using simple case
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
{-# INLINE toTitle #-}
toTitle :: Utf8 -> Utf8
toTitle = undefined

-- | /O(n)/ Left-justify a string to the given length, using the
-- specified fill character on the right.
-- Performs replacement on invalid scalar values.
--
-- Examples:
--
-- >>> justifyLeft 7 'x' "foo"
-- "fooxxxx"
--
-- >>> justifyLeft 3 'x' "foobar"
-- "foobar"
{-# INLINE justifyLeft #-}
justifyLeft :: Int -> Char -> Utf8 -> Utf8
justifyLeft = undefined

-- | /O(n)/ Right-justify a string to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- >>> justifyRight 7 'x' "bar"
-- "xxxxbar"
--
-- >>> justifyRight 3 'x' "foobar"
-- "foobar"
{-# INLINE justifyRight #-}
justifyRight :: Int -> Char -> Utf8 -> Utf8
justifyRight = undefined

-- | /O(n)/ Center a string to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- >>> center 8 'x' "HS"
-- "xxxHSxxx"
{-# INLINE center #-}
center :: Int -> Char -> Utf8 -> Utf8
center = undefined

-- | /O(n)/ The 'transpose' function transposes the rows and columns
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
{-# INLINE transpose #-}
transpose :: [Utf8] -> [Utf8]
transpose = Prelude.map pack . List.transpose . Prelude.map unpack

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

-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
{-# INLINE scanl #-}
scanl :: (Char -> Char -> Char) -> Char -> Utf8 -> Utf8
scanl f z t = unstream (Stream.scanl' f z (stream t))

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
{-# INLINE scanl1 #-}
scanl1 :: (Char -> Char -> Char) -> Utf8 -> Utf8
scanl1 f t = unstream (Stream.scanl1' f (stream t))

-- | /O(n)/ 'scanl'' is similar to 'foldl'', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl' f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl' f z xs) == foldl f z xs.
{-# INLINE scanl' #-}
scanl' :: (Char -> Char -> Char) -> Char -> Utf8 -> Utf8
scanl' f z t = unstream (Stream.scanl' f z (stream t))

-- | /O(n)/ 'scanl1'' is a variant of 'scanl'' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1' f [x1, x2, ...] == [x1, x1 `f` x2, ...]
{-# INLINE scanl1' #-}
scanl1' :: (Char -> Char -> Char) -> Utf8 -> Utf8
scanl1' f t = unstream (Stream.scanl1' f (stream t))

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
{-# INLINE scanr #-}
scanr :: (Char -> Char -> Char) -> Char -> Utf8 -> Utf8
scanr = undefined

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument. Performs replacement on invalid scalar values.
{-# INLINE scanr1 #-}
scanr1 :: (Char -> Char -> Char) -> Utf8 -> Utf8
scanr1 = undefined

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

-- | /O(n)/ 'take' @n@, applied to a 'Utf8', returns the prefix of the
-- 'Utf8' of length @n@, or the 'Utf8' itself if @n@ is greater than
-- the length of the Utf8.
{-# INLINE_NORMAL take #-}
take :: Int -> Utf8 -> Utf8
take n t = unstream (Stream.take n (stream t))

-- | /O(n)/ 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> takeEnd 3 "foobar"
-- "bar"
--
{-# INLINE_NORMAL takeEnd #-}
takeEnd :: Int -> Utf8 -> Utf8
takeEnd = undefined

{-
iterNEnd :: Int -> Utf8 -> Int
iterNEnd n t@(Utf8 _arr _off len) = loop (len-1) n
  where loop i !m
          | m <= 0    = i+1
          | i <= 0    = 0
          | otherwise = loop (i+d) (m-1)
          where d = reverseIter_ t i
-}

-- | /O(n)/ 'drop' @n@, applied to a 'Utf8', returns the suffix of the
-- 'Utf8' after the first @n@ characters, or the empty 'Utf8' if @n@
-- is greater than the length of the 'Utf8'.
{-# INLINE_NORMAL drop #-}
drop :: Int -> Utf8 -> Utf8
drop n t = unstream (Stream.drop n (stream t))

-- | /O(n)/ 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> dropEnd 3 "foobar"
-- "foo"
--
{-# INLINE_NORMAL dropEnd #-}
dropEnd :: Int -> Utf8 -> Utf8
dropEnd = undefined

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Utf8',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.
{-# INLINE_NORMAL takeWhile #-}
takeWhile :: (Char -> Bool) -> Utf8 -> Utf8
takeWhile p t = unstream (Stream.takeWhile p (stream t))

-- | /O(n)/ 'takeWhileEnd', applied to a predicate @p@ and a 'Utf8',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- >>> takeWhileEnd (=='o') "foo"
-- "oo"
--
{-# INLINE_NORMAL takeWhileEnd #-}
takeWhileEnd :: (Char -> Bool) -> Utf8 -> Utf8
takeWhileEnd = undefined

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.
{-# INLINE_NORMAL dropWhile #-}
dropWhile :: (Char -> Bool) -> Utf8 -> Utf8
dropWhile p t = unstream (Stream.dropWhile p (stream t))

-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- >>> dropWhileEnd (=='.') "foo..."
-- "foo"
{-# INLINE_NORMAL dropWhileEnd #-}
dropWhileEnd :: (Char -> Bool) -> Utf8 -> Utf8
dropWhileEnd = undefined

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.
{-# INLINE_NORMAL dropAround #-}
dropAround :: (Char -> Bool) -> Utf8 -> Utf8
dropAround p = dropWhile p . dropWhileEnd p

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
{-# INLINE stripStart #-}
stripStart :: Utf8 -> Utf8
stripStart = dropWhile isSpace

-- | /O(n)/ Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
{-# INLINE_NORMAL stripEnd #-}
stripEnd :: Utf8 -> Utf8
stripEnd = dropWhileEnd isSpace

-- | /O(n)/ Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
{-# INLINE_NORMAL strip #-}
strip :: Utf8 -> Utf8
strip = dropAround isSpace

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
elem c t = any (== c) t

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

-- | /O(n)/ 'filter', applied to a predicate and a 'Utf8',
-- returns a 'Utf8' containing those characters that satisfy the
-- predicate.
{-# INLINE filter #-}
filter :: (Char -> Bool) -> Utf8 -> Utf8
filter p t = unstream (Stream.filter p (stream t))

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
words t = split isSpace t

-- | /O(n)/ Breaks a 'Utf8' up into a list of 'Utf8's at
-- newline 'Char's. The resulting strings do not contain newlines.
{-# INLINE lines #-}
lines :: Utf8 -> [Utf8]
lines t = split (== '\n') t

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
{-# INLINE unlines #-}
unlines :: [Utf8] -> Utf8
unlines = concat . List.map (`snoc` '\n')

-- | /O(n)/ Joins words using single space characters.
{-# INLINE unwords #-}
unwords :: [Utf8] -> Utf8
unwords = intercalate (singleton ' ')
