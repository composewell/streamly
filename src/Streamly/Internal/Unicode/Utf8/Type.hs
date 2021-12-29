-- |
-- Module      : Streamly.Internal.Unicode.Utf8.Type
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Utf8.Type
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

    -- * Folds
    , write

    -- * Unfolds
    , read
    )
where


#include "inline.hs"

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Data.String (IsString(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Foreign.Type (Array)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream.IsStream (SerialT)
import Streamly.Internal.Data.Unfold (Unfold)
import System.IO.Unsafe (unsafePerformIO)

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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Internal.Unicode.Utf8 as Utf8

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

instance IsString Utf8 where
    {-# INLINE fromString #-}
    fromString = pack

instance Show Utf8 where
    {-# INLINE showsPrec #-}
    showsPrec p ps r = showsPrec p (unpack ps) r

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
