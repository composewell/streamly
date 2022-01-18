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
    , toStream
    , fromStream
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
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (second)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.String (IsString(..))
import Data.Word (Word8)
import GHC.Base (assert)
import GHC.IO.Encoding.Failure (isSurrogate)
import Streamly.Internal.Data.Array.Foreign.Type (Array)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream.IsStream (SerialT)
import Streamly.Internal.Data.Unfold (Unfold)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MArray
import qualified Streamly.Internal.Data.Fold.Type as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Prelude hiding (head, init, last, length, null, read, tail)

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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINE toStream #-}
toStream :: Utf8 -> SerialT IO Char
toStream = Unicode.decodeUtf8' . Array.toStream . toArray

{-# INLINE fromStream #-}
fromStream :: SerialT IO Char -> Utf8
fromStream =
    Utf8 . unsafePerformIO . Stream.fold Array.write . Unicode.encodeUtf8'

--------------------------------------------------------------------------------
-- Conversion to/from 'Utf8'
--------------------------------------------------------------------------------

{-# INLINE toArray #-}
toArray :: Utf8 -> Array Word8
toArray (Utf8 arr) = arr

-- | Convert a 'String' into a 'Utf8'. Performs
-- replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL pack #-}
pack :: String -> Utf8
pack = fromStream . Stream.fromList

-- | Convert a 'Utf8' into a 'String'.
--
-- /Time complexity:/ O(n)
{-# INLINE_NORMAL unpack #-}
unpack :: Utf8 -> String
unpack = unsafePerformIO . Stream.toList . toStream

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance IsString Utf8 where
    {-# INLINE fromString #-}
    fromString = pack

instance Show Utf8 where
    {-# INLINE showsPrec #-}
    showsPrec p ps r = showsPrec p (unpack ps) r

--------------------------------------------------------------------------------
-- Streamly style APIs
--------------------------------------------------------------------------------

-- XXX From the review:
--
-- This should be implemented as an Unfold m Char Word8 composed with the input
-- of Array.write. For that we would need to implement unfoldMany for folds:
--
-- > unfoldMany :: Unfold m a b -> Fold m b c -> Fold m a c
--
-- If we assume the argument fold to be a non-terminating then it should be easy
-- to implement. That is do not handle the done case, just error out in the done
-- case.
--
-- Once we have that then we can use:
--
-- > writeGeneric = Fold.unfoldMany readCharUtf8 A.write
--
-- For readCharUtf8 see https://github.com/composewell/streamly/pull/1055/files
{-# INLINE write #-}
write :: forall m. MonadIO m => Fold m Char Utf8
write = Fold.Fold step initial (return . Utf8 . Array.unsafeFreeze)

    where

    -- XXX Start of with some specific size?
    initial = return $ Fold.Partial MArray.nil

    -- XXX snocExp over snoc?
    step arr c =
        case ord c of
            x
                | x <= 0x7F -> do
                    arr1 <- arr `MArray.snoc` fromIntegral x
                    return $ Fold.Partial arr1
                | x <= 0x7FF -> do
                    arr1 <- arr `snoc2` c
                    return $ Fold.Partial arr1
                | x <= 0xFFFF ->
                    if isSurrogate c
                    then Fold.Partial <$> snoc3_ arr 239 191 189
                    else do
                        arr1 <- arr `snoc3` c
                        return $ Fold.Partial arr1
                | otherwise -> do
                    arr1 <- arr `snoc4` c
                    return $ Fold.Partial arr1

    {-# INLINE snoc2 #-}
    snoc2 :: MArray.Array Word8 -> Char -> m (MArray.Array Word8)
    snoc2 arr c =
        assert (n >= 0x80 && n <= 0x07ff)
            $ do
                arr1 <- arr `MArray.snoc` x1
                arr1 `MArray.snoc` x2

        where

        n = ord c
        x1 = fromIntegral $ (n `shiftR` 6) + 0xC0
        x2 = fromIntegral $ (n .&. 0x3F) + 0x80

    {-# INLINE snoc3_ #-}
    snoc3_ ::
           MArray.Array Word8
        -> Word8
        -> Word8
        -> Word8
        -> m (MArray.Array Word8)
    snoc3_ arr x1 x2 x3 = do
        arr1 <- arr `MArray.snoc` x1
        arr2 <- arr1 `MArray.snoc` x2
        arr2 `MArray.snoc` x3

    {-# INLINE snoc3 #-}
    snoc3 :: MArray.Array Word8 -> Char -> m (MArray.Array Word8)
    snoc3 arr c = assert (n >= 0x80 && n <= 0x07ff) (snoc3_ arr x1 x2 x3)

        where

        n = ord c
        x1 = fromIntegral $ (n `shiftR` 12) + 0xE0
        x2 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
        x3 = fromIntegral $ (n .&. 0x3F) + 0x80

    {-# INLINE snoc4 #-}
    snoc4 :: MArray.Array Word8 -> Char -> m (MArray.Array Word8)
    snoc4 arr c =
        assert (n >= 0x80 && n <= 0x07ff)
            $ do
                arr1 <- arr `MArray.snoc` x1
                arr2 <- arr1 `MArray.snoc` x2
                arr3 <- arr2 `MArray.snoc` x3
                arr3 `MArray.snoc` x4

        where

        n = ord c
        x1 = fromIntegral $ (n `shiftR` 18) + 0xF0
        x2 = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
        x3 = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
        x4 = fromIntegral $ (n .&. 0x3F) + 0x80

-- |
--
-- /Unimplemented/
{-# INLINE read #-}
read :: Unfold m Utf8 Char
read = undefined

--------------------------------------------------------------------------------
-- Basic functions
--------------------------------------------------------------------------------

singleton :: Char -> Utf8
singleton x = pack [x]

-- XXX From the review:
--
-- StreamD cons would be better here. And we should have a caveat that this
-- function should be avoided to build a big array using this, so you should not
-- be using foldr cons empty that would suck with StreamD cons. But an operation
-- like x cons xs would work much better with StreamD cons compared to regular
-- cons.
--
-- You can also memcpy if that turns out to be faster than stream.
-- | Adds a character to the front of a 'Utf8'. This function is more
-- costly than its 'List' counterpart because it requires copying a new array.
-- Performs replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
{-# INLINE cons #-}
cons :: Char -> Utf8 -> Utf8
cons c = fromStream . Stream.cons c . toStream

-- | Adds a character to the end of a 'Utf8'.  This copies the entire
-- array in the process, unless fused.   Performs replacement
-- on invalid scalar values.
--
-- /Time complexity:/ O(n)
--
-- /Unimplemented/
{-# INLINE snoc #-}
snoc :: Utf8 -> Char -> Utf8
snoc = undefined

-- | Appends one 'Utf8' to the other by copying both of them into a new
-- 'Utf8'.
--
-- /Time complexity:/ O(n)
{-# NOINLINE append #-}
append :: Utf8 -> Utf8 -> Utf8
append (Utf8 a) (Utf8 b) = Utf8 $ unsafePerformIO $ Array.splice a b

-- | Returns the first character of a 'Utf8', or 'Nothing' if empty.
--
--
-- /Time complexity:/ O(1)
{-# INLINE head #-}
head :: Utf8 -> Maybe Char
head = unsafePerformIO . Stream.head . toStream

-- XXX From the review:
--
-- We can use a length fold and a single char decoding fold in parallel on the
-- stream. Then we can use a array slice to get the tail of the array using the
-- length returned by the length fold.
--
-- Alternatively, we could get the head char, find its encoded length and use
-- that to slice the array.
-- | Returns the first character and rest of a 'Utf8', or 'Nothing' if
-- empty.
--
-- /Time complexity:/ O(1)
{-# INLINE_NORMAL uncons #-}
uncons :: Utf8 -> Maybe (Char, Utf8)
uncons = fmap (second fromStream) . unsafePerformIO . Stream.uncons . toStream

-- | Returns the last character of a 'Utf8', or 'Nothing' if empty.
--
--
-- /Time complexity:/ O(1)
--
-- /Unimplemented/
{-# INLINE_NORMAL last #-}
last :: Utf8 -> Char
last = undefined

-- | Returns all characters after the head of a 'Utf8', or 'Nothing' if
-- empty.
--
-- /Time complexity:/ O(1)
{-# INLINE_NORMAL tail #-}
tail :: Utf8 -> Maybe Utf8
tail = fmap snd . uncons

-- XXX From the review
--
-- If we can write a routine to decode utf8 in reverse then we can just decode
-- the last char from the end of the array and then slice it.
--
-- Otherwise, use last on the stream, get the encoded length of the last char
-- and use that to slice it.
-- | Returns all but the last character of a 'Utf8', or 'Nothing' if
-- empty.
--
-- /Time complexity:/ O(1)
{-# INLINE_NORMAL init #-}
init :: Utf8 -> Maybe Utf8
init = fmap fromStream . unsafePerformIO . Stream.init . toStream

-- | Returns all but the last character and the last character of a
-- 'Utf8', or 'Nothing' if empty.
--
-- /Time complexity:/ O(1)
--
-- /Unimplemented/
{-# INLINE unsnoc #-}
unsnoc :: Utf8 -> Maybe (Utf8, Char)
unsnoc = undefined

-- | Tests whether a 'Utf8' is empty or not.
--
-- /Time complexity:/ O(1)
{-# INLINE null #-}
null :: Utf8 -> Bool
null = Array.null . toArray

-- | Tests whether a 'Utf8' contains exactly one character.
--
-- /Time complexity:/ O(1)
--
-- /Unimplemented/
{-# INLINE isSingleton #-}
isSingleton :: Utf8 -> Bool
isSingleton = undefined

-- XXX From the review
--
-- We could possibly determine the length faster by using a custom routine that
-- counts the starting chars from the utf8 encoded bytes without decoding the
-- chars.
-- | Returns the number of characters in a 'Utf8'.
--
-- /Time complexity:/ O(n)
{-# INLINE length #-}
length :: Utf8 -> Int
length = unsafePerformIO . Stream.length . toStream

-- | Compare the count of characters in a 'Utf8' to a number.
--
-- This function gives the same answer as comparing against the result of
-- 'length', but can short circuit if the count of characters is greater than
-- the number, and hence be more efficient.
--
-- /Time complexity:/ O(n)
--
-- /Unimplemented/
{-# INLINE_NORMAL compareLength #-}
compareLength :: Utf8 -> Int -> Ordering
compareLength = undefined
