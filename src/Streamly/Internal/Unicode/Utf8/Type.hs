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

{-# INLINE read #-}
read :: Unfold m Utf8 Char
read = undefined

--------------------------------------------------------------------------------
-- Basic functions
--------------------------------------------------------------------------------

singleton :: Char -> Utf8
singleton x = pack [x]

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
{-# INLINE_NORMAL last #-}
last :: Utf8 -> Char
last = undefined

-- | Returns all characters after the head of a 'Utf8', or 'Nothing' if
-- empty.
--
-- /Time complexity:/ O(1)
{-# INLINE_NORMAL tail #-}
tail :: Utf8 -> Maybe Utf8
tail = fmap fromStream . unsafePerformIO . Stream.tail . toStream

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
{-# INLINE isSingleton #-}
isSingleton :: Utf8 -> Bool
isSingleton = undefined

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
{-# INLINE_NORMAL compareLength #-}
compareLength :: Utf8 -> Int -> Ordering
compareLength = undefined
