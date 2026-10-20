-- |
-- Module      : Streamly.Internal.Data.SeqMatch
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Helpers for searching a sequence of 'Unbox' elements in a stream.
--
-- A short pattern (at most one machine word in total size) is matched by
-- packing the elements into a 'Word', each element taking @8 * sizeOf a@
-- bits. A longer pattern is matched using a Karp-Rabin rolling hash of the
-- elements, and a hash match is confirmed by comparing bytes.

module Streamly.Internal.Data.SeqMatch
    (
    -- * Word packing
      elemMask
    , addToWord
    , wordToElem

    -- * Karp-Rabin hash
    , hashPower
    , addHash
    , arrayHash
    , elemHashAt
    )
where

import Data.Bits (shiftL, shiftR, xor, (.|.), (.&.))
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32, Word64)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.MutByteArray.Type (MutByteArray)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

-------------------------------------------------------------------------------
-- Word packing
-------------------------------------------------------------------------------

-- | A mask of the low @bits@ bits of a 'Word', @bits@ must be less than the
-- word size.
{-# INLINE elemMask #-}
elemMask :: Int -> Word
elemMask bits = (1 `shiftL` bits) - 1

-- | @addToWord bits word a@ shifts @word@ left by @bits@ and puts the low
-- @bits@ bits of @fromEnum a@ in the vacated bits.
{-# INLINE addToWord #-}
addToWord :: Enum a => Int -> Word -> a -> Word
addToWord bits wd a =
    (wd `shiftL` bits) .|. (fromIntegral (fromEnum a) .&. elemMask bits)

-- | @wordToElem bits word@ decodes the element packed by 'addToWord' in the
-- low @bits@ bits of @word@, @bits@ must be less than the word size.
{-# INLINE wordToElem #-}
wordToElem :: Enum a => Int -> Word -> a
wordToElem bits wd = toEnum $ fromIntegral (wd .&. elemMask bits)

-------------------------------------------------------------------------------
-- Karp-Rabin hash
-------------------------------------------------------------------------------

-- The hash of a sequence of elements e0, e1, ..., e(n-1) is:
--
-- h0 * k^(n-1) + h1 * k^(n-2) + ... + h(n-1)
--
-- in 'Word32' arithmetic, where hi is the hash of the bytes of ei computed by
-- 'elemHashAt'. Hashing the bytes, instead of 'fromEnum' of the elements,
-- works for any 'Unbox' type, e.g. 'fromEnum' fails on Word64 values above
-- @maxBound :: Int@.
--
-- If @h@ is the hash of a window of @n@ elements, sliding the window by one
-- element gives:
--
-- h * k + elemHash new - k^n * elemHash old

k :: Word32
k = 2891336453

-- | @hashPower n@ is @k^n@, the multiplier to remove the oldest element from
-- the hash of a window of @n@ elements.
{-# INLINE hashPower #-}
hashPower :: Int -> Word32
hashPower n = k ^ n

-- | @addHash h e@ appends an element with hash @e@ to a sequence with hash
-- @h@.
{-# INLINE addHash #-}
addHash :: Word32 -> Word32 -> Word32
addHash h e = h * k + e

-- | The hash of the bytes of the element at the given byte offset. Elements
-- of size 1, 2, 4 or 8 are read as one unsigned word; a 64-bit word is folded
-- to 32 bits.
{-# INLINE elemHashAt #-}
elemHashAt :: forall a. Unbox a => Proxy a -> MutByteArray -> Int -> IO Word32
elemHashAt _ mba off =
    case sizeOf (Proxy :: Proxy a) of
        1 -> fromIntegral <$> (peekAt off mba :: IO Word8)
        2 -> fromIntegral <$> (peekAt off mba :: IO Word16)
        4 -> peekAt off mba
        8 -> do
            w <- peekAt off mba :: IO Word64
            return $ fromIntegral (w `xor` (w `shiftR` 32))
        n -> go 0 off (off + n)

    where

    go h i end
        | i >= end = return h
        | otherwise = do
            b <- peekAt i mba :: IO Word8
            go (addHash h (fromIntegral b)) (i + 1) end

-- | The hash of the elements of an array.
{-# INLINE arrayHash #-}
arrayHash :: forall a. Unbox a => Array a -> Word32
arrayHash (Array contents start end) = unsafeInlineIO $ go 0 start

    where

    size = sizeOf (Proxy :: Proxy a)

    go h i
        | i >= end = return h
        | otherwise = do
            e <- elemHashAt (Proxy :: Proxy a) contents i
            go (addHash h e) (i + size)
