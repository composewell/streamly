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
    , elemWord
    , addToWord
    , addToSigns
    , wordToElem

    -- * Karp-Rabin hash
    , hashPower
    , addHash
    , arrayHash
    , elemHashAt
    )
where

import Data.Bits (finiteBitSize, shiftL, shiftR, xor, (.|.), (.&.))
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32, Word64)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.MutByteArray.Type (MutByteArray)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

-------------------------------------------------------------------------------
-- Word packing
-------------------------------------------------------------------------------

-- Decoding a packed element needs its sign. Alternatives considered:
--
-- * Test signedness of the type once using toEnum 0, toEnum 1 and
--   enumFromThen, then branch in the decode or use separate signed and
--   unsigned states: fails for an Enum instance partial at those values.
-- * One flag per stream, set if any fromEnum was negative: wrong if fromEnum
--   does not fit in the element bits, e.g. Float.
-- * Pack the Unbox bytes instead of fromEnum: exact for all types, costs a
--   store and a load per element.
--
-- We keep one sign bit per element in a separate word, see 'addToSigns'.
--
-- XXX The sequence search combinators could be specialized to the element
-- type using rewrite rules:
--
-- * For element types of at most word size the state machine may become
--   simpler. Check whether the current code already simplifies to the same
--   loop for such types or a specialized definition does better.
-- * Common signed and unsigned Enum types, e.g. Int8 and Word8, could be
--   rewritten to a definition that does not keep the sign bits, the
--   signedness of these types is known statically. Other types would fall
--   back to the definition that keeps the sign bit of each element, which is
--   slower.

-- | A mask of the low @bits@ bits of a 'Word', @bits@ must be less than the
-- word size.
{-# INLINE elemMask #-}
elemMask :: Int -> Word
elemMask bits = (1 `shiftL` bits) - 1

-- | @fromEnum a@ as a 'Word', the value packed by 'addToWord' and
-- 'addToSigns'.
{-# INLINE elemWord #-}
elemWord :: Enum a => a -> Word
elemWord = fromIntegral . fromEnum

-- | @addToWord bits word e@ shifts @word@ left by @bits@ and puts the low
-- @bits@ bits of @e@, built by 'elemWord', in the vacated bits.
{-# INLINE addToWord #-}
addToWord :: Int -> Word -> Word -> Word
addToWord bits wd e = (wd `shiftL` bits) .|. (e .&. elemMask bits)

-- | @addToSigns signs e@ shifts @signs@ left by one bit and puts the sign
-- bit of @e@, built by 'elemWord', in bit 0. Kept alongside the word built by
-- 'addToWord', bit @pos@ of @signs@ is the sign of the element packed @pos@
-- positions from the low end of the word.
{-# INLINE addToSigns #-}
addToSigns :: Word -> Word -> Word
addToSigns signs e = (signs `shiftL` 1) .|. (e `shiftR` (finiteBitSize e - 1))

-- | @wordToElem bits pos signs word@ decodes the element packed @pos@ positions
-- from the low end of @word@ by 'addToWord', with its sign from @signs@ built
-- by 'addToSigns'. @bits@ must be less than the word size. If the sign bit is
-- set the value is sign-extended from @bits@ bits, so @toEnum@ is called only
-- on @fromEnum@ of an element that was packed.
--
-- The decoding does not branch on the value of the element: the elements of
-- random data have the top bit set half of the time, a branch on it is
-- mispredicted often.
{-# INLINE wordToElem #-}
wordToElem :: Enum a => Int -> Int -> Word -> Word -> a
wordToElem bits pos signs wd =
    let i = fromIntegral ((wd `shiftR` (bits * pos)) .&. elemMask bits) :: Int
        s = fromIntegral ((signs `shiftR` pos) .&. 1) :: Int
     in toEnum $ i .|. (negate s `shiftL` bits)

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
