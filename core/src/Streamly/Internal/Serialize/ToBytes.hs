-- |
-- Module      : Streamly.Internal.Serialize.ToBytes
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Encode Haskell data types to byte streams.

module Streamly.Internal.Serialize.ToBytes
    (
    -- * Type class
      ToBytes (..)

    -- * Encoders
    , unit
    , bool
    , ordering
    , word8
    , word16be
    , word16le
    , word32be
    , word32le
    , word64be
    , word64le
    , word64host
    , int8
    , int16be
    , int16le
    , int32be
    , int32le
    , int64be
    , int64le
    )
where

#include "MachDeps.h"

import Data.Bits (shiftR)
import Data.Word (Word8, Word16, Word32, Word64)
import Streamly.Internal.Data.Stream (Stream, fromStreamD)
import Streamly.Internal.Data.Stream.StreamD (Step(..))

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD as D
import Data.Int (Int8, Int16, Int32, Int64)

-- XXX Use StreamD directly?

-- | A value of type '()' is encoded as @0@ in binary encoding.
--
-- @
-- 0 ==> ()
-- @
--
-- /Pre-release/
--
{-# INLINE unit #-}
unit :: Stream m Word8
unit = Stream.fromPure 0

{-# INLINE boolToWord8 #-}
boolToWord8 :: Bool -> Word8
boolToWord8 False = 0
boolToWord8 True = 1

-- | A value of type 'Bool' is encoded as follows in binary encoding.
--
-- @
-- 0 ==> False
-- 1 ==> True
-- @
--
-- /Pre-release/
--
{-# INLINE bool #-}
bool :: Bool -> Stream m Word8
bool = Stream.fromPure . boolToWord8

{-# INLINE orderingToWord8 #-}
orderingToWord8 :: Ordering -> Word8
orderingToWord8 LT = 0
orderingToWord8 EQ = 1
orderingToWord8 GT = 2

-- | A value of type 'Ordering' is encoded as follows in binary encoding.
--
-- @
-- 0 ==> LT
-- 1 ==> EQ
-- 2 ==> GT
-- @
--
-- /Pre-release/
--
{-# INLINE ordering #-}
ordering :: Ordering -> Stream m Word8
ordering = Stream.fromPure . orderingToWord8

-- | Stream a 'Word8'.
--
-- /Pre-release/
--
{-# INLINE word8 #-}
word8 :: Word8 -> Stream m Word8
word8 = Stream.fromPure

data W16State = W16B1 | W16B2 | W16Done

{-# INLINE word16beD #-}
word16beD :: Applicative m => Word16 -> D.Stream m Word8
word16beD w = D.Stream step W16B1

    where

    step _ W16B1 = pure $ Yield (fromIntegral (shiftR w 8) :: Word8) W16B2
    step _ W16B2 = pure $ Yield (fromIntegral w :: Word8) W16Done
    step _ W16Done = pure Stop

-- | Stream a 'Word16' as two bytes, the first byte is the MSB of the Word16
-- and second byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word16be #-}
word16be :: Monad m => Word16 -> Stream m Word8
word16be = fromStreamD . word16beD

-- | Little endian (LSB first) Word16
{-# INLINE word16leD #-}
word16leD :: Applicative m => Word16 -> D.Stream m Word8
word16leD w = D.Stream step W16B1

    where

    step _ W16B1 = pure $ Yield (fromIntegral w :: Word8) W16B2
    step _ W16B2 = pure $ Yield (fromIntegral (shiftR w 8) :: Word8) W16Done
    step _ W16Done = pure Stop

-- | Stream a 'Word16' as two bytes, the first byte is the LSB of the Word16
-- and second byte is the MSB (little endian representation).
--
-- /Pre-release/
--
{-# INLINE word16le #-}
word16le :: Monad m => Word16 -> Stream m Word8
word16le = fromStreamD . word16leD

data W32State = W32B1 | W32B2 | W32B3 | W32B4 | W32Done

-- | Big endian (MSB first) Word32
{-# INLINE word32beD #-}
word32beD :: Applicative m => Word32 -> D.Stream m Word8
word32beD w = D.Stream step W32B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR w n) :: Word8) s

    step _ W32B1 = yield 24 W32B2
    step _ W32B2 = yield 16 W32B3
    step _ W32B3 = yield 8 W32B4
    step _ W32B4 = pure $ Yield (fromIntegral w :: Word8) W32Done
    step _ W32Done = pure Stop

-- | Stream a 'Word32' as four bytes, the first byte is the MSB of the Word32
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word32be #-}
word32be :: Monad m => Word32 -> Stream m Word8
word32be = fromStreamD . word32beD

-- | Little endian (LSB first) Word32
{-# INLINE word32leD #-}
word32leD :: Applicative m => Word32 -> D.Stream m Word8
word32leD w = D.Stream step W32B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR w n) :: Word8) s

    step _ W32B1 = pure $ Yield (fromIntegral w :: Word8) W32B2
    step _ W32B2 = yield 8 W32B3
    step _ W32B3 = yield 16 W32B4
    step _ W32B4 = yield 24 W32Done
    step _ W32Done = pure Stop

-- | Stream a 'Word32' as four bytes, the first byte is the MSB of the Word32
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word32le #-}
word32le :: Monad m => Word32 -> Stream m Word8
word32le = fromStreamD . word32leD

data W64State =
    W64B1 | W64B2 | W64B3 | W64B4 | W64B5 | W64B6 | W64B7 | W64B8 | W64Done

-- | Big endian (MSB first) Word64
{-# INLINE word64beD #-}
word64beD :: Applicative m => Word64 -> D.Stream m Word8
word64beD w = D.Stream step W64B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR w n) :: Word8) s

    step _ W64B1 = yield 56 W64B2
    step _ W64B2 = yield 48 W64B3
    step _ W64B3 = yield 40 W64B4
    step _ W64B4 = yield 32 W64B5
    step _ W64B5 = yield 24 W64B6
    step _ W64B6 = yield 16 W64B7
    step _ W64B7 = yield  8 W64B8
    step _ W64B8 = pure $ Yield (fromIntegral w :: Word8) W64Done
    step _ W64Done = pure Stop

-- | Stream a 'Word64' as eight bytes, the first byte is the MSB of the Word64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word64be #-}
word64be :: Monad m => Word64 -> Stream m Word8
word64be = fromStreamD . word64beD

-- | Little endian (LSB first) Word64
{-# INLINE word64leD #-}
word64leD :: Applicative m => Word64 -> D.Stream m Word8
word64leD w = D.Stream step W64B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR w n) :: Word8) s

    step _ W64B1 = pure $ Yield (fromIntegral w :: Word8) W64B2
    step _ W64B2 = yield  8 W64B3
    step _ W64B3 = yield 16 W64B4
    step _ W64B4 = yield 24 W64B5
    step _ W64B5 = yield 32 W64B6
    step _ W64B6 = yield 40 W64B7
    step _ W64B7 = yield 48 W64B8
    step _ W64B8 = yield 56 W64Done
    step _ W64Done = pure Stop

-- | Stream a 'Word64' as eight bytes, the first byte is the MSB of the Word64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE word64le #-}
word64le :: Monad m => Word64 -> Stream m Word8
word64le = fromStreamD . word64leD

{-# INLINE int8 #-}
int8 :: Int8 -> Stream m Word8
int8 i = Stream.fromPure (fromIntegral i :: Word8)

-- | Stream a 'Int16' as two bytes, the first byte is the MSB of the Int16
-- and second byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int16beD #-}
int16beD :: Applicative m => Int16 -> D.Stream m Word8
int16beD i = D.Stream step W16B1

    where

    step _ W16B1 = pure $ Yield (fromIntegral (shiftR i 8) :: Word8) W16B2
    step _ W16B2 = pure $ Yield (fromIntegral i :: Word8) W16Done
    step _ W16Done = pure Stop

{-# INLINE int16be #-}
int16be :: Monad m => Int16 -> Stream m Word8
int16be = fromStreamD . int16beD

-- | Little endian (LSB first) Int16
{-# INLINE int16leD #-}
int16leD :: Applicative m => Int16 -> D.Stream m Word8
int16leD w = D.Stream step W16B1

    where

    step _ W16B1 = pure $ Yield (fromIntegral w :: Word8) W16B2
    step _ W16B2 = pure $ Yield (fromIntegral (shiftR w 8) :: Word8) W16Done
    step _ W16Done = pure Stop

-- | Stream a 'Int16' as two bytes, the first byte is the LSB of the Int16
-- and second byte is the MSB (little endian representation).
--
-- /Pre-release/
--
{-# INLINE int16le #-}
int16le :: Monad m => Int16 -> Stream m Word8
int16le = fromStreamD . int16leD

-- | Big endian (MSB first) Int32
{-# INLINE int32beD #-}
int32beD :: Applicative m => Int32 -> D.Stream m Word8
int32beD i = D.Stream step W32B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR i n) :: Word8) s

    step _ W32B1 = yield 24 W32B2
    step _ W32B2 = yield 16 W32B3
    step _ W32B3 = yield 8 W32B4
    step _ W32B4 = pure $ Yield (fromIntegral i :: Word8) W32Done
    step _ W32Done = pure Stop

{-# INLINE int32be #-}
int32be :: Monad m => Int32 -> Stream m Word8
int32be = fromStreamD . int32beD

-- | Little endian (LSB first) Int32
{-# INLINE int32leD #-}
int32leD :: Applicative m => Int32 -> D.Stream m Word8
int32leD w = D.Stream step W32B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR w n) :: Word8) s

    step _ W32B1 = pure $ Yield (fromIntegral w :: Word8) W32B2
    step _ W32B2 = yield 8 W32B3
    step _ W32B3 = yield 16 W32B4
    step _ W32B4 = yield 24 W32Done
    step _ W32Done = pure Stop

-- | Stream a 'Int32' as four bytes, the first byte is the MSB of the Int32
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int32le #-}
int32le :: Monad m => Int32 -> Stream m Word8
int32le = fromStreamD . int32leD

-- | Big endian (MSB first) Int64
{-# INLINE int64beD #-}
int64beD :: Applicative m => Int64 -> D.Stream m Word8
int64beD w = D.Stream step W64B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR w n) :: Word8) s

    step _ W64B1 = yield 56 W64B2
    step _ W64B2 = yield 48 W64B3
    step _ W64B3 = yield 40 W64B4
    step _ W64B4 = yield 32 W64B5
    step _ W64B5 = yield 24 W64B6
    step _ W64B6 = yield 16 W64B7
    step _ W64B7 = yield  8 W64B8
    step _ W64B8 = pure $ Yield (fromIntegral w :: Word8) W64Done
    step _ W64Done = pure Stop

-- | Stream a 'Int64' as eight bytes, the first byte is the MSB of the Int64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int64be #-}
int64be :: Monad m => Int64 -> Stream m Word8
int64be = fromStreamD . int64beD

-- | Little endian (LSB first) Int64
{-# INLINE int64leD #-}
int64leD :: Applicative m => Int64 -> D.Stream m Word8
int64leD w = D.Stream step W64B1

    where

    yield n s = pure $ Yield (fromIntegral (shiftR w n) :: Word8) s

    step _ W64B1 = pure $ Yield (fromIntegral w :: Word8) W64B2
    step _ W64B2 = yield  8 W64B3
    step _ W64B3 = yield 16 W64B4
    step _ W64B4 = yield 24 W64B5
    step _ W64B5 = yield 32 W64B6
    step _ W64B6 = yield 40 W64B7
    step _ W64B7 = yield 48 W64B8
    step _ W64B8 = yield 56 W64Done
    step _ W64Done = pure Stop

-- | Stream a 'Int64' as eight bytes, the first byte is the MSB of the Int64
-- and last byte is the LSB (big endian representation).
--
-- /Pre-release/
--
{-# INLINE int64le #-}
int64le :: Monad m => Int64 -> Stream m Word8
int64le = fromStreamD . int64leD
-------------------------------------------------------------------------------
-- Host byte order
-------------------------------------------------------------------------------

-- | Stream a 'Word64' as eight bytes in the host byte order.
--
-- /Pre-release/
--
{-# INLINE word64host #-}
word64host :: Monad m => Word64 -> Stream m Word8
word64host =
#ifdef WORDS_BIGENDIAN
    word64be
#else
    word64le
#endif

-------------------------------------------------------------------------------
-- Type class
-------------------------------------------------------------------------------

class ToBytes a where
    -- | Convert a Haskell type to a byte stream.
    toBytes :: a -> Stream m Word8
