{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Streamly.Internal.Data.Unboxed
    ( Storable
    , Unboxed
    , alignment
    , peekWith
    , pokeWith
    , sizeOf
    , ArrayContents(..)
    , castContents
    , touch
    , getMutableByteArray#
    , pin
    , unpin
    , newUnpinnedArrayContents
    ) where

#include "MachDeps.h"

#include "ArrayMacros.h"

import Data.Complex (Complex((:+)), realPart)
import GHC.Base (IO(..))
import GHC.Int (Int32(..), Int64(..))
import GHC.Word (Word8(..), Word64(..))

import GHC.Exts

--------------------------------------------------------------------------------
-- The ArrayContents type
--------------------------------------------------------------------------------

-- XXX can use UnliftedNewtypes
data ArrayContents a = ArrayContents !(MutableByteArray# RealWorld)

{-# INLINE getMutableByteArray# #-}
getMutableByteArray# :: ArrayContents a -> MutableByteArray# RealWorld
getMutableByteArray# (ArrayContents mbarr) = mbarr

{-# INLINE castContents #-}
castContents :: ArrayContents a -> ArrayContents b
castContents (ArrayContents mbarr) = ArrayContents mbarr

{-# INLINE touch #-}
touch :: ArrayContents a -> IO ()
touch (ArrayContents contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

{-# INLINE newUnpinnedArrayContents #-}
newUnpinnedArrayContents :: Int -> IO (ArrayContents a)
newUnpinnedArrayContents nbytes | nbytes < 0 =
  errorWithoutStackTrace "newUnpinnedArrayContents: size must be >= 0"
newUnpinnedArrayContents (I# nbytes) = IO $ \s ->
    case newByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = ArrayContents mbarr#
            in (# s', c #)

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

{-# INLINE isPinned #-}
isPinned :: ArrayContents a -> Bool
isPinned (ArrayContents arr#) =
    let pinnedInt = I# (isMutableByteArrayPinned# arr#)
     in pinnedInt == 1


{-# INLINE cloneMutableArrayWith# #-}
cloneMutableArrayWith#
    :: (Int# -> State# RealWorld -> (# State# RealWorld
                                     , MutableByteArray# RealWorld #))
    -> MutableByteArray# RealWorld
    -> State# RealWorld
    -> (# State# RealWorld, MutableByteArray# RealWorld #)
cloneMutableArrayWith# alloc# arr# s# =
    case getSizeofMutableByteArray# arr# s# of
        (# s1#, i# #) ->
            case alloc# i# s1# of
                (# s2#, arr1# #) ->
                    case copyMutableByteArray# arr# 0# arr1# 0# i# s2# of
                        s3# -> (# s3#, arr1# #)

{-# INLINE pin #-}
pin :: ArrayContents a -> IO (ArrayContents a)
pin arr@(ArrayContents marr#) =
    if isPinned arr
    then return arr
    else IO
             $ \s# ->
                   case cloneMutableArrayWith# newPinnedByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, ArrayContents marr1# #)

{-# INLINE unpin #-}
unpin :: ArrayContents a -> IO (ArrayContents a)
unpin arr@(ArrayContents marr#) =
    if not (isPinned arr)
    then return arr
    else IO
             $ \s# ->
                   case cloneMutableArrayWith# newByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, ArrayContents marr1# #)

--------------------------------------------------------------------------------
-- The Unboxed type class
--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,12,0)

#define SIZEOF_HSCHAR_PRIMITIVE 4#
#define SIZEOF_HSINT_PRIMITIVE 8#
#define SIZEOF_HSWORD_PRIMITIVE 8#
#define SIZEOF_WORD8_PRIMITIVE 1#
#define SIZEOF_WORD64_PRIMITIVE 8#
#define SIZEOF_HSDOUBLE_PRIMITIVE 8#
#define SIZEOF_INT32_PRIMITIVE 4#
#define SIZEOF_INT64_PRIMITIVE 8#

#ifdef __GHCJS__
#define WORD64TYP Word64#
#else
#define WORD64TYP Word#
#endif

#ifdef __GHCJS__
#define INT64TYP Int64#
#else
#define INT64TYP Int#
#endif

{-# INLINE readWord8ArrayAsWideChar# #-}
readWord8ArrayAsWideChar# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Char# #)
readWord8ArrayAsWideChar# arr# i# s# =
    readWideCharArray# arr# (quotInt# i# SIZEOF_HSCHAR_PRIMITIVE) s#

{-# INLINE writeWord8ArrayAsWideChar# #-}
writeWord8ArrayAsWideChar# ::
       MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsWideChar# arr# i# a# s# =
    writeWideCharArray# arr# (quotInt# i# SIZEOF_HSCHAR_PRIMITIVE) a# s#

{-# INLINE readWord8ArrayAsInt# #-}
readWord8ArrayAsInt# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt# arr# i# s# =
    readIntArray# arr# (quotInt# i# SIZEOF_HSINT_PRIMITIVE) s#

{-# INLINE writeWord8ArrayAsInt# #-}
writeWord8ArrayAsInt# ::
       MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt# arr# i# a# s# =
    writeIntArray# arr# (quotInt# i# SIZEOF_HSINT_PRIMITIVE) a# s#

{-# INLINE readWord8ArrayAsInt32# #-}
readWord8ArrayAsInt32# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt32# arr# i# s# =
    readInt32Array# arr# (quotInt# i# SIZEOF_INT32_PRIMITIVE) s#

{-# INLINE writeWord8ArrayAsInt32# #-}
writeWord8ArrayAsInt32# ::
       MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt32# arr# i# a# s# =
    writeInt32Array# arr# (quotInt# i# SIZEOF_INT32_PRIMITIVE) a# s#

{-# INLINE readWord8ArrayAsInt64# #-}
readWord8ArrayAsInt64# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, INT64TYP #)
readWord8ArrayAsInt64# arr# i# s# =
    readInt64Array# arr# (quotInt# i# SIZEOF_INT64_PRIMITIVE) s#

{-# INLINE writeWord8ArrayAsInt64# #-}
writeWord8ArrayAsInt64# ::
       MutableByteArray# d -> Int# -> INT64TYP -> State# d -> State# d
writeWord8ArrayAsInt64# arr# i# a# s# =
    writeInt64Array# arr# (quotInt# i# SIZEOF_INT64_PRIMITIVE) a# s#

{-# INLINE readWord8ArrayAsWord# #-}
readWord8ArrayAsWord# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord# arr# i# s# =
    readWordArray# arr# (quotInt# i# SIZEOF_HSWORD_PRIMITIVE) s#

{-# INLINE writeWord8ArrayAsWord# #-}
writeWord8ArrayAsWord# ::
       MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord# arr# i# a# s# =
    writeWordArray# arr# (quotInt# i# SIZEOF_HSWORD_PRIMITIVE) a# s#

{-# INLINE readWord8ArrayAsWord64# #-}
readWord8ArrayAsWord64# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, WORD64TYP #)
readWord8ArrayAsWord64# arr# i# s# =
    readWord64Array# arr# (quotInt# i# SIZEOF_WORD64_PRIMITIVE) s#

{-# INLINE writeWord8ArrayAsWord64# #-}
writeWord8ArrayAsWord64# ::
       MutableByteArray# d -> Int# -> WORD64TYP -> State# d -> State# d
writeWord8ArrayAsWord64# arr# i# a# s# =
    writeWord64Array# arr# (quotInt# i# SIZEOF_WORD64_PRIMITIVE) a# s#

{-# INLINE readWord8ArrayAsDouble# #-}
readWord8ArrayAsDouble# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)
readWord8ArrayAsDouble# arr# i# s# =
    readDoubleArray# arr# (quotInt# i# SIZEOF_HSDOUBLE_PRIMITIVE) s#

{-# INLINE writeWord8ArrayAsDouble# #-}
writeWord8ArrayAsDouble# ::
       MutableByteArray# d -> Int# -> Double# -> State# d -> State# d
writeWord8ArrayAsDouble# arr# i# a# s# =
    writeDoubleArray# arr# (quotInt# i# SIZEOF_HSDOUBLE_PRIMITIVE) a# s#

#endif

-- In theory we could convert a type to and from a byte stream and use that
-- to implement boxing, unboxing. But that would be inefficient. This type
-- class allows each primitive type to have its own specific efficient
-- implementation to read and write the type to memory.
--
-- The ArrayContents type currently uses a MutableByteArray#. However, we could
-- also use a foreign ptr instead. That would just make the implementation a
-- bit more complicated. In that case the type class would have to support
-- reading and writing to a Ptr as well, basically what Storable does. We will
-- also need to touch the anchoring ptr at the right points which is prone to
-- errors. However, it should be simple to implement unmanaged/read-only memory
-- arrays by allowing a Ptr type in ArrayContents, though it would require all
-- instances to support reading from a Ptr.
--
-- There is a reason for using byte offset instead of element index in read and
-- write operations in the type class. If we use element index slicing of the
-- array becomes rigid. We can only slice the array at addresses that are
-- aligned with the type, therefore, we cannot slice at misaligned location and
-- then cast the slice into another type which does not ncessarily align with
-- the original type.
--
-- As a side note, there seem to be no performance advantage of alignment
-- anymore, see
-- https://lemire.me/blog/2012/05/31/data-alignment-for-speed-myth-or-reality/
--
-- We try to keep the instances compatible with Storable. But we may have to
-- differ, for example a Bool instance can be implemented more efficiently
-- using a bit vector. Need to check the performance. We can possibly write
-- tests for checking compatibility with Storable.

-- | An 'Unboxed' type supplies operations for reading and writing the type
-- from and to a byte array in memory. The read operation constructs the boxed
-- type from an unboxed byte representation in memory. The write operation
-- writes the boxed type to an unboxed byte representation in memory.
--
-- This type class is similar to Storable. While Storable allows writing to a
-- Ptr, this type class allows writing to a MutableByteArray#.
class Unboxed a where
    sizeOf :: a -> Int
    alignment :: a -> Int

    -- Read an element of type "a" from a MutableByteArray given the byte
    -- index.
    readByteArray :: ArrayContents a -> Int -> IO a
    writeByteArray :: ArrayContents a -> Int -> a -> IO ()

#define DERIVE_UNBOXED(_type, _constructor, _sizeOf, _alignment, _readArray, _writeArray) \
instance Unboxed _type where {                                       \
; {-# INLINE sizeOf #-}                                              \
; sizeOf _ = _sizeOf                                                 \
; {-# INLINE alignment #-}                                           \
; alignment _ =  _alignment                                          \
; {-# INLINE readByteArray #-}                                       \
; readByteArray (ArrayContents mbarr) (I# n) = IO $ \s ->            \
      case _readArray mbarr n s of                                   \
          { (# s1, i #) -> (# s1, _constructor i #) }                \
; {-# INLINE writeByteArray #-}                                      \
; writeByteArray (ArrayContents mbarr) (I# n) (_constructor val) =   \
        IO $ \s -> (# _writeArray mbarr n val s, () #)               \
}

DERIVE_UNBOXED( Char
              , C#
              , SIZEOF_HSCHAR
              , ALIGNMENT_HSCHAR
              , readWord8ArrayAsWideChar#
              , writeWord8ArrayAsWideChar#)

DERIVE_UNBOXED( Int32
              , I32#
              , SIZEOF_INT32
              , ALIGNMENT_INT32
              , readWord8ArrayAsInt32#
              , writeWord8ArrayAsInt32#)

DERIVE_UNBOXED( Int
              , I#
              , SIZEOF_HSINT
              , ALIGNMENT_HSINT
              , readWord8ArrayAsInt#
              , writeWord8ArrayAsInt#)

DERIVE_UNBOXED( Int64
              , I64#
              , SIZEOF_INT64
              , ALIGNMENT_INT64
              , readWord8ArrayAsInt64#
              , writeWord8ArrayAsInt64#)

DERIVE_UNBOXED( Word
              , W#
              , SIZEOF_HSWORD
              , ALIGNMENT_HSWORD
              , readWord8ArrayAsWord#
              , writeWord8ArrayAsWord#)

DERIVE_UNBOXED( Word8
              , W8#
              , SIZEOF_WORD8
              , ALIGNMENT_WORD8
              , readWord8Array#
              , writeWord8Array#)

DERIVE_UNBOXED( Word64
              , W64#
              , SIZEOF_WORD64
              , ALIGNMENT_WORD64
              , readWord8ArrayAsWord64#
              , writeWord8ArrayAsWord64#)

DERIVE_UNBOXED( Double
              , D#
              , SIZEOF_DOUBLE
              , ALIGNMENT_DOUBLE
              , readWord8ArrayAsDouble#
              , writeWord8ArrayAsDouble#)

instance Unboxed Bool where
    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (undefined :: Word8)

    {-# INLINE alignment #-}
    alignment _ = alignment (undefined :: Word8)

    {-# INLINE readByteArray #-}
    readByteArray arr i = do
        res <- readByteArray (castContents arr) i
        return $ res /= (0 :: Word8)

    {-# INLINE writeByteArray #-}
    writeByteArray arr i a =
        case a of
            True -> writeByteArray (castContents arr) i (1 :: Word8)
            False -> writeByteArray (castContents arr) i (0 :: Word8)

instance forall a. Unboxed a => Unboxed (Complex a) where
    {-# INLINE sizeOf #-}
    sizeOf a = 2 * sizeOf (realPart a)

    {-# INLINE alignment #-}
    alignment a = alignment (realPart a)

    {-# INLINE readByteArray #-}
    readByteArray arr i = do
        let contents = castContents arr :: ArrayContents a
        real <- readByteArray contents i
        img <- readByteArray contents (i + SIZE_OF(a))
        return $ real :+ img

    {-# INLINE writeByteArray #-}
    writeByteArray arr i (real :+ img) = do
        let contents = castContents arr :: ArrayContents a
        writeByteArray contents i real
        writeByteArray contents (i + SIZE_OF(a)) img

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

type Storable a = Unboxed a

{-# INLINE peekWith #-}
peekWith :: Unboxed a => ArrayContents a -> Int -> IO a
peekWith = readByteArray

{-# INLINE pokeWith #-}
pokeWith :: Unboxed a => ArrayContents a -> Int -> a -> IO ()
pokeWith = writeByteArray
