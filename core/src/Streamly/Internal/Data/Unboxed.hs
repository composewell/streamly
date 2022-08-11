{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Streamly.Internal.Data.Unboxed
    ( Unboxed
    , Unbox(..)
    , alignment
    , peekWith
    , pokeWith
    , sizeOf
    , MutableByteArray(..)
    , castContents
    , touch
    , getMutableByteArray#
    , pin
    , unpin
    , newUnpinnedArrayContents
    ) where

#include "MachDeps.h"

#include "ArrayMacros.h"

import Data.Complex (Complex((:+)))
import GHC.Base (IO(..))
import GHC.Int (Int32(..), Int64(..))
import GHC.Word (Word8(..), Word64(..))
import Foreign.Storable (Storable(..))

import GHC.Exts

--------------------------------------------------------------------------------
-- The ArrayContents type
--------------------------------------------------------------------------------

-- XXX can use UnliftedNewtypes
data MutableByteArray a = MutableByteArray !(MutableByteArray# RealWorld)

{-# INLINE getMutableByteArray# #-}
getMutableByteArray# :: MutableByteArray a -> MutableByteArray# RealWorld
getMutableByteArray# (MutableByteArray mbarr) = mbarr

{-# INLINE castContents #-}
castContents :: MutableByteArray a -> MutableByteArray b
castContents (MutableByteArray mbarr) = MutableByteArray mbarr

{-# INLINE touch #-}
touch :: MutableByteArray a -> IO ()
touch (MutableByteArray contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

{-# INLINE newUnpinnedArrayContents #-}
newUnpinnedArrayContents :: Int -> IO (MutableByteArray a)
newUnpinnedArrayContents nbytes | nbytes < 0 =
  errorWithoutStackTrace "newUnpinnedArrayContents: size must be >= 0"
newUnpinnedArrayContents (I# nbytes) = IO $ \s ->
    case newByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = MutableByteArray mbarr#
            in (# s', c #)

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

{-# INLINE isPinned #-}
isPinned :: MutableByteArray a -> Bool
isPinned (MutableByteArray arr#) =
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
pin :: MutableByteArray a -> IO (MutableByteArray a)
pin arr@(MutableByteArray marr#) =
    if isPinned arr
    then return arr
    else IO
             $ \s# ->
                   case cloneMutableArrayWith# newPinnedByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, MutableByteArray marr1# #)

{-# INLINE unpin #-}
unpin :: MutableByteArray a -> IO (MutableByteArray a)
unpin arr@(MutableByteArray marr#) =
    if not (isPinned arr)
    then return arr
    else IO
             $ \s# ->
                   case cloneMutableArrayWith# newByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, MutableByteArray marr1# #)

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
readWord8ArrayAsWideChar# arr# i# =
    readWideCharArray# arr# (quotInt# i# SIZEOF_HSCHAR_PRIMITIVE)

{-# INLINE writeWord8ArrayAsWideChar# #-}
writeWord8ArrayAsWideChar# ::
       MutableByteArray# d -> Int# -> Char# -> State# d -> State# d
writeWord8ArrayAsWideChar# arr# i# =
    writeWideCharArray# arr# (quotInt# i# SIZEOF_HSCHAR_PRIMITIVE)

{-# INLINE readWord8ArrayAsInt# #-}
readWord8ArrayAsInt# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt# arr# i# =
    readIntArray# arr# (quotInt# i# SIZEOF_HSINT_PRIMITIVE)

{-# INLINE writeWord8ArrayAsInt# #-}
writeWord8ArrayAsInt# ::
       MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt# arr# i# =
    writeIntArray# arr# (quotInt# i# SIZEOF_HSINT_PRIMITIVE)

{-# INLINE readWord8ArrayAsInt32# #-}
readWord8ArrayAsInt32# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Int# #)
readWord8ArrayAsInt32# arr# i# =
    readInt32Array# arr# (quotInt# i# SIZEOF_INT32_PRIMITIVE)

{-# INLINE writeWord8ArrayAsInt32# #-}
writeWord8ArrayAsInt32# ::
       MutableByteArray# d -> Int# -> Int# -> State# d -> State# d
writeWord8ArrayAsInt32# arr# i# =
    writeInt32Array# arr# (quotInt# i# SIZEOF_INT32_PRIMITIVE)

{-# INLINE readWord8ArrayAsInt64# #-}
readWord8ArrayAsInt64# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, INT64TYP #)
readWord8ArrayAsInt64# arr# i# =
    readInt64Array# arr# (quotInt# i# SIZEOF_INT64_PRIMITIVE)

{-# INLINE writeWord8ArrayAsInt64# #-}
writeWord8ArrayAsInt64# ::
       MutableByteArray# d -> Int# -> INT64TYP -> State# d -> State# d
writeWord8ArrayAsInt64# arr# i# =
    writeInt64Array# arr# (quotInt# i# SIZEOF_INT64_PRIMITIVE)

{-# INLINE readWord8ArrayAsWord# #-}
readWord8ArrayAsWord# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)
readWord8ArrayAsWord# arr# i# =
    readWordArray# arr# (quotInt# i# SIZEOF_HSWORD_PRIMITIVE)

{-# INLINE writeWord8ArrayAsWord# #-}
writeWord8ArrayAsWord# ::
       MutableByteArray# d -> Int# -> Word# -> State# d -> State# d
writeWord8ArrayAsWord# arr# i# =
    writeWordArray# arr# (quotInt# i# SIZEOF_HSWORD_PRIMITIVE)

{-# INLINE readWord8ArrayAsWord64# #-}
readWord8ArrayAsWord64# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, WORD64TYP #)
readWord8ArrayAsWord64# arr# i#  =
    readWord64Array# arr# (quotInt# i# SIZEOF_WORD64_PRIMITIVE)

{-# INLINE writeWord8ArrayAsWord64# #-}
writeWord8ArrayAsWord64# ::
       MutableByteArray# d -> Int# -> WORD64TYP -> State# d -> State# d
writeWord8ArrayAsWord64# arr# i# =
    writeWord64Array# arr# (quotInt# i# SIZEOF_WORD64_PRIMITIVE)

{-# INLINE readWord8ArrayAsDouble# #-}
readWord8ArrayAsDouble# ::
       MutableByteArray# d -> Int# -> State# d -> (# State# d, Double# #)
readWord8ArrayAsDouble# arr# i# =
    readDoubleArray# arr# (quotInt# i# SIZEOF_HSDOUBLE_PRIMITIVE)

{-# INLINE writeWord8ArrayAsDouble# #-}
writeWord8ArrayAsDouble# ::
       MutableByteArray# d -> Int# -> Double# -> State# d -> State# d
writeWord8ArrayAsDouble# arr# i# =
    writeDoubleArray# arr# (quotInt# i# SIZEOF_HSDOUBLE_PRIMITIVE)

#endif

-- In theory we could convert a type to and from a byte stream and use that
-- to implement boxing, unboxing. But that would be inefficient. This type
-- class allows each primitive type to have its own specific efficient
-- implementation to read and write the type to memory.
--
-- This is a typeclass that uses MutableByteArray but could use ForeignPtr or
-- any other representation of memory. We could make this a multiparameter type
-- class if necessary.
--
-- If the type class would have to support reading and writing to a Ptr as well,
-- basically what Storable does. We will also need to touch the anchoring ptr at
-- the right points which is prone to errors. However, it should be simple to
-- implement unmanaged/read-only memory arrays by allowing a Ptr type in
-- ArrayContents, though it would require all instances to support reading from
-- a Ptr.
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
-- We have to keep the instances compatible with Storable as we depend on it for
-- size and alignment. We can possibly write tests for checking compatibility
-- with Storable.

-- | An 'Unbox' type supplies operations for reading and writing the type
-- from and to a byte array in memory. The read operation constructs the boxed
-- type from an unboxed byte representation in memory. The write operation
-- writes the boxed type to an unboxed byte representation in memory.
--
-- This type class dependes on Storable for size and alignment of the type. It
-- is similar to Storable in the sense that it has corresponding operations for
-- peekByteOff and pokeByteOff. While Storable allows writing to a Ptr, this
-- type class allows writing to a MutableByteArray#.
class Storable a => Unbox a where
    -- | Read an element of type "a" from a MutableByteArray given the byte
    -- index.
    box :: MutableByteArray a -> Int -> IO a
    -- | Write an element of type "a" to a MutableByteArray given the byte
    -- index.
    unbox :: MutableByteArray a -> Int -> a -> IO ()

type Unboxed a = (Unbox a, Storable a)

#define DERIVE_UNBOXED(_type, _constructor, _readArray, _writeArray) \
instance Unbox _type where {                                         \
; {-# INLINE box #-}                                                 \
; box (MutableByteArray mbarr) (I# n) = IO $ \s ->                      \
      case _readArray mbarr n s of                                   \
          { (# s1, i #) -> (# s1, _constructor i #) }                \
; {-# INLINE unbox #-}                                               \
; unbox (MutableByteArray mbarr) (I# n) (_constructor val) =            \
        IO $ \s -> (# _writeArray mbarr n val s, () #)               \
}

DERIVE_UNBOXED( Char
              , C#
              , readWord8ArrayAsWideChar#
              , writeWord8ArrayAsWideChar#)

DERIVE_UNBOXED( Int32
              , I32#
              , readWord8ArrayAsInt32#
              , writeWord8ArrayAsInt32#)

DERIVE_UNBOXED( Int
              , I#
              , readWord8ArrayAsInt#
              , writeWord8ArrayAsInt#)

DERIVE_UNBOXED( Int64
              , I64#
              , readWord8ArrayAsInt64#
              , writeWord8ArrayAsInt64#)

DERIVE_UNBOXED( Word
              , W#
              , readWord8ArrayAsWord#
              , writeWord8ArrayAsWord#)

DERIVE_UNBOXED( Word8
              , W8#
              , readWord8Array#
              , writeWord8Array#)

DERIVE_UNBOXED( Word64
              , W64#
              , readWord8ArrayAsWord64#
              , writeWord8ArrayAsWord64#)

DERIVE_UNBOXED( Double
              , D#
              , readWord8ArrayAsDouble#
              , writeWord8ArrayAsDouble#)

instance Unbox Bool where

    {-# INLINE box #-}
    box arr i = do
        res <- box (castContents arr) i
        return $ res /= (0 :: Int32)

    {-# INLINE unbox #-}
    unbox arr i a =
        if a
        then unbox (castContents arr) i (1 :: Int32)
        else unbox (castContents arr) i (0 :: Int32)

instance forall a. Unboxed a => Unbox (Complex a) where

    {-# INLINE box #-}
    box arr i = do
        let contents = castContents arr :: MutableByteArray a
        real <- box contents i
        img <- box contents (i + SIZE_OF(a))
        return $ real :+ img

    {-# INLINE unbox #-}
    unbox arr i (real :+ img) = do
        let contents = castContents arr :: MutableByteArray a
        unbox contents i real
        unbox contents (i + SIZE_OF(a)) img

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{-# INLINE peekWith #-}
peekWith :: Unboxed a => MutableByteArray a -> Int -> IO a
peekWith = box

{-# INLINE pokeWith #-}
pokeWith :: Unboxed a => MutableByteArray a -> Int -> a -> IO ()
pokeWith = unbox
