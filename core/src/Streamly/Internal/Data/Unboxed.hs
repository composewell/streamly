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
    , newUnpinnedBytes
    , newAlignedPinnedBytes
    , nil
    ) where

#include "MachDeps.h"

#include "ArrayMacros.h"

import Data.Complex (Complex((:+)))
import Data.Functor ((<&>))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Foreign.Ptr (IntPtr(..), WordPtr(..))
import Foreign.Storable (Storable(..))
import GHC.Base (IO(..))
import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Real (Ratio(..))
import GHC.Stable (StablePtr(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
#if MIN_VERSION_base(4,15,0)
import GHC.RTS.Flags (IoSubSystem(..))
#endif
import System.IO.Unsafe (unsafePerformIO)

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

{-# NOINLINE nil #-}
nil :: MutableByteArray a
nil = unsafePerformIO $ newUnpinnedBytes 0

{-# INLINE newUnpinnedBytes #-}
newUnpinnedBytes :: Int -> IO (MutableByteArray a)
newUnpinnedBytes nbytes | nbytes < 0 =
  errorWithoutStackTrace "newUnpinnedBytes: size must be >= 0"
newUnpinnedBytes (I# nbytes) = IO $ \s ->
    case newByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = MutableByteArray mbarr#
            in (# s', c #)

{-# INLINE newAlignedPinnedBytes #-}
newAlignedPinnedBytes :: Int -> Int -> IO (MutableByteArray a)
newAlignedPinnedBytes nbytes _align | nbytes < 0 =
  errorWithoutStackTrace "newAlignedPinnedBytes: size must be >= 0"
newAlignedPinnedBytes (I# nbytes) (I# align) = IO $ \s ->
    case newAlignedPinnedByteArray# nbytes align s of
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
; box (MutableByteArray mbarr) (I# n) = IO $ \s ->                   \
      case _readArray mbarr n s of                                   \
          { (# s1, i #) -> (# s1, _constructor i #) }                \
; {-# INLINE unbox #-}                                               \
; unbox (MutableByteArray mbarr) (I# n) (_constructor val) =         \
        IO $ \s -> (# _writeArray mbarr n val s, () #)               \
}

#define DERIVE_WRAPPED_UNBOX(_constraint, _type, _constructor) \
instance _constraint Unbox _type where                        \
; {-# INLINE box #-}                                          \
; box arr i = _constructor <$> box (castContents arr) i       \
; {-# INLINE unbox #-}                                        \
; unbox arr i (_constructor a) = unbox (castContents arr) i a

#define DERIVE_BINARY_UNBOX(_constraint, _type, _constructor, _innerType) \
instance _constraint Unbox _type where {                                  \
; {-# INLINE box #-}                                                      \
; box arr i =                                                             \
   let contents = castContents arr :: MutableByteArray _innerType         \
    in box contents i >>=                                                 \
         (\p1 -> box contents (i + SIZE_OF(_innerType))                   \
             <&> _constructor p1)                                         \
; {-# INLINE unbox #-}                                                    \
; unbox arr i (_constructor p1 p2) =                                      \
   let contents = castContents arr :: MutableByteArray _innerType         \
    in unbox contents i p1 >>                                             \
       unbox contents (i + SIZE_OF(_innerType)) p2                        \
}

DERIVE_UNBOXED( Char
              , C#
              , readWord8ArrayAsWideChar#
              , writeWord8ArrayAsWideChar#)

DERIVE_UNBOXED( Int8
              , I8#
              , readInt8Array#
              , writeInt8Array#)

DERIVE_UNBOXED( Int16
              , I16#
              , readWord8ArrayAsInt16#
              , writeWord8ArrayAsInt16#)

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

DERIVE_UNBOXED( Word16
              , W16#
              , readWord8ArrayAsWord16#
              , writeWord8ArrayAsWord16#)

DERIVE_UNBOXED( Word32
              , W32#
              , readWord8ArrayAsWord32#
              , writeWord8ArrayAsWord32#)

DERIVE_UNBOXED( Word64
              , W64#
              , readWord8ArrayAsWord64#
              , writeWord8ArrayAsWord64#)

DERIVE_UNBOXED( Double
              , D#
              , readWord8ArrayAsDouble#
              , writeWord8ArrayAsDouble#)

DERIVE_UNBOXED( Float
              , F#
              , readWord8ArrayAsFloat#
              , writeWord8ArrayAsFloat#)

DERIVE_UNBOXED( (StablePtr a)
              , StablePtr
              , readWord8ArrayAsStablePtr#
              , writeWord8ArrayAsStablePtr#)

DERIVE_UNBOXED( (Ptr a)
              , Ptr
              , readWord8ArrayAsAddr#
              , writeWord8ArrayAsAddr#)

DERIVE_UNBOXED( (FunPtr a)
              , FunPtr
              , readWord8ArrayAsAddr#
              , writeWord8ArrayAsAddr#)

DERIVE_WRAPPED_UNBOX(,IntPtr,IntPtr)
DERIVE_WRAPPED_UNBOX(,WordPtr,WordPtr)
DERIVE_WRAPPED_UNBOX(Unbox a =>,(Identity a),Identity)
#if MIN_VERSION_base(4,14,0)
DERIVE_WRAPPED_UNBOX(Unbox a =>,(Down a),Down)
#endif
DERIVE_WRAPPED_UNBOX(Unbox a =>,(Const a b),Const)
DERIVE_BINARY_UNBOX(forall a. Unboxed a =>,(Complex a),(:+),a)
DERIVE_BINARY_UNBOX(forall a. (Integral a, Unboxed a) =>,(Ratio a),(:%),a)
DERIVE_BINARY_UNBOX(,Fingerprint,Fingerprint,Word64)

instance Unbox () where

    {-# INLINE box #-}
    box _ _ = return ()

    {-# INLINE unbox #-}
    unbox _ _ _ = return ()

#if MIN_VERSION_base(4,15,0)
instance Unbox IoSubSystem where

    {-# INLINE box #-}
    box arr i = toEnum <$> box (castContents arr) i

    {-# INLINE unbox #-}
    unbox arr i a = unbox (castContents arr) i (fromEnum a)
#endif

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

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{-# INLINE peekWith #-}
peekWith :: Unboxed a => MutableByteArray a -> Int -> IO a
peekWith = box

{-# INLINE pokeWith #-}
pokeWith :: Unboxed a => MutableByteArray a -> Int -> a -> IO ()
pokeWith = unbox
