{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Streamly.Internal.Data.Unboxed
    ( Storable
    , alignment
    , peek
    , poke
    , peekWith
    , pokeWith
    , sizeOf
    , ArrayContents(..)
    , castContents
    , touch
    , getInternalMutableByteArray
    ) where

#ifndef USE_STORABLE
#include "MachDeps.h"
#endif

#include "ArrayMacros.h"

import GHC.Exts

import GHC.Base (IO(..))

--------------------------------------------------------------------------------
-- Imports for Unboxed
--------------------------------------------------------------------------------

#ifndef USE_STORABLE
import GHC.Word (Word8(..), Word64(..))
import Foreign.Ptr (castPtr)
#endif

--------------------------------------------------------------------------------
-- Imports for Storable
--------------------------------------------------------------------------------

#ifdef USE_STORABLE
import Foreign.Storable (Storable(..))
#endif

--------------------------------------------------------------------------------
-- The ArrayContents type
--------------------------------------------------------------------------------

#ifdef USE_FOREIGN_PTR
newtype ArrayContents a = ArrayContents Addr# ForeignPtrContents
#define UNPACKIF
#else
-- XXX can use UnliftedNewtypes
data ArrayContents a = ArrayContents !(MutableByteArray# RealWorld)

-- XXX rename to getMutableByteArray#
{-# INLINE getInternalMutableByteArray #-}
getInternalMutableByteArray :: ArrayContents a -> MutableByteArray# RealWorld
getInternalMutableByteArray (ArrayContents mbarr) = mbarr

{-# INLINE castContents #-}
castContents :: ArrayContents a -> ArrayContents b
castContents (ArrayContents mbarr) = ArrayContents mbarr

{-# INLINE touch #-}
touch :: ArrayContents a -> IO ()
touch (ArrayContents contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

#define UNPACKIF {-# UNPACK #-}
#endif

--------------------------------------------------------------------------------
-- The Unboxed type class
--------------------------------------------------------------------------------

#ifndef USE_STORABLE

-- XXX This is a superset of Storable. Put a Storable constraint on this
-- and remove sizeOf, alignment, readOffPtr, writeOffPtr from this. Just keep
-- the bytearray reading/writing functions.

-- | Keep the instances compatible with Storable.
class Unboxed a where
    sizeOf :: a -> Int
    alignment :: a -> Int

    -- Read an element of type "a" from a MutableByteArray given the element
    -- index.
    readByteArray :: ArrayContents a -> Int -> IO a
    writeByteArray :: ArrayContents a -> Int -> a -> IO ()

    -- Read an element of type "a" from a Ptr given the element index.
    readOffPtr :: Ptr a -> Int -> IO a
    writeOffPtr :: Ptr a -> Int -> a -> IO ()

#define DERIVE_UNBOXED(_type, _constructor, _sizeOf, _alignment, _readArray, _writeArray, _readAddr, _writeAddr) \
instance Unboxed _type where {                                     \
  {-# INLINE sizeOf #-}                                            \
; sizeOf _ = _sizeOf                                               \
; {-# INLINE alignment #-}                                         \
; alignment _ = _alignment                                         \
; {-# INLINE readByteArray #-}                                     \
; readByteArray (ArrayContents mbarr) (I# n) = IO $ \s ->          \
      case _readArray mbarr n s of                                 \
          { (# s1, i #) -> (# s1, _constructor i #) }              \
; {-# INLINE writeByteArray #-}                                    \
; writeByteArray (ArrayContents mbarr) (I# n) (_constructor val) = \
        IO $ \s -> (# _writeArray mbarr n val s, () #)             \
; {-# INLINE readOffPtr #-}                                        \
; readOffPtr (Ptr addr) (I# n) = IO $ \s ->                        \
      case _readAddr addr n s of                                   \
          { (# s1, i #) -> (# s1, _constructor i #) }              \
; {-# INLINE writeOffPtr #-}                                       \
; writeOffPtr (Ptr addr) (I# n) (_constructor val) =               \
     IO $ \s -> (# _writeAddr addr n val s, () #)                  \
}

-- XXX Why not always use a Word8 mutable byte array and use byte offsets to
-- read and write using readWord8ArrayAsInt# etc. Is there an advantage of
-- using readIntArray# vs readWord8ArrayAsInt#. If we use a Word8 array we can
-- control the alignment and casts. It will be possible to cast a misaligned
-- slice. There seem to be no performance advantage of alignment anymore, seee
-- https://lemire.me/blog/2012/05/31/data-alignment-for-speed-myth-or-reality/
-- . We can also check if GHC's implementation of the two APIs makes any
-- difference.

DERIVE_UNBOXED( Char
              , C#
              , SIZEOF_HSCHAR
              , ALIGNMENT_HSCHAR
              , readWideCharArray#
              , writeWideCharArray#
              , readWideCharOffAddr#
              , writeWideCharOffAddr#)

DERIVE_UNBOXED( Int
              , I#
              , SIZEOF_HSINT
              , ALIGNMENT_HSINT
              , readIntArray#
              , writeIntArray#
              , readIntOffAddr#
              , writeIntOffAddr#)

DERIVE_UNBOXED( Word
              , W#
              , SIZEOF_HSWORD
              , ALIGNMENT_HSWORD
              , readWordArray#
              , writeWordArray#
              , readWordOffAddr#
              , writeWordOffAddr#)

DERIVE_UNBOXED( Word8
              , W8#
              , SIZEOF_WORD8
              , ALIGNMENT_WORD8
              , readWord8Array#
              , writeWord8Array#
              , readWord8OffAddr#
              , writeWord8OffAddr#)

DERIVE_UNBOXED( Word64
              , W64#
              , SIZEOF_WORD64
              , ALIGNMENT_WORD64
              , readWord64Array#
              , writeWord64Array#
              , readWord64OffAddr#
              , writeWord64OffAddr#)

DERIVE_UNBOXED( Double
              , D#
              , SIZEOF_HSDOUBLE
              , ALIGNMENT_HSDOUBLE
              , readDoubleArray#
              , writeDoubleArray#
              , readDoubleOffAddr#
              , writeDoubleOffAddr#)

instance Unboxed Bool where
    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (undefined :: Int)
    {-# INLINE alignment #-}
    alignment _ = alignment (undefined :: Int)
    {-# INLINE readByteArray #-}
    readByteArray arr i = do
        res <- readByteArray (castContents arr) i
        return $ res /= (0 :: Int)
    {-# INLINE writeByteArray #-}
    writeByteArray arr i a =
        case a of
            True -> writeByteArray (castContents arr) i (1 :: Int)
            False -> writeByteArray (castContents arr) i (0 :: Int)
    {-# INLINE readOffPtr #-}
    readOffPtr ptr i = do
        res <- readOffPtr (castPtr ptr) i
        return $ res /= (0 :: Int)
    {-# INLINE writeOffPtr #-}
    writeOffPtr ptr i a =
        case a of
            True -> writeOffPtr (castPtr ptr) i (1 :: Int)
            False -> writeOffPtr (castPtr ptr) i (0 :: Int)
#endif

--------------------------------------------------------------------------------
-- Functions for Unboxed
--------------------------------------------------------------------------------

#ifndef USE_STORABLE

type Storable = Unboxed

{-# INLINE peek #-}
peek :: Unboxed a => Ptr a -> IO a
peek ptr = readOffPtr ptr 0

{-# INLINE poke #-}
poke :: Unboxed a => Ptr a -> a -> IO ()
poke ptr = writeOffPtr ptr 0

{-# INLINE peekWith #-}
peekWith :: Unboxed a => ArrayContents a -> Int -> IO a
peekWith = readByteArray

{-# INLINE pokeWith #-}
pokeWith :: Unboxed a => ArrayContents a -> Int -> a -> IO ()
pokeWith = writeByteArray

#else

--------------------------------------------------------------------------------
-- Functions for Storable
--------------------------------------------------------------------------------

{-# INLINE peekWith #-}
peekWith :: Storable a => ArrayContents a -> Int -> IO a
peekWith contents i =
    let !ptr =
            Ptr
                (byteArrayContents#
                     (unsafeCoerce# (getInternalMutableByteArray contents)))
     in do
       r <- peekElemOff ptr i
       touch contents
       return r

{-# INLINE pokeWith #-}
pokeWith :: Storable a => ArrayContents a -> Int -> a -> IO ()
pokeWith contents i a =
    let !ptr =
            Ptr
                (byteArrayContents#
                     (unsafeCoerce# (getInternalMutableByteArray contents)))
     in pokeElemOff ptr i a >> touch contents

#endif
