{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE FlexibleContexts          #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Prim.Pinned.Mutable.Array.Types
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Prim.Pinned.Mutable.Array.Types
    (
      Array (..)

    -- * Construction
    , newArray
    , newAlignedArray
    , writeArray

    , spliceTwo
    , unsafeCopy

    , fromListM
    , fromListNM
    , fromStreamDN
    , fromStreamD

    -- * Streams of arrays
    , fromStreamDArraysOf

    , packArraysChunksOf
    , lpackArraysChunksOf

#if !defined(mingw32_HOST_OS)
--    , groupIOVecsOf
#endif

    -- * Elimination
    , unsafeIndexM
    , length
    , byteLength

    , writeN
    , writeNAligned
    , write

    -- * Utilities
    , resizeArray
    , shrinkArray

    , fPlainPtrToW8Array
    , touchArray
    , withArrayAsPtr
    )
where

import Data.Word (Word8)
import GHC.ForeignPtr
import GHC.IO (IO(..))

#include "mutable-prim-array-types.hs"

-------------------------------------------------------------------------------
-- Allocation (Pinned)
-------------------------------------------------------------------------------

-- XXX we can use a single newArray routine which accepts an allocation
-- function which could be newByteArray#, newPinnedByteArray# or
-- newAlignedPinnedByteArray#. That function can go in the common include file.
--
-- | Allocate an array that is pinned and can hold 'count' items.  The memory of
-- the array is uninitialized.
--
-- Note that this is internal routine, the reference to this array cannot be
-- given out until the array has been written to and frozen.
{-# INLINE newArray #-}
newArray ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> m (Array (PrimState m) a)
newArray (I# n#) =
    primitive $ \s# ->
        let bytes = n# *# sizeOf# (undefined :: a)
         in case newPinnedByteArray# bytes s# of
            (# s1#, arr# #) -> (# s1#, Array arr# #)

-- Change order of args?
-- | Allocate a new array aligned to the specified alignment and using pinned
-- memory.
{-# INLINE newAlignedArray #-}
newAlignedArray ::
       forall m a. (PrimMonad m, Prim a)
    => Int -- size
    -> Int -- Alignment
    -> m (Array (PrimState m) a)
newAlignedArray (I# n#) (I# a#) =
    primitive $ \s# ->
        let bytes = n# *# sizeOf# (undefined :: a)
         in case newAlignedPinnedByteArray# bytes a# s# of
            (# s1#, arr# #) -> (# s1#, Array arr# #)

-- | Resize (pinned) mutable byte array to new specified size (in elem
-- count). The returned array is either the original array resized in-place or,
-- if not possible, a newly allocated (pinned) array (with the original content
-- copied over).
{-# INLINE resizeArray #-}
resizeArray ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a
    -> Int -- ^ new size
    -> m (Array (PrimState m) a)
resizeArray arr i =
    if len == i
    then return arr
    else if i < len
         then shrinkArray arr i >> return arr
         else do
             nArr <- newArray i
             unsafeCopy nArr 0 arr 0 len
             return nArr

    where

    len = length arr

-------------------------------------------------------------------------------
-- Aligned Construction
-------------------------------------------------------------------------------

-- XXX we can also factor out common code in writeN and writeNAligned in the
-- same way as suggested above.
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> Int
    -> Fold m a (Array (PrimState m) a)
writeNAligned align limit = Fold step initial extract

    where

    initial = do
        marr <- newAlignedArray limit align
        return (marr, 0)

    step (marr, i) x
        | i == limit = return (marr, i)
        | otherwise = do
            writeArray marr i x
            return (marr, i + 1)

    extract (marr, len) = shrinkArray marr len >> return marr

-------------------------------------------------------------------------------
-- Mutation with pointers
-------------------------------------------------------------------------------

-- XXX This section can probably go in a common include file for pinned arrays.

-- Change name later.
{-# INLINE toPtr #-}
toPtr :: Array s a -> Ptr a
toPtr (Array arr#) = Ptr (byteArrayContents# (unsafeCoerce# arr#))

-- XXX remove this?
fPlainPtrToW8Array :: ForeignPtr a -> Array RealWorld Word8
fPlainPtrToW8Array (ForeignPtr _ (PlainPtr mb)) = Array mb
fPlainPtrToW8Array _ =
    error "fPlainPtrToW8Array can only be used when the ForeignPtr does not have finalizers."

{-# INLINE touchArray #-}
touchArray :: Array s a -> IO ()
touchArray arr = IO $ \s -> case touch# arr s of s' -> (# s', () #)

{-# INLINE withArrayAsPtr #-}
withArrayAsPtr :: Array s a -> (Ptr a -> IO b) -> IO b
withArrayAsPtr arr f = do
    r <- f (toPtr arr)
    touchArray arr
    return r
