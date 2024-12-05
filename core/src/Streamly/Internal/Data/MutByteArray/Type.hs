{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Data.MutByteArray.Type
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.MutByteArray.Type
    (
    -- ** MutByteArray
      MutByteArray(..)
    , getMutByteArray#

    -- ** Pinning
    , PinnedState(..)
    , isPinned
    , pin
    , unpin

    -- ** Allocation
    , empty
    , newAs
    , new
    , pinnedNew
    , reallocSliceAs

    -- ** Access
    , length
    , unsafePutSlice
    , unsafeCloneSliceAs
    , unsafeCloneSlice
    , unsafePinnedCloneSlice
    , unsafePinnedAsPtr
    , unsafeAsPtr

    -- ** Deprecated
    , MutableByteArray
    , getMutableByteArray#
    , newBytesAs
    , sizeOfMutableByteArray
    , putSliceUnsafe
    , cloneSliceUnsafeAs
    , cloneSliceUnsafe
    , pinnedCloneSliceUnsafe
    , pinnedNewAlignedBytes
    , asPtrUnsafe
    , nil
    ) where

#include "deprecation.h"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
#ifdef DEBUG
import Debug.Trace (trace)
#endif
import GHC.Base (IO(..))
import System.IO.Unsafe (unsafePerformIO)

import GHC.Exts
import Prelude hiding (length)

--------------------------------------------------------------------------------
-- The ArrayContents type
--------------------------------------------------------------------------------

data PinnedState
    = Pinned
    | Unpinned deriving (Show, Eq)

-- XXX can use UnliftedNewtypes

-- | A lifted mutable byte array type wrapping @MutableByteArray# RealWorld@.
-- This is a low level array used to back high level unboxed arrays and
-- serialized data.
data MutByteArray = MutByteArray (MutableByteArray# RealWorld)

{-# DEPRECATED MutableByteArray "Please use MutByteArray instead" #-}
type MutableByteArray = MutByteArray

{-# INLINE getMutByteArray# #-}
getMutableByteArray#, getMutByteArray# :: MutByteArray -> MutableByteArray# RealWorld
getMutByteArray# (MutByteArray mbarr) = mbarr

-- | Return the size of the array in bytes.
{-# INLINE length #-}
sizeOfMutableByteArray, length :: MutByteArray -> IO Int
length (MutByteArray arr) =
    IO $ \s ->
        case getSizeofMutableByteArray# arr s of
            (# s1, i #) -> (# s1, I# i #)

{-# INLINE touch #-}
touch :: MutByteArray -> IO ()
touch (MutByteArray contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

-- XXX Some functions in this module are "IO" and others are "m", we need to
-- make it consistent.

-- | NOTE: this is deprecated because it can lead to accidental problems if the
-- user tries to use it to mutate the array because it does not return the new
-- array after pinning.
{-# DEPRECATED unsafePinnedAsPtr "Pin the array and then use unsafeAsPtr." #-}
{-# INLINE unsafePinnedAsPtr #-}
unsafePinnedAsPtr :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
unsafePinnedAsPtr arr f = do
    arr1 <- liftIO $ pin arr
    unsafeAsPtr arr1 f

{-# DEPRECATED asPtrUnsafe "Pin the array and then use unsafeAsPtr." #-}
{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
asPtrUnsafe = unsafePinnedAsPtr

-- | Use a @MutByteArray@ as @Ptr a@. This is useful when we want to pass
-- an array as a pointer to some operating system call or to a "safe" FFI call.
--
-- /Unsafe/ WARNING:
--
-- 1. Will lead to memory corruption if the array is not pinned. Use
-- only if the array is known to be pinned already or pin it explicitly.
--
-- 2. Ensure that the pointer is accessed within the legal bounds of the array.
-- The size of the MutByteArray must be taken into account.
--
-- /Pre-release/
--
{-# INLINE unsafeAsPtr #-}
unsafeAsPtr :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
unsafeAsPtr arr f = do
    when (not (isPinned arr))
        $ error "unsafeAsPtr requires the array to be pinned"

    let !ptr = Ptr (byteArrayContents#
                     (unsafeCoerce# (getMutByteArray# arr)))
    r <- f ptr
    -- While f is using the bare pointer, the MutByteArray may be garbage
    -- collected by the GC, tell the GC that we are still using it.
    liftIO $ touch arr
    return r

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

{-# NOINLINE empty #-}
empty :: MutByteArray
empty = unsafePerformIO $ new 0

{-# DEPRECATED nil "Please use empty instead" #-}
nil :: MutByteArray
nil = empty

-- XXX add "newRoundedUp" to round up the large size to the next page boundary
-- and return the allocated size.
{-# INLINE new #-}
new :: Int -> IO MutByteArray
new nbytes | nbytes < 0 =
  errorWithoutStackTrace "newByteArray: size must be >= 0"
new (I# nbytes) = IO $ \s ->
    case newByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = MutByteArray mbarr#
            in (# s', c #)

{-# INLINE pinnedNew #-}
pinnedNew :: Int -> IO MutByteArray
pinnedNew nbytes | nbytes < 0 =
  errorWithoutStackTrace "pinnedNew: size must be >= 0"
pinnedNew (I# nbytes) = IO $ \s ->
    case newPinnedByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = MutByteArray mbarr#
            in (# s', c #)

{-# DEPRECATED pinnedNewAlignedBytes "Please use pinnedNew instead" #-}
{-# INLINE pinnedNewAlignedBytes #-}
pinnedNewAlignedBytes :: Int -> Int -> IO MutByteArray
pinnedNewAlignedBytes nbytes _align | nbytes < 0 =
  errorWithoutStackTrace "pinnedNewAlignedBytes: size must be >= 0"
pinnedNewAlignedBytes (I# nbytes) (I# align) = IO $ \s ->
    case newAlignedPinnedByteArray# nbytes align s of
        (# s', mbarr# #) ->
           let c = MutByteArray mbarr#
            in (# s', c #)

{-# INLINE newAs #-}
newBytesAs, newAs :: PinnedState -> Int -> IO MutByteArray
newAs Unpinned = new
newAs Pinned = pinnedNew

-- | @reallocSliceAs pinType newLen array offset len@ reallocates a slice
-- from @array@ starting at @offset@ and having length @len@ to a new array of
-- length @newLen@ copying the old data to the new. Note that if the @newLen@
-- is smaller than @len@ it will truncate the old data.
{-# INLINE reallocSliceAs #-}
reallocSliceAs ::
    PinnedState -> Int -> MutByteArray -> Int -> Int -> IO MutByteArray
reallocSliceAs ps newLen (MutByteArray src#) srcStart srcLen = do
    MutByteArray dst# <- newBytesAs ps newLen

    -- Copy old data
    let !(I# srcStart#) = srcStart
        !(I# newLen#) = min srcLen newLen
    IO $ \s# -> (# copyMutableByteArray# src# srcStart#
                        dst# 0# newLen# s#, MutByteArray dst# #)

-------------------------------------------------------------------------------
-- Copying
-------------------------------------------------------------------------------

-- Note: Array copy is more efficient than streaming copy.
-- CopyMutableByteArray# translates to genMemcpy in GHC/CmmToAsm/X86/CodeGen.hs
-- glibc memcpy copies bytes/words/pages - unrolls the loops:
-- https://github.com/bminor/glibc/blob/4290aed05135ae4c0272006442d147f2155e70d7/string/memcpy.c
-- https://github.com/bminor/glibc/blob/4290aed05135ae4c0272006442d147f2155e70d7/string/wordcopy.c

-- | Put a sub range of a source array into a subrange of a destination array.
-- This is not safe as it does not check the bounds of neither the src array
-- nor the destination array.
{-# INLINE unsafePutSlice #-}
putSliceUnsafe, unsafePutSlice ::
       MonadIO m
    => MutByteArray
    -> Int
    -> MutByteArray
    -> Int
    -> Int
    -> m ()
unsafePutSlice src srcStartBytes dst dstStartBytes lenBytes = liftIO $ do
#ifdef DEBUG
    srcLen <- length src
    dstLen <- length dst
    when (srcLen - srcStartBytes < lenBytes)
        $ error $ "unsafePutSlice: src overflow: start" ++ show srcStartBytes
            ++ " end " ++ show srcLen ++ " len " ++ show lenBytes
    when (dstLen - dstStartBytes < lenBytes)
        $ error $ "unsafePutSlice: dst overflow: start" ++ show dstStartBytes
            ++ " end " ++ show dstLen ++ " len " ++ show lenBytes
#endif
    let !(I# srcStartBytes#) = srcStartBytes
        !(I# dstStartBytes#) = dstStartBytes
        !(I# lenBytes#) = lenBytes
    let arrS# = getMutByteArray# src
        arrD# = getMutByteArray# dst
    IO $ \s# -> (# copyMutableByteArray#
                    arrS# srcStartBytes# arrD# dstStartBytes# lenBytes# s#
                , () #)

-- | Unsafe as it does not check whether the start offset and length supplied
-- are valid inside the array.
{-# INLINE unsafeCloneSliceAs #-}
cloneSliceUnsafeAs, unsafeCloneSliceAs :: MonadIO m =>
    PinnedState -> Int -> Int -> MutByteArray -> m MutByteArray
unsafeCloneSliceAs ps srcOff srcLen src =
    liftIO $ do
        mba <- newAs ps srcLen
        unsafePutSlice src srcOff mba 0 srcLen
        return mba

-- | @unsafeCloneSlice offset len arr@ clones a slice of the supplied array
-- starting at the given offset and equal to the given length.
{-# INLINE unsafeCloneSlice #-}
cloneSliceUnsafe, unsafeCloneSlice :: MonadIO m => Int -> Int -> MutByteArray -> m MutByteArray
unsafeCloneSlice = unsafeCloneSliceAs Unpinned

-- | @unsafePinnedCloneSlice offset len arr@
{-# INLINE unsafePinnedCloneSlice #-}
pinnedCloneSliceUnsafe, unsafePinnedCloneSlice :: MonadIO m =>
    Int -> Int -> MutByteArray -> m MutByteArray
unsafePinnedCloneSlice = unsafeCloneSliceAs Pinned

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

-- | Return 'True' if the array is allocated in pinned memory.
{-# INLINE isPinned #-}
isPinned :: MutByteArray -> Bool
isPinned (MutByteArray arr#) =
    let pinnedInt = I# (isMutableByteArrayPinned# arr#)
     in pinnedInt /= 0


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

-- | Return a copy of the array in pinned memory if unpinned, else return the
-- original array.
{-# INLINE pin #-}
pin :: MutByteArray -> IO MutByteArray
pin arr@(MutByteArray marr#) =
    if isPinned arr
    then return arr
    else
#ifdef DEBUG
      do
        -- XXX dump stack trace
        trace ("pin: Copying array") (return ())
#endif
        IO
             $ \s# ->
                   case cloneMutableArrayWith# newPinnedByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, MutByteArray marr1# #)

-- | Return a copy of the array in unpinned memory if pinned, else return the
-- original array.
{-# INLINE unpin #-}
unpin :: MutByteArray -> IO MutByteArray
unpin arr@(MutByteArray marr#) =
    if not (isPinned arr)
    then return arr
    else
#ifdef DEBUG
      do
        -- XXX dump stack trace
        trace ("unpin: Copying array") (return ())
#endif
        IO
             $ \s# ->
                   case cloneMutableArrayWith# newByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, MutByteArray marr1# #)

--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------

RENAME(getMutableByteArray#, getMutByteArray#)
RENAME(newBytesAs, newAs)
RENAME(sizeOfMutableByteArray, length)
RENAME(putSliceUnsafe, unsafePutSlice)
RENAME(cloneSliceUnsafeAs, unsafeCloneSliceAs)
RENAME(cloneSliceUnsafe, unsafeCloneSlice)
RENAME(pinnedCloneSliceUnsafe, unsafePinnedCloneSlice)
