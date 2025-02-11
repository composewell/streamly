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
    , getMutableByteArray# -- XXX getMutByteArray#

    -- ** Pinning
    , PinnedState(..)
    , isPinned
    , pin
    , unpin

    -- ** Allocation
    , empty
    , newBytesAs -- XXX should be removed
    , new
    , pinnedNew
    , pinnedNewAlignedBytes -- XXX should be removed

    -- ** Access
    , sizeOfMutableByteArray -- XXX length
    , putSliceUnsafe
    , cloneSliceUnsafeAs
    , cloneSliceUnsafe
    , pinnedCloneSliceUnsafe
    , unsafePinnedAsPtr
    , unsafeAsPtr

    -- ** Deprecated
    , MutableByteArray
    , asPtrUnsafe
    , nil
    ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
#ifdef DEBUG
import Debug.Trace (trace)
#endif
import GHC.Base (IO(..))
import System.IO.Unsafe (unsafePerformIO)

import GHC.Exts

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

{-# INLINE getMutableByteArray# #-}
getMutableByteArray# :: MutByteArray -> MutableByteArray# RealWorld
getMutableByteArray# (MutByteArray mbarr) = mbarr

-- | Return the size of the array in bytes.
{-# INLINE sizeOfMutableByteArray #-}
sizeOfMutableByteArray :: MutByteArray -> IO Int
sizeOfMutableByteArray (MutByteArray arr) =
    IO $ \s ->
        case getSizeofMutableByteArray# arr s of
            (# s1, i #) -> (# s1, I# i #)

{-# INLINE touch #-}
touch :: MutByteArray -> IO ()
touch (MutByteArray contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

-- XXX Some functions in this module are "IO" and others are "m", we need to
-- make it consistent.

-- XXX We can provide another API for "unsafe" FFI calls passing an unlifted
-- pointer to the FFI call. For unsafe calls we do not need to pin the array.
-- We can pass an unlifted pointer to the FFI routine to avoid GC kicking in
-- before the pointer is wrapped.
--
-- From the GHC manual:
--
-- GHC, since version 8.4, guarantees that garbage collection will never occur
-- during an unsafe call, even in the bytecode interpreter, and further
-- guarantees that unsafe calls will be performed in the calling thread. Making
-- it safe to pass heap-allocated objects to unsafe functions.

-- | NOTE: this is deprecated because it can lead to accidental problems if the
-- user tries to use it to mutate the array because it does not return the new
-- array after pinning.
{-# DEPRECATED unsafePinnedAsPtr "Pin the array and then use unsafeAsPtr." #-}
{-# INLINE unsafePinnedAsPtr #-}
unsafePinnedAsPtr :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
unsafePinnedAsPtr arr f = do
    arr1 <- liftIO $ pin arr
    unsafeAsPtr arr1 f

{-# DEPRECATED asPtrUnsafe "Please use unsafePinnedAsPtr instead." #-}
{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
asPtrUnsafe = unsafePinnedAsPtr

-- | For use with unsafe FFI functions. Does not force pin the array memory.
{-# INLINE unsafeAsPtr #-}
unsafeAsPtr :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
unsafeAsPtr arr f = do
    when (not (isPinned arr))
        $ error "unsafeAsPtr requires the array to be pinned"
    let !ptr = Ptr (byteArrayContents#
                     (unsafeCoerce# (getMutableByteArray# arr)))
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

-- XXX add "newRounded" to round up the large size to the next page boundary
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

{-# INLINE pinnedNewAlignedBytes #-}
pinnedNewAlignedBytes :: Int -> Int -> IO MutByteArray
pinnedNewAlignedBytes nbytes _align | nbytes < 0 =
  errorWithoutStackTrace "pinnedNewAlignedBytes: size must be >= 0"
pinnedNewAlignedBytes (I# nbytes) (I# align) = IO $ \s ->
    case newAlignedPinnedByteArray# nbytes align s of
        (# s', mbarr# #) ->
           let c = MutByteArray mbarr#
            in (# s', c #)

{-# INLINE newBytesAs #-}
newBytesAs :: PinnedState -> Int -> IO MutByteArray
newBytesAs Unpinned = new
newBytesAs Pinned = pinnedNew

-------------------------------------------------------------------------------
-- Copying
-------------------------------------------------------------------------------

-- | Put a sub range of a source array into a subrange of a destination array.
-- This is not safe as it does not check the bounds of neither the src array
-- nor the destination array.
{-# INLINE putSliceUnsafe #-}
putSliceUnsafe ::
       MonadIO m
    => MutByteArray
    -> Int
    -> MutByteArray
    -> Int
    -> Int
    -> m ()
putSliceUnsafe src srcStartBytes dst dstStartBytes lenBytes = liftIO $ do
#ifdef DEBUG
    srcLen <- sizeOfMutableByteArray src
    dstLen <- sizeOfMutableByteArray dst
    when (srcLen - srcStartBytes < lenBytes)
        $ error $ "putSliceUnsafe: src overflow: start" ++ show srcStartBytes
            ++ " end " ++ show srcLen ++ " len " ++ show lenBytes
    when (dstLen - dstStartBytes < lenBytes)
        $ error $ "putSliceUnsafe: dst overflow: start" ++ show dstStartBytes
            ++ " end " ++ show dstLen ++ " len " ++ show lenBytes
#endif
    let !(I# srcStartBytes#) = srcStartBytes
        !(I# dstStartBytes#) = dstStartBytes
        !(I# lenBytes#) = lenBytes
    let arrS# = getMutableByteArray# src
        arrD# = getMutableByteArray# dst
    IO $ \s# -> (# copyMutableByteArray#
                    arrS# srcStartBytes# arrD# dstStartBytes# lenBytes# s#
                , () #)

-- | Unsafe as it does not check whether the start offset and length supplied
-- are valid inside the array.
{-# INLINE cloneSliceUnsafeAs #-}
cloneSliceUnsafeAs :: MonadIO m =>
    PinnedState -> Int -> Int -> MutByteArray -> m MutByteArray
cloneSliceUnsafeAs ps srcOff srcLen src =
    liftIO $ do
        mba <- newBytesAs ps srcLen
        putSliceUnsafe src srcOff mba 0 srcLen
        return mba

-- | @cloneSliceUnsafe offset len arr@ clones a slice of the supplied array
-- starting at the given offset and equal to the given length.
{-# INLINE cloneSliceUnsafe #-}
cloneSliceUnsafe :: MonadIO m => Int -> Int -> MutByteArray -> m MutByteArray
cloneSliceUnsafe = cloneSliceUnsafeAs Unpinned

-- | @pinnedCloneSliceUnsafe offset len arr@
{-# INLINE pinnedCloneSliceUnsafe #-}
pinnedCloneSliceUnsafe :: MonadIO m =>
    Int -> Int -> MutByteArray -> m MutByteArray
pinnedCloneSliceUnsafe = cloneSliceUnsafeAs Pinned

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
