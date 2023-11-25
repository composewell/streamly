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
    , MutableByteArray
    , getMutableByteArray#

    -- ** Pinning
    , PinnedState(..)
    , isPinned
    , pin
    , unpin

    -- ** Allocation
    , nil
    , newBytesAs
    , new
    , pinnedNew
    , pinnedNewAlignedBytes

    -- ** Access
    , sizeOfMutableByteArray
    , putSliceUnsafe
    , asPtrUnsafe
    ) where

import Control.Monad.IO.Class (MonadIO(..))
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
    | Unpinned

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

-- | Use a @MutByteArray@ as @Ptr a@. This is useful when we want to pass
-- an array as a pointer to some operating system call or to a "safe" FFI call.
--
-- If the array is not pinned it is copied to pinned memory before passing it
-- to the monadic action.
--
-- /Performance Notes:/ Forces a copy if the array is not pinned. It is advised
-- that the programmer keeps this in mind and creates a pinned array
-- opportunistically before this operation occurs, to avoid the cost of a copy
-- if possible.
--
-- /Unsafe/ because of direct pointer operations. The user must ensure that
-- they are writing within the legal bounds of the array.
--
-- /Pre-release/
--
{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
asPtrUnsafe arr f = do
  contents <- liftIO $ pin arr
  let !ptr = Ptr (byteArrayContents#
                     (unsafeCoerce# (getMutableByteArray# contents)))
  r <- f ptr
  liftIO $ touch contents
  return r

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

{-# NOINLINE nil #-}
nil :: MutByteArray
nil = unsafePerformIO $ new 0

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
  errorWithoutStackTrace "pinnedNewByteArray: size must be >= 0"
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
-- This is not safe as it does not check the bounds.
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
    dstLen <- sizeOfMutableByteArray src
    when (srcLen - srcStartBytes < lenBytes) $ error "SRC: Insufficient length."
    when (dstLen - dstStartBytes < lenBytes) $ error "DST: Insufficient length."
#endif
    let !(I# srcStartBytes#) = srcStartBytes
        !(I# dstStartBytes#) = dstStartBytes
        !(I# lenBytes#) = lenBytes
    let arrS# = getMutableByteArray# src
        arrD# = getMutableByteArray# dst
    IO $ \s# -> (# copyMutableByteArray#
                    arrS# srcStartBytes# arrD# dstStartBytes# lenBytes# s#
                , () #)

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

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
