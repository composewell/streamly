{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

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

    -- ** Helpers
    , touch

    -- ** Pinning
    , PinnedState(..)
    , isPinned
    , pin
    , unpin

    -- ** Allocation
    , empty
    , newAs
    , new
    , pinnedNew -- XXX new'
    , reallocSliceAs

    -- ** Access
    , length
    , unsafeAsPtr

    -- ** Modify
    , unsafePutSlice
    , unsafePutPtrN

    -- ** Copy
    , unsafeCloneSliceAs
    , unsafeCloneSlice
    , unsafePinnedCloneSlice -- XXX unsafeCloneSlice'

    -- ** Compare
    , unsafeByteCmp

    -- ** Capacity Management
    , blockSize
    , largeObjectThreshold

    -- ** Mutable Cells
    , mutVar'
    , withMutVar'
    , emptyMutVar'

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
    , unsafePinnedAsPtr
    , nil
    ) where

#include "deprecation.h"

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Word (Word8)
#ifdef DEBUG
import Debug.Trace (trace)
#endif
import Foreign.C.Types (CSize(..))
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
unsafePinnedAsPtr arr0 f = do
    arr <- liftIO $ pin arr0
    let !ptr = Ptr (byteArrayContents#
                     (unsafeCoerce# (getMutByteArray# arr)))
    r <- f ptr
    liftIO $ touch arr
    return r

{-# DEPRECATED asPtrUnsafe "Pin the array and then use unsafeAsPtr." #-}
{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => MutByteArray -> (Ptr a -> m b) -> m b
asPtrUnsafe = unsafePinnedAsPtr

-- XXX We are not passing the length like in the MutArray API or Array API.

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
unsafeAsPtr :: MonadIO m => MutByteArray -> (Ptr a -> IO b) -> m b
unsafeAsPtr arr f = liftIO $ do
    when (not (isPinned arr))
        $ error "unsafeAsPtr requires the array to be pinned"

    let !ptr = Ptr (byteArrayContents#
                     (unsafeCoerce# (getMutByteArray# arr)))
    r <- f ptr
    -- While f is using the bare pointer, the MutByteArray may be garbage
    -- collected by the GC, tell the GC that we are still using it.
    touch arr
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

-- XXX Should we use bitshifts in calculations or it gets optimized by the
-- compiler/processor itself?
--
-- | The page or block size used by the GHC allocator. Allocator allocates at
-- least a block and then allocates smaller allocations from within a block.
blockSize :: Int
blockSize = 4 * 1024

-- | Allocations larger than 'largeObjectThreshold' are in multiples of block
-- size and are always pinned. The space beyond the end of a large object up to
-- the end of the block is unused.
largeObjectThreshold :: Int
largeObjectThreshold = (blockSize * 8) `div` 10

{-# INLINE pinnedNewRaw #-}
pinnedNewRaw :: Int -> IO MutByteArray
pinnedNewRaw (I# nbytes) = IO $ \s ->
    case newPinnedByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = MutByteArray mbarr#
            in (# s', c #)

{-# INLINE pinnedNew #-}
pinnedNew :: Int -> IO MutByteArray
pinnedNew nbytes | nbytes < 0 =
  errorWithoutStackTrace "pinnedNew: size must be >= 0"
pinnedNew nbytes = pinnedNewRaw nbytes

-- XXX add "newRoundedUp" to round up the large size to the next page boundary
-- and return the allocated size.
-- Uses the pinned version of allocated if the size required is >
-- largeObjectThreshold
{-# INLINE new #-}
new :: Int -> IO MutByteArray
new nbytes | nbytes > largeObjectThreshold = pinnedNewRaw nbytes
new nbytes | nbytes < 0 =
  errorWithoutStackTrace "newByteArray: size must be >= 0"
new (I# nbytes) = IO $ \s ->
    case newByteArray# nbytes s of
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

-- | Like mutVar' but without initialization.
{-# NOINLINE emptyMutVar' #-}
emptyMutVar' :: Int -> MutByteArray
emptyMutVar' = unsafePerformIO . pinnedNew

-- | A pinned mutable cell to be used for variable length data in local scopes
-- for temporary storage, reducing allocations and for better cache benefits.
-- It is pure so that we can declare it within a global or local scope without
-- a monadic context. In local scopes it may be better to use the IO monad
-- instead of declaring mutvars.
--
-- MutByteArray 'Unbox' and 'Serialize' instances can be used to conveniently
-- use the mutable cell.
--
-- Also see "Streamly.Internal.Data.IORef.Unboxed" for safer and easier to use
-- mutvars for fixed length data structrures. See
-- "Streamly.Internal.Data.MutArray" for array type mutvars.
--
-- /Unsafe:/ you should not write beyond the length of the mutvar.
--
{-# NOINLINE mutVar' #-}
mutVar' :: Int -> (MutByteArray -> IO Int) -> (MutByteArray, Int)
mutVar' n action = unsafePerformIO $ do
    arr <- pinnedNew n
    i <- action arr
    -- return $ Tuple' arr i
    return (arr, i)

withMutVar' :: Int -> (MutByteArray -> IO a) -> IO a
withMutVar' n action = do
    arr <- pinnedNew n
    action arr

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

-- | @unsafePutSlice src srcOffset dst dstOffset len@ copies @len@ bytes from
-- @src@ at @srcOffset@ to dst at @dstOffset@.
--
-- This is unsafe as it does not check the bounds of @src@ or @dst@.
--
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

foreign import ccall unsafe "string.h memcpy" c_memcpy_pinned
    :: Addr# -> Addr# -> CSize -> IO (Ptr Word8)

-- | @unsafePutPtrN srcPtr dst dstOffset len@ copies @len@ bytes from @srcPtr@
-- to dst at @dstOffset@.
--
-- /Unsafe/:
--
-- The caller has to ensure that:
--
-- * the MutByteArray @dst@ is valid up to @dstOffset + len@.
-- * the @srcPtr@ is alive and pinned during the call.
-- * the @srcPtr@ is valid up to length @len@.
--
{-# INLINE unsafePutPtrN #-}
unsafePutPtrN ::
       MonadIO m
    => Ptr Word8
    -> MutByteArray
    -> Int
    -> Int
    -> m ()
unsafePutPtrN (Ptr srcAddr) dst dstOffset len = liftIO $ do
#ifdef DEBUG
    dstLen <- length dst
    when (dstLen - dstOffset < len)
        $ error $ "unsafePutPtrN: dst overflow: start" ++ show dstOffset
            ++ " end " ++ show dstLen ++ " len " ++ show len
#endif
    let !dstAddr# = byteArrayContents# (unsafeCoerce# (getMutByteArray# dst))
        !(I# dstOff#) = dstOffset
        !dstAddr1# = plusAddr# dstAddr# dstOff#
    _ <- c_memcpy_pinned dstAddr1# srcAddr (fromIntegral len)
    pure ()

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

unsafeByteCmp
    :: MutByteArray -> Int -> MutByteArray -> Int -> Int -> IO Int
unsafeByteCmp
    (MutByteArray marr1) (I# st1#) (MutByteArray marr2) (I# st2#) (I# len#) =
    IO $ \s# ->
        let res =
                I#
                    (compareByteArrays#
                         (unsafeCoerce# marr1)
                         st1#
                         (unsafeCoerce# marr2)
                         st2#
                         len#)
         in (# s#, res #)

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
