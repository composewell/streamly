{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : Streamly.Internal.Data.CString
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- MutByteArray representing null terminated c strings.
-- All APIs in this module are unsafe and caution must be used when using them.
-- Completely experimental. Everything is subject to change without notice.

module Streamly.Internal.Data.CString
    (
      splice
    , spliceCString
    , splicePtrN
    , putCString
    , length
    )

where

#ifdef DEBUG
#include "assert.hs"
#endif

import GHC.Ptr (Ptr(..), castPtr)
import Foreign.C (CString, CSize(..))
import GHC.Exts (MutableByteArray#, RealWorld)
import GHC.Word (Word8)

import Streamly.Internal.Data.MutByteArray.Type hiding (length)

import Prelude hiding (length)

-- XXX Use cstringLength# from GHC.CString in ghc-prim
foreign import ccall unsafe "string.h strlen" c_strlen
    :: MutableByteArray# RealWorld -> IO CSize

-- XXX Use cstringLength# from GHC.CString in ghc-prim
foreign import ccall unsafe "string.h strlen" c_strlen_pinned
    :: CString -> IO CSize

{-# INLINE length #-}
length :: MutByteArray -> IO Int
length (MutByteArray src#) = do
    fmap fromIntegral $ c_strlen src#

-- | Join two null terminated cstrings, the null byte of the first string is
-- overwritten. Does not check the destination length or source length.
-- Destination must have enough space to accomodate src.
--
-- Returns the offset of the null byte.
--
-- /Unsafe/
splice :: MutByteArray -> MutByteArray -> IO Int
splice dst@(MutByteArray dst#) src@(MutByteArray src#) = do
    srcLen <- fmap fromIntegral $ c_strlen src#
#ifdef DEBUG
    srcLen1 <- length src
    assertM(srcLen <= srcLen1)
#endif
    dstLen <- fmap fromIntegral $ c_strlen dst#
#ifdef DEBUG
    dstLen1 <- length dst
    assertM(dstLen <= dstLen1)
    assertM(dstLen + srcLen + 1 <= dstLen1)
#endif
    unsafePutSlice src 0 dst dstLen (srcLen + 1)
    return $ dstLen + srcLen

-- | Append specified number of bytes from a Ptr to a MutByteArray CString. The
-- null byte of CString is overwritten and the result is terminated with a null
-- byte.
{-# INLINE splicePtrN #-}
splicePtrN :: MutByteArray -> Ptr Word8 -> Int -> IO Int
splicePtrN dst@(MutByteArray dst#) src srcLen = do
    dstLen <- fmap fromIntegral $ c_strlen dst#
#ifdef DEBUG
    dstLen1 <- length dst
    assertM(dstLen <= dstLen1)
    assertM(dstLen + srcLen + 1 <= dstLen1)
#endif
    -- unsafePutSlice src 0 dst dstLen srcLen
    -- XXX unsafePutPtrN signature consistency with serialization routines
    -- XXX unsafePutSlice as well
    unsafePutPtrN src dst dstLen (srcLen + 1)
    return $ dstLen + srcLen

-- | Join a null terminated cstring MutByteByteArray with a null terminated
-- cstring Ptr.
{-# INLINE spliceCString #-}
spliceCString :: MutByteArray -> CString -> IO Int
spliceCString dst src = do
    srcLen <- fmap fromIntegral $ c_strlen_pinned src
    splicePtrN dst (castPtr src) srcLen

-- XXX this is CString serialization.

-- | @putCString dst dstOffset cstr@ writes the cstring cstr at dstOffset in
-- the dst MutByteArray. The result is terminated by a null byte.
{-# INLINE putCString #-}
putCString :: MutByteArray -> Int -> CString -> IO Int
putCString dst off src = do
    srcLen <- fmap fromIntegral $ c_strlen_pinned src
    unsafePutPtrN (castPtr src) dst off (srcLen + 1)
    return $ off + srcLen
