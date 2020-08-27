#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Foreign.Malloc
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Foreign.Malloc
    (
      mallocForeignPtrAlignedBytes
    , mallocForeignPtrAlignedUnmanagedBytes
    )
where


-- We use GHC malloc by default

import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc (mallocBytes)
#ifdef USE_C_MALLOC
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
#endif

import qualified GHC.ForeignPtr as GHC

{-# INLINE mallocForeignPtrAlignedBytes #-}
mallocForeignPtrAlignedBytes :: Int -> Int -> IO (GHC.ForeignPtr a)
#ifdef USE_C_MALLOC
mallocForeignPtrAlignedBytes size _alignment = do
    p <- mallocBytes size
    newForeignPtr finalizerFree p
#else
mallocForeignPtrAlignedBytes =
    GHC.mallocPlainForeignPtrAlignedBytes
#endif

-- memalign alignment size
-- foreign import ccall unsafe "stdlib.h posix_memalign" _memalign :: CSize -> CSize -> IO (Ptr a)

mallocForeignPtrAlignedUnmanagedBytes :: Int -> Int -> IO (ForeignPtr a)
mallocForeignPtrAlignedUnmanagedBytes size _alignment = do
    -- XXX use posix_memalign/aligned_alloc for aligned allocation
    p <- mallocBytes size
    newForeignPtr_ p
