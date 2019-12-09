{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}

#include "../../inline.hs"

-- |
-- Module      : Streamly.Memory.Malloc
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Memory.Malloc
    (
      mallocForeignPtrAlignedBytes
    , mallocForeignPtrAlignedUnmanagedBytes
    )
where

#define USE_GHC_MALLOC

import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc (mallocBytes)
#ifndef USE_GHC_MALLOC
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
#endif

import qualified GHC.ForeignPtr as GHC

{-# INLINE mallocForeignPtrAlignedBytes #-}
mallocForeignPtrAlignedBytes :: Int -> Int -> IO (GHC.ForeignPtr a)
#ifdef USE_GHC_MALLOC
mallocForeignPtrAlignedBytes size alignment = do
    GHC.mallocPlainForeignPtrAlignedBytes size alignment
#else
mallocForeignPtrAlignedBytes size _alignment = do
    p <- mallocBytes size
    newForeignPtr finalizerFree p
#endif

-- memalign alignment size
-- foreign import ccall unsafe "stdlib.h posix_memalign" _memalign :: CSize -> CSize -> IO (Ptr a)

mallocForeignPtrAlignedUnmanagedBytes :: Int -> Int -> IO (ForeignPtr a)
mallocForeignPtrAlignedUnmanagedBytes size _alignment = do
    -- XXX use posix_memalign/aligned_alloc for aligned allocation
    p <- mallocBytes size
    newForeignPtr_ p
