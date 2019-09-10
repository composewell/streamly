{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Memory.Array.Types
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Memory.Malloc
    (
      mallocForeignPtrAlignedBytes
    )
where

#define USE_GHC_MALLOC

#ifndef USE_GHC_MALLOC
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, finalizerFree)
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
