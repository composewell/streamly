#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unboxed pinned mutable array type for 'Storable' types with an option to use
-- foreign (non-GHC) memory allocators. Fulfils the following goals:
--
-- * Random access (array)
-- * Efficient storage (unboxed)
-- * Performance (unboxed access)
-- * Performance - in-place operations (mutable)
-- * Performance - GC (pinned, mutable)
-- * interfacing with OS (pinned)
-- * Fragmentation control (foreign allocators)
--
-- Stream and Fold APIs allow easy, efficient and convenient operations on
-- arrays.

module Streamly.Internal.Data.Array.Foreign.Mut
    (
      module Streamly.Internal.Data.Array.Foreign.Mut.Type
    )
where

import Prelude hiding (foldr, length, read, splitAt)

import Streamly.Internal.Data.Array.Foreign.Mut.Type
