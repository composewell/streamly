{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.MutArray
-- Copyright   : (c) 2022 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module provides a mutable version of "Streamly.Data.Array". The
-- contents of a mutable array can be modified in-place. For general
-- documentation, please refer to the original module.
--
-- Please refer to "Streamly.Internal.Data.Array.Mut" for functions that have
-- not yet been released.
--
-- For mutable arrays that work on boxed types, not requiring the 'Unbox'
-- constraint, please refer to "Streamly.Data.MutArray.Generic".

module Streamly.Data.MutArray
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Mutable Array Type
      MutArray

    -- * Construction

    -- Uninitialized Arrays
    , new
    , pinnedNew

    -- From containers
    , fromListN
    , fromList
    , writeN      -- drop new
    , write       -- full buffer
    -- writeLastN

    -- * Appending elements
    , snoc

    -- * Appending streams
    , writeAppendN -- XXX Rename to writeSnocN?
    , writeAppend -- XXX Rename to writeSnoc?

    -- * Inplace mutation
    , putIndex
    , putIndexUnsafe
    , modifyIndex
    , modifyIndexUnsafe
    , modify

    -- * Random access
    , getIndex
    , getIndexUnsafe

    -- * Conversion
    , toList

    -- * Streams
    , read
    , readRev

    -- * Unfolds
    , reader
    , readerRev

    -- * Casting
    , cast
    , asBytes

    -- * Size
    , length

    -- * Unbox Type Class
    , Unbox (..)

    -- * Pinning & Unpinning
    , pin
    , unpin
    , isPinned

    -- * Deprecated
    , newPinned
    )
where

import Prelude hiding (length, read)
import Streamly.Internal.Data.Array.Mut
import Streamly.Internal.Data.Unbox (Unbox (..))
import Control.Monad.IO.Class (MonadIO)

#include "DocTestDataMutArray.hs"

{-# DEPRECATED newPinned "Please use pinnedNew instead." #-}
{-# INLINE newPinned #-}
newPinned :: forall m a. (MonadIO m, Unbox a) => Int -> m (MutArray a)
newPinned = pinnedNew
