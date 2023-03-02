-- |
-- Module      : Streamly.Data.Array.Mut
-- Copyright   : (c) 2022 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module provides mutable arrays suitable for long lived data storage,
-- random access, and mutation.
--

module Streamly.Data.Array.Mut
    (
    -- * Types
      MutArray

    -- * Construction

    -- Uninitialized Arrays
    , new
    , newPinned

    -- From containers
    , fromListN
    , fromList
    , writeN      -- drop new
    , write       -- full buffer
    -- writeLastN

    -- * Appending elements
    , snoc

    -- * Appending streams
    , writeAppendN
    , writeAppend

    -- * Inplace mutation
    , putIndex

    -- * Random access
    , getIndex

    -- * Conversion
    , toList

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
    )
where

import Prelude hiding (length, read)
import Streamly.Internal.Data.Array.Mut
import Streamly.Internal.Data.Unboxed (Unbox (..))
