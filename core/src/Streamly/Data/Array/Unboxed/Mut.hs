-- |
-- Module      : Streamly.Data.Array.Unboxed.Mut
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

module Streamly.Data.Array.Unboxed.Mut
    (
      Unbox (..)
    , Array

    -- * Arrays
    -- ** Construction

    -- Uninitialized Arrays
    , new
    , newPinned

    -- From containers
    , fromListN
    , fromList
    , writeN      -- drop new
    , write       -- full buffer
    -- writeLastN

    -- ** Appending elements
    , snoc

    -- ** Appending streams
    , appendN
    , append

    -- ** Inplace mutation
    , putIndex

    -- ** Random access
    , getIndex

    -- ** Elimination
    , toList
    , read
    , readRev

    -- ** Casting
    , cast
    , asBytes

    -- ** Size
    , length
    )
where

import Prelude hiding (length, read)
import Streamly.Internal.Data.Array.Unboxed.Mut
import Streamly.Internal.Data.Unboxed (Unbox (..))
