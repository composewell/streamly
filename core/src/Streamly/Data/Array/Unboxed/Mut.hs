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
      A.Array

    -- * Arrays
    -- ** Construction

    -- Uninitialized Arrays
    , A.newArray

    -- From containers
    , A.fromListN
    , A.fromList
    , A.writeN      -- drop new
    , A.write       -- full buffer
    -- writeLastN

    -- ** Appending elements
    , A.snoc

    -- ** Appending streams
    , A.appendN
    , A.append

    -- ** Inplace mutation
    , A.putIndex

    -- ** Random access
    , A.getIndex

    -- ** Elimination
    , A.toList
    , A.read
    , A.readRev

    -- ** Casting
    , A.cast
    , A.asBytes

    -- ** Size
    , A.length
    )
where

import Streamly.Internal.Data.Array.Unboxed.Mut as A
