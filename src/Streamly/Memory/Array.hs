#include "inline.hs"

-- |
-- Module      : Streamly.Memory.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides immutable arrays in pinned memory (non GC memory)
-- suitable for long lived data storage, random access and for interfacing with
-- the operating system.
--
-- Arrays in this module are chunks of pinned memory that hold a sequence of
-- 'Storable' values of a given type, they cannot store non-serializable data
-- like functions.  Once created an array cannot be modified.  Pinned memory
-- allows efficient buffering of long lived data without adding any impact to
-- GC. One array is just one pointer visible to GC and it does not have to be
-- copied across generations.  Moreover, pinned memory allows communication
-- with foreign consumers and producers (e.g. file or network IO) without
-- copying the data.
--
-- = Programmer Notes
--
-- To apply a transformation to an array use 'read' to unfold the array into a
-- stream, apply a transformation on the stream and then use 'write' to fold it
-- back to an array.
--
-- This module is designed to be imported qualified:
--
-- > import qualified Streamly.Array as A
--
-- For experimental APIs see "Streamly.Internal.Data.Array".

module Streamly.Memory.Array
    {-# DEPRECATED "Use Streamly.Data.Array.Foreign instead" #-}
    (
      A.Array

    -- * Arrays
    -- ** Construction
    -- | When performance matters, the fastest way to generate an array is
    -- 'writeN'. 'IsList' and 'IsString' instances can be
    -- used to conveniently construct arrays from literal values.
    -- 'OverloadedLists' extension or 'fromList' can be used to construct an
    -- array from a list literal.  Similarly, 'OverloadedStrings' extension or
    -- 'fromList' can be used to construct an array from a string literal.

    -- Pure List APIs
    , A.fromListN
    , A.fromList

    -- Monadic APIs
    , A.writeN      -- drop new
    , A.write       -- full buffer
    -- , writeLastN -- drop old (ring buffer)

    -- ** Elimination
    -- 'GHC.Exts.toList' from "GHC.Exts" can be used to convert an array to a
    -- list.

    , A.toList
    , A.read

    -- ** Random Access
    , A.length
    -- , (!!)
    -- , A.readIndex
    )
where

import Streamly.Internal.Data.Array as A
