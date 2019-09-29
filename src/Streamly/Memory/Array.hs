{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- This module provides immutable arrays suitable for storage and random
-- access. Once created an array cannot be mutated without copying.  Arrays in
-- this module are chunks of memory that hold a sequence of 'Storable' values
-- of a given type. They are designed to store serializable data, they cannot
-- store non-serializable data like functions, this is reflected by the
-- 'Storable' constraint. For efficient buffering of data, arrays use pinned
-- memory and therefore can hold arbitrary number of elements with a single
-- movable pointer visible to GC, therefore, adding no pressure to GC.
-- Moreover, pinned memory allows communication with foreign consumers and
-- producers (e.g. file or network IO) without copying the data.
--
-- By design, there are no array transformation operations provided in this
-- module, only conversion to and from stream is provided.  Arrays can be
-- operated upon /efficiently/ by converting them into a stream, applying the
-- desired stream transformations from "Streamly.Prelude" and then converting
-- it back to an array.
--
-- This module is designed to be imported qualified:
--
-- > import qualified Streamly.Array as A

-- To summarize:
--
--  Arrays are finite and fixed in size
--  provide /O(1)/ access to elements
--  store only data and not functions
--  provide efficient IO interfacing
--
-- 'Foldable' instance is not provided because the implementation would be much
-- less efficient compared to folding via streams.  'Semigroup' and 'Monoid'
-- instances are deliberately not provided to avoid misuse; concatenating
-- arrays using binary operations can be highly inefficient.  Instead, use
-- 'Streamly.Memory.Array.Stream.toArray' to concatenate N arrays at once.
--
-- Each array is one pointer visible to the GC.  Too many small arrays (e.g.
-- single byte) are only as good as holding those elements in a Haskell list.
-- However, small arrays can be compacted into large ones to reduce the
-- overhead. To hold 32GB memory in 32k sized buffers we need 1 million arrays
-- if we use one array for each chunk. This is still significant to add
-- pressure to GC.  However, we can create arrays of arrays (trees) to scale to
-- arbitrarily large amounts of memory but still using small chunks of
-- contiguous memory.

module Streamly.Memory.Array
    (
      A.Array

    -- , defaultChunkSize

    -- * Arrays
    -- ** Construction
    -- | When performance matters, the fastest way to generate an array is
    -- 'writeN'. For regular use, 'IsList' and 'IsString' instances can be
    -- used to conveniently construct arrays from literal values.
    -- 'OverloadedLists' extension or 'fromList' can be used to construct an
    -- array from a list literal.  Similarly, 'OverloadedStrings' extension or
    -- 'fromList' can be used to construct an array from a string literal.

    -- Pure List APIs
    , A.fromListN
    , A.fromList

    -- Monadic APIs
    -- , newArray
    , A.writeN      -- drop new
    , A.write         -- full buffer
    -- , writeLastN -- drop old (ring buffer)

    -- ** Elimination
    -- 'GHC.Exts.toList' from "GHC.Exts" can be used to convert an array to a
    -- list.

    , A.toList
    , A.read

    -- ** Random Access
    , A.length
    -- , (!!)

    , A.readIndex
    )
where

import Streamly.Internal.Memory.Array as A
