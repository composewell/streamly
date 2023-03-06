-- |
-- Module      : Streamly.Data.MutArray
-- Copyright   : (c) 2022 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Mutable version of "Streamly.Data.Array". Please refer to that module for
-- general documentation. The contents of mutable ararys can be modified
-- in-place.
--
-- See "Streamly.Data.MutArray.Generic" for mutable arrays that work for boxed
-- types i.e. not requiring the 'Unbox' constraint.

module Streamly.Data.MutArray
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
