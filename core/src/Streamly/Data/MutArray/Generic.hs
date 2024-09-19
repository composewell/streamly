{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.MutArray.Generic
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unconstrained version of "Streamly.Data.MutArray" module.
--
-- See the "Streamly.Data.MutArray" module for documentation.
--
module Streamly.Data.MutArray.Generic
(
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Type
      MutArray

    -- * Construction
    , emptyOf
    , fromListN
    , fromList
    , createOf
    , create

    -- * Appending elements
    , snoc

    -- * Inplace mutation
    , putIndex
    , unsafePutIndex
    , modifyIndex
    , unsafeModifyIndex
    -- , modify

    -- * Random reads
    , getIndex
    , unsafeGetIndex

    -- * Conversion
    , toList

    -- * Streams
    , read
    , readRev

    -- * Unfolds
    , reader
    -- , readerRev

    -- * Size
    , length

    -- * Deprecated
    , new
    , writeN
    , write
    , modifyIndexUnsafe
    , putIndexUnsafe
    , getIndexUnsafe
    )
where

import Streamly.Internal.Data.MutArray.Generic
import  Prelude hiding (read, length)

#include "DocTestDataMutArrayGeneric.hs"
