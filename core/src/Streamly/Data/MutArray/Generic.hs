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
    , writeN

    -- * Appending elements
    , new
    , snoc

    -- * Conversion
    , toList

    -- * Unfolds
    , reader

     -- * Streams
    , read

    -- * Random reads
    , getIndex

    -- * Inplace mutation
    , putIndex
    , modifyIndex

    -- * Size
    , length
    )
where

import Streamly.Internal.Data.Array.Generic.Mut.Type
import  Prelude hiding (read, length)

#include "DocTestDataMutArrayGeneric.hs"
