-- |
-- Module      : Streamly.Data.Array.Generic.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Mutable arrays for all types (unconstrained).
--
module Streamly.Data.Array.Generic.Mut
(
    -- * Type
      Array

    -- * Construction
    , writeN

    -- * Appending elements
    , new
    , snoc

    -- * Conversion
    , toList

    -- * Unfolds
    , reader

    -- * Random reads
    , getIndex

    -- * Inplace mutation
    , putIndex
    , modifyIndex
    )
where

import Streamly.Internal.Data.Array.Generic.Mut.Type
