-- |
-- Module      : Streamly.Data.Array.Generic.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Generic mutable array type with an option to use
-- foreign (non-GHC) memory allocators. Fulfils the following goals:
--
-- * Random access (array)
-- * Performance - in-place operations (mutable)
-- * Fragmentation control (foreign allocators)
--
-- Stream and Fold APIs allow easy, efficient and convenient operations on
-- arrays.
module Streamly.Data.Array.Generic.Mut
(
    -- * Type
      Array

    -- * Constructing and Writing
    -- ** Construction
    -- *** Uninitialized Arrays
    , new

    -- * Eliminating and Reading
    -- ** Unfolds
    , reader

    -- *** From streams
    , writeN

    -- * Inplace mutation
    , putIndex
    , modifyIndex

    -- * Growing and Shrinking
    -- Arrays grow only at the end, though it is possible to grow on both sides
    -- and therefore have a cons as well as snoc. But that will require two
    -- bounds in the array representation.

    -- ** Appending elements
    , snoc

    -- * Eliminating
    , toList

    -- ** Random reads
    , getIndex

    )
where

import Streamly.Internal.Data.Array.Generic.Mut.Type
