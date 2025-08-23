{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.RingArray
-- Copyright   : (c) 2025 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module provides APIs to create and use unboxed, mutable ring arrays of
-- fixed size. Ring arrays are useful to keep a circular buffer or a sliding
-- window of elements.
--
-- RingArrays are of fixed size but there is a way to expand the size of the
-- ring, you can copy the ring to a MutArray, expand the MutArray and the cast
-- it back to RingArray.
--
-- This module is designed to be imported qualified:
--
-- >>> import qualified Streamly.Data.RingArray as Ring
--
-- Please refer to "Streamly.Internal.Data.RingArray" for functions that have
-- not yet been released.
--

module Streamly.Data.RingArray
    ( RingArray

    -- * Construction
    , createOfLast
    , castMutArray -- XXX this is unsafeFreeze in Array module
    , castMutArrayWith
    -- , unsafeCastMutArray
    -- , unsafeCastMutArrayWith

    -- * Moving the Head
    , moveForward
    , moveReverse
    -- , moveBy

    -- * In-place Mutation
    , insert
    , replace
    , replace_
    , putIndex
    , modifyIndex

    -- * Random Access
    , getIndex
    , unsafeGetIndex
    , unsafeGetHead

    -- * Conversion
    , toList
    , toMutArray

    -- * Streams
    , read
    , readRev

    -- * Unfolds
    , reader
    , readerRev

    -- * Size
    , length
    , byteLength

    -- * Casting
    , cast
    -- , unsafeCast
    , asBytes
    , asMutArray
    -- , asMutArray_

    -- * Folds
    -- , foldlM'
    , fold

    -- * Stream of Rings
    , ringsOf
    , scanRingsOf

    -- * Fast Byte Comparisons
    , eqArray
    , eqArrayN

    ) where

import Streamly.Internal.Data.RingArray
import Prelude hiding (read, length)
