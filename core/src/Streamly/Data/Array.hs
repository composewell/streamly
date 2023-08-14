{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Unboxed immutable arrays with streaming interfaces.
--
-- Please refer to "Streamly.Internal.Data.Array" for functions that have not
-- yet been released.
--
-- For arrays that work on boxed types, not requiring the 'Unbox' constraint,
-- please refer to "Streamly.Data.Array.Generic". For arrays that can be
-- mutated in-place, please see "Streamly.Data.MutArray".

module Streamly.Data.Array
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Overview
    -- $overview

    -- * The Array Type
      Array

    -- * Construction
    -- | When performance matters, the fastest way to generate an array is
    -- 'writeN'. 'IsList' and 'IsString' instances can be
    -- used to conveniently construct arrays from literal values.
    -- 'OverloadedLists' extension or 'fromList' can be used to construct an
    -- array from a list literal.  Similarly, 'OverloadedStrings' extension or
    -- 'fromList' can be used to construct an array from a string literal.

    -- Pure List APIs
    , fromListN
    , fromList

    -- Monadic APIs
    , writeN      -- drop new
    , write       -- full buffer
    , writeLastN    -- drop old (ring buffer)

    -- * Conversion
    -- 'GHC.Exts.toList' from "GHC.Exts" can be used to convert an array to a
    -- list.
    , toList

    -- * Unfolds
    , reader
    , readerRev

    -- * Stream of Arrays
    , chunksSplitOn

    -- * Casting
    , cast
    , asBytes

    -- * Random Access
    , length
    -- , (!!)
    , getIndex

    -- * Unbox Type Class
    , Unbox (..)

    -- * Pinning & Unpinning
    , pin
    , unpin
    , isPinned

    -- * Deprecated
    , read
    , readRev
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Array hiding (read, readRev)
import Streamly.Internal.Data.Unbox (Unbox (..))
import Streamly.Internal.Data.Unfold (Unfold)

import Prelude hiding (read, length)

#include "DocTestDataArray.hs"

-- $overview
--
-- This module provides APIs to create and use unboxed immutable arrays. Once
-- created, their contents cannot be modified. Only types that are unboxable
-- via the 'Unbox' type class can be stored in these arrays. Note that the
-- array memory grows automatically when creating a new array, therefore, an
-- array can be created from a variable length stream.
--
-- == Folding Arrays
--
-- Convert array to stream, and fold the stream:
--
-- >>> fold f arr = Stream.unfold Array.reader arr & Stream.fold f
-- >>> fold Fold.sum (Array.fromList [1,2,3::Int])
-- 6
--
-- == Transforming Arrays
--
-- Convert array to stream, transform, and fold back to array:
--
-- >>> amap f arr = Stream.unfold Array.reader arr & fmap f & Stream.fold Array.write
-- >>> amap (+1) (Array.fromList [1,2,3::Int])
-- fromList [2,3,4]
--
-- == Pinned and Unpinned Arrays
--
-- The array type can use both pinned and unpinned memory under the hood. The
-- default array creation operations create unpinned arrays. IO operations
-- automatically copy an array to pinned memory if the array passed to it is
-- unpinned. Programmers can use appropriate pinned array generation APIs to
-- reduce the copying if it happens.
--
-- Unpinned arrays have the advantage of allowing automatic defragmentation of
-- the memory by GC. Whereas pinned arrays have the advantage of not requiring
-- a copy by GC. Normally you would want to use unpinned arrays. However, in
-- some cases, for example, for long lived large data storage, and for
-- interfacing with the operating system or foreign (non-Haskell) consumers you
-- may want to use pinned arrays.
--
-- == Creating Arrays from Non-IO Streams
--
-- Array creation folds require 'MonadIO' otherwise the compiler may
-- incorrectly share the array memory thinking it is pure.
--
-- See the 'fromPureStream' unreleased API to generate an array from an
-- Identity stream safely without using MonadIO constraint.
--
--
-- Note that 'Identity' streams can be generalized to IO streams:
--
-- >>> pure = Stream.fromList [1,2,3] :: Stream Identity Int
-- >>> generally = Stream.morphInner (return . runIdentity)
-- >>> Stream.fold Array.write (generally pure :: Stream IO Int)
-- fromList [1,2,3]
--
-- == Programming Tips
--
-- This module is designed to be imported qualified:
--
-- >>> import qualified Streamly.Data.Array as Array

-- | Same as 'reader'
--
{-# DEPRECATED read "Please use 'reader' instead" #-}
{-# INLINE_NORMAL read #-}
read :: (Monad m, Unbox a) => Unfold m (Array a) a
read = reader

-- | Same as 'readerRev'
--
{-# DEPRECATED readRev "Please use 'readerRev' instead" #-}
{-# INLINE_NORMAL readRev #-}
readRev :: (Monad m, Unbox a) => Unfold m (Array a) a
readRev = readerRev
