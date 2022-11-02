-- |
-- Module      : Streamly.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
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
-- Array creation APIs require a 'MonadIO' Monad, except 'fromList' which is a
-- pure API. To operate on streams in pure Monads like 'Identity' you can hoist
-- it to IO monad as follows:
--
-- >>> import Data.Functor.Identity (Identity, runIdentity)
-- >>> s = Stream.fromList [1..10] :: Stream Identity Int
-- >>> s1 = Stream.hoist (return . runIdentity) s :: Stream IO Int
-- >>> Stream.fold Array.write s1 :: IO (Array Int)
-- [1,2,3,4,5,6,7,8,9,10]
--
-- 'unsafePerformIO' can be used to get a pure API from IO, as long as you know
-- it is safe to do so:
--
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> unsafePerformIO $ Stream.fold Array.write s1 :: Array Int
-- [1,2,3,4,5,6,7,8,9,10]
--
-- To apply a transformation to an array use 'read' to unfold the array into a
-- stream, apply a transformation on the stream and then use 'write' to fold it
-- back to an array.
--
-- This module is designed to be imported qualified:
--
-- > import qualified Streamly.Data.Array as Array
--
-- For experimental APIs see "Streamly.Internal.Data.Array.Unboxed".

module Streamly.Data.Array
    (
    -- * Types
      Unbox (..)
    , A.Array

    -- * Construction
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
    , writeLastN    -- drop old (ring buffer)

    -- * Conversion
    -- 'GHC.Exts.toList' from "GHC.Exts" can be used to convert an array to a
    -- list.
    , A.toList

    -- * Unfolds
    , A.reader
    , A.readerRev

    -- * Casting
    , cast
    , asBytes

    -- * Random Access
    , A.length
    -- , (!!)
    , A.getIndex

    -- * Deprecated
    , read
    , readRev
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.Array.Unboxed as A hiding (read, readRev)

import Streamly.Internal.Data.Unboxed (Unbox (..))
import Prelude hiding (read)

-- $setup
-- >>> :m
-- >>> :set -XFlexibleContexts
-- >>> import Streamly.Internal.Data.Stream (Stream)
-- >>> import Streamly.Data.Array (Array)
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
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
