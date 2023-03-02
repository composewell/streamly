#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unboxed pinned mutable array type for 'Unboxed' types with an option to use
-- foreign (non-GHC) memory allocators. Fulfils the following goals:
--
-- * Random access (array)
-- * Efficient storage (unboxed)
-- * Performance (unboxed access)
-- * Performance - in-place operations (mutable)
-- * Performance - GC (pinned, mutable)
-- * interfacing with OS (pinned)
-- * Fragmentation control (foreign allocators)
--
-- Stream and Fold APIs allow easy, efficient and convenient operations on
-- arrays.

module Streamly.Internal.Data.Array.Mut
    (
      module Streamly.Internal.Data.Array.Mut.Type
    , splitOn
    , genSlicesFromLen
    , getSlicesFromLen
    , fromStream
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Unboxed (Unbox)
import Streamly.Internal.Data.Stream.StreamD (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Unfold as Unfold

import Prelude hiding (foldr, length, read, splitAt)
import Streamly.Internal.Data.Array.Mut.Type

-- | Split the array into a stream of slices using a predicate. The element
-- matching the predicate is dropped.
--
-- /Pre-release/
{-# INLINE splitOn #-}
splitOn :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> Stream m (MutArray a)
splitOn predicate arr =
    fmap (\(i, len) -> getSliceUnsafe i len arr)
        $ D.sliceOnSuffix predicate (toStreamD arr)

-- | Generate a stream of array slice descriptors ((index, len)) of specified
-- length from an array, starting from the supplied array index. The last slice
-- may be shorter than the requested length depending on the array length.
--
-- /Pre-release/
{-# INLINE genSlicesFromLen #-}
genSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (Int, Int)
genSlicesFromLen from len =
    let fromThenTo n = (from, from + len, n - 1)
        mkSlice n i = return (i, min len (n - i))
     in Unfold.lmap length
        $ Unfold.mapM2 mkSlice
        $ Unfold.lmap fromThenTo Unfold.enumerateFromThenTo

-- | Generate a stream of slices of specified length from an array, starting
-- from the supplied array index. The last slice may be shorter than the
-- requested length depending on the array length.
--
-- /Pre-release/
{-# INLINE getSlicesFromLen #-}
getSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (MutArray a)
getSlicesFromLen from len =
    let mkSlice arr (i, n) = return $ getSliceUnsafe i n arr
     in Unfold.mapM2 mkSlice (genSlicesFromLen from len)

-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'writeN' is at least twice
-- as efficient when the size is already known.
--
-- Note that if the input stream is too large memory allocation for the array
-- may fail.  When the stream size is not known, `arraysOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
-- /Pre-release/
{-# INLINE fromStream #-}
fromStream :: (MonadIO m, Unbox a) => Stream m a -> m (MutArray a)
fromStream = fromStreamD
-- fromStream (Stream m) = P.fold write m
