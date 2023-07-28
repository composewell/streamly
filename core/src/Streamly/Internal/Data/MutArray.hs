-- |
-- Module      : Streamly.Internal.Data.MutArray
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.MutArray
    (
      module Streamly.Internal.Data.MutArray.Type
    , splitOn
    , genSlicesFromLen
    , getSlicesFromLen
    , fromStream
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Unfold as Unfold

import Prelude hiding (foldr, length, read, splitAt)
import Streamly.Internal.Data.MutArray.Type

-- | Split the array into a stream of slices using a predicate. The element
-- matching the predicate is dropped.
--
-- /Pre-release/
{-# INLINE splitOn #-}
splitOn :: (MonadIO m, Unbox a) =>
    (a -> Bool) -> MutArray a -> Stream m (MutArray a)
splitOn predicate arr =
    fmap (\(i, len) -> getSliceUnsafe i len arr)
        $ D.sliceOnSuffix predicate (read arr)

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
-- may fail.  When the stream size is not known, `chunksOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
-- /Pre-release/
{-# INLINE fromStream #-}
fromStream :: (MonadIO m, Unbox a) => Stream m a -> m (MutArray a)
fromStream = fromStreamD
-- fromStream (Stream m) = P.fold write m
