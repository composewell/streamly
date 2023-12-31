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
    -- * MutArray.Type module
      module Streamly.Internal.Data.MutArray.Type
    -- * MutArray module
    , sliceIndexerFromLen
    , slicerFromLen
    , compactChunksLE
    -- * Unboxed IORef
    , module Streamly.Internal.Data.IORef.Unboxed

    -- * Deprecated
    , genSlicesFromLen
    , getSlicesFromLen
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

-- import qualified Streamly.Internal.Data.Stream.Nesting as Stream
-- import qualified Streamly.Internal.Data.Stream.Transform as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Prelude hiding (foldr, length, read, splitAt)
import Streamly.Internal.Data.MutArray.Type
import Streamly.Internal.Data.IORef.Unboxed

-- | Generate a stream of array slice descriptors ((index, len)) of specified
-- length from an array, starting from the supplied array index. The last slice
-- may be shorter than the requested length depending on the array length.
--
-- /Pre-release/
{-# INLINE sliceIndexerFromLen #-}
sliceIndexerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (Int, Int)
sliceIndexerFromLen from len =
    let fromThenTo n = (from, from + len, n - 1)
        mkSlice n i = return (i, min len (n - i))
     in Unfold.lmap length
        $ Unfold.mapM2 mkSlice
        $ Unfold.lmap fromThenTo Unfold.enumerateFromThenTo

{-# DEPRECATED genSlicesFromLen "Please use sliceIndexerFromLen instead." #-}
genSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (Int, Int)
genSlicesFromLen = sliceIndexerFromLen

-- | Generate a stream of slices of specified length from an array, starting
-- from the supplied array index. The last slice may be shorter than the
-- requested length depending on the array length.
--
-- /Pre-release/
{-# INLINE slicerFromLen #-}
slicerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (MutArray a)
slicerFromLen from len =
    let mkSlice arr (i, n) = return $ getSliceUnsafe i n arr
     in Unfold.mapM2 mkSlice (sliceIndexerFromLen from len)

{-# DEPRECATED getSlicesFromLen "Please use slicerFromLen instead." #-}
getSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (MutArray a)
getSlicesFromLen = slicerFromLen

-- | Scan @compactChunksLE n@ coalesces adjacent arrays in the input stream
-- only if the combined size would be less than or equal to n.
{-# INLINE compactChunksLE #-}
compactChunksLE :: (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (MutArray a)
-- compactLE n = Stream.catRights . Stream.parseManyD (pCompactChunksLE n)
compactChunksLE = rCompactChunksLE
