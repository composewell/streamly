-- |
-- Module      : Streamly.Internal.Data.MutArray
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- XXX To detect array overflow issues we can have a debug mode in RTS where we
-- allocate one additional page beyond a large allocation and unmap that page
-- so that we get segfault if it is accessed. Also any unpinned large
-- allocations can be kept unmapped for a while after being freed in case those
-- are being used by someone, also we can aggressively move such pages to
-- detect problems more quickly..
--
module Streamly.Internal.Data.MutArray
    (
    -- * MutArray.Type module
      module Streamly.Internal.Data.MutArray.Type
    -- * MutArray module
    , sliceIndexerFromLen
    , slicerFromLen
    , compactLE
    , pinnedCompactLE
    , compactOnByte
    , compactOnByteSuffix
    -- * Unboxed IORef
    , module Streamly.Internal.Data.IORef.Unboxed

    -- * Deprecated
    , genSlicesFromLen
    , getSlicesFromLen
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Streamly.Internal.Data.MutByteArray.Type (PinnedState(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.Nesting as Stream
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

-------------------------------------------------------------------------------
-- Compacting Streams of Arrays
-------------------------------------------------------------------------------

-- | @compactLE maxElems@ coalesces adjacent arrays in the input stream
-- only if the combined size would be less than or equal to @maxElems@
-- elements. Note that it won't split an array if the original array is already
-- larger than maxElems.
--
-- @maxElems@ must be greater than 0.
--
-- Generates unpinned arrays irrespective of the pinning status of input
-- arrays.
{-# INLINE compactLE #-}
compactLE :: (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (MutArray a)
-- XXX compactLE can be moved to MutArray/Type if we are not using the parser
-- to implement it.
compactLE = compactLeAs Unpinned
-- The parser version turns out to be a little bit slower.
-- compactLE n = Stream.catRights . Stream.parseManyD (pCompactLE n)

-- | Pinned version of 'compactLE'.
{-# INLINE pinnedCompactLE #-}
pinnedCompactLE :: forall m a. (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> Stream m (MutArray a)
pinnedCompactLE = compactLeAs Pinned
-- pinnedCompactLE n = Stream.catRights . Stream.parseManyD (pPinnedCompactLE n)

data SplitState s arr
    = Initial s
    | Buffering s arr
    | Splitting s arr
    | Yielding arr (SplitState s arr)
    | Finishing

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
{-# INLINE_NORMAL _compactOnByteCustom #-}
_compactOnByteCustom
    :: MonadIO m
    => Word8
    -> Stream m (MutArray Word8)
    -> Stream m (MutArray Word8)
_compactOnByteCustom byte (Stream.Stream step state) =
    Stream.Stream step' (Initial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Initial st) = do
        r <- step gst st
        case r of
            Stream.Yield arr s -> do
                (arr1, marr2) <- breakOn byte arr
                return $ case marr2 of
                    Nothing   -> Stream.Skip (Buffering s arr1)
                    Just arr2 -> Stream.Skip (Yielding arr1 (Splitting s arr2))
            Stream.Skip s -> return $ Stream.Skip (Initial s)
            Stream.Stop -> return Stream.Stop

    step' gst (Buffering st buf) = do
        r <- step gst st
        case r of
            Stream.Yield arr s -> do
                (arr1, marr2) <- breakOn byte arr
                -- XXX Use spliceExp instead and then rightSize?
                buf1 <- splice buf arr1
                return $ case marr2 of
                    Nothing -> Stream.Skip (Buffering s buf1)
                    Just x -> Stream.Skip (Yielding buf1 (Splitting s x))
            Stream.Skip s -> return $ Stream.Skip (Buffering s buf)
            Stream.Stop -> return $
                if byteLength buf == 0
                then Stream.Stop
                else Stream.Skip (Yielding buf Finishing)

    step' _ (Splitting st buf) = do
        (arr1, marr2) <- breakOn byte buf
        return $ case marr2 of
                Nothing -> Stream.Skip $ Buffering st arr1
                Just arr2 -> Stream.Skip $ Yielding arr1 (Splitting st arr2)

    step' _ (Yielding arr next) = return $ Stream.Yield arr next
    step' _ Finishing = return Stream.Stop

-- XXX implement predicate based version of this
-- XXX Naming of predicate based vs custom version

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
{-# INLINE compactOnByte #-}
compactOnByte
    :: (MonadIO m)
    => Word8
    -> Stream m (MutArray Word8)
    -> Stream m (MutArray Word8)
-- XXX compare perf of custom vs idiomatic version
-- compactOnByte = _compactOnByteCustom
-- XXX use spliceExp and rightSize?
compactOnByte byte = Stream.splitInnerBy (breakOn byte) splice

-- | Like 'compactOnByte' considers the separator in suffix position instead of
-- infix position.
{-# INLINE compactOnByteSuffix #-}
compactOnByteSuffix
    :: (MonadIO m)
    => Word8
    -> Stream m (MutArray Word8)
    -> Stream m (MutArray Word8)
compactOnByteSuffix byte =
        -- XXX use spliceExp and rightSize?
        Stream.splitInnerBySuffix
            (\arr -> byteLength arr == 0) (breakOn byte) splice
