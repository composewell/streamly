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
-- detect problems more quickly.
--
module Streamly.Internal.Data.MutArray
    (
    -- * MutArray.Type module
      module Streamly.Internal.Data.MutArray.Type
    -- * MutArray module
    , indexerFromLen
    , splitterFromLen
    -- , splitFromLen
    -- , slicesOf
    , compactMax
    , compactMax'
    , compactSepByByte_
    , compactEndByByte_
    , compactEndByLn_
    , createOfLast
    -- * Unboxed IORef
    , module Streamly.Internal.Data.IORef.Unboxed

    -- XXX Do not expose these yet, we should perhaps expose only the Get/Put
    -- monads instead? Decide after implementing the monads.

    -- * Serialization
    , serialize
    , deserialize
    , serializePtrN
    , deserializePtrN

    -- * Deprecated
    , slicerFromLen
    , sliceIndexerFromLen
    , genSlicesFromLen
    , getSlicesFromLen
    , compactLE
    , pinnedCompactLE
    , compactOnByte
    , compactOnByteSuffix
    )
where

#include "assert.hs"
#include "deprecation.h"
#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Streamly.Internal.Data.MutByteArray.Type (PinnedState(..))
import Streamly.Internal.Data.Serialize.Type (Serialize)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Fold.Type (Fold)

import qualified Streamly.Internal.Data.RingArray as RingArray
import qualified Streamly.Internal.Data.Serialize.Type as Serialize
import qualified Streamly.Internal.Data.Stream.Nesting as Stream
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Fold.Type as Fold
-- import qualified Streamly.Internal.Data.Stream.Transform as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Prelude hiding (foldr, length, read, splitAt)
import Streamly.Internal.Data.MutArray.Type
import Streamly.Internal.Data.IORef.Unboxed hiding (mutVar', emptyMutVar')

-- | Generate a stream of array slice descriptors ((index, len)) of specified
-- length from an array, starting from the supplied array index. The last slice
-- may be shorter than the requested length depending on the array length.
--
-- /Pre-release/
{-# INLINE indexerFromLen #-}
indexerFromLen, sliceIndexerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (Int, Int)
indexerFromLen from len =
    let fromThenTo n = (from, from + len, n - 1)
        mkSlice n i = return (i, min len (n - i))
     in Unfold.lmap length
        $ Unfold.mapM2 mkSlice
        $ Unfold.lmap fromThenTo Unfold.enumerateFromThenTo
RENAME(sliceIndexerFromLen,indexerFromLen)

{-# DEPRECATED genSlicesFromLen "Please use indexerFromLen instead." #-}
genSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (Int, Int)
genSlicesFromLen = indexerFromLen

-- | Generate a stream of slices of specified length from an array, starting
-- from the supplied array index. The last slice may be shorter than the
-- requested length depending on the array length.
--
-- /Pre-release/
{-# INLINE splitterFromLen #-}
splitterFromLen, slicerFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (MutArray a)
splitterFromLen from len =
    let mkSlice arr (i, n) = return $ unsafeSliceOffLen i n arr
     in Unfold.mapM2 mkSlice (indexerFromLen from len)
RENAME(slicerFromLen,splitterFromLen)

{-# DEPRECATED getSlicesFromLen "Please use splitterFromLen instead." #-}
getSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (MutArray a) (MutArray a)
getSlicesFromLen = splitterFromLen

--------------------------------------------------------------------------------
-- Serialization/Deserialization using Serialize
--------------------------------------------------------------------------------

{-# INLINE unsafeSerialize #-}
unsafeSerialize :: (MonadIO m, Serialize a) =>
    MutArray Word8 -> a -> m (MutArray Word8)
unsafeSerialize (MutArray mbarr start end bound) a = do
#ifdef DEBUG
    let len = Serialize.addSizeTo 0 a
    assertM(bound - end >= len)
#endif
    off <- liftIO $ Serialize.serializeAt end mbarr a
    pure $ MutArray mbarr start off bound

{-# NOINLINE serializeRealloc #-}
serializeRealloc :: forall m a. (MonadIO m, Serialize a) =>
       (Int -> Int)
    -> MutArray Word8
    -> a
    -> m (MutArray Word8)
serializeRealloc sizer arr x = do
    let len = Serialize.addSizeTo 0 x
    arr1 <- liftIO $ reallocBytesWith "serializeRealloc" sizer len arr
    unsafeSerialize arr1 x

{-# INLINE serializeWith #-}
serializeWith :: forall m a. (MonadIO m, Serialize a) =>
       (Int -> Int)
    -> MutArray Word8
    -> a
    -> m (MutArray Word8)
serializeWith sizer arr@(MutArray mbarr start end bound) x = do
    let len = Serialize.addSizeTo 0 x
    if (bound - end) >= len
    then do
        off <- liftIO $ Serialize.serializeAt end mbarr x
        assertM(len <= off)
        pure $ MutArray mbarr start off bound
    -- XXX this will inhibit unboxing?
    else serializeRealloc sizer arr x

-- | Serializes a (Ptr, len) pair in the same way as an array. The serialized
-- value can be de-serialized as an array or consumed as a pointer using
-- deserializePtrN.
--
-- The Ptr must be pinned or the existence of the Ptr must be ensured by the
-- user of this API.
--
-- /Unimplemented/
{-# INLINE serializePtrN #-}
serializePtrN :: -- (MonadIO m) =>
    MutArray Word8 -> Ptr a -> Int -> m (MutArray Word8)
-- assert/error out if Ptr is not pinned. unsafe prefix?
-- First serialize the length and then splice the ptr
serializePtrN _arr _ptr _len = undefined

-- | Consume a serialized array or (Ptr, length) from the MutArray using an IO
-- action that consumes the pointer directly.
--
-- WARNING! The array must be a pinned array.
--
-- /Unimplemented/
{-# INLINE deserializePtrN #-}
deserializePtrN :: -- (MonadIO m) =>
    MutArray Word8 -> (Ptr a -> Int -> m b) -> m (a, MutArray Word8)
-- assert/error out if the array is not pinned. unsafe prefix?
deserializePtrN _arr _action = undefined

-- | Serialize the supplied Haskell value at the end of the mutable array,
-- growing the array size. If there is no reserve capacity left in the array
-- the array is reallocated to double the current size.
--
-- Like 'snoc' except that the value is serialized to the byte array.
--
-- Note: If you are serializing a large number of small fields, and the types
-- are statically known, then it may be more efficient to declare a record of
-- those fields and derive an 'Serialize' instance of the entire record.
--
-- /Unstable API/
{-# INLINE serialize #-}
serialize :: forall m a. (MonadIO m, Serialize a) =>
    MutArray Word8 -> a -> m (MutArray Word8)
serialize = serializeWith f

    where

    f oldSize =
        if isPower2 oldSize
        then oldSize * 2
        else roundUpToPower2 oldSize * 2

-- | Deserialize a Haskell value from the beginning of a mutable array. The
-- deserialized value is removed from the array and the remaining array is
-- returned.
--
-- Like 'uncons' except that the value is deserialized from the byte array.
--
-- Note: If you are deserializing a large number of small fields, and the types
-- are statically known, then it may be more efficient to declare a record of
-- those fields and derive 'Serialize' instance of the entire record.
--
-- /Unstable API/
{-# INLINE deserialize #-}
deserialize :: (MonadIO m, Serialize a) =>
    MutArray Word8 -> m (a, MutArray Word8)
deserialize arr@(MutArray {..}) = do
    let lenArr = byteLength arr
    (off, val) <-
        liftIO $ Serialize.deserializeAt arrStart arrContents (arrStart + lenArr)
    assertM(off <= arrStart + lenArr)
    pure (val, MutArray arrContents off arrEnd arrBound)

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
{-# INLINE compactMax #-}
compactMax, compactLE :: (MonadIO m, Unbox a) =>
    Int -> Stream m (MutArray a) -> Stream m (MutArray a)
-- XXX compactLE can be moved to MutArray/Type if we are not using the parser
-- to implement it.
compactMax = compactLeAs Unpinned
-- The parser version turns out to be a little bit slower.
-- compactLE n = Stream.catRights . Stream.parseManyD (pCompactLE n)

RENAME(compactLE,compactMax)

-- | Like 'compactBySizeLE' but generates pinned arrays.
{-# INLINE_NORMAL compactMax' #-}
compactMax', pinnedCompactLE :: forall m a. (MonadIO m, Unbox a)
    => Int -> Stream m (MutArray a) -> Stream m (MutArray a)
compactMax' = compactLeAs Pinned
-- compactMax' n = Stream.catRights . Stream.parseManyD (pPinnedCompactLE n)

{-# DEPRECATED pinnedCompactLE "Please use compactMax' instead." #-}
{-# INLINE pinnedCompactLE #-}
pinnedCompactLE = compactMax'

data SplitState s arr
    = Initial s
    | Buffering s arr
    | Splitting s arr
    | Yielding arr (SplitState s arr)
    | Finishing

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
{-# INLINE_NORMAL _compactSepByByteCustom #-}
_compactSepByByteCustom
    :: MonadIO m
    => Word8
    -> Stream m (MutArray Word8)
    -> Stream m (MutArray Word8)
_compactSepByByteCustom byte (Stream.Stream step state) =
    Stream.Stream step' (Initial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Initial st) = do
        r <- step gst st
        case r of
            Stream.Yield arr s -> do
                (arr1, marr2) <- breakEndByWord8_ byte arr
                return $ case marr2 of
                    Nothing   -> Stream.Skip (Buffering s arr1)
                    Just arr2 -> Stream.Skip (Yielding arr1 (Splitting s arr2))
            Stream.Skip s -> return $ Stream.Skip (Initial s)
            Stream.Stop -> return Stream.Stop

    step' gst (Buffering st buf) = do
        r <- step gst st
        case r of
            Stream.Yield arr s -> do
                (arr1, marr2) <- breakEndByWord8_ byte arr
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
        (arr1, marr2) <- breakEndByWord8_ byte buf
        return $ case marr2 of
                Nothing -> Stream.Skip $ Buffering st arr1
                Just arr2 -> Stream.Skip $ Yielding arr1 (Splitting st arr2)

    step' _ (Yielding arr next) = return $ Stream.Yield arr next
    step' _ Finishing = return Stream.Stop

-- XXX implement predicate based version of this compactSepBy_, compactEndBy_
-- XXX the versions that use equality can be named compactSepByElem_ etc. The
-- byte/word etc versions of that can be specialized using rewrite rules.

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
{-# INLINE compactSepByByte_ #-}
compactSepByByte_, compactOnByte
    :: (MonadIO m)
    => Word8
    -> Stream m (MutArray Word8)
    -> Stream m (MutArray Word8)
-- XXX compare perf of custom vs idiomatic version
-- compactOnByte = _compactOnByteCustom
-- XXX use spliceExp and rightSize?
compactSepByByte_ byte = Stream.splitInnerBy (breakEndByWord8_ byte) splice

RENAME(compactOnByte,compactSepByByte_)

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
{-# INLINE compactEndByByte_ #-}
compactEndByByte_, compactOnByteSuffix
    :: (MonadIO m)
    => Word8
    -> Stream m (MutArray Word8)
    -> Stream m (MutArray Word8)
compactEndByByte_ byte =
        -- XXX use spliceExp and rightSize?
        Stream.splitInnerBySuffix
            (\arr -> byteLength arr == 0) (breakEndByWord8_ byte) splice

RENAME(compactOnByteSuffix,compactEndByByte_)

-- XXX On windows we should compact on "\r\n". We can just compact on '\n' and
-- drop the last byte in each array if it is '\r'.

-- | Compact byte arrays on newline character, dropping the newline char.
{-# INLINE compactEndByLn_ #-}
compactEndByLn_ :: MonadIO m
    => Stream m (MutArray Word8)
    -> Stream m (MutArray Word8)
compactEndByLn_ = compactEndByByte_ 10

-- | @createOfLast n@ folds a maximum of @n@ elements from the end of the input
-- stream to an 'MutArray'.
--
{-# INLINE createOfLast #-}
createOfLast :: (Unbox a, MonadIO m) => Int -> Fold m a (MutArray a)
createOfLast n =
    Fold.ifThen
        (pure (n <= 0))
        (Fold.fromPure empty)
        (Fold.rmapM RingArray.toMutArray $ RingArray.createOfLast n)
