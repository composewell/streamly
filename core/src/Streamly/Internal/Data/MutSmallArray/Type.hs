{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Streamly.Internal.Data.MutSmallArray.Type
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.MutSmallArray.Type
    (
    -- ** Type
      MutArray (..)
      {-
    , pin
    , unpin
    , isPinned
    -}

    -- -- * Constructing and Writing
    -- ** Construction
    , empty

    -- *** Uninitialized Arrays
    {-
    , pinnedNew
    , pinnedNewBytes
    , pinnedNewAligned
    -}
    , new
    , newArrayWith

    -- *** From streams
    , unsafeCreateOfWith
    , createOfWith
    -- , unsafeCreateOf
    -- , pinnedWriteNUnsafe
    , createOf
    {-
    , pinnedWriteN

    , writeWith
    , write
    , pinnedWrite
    , writeRevN
    -- , writeRev

    -- *** From containers
    , fromListN
    , pinnedFromListN
    , fromList
    , pinnedFromList
    , fromListRevN
    , fromListRev
    , fromStreamDN
    , fromStreamD
    , fromPureStream
    -}
    , fromPureStreamN
    , fromByteStr#

    -- ** Random writes
    , putIndex
    , putIndexUnsafe
    {-
    , putIndices
    -- , putFromThenTo
    -- , putFrom -- start writing at the given position
    -- , putUpto -- write from beginning up to the given position
    -- , putFromTo
    -- , putFromRev
    -- , putUptoRev
    , modifyIndexUnsafe
    , modifyIndex
    , modifyIndices
    , modify
    , swapIndices
    , unsafeSwapIndices
    -}

    -- ** Eliminating and Reading

    -- *** To streams
    , reader
    {-
    , readerRevWith
    , readerRev

    -- *** To containers
    , toStreamDWith
    , toStreamDRevWith
    , toStreamKWith
    , toStreamKRevWith
    -}
    , read
    {-
    , readRev
    , toStreamK
    , toStreamKRev
    , toList
    -}

    -- experimental
    , producerWith
    , producer

    {-
    -- *** Random reads
    , getIndex
    , getIndexUnsafe
    , getIndices
    , getIndicesD
    -- , getFromThenTo
    , getIndexRev
    -}

    -- ** Size
    , length
    , byteLength

    -- ** Casting
    , cast
    , castUnsafe
    , asBytes
    , asPtrUnsafe
    {-

    -- ** Folding
    , foldl'
    , foldr
    , cmp

    -- ** Arrays of arrays
    --  We can add dimensionality parameter to the array type to get
    --  multidimensional arrays. Multidimensional arrays would just be a
    --  convenience wrapper on top of single dimensional arrays.

    -- | Operations dealing with multiple arrays, streams of arrays or
    -- multidimensional array representations.

    -- *** Construct from streams
    , chunksOf
    , pinnedChunksOf
    , writeChunks

    -- *** Eliminate to streams
    , flattenArrays
    , flattenArraysRev
    , fromArrayStreamK

    -- *** Construct from arrays
    -- get chunks without copying
    , getSliceUnsafe
    , getSlice
    -- , getSlicesFromLenN
    , splitAt -- XXX should be able to express using getSlice
    , breakOn

    -- ** Cloning arrays
    , clone
    , pinnedClone
    -}

    -- ** Appending arrays
    , putSliceUnsafe
    , toPinnedCString
    , splice
    )
where

#include "assert.hs"
#include "inline.hs"
#include "ArrayMacros.h"
#include "MachDeps.h"

-- import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Bits (shiftR, (.|.), (.&.))
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Foreign.C.Types (CSize(..))
-- import Foreign.Ptr (plusPtr, minusPtr, nullPtr)
import Streamly.Internal.Data.MutByteArray.Type
    ( MutByteArray(..)
    , PinnedState(..)
    )
import Streamly.Internal.Data.Unbox (Unbox(..))
import System.IO.Unsafe (unsafePerformIO)
{-
import GHC.Base
    ( IO(..)
    , Int(..)
    , compareByteArrays#
    , copyMutableByteArray#
    )
-}
-- import GHC.Base (noinline)
import GHC.Exts (Addr#)
import GHC.Ptr (Ptr(..))

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.Stream.Type (Stream)
-- import Streamly.Internal.Data.StreamK.Type (StreamK)
-- import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
-- import Streamly.Internal.System.IO (arrayPayloadSize, defaultChunkSize)

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.MutByteArray.Type as MutByteArray
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.Type as D
import qualified Streamly.Internal.Data.Stream.Lift as D
-- import qualified Streamly.Internal.Data.StreamK.Type as K
-- import qualified Prelude

import Prelude hiding
    (Foldable(..), read, unlines, splitAt, reverse, truncate)

#include "DocTestDataMutArray.hs"

-------------------------------------------------------------------------------
-- MutArray Data Type
-------------------------------------------------------------------------------

newtype MutArray a = MutArray MutByteArray

{-
-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

-- | Return a copy of the array in pinned memory if unpinned, else return the
-- original array.
{-# INLINE pin #-}
pin :: MutArray a -> IO (MutArray a)
pin arr@MutArray{..} =
    if Unboxed.isPinned arrContents
    then pure arr
    else pinnedClone arr

-- | Return a copy of the array in unpinned memory if pinned, else return the
-- original array.
{-# INLINE unpin #-}
unpin :: MutArray a -> IO (MutArray a)
unpin arr@MutArray{..} =
    if Unboxed.isPinned arrContents
    then clone arr
    else pure arr

-- | Return 'True' if the array is allocated in pinned memory.
{-# INLINE isPinned #-}
isPinned :: MutArray a -> Bool
isPinned MutArray{..} = Unboxed.isPinned arrContents
-}

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

empty :: MutArray a
empty = MutArray MutByteArray.nil

-- | @newArrayWith allocator alignment count@ allocates a new array of zero
-- length and with a capacity to hold @count@ elements, using @allocator
-- size alignment@ as the memory allocator function.
--
-- Alignment must be greater than or equal to machine word size and a power of
-- 2.
--
-- Alignment is ignored if the allocator allocates unpinned memory.
--
-- /Pre-release/
{-# INLINE newArrayWith #-}
newArrayWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> Int -> m MutByteArray) -> Int -> Int -> m (MutArray a)
newArrayWith alloc alignSize count = do
    let size = max (count * SIZE_OF(a)) 0
    contents <- alloc size alignSize
    return $ MutArray contents

{-
{-# INLINE newBytesAs #-}
newBytesAs :: MonadIO m => PinnedState -> Int -> m (MutArray a)
newBytesAs ps bytes = do
    contents <- liftIO $ MutByteArray.newBytesAs ps bytes
    return $ MutArray contents

-- | Allocates a pinned empty array that can hold 'count' items.  The memory of
-- the array is uninitialized and the allocation is aligned as per the
-- 'Unboxed' instance of the type.
--
-- /Pre-release/
{-# INLINE pinnedNewBytes #-}
pinnedNewBytes :: MonadIO m =>
#ifdef DEVBUILD
    Unbox a =>
#endif
    Int -> m (MutArray a)
pinnedNewBytes = newBytesAs Pinned

-- | Like 'newArrayWith' but using an allocator is a pinned memory allocator and
-- the alignment is dictated by the 'Unboxed' instance of the type.
--
-- /Internal/
{-# INLINE pinnedNewAligned #-}
pinnedNewAligned :: (MonadIO m, Unbox a) => Int -> Int -> m (MutArray a)
pinnedNewAligned =
    newArrayWith (\s a -> liftIO $ Unboxed.pinnedNewAlignedBytes s a)
-}

{-# INLINE newAs #-}
newAs :: (MonadIO m, Unbox a) => PinnedState -> Int -> m (MutArray a)
newAs ps =
    newArrayWith
        (\s _ -> liftIO $ MutByteArray.newBytesAs ps s)
        (error "new: alignment is not used in unpinned arrays.")

{-
-- XXX can unaligned allocation be more efficient when alignment is not needed?
--
-- | Allocates an empty pinned array that can hold 'count' items.  The memory of
-- the array is uninitialized and the allocation is aligned as per the 'Unboxed'
-- instance of the type.
--
{-# INLINE pinnedNew #-}
pinnedNew :: forall m a. (MonadIO m, Unbox a) => Int -> m (MutArray a)
pinnedNew = newAs Pinned
-}

-- | Allocates an empty unpinned array that can hold 'count' items.  The memory
-- of the array is uninitialized.
--
{-# INLINE new #-}
new :: (MonadIO m, Unbox a) => Int -> m (MutArray a)
new = newAs Unpinned

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

-- | Write the given element to the given index of the array. Does not check if
-- the index is out of bounds of the array.
--
-- /Pre-release/
{-# INLINE putIndexUnsafe #-}
putIndexUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> MutArray a -> a -> m ()
putIndexUnsafe i (MutArray contents) x = do
    let index = INDEX_OF(0, i, a)
    assert (i >= 0 && INDEX_VALID(index, undefined, a)) (return ()) -- arrEND
    liftIO $ pokeAt index contents  x

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex ix arr val = MutArray.modifyIndex ix arr (const (val, ()))
-- >>> f = MutArray.putIndices
-- >>> putIndex ix arr val = Stream.fold (f arr) (Stream.fromPure (ix, val))
--
{-# INLINE putIndex #-}
putIndex :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> a -> m ()
putIndex i (MutArray contents) x = do
    let index = INDEX_OF(0,i,a)
    if i >= 0 && INDEX_VALID(index,undefined,a) -- arrEnd
    then liftIO $ pokeAt index contents  x
    else invalidIndex "putIndex" i

{-
-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: forall m a. (MonadIO m, Unbox a)
    => MutArray a -> Fold m (Int, a) ()
putIndices arr = FL.foldlM' step (return ())

    where

    step () (i, x) = liftIO (putIndex i arr x)

-- | Modify a given index of an array using a modifier function.
--
-- Unsafe because it does not check the bounds of the array.
--
-- /Pre-release/
modifyIndexUnsafe :: forall m a b. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> (a -> (a, b)) -> m b
modifyIndexUnsafe i MutArray{..} f = liftIO $ do
        let index = INDEX_OF(arrStart,i,a)
        assert (i >= 0 && INDEX_NEXT(index,a) <= arrEnd) (return ())
        r <- peekAt index arrContents
        let (x, res) = f r
        pokeAt index arrContents  x
        return res

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndex :: forall m a b. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> (a -> (a, b)) -> m b
modifyIndex i MutArray{..} f = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ do
        r <- peekAt index arrContents
        let (x, res) = f r
        pokeAt index arrContents  x
        return res
    else invalidIndex "modifyIndex" i

-- | Modify the array indices generated by the supplied stream.
--
-- /Pre-release/
{-# INLINE modifyIndices #-}
modifyIndices :: forall m a . (MonadIO m, Unbox a)
    => MutArray a -> (Int -> a -> a) -> Fold m Int ()
modifyIndices arr f = FL.foldlM' step initial

    where

    initial = return ()

    step () i =
        let f1 x = (f i x, ())
         in modifyIndex i arr f1

-- | Modify each element of an array using the supplied modifier function.
--
-- This is an in-place equivalent of an immutable map operation.
--
-- /Pre-release/
modify :: forall m a. (MonadIO m, Unbox a)
    => MutArray a -> (a -> a) -> m ()
modify MutArray{..} f = liftIO $
    go arrStart

    where

    go i =
        when (INDEX_VALID(i,arrEnd,a)) $ do
            r <- peekAt i arrContents
            pokeAt i arrContents (f r)
            go (INDEX_NEXT(i,a))

-- XXX We could specify the number of bytes to swap instead of Proxy. Need
-- to ensure that the memory does not overlap.
{-# INLINE swapArrayByteIndices #-}
swapArrayByteIndices ::
       forall a. Unbox a
    => Proxy a
    -> MutByteArray
    -> Int
    -> Int
    -> IO ()
swapArrayByteIndices _ arrContents i1 i2 = do
    r1 <- peekAt i1 arrContents
    r2 <- peekAt i2 arrContents
    pokeAt i1 arrContents (r2 :: a)
    pokeAt i2 arrContents (r1 :: a)

-- | Swap the elements at two indices without validating the indices.
--
-- /Unsafe/: This could result in memory corruption if indices are not valid.
--
-- /Pre-release/
{-# INLINE unsafeSwapIndices #-}
unsafeSwapIndices :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> MutArray a -> m ()
unsafeSwapIndices i1 i2 MutArray{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        swapArrayByteIndices (Proxy :: Proxy a) arrContents t1 t2

-- | Swap the elements at two indices.
--
-- /Pre-release/
swapIndices :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> MutArray a -> m ()
swapIndices i1 i2 MutArray{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        when (i1 < 0 || INDEX_INVALID(t1,arrEnd,a))
            $ invalidIndex "swapIndices" i1
        when (i2 < 0 || INDEX_INVALID(t2,arrEnd,a))
            $ invalidIndex "swapIndices" i2
        swapArrayByteIndices (Proxy :: Proxy a) arrContents t1 t2

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- XXX Can this be deduplicated with array/foreign

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m a
getIndexUnsafe i MutArray{..} = do
    let index = INDEX_OF(arrStart,i,a)
    assert (i >= 0 && INDEX_VALID(index,arrEnd,a)) (return ())
    liftIO $ peekAt index arrContents

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m (Maybe a)
getIndex i MutArray{..} = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ Just <$> peekAt index arrContents
    else return Nothing

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m a
getIndexRev i MutArray{..} = do
    let index = RINDEX_OF(arrEnd,i,a)
    if i >= 0 && index >= arrStart
    then liftIO $ peekAt index arrContents
    else invalidIndex "getIndexRev" i

data GetIndicesState contents start end st =
    GetIndicesState contents start end st

-- | Given an unfold that generates array indices, read the elements on those
-- indices from the supplied MutArray. An error is thrown if an index is out of
-- bounds.
--
-- /Pre-release/
{-# INLINE getIndicesD #-}
getIndicesD :: (Monad m, Unbox a) =>
    (forall b. IO b -> m b) -> D.Stream m Int -> Unfold m (MutArray a) a
getIndicesD liftio (D.Stream stepi sti) = Unfold step inject

    where

    inject (MutArray contents start end _) =
        return $ GetIndicesState contents start end sti

    {-# INLINE_LATE step #-}
    step (GetIndicesState contents start end st) = do
        r <- stepi defState st
        case r of
            D.Yield i s -> do
                x <- liftio $ getIndex i (MutArray contents start end undefined)
                case x of
                    Just v -> return $ D.Yield v (GetIndicesState contents start end s)
                    Nothing -> error "Invalid Index"
            D.Skip s -> return $ D.Skip (GetIndicesState contents start end s)
            D.Stop -> return D.Stop

-------------------------------------------------------------------------------
-- Subarrays
-------------------------------------------------------------------------------

-- XXX We can also get immutable slices.

-- | /O(1)/ Slice an array in constant time.
--
-- Unsafe: The bounds of the slice are not checked.
--
-- /Unsafe/
--
-- /Pre-release/
{-# INLINE getSliceUnsafe #-}
getSliceUnsafe :: forall a. Unbox a
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
getSliceUnsafe index len (MutArray contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in assert
            (index >= 0 && len >= 0 && end <= e)
            -- Note: In a slice we always use bound = end so that the slice
            -- user cannot overwrite elements beyond the end of the slice.
            (MutArray contents fp1 end end)

-- | /O(1)/ Slice an array in constant time. Throws an error if the slice
-- extends out of the array bounds.
--
-- /Pre-release/
{-# INLINE getSlice #-}
getSlice :: forall a. Unbox a =>
       Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
getSlice index len (MutArray contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in if index >= 0 && len >= 0 && end <= e
        -- Note: In a slice we always use bound = end so that the slice user
        -- cannot overwrite elements beyond the end of the slice.
        then MutArray contents fp1 end end
        else error
                $ "getSlice: invalid slice, index "
                ++ show index ++ " length " ++ show len
-}

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

-- We do not realloc the underlying MutByteArray, therefore, the length cannot
-- change, it is immutable.

-- | /O(1)/ Get the byte length of the array.
--
{-# INLINE byteLength #-}
byteLength :: MutArray a -> Int
byteLength (MutArray contents) =
    unsafePerformIO $ MutByteArray.sizeOfMutableByteArray contents

-- Note: try to avoid the use of length in performance sensitive internal
-- routines as it involves a costly 'div' operation. Instead use the end ptr
-- in the array to check the bounds etc.
--
-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- Note that 'byteLength' is less expensive than this operation, as 'length'
-- involves a costly division operation.
--
{-# INLINE length #-}
length :: forall a. Unbox a => MutArray a -> Int
length arr = do
    let blen = byteLength arr
        elemSize = SIZE_OF(a)
     in assert (blen `mod` elemSize == 0) (blen `div` elemSize)

{-
-------------------------------------------------------------------------------
-- Streams of arrays - Creation
-------------------------------------------------------------------------------

data GroupState s contents start end bound
    = GroupStart s
    | GroupBuffer s contents start end bound
    | GroupYield
        contents start end bound (GroupState s contents start end bound)
    | GroupFinish

{-# INLINE_NORMAL chunksOfAs #-}
chunksOfAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> D.Stream m a -> D.Stream m (MutArray a)
chunksOfAs ps n (D.Stream step state) =
    D.Stream step' (GroupStart state)

    where

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.MutArray.Mut.Type.chunksOf: "
                    ++ "the size of arrays [" ++ show n
                    ++ "] must be a natural number"
        (MutArray contents start end bound :: MutArray a) <- newAs ps n
        return $ D.Skip (GroupBuffer st contents start end bound)

    step' gst (GroupBuffer st contents start end bound) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                liftIO $ pokeAt end contents  x
                let end1 = INDEX_NEXT(end,a)
                return $
                    if end1 >= bound
                    then D.Skip
                            (GroupYield
                                contents start end1 bound (GroupStart s))
                    else D.Skip (GroupBuffer s contents start end1 bound)
            D.Skip s ->
                return $ D.Skip (GroupBuffer s contents start end bound)
            D.Stop ->
                return
                    $ D.Skip (GroupYield contents start end bound GroupFinish)

    step' _ (GroupYield contents start end bound next) =
        return $ D.Yield (MutArray contents start end bound) next

    step' _ GroupFinish = return D.Stop

-- | @chunksOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- >>> chunksOf n = Stream.foldMany (MutArray.writeN n)
--
-- /Pre-release/
{-# INLINE_NORMAL chunksOf #-}
chunksOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (MutArray a)
-- XXX the idiomatic implementation leads to large regression in the D.reverse'
-- benchmark. It seems it has difficulty producing optimized code when
-- converting to StreamK. Investigate GHC optimizations.
-- chunksOf n = D.foldMany (writeN n)
chunksOf = chunksOfAs Unpinned

-- | Like 'chunksOf' but creates pinned arrays.
{-# INLINE_NORMAL pinnedChunksOf #-}
pinnedChunksOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (MutArray a)
-- pinnedChunksOf n = D.foldMany (pinnedWriteN n)
pinnedChunksOf = chunksOfAs Pinned

-- XXX This should take a PinnedState
-- XXX buffer to a list instead?
-- | Buffer the stream into arrays in memory.
{-# INLINE arrayStreamKFromStreamDAs #-}
arrayStreamKFromStreamDAs :: forall m a. (MonadIO m, Unbox a) =>
    PinnedState -> D.Stream m a -> m (StreamK m (MutArray a))
arrayStreamKFromStreamDAs ps =
    let n = allocBytesToElemCount (undefined :: a) defaultChunkSize
     in D.foldr K.cons K.nil . chunksOfAs ps n

-------------------------------------------------------------------------------
-- Streams of arrays - Flattening
-------------------------------------------------------------------------------

data FlattenState s contents a =
      OuterLoop s
    | InnerLoop s contents !Int !Int

-- | Use the "reader" unfold instead.
--
-- @flattenArrays = unfoldMany reader@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
flattenArrays (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield MutArray{..} s ->
                D.Skip (InnerLoop s arrContents arrStart arrEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p end) | assert (p <= end) (p == end) =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p end) = do
        x <- liftIO $ peekAt p contents
        return $ D.Yield x (InnerLoop st contents (INDEX_NEXT(p,a)) end)

-- | Use the "readerRev" unfold instead.
--
-- @flattenArrays = unfoldMany readerRev@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
flattenArraysRev (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield MutArray{..} s ->
                let p = INDEX_PREV(arrEnd,a)
                 in D.Skip (InnerLoop s arrContents p arrStart)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p start) | p < start =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p start) = do
        x <- liftIO $ peekAt p contents
        let cur = INDEX_PREV(p,a)
        return $ D.Yield x (InnerLoop st contents cur start)
-}

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !MutByteArray   -- contents
    {-# UNPACK #-} !Int                -- index 1
    {-# UNPACK #-} !Int                -- index 2

toArrayUnsafe :: MutArray a -> ArrayUnsafe a
toArrayUnsafe arr@(MutArray contents) = ArrayUnsafe contents 0 (byteLength arr)

fromArrayUnsafe :: ArrayUnsafe a -> MutArray a
fromArrayUnsafe (ArrayUnsafe contents _ _) = MutArray contents

{-# INLINE_NORMAL producerWith #-}
producerWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> Producer m (MutArray a) a
producerWith liftio = Producer step (return . toArrayUnsafe) extract
    where

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe _ cur end)
        | assert (cur <= end) (cur == end) = return D.Stop
    step (ArrayUnsafe contents cur end) = do
            -- When we use a purely lazy Monad like Identity, we need to force a
            -- few actions for correctness and execution order sanity. We want
            -- the peek to occur right here and not lazily at some later point
            -- because we want the peek to be ordered with respect to the touch.
            !x <- liftio $ peekAt cur contents
            return $ D.Yield x (ArrayUnsafe contents (INDEX_NEXT(cur,a)) end)

    extract = return . fromArrayUnsafe

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: forall m a. (MonadIO m, Unbox a) => Producer m (MutArray a) a
producer = producerWith liftIO

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: forall m a. (MonadIO m, Unbox a) => Unfold m (MutArray a) a
reader = Producer.simplify producer

{-
{-# INLINE_NORMAL readerRevWith #-}
readerRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> Unfold m (MutArray a) a
readerRevWith liftio = Unfold step inject
    where

    inject (MutArray contents start end _) =
        let p = INDEX_PREV(end,a)
         in return $ ArrayUnsafe contents start p

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe _ start p) | p < start = return D.Stop
    step (ArrayUnsafe contents start p) = do
        !x <- liftio $ peekAt p contents
        return $ D.Yield x (ArrayUnsafe contents start (INDEX_PREV(p,a)))

-- | Unfold an array into a stream in reverse order.
--
{-# INLINE_NORMAL readerRev #-}
readerRev :: forall m a. (MonadIO m, Unbox a) => Unfold m (MutArray a) a
readerRev = readerRevWith liftIO

-------------------------------------------------------------------------------
-- to Lists and streams
-------------------------------------------------------------------------------

{-
-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Unbox a => (a -> b -> b) -> b -> MutArray a -> b
toListFB c n MutArray{..} = go arrStart
    where

    go p | assert (p <= arrEnd) (p == arrEnd) = n
    go p =
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        -- XXX
        let !x = unsafeInlineIO $ do
                    r <- peekAt arrContents p
                    return r
        in c x (go (PTR_NEXT(p,a)))
-}

-- XXX Monadic foldr/build fusion?
-- Reference: https://www.researchgate.net/publication/220676509_Monadic_augment_and_generalised_short_cut_fusion

-- | Convert a 'MutArray' into a list.
--
{-# INLINE toList #-}
toList :: forall m a. (MonadIO m, Unbox a) => MutArray a -> m [a]
toList MutArray{..} = liftIO $ go arrStart
    where

    go p | assert (p <= arrEnd) (p == arrEnd) = return []
    go p = do
        x <- peekAt p arrContents
        (:) x <$> go (INDEX_NEXT(p,a))
-}

{-# INLINE_NORMAL toStreamWith #-}
toStreamWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> D.Stream m a
toStreamWith liftio arr@(MutArray contents) = D.Stream step 0

    where

    arrEnd = byteLength arr

    {-# INLINE_LATE step #-}
    step _ p | assert (p <= arrEnd) (p == arrEnd) = return D.Stop
    step _ p = liftio $ do
        r <- peekAt p contents
        return $ D.Yield r (INDEX_NEXT(p,a))

-- | Convert a 'MutArray' into a stream.
--
-- >>> read = Stream.unfold MutArray.reader
--
{-# INLINE_NORMAL read #-}
read :: forall m a. (MonadIO m, Unbox a) => MutArray a -> D.Stream m a
read = toStreamWith liftIO

{-
{-# INLINE toStreamKWith #-}
toStreamKWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> StreamK m a
toStreamKWith liftio MutArray{..} = go arrStart

    where

    go p | assert (p <= arrEnd) (p == arrEnd) = K.nil
         | otherwise =
        let elemM = peekAt p arrContents
        in liftio elemM `K.consM` go (INDEX_NEXT(p,a))

{-# INLINE toStreamK #-}
toStreamK :: forall m a. (MonadIO m, Unbox a) => MutArray a -> StreamK m a
toStreamK = toStreamKWith liftIO

{-# INLINE_NORMAL toStreamDRevWith #-}
toStreamDRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> D.Stream m a
toStreamDRevWith liftio MutArray{..} =
    let p = INDEX_PREV(arrEnd,a)
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p < arrStart = return D.Stop
    step _ p = liftio $ do
        r <- peekAt p arrContents
        return $ D.Yield r (INDEX_PREV(p,a))

-- | Convert a 'MutArray' into a stream in reverse order.
--
-- >>> readRev = Stream.unfold MutArray.readerRev
--
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (MonadIO m, Unbox a) => MutArray a -> D.Stream m a
readRev = toStreamDRevWith liftIO

{-# INLINE toStreamKRevWith #-}
toStreamKRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> StreamK m a
toStreamKRevWith liftio MutArray {..} =
    let p = INDEX_PREV(arrEnd,a)
    in go p

    where

    go p | p < arrStart = K.nil
         | otherwise =
        let elemM = peekAt p arrContents
        in liftio elemM `K.consM` go (INDEX_PREV(p,a))

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. (MonadIO m, Unbox a) => MutArray a -> StreamK m a
toStreamKRev = toStreamKRevWith liftIO

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- XXX Need something like "MutArray m a" enforcing monadic action to avoid the
-- possibility of such APIs.
--
-- | Strict left fold of an array.
{-# INLINE_NORMAL foldl' #-}
foldl' :: (MonadIO m, Unbox a) => (b -> a -> b) -> b -> MutArray a -> m b
foldl' f z arr = D.foldl' f z $ read arr

-- | Right fold of an array.
{-# INLINE_NORMAL foldr #-}
foldr :: (MonadIO m, Unbox a) => (a -> b -> b) -> b -> MutArray a -> m b
foldr f z arr = D.foldr f z $ read arr

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- Note: Arrays may be allocated with a specific alignment at the beginning of
-- the array. If you need to maintain that alignment on reallocations then you
-- can resize the array manually before append, using an aligned resize
-- operation.

-- XXX Keep the bound intact to not lose any free space? Perf impact?

-- | @writeAppendNUnsafe n arr@ appends up to @n@ input items to the supplied
-- array.
--
-- Unsafe: Do not drive the fold beyond @n@ elements, it will lead to memory
-- corruption or segfault.
--
-- Any free space left in the array after appending @n@ elements is lost.
--
-- /Internal/
{-# INLINE_NORMAL writeAppendNUnsafe #-}
writeAppendNUnsafe :: forall m a. (MonadIO m, Unbox a) =>
       Int
    -> m (MutArray a)
    -> Fold m a (MutArray a)
writeAppendNUnsafe n action =
    fmap fromArrayUnsafe $ FL.foldlM' step initial

    where

    initial = do
        assert (n >= 0) (return ())
        arr@(MutArray _ _ end bound) <- action
        let free = bound - end
            needed = n * SIZE_OF(a)
        -- XXX We can also reallocate if the array has too much free space,
        -- otherwise we lose that space.
        arr1 <-
            if free < needed
            then noinline reallocWith "writeAppendNUnsafeWith" (+ needed) needed arr
            else return arr
        return $ toArrayUnsafe arr1

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeAt end contents x
        return $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

-- | Append @n@ elements to an existing array. Any free space left in the array
-- after appending @n@ elements is lost.
--
-- >>> writeAppendN n initial = Fold.take n (MutArray.writeAppendNUnsafe n initial)
--
{-# INLINE_NORMAL writeAppendN #-}
writeAppendN :: forall m a. (MonadIO m, Unbox a) =>
    Int -> m (MutArray a) -> Fold m a (MutArray a)
writeAppendN n initial = FL.take n (writeAppendNUnsafe n initial)

-- | @writeAppendWith realloc action@ mutates the array generated by @action@ to
-- append the input stream. If there is no reserved space available in the
-- array it is reallocated to a size in bytes  determined by @realloc oldSize@,
-- where @oldSize@ is the current size of the array in bytes.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> writeAppendWith sizer = Fold.foldlM' (MutArray.snocWith sizer)
--
-- /Pre-release/
{-# INLINE writeAppendWith #-}
writeAppendWith :: forall m a. (MonadIO m, Unbox a) =>
    (Int -> Int) -> m (MutArray a) -> Fold m a (MutArray a)
writeAppendWith sizer = FL.foldlM' (snocWith sizer)

-- | @append action@ mutates the array generated by @action@ to append the
-- input stream. If there is no reserved space available in the array it is
-- reallocated to double the size.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> writeAppend = MutArray.writeAppendWith (* 2)
--
{-# INLINE writeAppend #-}
writeAppend :: forall m a. (MonadIO m, Unbox a) =>
    m (MutArray a) -> Fold m a (MutArray a)
writeAppend = writeAppendWith (* 2)
-}

-- XXX We can carry bound as well in the state to make sure we do not lose the
-- remaining capacity. Need to check perf impact.
--
-- | Like 'writeNUnsafe' but takes a new array allocator @alloc size@ function
-- as argument.
--
-- >>> writeNWithUnsafe alloc n = MutArray.writeAppendNUnsafe (alloc n) n
--
-- /Pre-release/
{-# INLINE_NORMAL unsafeCreateOfWith #-}
unsafeCreateOfWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
unsafeCreateOfWith alloc n = MutArray . fst <$> FL.foldlM' step initial

    where

    initial = do
        MutArray contents <- alloc n
        return (contents, 0)

    step (contents, end) x = do
        liftIO $ pokeAt end contents x
        return (contents, INDEX_NEXT(end,a))

{-
{-# INLINE_NORMAL writeNUnsafeAs #-}
writeNUnsafeAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> Fold m a (MutArray a)
writeNUnsafeAs ps = writeNWithUnsafe (newAs ps)

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- >>> writeNUnsafe = MutArray.writeNWithUnsafe MutArray.new
--
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
writeNUnsafe = writeNUnsafeAs Unpinned

-- | Like 'writeNUnsafe' but creates a pinned array.
{-# INLINE_NORMAL pinnedWriteNUnsafe #-}
pinnedWriteNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
pinnedWriteNUnsafe = writeNUnsafeAs Pinned
-}

-- | @writeNWith alloc n@ folds a maximum of @n@ elements into an array
-- allocated using the @alloc@ function.
--
-- >>> writeNWith alloc n = Fold.take n (MutArray.writeNWithUnsafe alloc n)
-- >>> writeNWith alloc n = MutArray.writeAppendN (alloc n) n
--
{-# INLINE_NORMAL createOfWith #-}
createOfWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
createOfWith alloc n = FL.take n (unsafeCreateOfWith alloc n)

{-# INLINE_NORMAL createOfAs #-}
createOfAs ::
       forall m a. (MonadIO m, Unbox a)
    => PinnedState
    -> Int
    -> Fold m a (MutArray a)
createOfAs ps = createOfWith (newAs ps)

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'MutArray'.
--
-- >>> writeN = MutArray.writeNWith MutArray.new
-- >>> writeN n = Fold.take n (MutArray.writeNUnsafe n)
-- >>> writeN n = MutArray.writeAppendN n (MutArray.new n)
--
{-# INLINE_NORMAL createOf #-}
createOf :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
createOf = createOfAs Unpinned

{-
-- | Like 'writeN' but creates a pinned array.
{-# INLINE_NORMAL pinnedWriteN #-}
pinnedWriteN ::
       forall m a. (MonadIO m, Unbox a)
    => Int
    -> Fold m a (MutArray a)
pinnedWriteN = writeNAs Pinned

-- | Like writeNWithUnsafe but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWithUnsafe #-}
writeRevNWithUnsafe :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeRevNWithUnsafe alloc n = fromArrayUnsafe <$> FL.foldlM' step initial

    where

    toArrayUnsafeRev (MutArray contents _ _ bound) =
         ArrayUnsafe contents bound bound

    initial = toArrayUnsafeRev <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        let ptr = INDEX_PREV(start,a)
        liftIO $ pokeAt ptr contents x
        return
          $ ArrayUnsafe contents ptr end

-- | Like writeNWith but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWith #-}
writeRevNWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeRevNWith alloc n = FL.take n (writeRevNWithUnsafe alloc n)

-- | Like writeN but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE_NORMAL writeRevN #-}
writeRevN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
writeRevN = writeRevNWith new

-- | @pinnedWriteNAligned align n@ folds a maximum of @n@ elements from the
-- input stream to a 'MutArray' aligned to the given size.
--
-- >>> pinnedWriteNAligned align = MutArray.writeNWith (MutArray.pinnedNewAligned align)
-- >>> pinnedWriteNAligned align n = MutArray.writeAppendN n (MutArray.pinnedNewAligned align n)
--
-- /Pre-release/
--
{-# INLINE_NORMAL pinnedWriteNAligned #-}
pinnedWriteNAligned :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> Fold m a (MutArray a)
pinnedWriteNAligned align = writeNWith (pinnedNewAligned align)

-- XXX Buffer to a list instead?

-- | Buffer a stream into a stream of arrays.
--
-- >>> writeChunks n = Fold.many (MutArray.writeN n) Fold.toStreamK
--
-- Breaking an array into an array stream  can be useful to consume a large
-- array sequentially such that memory of the array is released incrementatlly.
--
-- See also: 'arrayStreamKFromStreamD'.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL writeChunks #-}
writeChunks :: (MonadIO m, Unbox a) =>
    Int -> Fold m a (StreamK n (MutArray a))
writeChunks n = FL.many (writeN n) FL.toStreamK

{-# INLINE_NORMAL writeWithAs #-}
writeWithAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> Fold m a (MutArray a)
-- writeWithAs ps n = FL.rmapM rightSize $ writeAppendWith (* 2) (newAs ps n)
writeWithAs ps elemCount =
    FL.rmapM extract $ FL.foldlM' step initial

    where

    initial = do
        when (elemCount < 0) $ error "writeWith: elemCount is negative"
        liftIO $ newAs ps elemCount

    step arr@(MutArray _ start end bound) x
        | INDEX_NEXT(end,a) > bound = do
        let oldSize = end - start
            newSize = max (oldSize * 2) 1
        arr1 <- liftIO $ reallocExplicit (SIZE_OF(a)) newSize arr
        snocUnsafe arr1 x
    step arr x = snocUnsafe arr x

    extract = liftIO . rightSize

-- XXX Compare writeWith with fromStreamD which uses an array of streams
-- implementation. We can write this using writeChunks above if that is faster.
-- If writeWith is faster then we should use that to implement
-- fromStreamD.
--
-- XXX The realloc based implementation needs to make one extra copy if we use
-- shrinkToFit.  On the other hand, the stream of arrays implementation may
-- buffer the array chunk pointers in memory but it does not have to shrink as
-- we know the exact size in the end. However, memory copying does not seem to
-- be as expensive as the allocations. Therefore, we need to reduce the number
-- of allocations instead. Also, the size of allocations matters, right sizing
-- an allocation even at the cost of copying sems to help.  Should be measured
-- on a big stream with heavy calls to toArray to see the effect.
--
-- XXX check if GHC's memory allocator is efficient enough. We can try the C
-- malloc to compare against.

-- | @writeWith minCount@ folds the whole input to a single array. The array
-- starts at a size big enough to hold minCount elements, the size is doubled
-- every time the array needs to be grown.
--
-- /Caution! Do not use this on infinite streams./
--
-- >>> f n = MutArray.writeAppendWith (* 2) (MutArray.new n)
-- >>> writeWith n = Fold.rmapM MutArray.rightSize (f n)
-- >>> writeWith n = Fold.rmapM MutArray.fromArrayStreamK (MutArray.writeChunks n)
--
-- /Pre-release/
{-# INLINE_NORMAL writeWith #-}
writeWith :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
-- writeWith n = FL.rmapM rightSize $ writeAppendWith (* 2) (new n)
writeWith = writeWithAs Unpinned

-- | Fold the whole input to a single array.
--
-- Same as 'writeWith' using an initial array size of 'arrayChunkBytes' bytes
-- rounded up to the element size.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Unbox a) => Fold m a (MutArray a)
write = writeWith (allocBytesToElemCount (undefined :: a) arrayChunkBytes)

-- | Like 'write' but creates a pinned array.
{-# INLINE pinnedWrite #-}
pinnedWrite :: forall m a. (MonadIO m, Unbox a) => Fold m a (MutArray a)
pinnedWrite =
    writeWithAs Pinned (allocBytesToElemCount (undefined :: a) arrayChunkBytes)

-------------------------------------------------------------------------------
-- construct from streams, known size
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromStreamDNAs #-}
fromStreamDNAs :: forall m a. (MonadIO m, Unbox a)
    => PinnedState -> Int -> D.Stream m a -> m (MutArray a)
fromStreamDNAs ps limit str = do
    (arr :: MutArray a) <- liftIO $ newAs ps limit
    end <- D.foldlM' (fwrite (arrContents arr)) (return $ arrEnd arr) $ D.take limit str
    return $ arr {arrEnd = end}

    where

    fwrite arrContents ptr x = do
        liftIO $ pokeAt ptr arrContents  x
        return $ INDEX_NEXT(ptr,a)

-- | Use the 'writeN' fold instead.
--
-- >>> fromStreamDN n = Stream.fold (MutArray.writeN n)
--
{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> m (MutArray a)
-- fromStreamDN n = D.fold (writeN n)
fromStreamDN = fromStreamDNAs Unpinned

-- | Create a 'MutArray' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
{-# INLINABLE fromListN #-}
fromListN :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
fromListN n xs = fromStreamDN n $ D.fromList xs

-- | Like 'fromListN' but creates a pinned array.
{-# INLINABLE pinnedFromListN #-}
pinnedFromListN :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
pinnedFromListN n xs = fromStreamDNAs Pinned n $ D.fromList xs

-- | Like fromListN but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE fromListRevN #-}
fromListRevN :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
fromListRevN n xs = D.fold (writeRevN n) $ D.fromList xs
-}

-- | Convert a pure stream in Identity monad to a mutable array.
{-# INLINABLE fromPureStreamN #-}
fromPureStreamN :: (MonadIO m, Unbox a) =>
    Int -> Stream Identity a -> m (MutArray a)
fromPureStreamN n xs =
    D.fold (createOf n) $ D.morphInner (return . runIdentity) xs

-- XXX Need to share code between MutSmallArray and MutArray

foreign import ccall unsafe "string.h strlen" c_strlen
    :: Ptr Word8 -> IO CSize

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

{-# INLINABLE fromByteStr# #-}
fromByteStr# :: MonadIO m => Addr# -> m (MutArray Word8)
fromByteStr# addr = do
    -- It is better to count the size first and allocate exact space.
    -- Also, memcpy is better than stream copy when the size is known.
    -- C strlen compares 4 bytes at a time, so is better than the stream
    -- version. https://github.com/bminor/glibc/blob/master/string/strlen.c
    -- XXX We can possibly use a stream of Word64 to do the same.
    -- fromByteStr# addr = fromPureStream (D.fromByteStr# addr)
    len <- liftIO $ c_strlen (Ptr addr)
    let lenInt = fromIntegral len
    arr <- new lenInt
    _ <- asUnpinnedPtrUnsafe arr (\ptr -> liftIO $ c_memcpy ptr (Ptr addr) len)
    return arr

{-
-- | Convert a pure stream in Identity monad to a mutable array.
{-# INLINABLE fromPureStream #-}
fromPureStream :: (MonadIO m, Unbox a) => Stream Identity a -> m (MutArray a)
fromPureStream xs =
    liftIO $ D.fold write $ D.morphInner (return . runIdentity) xs

-------------------------------------------------------------------------------
-- convert stream to a single array
-------------------------------------------------------------------------------

{-# INLINE arrayStreamKLength #-}
arrayStreamKLength :: (Monad m, Unbox a) => StreamK m (MutArray a) -> m Int
arrayStreamKLength as = K.foldl' (+) 0 (K.map length as)

-- | Convert an array stream to an array. Note that this requires peak memory
-- that is double the size of the array stream.
--
{-# INLINE fromArrayStreamK #-}
fromArrayStreamK :: (Unbox a, MonadIO m) =>
    StreamK m (MutArray a) -> m (MutArray a)
fromArrayStreamK as = do
    len <- arrayStreamKLength as
    fromStreamDN len $ D.unfoldMany reader $ D.fromStreamK as

{-# INLINE fromStreamDAs #-}
fromStreamDAs ::
       (MonadIO m, Unbox a) => PinnedState -> D.Stream m a -> m (MutArray a)
fromStreamDAs ps m = arrayStreamKFromStreamDAs ps m >>= fromArrayStreamK

-- CAUTION: a very large number (millions) of arrays can degrade performance
-- due to GC overhead because we need to buffer the arrays before we flatten
-- all the arrays.
--
-- XXX Compare if this is faster or "fold write".
--
-- | We could take the approach of doubling the memory allocation on each
-- overflow. This would result in more or less the same amount of copying as in
-- the chunking approach. However, if we have to shrink in the end then it may
-- result in an extra copy of the entire data.
--
-- >>> fromStreamD = StreamD.fold MutArray.write
--
{-# INLINE fromStreamD #-}
fromStreamD :: (MonadIO m, Unbox a) => D.Stream m a -> m (MutArray a)
fromStreamD = fromStreamDAs Unpinned

-- | Create a 'MutArray' from a list. The list must be of finite size.
--
{-# INLINE fromList #-}
fromList :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
fromList xs = fromStreamD $ D.fromList xs

-- | Like 'fromList' but creates a pinned array.
{-# INLINE pinnedFromList #-}
pinnedFromList :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
pinnedFromList xs = fromStreamDAs Pinned $ D.fromList xs

-- XXX We are materializing the whole list first for getting the length. Check
-- if the 'fromList' like chunked implementation would fare better.

-- | Like 'fromList' but writes the contents of the list in reverse order.
{-# INLINE fromListRev #-}
fromListRev :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
fromListRev xs = fromListRevN (Prelude.length xs) xs

-------------------------------------------------------------------------------
-- Cloning
-------------------------------------------------------------------------------

{-# INLINE cloneAs #-}
cloneAs ::
    ( MonadIO m
#ifdef DEVBUILD
    , Unbox a
#endif
    )
    => PinnedState -> MutArray a -> m (MutArray a)
cloneAs ps src =
    liftIO $ do
        let startSrc = arrStart src
            srcLen = arrEnd src - startSrc
        newArrContents <-
            Unboxed.cloneSliceUnsafeAs ps startSrc srcLen (arrContents src)
        return $ MutArray newArrContents 0 srcLen srcLen

{-# INLINE clone #-}
clone ::
    ( MonadIO m
#ifdef DEVBUILD
    , Unbox a
#endif
    )
    => MutArray a -> m (MutArray a)
clone = cloneAs Unpinned

{-# INLINE pinnedClone #-}
pinnedClone ::
    ( MonadIO m
#ifdef DEVBUILD
    , Unbox a
#endif
    )
    => MutArray a -> m (MutArray a)
pinnedClone = cloneAs Pinned
-}

-------------------------------------------------------------------------------
-- Combining
-------------------------------------------------------------------------------

-- | Put a sub range of a source array into a subrange of a destination array.
-- This is not safe as it does not check the bounds of neither the src array
-- nor the destination array.
{-# INLINE putSliceUnsafe #-}
putSliceUnsafe ::
       forall m a. (MonadIO m, Unbox a)
    => MutArray a
    -> Int
    -> MutArray a
    -> Int
    -> Int
    -> m ()
putSliceUnsafe (MutArray src) srcStart (MutArray dst) dstStart len =
    let srcStartBytes = srcStart * SIZE_OF(a)
        dstStartBytes = dstStart * SIZE_OF(a)
     in MutByteArray.putSliceUnsafe src srcStartBytes dst dstStartBytes len

{-# INLINE splice #-}
splice :: forall m a. MonadIO m => MutArray a -> MutArray a -> m (MutArray a)
splice arr1@(MutArray ba1) arr2@(MutArray ba2) = do
    let
        len1 = byteLength arr1
        len2 = byteLength arr2
        len = len1 + len2
    newArrContents <-
        if MutByteArray.isPinned ba1
        then liftIO $ MutByteArray.pinnedNew len
        else liftIO $ MutByteArray.new len
    MutByteArray.putSliceUnsafe ba1 0 newArrContents 0 len1
    MutByteArray.putSliceUnsafe ba2 0 newArrContents len1 len2
    return $ MutArray newArrContents

{-# INLINABLE toPinnedCString #-}
toPinnedCString :: MonadIO m => MutArray Word8 -> m (MutArray Word8)
toPinnedCString arr@(MutArray barr) = do
    let len = byteLength arr
    arr1@(MutArray barr1) <- newAs Pinned (len + 1)
    MutByteArray.putSliceUnsafe barr 0 barr1 0 len
    putIndexUnsafe len arr1 0
    return arr1

-------------------------------------------------------------------------------
-- Casting
-------------------------------------------------------------------------------

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The array size must be a multiple of the size of type @b@
-- otherwise accessing the last element of the array may result into a crash or
-- a random value.
--
-- /Pre-release/
--
castUnsafe :: MutArray a -> MutArray b
castUnsafe (MutArray contents) = MutArray contents

-- | Cast an @MutArray a@ into an @MutArray Word8@.
--
asBytes :: MutArray a -> MutArray Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
cast :: forall a b. Unbox b => MutArray a -> Maybe (MutArray b)
cast arr =
    let len = byteLength arr
        r = len `mod` SIZE_OF(b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

-- XXX We can provide another API for "unsafe" FFI calls passing an unlifted
-- pointer to the FFI call. For unsafe calls we do not need to pin the array.
-- We can pass an unlifted pointer to the FFI routine to avoid GC kicking in
-- before the pointer is wrapped.
--
-- From the GHC manual:
--
-- GHC, since version 8.4, guarantees that garbage collection will never occur
-- during an unsafe call, even in the bytecode interpreter, and further
-- guarantees that unsafe calls will be performed in the calling thread. Making
-- it safe to pass heap-allocated objects to unsafe functions.

-- Should we just name it asPtr, the unsafety is implicit for any pointer
-- operations. And we are safe from Haskell perspective because we will be
-- pinning the memory.

-- | Use a @MutArray a@ as @Ptr a@. This is useful when we want to pass an
-- array as a pointer to some operating system call or to a "safe" FFI call.
--
-- If the array is not pinned it is copied to pinned memory before passing it
-- to the monadic action.
--
-- /Performance Notes:/ Forces a copy if the array is not pinned. It is advised
-- that the programmer keeps this in mind and creates a pinned array
-- opportunistically before this operation occurs, to avoid the cost of a copy
-- if possible.
--
-- /Unsafe/ because of direct pointer operations. The user must ensure that
-- they are writing within the legal bounds of the array.
--
-- /Pre-release/
--
{-# INLINE asPtrUnsafe #-}
asPtrUnsafe :: MonadIO m => MutArray a -> (Ptr a -> m b) -> m b
asPtrUnsafe (MutArray barr) = MutByteArray.asPtrUnsafe barr

{-# INLINE asUnpinnedPtrUnsafe #-}
asUnpinnedPtrUnsafe :: MonadIO m => MutArray a -> (Ptr a -> m b) -> m b
asUnpinnedPtrUnsafe (MutArray barr) = MutByteArray.asUnpinnedPtrUnsafe barr

{-
-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- | Compare the length of the arrays. If the length is equal, compare the
-- lexicographical ordering of two underlying byte arrays otherwise return the
-- result of length comparison.
--
-- /Pre-release/
{-# INLINE cmp #-}
cmp :: MonadIO m => MutArray a -> MutArray a -> m Ordering
cmp arr1 arr2 =
    liftIO
        $ do
            let marr1 = getMutableByteArray# (arrContents arr1)
                marr2 = getMutableByteArray# (arrContents arr2)
                !(I# st1#) = arrStart arr1
                !(I# st2#) = arrStart arr2
                !(I# len#) = byteLength arr1
            case compare (byteLength arr1) (byteLength arr2) of
                EQ -> do
                    r <- liftIO $ IO $ \s# ->
                             let res =
                                     I#
                                         (compareByteArrays#
                                              (unsafeCoerce# marr1)
                                              st1#
                                              (unsafeCoerce# marr2)
                                              st2#
                                              len#)
                              in (# s#, res #)
                    return $ compare r 0
                x -> return x
-}
