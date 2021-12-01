-- |
-- Module      : Streamly.Internal.Data.Ring.Foreign
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A ring array is a circular mutable array.

-- XXX Write benchmarks
-- XXX Make the implementation similar to mutable array
-- XXX Rename this module to Data.RingArray.Storable

module Streamly.Internal.Data.Ring.Foreign
    ( Ring(..)

    -- * Construction
    , new
    , newRing

    , writeNUnsafe
    , writeN

    , advance
    , retreat
    , moveBy
    , startOf

    -- * Random writes
    , unsafeInsert
    , slide
    , putIndex
    , putIndexUnsafe
    , modifyIndex
    , modifyIndexUnsafe

    -- * Unfolds
    , read
    , readRev

    -- * Random reads
    , getIndex
    , getIndexUnsafe
    , getIndexRev
    , getIndexRevUnsafe

    -- * Size
    , length
    , byteLength
    -- , capacity
    , byteCapacity
    , bytesFree

    -- * Casting
    , cast
    , castUnsafe
    , asBytes
    , fromArray

    -- * Folds
    , unsafeFoldRing
    , unsafeFoldRingM
    , unsafeFoldRingFullM
    , unsafeFoldRingNM

    -- * Stream of Arrays
    , ringsOf

    -- * Fast Byte Comparisons
    , unsafeEqArray
    , unsafeEqArrayN
    ) where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (ForeignPtr(..), mallocPlainForeignPtrAlignedBytes)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Array.Foreign.Mut.Type (memcmp)
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Array.Foreign.Mut.Type
    (ArrayContents, touch, fptrToArrayContents)

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Unfold.Type as Unfold

import Prelude hiding (length, concat, read)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Ring.Foreign as Ring

-- | A ring buffer is a mutable array of fixed size. Initially the array is
-- empty, with ringStart pointing at the start of allocated memory. We call the
-- next location to be written in the ring as ringHead. Initially ringHead ==
-- ringStart. When the first item is added, ringHead points to ringStart +
-- sizeof item. When the buffer becomes full ringHead would wrap around to
-- ringStart. When the buffer is full, ringHead always points at the oldest
-- item in the ring and the newest item added always overwrites the oldest
-- item.
--
-- When using it we should keep in mind that a ringBuffer is a mutable data
-- structure. We should not leak out references to it for immutable use.
--
data Ring a = Ring
    { ringContents ::
#ifndef USE_FOREIGN_PTR
        {-# UNPACK #-}
#endif
          !ArrayContents
    , ringStart :: {-# UNPACK #-} !(Ptr a) -- first address
    , ringBound :: {-# UNPACK #-} !(Ptr a) -- first address beyond allocated memory
    }

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Get the first address of the ring as a pointer.
startOf :: Ring a -> Ptr a
startOf = ringStart

-- | Create a new ringbuffer and return the ring buffer and the ringHead.
-- Returns the ring and the ringHead, the ringHead is same as ringStart.
{-# INLINE new #-}
new :: forall a. Storable a => Int -> IO (Ring a, Ptr a)
new count = do
    let size = count * sizeOf (undefined :: a)
    ForeignPtr addr# fconts <-
        mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: a))
    let p = Ptr addr#
    return (Ring
        { ringContents = fptrToArrayContents fconts
        , ringStart = p
        , ringBound = p `plusPtr` size
        }, p)

-- XXX Rename this to "new".
--
-- | @newRing count@ allocates an empty array that can hold 'count' items.  The
-- memory of the array is uninitialized and the allocation is aligned as per
-- the 'Storable' instance of the type.
--
-- /Unimplemented/
{-# INLINE newRing #-}
newRing :: Int -> m (Ring a)
newRing = undefined

-- | Advance the ringHead by 1 item, wrap around if we hit the end of the ring
-- array.
{-# INLINE advance #-}
advance :: forall a. Storable a => Ring a -> Ptr a -> Ptr a
advance Ring{..} ringHead =
    let ptr = ringHead `plusPtr` sizeOf (undefined :: a)
    in if ptr <  ringBound
       then ptr
       else ringStart

-- | Retreat the ringHead by 1 item, wrap around backwards if we hit the start
-- of the ring array.
{-# INLINE retreat #-}
retreat :: forall a. Storable a => Ring a -> Ptr a -> Ptr a
retreat Ring{..} ringHead =
    let ptr = ringHead `plusPtr` negate (sizeOf (undefined :: a))
    in if ptr > ringStart
       then ptr
       else ringBound

-- | Move the ringHead by n items. The direction depends on the sign on whether
-- n is positive or negative. Wrap around if we hit the beginning or end of the
-- array.
{-# INLINE moveBy #-}
moveBy :: forall a. Storable a => Int -> Ring a -> Ptr a -> Ptr a
moveBy by Ring {..} ringHead = ringStartPtr `plusPtr` advanceFromHead

    where

    elemSize = sizeOf (undefined :: a)
    ringStartPtr = ringStart
    lenInBytes = ringBound `minusPtr` ringStartPtr
    offInBytes = ringHead `minusPtr` ringStartPtr
    len = assert (lenInBytes `mod` elemSize == 0) $ lenInBytes `div` elemSize
    off = assert (offInBytes `mod` elemSize == 0) $ offInBytes `div` elemSize
    advanceFromHead = (off + by `mod` len) * elemSize

-- | @writeNUnsafe n@ is a rolling fold that keeps the last n elements of the
-- stream in a ring array.
--
-- /Unimplemented/
{-# INLINE writeNUnsafe #-}
writeNUnsafe :: (Storable a, MonadIO m) => Int -> Fold m a (Ring a, Ptr a)
writeNUnsafe n = Fold step (FL.Partial <$> liftIO (new n)) return

    where

    step (rb, p) a = liftIO $ do
        p1 <- unsafeInsert rb p a
        touch $ ringContents rb
        return $ FL.Partial (rb, p1)

-- XXX Move the writeLastN from array module here.
--
-- | @writeN n@ is a rolling fold that keeps the last n elements of the stream
-- in a ring array.
--
-- /Unimplemented/
{-# INLINE writeN #-}
writeN :: -- (Storable a, MonadIO m) =>
    Int -> Fold m a (Ring a)
writeN = undefined

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Cast a mutable array to a ring array.
fromArray :: MA.Array a -> Ring a
fromArray MA.Array {..} =
    Ring {ringContents = arrContents, ringStart = arrStart, ringBound = aEnd}

-------------------------------------------------------------------------------
-- Conversion to/from array
-------------------------------------------------------------------------------

-- | Modify a given index of a ring array using a modifier function.
--
-- /Unimplemented/
modifyIndexUnsafe :: forall m a b. (MonadIO m, Storable a) =>
    Ring a -> Ptr a -> Int -> (a -> (a, b)) -> m b
modifyIndexUnsafe rb@(Ring {..}) ringHead i f =
    MA.unsafeWithArrayContents ringContents ringHead $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = moveBy i rb ptr
        assert (i >= 0 && elemPtr `plusPtr` elemSize <= ringBound) (return ())
        r <- liftIO $ peek elemPtr
        let (x, res) = f r
        liftIO $ poke elemPtr x
        return res

-- XXX Determine the start index?
-- | Modify a given index of a ring array using a modifier function.
--
-- /Unimplemented/
modifyIndex :: -- forall m a b. (MonadIO m, Storable a) =>
    Ring a -> Int -> (a -> (a, b)) -> m b
modifyIndex = undefined

-- | Write the given element to the given index of the ring array. Does not
-- check if the index is out of bounds of the ring array.
--
-- /Pre-release/
{-# INLINE putIndexUnsafe #-}
putIndexUnsafe :: forall m a. (MonadIO m, Storable a)
    => Ring a -> Ptr a -> Int -> a -> m ()
putIndexUnsafe rb@(Ring {..}) ringHead i x =
    MA.unsafeWithArrayContents ringContents ringHead $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = moveBy i rb ptr
        assert (i >= 0 && elemPtr `plusPtr` elemSize <= ringBound) (return ())
        liftIO $ poke elemPtr x

-- | /O(1)/ Write the given element at the given index in the ring array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex arr ix val = Ring.modifyIndex arr ix (const (val, ()))
--
-- /Unimplemented/
{-# INLINE putIndex #-}
putIndex :: -- (MonadIO m, Storable a) =>
    Ring a -> Int -> a -> m ()
putIndex = undefined

-- | Insert an item at the head of the ring, when the ring is full this
-- replaces the oldest item in the ring with the new item. This is unsafe
-- beause ringHead supplied is not verified to be within the Ring. Also,
-- the ringStart foreignPtr must be guaranteed to be alive by the caller.
{-# INLINE unsafeInsert #-}
unsafeInsert :: Storable a => Ring a -> Ptr a -> a -> IO (Ptr a)
unsafeInsert rb ringHead newVal = do
    poke ringHead newVal
    -- touchForeignPtr (ringStart rb)
    return $ advance rb ringHead

-- | Insert an item at the head of the ring, when the ring is full this
-- replaces the oldest item in the ring with the new item.
--
-- /Unimplemented/
slide :: forall m a. (MonadIO m, Storable a) =>
    Ring a -> Ptr a -> a -> m (Ptr a)
slide rb ringHead newVal = liftIO $ do
    let p = retreat rb ringHead
    poke p newVal
    return p

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because the supplied Ptr is not checked to be in range.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe ::
       forall m a. (MonadIO m, Storable a)
    => Ring a -> Ptr a -> Int -> m a
getIndexUnsafe rb@(Ring {..}) ringHead i =
    MA.unsafeWithArrayContents ringContents ringHead $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = moveBy i rb ptr
        assert (i >= 0 && elemPtr `plusPtr` elemSize <= ringBound) (return ())
        liftIO $ peek elemPtr

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: -- (MonadIO m, Storable a) =>
    Ring a -> Int -> m a
getIndex = undefined

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because the supplied Ptr is not checked to be in range.
{-# INLINE_NORMAL getIndexRevUnsafe #-}
getIndexRevUnsafe ::
       forall m a. (MonadIO m, Storable a)
    => Ring a -> Ptr a -> Int -> m a
getIndexRevUnsafe rb@(Ring {..}) ringHead i =
    MA.unsafeWithArrayContents ringContents ringHead $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = moveBy (negate i) rb ptr
        assert (i >= 0 && elemPtr `plusPtr` elemSize <= ringBound) (return ())
        liftIO $ peek elemPtr

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: -- (MonadIO m, Storable a) =>
    Ring a -> Int -> m a
getIndexRev = undefined

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

-- | /O(1)/ Get the byte length of the array.
--
-- /Unimplemented/
{-# INLINE byteLength #-}
byteLength :: Ring a -> Int
byteLength Ring {..} =
    let len = ringBound `minusPtr` ringStart
     in assert (len >= 0) len

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- Note that 'byteLength' is less expensive than this operation, as 'length'
-- involves a costly division operation.
--
-- /Unimplemented/
{-# INLINE length #-}
length :: forall a. Storable a => Ring a -> Int
length rb =
    let elemSize = sizeOf (undefined :: a)
        blen = byteLength rb
     in assert (blen `mod` elemSize == 0) (blen `div` elemSize)

-- XXX There isn't a byteCapacity for a ring. Maybe we should have a
-- byteCapacity for a ring.
-- | Get the total capacity of an array. An array may have space reserved
-- beyond the current used length of the array.
--
-- /Pre-release/
{-# INLINE byteCapacity #-}
byteCapacity :: Ring a -> Int
byteCapacity = undefined

-- | The remaining capacity in the array for appending more elements without
-- reallocation.
--
-- /Pre-release/
{-# INLINE bytesFree #-}
bytesFree :: Ring a -> Int
bytesFree = undefined

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- XXX Will the loop fuse?
-- XXX Design this better?
-- | Resumable unfold of a ring array.
--
{-# INLINE_NORMAL producer #-}
producer ::
       forall m a. (MonadIO m, Storable a)
    => Producer m (Ring a, Ptr a, Ptr a) a
producer = Producer step inject extract

    where

    inject (rb, rh, cur) = return (rb, rh, cur, False)

    {-# INLINE step_ #-}
    step_ rb rh cur = do
        x <- liftIO $ peek cur
        let cur1 = advance rb cur
        return $ D.Yield x (rb, rh, cur1, True)

    {-# INLINE_LATE step #-}
    step (rb, rh, cur, False) = step_ rb rh cur
    step (rb, rh, cur, True)
        | assert (cur <= ringStart rb) (cur == rh) = do
            liftIO $ touch $ ringContents rb
            return D.Stop
    step (rb, rh, cur, True) = step_ rb rh cur

    extract (rb, rh, cur, _) = return (rb, rh, cur)

-- XXX Design the API better
-- | Unfold a ring array into a stream.
--
-- /Unimplemented/
{-# INLINE_NORMAL read #-}
read ::  forall m a. (MonadIO m, Storable a) => Unfold m (Ring a, Ptr a) a
read = Unfold.lmap (\(rb, rh) -> (rb, rh, rh)) $ Producer.simplify producer

-- | Unfold a ring array into a stream in reverse order.
--
-- /Unimplemented/
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (MonadIO m, Storable a) => Unfold m (Ring a, Ptr a) a
readRev = Unfold step inject

    where

    inject (rb, rh) = return (rb, rh, rh, False)

    {-# INLINE step_ #-}
    step_ rb rh cur = do
        x <- liftIO $ peek cur
        let cur1 = retreat rb cur
        return $ D.Yield x (rb, rh, cur1, True)

    {-# INLINE_LATE step #-}
    step (rb, rh, cur, False) = step_ rb rh cur
    step (rb, rh, cur, True)
        | assert (cur <= ringStart rb) (cur == rh) = do
            liftIO $ touch $ ringContents rb
            return D.Stop
    step (rb, rh, cur, True) = step_ rb rh cur

-------------------------------------------------------------------------------
-- Stream of arrays
-------------------------------------------------------------------------------

-- XXX Move this module to a lower level Ring/Type module and move ringsOf to a
-- higher level ring module where we can import "scan".

-- | @ringsOf n stream@ groups the input stream into a stream of
-- ring arrays of size n. Each ring is a sliding window of size n.
--
-- /Unimplemented/
{-# INLINE_NORMAL ringsOf #-}
ringsOf :: -- forall m a. (MonadIO m, Storable a) =>
    Int -> SerialT m a -> SerialT m (MA.Array a)
ringsOf = undefined -- Stream.scan (writeN n)

-------------------------------------------------------------------------------
-- Casting
-------------------------------------------------------------------------------

-- | Cast a ring array having elements of type @a@ into a ring array having
-- elements of type @b@. The ring array size must be a multiple of the size of
-- type @b@.
--
-- /Unimplemented/
--
castUnsafe :: Ring a -> Ring b
castUnsafe (Ring contents start end) =
    Ring contents (castPtr start) (castPtr end)

-- | Cast an @Array a@ into an @Array Word8@.
--
-- /Unimplemented/
--
asBytes :: Ring a -> Ring Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
-- /Pre-release/
--
cast :: forall a b. Storable b => Ring a -> Maybe (Ring b)
cast arr =
    let len = byteLength arr
        r = len `mod` sizeOf (undefined :: b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- XXX remove all usage of unsafeInlineIO
--
-- | Like 'unsafeEqArray' but compares only N bytes instead of entire length of
-- the ring buffer. This is unsafe because the ringHead Ptr is not checked to
-- be in range.
{-# INLINE unsafeEqArrayN #-}
unsafeEqArrayN :: MonadIO m => Ring a -> Ptr a -> A.Array a -> Int -> m Bool
unsafeEqArrayN Ring{..} rh A.Array{..} n = liftIO $ do
            let rs = ringStart
                as = arrStart
            assert (aEnd `minusPtr` as >= ringBound `minusPtr` rs) (return ())
            let len = ringBound `minusPtr` rh
            r1 <- memcmp (castPtr rh) (castPtr as) (min len n)
            r2 <- if n > len
                then memcmp (castPtr rs) (castPtr (as `plusPtr` len))
                              (min (rh `minusPtr` rs) (n - len))
                else return True
            -- XXX enable these, check perf impact
            -- touchForeignPtr ringStart
            -- touchForeignPtr aStart
            return (r1 && r2)

-- | Byte compare the entire length of ringBuffer with the given array,
-- starting at the supplied ringHead pointer.  Returns true if the Array and
-- the ringBuffer have identical contents.
--
-- This is unsafe because the ringHead Ptr is not checked to be in range. The
-- supplied array must be equal to or bigger than the ringBuffer, ARRAY BOUNDS
-- ARE NOT CHECKED.
{-# INLINE unsafeEqArray #-}
unsafeEqArray :: MonadIO m => Ring a -> Ptr a -> A.Array a -> m Bool
unsafeEqArray Ring{..} rh A.Array{..} = liftIO $ do
            let rs = ringStart
            let as = arrStart
            assert (aEnd `minusPtr` as >= ringBound `minusPtr` rs)
                   (return ())
            let len = ringBound `minusPtr` rh
            r1 <- memcmp (castPtr rh) (castPtr as) len
            r2 <- memcmp (castPtr rs) (castPtr (as `plusPtr` len))
                           (rh `minusPtr` rs)
            -- XXX enable these, check perf impact
            -- touchForeignPtr ringStart
            -- touchForeignPtr aStart
            return (r1 && r2)

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- XXX We can unfold it into a stream and fold the stream instead.
-- XXX use MonadIO
--
-- | Fold the buffer starting from ringStart up to the given 'Ptr' using a pure
-- step function. This is useful to fold the items in the ring when the ring is
-- not full. The supplied pointer is usually the end of the ring.
--
-- Unsafe because the supplied Ptr is not checked to be in range.
{-# INLINE unsafeFoldRing #-}
unsafeFoldRing :: forall m a b. (MonadIO m, Storable a)
    => Ptr a -> (b -> a -> b) -> b -> Ring a -> m b
unsafeFoldRing ptr f z Ring{..} = liftIO $ go z ringStart ptr

    where
      go !acc !p !q
        | p == q = return acc
        | otherwise = do
            x <- peek p
            touch ringContents
            go (f acc x) (p `plusPtr` sizeOf (undefined :: a)) q

-- | Like unsafeFoldRing but with a monadic step function.
{-# INLINE unsafeFoldRingM #-}
unsafeFoldRingM :: forall m a b. (MonadIO m, Storable a)
    => Ptr a -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingM ptr f z Ring {..} =
    go z ringStart ptr
  where
    go !acc !start !end
        | start == end = return acc
        | otherwise = do
            x <- liftIO $ peek start
            liftIO $ touch ringContents
            acc' <- f acc x
            go acc' (start `plusPtr` sizeOf (undefined :: a)) end

-- | Fold the entire length of a ring buffer starting at the supplied ringHead
-- pointer.  Assuming the supplied ringHead pointer points to the oldest item,
-- this would fold the ring starting from the oldest item to the newest item in
-- the ring.
--
-- Note, this will crash on ring of 0 size.
--
{-# INLINE unsafeFoldRingFullM #-}
unsafeFoldRingFullM :: forall m a b. (MonadIO m, Storable a)
    => Ptr a -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingFullM rh f z rb@Ring {..} =
    go z rh
  where
    go !acc !start = do
        x <- liftIO $ peek start
        liftIO $ touch ringContents
        acc' <- f acc x
        let ptr = advance rb start
        if ptr == rh
            then return acc'
            else go acc' ptr

-- | Fold @Int@ items in the ring starting at @Ptr a@.  Won't fold more
-- than the length of the ring.
--
-- Note, this will crash on ring of 0 size.
--
{-# INLINE unsafeFoldRingNM #-}
unsafeFoldRingNM :: forall m a b. (MonadIO m, Storable a)
    => Int -> Ptr a -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingNM count rh f z rb@Ring {..} =
    go count z rh

    where

    go 0 acc _ = return acc
    go !n !acc !start = do
        x <- liftIO $ peek start
        liftIO $ touch ringContents
        acc' <- f acc x
        let ptr = advance rb start
        if ptr == rh || n == 0
            then return acc'
            else go (n - 1) acc' ptr
