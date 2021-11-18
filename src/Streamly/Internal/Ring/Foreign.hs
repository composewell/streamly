-- |
-- Module      : Streamly.Internal.Ring.Foreign
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

module Streamly.Internal.Ring.Foreign
    (
    -- * Type
    Ring(..)

    -- * Construction
    , new
    , newRing
    , writeN

    , advance
    , moveBy
    , startOf

    -- * Random writes
    , unsafeInsert
    , slide
    , putIndex
    , modifyIndex

    -- * Unfolds
    , read
    , readRev

    -- * Random reads
    , getIndex
    , getIndexUnsafe
    , getIndexRev

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
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Array.Foreign.Mut.Type (Array, memcmp)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
-- import Streamly.Internal.Data.Stream.IsStream.Transform (scan)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Array.Foreign.Type as A

import Prelude hiding (length, concat, read)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Ring.Foreign as Ring

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
    { ringStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , ringBound :: {-# UNPACK #-} !(Ptr a)        -- first address beyond allocated memory
    }

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Get the first address of the ring as a pointer.
startOf :: Ring a -> Ptr a
startOf = unsafeForeignPtrToPtr . ringStart

-- | Create a new ringbuffer and return the ring buffer and the ringHead.
-- Returns the ring and the ringHead, the ringHead is same as ringStart.
{-# INLINE new #-}
new :: forall a. Storable a => Int -> IO (Ring a, Ptr a)
new count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: a))
    let p = unsafeForeignPtrToPtr fptr
    return (Ring
        { ringStart = fptr
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

-- | Advance the ringHead by 1 item, wrap around if we hit the end of the
-- array.
{-# INLINE advance #-}
advance :: forall a. Storable a => Ring a -> Ptr a -> Ptr a
advance Ring{..} ringHead =
    let ptr = ringHead `plusPtr` sizeOf (undefined :: a)
    in if ptr <  ringBound
       then ptr
       else unsafeForeignPtrToPtr ringStart

-- | Move the ringHead by n items. The direction depends on the sign on whether
-- n is positive or negative. Wrap around if we hit the beginning or end of the
-- array.
{-# INLINE moveBy #-}
moveBy :: forall a. Storable a => Int -> Ring a -> Ptr a -> Ptr a
moveBy by Ring {..} ringHead = ringStartPtr `plusPtr` advanceFromHead

    where

    elemSize = sizeOf (undefined :: a)
    ringStartPtr = unsafeForeignPtrToPtr ringStart
    lenInBytes = ringBound `minusPtr` ringStartPtr
    offInBytes = ringHead `minusPtr` ringStartPtr
    len = assert (lenInBytes `mod` elemSize == 0) $ lenInBytes `div` elemSize
    off = assert (offInBytes `mod` elemSize == 0) $ offInBytes `div` elemSize
    advanceFromHead = (off + by `mod` len) * elemSize

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
fromArray :: Array a -> Ring a
fromArray = undefined

-------------------------------------------------------------------------------
-- Conversion to/from array
-------------------------------------------------------------------------------

-- | Modify a given index of a ring array using a modifier function.
--
-- /Unimplemented/
modifyIndex :: -- forall m a b. (MonadIO m, Storable a) =>
    Ring a -> Int -> (a -> (a, b)) -> m b
modifyIndex = undefined

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
slide :: -- forall m a. (MonadIO m, Storable a) =>
    Ring a -> a -> m (Ring a)
slide = undefined

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the ring array.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe :: -- forall m a. (MonadIO m, Storable a) =>
    Ring a -> Int -> m a
getIndexUnsafe = undefined

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: -- (MonadIO m, Storable a) =>
    Ring a -> Int -> m a
getIndex = undefined

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
byteLength = undefined

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- Note that 'byteLength' is less expensive than this operation, as 'length'
-- involves a costly division operation.
--
-- /Unimplemented/
{-# INLINE length #-}
length :: -- forall a. Storable a =>
    Ring a -> Int
length = undefined

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

-- | Unfold a ring array into a stream.
--
-- /Unimplemented/
{-# INLINE_NORMAL read #-}
read :: -- forall m a. (MonadIO m, Storable a) =>
    Unfold m (Ring a) a
read = undefined

-- | Unfold a ring array into a stream in reverse order.
--
-- /Unimplemented/
{-# INLINE_NORMAL readRev #-}
readRev :: -- forall m a. (MonadIO m, Storable a) =>
    Unfold m (Array a) a
readRev = undefined

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
    Int -> SerialT m a -> SerialT m (Array a)
ringsOf = undefined -- Stream.scan (writeN n)

-------------------------------------------------------------------------------
-- Casting
-------------------------------------------------------------------------------

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The array size must be a multiple of the size of type @b@.
--
-- /Unimplemented/
--
castUnsafe :: Ring a -> Ring b
castUnsafe = undefined

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
unsafeEqArrayN :: Ring a -> Ptr a -> A.Array a -> Int -> Bool
unsafeEqArrayN Ring{..} rh A.Array{..} n =
    let !res = unsafeInlineIO $ do
            let rs = unsafeForeignPtrToPtr ringStart
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
    in res

-- | Byte compare the entire length of ringBuffer with the given array,
-- starting at the supplied ringHead pointer.  Returns true if the Array and
-- the ringBuffer have identical contents.
--
-- This is unsafe because the ringHead Ptr is not checked to be in range. The
-- supplied array must be equal to or bigger than the ringBuffer, ARRAY BOUNDS
-- ARE NOT CHECKED.
{-# INLINE unsafeEqArray #-}
unsafeEqArray :: Ring a -> Ptr a -> A.Array a -> Bool
unsafeEqArray Ring{..} rh A.Array{..} =
    let !res = unsafeInlineIO $ do
            let rs = unsafeForeignPtrToPtr ringStart
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
    in res

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
unsafeFoldRing :: forall a b. Storable a
    => Ptr a -> (b -> a -> b) -> b -> Ring a -> b
unsafeFoldRing ptr f z Ring{..} =
    let !res = unsafeInlineIO $ withForeignPtr ringStart $ \p ->
                    go z p ptr
    in res
    where
      go !acc !p !q
        | p == q = return acc
        | otherwise = do
            x <- peek p
            go (f acc x) (p `plusPtr` sizeOf (undefined :: a)) q

-- XXX Can we remove MonadIO here?
withForeignPtrM :: MonadIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtrM fp fn = do
    r <- fn $ unsafeForeignPtrToPtr fp
    liftIO $ touchForeignPtr fp
    return r

-- | Like unsafeFoldRing but with a monadic step function.
{-# INLINE unsafeFoldRingM #-}
unsafeFoldRingM :: forall m a b. (MonadIO m, Storable a)
    => Ptr a -> (b -> a -> m b) -> b -> Ring a -> m b
unsafeFoldRingM ptr f z Ring {..} =
    withForeignPtrM ringStart $ \x -> go z x ptr
  where
    go !acc !start !end
        | start == end = return acc
        | otherwise = do
            let !x = unsafeInlineIO $ peek start
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
    withForeignPtrM ringStart $ \_ -> go z rh
  where
    go !acc !start = do
        let !x = unsafeInlineIO $ peek start
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
    withForeignPtrM ringStart $ \_ -> go count z rh

    where

    go 0 acc _ = return acc
    go !n !acc !start = do
        let !x = unsafeInlineIO $ peek start
        acc' <- f acc x
        let ptr = advance rb start
        if ptr == rh || n == 0
            then return acc'
            else go (n - 1) acc' ptr
