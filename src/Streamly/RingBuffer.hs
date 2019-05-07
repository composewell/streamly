{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Streamly.RingBuffer
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.RingBuffer
    ( RingBuffer(..)

    -- * Construction
    , unsafeNew

    -- * Modification
    , insert

    -- * Folds
    , foldRing
    , foldRingM
    , foldRingFullM

    -- * Fast Byte Comparisons
    , unsafeEqArray
    , unsafeEqArrayN
    ) where

import Control.Exception (assert)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
import GHC.Ptr (Ptr(..))
import Prelude hiding (length, concat)

import qualified Streamly.Array.Types as A

-- | A ring buffer is a mutable array of fixed size. Initially the array is
-- empty, with ringStart pointing at the start of allocated memory. We call the
-- next location to be written in the ring as ringHead. Initially ringHead ==
-- ringStart. When the first item is added ringHead points to ringStart +
-- sizeof item. When the buffer becomes full ringHead would wrap around to
-- ringStart. When the buffer is full, ringHead always points at the oldest
-- item in the ring and the newest item added will always overwrite the oldest
-- item.
--
-- When using it we should keep in mind that a ringBuffer is a mutable data
-- structure. We should not leak out references to it for immutable use.
--
data RingBuffer a = RingBuffer
    { ringStart :: !(ForeignPtr a) -- first address
    , ringBound :: !(Ptr a)        -- first address beyond allocated memory
    }

-- | Create a new ringbuffer and return the ring buffer and the ringHead.
{-# INLINE unsafeNew #-}
unsafeNew :: forall a. Storable a => Int -> IO (RingBuffer a, Ptr a)
unsafeNew count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: a))
    let p = unsafeForeignPtrToPtr fptr
    return $ (RingBuffer
        { ringStart = fptr
        , ringBound = p `plusPtr` size
        }, p)

-- | Advance the ringHead by 1 item, wrap around if we hit the end of the
-- array.
{-# INLINE advance #-}
advance :: forall a. Storable a => RingBuffer a -> Ptr a -> Ptr a
advance RingBuffer{..} ringHead =
    let ptr = ringHead `plusPtr` sizeOf (undefined :: a)
    in if ptr <  ringBound
       then ptr
       else unsafeForeignPtrToPtr ringStart

-- | Insert an item at the head of the ring, when the ring is full this
-- replaces the oldest item in the ring with the new item.
{-# INLINE insert #-}
insert :: Storable a => RingBuffer a -> Ptr a -> a -> IO (Ptr a)
insert rb ringHead newVal = do
    poke ringHead newVal
    -- touchForeignPtr (ringStart rb)
    return $ advance rb ringHead

-- | Like 'unsafeEqArray' but compares only N bytes instead of entire length of
-- the ring buffer.
{-# INLINE unsafeEqArrayN #-}
unsafeEqArrayN :: RingBuffer a -> Ptr a -> A.Array a -> Int -> Bool
unsafeEqArrayN RingBuffer{..} rh A.Array{..} n =
    let !res = A.unsafeInlineIO $ do
            let rs = unsafeForeignPtrToPtr ringStart
            let as = unsafeForeignPtrToPtr aStart
            assert (aBound `minusPtr` as >= ringBound `minusPtr` rs) (return ())
            let len = ringBound `minusPtr` rh
            r1 <- A.memcmp (castPtr rh) (castPtr as) (min len n)
            r2 <- if n > len
                then A.memcmp (castPtr rs) (castPtr (as `plusPtr` len))
                              (min (rh `minusPtr` rs) (n - len))
                else return True
            -- touchForeignPtr ringStart
            -- touchForeignPtr aStart
            return (r1 && r2)
    in res

-- | Byte compare the entire length of ringBuffer with the given array,
-- starting at the supplied pointer. The supplied pointer is usually the
-- ringHead, therefore it would compare the ringBuffer starting at the oldest
-- item in the ring up to the newest item in the ring.
--
-- Returns true if the Array and the ringBuffer have identical contents. The
-- supplied array must be equal to or bigger than the ringBuffer, ARRAY BOUNDS
-- ARE NOT CHECKED.
{-# INLINE unsafeEqArray #-}
unsafeEqArray :: RingBuffer a -> Ptr a -> A.Array a -> Bool
unsafeEqArray RingBuffer{..} rh A.Array{..} =
    let !res = A.unsafeInlineIO $ do
            let rs = unsafeForeignPtrToPtr ringStart
            let as = unsafeForeignPtrToPtr aStart
            assert (aBound `minusPtr` as >= ringBound `minusPtr` rs)
                   (return ())
            let len = ringBound `minusPtr` rh
            r1 <- A.memcmp (castPtr rh) (castPtr as) len
            r2 <- A.memcmp (castPtr rs) (castPtr (as `plusPtr` len))
                           (rh `minusPtr` rs)
            -- touchForeignPtr ringStart
            -- touchForeignPtr aStart
            return (r1 && r2)
    in res

-- | Fold the buffer starting from ringStart to a given position using a pure
-- step function. This is useful to fold the items in the ring when the ring is
-- not full. The supplied pointer is usually the end of the ring.
{-# INLINE foldRing #-}
foldRing :: forall a b. Storable a
    => Ptr a -> (b -> a -> b) -> b -> RingBuffer a -> b
foldRing ptr f z RingBuffer{..} =
    let !res = A.unsafeInlineIO $ withForeignPtr ringStart $ \p ->
                    go z p ptr
    in res
    where
      go !acc !p !q
        | p == q = return acc
        | otherwise = do
            x <- peek p
            go (f acc x) (p `plusPtr` sizeOf (undefined :: a)) q

-- | Like foldRing but with a monadic step function.
{-# INLINE foldRingM #-}
foldRingM :: forall m a b. (Monad m, Storable a)
    => Ptr a -> (b -> a -> m b) -> b -> RingBuffer a -> m b
foldRingM ptr f z RingBuffer{..} = go z (unsafeForeignPtrToPtr ringStart) ptr
    where
      go !acc !start !end
        | start == end = return acc
        | otherwise = do
            let !x = A.unsafeInlineIO $ peek start
            acc' <- f acc x
            go acc' (start `plusPtr` sizeOf (undefined :: a)) end

-- | Fold the entire length of a ring buffer starting at the supplied ringHead
-- pointer.  Assuming the supplied ringHead pointer points to the oldest item,
-- this would fold the ring starting from the oldest item to the newest item in
-- the ring.
{-# INLINE foldRingFullM #-}
foldRingFullM :: forall m a b. (Monad m, Storable a)
    => Ptr a -> (b -> a -> m b) -> b -> RingBuffer a -> m b
foldRingFullM rh f z rb@RingBuffer{..} = go z rh
    where
      go !acc !start = do
            let !x = A.unsafeInlineIO $ peek start
            acc' <- f acc x
            let ptr = advance rb start
            if ptr == rh
            then return acc'
            else go acc' ptr
