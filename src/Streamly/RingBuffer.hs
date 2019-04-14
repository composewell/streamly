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
    , unsafeNew
    , insert
    , bufcmp
    , advance
    , foldAll
    ) where

import Control.Exception (assert)
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.Ptr (Ptr(..))
import Prelude hiding (length, concat)

import qualified Streamly.Array.Types as A

-- XXX this is mutable and can only be used if you make sure to never leak the
-- references. TODO - use types to ensure that.
--
-- XXX keep it cacheline aligned
-- XXX keep them unpacked
-- XXX this is highly performance optimized, we do not even track whether the
-- ring is full, the user must track that.
-- XXX we can keep ringBound and ringHead inside the ringStart memory itself
data RingBuffer a = Storable a => RingBuffer
    { ringStart :: !(ForeignPtr a) -- first address
    , ringBound :: !(Ptr a)        -- first address beyond allocated memory
    -- Points to the next empty location in the ring, when the ring is full
    -- this happens to be the oldest item in the ring which will be replaced by
    -- a new insert.
    -- , ringHead  :: !(Ptr a)
    }

{-# INLINE unsafeNew #-}
unsafeNew :: forall a. Storable a => Int -> IO (RingBuffer a, Ptr a)
unsafeNew count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- mallocPlainForeignPtrBytes size
    let p = unsafeForeignPtrToPtr fptr
    return $ (RingBuffer
        { ringStart = fptr
        , ringBound = p `plusPtr` size
        -- , ringHead  = p
        }, p)

{-# INLINE advance #-}
advance :: forall a. Storable a => RingBuffer a -> Ptr a -> Ptr a
advance RingBuffer{..} ringHead =
    let ptr = ringHead `plusPtr` sizeOf (undefined :: a)
    in if ptr <  ringBound
       then ptr
       else unsafeForeignPtrToPtr ringStart

-- | insert an item at the head of the ring, when the ring is full this
-- replaces the oldest item in the ring with the new item.
{-# INLINE insert #-}
insert :: Storable a => RingBuffer a -> Ptr a -> a -> IO (Ptr a)
insert rb ringHead newVal = do
    poke ringHead newVal
    -- XXX do we need this?
    -- RingBuffer is still referring to the ptr
    -- touchForeignPtr (ringStart rb)
    let ptr = advance rb ringHead
    return ptr

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- XXX we are converting Int to CSize
-- return True if the memory locations have identical contents
{-# INLINE memcmp #-}
memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
memcmp p1 p2 len = do
    r <- c_memcmp p1 p2 (fromIntegral len)
    return $ r == 0

-- Compare the entire ringBuffer with the given array, returns true if the
-- Array and the ringBuffer have identical contents. The supplied array must be
-- equal to or bigger than the ringBuffer, bounds are not checked.
{-# INLINE bufcmp #-}
bufcmp :: RingBuffer a -> A.Array a -> Bool
bufcmp RingBuffer{..} A.Array{..} =
    let !res = A.unsafeDangerousPerformIO $ do
            let rs = unsafeForeignPtrToPtr ringStart
            let as = unsafeForeignPtrToPtr aStart
            assert (aBound `minusPtr` as >= ringBound `minusPtr` rs) (return ())
            r <- memcmp (castPtr rs) (castPtr as) (ringBound `minusPtr` rs)
            -- XXX do we need these?
            -- touchForeignPtr ringStart
            -- touchForeignPtr aStart
            return r
    in res

-- Folds the whole ring including unused items if any, in no particular order.
-- XXX we can have another API to fold only the used items from head to tail
-- order.
{-# INLINE foldAll #-}
foldAll :: forall a b. Storable a => (b -> a -> b) -> b -> RingBuffer a -> b
foldAll f z RingBuffer{..} =
    let !res = A.unsafeDangerousPerformIO $ withForeignPtr ringStart $ \p ->
                    go z p ringBound
    in res
    where
      go !acc !p !q
        | p == q = return acc
        | otherwise = do
            x <- peek p
            go (f acc x) (p `plusPtr` sizeOf (undefined :: a)) q
