{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "Streams/inline.hs"

-- |
-- Module      : Streamly.Array
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Arrays as implemented in this module are chunks of memory that can hold a
-- sequence of 'Storable' values of the same type. Unlike streams, arrays are
-- necessarily /finite/.  The size of an array is pre-determined and does not
-- grow dynamically.
--
-- Most importantly, arrays use memory that is out of the ambit of GC and
-- therefore can hold arbitrary number of elements without adding any pressure
-- to GC. Moreover, they can be used to communicate with foreign consumers and
-- producers (e.g. file and network IO) without copying the data.

-- Each array is one pointer visible to the GC.  Too many small arrays (e.g.
-- single byte) are only as good as holding those elements in a Haskell list.
-- However, small arrays can be compacted into large ones to reduce the
-- overhead. To hold 32GB memory in 32k sized buffers we need 1 million arrays
-- if we use one array for each chunk. This is still significant to add
-- pressure to GC.  However, we can create arrays of arrays (trees) to scale to
-- arbitrarily large amounts of memory but still using small chunks of
-- contiguous memory.

module Streamly.Array
    (
      Array

    -- * Construction/Generation
    , nil
    , singleton
    , fromList
    , readHandleChunksOf

    -- * Elimination/Folds
    , foldl'
    , null
    , length
    , last

    , toList
    , toHandle
    , concatArray
    , concatToHandle
    )
where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (runIdentity)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr)
import Foreign.Storable (Storable(..))
import System.IO (Handle, hGetBufSome, hPutBuf)
import Prelude hiding (length, null, last)
import qualified Prelude

import GHC.Base (nullAddr#)
import GHC.ForeignPtr (ForeignPtr(..), mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)
import GHC.Ptr (Ptr(..))

import Streamly.Streams.StreamK.Type (IsStream, mkStream)
import Streamly.Array.Types

import Streamly.Streams.Serial (SerialT)
import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
-- import qualified Streamly.Streams.StreamD.Type as D
import qualified Streamly.Streams.StreamD as D

-------------------------------------------------------------------------------
-- Compacting streams of arrays
-------------------------------------------------------------------------------

{-
-- we can call these regroupXXX or reArrayXXX
--
-- Compact buffers in a stream such that each resulting buffer contains exactly
-- N elements.
compactN :: Int -> Int -> t m (Array a) -> t m (Array a)
compactN n vectors =

-- This can be useful if the input stream may "suspend" before generating
-- further output. So we can emit a vector early without waiting. It will emit
-- a vector of at least 1 element.
compactUpTo :: Int -> t m (Array a) -> t m (Array a)
compactUpTo hi vectors =

-- wait for minimum amount to be collected but don't wait for the upper limit
-- if the input stream suspends. But never go beyond the upper limit.
compactMinUpTo :: Int -> Int -> t m (Array a) -> t m (Array a)
compactMinUpTo lo hi vectors =

-- The buffer is emitted as soon as a complete marker sequence is detected. The
-- emitted buffer contains the sequence as suffix.
compactUpToMarker :: Array a -> t m (Array a) -> t m (Array a)
compactUpToMarker hi marker =

-- Buffer upto a max count or until timeout occurs. If timeout occurs without a
-- single element in the buffer it raises an exception.
compactUpToWithTimeout :: Int -> Int -> t m (Array a) -> t m (Array a)
compactUpToWithTimeout hi time =

-- Wait until min elements are collected irrespective of time. After collecting
-- minimum elements if timeout occurs return the buffer immediately else wait
-- upto timeout or max limit.
compactInRangeWithTimeout ::
    Int -> Int -> Int -> t m (Array a) -> t m (Array a)
compactInRangeWithTimeout lo hi time =

-- Compact the contiguous sequences into a single vector.
compactToReorder :: (a -> a -> Int) -> t m (Array a) -> t m (Array a)

-------------------------------------------------------------------------------
-- deCompact streams of arrays
-------------------------------------------------------------------------------

-- split buffers into smaller buffers
-- deCompactBuffers :: Int -> Int -> t m Buffer -> t m Buffer
-- deCompactBuffers maxSize tolerance =

-------------------------------------------------------------------------------
-- Scatter/Gather IO
-------------------------------------------------------------------------------

-- When each IO operation has a significant system overhead, it may be more
-- efficient to do gather IO. But when the buffers are too small we may want to
-- copy multiple of them in a single buffer rather than setting up a gather
-- list. In that case, a gather list may have more overhead compared to just
-- copying. If the buffer is larger than a limit we may just keep a single
-- buffer in a gather list.
--
-- gatherBuffers :: Int -> t m Buffer -> t m GatherBuffer
-- gatherBuffers maxLimit bufs =
-}

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- XXX Use stream and toArray to create a vector.

-- Represent a null pointer for an empty vector
nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

-- | An empty array.
--
-- @
-- > toList nil
-- []
-- @
--
-- @since 0.7.0
{-# INLINE nil #-}
nil :: Storable a => Array a
nil = Array
    { aStart = nullForeignPtr
    , aEnd = Ptr nullAddr#
    , aBound = Ptr nullAddr#
    }

-- XXX should we use unsafePerformIO instead?
{-# INLINE singleton #-}
singleton :: forall a. Storable a => a -> Array a
singleton a =
    let !v = unsafeDupablePerformIO $ withNewArray 1 $ \p -> poke p a
    in (v {aEnd = aEnd v `plusPtr` (sizeOf (undefined :: a))})

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
{-# INLINE fromHandleSome #-}
fromHandleSome :: Int -> Handle -> IO ByteArray
fromHandleSome size h = do
    ptr <- mallocPlainForeignPtrBytes size
    withForeignPtr ptr $ \p -> do
        n <- hGetBufSome h p size
        let v = Array
                { aStart = ptr
                , aEnd   = p `plusPtr` n
                , aBound = p `plusPtr` size
                }
        -- XXX shrink only if the diff is significant
        shrinkToFit v

-- | @readHandleChunksOf size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @size@.
-- 'readHandleChunksOf' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
{-# INLINE readHandleChunksOf #-}
readHandleChunksOf :: (IsStream t, MonadIO m) => Int -> Handle -> t m ByteArray
readHandleChunksOf size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld sng _ -> do
        vec <- liftIO $ fromHandleSome size h
        if length vec < size
        then sng vec
        else yld vec go

-------------------------------------------------------------------------------
-- Cast arrays from one type to another
-------------------------------------------------------------------------------

-- XXX The first array must be an exact multiple of (sizeOf b).
-- Can be useful to manipulate larger size elements more efficiently, e.g.
-- copying Word64 instead of Word8.
-- castArray :: (Storable a, Storable b) => Array a -> Array b
-- castArray Array{..} =

-- split an array to remove the unaligned part at the end into a separate
-- array. Useful to copy the aligned portion more efficiently.
--
-- splitUnaligned :: Int -> Array a -> (Array a, Array a)

-- Like concatArray but while concating casts the array from one type to
-- another. Useful to combine array chunks into arrays that can be manipulated
-- more efficiently.
--
-- concatCastArray

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z Array{..} =
    unsafeDangerousPerformIO $ withForeignPtr aStart $ \p -> go z p aEnd
    where
      go !acc !p !q
        | p == q = return acc
        | otherwise = do
            x <- peek p
            go (f acc x) (p `plusPtr` sizeOf (undefined :: a)) q

{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length Array{..} =
    let p = unsafeForeignPtrToPtr aStart
        aLen = aEnd `minusPtr` p
    in assert (aLen >= 0) (aLen `div` sizeOf (undefined :: a))

{-# INLINE null #-}
null :: Array a -> Bool
null Array{..} =
    let start = unsafeForeignPtrToPtr aStart
    in assert (aEnd >= start) $ aEnd <= start

{-# INLINE last #-}
last :: forall a. Storable a => Array a -> Maybe a
last arr@Array{..} =
    if null arr
    then Nothing
    else Just $!
        let p = aEnd `plusPtr` negate (sizeOf (undefined :: a))
        in unsafeDangerousPerformIO $ do
            x <- peek p
            touchForeignPtr aStart
            return x

-------------------------------------------------------------------------------
-- Elimination/folding
-------------------------------------------------------------------------------

-- | Writing a stream to a file handle
{-# INLINE toHandle #-}
toHandle :: Handle -> ByteArray -> IO ()
toHandle _ v | null v = return ()
toHandle h v@Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p (length v)

-------------------------------------------------------------------------------
-- Streams of Arrays
-------------------------------------------------------------------------------

-- | Convert a stream of Arrays into a stream of elements
{-# INLINE concatArray #-}
concatArray :: (IsStream t, Monad m, Storable a) => t m (Array a) -> t m a
concatArray m = D.fromStreamD $ D.concatArray (D.toStreamD m)

-- XXX we should use overWrite/write
{-# INLINE concatToHandle #-}
concatToHandle :: MonadIO m => Handle -> SerialT m ByteArray -> m ()
concatToHandle h m = S.mapM_ (liftIO . toHandle h) m
