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
-- Arrays are the computing duals of streams.  Streams are good at sequential
-- access, immutable transformations of in-transit data whereas arrays are good
-- at random access, in-place transformations of buffered data.  Unlike streams
-- which are potentially infinite, arrays are necessarily /finite/.  Arrays can
-- be used as an efficient interface between streams and external storage
-- systems like memory, files and network. Streams and arrays complete each
-- other to provide a general purpose computing system. The design of streamly
-- as a general purpose computing framework is centered around these two
-- fundamental aspects of computing and storage.
--
-- Even though the implementation allows for in-place mutation, the current
-- implementation of arrays is immutable. Arrays are of fixed size. The size of
-- an array is fixed at creation and cannot be changed later without copying.
-- Arrays are chunks of memory that hold a sequence of 'Storable' values of a
-- given type. They are designed to store serializable data, they cannot store
-- non-serializable data like functions, this is reflected by the 'Storable'
-- constraint. For efficient buffering of data, arrays use pinned memory and
-- therefore can hold arbitrary number of elements with a single movable
-- pointer visible to GC, therefore, adding no pressure to GC.  Moreover,
-- pinned memory allows communication with foreign consumers and producers
-- (e.g. file or network IO) without copying the data.
--
-- By design, there are no transformation operations provided in this module,
-- only stream conversion and IO routines are provided. An array is purely a
-- data store, therefore, it is not a 'Functor'. Arrays can be operated upon
-- /efficiently/ by converting them into a stream, applying the desired
-- transformations on the stream and then converting it back to an array.
-- 'Foldable' instance is not provided because the implementation would be much
-- less efficient compared to folding via streams.  'Semigroup' and 'Monoid'
-- instances are deliberately not provided to avoid misuse; concatenating
-- arrays using binary operations can be highly inefficient.  Instead, use
-- 'spliceArrays' to concatenate N arrays at once.
--
-- To summarize:
--
-- * Arrays are finite and fixed in size
-- * provide /O(1)/ access to elements
-- * store only data and not functions
-- * provide efficient IO interfacing

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

    -- * Construction
    -- , fromStream
    , fromStreamN
    -- , fromList
    -- , fromListN

    -- * Elimination
    -- , toList
    , toStream

    -- * Random Access
    , length
    , (!!)

    -- * Streams of Arrays
    -- , defaultChunkSize
    -- , fromStreamArraysOf
    , flattenArrays
    , spliceArrays

    -- * IO: Single Array
    -- , fromHandleUpto
    -- , fromHandleN
    -- , fromHandlePosUpto
    -- , fromHandlePosN
    , toHandle

    -- * IO: Stream of Arrays
    -- , fromHandleArraysUpto
    -- , fromHandleArraysOf
    , fromHandleArrays
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import System.IO (Handle, hGetBufSome, hPutBuf)
import Prelude hiding (length, null, last, map, (!!))

import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)

import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream, mkStream)
import Streamly.Array.Types

import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Prelude as S

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- @since 0.7.0
{-# INLINE fromStreamN #-}
fromStreamN :: (Monad m, Storable a) => Int -> SerialT m a -> m (Array a)
fromStreamN n m = fromStreamDN n $ D.toStreamD m
-- fromStreamN n = FL.foldl (FL.toArrayN n)

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Convert an 'Array' into a stream.
--
-- @since 0.7.0
{-# INLINABLE toStream #-}
toStream :: (Monad m, IsStream t, Storable a) => Array a -> t m a
toStream = P.fromArray

{-# INLINE null #-}
null :: Storable a => Array a -> Bool
null arr = length arr == 0

{-# INLINE _last #-}
_last :: forall a. Storable a => Array a -> Maybe a
_last arr = arr !! (length arr - 1)

-------------------------------------------------------------------------------
-- Random Access
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index.
--
-- @since 0.7.0
{-# INLINE (!!) #-}
(!!) :: Storable a => Array a -> Int -> Maybe a
arr !! i =
    if i < 0 || i > length arr - 1
    then Nothing
    else Just $ unsafeIndex arr i

-------------------------------------------------------------------------------
-- Streams of Arrays
-------------------------------------------------------------------------------

-- | Convert a stream of Arrays into a stream of their elements.
--
-- @since 0.7.0
{-# INLINE flattenArrays #-}
flattenArrays :: (IsStream t, Monad m, Storable a) => t m (Array a) -> t m a
flattenArrays m = D.fromStreamD $ D.flattenArrays (D.toStreamD m)

{-
-- | @fromStreamArraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
{-# INLINE fromStreamArraysOf #-}
fromStreamArraysOf :: (IsStream t, Monad m, Storable a)
    => Int -> t m a -> t m (Array a)
fromStreamArraysOf n str =
    D.fromStreamD $ D.groupsOf n (fromStreamN n) (D.toStreamD str)
    -}

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINABLE spliceArrays #-}
spliceArrays :: (Monad m, Storable a) => SerialT m (Array a) -> m (Array a)
spliceArrays s = do
    buffered <- S.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)

    let !arr = unsafeDupablePerformIO $ unsafeNew len
    end <- S.foldl' write (aEnd arr) buffered
    return $ arr {aEnd = end}

    where

    write dst Array{..} =
        let !dst' = unsafeInlineIO $ withForeignPtr aStart $ \src -> do
                        let len = aEnd `minusPtr` src
                        memcpy (castPtr dst) (castPtr src) len
                        return $ dst `plusPtr` len
         in dst'

{-
-- CAUTION: a very large number (millions) of arrays can degrade performance
-- due to GC overhead because we need to buffer the arrays before we flatten
-- all the arrays.
--
-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'fromStreamN' is more efficient
-- when the size is already known.
--
{-# INLINE fromStream #-}
fromStream :: (Monad m, Storable a) => SerialT m a -> m (Array a)
fromStream m = do
    let s = fromStreamArraysOf defaultChunkSize $ D.toStreamD m
        buffered = S.foldr S.cons S.nil s
        len = S.sum (S.map length buffered)
    fromStreamN len $ flattenArrays buffered
-}

{-
-- Move this to FileIO
-- XXX we should use overWrite/write
-- | Write a stream of arrays to a handle.
{-# INLINE toHandleArrays #-}
toHandleArrays :: (MonadIO m, Storable a) => Handle -> SerialT m (Array a) -> m ()
toHandleArrays h m = S.mapM_ (liftIO . toHandle h) m
-}

-------------------------------------------------------------------------------
-- IO (Input)
-------------------------------------------------------------------------------

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
{-# INLINABLE fromHandleUpto #-}
fromHandleUpto :: Int -> Handle -> IO (Array Word8)
fromHandleUpto size h = do
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

-------------------------------------------------------------------------------
-- IO (output)
-------------------------------------------------------------------------------

-- | Write an Array to a file handle.
--
-- @since 0.7.0
{-# INLINABLE toHandle #-}
toHandle :: Storable a => Handle -> Array a -> IO ()
toHandle _ arr | null arr = return ()
toHandle h Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @fromHandleArraysUpto size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @size@.
-- 'fromHandleArraysUpto' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
{-# INLINABLE fromHandleArraysUpto #-}
fromHandleArraysUpto :: (IsStream t, MonadIO m)
    => Int -> Handle -> t m (Array Word8)
fromHandleArraysUpto size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld sng _ -> do
        vec <- liftIO $ fromHandleUpto size h
        if length vec < size
        then sng vec
        else yld vec go

-- XXX read 'Array a' instead of Word8
--
-- | @fromHandleArrays h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @defaultChunkSize@.
-- 'fromHandleArrays' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
--
-- @since 0.7.0
{-# INLINE fromHandleArrays #-}
fromHandleArrays :: (IsStream t, MonadIO m) => Handle -> t m (Array Word8)
fromHandleArrays = fromHandleArraysUpto defaultChunkSize
