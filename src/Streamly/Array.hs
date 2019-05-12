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
--
-- 'ByteString' data type from the 'bytestring' package and the 'Text' data
-- type from the 'text' package are just special cases of arrays.  'ByteString'
-- is equivalent to @Array Word8@ and 'Text' is equivalent to a @utf16@ encoded
-- @Array Word8@. All the 'bytestring' and 'text' operations can be performed
-- on arrays with equivalent or better performance by converting them to and
-- from streams.
--
-- This module is designed to be imported qualified:
--
-- > import qualified Streamly.Array as A

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

    -- , defaultChunkSize

    -- * Construction
    -- | When performance matters, the fastest way to generate an array is
    -- 'writeN'. For regular use, 'IsList' and 'IsString' instances can be
    -- used to conveniently construct arrays from literal values.
    -- 'OverloadedLists' extension or 'fromList' can be used to construct an
    -- array from a list literal.  Similarly, 'OverloadedStrings' extension or
    -- 'fromList' can be used to construct an array from a string literal.

    -- , newArray
    , writeN
    , write
    , fromListN
    , fromList

    -- Folds
    , toArrayN
    -- , toArrays
    -- , toArray

    -- Streams
    , arraysOf

    -- * Elimination
    -- 'GHC.Exts.toList' from "GHC.Exts" can be used to convert an array to a
    -- list.

    , read
    , readRev
    , toList
    , flattenArrays
    -- , flattenArraysRev
    , spliceArrays

    -- * Random Access
    , length
    -- , (!!)

    , readIndex
    {-
    , readSlice
    , readSliceRev

    , writeIndex
    , writeSlice
    , writeSliceRev
    -}
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read)

import Streamly.Array.Types hiding (flattenArrays, newArray)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)


import qualified Streamly.Array.Types as A
import qualified Streamly.Prelude as S
import qualified Streamly.Streams.StreamD as D

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

{-
-- | Create a new uninitialized array of given length.
--
-- @since 0.7.0
newArray :: (MonadIO m, Storable a) => Int -> m (Array a)
newArray len = undefined
-}

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- @since 0.7.0
{-# INLINE writeN #-}
writeN :: (MonadIO m, Storable a) => Int -> SerialT m a -> m (Array a)
writeN n m = fromStreamDN n $ D.toStreamD m

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Convert an 'Array' into a stream.
--
-- @since 0.7.0
{-# INLINE_EARLY read #-}
read :: (Monad m, IsStream t, Storable a) => Array a -> t m a
read = D.fromStreamD . toStreamD
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.read fallback to StreamK" [1]
--     forall a. S.readK (read a) = K.fromArray a #-}

-- | Convert an 'Array' into a stream in reverse order.
--
-- @since 0.7.0
{-# INLINE_EARLY readRev #-}
readRev :: (Monad m, IsStream t, Storable a) => Array a -> t m a
readRev = D.fromStreamD . toStreamDRev
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.readRev fallback to StreamK" [1]
--     forall a. S.toStreamK (readRev a) = K.revFromArray a #-}

{-# INLINE _null #-}
_null :: Storable a => Array a -> Bool
_null arr = length arr == 0

{-# INLINE _last #-}
_last :: forall a. Storable a => Array a -> Maybe a
_last arr = readIndex arr (length arr - 1)

-------------------------------------------------------------------------------
-- Random Access
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index.
--
-- @since 0.7.0
{-# INLINE readIndex #-}
readIndex :: Storable a => Array a -> Int -> Maybe a
readIndex arr i =
    if i < 0 || i > length arr - 1
    then Nothing
    else Just $ unsafeIndex arr i

{-
-- | @readSlice arr i count@ streams a slice of the array @arr@ starting
-- at index @i@ and reading up to @count@ elements in the forward direction
-- ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE readSlice #-}
readSlice :: (IsStream t, Monad m, Storable a)
    => Array a -> Int -> Int -> t m a
readSlice arr i len = undefined

-- | @readSliceRev arr i count@ streams a slice of the array @arr@ starting at
-- index @i@ and reading up to @count@ elements in the reverse direction ending
-- at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE readSliceRev #-}
readSliceRev :: (IsStream t, Monad m, Storable a)
    => Array a -> Int -> Int -> t m a
readSliceRev arr i len = undefined

-- | /O(1)/ Write the given element at the given index in the array.
--
-- @since 0.7.0
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Storable a) => Array a -> Int -> a -> m ()
writeIndex arr i a = undefined

-- | @writeSlice arr i count stream@ writes a stream to the array @arr@
-- starting at index @i@ and writing up to @count@ elements in the forward
-- direction ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE writeSlice #-}
writeSlice :: (IsStream t, Monad m, Storable a)
    => Array a -> Int -> Int -> t m a -> m ()
writeSlice arr i len s = undefined

-- | @writeSliceRev arr i count stream@ writes a stream to the array @arr@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE writeSliceRev #-}
writeSliceRev :: (IsStream t, Monad m, Storable a)
    => Array a -> Int -> Int -> t m a -> m ()
writeSliceRev arr i len s = undefined
-}

-------------------------------------------------------------------------------
-- Streams of Arrays
-------------------------------------------------------------------------------

-- | Convert a stream of arrays into a stream of their elements.
--
-- @since 0.7.0
{-# INLINE flattenArrays #-}
flattenArrays :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
flattenArrays m = D.fromStreamD $ A.flattenArrays (D.toStreamD m)

-- XXX should we have a reverseArrays API to reverse the stream of arrays
-- instead?
--
-- | Convert a stream of arrays into a stream of their elements reversing the
-- contents of each array before flattening.
--
-- @since 0.7.0
{-# INLINE _flattenArraysRev #-}
_flattenArraysRev :: (IsStream t, MonadIO m, Storable a) => t m (Array a) -> t m a
_flattenArraysRev m = D.fromStreamD $ A.flattenArraysRev (D.toStreamD m)

-- |
-- > arraysOf n = FL.groupsOf n (FL.toArrayN n)
--
-- Groups the elements in an input stream into arrays of given size.
--
-- @since 0.7.0
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n str =
    D.fromStreamD $ fromStreamDArraysOf n (D.toStreamD str)

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINABLE spliceArrays #-}
spliceArrays :: (MonadIO m, Storable a) => SerialT m (Array a) -> m (Array a)
spliceArrays s = do
    buffered <- S.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)

    arr <- liftIO $ A.newArray len
    end <- S.foldlM' writeArr (aEnd arr) buffered
    return $ arr {aEnd = end}

    where

    writeArr dst Array{..} =
        liftIO $ withForeignPtr aStart $ \src -> do
                        let len = aEnd `minusPtr` src
                        memcpy (castPtr dst) (castPtr src) len
                        return $ dst `plusPtr` len

-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'writeN' is at least twice
-- as efficient when the size is already known.
--
-- Note that if the input stream is too large memory allocation for the array
-- may fail.  When the stream size is not known, `arraysOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
{-# INLINE write #-}
write :: (MonadIO m, Storable a) => SerialT m a -> m (Array a)
write m = A.fromStreamD $ D.toStreamD m
