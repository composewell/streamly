{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Memory.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To summarize:
--
--  * Arrays are finite and fixed in size
--  * provide /O(1)/ access to elements
--  * store only data and not functions
--  * provide efficient IO interfacing
--
-- 'Foldable' instance is not provided because the implementation would be much
-- less efficient compared to folding via streams.  'Semigroup' and 'Monoid'
-- instances should be used with care; concatenating arrays using binary
-- operations can be highly inefficient.  Instead, use
-- 'Streamly.Internal.Memory.ArrayStream.toArray' to concatenate N arrays at
-- once.
--
-- Each array is one pointer visible to the GC.  Too many small arrays (e.g.
-- single byte) are only as good as holding those elements in a Haskell list.
-- However, small arrays can be compacted into large ones to reduce the
-- overhead. To hold 32GB memory in 32k sized buffers we need 1 million arrays
-- if we use one array for each chunk. This is still significant to add
-- pressure to GC.

module Streamly.Internal.Memory.Array
    (
      Array

    -- , defaultChunkSize

    -- * Construction

    -- Pure List APIs
    , A.fromListN
    , A.fromList

    -- Stream Folds
    , fromStreamN
    , fromStream

    -- Monadic APIs
    -- , newArray
    , A.writeN      -- drop new
    , A.write         -- full buffer
    -- , writeLastN -- drop old (ring buffer)

    -- * Elimination
    -- 'GHC.Exts.toList' from "GHC.Exts" can be used to convert an array to a
    -- list.

    , A.toList
    , A.read
    , readRev

    -- * Random Access
    , length
    , null
    -- , (!!)

    , readIndex
    , last
    {-
    , readSlice
    , readSliceRev
    -}

    , writeIndex
    {-
    , writeSlice
    , writeSliceRev
    -}

    -- * Immutable Transformations
    , streamTransform

    -- * Folding Arrays
    , streamFold
    , fold
    )
where

import Control.Monad.IO.Class (MonadIO(..))
-- import Data.Functor.Identity (Identity)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Memory.Array.Types (Array(..), length)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)
import Streamly.Internal.Data.Fold.Types (Fold(..))

import qualified Streamly.Internal.Memory.Array.Types as A
import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Streams.Serial as Serial
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
-- /Internal/
{-# INLINE fromStreamN #-}
fromStreamN :: (MonadIO m, Storable a) => Int -> SerialT m a -> m (Array a)
fromStreamN n m = do
    if n < 0 then error "writeN: negative write count specified" else return ()
    A.fromStreamDN n $ D.toStreamD m

-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'writeN' is at least twice
-- as efficient when the size is already known.
--
-- Note that if the input stream is too large memory allocation for the array
-- may fail.  When the stream size is not known, `arraysOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
-- /Internal/
{-# INLINE fromStream #-}
fromStream :: (MonadIO m, Storable a) => SerialT m a -> m (Array a)
fromStream = P.runFold A.write
-- write m = A.fromStreamD $ D.toStreamD m

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Convert an 'Array' into a stream in reverse order.
--
-- @since 0.7.0
{-# INLINE_EARLY readRev #-}
readRev :: (Monad m, IsStream t, Storable a) => Array a -> t m a
readRev = D.fromStreamD . A.toStreamDRev
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.readRev fallback to StreamK" [1]
--     forall a. S.toStreamK (readRev a) = K.revFromArray a #-}

-- | > null arr = length arr == 0
--
-- /Internal/
{-# INLINE null #-}
null :: Storable a => Array a -> Bool
null arr = length arr == 0

-- | > last arr = readIndex arr (length arr - 1)
--
-- /Internal/
{-# INLINE last #-}
last :: Storable a => Array a -> Maybe a
last arr = readIndex arr (length arr - 1)

-------------------------------------------------------------------------------
-- Random Access
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Searching
-------------------------------------------------------------------------------

{-
-- | Perform a binary search in the array to find an element.
bsearch :: a -> Array a -> Maybe Bool
bsearch = undefined

-- | Perform a binary search in the array to find an element index.
{-# INLINE elemIndex #-}
bsearchIndex :: a -> Array a -> Maybe Int
bsearchIndex elem arr = undefined

-- find/findIndex etc can potentially be implemented more efficiently on arrays
-- compared to streams by using SIMD instructions.

find :: (a -> Bool) -> Array a -> Bool
find = undefined

findIndex :: (a -> Bool) -> Array a -> Maybe Int
findIndex = undefined

findIndices :: (a -> Bool) -> Array a -> Array Int
findIndices = undefined
-}

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- XXX We can potentially use SIMD instructions on arrays to fold faster.

-------------------------------------------------------------------------------
-- Slice and splice
-------------------------------------------------------------------------------

{-
slice :: Int -> Int -> Array a
slice begin end arr = undefined

splitAt :: Int -> Array a -> (Array a, Array a)
splitAt i arr = undefined

-- XXX This operation can be performed efficiently via streams.
-- | Append two arrays together to create a single array.
splice :: Array a -> Array a -> Array a
splice arr1 arr2 = undefined

-------------------------------------------------------------------------------
-- In-place mutation APIs
-------------------------------------------------------------------------------

-- | Partition an array into two halves using a partitioning predicate. The
-- first half retains values where the predicate is 'False' and the second half
-- retains values where the predicate is 'True'.
{-# INLINE partitionBy #-}
partitionBy :: (a -> Bool) -> Array a -> (Array a, Array a)
partitionBy f arr = undefined

-- | Shuffle corresponding elements from two arrays using a shuffle function.
-- If the shuffle function returns 'False' then do nothing otherwise swap the
-- elements. This can be used in a bottom up fold to shuffle or reorder the
-- elements.
shuffleBy :: (a -> a -> m Bool) -> Array a -> Array a -> m (Array a)
shuffleBy f arr1 arr2 = undefined

-- XXX we can also make the folds partial by stopping at a certain level.
--
-- | Perform a top down hierarchical recursive partitioning fold of items in
-- the container using the given function as the partition function.
--
-- This will perform a quick sort if the partition function is
-- 'partitionBy (< pivot)'.
--
-- @since 0.7.0
{-# INLINABLE foldtWith #-}
foldtWith :: Int -> (Array a -> Array a -> m (Array a)) -> Array a -> m (Array a)
foldtWith level f = undefined

-- | Perform a pairwise bottom up fold recursively merging the pairs. Level
-- indicates the level in the tree where the fold would stop.
--
-- This will perform a random shuffle if the shuffle function is random.
-- If we stop at level 0 and repeatedly apply the function then we can do a
-- bubble sort.
foldbWith :: Int -> (Array a -> Array a -> m (Array a)) -> Array a -> m (Array a)
foldbWith level f = undefined
-}

-- XXX consider the bulk update/accumulation/permutation APIs from vector.

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

-- | /O(1)/ Lookup the element at the given index, starting from 0.
--
-- @since 0.7.0
{-# INLINE readIndex #-}
readIndex :: Storable a => Array a -> Int -> Maybe a
readIndex arr i =
    if i < 0 || i > length arr - 1
        then Nothing
        else A.unsafeInlineIO $
             withForeignPtr (aStart arr) $ \p -> fmap Just $ peekElemOff p i

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
-}

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- /Internal/
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Storable a) => Array a -> Int -> a -> m ()
writeIndex arr i a = do
    let maxIndex = length arr - 1
    if i < 0
    then error "writeIndex: negative array index"
    else if i > maxIndex
         then error $ "writeIndex: specified array index " ++ show i
                    ++ " is beyond the maximum index " ++ show maxIndex
         else
            liftIO $ withForeignPtr (aStart arr) $ \p ->
                pokeElemOff p i a

{-
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
-- Transform via stream operations
-------------------------------------------------------------------------------

-- for non-length changing operations we can use the original length for
-- allocation. If we can predict the length then we can use the prediction for
-- new allocation. Otherwise we can use a hint and adjust dynamically.

{-
-- | Transform an array into another array using a pipe transformation
-- operation.
--
-- @since 0.7.0
{-# INLINE runPipe #-}
runPipe :: (MonadIO m, Storable a, Storable b)
    => Pipe m a b -> Array a -> m (Array b)
runPipe f arr = P.runPipe (toArrayMinChunk (length arr)) $ f (A.read arr)
-}

-- | Transform an array into another array using a stream transformation
-- operation.
--
-- /Internal/
{-# INLINE streamTransform #-}
streamTransform :: forall m a b. (MonadIO m, Storable a, Storable b)
    => (SerialT m a -> SerialT m b) -> Array a -> m (Array b)
streamTransform f arr =
    P.runFold (A.toArrayMinChunk (alignment (undefined :: a)) (length arr))
        $ f (A.read arr)

-- | Fold an array using a 'Fold'.
--
-- /Internal/
{-# INLINE fold #-}
fold :: forall m a b. (MonadIO m, Storable a) => Fold m a b -> Array a -> m b
fold f arr = P.runFold f $ (A.read arr :: Serial.SerialT m a)

-- | Fold an array using a stream fold operation.
--
-- /Internal/
{-# INLINE streamFold #-}
streamFold :: (MonadIO m, Storable a) => (SerialT m a -> m b) -> Array a -> m b
streamFold f arr = f (A.read arr)
