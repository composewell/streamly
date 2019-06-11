{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Mem.Array
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

module Streamly.Mem.Array
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
    , toArray

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
    -- * Immutable Transformations
    , transformWith

    -- * Folds
    , foldWith
    , foldArray
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read)

import Streamly.Mem.Array.Types hiding (flattenArrays, newArray)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)
import Streamly.Fold.Types (Fold(..))

import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array.Types as A
import qualified Streamly.Prelude as S
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.Prelude as P

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
writeN n m = do
    if n < 0 then error "writeN: negative write count specified" else return ()
    fromStreamDN n $ D.toStreamD m

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

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

-- exponentially increasing sizes of the chunks upto the max limit.
-- XXX this will be easier to implement with parsers/terminating folds
-- With this we should be able to reduce the number of chunks/allocations.
-- The reallocation/copy based toArray can also be implemented using this.
--
{-
{-# INLINE toArraysInRange #-}
toArraysInRange :: (IsStream t, MonadIO m, Storable a)
    => Int -> Int -> Fold m (Array a) b -> Fold m a b
toArraysInRange low high (Fold step initial extract) =
-}

-- | Fold the input to a pure buffered stream (List) of arrays.
{-# INLINE _toArraysOf #-}
_toArraysOf :: (MonadIO m, Storable a)
    => Int -> Fold m a (SerialT Identity (Array a))
_toArraysOf n = FL.lchunksOf n (toArrayN n) FL.toStream

-- XXX The realloc based implementation needs to make one extra copy if we use
-- shrinkToFit.  On the other hand, the stream of arrays implementation may
-- buffer the array chunk pointers in memory but it does not have to shrink as
-- we know the exact size in the end. However, memory copying does not seems to
-- be as expensive as the allocations. Therefore, we need to reduce the number
-- of allocations instead. Also, the size of allocations matters, right sizing
-- an allocation even at the cost of copying sems to help.  Should be measured
-- on a big stream with heavy calls to toArray to see the effect.
--
-- XXX check if GHC's memory allocator is efficient enough. We can try the C
-- malloc to compare against.

{-# INLINE bytesToCount #-}
bytesToCount :: Storable a => a -> Int -> Int
bytesToCount x n =
    let elemSize = sizeOf x
    in n + elemSize - 1 `div` elemSize

{-# INLINE toArrayMinChunk #-}
toArrayMinChunk :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
-- toArrayMinChunk n = FL.mapM spliceArrays $ toArraysOf n
toArrayMinChunk elemCount = Fold step initial extract

    where

    insertElem (Array start end bound) x = do
        liftIO $ poke end x
        return $ Array start (end `plusPtr` sizeOf (undefined :: a)) bound

    initial = do
        when (elemCount < 0) $ error "toArrayMinChunk: elemCount is negative"
        liftIO $ A.newArray elemCount
    step arr@(Array _ end bound) x | end == bound = do
        arr1 <- liftIO $ reallocDouble 1 arr
        insertElem arr1 x
    step arr x = insertElem arr x
    extract = liftIO . shrinkToFit

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE toArray #-}
toArray :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
toArray = toArrayMinChunk (bytesToCount (undefined :: a) (mkChunkSize 1024))

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

-- XXX Both of these implementations of splicing seem to perform equally well.
-- We need to perform benchmarks over a range of sizes though.

{-# INLINE _spliceArraysRealloced #-}
_spliceArraysRealloced :: (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
_spliceArraysRealloced s = do
    buffered <- P.foldr S.cons S.nil s
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

{-# INLINE spliceArraysBuffered #-}
spliceArraysBuffered :: forall m a. (MonadIO m, Storable a)
    => SerialT m (Array a) -> m (Array a)
spliceArraysBuffered s = do
    idst <- liftIO $ A.newArray (bytesToCount (undefined :: a)
                                (mkChunkSizeKB 4))
    arr <- S.foldlM' appendArr idst s
    liftIO $ shrinkToFit arr

    where

    appendArr dst@(Array _ end bound) src  = liftIO $ do
        let srcLen = aEnd src `minusPtr` unsafeForeignPtrToPtr (aStart src)
        dst1 <-
            if end `plusPtr` srcLen >= bound
            then reallocDouble srcLen dst
            else return dst

        withForeignPtr (aStart dst1) $ \_ -> do
            withForeignPtr (aStart src) $ \psrc -> do
                let pdst = aEnd dst1
                memcpy (castPtr pdst) (castPtr psrc) srcLen
                return $ dst1 { aEnd = pdst `plusPtr` srcLen }

-- | Given a stream of arrays, splice them all together to generate a single
-- array. The stream must be /finite/.
--
-- @since 0.7.0
{-# INLINE spliceArrays #-}
spliceArrays :: (MonadIO m, Storable a) => SerialT m (Array a) -> m (Array a)
spliceArrays s = do
    r <- S.uncons s
    case r of
        Nothing -> write S.nil
        Just (h, t) -> do
            r1 <- S.uncons t
            case r1 of
                Nothing -> return h
                Just (h1, t1) -> spliceArraysBuffered $
                    h `S.cons` h1 `S.cons` t1
-- spliceArrays = _spliceArraysRealloced

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
write = FL.foldl' toArray
-- write m = A.fromStreamD $ D.toStreamD m

-- XXX efficiently compare two streams of arrays. Two streams can have chunks
-- of different sizes, we can handle that in the stream comparison abstraction.
-- This could be useful e.g. to fast compare whether two files differ.

-------------------------------------------------------------------------------
-- Transform via stream operations
-------------------------------------------------------------------------------

-- for non-length changing operations we can use the original length for
-- allocation. If we can predict the length then we can use the prediction for
-- new allocation. Otherwise we can use a hint and adjust dynamically.

-- | Transform an array into another array using a stream transformation
-- operation.
--
-- @since 0.7.0
{-# INLINE transformWith #-}
transformWith :: (MonadIO m, Storable a, Storable b)
    => (SerialT m a -> SerialT m b) -> Array a -> m (Array b)
transformWith f arr = FL.foldl' (toArrayMinChunk (length arr)) $ f (read arr)

-- | Fold an array using a 'Fold'.
--
-- @since 0.7.0
{-# INLINE foldArray #-}
foldArray :: (MonadIO m, Storable a) => Fold m a b -> Array a -> m b
foldArray f arr = FL.foldl' f (read arr)

-- | Fold an array using a stream fold operation.
--
-- @since 0.7.0
{-# INLINE foldWith #-}
foldWith :: (MonadIO m, Storable a) => (SerialT m a -> m b) -> Array a -> m b
foldWith f arr = f (read arr)
