{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Storable.Foreign
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

module Streamly.Internal.Data.Array.Storable.Foreign
    (
      Array

    -- , defaultChunkSize

    -- * Construction

    -- Pure, From Static Memory (Unsafe)
    -- We can use fromPtrM#, fromCStringM# and fromAddrM# to create arrays from
    -- a dynamic memory address which requires a finalizer.
    , A.fromPtr
    , A.fromAddr#
    , A.fromCString#

    -- Pure List APIs
    , A.fromListN
    , A.fromList

    -- Stream Folds
    , fromStreamN
    , fromStream

    -- Monadic APIs
    -- , newArray
    , A.writeN      -- drop new
    , A.writeNAligned
    , A.write       -- full buffer
    -- , writeLastN -- drop old (ring buffer)

    -- * Elimination

    , A.toList
    , toStream
    , toStreamRev
    , read
    , ReadUState (..)
    , unsafeRead
    -- , readChunksOf

    -- * Random Access
    , length
    , null
    , last
    -- , (!!)
    , readIndex
    , A.unsafeIndex
    -- , readIndices
    -- , readRanges

    -- , readFrom    -- read from a given position to the end of file
    -- , readFromRev -- read from a given position to the beginning of file
    -- , readTo      -- read from beginning up to the given position
    -- , readToRev   -- read from end to the given position in file
    -- , readFromTo
    -- , readFromThenTo

    -- , readChunksOfFrom
    -- , ...

    -- , writeIndex
    -- , writeFrom -- start writing at the given position
    -- , writeFromRev
    -- , writeTo   -- write from beginning up to the given position
    -- , writeToRev
    -- , writeFromTo
    -- , writeFromThenTo
    --
    -- , writeChunksOfFrom
    -- , ...

    , writeIndex
    --, writeIndices
    --, writeRanges

    -- -- * Search
    -- , bsearch
    -- , bsearchIndex
    -- , find
    -- , findIndex
    -- , findIndices

    -- -- * In-pace mutation (for Mutable Array type)
    -- , partitionBy
    -- , shuffleBy
    -- , foldtWith
    -- , foldbWith

    -- * Immutable Transformations
    , streamTransform

    -- * Casting
    , cast
    , unsafeCast
    , asPtr
    , asByteArray
    , asCString

    -- * Folding Arrays
    , streamFold
    , fold

    -- * Folds with Array as the container
    , D.lastN
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Data.Word (Word8)
-- import Data.Functor.Identity (Identity)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr, castForeignPtr)
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read, concat)

import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))
import GHC.Prim (touch#)
import GHC.IO (IO(..))

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array(..), length)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- /Internal/
{-# INLINE fromStreamN #-}
fromStreamN :: (MonadIO m, Storable a) => Int -> SerialT m a -> m (Array a)
fromStreamN n m = do
    when (n < 0) $ error "writeN: negative write count specified"
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
fromStream = P.foldOnce A.write
-- write m = A.fromStreamD $ D.toStreamD m

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Convert an 'Array' into a stream.
--
-- /Internal/
{-# INLINE_EARLY toStream #-}
toStream :: (Monad m, K.IsStream t, Storable a) => Array a -> t m a
toStream = D.fromStreamD . A.toStreamD
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.read fallback to StreamK" [1]
--     forall a. S.readK (read a) = K.fromArray a #-}

-- | Convert an 'Array' into a stream in reverse order.
--
-- /Internal/
{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (Monad m, IsStream t, Storable a) => Array a -> t m a
toStreamRev = D.fromStreamD . A.toStreamDRev
-- XXX add fallback to StreamK rule
-- {-# RULES "Streamly.Array.readRev fallback to StreamK" [1]
--     forall a. S.toStreamK (readRev a) = K.revFromArray a #-}

data ReadUState a = ReadUState
    {-# UNPACK #-} !(ForeignPtr a)  -- foreign ptr with end of array pointer
    {-# UNPACK #-} !(Ptr a)         -- current pointer

-- | Unfold an array into a stream.
--
-- @since 0.7.0
{-# INLINE_NORMAL read #-}
read :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
read = Unfold step inject
    where

    inject (Array (ForeignPtr start contents) (Ptr end)) =
        return $ ReadUState (ForeignPtr end contents) (Ptr start)

    {-# INLINE_LATE step #-}
    step (ReadUState fp@(ForeignPtr end _) p) | p == Ptr end =
        let x = A.unsafeInlineIO $ touchForeignPtr fp
        in x `seq` return D.Stop
    step (ReadUState fp p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = A.unsafeInlineIO $ peek p
            return $ D.Yield x
                (ReadUState fp (p `plusPtr` sizeOf (undefined :: a)))

-- | Unfold an array into a stream, does not check the end of the array, the
-- user is responsible for terminating the stream within the array bounds. For
-- high performance application where the end condition can be determined by
-- a terminating fold.
--
-- Written in the hope that it may be faster than "read", however, in the case
-- for which this was written, "read" proves to be faster even though the core
-- generated with unsafeRead looks simpler.
--
-- /Internal/
--
{-# INLINE_NORMAL unsafeRead #-}
unsafeRead :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
unsafeRead = Unfold step inject
    where

    inject (Array fp _) = return fp

    {-# INLINE_LATE step #-}
    step (ForeignPtr p contents) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = A.unsafeInlineIO $ do
                        r <- peek (Ptr p)
                        touch contents
                        return r
            let !(Ptr p1) = Ptr p `plusPtr` sizeOf (undefined :: a)
            return $ D.Yield x (ForeignPtr p1 contents)

    touch r = IO $ \s -> case touch# r s of s' -> (# s', () #)

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
-- /Internal/
{-# INLINE readIndex #-}
readIndex :: Storable a => Array a -> Int -> Maybe a
readIndex arr i =
    if i < 0 || i > length arr - 1
        then Nothing
        else A.unsafeInlineIO $
             withForeignPtr (aStart arr) $ \p -> Just <$> peekElemOff p i

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
    => Array a -> Int -> Int -> t m a -> m (Array a)
writeSlice arr i len s = undefined

-- | @writeSliceRev arr i count stream@ writes a stream to the array @arr@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE writeSliceRev #-}
writeSliceRev :: (IsStream t, Monad m, Storable a)
    => Array a -> Int -> Int -> t m a -> m (Array a)
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
    P.foldOnce (A.toArrayMinChunk (alignment (undefined :: a)) (length arr))
        $ f (toStream arr)

-------------------------------------------------------------------------------
-- Casts
-------------------------------------------------------------------------------

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The array size must be a multiple of the size of type @b@
-- otherwise accessing the last element of the array may result into a crash or
-- a random value.
--
-- /Internal/
--
unsafeCast ::
#ifdef DEVBUILD
    Storable b =>
#endif
    Array a -> Array b
unsafeCast (Array start end) = Array (castForeignPtr start) (castPtr end)

-- | Cast an array into a Word8 array
--
-- /Internal/
--
asByteArray :: Array a -> Array Word8
asByteArray = unsafeCast

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
-- /Internal/
--
cast :: forall a b. (Storable b) => Array a -> Maybe (Array b)
cast arr =
    let len = A.byteLength arr
        r = len `mod` sizeOf (undefined :: b)
     in if r /= 0
        then Nothing
        else Just $ unsafeCast arr

-- | Use an @Array a@ as @Ptr b@.
--
-- /Unsafe/
--
-- /Internal/
--
asPtr :: Array a -> (Ptr b -> IO c) -> IO c
asPtr Array{..} act = do
    withForeignPtr aStart $ \ptr -> act (castPtr ptr)

-- | Convert an array of any type into a null terminated CString Ptr.
--
-- /Unsafe/
--
-- /O(n) Time: (creates a copy of the array)/
--
-- /Internal/
--
asCString :: Array a -> (CString -> IO b) -> IO b
asCString arr act = do
    let Array{..} = asByteArray arr <> A.fromList [0]
    withForeignPtr aStart $ \ptr -> act (castPtr ptr)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | Fold an array using a 'Fold'.
--
-- /Internal/
{-# INLINE fold #-}
fold :: forall m a b. (MonadIO m, Storable a) => Fold m a b -> Array a -> m b
fold f arr = P.foldOnce f (toStream arr :: Serial.SerialT m a)

-- | Fold an array using a stream fold operation.
--
-- /Internal/
{-# INLINE streamFold #-}
streamFold :: (MonadIO m, Storable a) => (SerialT m a -> m b) -> Array a -> m b
streamFold f arr = f (toStream arr)
