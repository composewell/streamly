-- |
-- Module      : Streamly.Internal.Data.Array
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
-- 'Streamly.Internal.Data.Stream.Chunked.toArray' to concatenate N
-- arrays at once.
--
-- Each array is one pointer visible to the GC.  Too many small arrays (e.g.
-- single byte) are only as good as holding those elements in a Haskell list.
-- However, small arrays can be compacted into large ones to reduce the
-- overhead. To hold 32GB memory in 32k sized buffers we need 1 million arrays
-- if we use one array for each chunk. This is still significant to add
-- pressure to GC.

module Streamly.Internal.Data.Array
    (
      Array

    -- * Construction

    -- Pure List APIs
    , A.fromListN
    , A.fromList

    -- Stream Folds
    , fromStreamN
    , fromStream

    -- Monadic Folds
    , A.writeN      -- drop new
    , A.writeNAligned
    , A.write       -- full buffer
    , writeLastN

    -- * Elimination
    -- ** Conversion
    , A.toList

    -- ** Streams
    , A.read
    , A.readRev

    -- ** Unfolds
    , reader
    , readerUnsafe
    , A.readerRev
    , producer -- experimental

    -- * Random Access
    -- , (!!)
    , getIndex
    , A.unsafeIndex -- XXX Rename to getIndexUnsafe??
    , getIndexRev
    , last           -- XXX getIndexLast?
    , getIndices
    , getIndicesFromThenTo
    -- , getIndicesFrom    -- read from a given position to the end of file
    -- , getIndicesUpto    -- read from beginning up to the given position
    -- , getIndicesFromTo
    -- , getIndicesFromRev  -- read from a given position to the beginning of file
    -- , getIndicesUptoRev  -- read from end to the given position in file

    -- * Size
    , length
    , null

    -- * Search
    , binarySearch
    , findIndicesOf
    -- , findIndexOf
    -- , find

    -- * Casting
    , cast
    , asBytes
    , castUnsafe
    , asPtrUnsafe
    , asCStringUnsafe
    , A.unsafeFreeze -- asImmutableUnsafe?
    , A.unsafeThaw   -- asMutableUnsafe?

    -- * Subarrays
    , getSliceUnsafe
    -- , getSlice
    , genSlicesFromLen
    , getSlicesFromLen
    , splitOn

    -- * Streaming Operations
    , streamTransform

    -- ** Folding
    , streamFold
    , fold

    -- * Deprecated
    , A.toStream
    , A.toStreamRev
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Unboxed
    ( Unbox
    , castContents
    , peekWith
    , sizeOf
    )
import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Data.Array.Mut.Type (ArrayUnsafe(..))
import Streamly.Internal.Data.Array.Type
    (Array(..), length, asPtrUnsafe)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple3Fused'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Array.Mut.Type as MA
import qualified Streamly.Internal.Data.Array.Mut as MA
import qualified Streamly.Internal.Data.Array.Type as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Producer.Type as Producer
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Ring.Unboxed as RB
import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- /Pre-release/
{-# INLINE fromStreamN #-}
fromStreamN :: (MonadIO m, Unbox a) => Int -> Stream m a -> m (Array a)
fromStreamN n m = do
    when (n < 0) $ error "writeN: negative write count specified"
    A.fromStreamDN n $ Stream.toStreamD m

-- | Create an 'Array' from a stream. This is useful when we want to create a
-- single array from a stream of unknown size. 'writeN' is at least twice
-- as efficient when the size is already known.
--
-- Note that if the input stream is too large memory allocation for the array
-- may fail.  When the stream size is not known, `arraysOf` followed by
-- processing of indvidual arrays in the resulting stream should be preferred.
--
-- /Pre-release/
{-# INLINE fromStream #-}
fromStream :: (MonadIO m, Unbox a) => Stream m a -> m (Array a)
fromStream m = P.fold A.write $ Stream.toStreamK m
-- write m = A.fromStreamD $ D.fromStreamK m

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE_NORMAL producer #-}
producer :: forall m a. (Monad m, Unbox a) => Producer m (Array a) a
producer =
    Producer.translate A.unsafeThaw A.unsafeFreeze
        $ MA.producerWith (return . unsafeInlineIO)

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: forall m a. (Monad m, Unbox a) => Unfold m (Array a) a
reader = Producer.simplify producer

-- | Unfold an array into a stream, does not check the end of the array, the
-- user is responsible for terminating the stream within the array bounds. For
-- high performance application where the end condition can be determined by
-- a terminating fold.
--
-- Written in the hope that it may be faster than "read", however, in the case
-- for which this was written, "read" proves to be faster even though the core
-- generated with unsafeRead looks simpler.
--
-- /Pre-release/
--
{-# INLINE_NORMAL readerUnsafe #-}
readerUnsafe :: forall m a. (Monad m, Unbox a) => Unfold m (Array a) a
readerUnsafe = Unfold step inject
    where

    inject (Array contents start end) =
        return (ArrayUnsafe contents end start)

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe contents end p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ peekWith contents p
            let !p1 = INDEX_NEXT(p,a)
            return $ D.Yield x (ArrayUnsafe contents end p1)

-- |
--
-- >>> import qualified Streamly.Internal.Data.Array.Type as Array
-- >>> null arr = Array.byteLength arr == 0
--
-- /Pre-release/
{-# INLINE null #-}
null :: Array a -> Bool
null arr = A.byteLength arr == 0

-- | Like 'getIndex' but indexes the array in reverse from the end.
--
-- /Pre-release/
{-# INLINE getIndexRev #-}
getIndexRev :: forall a. Unbox a => Int -> Array a -> Maybe a
getIndexRev i arr =
    unsafeInlineIO
        $ do
                let elemPtr = RINDEX_OF(arrEnd arr, i, a)
                if i >= 0 && elemPtr >= arrStart arr
                then Just <$> peekWith (arrContents arr) elemPtr
                else return Nothing

-- |
--
-- >>> import qualified Streamly.Internal.Data.Array as Array
-- >>> last arr = Array.getIndexRev arr 0
--
-- /Pre-release/
{-# INLINE last #-}
last :: Unbox a => Array a -> Maybe a
last = getIndexRev 0

-------------------------------------------------------------------------------
-- Folds with Array as the container
-------------------------------------------------------------------------------

-- | @writeLastN n@ folds a maximum of @n@ elements from the end of the input
-- stream to an 'Array'.
--
{-# INLINE writeLastN #-}
writeLastN ::
       (Storable a, Unbox a, MonadIO m) => Int -> Fold m a (Array a)
writeLastN n
    | n <= 0 = fmap (const mempty) FL.drain
    | otherwise = A.unsafeFreeze <$> Fold step initial done

    where

    step (Tuple3Fused' rb rh i) a = do
        rh1 <- liftIO $ RB.unsafeInsert rb rh a
        return $ FL.Partial $ Tuple3Fused' rb rh1 (i + 1)

    initial =
        let f (a, b) = FL.Partial $ Tuple3Fused' a b (0 :: Int)
         in fmap f $ liftIO $ RB.new n

    done (Tuple3Fused' rb rh i) = do
        arr <- liftIO $ MA.newPinned n
        foldFunc i rh snoc' arr rb

    -- XXX We should write a read unfold for ring.
    snoc' b a = liftIO $ MA.snocUnsafe b a

    foldFunc i
        | i < n = RB.unsafeFoldRingM
        | otherwise = RB.unsafeFoldRingFullM

-------------------------------------------------------------------------------
-- Random Access
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Searching
-------------------------------------------------------------------------------

-- | Given a sorted array, perform a binary search to find the given element.
-- Returns the index of the element if found.
--
-- /Unimplemented/
{-# INLINE binarySearch #-}
binarySearch :: a -> Array a -> Maybe Int
binarySearch = undefined

-- find/findIndex etc can potentially be implemented more efficiently on arrays
-- compared to streams by using SIMD instructions.
-- We can also return a bit array instead.

-- | Perform a linear search to find all the indices where a given element is
-- present in an array.
--
-- /Unimplemented/
findIndicesOf :: (a -> Bool) -> Unfold Identity (Array a) Int
findIndicesOf = undefined

{-
findIndexOf :: (a -> Bool) -> Array a -> Maybe Int
findIndexOf p = Unfold.fold Fold.one . Stream.unfold (findIndicesOf p)

find :: (a -> Bool) -> Array a -> Bool
find = Unfold.fold Fold.null . Stream.unfold (findIndicesOf p)
-}

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- XXX We can potentially use SIMD instructions on arrays to fold faster.

-------------------------------------------------------------------------------
-- Slice
-------------------------------------------------------------------------------

-- | /O(1)/ Slice an array in constant time.
--
-- Caution: The bounds of the slice are not checked.
--
-- /Unsafe/
--
-- /Pre-release/
{-# INLINE getSliceUnsafe #-}
getSliceUnsafe ::
       forall a. Unbox a
    => Int -- ^ starting index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
getSliceUnsafe index len (Array contents start e) =
    let size = SIZE_OF(a)
        start1 = start + (index * size)
        end1 = start1 + (len * size)
     in assert (end1 <= e) (Array contents start1 end1)

-- | Split the array into a stream of slices using a predicate. The element
-- matching the predicate is dropped.
--
-- /Pre-release/
{-# INLINE splitOn #-}
splitOn :: (Monad m, Unbox a) =>
    (a -> Bool) -> Array a -> Stream m (Array a)
splitOn predicate arr =
    Stream.fromStreamD
        $ fmap (\(i, len) -> getSliceUnsafe i len arr)
        $ D.sliceOnSuffix predicate (A.toStreamD arr)

{-# INLINE genSlicesFromLen #-}
genSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Int, Int)
genSlicesFromLen from len =
    Unfold.lmap A.unsafeThaw (MA.genSlicesFromLen from len)

-- | Generate a stream of slices of specified length from an array, starting
-- from the supplied array index. The last slice may be shorter than the
-- requested length.
--
-- /Pre-release//
{-# INLINE getSlicesFromLen #-}
getSlicesFromLen :: forall m a. (Monad m, Unbox a)
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Unfold m (Array a) (Array a)
getSlicesFromLen from len =
    fmap A.unsafeFreeze
        $ Unfold.lmap A.unsafeThaw (MA.getSlicesFromLen from len)

-------------------------------------------------------------------------------
-- Random reads and writes
-------------------------------------------------------------------------------

-- XXX Change this to a partial function instead of a Maybe type? And use
-- MA.getIndex instead.
--
-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: forall a. Unbox a => Int -> Array a -> Maybe a
getIndex i arr =
    unsafeInlineIO
        $ do
                let elemPtr = INDEX_OF(arrStart arr, i, a)
                if i >= 0 && INDEX_VALID(elemPtr, arrEnd arr, a)
                then Just <$> peekWith (arrContents arr) elemPtr
                else return Nothing

-- | Given a stream of array indices, read the elements on those indices from
-- the supplied Array. An exception is thrown if an index is out of bounds.
--
-- This is the most general operation. We can implement other operations in
-- terms of this:
--
-- @
-- read =
--      let u = lmap (\arr -> (0, length arr - 1)) Unfold.enumerateFromTo
--       in Unfold.lmap f (getIndices arr)
--
-- readRev =
--      let i = length arr - 1
--       in Unfold.lmap f (getIndicesFromThenTo i (i - 1) 0)
-- @
--
-- /Pre-release/
{-# INLINE getIndices #-}
getIndices :: (Monad m, Unbox a) => Stream m Int -> Unfold m (Array a) a
getIndices m =
    let unf = MA.getIndicesD (return . unsafeInlineIO) $ D.fromStreamK $ Stream.toStreamK m
     in Unfold.lmap A.unsafeThaw unf

-- | Unfolds @(from, then, to, array)@ generating a finite stream whose first
-- element is the array value from the index @from@ and the successive elements
-- are from the indices in increments of @then@ up to @to@. Index enumeration
-- can occur downwards or upwards depending on whether @then@ comes before or
-- after @from@.
--
-- @
-- getIndicesFromThenTo =
--     let f (from, next, to, arr) =
--             (Stream.enumerateFromThenTo from next to, arr)
--      in Unfold.lmap f getIndices
-- @
--
-- /Unimplemented/
{-# INLINE getIndicesFromThenTo #-}
getIndicesFromThenTo :: Unfold m (Int, Int, Int, Array a) a
getIndicesFromThenTo = undefined

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
{-# INLINE runPipe #-}
runPipe :: (MonadIO m, Unbox a, Unbox b)
    => Pipe m a b -> Array a -> m (Array b)
runPipe f arr = P.runPipe (toArrayMinChunk (length arr)) $ f (A.read arr)
-}

-- XXX For transformations that cannot change the number of elements e.g. "map"
-- we can use a predetermined array length.
--
-- | Transform an array into another array using a stream transformation
-- operation.
--
-- /Pre-release/
{-# INLINE streamTransform #-}
streamTransform :: forall m a b. (MonadIO m, Unbox a, Unbox b)
    => (Stream m a -> Stream m b) -> Array a -> m (Array b)
streamTransform f arr =
    P.fold (A.writeWith (length arr)) $ Stream.toStreamK $ f (A.read arr)

-------------------------------------------------------------------------------
-- Casts
-------------------------------------------------------------------------------

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The array size must be a multiple of the size of type @b@
-- otherwise accessing the last element of the array may result into a crash or
-- a random value.
--
-- /Pre-release/
--
castUnsafe ::
#ifdef DEVBUILD
    Unboxed b =>
#endif
    Array a -> Array b
castUnsafe (Array contents start end) =
    Array (castContents contents) start end

-- | Cast an @Array a@ into an @Array Word8@.
--
--
asBytes :: Array a -> Array Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
--
cast :: forall a b. (Unbox b) => Array a -> Maybe (Array b)
cast arr =
    let len = A.byteLength arr
        r = len `mod` SIZE_OF(b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

-- | Convert an array of any type into a null terminated CString Ptr.
--
-- /Unsafe/
--
-- /O(n) Time: (creates a copy of the array)/
--
-- /Pre-release/
--
asCStringUnsafe :: Array a -> (CString -> IO b) -> IO b
asCStringUnsafe arr act = do
    -- XXX Ensure a pinned allocation here.
    let arr1 = asBytes arr <> A.fromList [0]
    asPtrUnsafe arr1 $ \ptr -> act (castPtr ptr)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- XXX We can directly use toStreamD and D.fold here.

-- | Fold an array using a 'Fold'.
--
-- /Pre-release/
{-# INLINE fold #-}
fold :: forall m a b. (Monad m, Unbox a) => Fold m a b -> Array a -> m b
fold f arr = P.fold f (Stream.toStreamK (A.read arr))

-- | Fold an array using a stream fold operation.
--
-- /Pre-release/
{-# INLINE streamFold #-}
streamFold :: (Monad m, Unbox a) => (Stream m a -> m b) -> Array a -> m b
streamFold f arr = f (A.read arr)
