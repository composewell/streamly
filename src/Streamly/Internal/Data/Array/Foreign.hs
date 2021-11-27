#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Foreign
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
-- 'Streamly.Internal.Data.Array.Stream.Foreign.toArray' to concatenate N
-- arrays at once.
--
-- Each array is one pointer visible to the GC.  Too many small arrays (e.g.
-- single byte) are only as good as holding those elements in a Haskell list.
-- However, small arrays can be compacted into large ones to reduce the
-- overhead. To hold 32GB memory in 32k sized buffers we need 1 million arrays
-- if we use one array for each chunk. This is still significant to add
-- pressure to GC.

module Streamly.Internal.Data.Array.Foreign
    (
      Array

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

    -- Monadic Folds
    , A.writeN      -- drop new
    , A.writeNAligned
    , A.write       -- full buffer
    , writeLastN

    -- * Elimination
    , A.toList
    , A.toStream
    , A.toStreamRev
    , read
    , unsafeRead    -- XXX readUnsafe?
    , A.readRev
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
    , unsafeCast   -- castUnsafe?
    , unsafeAsPtr  -- asPtrUnsafe?
    , unsafeAsCString -- asCStringUnsafe?
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
    )
where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.Ptr (plusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read, concat)

import GHC.Ptr (Ptr(..))

import Streamly.Internal.Data.Array.Foreign.Mut.Type (ReadUState(..), touch)
import Streamly.Internal.Data.Array.Foreign.Type (Array(..), length)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer(..))
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple3'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (unsafeInlineIO)

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Array.Foreign.Mut as MA
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Ring.Foreign as RB

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- /Pre-release/
{-# INLINE fromStreamN #-}
fromStreamN :: (MonadIO m, Storable a) => Int -> SerialT m a -> m (Array a)
fromStreamN n (SerialT m) = do
    when (n < 0) $ error "writeN: negative write count specified"
    A.fromStreamDN n $ D.fromStreamK m

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
fromStream :: (MonadIO m, Storable a) => SerialT m a -> m (Array a)
fromStream (SerialT m) = P.fold A.write m
-- write m = A.fromStreamD $ D.fromStreamK m

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE_NORMAL producer #-}
producer :: forall m a. (Monad m, Storable a) => Producer m (Array a) a
producer = Producer step inject extract
    where

    {-# INLINE inject #-}
    inject (Array contents start end) = return $ ReadUState contents end start

    {-# INLINE_LATE step #-}
    step (ReadUState contents end cur)
        | assert (cur <= end) (cur == end) =
            let x = unsafeInlineIO $ touch contents
            in x `seq` return D.Stop
    step (ReadUState contents end cur) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ peek cur
                cur1 = cur `plusPtr` sizeOf (undefined :: a)
            return $ D.Yield x (ReadUState contents end cur1)

    extract (ReadUState contents end cur) = return $ Array contents cur end

-- | Unfold an array into a stream.
--
-- /Since 0.7.0 (Streamly.Memory.Array)/
--
-- @since 0.8.0
{-# INLINE_NORMAL read #-}
read :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
read = Producer.simplify producer

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
{-# INLINE_NORMAL unsafeRead #-}
unsafeRead :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
unsafeRead = Unfold step inject
    where

    inject (Array contents start end) =
        return (ReadUState contents end start)

    {-# INLINE_LATE step #-}
    step (ReadUState contents end p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ do
                        r <- peek p
                        touch contents
                        return r
            let !p1 = p `plusPtr` sizeOf (undefined :: a)
            return $ D.Yield x (ReadUState contents end p1)

-- |
--
-- >>> import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
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
getIndexRev :: forall a. Storable a => Array a -> Int -> Maybe a
getIndexRev arr i =
    unsafeInlineIO
        $ MA.unsafeWithArrayContents (arrContents arr) (arrStart arr)
            $ \ptr -> do
                let elemSize = sizeOf (undefined :: a)
                    elemPtr = aEnd arr `plusPtr` negate (elemSize * (i + 1))
                if i >= 0 && elemPtr >= ptr
                then Just <$> peek elemPtr
                else return Nothing

-- |
--
-- >>> import qualified Streamly.Internal.Data.Array.Foreign as Array
-- >>> last arr = Array.getIndexRev arr 0
--
-- /Pre-release/
{-# INLINE last #-}
last :: Storable a => Array a -> Maybe a
last arr = getIndexRev arr 0

-------------------------------------------------------------------------------
-- Folds with Array as the container
-------------------------------------------------------------------------------

-- | @writeLastN n@ folds a maximum of @n@ elements from the end of the input
-- stream to an 'Array'.
--
-- @since 0.8.0
{-# INLINE writeLastN #-}
writeLastN :: (Storable a, MonadIO m) => Int -> Fold m a (Array a)
writeLastN n
    | n <= 0 = fmap (const mempty) FL.drain
    | otherwise = A.unsafeFreeze <$> Fold step initial done

    where

    step (Tuple3' rb rh i) a = do
        rh1 <- liftIO $ RB.unsafeInsert rb rh a
        return $ FL.Partial $ Tuple3' rb rh1 (i + 1)

    initial =
        let f (a, b) = FL.Partial $ Tuple3' a b (0 :: Int)
         in fmap f $ liftIO $ RB.new n

    done (Tuple3' rb rh i) = do
        arr <- liftIO $ MA.newArray n
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
findIndexOf p = Unfold.fold Fold.head . Stream.unfold (findIndicesOf p)

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
       forall a. Storable a
    => Int -- ^ starting index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
getSliceUnsafe index len (Array contents start e) =
    let size = sizeOf (undefined :: a)
        fp1 = start `plusPtr` (index * size)
        end = fp1 `plusPtr` (len * size)
     in assert (end <= e) (Array contents fp1 end)

-- | Split the array into a stream of slices using a predicate. The element
-- matching the predicate is dropped.
--
-- /Pre-release/
{-# INLINE splitOn #-}
splitOn :: (Monad m, Storable a) =>
    (a -> Bool) -> Array a -> SerialT m (Array a)
splitOn predicate arr =
    IsStream.fromStreamD
        $ fmap (\(i, len) -> getSliceUnsafe i len arr)
        $ D.sliceOnSuffix predicate (A.toStreamD arr)

{-# INLINE genSlicesFromLen #-}
genSlicesFromLen :: forall m a. (Monad m, Storable a)
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
getSlicesFromLen :: forall m a. (Monad m, Storable a)
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
-- @since 0.8.0
{-# INLINE getIndex #-}
getIndex :: forall a. Storable a => Array a -> Int -> Maybe a
getIndex arr i =
    unsafeInlineIO
        $ MA.unsafeWithArrayContents (arrContents arr) (arrStart arr)
            $ \ptr -> do
                let elemSize = sizeOf (undefined :: a)
                    elemPtr = ptr `plusPtr` (elemSize * i)
                if i >= 0 && elemPtr `plusPtr` elemSize <= aEnd arr
                then Just <$> peek elemPtr
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
-- /Unimplemented/
{-# INLINE getIndices #-}
getIndices :: Unfold m (Array a) Int -> Unfold m (Array a) a
getIndices = undefined

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
-- @since 0.7.0
{-# INLINE runPipe #-}
runPipe :: (MonadIO m, Storable a, Storable b)
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
streamTransform :: forall m a b. (MonadIO m, Storable a, Storable b)
    => (SerialT m a -> SerialT m b) -> Array a -> m (Array b)
streamTransform f arr =
    P.fold (A.writeWith (length arr)) $ getSerialT $ f (A.toStream arr)

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
unsafeCast ::
#ifdef DEVBUILD
    Storable b =>
#endif
    Array a -> Array b
unsafeCast (Array contents start end) =
    Array contents (castPtr start) (castPtr end)

-- | Cast an @Array a@ into an @Array Word8@.
--
-- @since 0.8.0
--
asBytes :: Array a -> Array Word8
asBytes = unsafeCast

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
-- @since 0.8.0
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
-- /Pre-release/
--
unsafeAsPtr :: Array a -> (Ptr b -> IO c) -> IO c
unsafeAsPtr Array{..} act = do
    MA.unsafeWithArrayContents arrContents arrStart $ \ptr -> act (castPtr ptr)

-- | Convert an array of any type into a null terminated CString Ptr.
--
-- /Unsafe/
--
-- /O(n) Time: (creates a copy of the array)/
--
-- /Pre-release/
--
unsafeAsCString :: Array a -> (CString -> IO b) -> IO b
unsafeAsCString arr act = do
    let Array{..} = asBytes arr <> A.fromList [0]
    MA.unsafeWithArrayContents arrContents arrStart $ \ptr -> act (castPtr ptr)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | Fold an array using a 'Fold'.
--
-- /Pre-release/
{-# INLINE fold #-}
fold :: forall m a b. (MonadIO m, Storable a) => Fold m a b -> Array a -> m b
fold f arr = P.fold f (getSerialT (A.toStream arr))

-- | Fold an array using a stream fold operation.
--
-- /Pre-release/
{-# INLINE streamFold #-}
streamFold :: (MonadIO m, Storable a) => (SerialT m a -> m b) -> Array a -> m b
streamFold f arr = f (A.toStream arr)
