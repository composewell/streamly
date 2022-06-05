{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Mut.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Mutable arrays and file system files are quite similar, they can grow and
-- their content is mutable. Therefore, both have similar APIs as well. We
-- strive to keep the API consistent for both. Ideally, you should be able to
-- replace one with another with little changes to the code.

module Streamly.Internal.Data.Array.Foreign.Mut.Type
    (
    -- * Type
    -- $arrayNotes
      Array (..)
    , ArrayContents
    , nilArrayContents
    , touch
    , pin
    , unpin

    -- * Constructing and Writing
    -- ** Construction
    , nil

    -- *** Uninitialized Arrays
    , newArray
    , newPinnedArrayBytes
    , newArrayAligned
    , newArrayWith
    , newAlignedArrayContents

    -- *** Initialized Arrays
    , withNewArrayUnsafe

    -- *** From streams
    , ArrayUnsafe (..)
    , writeNWithUnsafe
    , writeNWith
    , writeNUnsafe
    , writeN
    , writeNAligned

    , writeWith
    , write

    , writeRevN
    -- , writeRev

    -- ** From containers
    , fromListN
    , fromList
    , fromListRevN
    , fromListRev
    , fromStreamDN
    , fromStreamD

    -- * Random writes
    , putIndex
    , putIndexUnsafe
    , putIndices
    -- , putFromThenTo
    -- , putFrom -- start writing at the given position
    -- , putUpto -- write from beginning up to the given position
    -- , putFromTo
    -- , putFromRev
    -- , putUptoRev
    , modifyIndexUnsafe
    , modifyIndex
    , modifyIndices
    , modify
    , swapIndices
    , unsafeSwapIndices

    -- * Growing and Shrinking
    -- Arrays grow only at the end, though it is possible to grow on both sides
    -- and therefore have a cons as well as snoc. But that will require two
    -- bounds in the array representation.

    -- ** Appending elements
    , snocWith
    , snoc
    , snocLinear
    , snocMay
    , snocUnsafe

    -- ** Appending streams
    , appendNUnsafe
    , appendN
    , appendWith
    , append

    -- * Eliminating and Reading

    -- ** To streams
    , read
    , readRevWith
    , readRev

    -- ** To containers
    , toStreamDWith
    , toStreamDRevWith
    , toStreamKWith
    , toStreamKRevWith
    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toList

    -- experimental
    , producerWith
    , producer

    -- ** Random reads
    , getIndex
    , getIndexUnsafe
    , getIndices
    , getIndicesD
    -- , getFromThenTo
    , getIndexRev

    -- * Memory Management
    , blockSize
    , arrayChunkBytes
    , allocBytesToElemCount
    , realloc
    , resize
    , resizeExp
    , rightSize

    -- * Size
    , length
    , byteLength
    -- , capacity
    , byteCapacity
    , bytesFree

    -- * In-place Mutation Algorithms
    , strip
    , reverse
    , permute
    , partitionBy
    , shuffleBy
    , divideBy
    , mergeBy
    , bubble

    -- * Casting
    , cast
    , castUnsafe
    , asBytes
    , asPtrUnsafe

    -- * Folding
    , foldl'
    , foldr
    , cmp

    -- * Arrays of arrays
    --  We can add dimensionality parameter to the array type to get
    --  multidimensional arrays. Multidimensional arrays would just be a
    --  convenience wrapper on top of single dimensional arrays.

    -- | Operations dealing with multiple arrays, streams of arrays or
    -- multidimensional array representations.

    -- ** Construct from streams
    , arraysOf
    , arrayStreamKFromStreamD
    , writeChunks

    -- ** Eliminate to streams
    , flattenArrays
    , flattenArraysRev
    , fromArrayStreamK

    -- ** Construct from arrays
    -- get chunks without copying
    , getSliceUnsafe
    , getSlice
    -- , getSlicesFromLenN
    , splitAt -- XXX should be able to express using getSlice
    , breakOn

    -- ** Appending arrays
    , spliceCopy
    , spliceWith
    , splice
    , spliceExp
    , spliceUnsafe
    , putSliceUnsafe
    -- , putSlice
    -- , appendSlice
    -- , appendSliceFrom

    -- * Utilities
    , roundUpToPower2
    , memcpy
    , memcmp
    , c_memchr
    )
where

-- Notes:
-- When we use a purely lazy Monad like Identity, we need to force ordering of
-- some actions for correctness.

#include "inline.hs"
#include "ArrayMacros.h"
#include "MachDeps.h"

import Control.Exception (assert)
import Control.DeepSeq (NFData(..), NFData1(..))
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, (.|.), (.&.))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.Ptr (plusPtr, minusPtr, nullPtr)
import Streamly.Internal.Control.Exception (assertM)
import Streamly.Internal.Data.Unboxed
    ( ArrayContents(..)
    , Storable
    , alignment
    , castContents
    , getMutableByteArray#
    , peekWith
    , pokeWith
    , sizeOf
    , touch
    )
import GHC.Base
    ( IO(..), byteArrayContents#
    , Int(..), newAlignedPinnedByteArray#
    , copyMutableByteArray#, compareByteArrays#
    )
import GHC.Base (noinline)
import GHC.Exts (unsafeCoerce#)
import GHC.Ptr (Ptr(..))

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (arrayPayloadSize, defaultChunkSize)
import System.IO.Unsafe (unsafePerformIO)

#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Unboxed as Unboxed
import qualified Prelude

import Prelude hiding
    (length, foldr, read, unlines, splitAt, reverse, truncate)

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as Array
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.StreamD as StreamD
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Type as Fold

-------------------------------------------------------------------------------
-- Foreign helpers
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- | Given a 'Storable' type (unused first arg) and a number of bytes, return
-- how many elements of that type will completely fit in those bytes.
--
{-# INLINE bytesToElemCount #-}
bytesToElemCount :: forall a. Storable a => a -> Int -> Int
bytesToElemCount _ n = n `div` SIZE_OF(a)

-- XXX we are converting Int to CSize
memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy dst src len = void (c_memcpy dst src (fromIntegral len))

-- XXX we are converting Int to CSize
-- return True if the memory locations have identical contents
{-# INLINE memcmp #-}
memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
memcmp p1 p2 len = do
    r <- c_memcmp p1 p2 (fromIntegral len)
    return $ r == 0

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- $arrayNotes
--
-- We can use a 'Storable' constraint in the Array type and the constraint can
-- be automatically provided to a function that pattern matches on the Array
-- type. However, it has huge performance cost, so we do not use it.
-- Investigate a GHC improvement possiblity.

-- XXX Rename the fields to better names.

-- | An unboxed, pinned mutable array. An array is created with a given length
-- and capacity. Length is the number of valid elements in the array.  Capacity
-- is the maximum number of elements that the array can be expanded to without
-- having to reallocate the memory.
--
-- The elements in the array can be mutated in-place without changing the
-- reference (constructor). However, the length of the array cannot be mutated
-- in-place.  A new array reference is generated when the length changes.  When
-- the length is increased (upto the maximum reserved capacity of the array),
-- the array is not reallocated and the new reference uses the same underlying
-- memory as the old one.
--
-- Several routines in this module allow the programmer to control the capacity
-- of the array. The programmer can control the trade-off between memory usage
-- and performance impact due to reallocations when growing or shrinking the
-- array.
--
data Array a =
#ifdef DEVBUILD
    Storable a =>
#endif
    -- The array is a range into arrContents. arrContents may be a superset of
    -- the slice represented by the array. All offsets are in bytes.
    Array
    { arrContents :: {-# UNPACK #-} !(ArrayContents a)
    , arrStart :: {-# UNPACK #-} !Int  -- ^ index into arrContents
    , aEnd   :: {-# UNPACK #-} !Int    -- ^ index into arrContents
                                       -- Represents the first invalid index of
                                       -- the array.
    , aBound :: {-# UNPACK #-} !Int    -- ^ first invalid index of arrContents.
    }

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

{-# INLINE pin #-}
pin :: Array a -> IO (Array a)
pin arr@(Array{..}) = do
    contents <- Unboxed.pin arrContents
    return $ arr {arrContents = contents}

{-# INLINE unpin #-}
unpin :: Array a -> IO (Array a)
unpin arr@(Array{..}) = do
    contents <- Unboxed.unpin arrContents
    return $ arr {arrContents = contents}

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- XXX Change the names to use "new" instead of "newArray". That way we can use
-- the same names for managed file system objects as well. For unmanaged ones
-- we can use open/create etc as usual.
--
-- A new array is similar to "touch" creating a zero length file. An mmapped
-- array would be similar to a sparse file with holes. TBD: support mmapped
-- files and arrays.

-- GHC always guarantees word-aligned memory, alignment is important only when
-- we need more than that.  See stg_newAlignedPinnedByteArrayzh and
-- allocatePinned in GHC source.

-- | @newArrayWith allocator alignment count@ allocates a new array of zero
-- length and with a capacity to hold @count@ elements, using @allocator
-- size alignment@ as the memory allocator function.
--
-- Alignment must be greater than or equal to machine word size and a power of
-- 2.
--
-- /Pre-release/
{-# INLINE newArrayWith #-}
newArrayWith :: forall m a. (MonadIO m, Storable a)
    => (Int -> Int -> m (ArrayContents a)) -> Int -> Int -> m (Array a)
newArrayWith alloc alignSize count = do
    let size = max (count * SIZE_OF(a)) 0
    contents <- alloc size alignSize
    return $ Array
        { arrContents = contents
        , arrStart = 0
        , aEnd   = 0
        , aBound = size
        }

-- XXX Move this to Unboxed and rename this to newPinnedAlignedBytes?
-- XXX Similarly have newUnpinnedBytes
{-# INLINE newAlignedArrayContents #-}
newAlignedArrayContents :: Int -> Int -> IO (ArrayContents a)
newAlignedArrayContents nbytes _align | nbytes < 0 =
  errorWithoutStackTrace "newAlignedArrayContents: size must be >= 0"
newAlignedArrayContents (I# nbytes) (I# align) = IO $ \s ->
    case newAlignedPinnedByteArray# nbytes align s of
        (# s', mbarr# #) ->
           let c = ArrayContents mbarr#
            in (# s', c #)

{-# NOINLINE nilArrayContents #-}
nilArrayContents :: ArrayContents a
nilArrayContents =
    unsafePerformIO $ newAlignedArrayContents 0 0

nil ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Array a
nil = Array nilArrayContents 0 0 0

-- | Like 'newArrayWith' but using an allocator that aligns the memory to the
-- alignment dictated by the 'Storable' instance of the type.
--
-- /Internal/
{-# INLINE newArrayAligned #-}
newArrayAligned :: (MonadIO m, Storable a) => Int -> Int -> m (Array a)
newArrayAligned = newArrayWith (\s a -> liftIO $ newAlignedArrayContents s a)

-- XXX Intead of newArray, make newPinnedArray and newUnpinnedArray. Make it
-- more explicit.
-- XXX can unaligned allocation be more efficient when alignment is not needed?
--
-- | Allocates an empty array that can hold 'count' items.  The memory of the
-- array is uninitialized and the allocation is aligned as per the 'Storable'
-- instance of the type.
--
-- /Pre-release/
{-# INLINE newArray #-}
newArray :: forall m a. (MonadIO m, Storable a) => Int -> m (Array a)
newArray = newArrayAligned (alignment (undefined :: a))

-- | Allocates a pinned empty array that can hold 'count' items.  The memory of
-- the array is uninitialized and the allocation is aligned as per the
-- 'Storable' instance of the type.
--
-- /Pre-release/
{-# INLINE newPinnedArrayBytes #-}
newPinnedArrayBytes :: forall m a. (MonadIO m, Storable a) => Int -> m (Array a)
newPinnedArrayBytes bytes = do
    contents <-
        liftIO $ newAlignedArrayContents bytes (alignment (undefined :: a))
    return $ Array
        { arrContents = contents
        , arrStart = 0
        , aEnd   = 0
        , aBound = bytes
        }

-- | Allocate an Array of the given size and run an IO action passing the array
-- start pointer.
--
-- /Internal/
{-# INLINE withNewArrayUnsafe #-}
withNewArrayUnsafe ::
       (MonadIO m, Storable a) => Int -> (Ptr a -> m ()) -> m (Array a)
withNewArrayUnsafe count f = do
    arr <- newArray count
    asPtrUnsafe arr
        $ \p -> f p >> return arr

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

-- | Write the given element to the given index of the array. Does not check if
-- the index is out of bounds of the array.
--
-- /Pre-release/
{-# INLINE putIndexUnsafe #-}
putIndexUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> a -> Array a -> m ()
putIndexUnsafe i x (Array {..}) = do
    let index = INDEX_OF(arrStart, i, a)
    assert (i >= 0 && INDEX_VALID(index, aEnd, a)) (return ())
    liftIO $ pokeWith arrContents index x

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex arr ix val = Array.modifyIndex ix (const (val, ())) arr
-- >>> f = Array.putIndices
-- >>> putIndex ix val arr = Stream.fold (f arr) (Stream.fromPure (ix, val))
--
-- /Pre-release/
{-# INLINE putIndex #-}
putIndex :: forall m a. (MonadIO m, Storable a) => Int -> a -> Array a -> m ()
putIndex i x Array{..} = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,aEnd,a)
    then liftIO $ pokeWith arrContents index x
    else invalidIndex "putIndex" i

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: forall m a. (MonadIO m, Storable a)
    => Array a -> Fold m (Int, a) ()
putIndices arr = FL.foldlM' step (return ())

    where

    step () (i, x) = liftIO (putIndex i x arr)

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndexUnsafe :: forall m a b. (MonadIO m, Storable a) =>
    Int -> (a -> (a, b)) -> Array a -> m b
modifyIndexUnsafe i f (Array{..}) = liftIO $ do
        let index = INDEX_OF(arrStart,i,a)
        assert (i >= 0 && INDEX_NEXT(index,a) <= aEnd) (return ())
        r <- peekWith arrContents index
        let (x, res) = f r
        pokeWith arrContents index x
        return res

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndex :: forall m a b. (MonadIO m, Storable a) =>
    Int -> (a -> (a, b)) -> Array a -> m b
modifyIndex i f Array{..} = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,aEnd,a)
    then liftIO $ do
        r <- peekWith arrContents index
        let (x, res) = f r
        pokeWith arrContents index x
        return res
    else invalidIndex "modifyIndex" i


-- | Modify the array indices generated by the supplied stream.
--
-- /Pre-release/
{-# INLINE modifyIndices #-}
modifyIndices :: forall m a . (MonadIO m, Storable a)
    => (Int -> a -> a) -> Array a -> Fold m Int ()
modifyIndices f arr = FL.foldlM' step initial

    where

    initial = return ()

    step () i =
        let f1 x = (f i x, ())
         in modifyIndex i f1 arr

-- | Modify each element of an array using the supplied modifier function.
--
-- /Pre-release/
modify :: forall m a. (MonadIO m, Storable a)
    => (a -> a) -> Array a -> m ()
modify f Array{..} = liftIO $
    go arrStart

    where

    go i =
        when (INDEX_VALID(i,aEnd,a)) $ do
            r <- peekWith arrContents i
            pokeWith arrContents i (f r)
            go (INDEX_NEXT(i,a))

{-# INLINE swapArrayByteIndices #-}
swapArrayByteIndices :: Storable a => ArrayContents a -> Int -> Int -> IO ()
swapArrayByteIndices arrContents i1 i2 = do
    r1 <- peekWith arrContents i1
    r2 <- peekWith arrContents i2
    pokeWith arrContents i1 r2
    pokeWith arrContents i2 r1

-- | Swap the elements at two indices without validating the indices.
--
-- /Unsafe/: This could result in memory corruption if indices are not valid.
--
-- /Pre-release/
{-# INLINE unsafeSwapIndices #-}
unsafeSwapIndices :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Array a -> m ()
unsafeSwapIndices i1 i2 Array{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        swapArrayByteIndices arrContents t1 t2

-- | Swap the elements at two indices.
--
-- /Pre-release/
swapIndices :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Array a -> m ()
swapIndices i1 i2 Array{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        when (i1 < 0 || INDEX_INVALID(t1,aEnd,a))
            $ invalidIndex "swapIndices" i1
        when (i2 < 0 || INDEX_INVALID(t2,aEnd,a))
            $ invalidIndex "swapIndices" i2
        swapArrayByteIndices arrContents t1 t2

-------------------------------------------------------------------------------
-- Rounding
-------------------------------------------------------------------------------

-- XXX Should we use bitshifts in calculations or it gets optimized by the
-- compiler/processor itself?
--
-- | The page or block size used by the GHC allocator. Allocator allocates at
-- least a block and then allocates smaller allocations from within a block.
blockSize :: Int
blockSize = 4 * 1024

-- | Allocations larger than 'largeObjectThreshold' are in multiples of block
-- size and are always pinned. The space beyond the end of a large object up to
-- the end of the block is unused.
largeObjectThreshold :: Int
largeObjectThreshold = (blockSize * 8) `div` 10

-- XXX Should be done only when we are using the GHC allocator.
-- | Round up an array larger than 'largeObjectThreshold' to use the whole
-- block.
{-# INLINE roundUpLargeArray #-}
roundUpLargeArray :: Int -> Int
roundUpLargeArray size =
    if size >= largeObjectThreshold
    then
        assert
            (blockSize /= 0 && ((blockSize .&. (blockSize - 1)) == 0))
            ((size + blockSize - 1) .&. negate blockSize)
    else size

{-# INLINE isPower2 #-}
isPower2 :: Int -> Bool
isPower2 n = n .&. (n - 1) == 0

{-# INLINE roundUpToPower2 #-}
roundUpToPower2 :: Int -> Int
roundUpToPower2 n =
#if WORD_SIZE_IN_BITS == 64
    1 + z6
#else
    1 + z5
#endif

    where

    z0 = n - 1
    z1 = z0 .|. z0 `shiftR` 1
    z2 = z1 .|. z1 `shiftR` 2
    z3 = z2 .|. z2 `shiftR` 4
    z4 = z3 .|. z3 `shiftR` 8
    z5 = z4 .|. z4 `shiftR` 16
    z6 = z5 .|. z5 `shiftR` 32

-- | @allocBytesToBytes elem allocatedBytes@ returns the array size in bytes
-- such that the real allocation is less than or equal to @allocatedBytes@,
-- unless @allocatedBytes@ is less than the size of one array element in which
-- case it returns one element's size.
--
{-# INLINE allocBytesToBytes #-}
allocBytesToBytes :: forall a. Storable a => a -> Int -> Int
allocBytesToBytes _ n = max (arrayPayloadSize n) (SIZE_OF(a))

-- | Given a 'Storable' type (unused first arg) and real allocation size
-- (including overhead), return how many elements of that type will completely
-- fit in it, returns at least 1.
--
{-# INLINE allocBytesToElemCount #-}
allocBytesToElemCount :: Storable a => a -> Int -> Int
allocBytesToElemCount x bytes =
    let n = bytesToElemCount x (allocBytesToBytes x bytes)
     in assert (n >= 1) n

-- | The default chunk size by which the array creation routines increase the
-- size of the array when the array is grown linearly.
arrayChunkBytes :: Int
arrayChunkBytes = 1024

-------------------------------------------------------------------------------
-- Resizing
-------------------------------------------------------------------------------

-- | Round the second argument down to multiples of the first argument.
{-# INLINE roundDownTo #-}
roundDownTo :: Int -> Int -> Int
roundDownTo elemSize size = size - (size `mod` elemSize)

-- XXX See if resizing can be implemented by reading the old array as a stream
-- and then using writeN to the new array.
--
-- NOTE: we are passing elemSize explicitly to avoid a Storable constraint.
-- Since this is not inlined Storable consrraint leads to dictionary passing
-- which complicates some inspection tests.
--
{-# NOINLINE reallocAligned #-}
reallocAligned :: Int -> Int -> Int -> Array a -> IO (Array a)
reallocAligned elemSize alignSize newCapacityInBytes Array{..} = do
    assertM (aEnd <= aBound)

    -- Allocate new array
    let newCapMaxInBytes = roundUpLargeArray newCapacityInBytes
    contents <- newAlignedArrayContents newCapMaxInBytes alignSize
    let !(ArrayContents mbarrFrom#) = arrContents
        !(ArrayContents mbarrTo#) = contents

    -- Copy old data
    let oldStart = arrStart
        !(I# oldStartInBytes#) = oldStart
        oldSizeInBytes = aEnd - oldStart
        newCapInBytes = roundDownTo elemSize newCapMaxInBytes
        !newLenInBytes@(I# newLenInBytes#) = min oldSizeInBytes newCapInBytes
    assert (oldSizeInBytes `mod` elemSize == 0) (return ())
    assert (newLenInBytes >= 0) (return ())
    assert (newLenInBytes `mod` elemSize == 0) (return ())
    IO $ \s# -> (# copyMutableByteArray# mbarrFrom# oldStartInBytes#
                        mbarrTo# 0# newLenInBytes# s#, () #)

    return $ Array
        { arrStart = 0
        , arrContents = contents
        , aEnd   = newLenInBytes
        , aBound = newCapInBytes
        }

-- | @realloc newCapacity array@ reallocates the array to the specified
-- capacity in bytes.
--
-- If the new size is less than the original array the array gets truncated.
-- If the new size is not a multiple of array element size then it is rounded
-- down to multiples of array size.  If the new size is more than
-- 'largeObjectThreshold' then it is rounded up to the block size (4K).
--
{-# INLINABLE realloc #-}
realloc :: forall m a. (MonadIO m, Storable a) => Int -> Array a -> m (Array a)
realloc bytes arr =
    liftIO $ reallocAligned (SIZE_OF(a)) (alignment (undefined :: a)) bytes arr

-- | @reallocWith label capSizer minIncrBytes array@. The label is used
-- in error messages and the capSizer is used to determine the capacity of the
-- new array in bytes given the current byte length of the array.
reallocWith :: forall m a. (MonadIO m , Storable a) =>
       String
    -> (Int -> Int)
    -> Int
    -> Array a
    -> m (Array a)
reallocWith label capSizer minIncrBytes arr = do
    let oldSizeBytes = aEnd arr - arrStart arr
        newCapBytes = capSizer oldSizeBytes
        newSizeBytes = oldSizeBytes + minIncrBytes
        safeCapBytes = max newCapBytes newSizeBytes
    assertM (safeCapBytes >= newSizeBytes || error (badSize newSizeBytes))

    realloc safeCapBytes arr

    where

    badSize newSize =
        concat
            [ label
            , ": new array size (in bytes) is less than required size "
            , show newSize
            , ". Please check the sizing function passed."
            ]

-- | @resize newCapacity array@ changes the total capacity of the array so that
-- it is enough to hold the specified number of elements.  Nothing is done if
-- the specified capacity is less than the length of the array.
--
-- If the capacity is more than 'largeObjectThreshold' then it is rounded up to
-- the block size (4K).
--
-- /Pre-release/
{-# INLINE resize #-}
resize :: forall m a. (MonadIO m, Storable a) =>
    Int -> Array a -> m (Array a)
resize nElems arr@Array{..} = do
    let req = SIZE_OF(a) * nElems
        len = aEnd - arrStart
    if req < len
    then return arr
    else realloc req arr

-- | Like 'resize' but if the byte capacity is more than 'largeObjectThreshold'
-- then it is rounded up to the closest power of 2.
--
-- /Pre-release/
{-# INLINE resizeExp #-}
resizeExp :: forall m a. (MonadIO m, Storable a) =>
    Int -> Array a -> m (Array a)
resizeExp nElems arr@Array{..} = do
    let req = roundUpLargeArray (SIZE_OF(a) * nElems)
        req1 =
            if req > largeObjectThreshold
            then roundUpToPower2 req
            else req
        len = aEnd - arrStart
    if req1 < len
    then return arr
    else realloc req1 arr

-- | Resize the allocated memory to drop any reserved free space at the end of
-- the array and reallocate it to reduce wastage.
--
-- Up to 25% wastage is allowed to avoid reallocations.  If the capacity is
-- more than 'largeObjectThreshold' then free space up to the 'blockSize' is
-- retained.
--
-- /Pre-release/
{-# INLINE rightSize #-}
rightSize :: forall m a. (MonadIO m, Storable a) => Array a -> m (Array a)
rightSize arr@Array{..} = do
    assert (aEnd <= aBound) (return ())
    let start = arrStart
        len = aEnd - start
        capacity = aBound - start
        target = roundUpLargeArray len
        waste = aBound - aEnd
    assert (target >= len) (return ())
    assert (len `mod` SIZE_OF(a) == 0) (return ())
    -- We trade off some wastage (25%) to avoid reallocations and copying.
    if target < capacity && len < 3 * waste
    then realloc target arr
    else return arr

-------------------------------------------------------------------------------
-- Snoc
-------------------------------------------------------------------------------

-- XXX We can possibly use a smallMutableByteArray to hold the start, end,
-- bound pointers.  Using fully mutable handle will ensure that we do not have
-- multiple references to the same array of different lengths lying around and
-- potentially misused. In that case "snoc" need not return a new array (snoc
-- :: Array a -> a -> m ()), it will just modify the old reference.  The array
-- length will be mutable.  This means the length function would also be
-- monadic.  Mutable arrays would behave more like files that grow in that
-- case.

-- | Snoc using a 'Ptr'. Low level reusable function.
--
-- /Internal/
{-# INLINE snocNewEnd #-}
snocNewEnd :: (MonadIO m, Storable a) => Int -> Array a -> a -> m (Array a)
snocNewEnd newEnd arr@Array{..} x = liftIO $ do
    assert (newEnd <= aBound) (return ())
    pokeWith arrContents aEnd x
    return $ arr {aEnd = newEnd}

-- | Really really unsafe, appends the element into the first array, may
-- cause silent data corruption or if you are lucky a segfault if the first
-- array does not have enough space to append the element.
--
-- /Internal/
{-# INLINE snocUnsafe #-}
snocUnsafe :: forall m a. (MonadIO m, Storable a) =>
    Array a -> a -> m (Array a)
snocUnsafe arr@Array{..} = snocNewEnd (INDEX_NEXT(aEnd,a)) arr

-- | Like 'snoc' but does not reallocate when pre-allocated array capacity
-- becomes full.
--
-- /Internal/
{-# INLINE snocMay #-}
snocMay :: forall m a. (MonadIO m, Storable a) =>
    Array a -> a -> m (Maybe (Array a))
snocMay arr@Array{..} x = liftIO $ do
    let newEnd = INDEX_NEXT(aEnd,a)
    if newEnd <= aBound
    then Just <$> snocNewEnd newEnd arr x
    else return Nothing

-- NOINLINE to move it out of the way and not pollute the instruction cache.
{-# NOINLINE snocWithRealloc #-}
snocWithRealloc :: forall m a. (MonadIO m, Storable a) =>
       (Int -> Int)
    -> Array a
    -> a
    -> m (Array a)
snocWithRealloc sizer arr x = do
    arr1 <- liftIO $ reallocWith "snocWith" sizer (SIZE_OF(a)) arr
    snocUnsafe arr1 x

-- | @snocWith sizer arr elem@ mutates @arr@ to append @elem@. The length of
-- the array increases by 1.
--
-- If there is no reserved space available in @arr@ it is reallocated to a size
-- in bytes determined by the @sizer oldSizeBytes@ function, where
-- @oldSizeBytes@ is the original size of the array in bytes.
--
-- If the new array size is more than 'largeObjectThreshold' we automatically
-- round it up to 'blockSize'.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- /Pre-release/
{-# INLINE snocWith #-}
snocWith :: forall m a. (MonadIO m, Storable a) =>
       (Int -> Int)
    -> Array a
    -> a
    -> m (Array a)
snocWith allocSize arr x = liftIO $ do
    let newEnd = INDEX_NEXT(aEnd arr,a)
    if newEnd <= aBound arr
    then snocNewEnd newEnd arr x
    else snocWithRealloc allocSize arr x

-- | The array is mutated to append an additional element to it. If there
-- is no reserved space available in the array then it is reallocated to grow
-- it by 'arrayChunkBytes' rounded up to 'blockSize' when the size becomes more
-- than 'largeObjectThreshold'.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- Performs O(n^2) copies to grow but is thrifty on memory.
--
-- /Pre-release/
{-# INLINE snocLinear #-}
snocLinear :: forall m a. (MonadIO m, Storable a) => Array a -> a -> m (Array a)
snocLinear = snocWith (+ allocBytesToBytes (undefined :: a) arrayChunkBytes)

-- | The array is mutated to append an additional element to it. If there is no
-- reserved space available in the array then it is reallocated to double the
-- original size.
--
-- This is useful to reduce allocations when appending unknown number of
-- elements.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- >>> snoc = Array.snocWith (* 2)
--
-- Performs O(n * log n) copies to grow, but is liberal with memory allocation.
--
-- /Pre-release/
{-# INLINE snoc #-}
snoc :: forall m a. (MonadIO m, Storable a) => Array a -> a -> m (Array a)
snoc = snocWith f

    where

    f oldSize =
        if isPower2 oldSize
        then oldSize * 2
        else roundUpToPower2 oldSize * 2

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- XXX Can this be deduplicated with array/foreign

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe :: forall m a. (MonadIO m, Storable a) => Int -> Array a -> m a
getIndexUnsafe i (Array {..}) = do
    let index = INDEX_OF(arrStart,i,a)
    assert (i >= 0 && INDEX_VALID(index,aEnd,a)) (return ())
    liftIO $ peekWith arrContents index

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: forall m a. (MonadIO m, Storable a) => Int -> Array a -> m a
getIndex i Array{..} = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,aEnd,a)
    then liftIO $ peekWith arrContents index
    else invalidIndex "getIndex" i

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: forall m a. (MonadIO m, Storable a) => Int -> Array a -> m a
getIndexRev i Array{..} = do
    let index = RINDEX_OF(aEnd,i,a)
    if i >= 0 && index >= arrStart
    then liftIO $ peekWith arrContents index
    else invalidIndex "getIndexRev" i

data GetIndicesState contents start end st =
    GetIndicesState contents start end st

-- | Given an unfold that generates array indices, read the elements on those
-- indices from the supplied Array. An error is thrown if an index is out of
-- bounds.
--
-- /Pre-release/
{-# INLINE getIndicesD #-}
getIndicesD :: (Monad m, Storable a) =>
    (forall b. IO b -> m b) -> D.Stream m Int -> Unfold m (Array a) a
getIndicesD liftio (D.Stream stepi sti) = Unfold step inject

    where

    inject (Array contents start end _) =
        return $ GetIndicesState contents start end sti

    {-# INLINE_LATE step #-}
    step (GetIndicesState contents start end st) = do
        r <- stepi defState st
        case r of
            D.Yield i s -> do
                x <- liftio $ getIndex i (Array contents start end undefined)
                return $ D.Yield x (GetIndicesState contents start end s)
            D.Skip s -> return $ D.Skip (GetIndicesState contents start end s)
            D.Stop -> return D.Stop

{-# INLINE getIndices #-}
getIndices :: (MonadIO m, Storable a) => Stream m Int -> Unfold m (Array a) a
getIndices = getIndicesD liftIO . Stream.toStreamD

-------------------------------------------------------------------------------
-- Subarrays
-------------------------------------------------------------------------------

-- XXX We can also get immutable slices.

-- | /O(1)/ Slice an array in constant time.
--
-- Unsafe: The bounds of the slice are not checked.
--
-- /Unsafe/
--
-- /Pre-release/
{-# INLINE getSliceUnsafe #-}
getSliceUnsafe :: forall a. Storable a
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
getSliceUnsafe index len (Array contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in assert
            (index >= 0 && len >= 0 && end <= e)
            -- Note: In a slice we always use bound = end so that the slice
            -- user cannot overwrite elements beyond the end of the slice.
            (Array contents fp1 end end)

-- | /O(1)/ Slice an array in constant time. Throws an error if the slice
-- extends out of the array bounds.
--
-- /Pre-release/
{-# INLINE getSlice #-}
getSlice :: forall a. Storable a =>
       Int -- ^ from index
    -> Int -- ^ length of the slice
    -> Array a
    -> Array a
getSlice index len (Array contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in if index >= 0 && len >= 0 && end <= e
        -- Note: In a slice we always use bound = end so that the slice user
        -- cannot overwrite elements beyond the end of the slice.
        then Array contents fp1 end end
        else error
                $ "getSlice: invalid slice, index "
                ++ show index ++ " length " ++ show len

-------------------------------------------------------------------------------
-- In-place mutation algorithms
-------------------------------------------------------------------------------

-- XXX consider the bulk update/accumulation/permutation APIs from vector.

-- | You may not need to reverse an array because you can consume it in reverse
-- using 'readRev'. To reverse large arrays you can read in reverse and write
-- to another array. However, in-place reverse can be useful to take adavantage
-- of cache locality and when you do not want to allocate additional memory.
--
-- /Pre-release/
{-# INLINE reverse #-}
reverse :: forall m a. (MonadIO m, Storable a) => Array a -> m ()
reverse Array{..} = liftIO $ do
    let l = arrStart
        h = INDEX_PREV(aEnd,a)
     in swap l h

    where

    swap l h = do
        when (l < h) $ do
            swapArrayByteIndices arrContents l h
            swap (INDEX_NEXT(l,a)) (INDEX_PREV(h,a))

-- | Generate the next permutation of the sequence, returns False if this is
-- the last permutation.
--
-- /Unimplemented/
{-# INLINE permute #-}
permute :: Array a -> m Bool
permute = undefined

-- | Partition an array into two halves using a partitioning predicate. The
-- first half retains values where the predicate is 'False' and the second half
-- retains values where the predicate is 'True'.
--
-- /Pre-release/
{-# INLINE partitionBy #-}
partitionBy :: forall m a. (MonadIO m, Storable a)
    => (a -> Bool) -> Array a -> m (Array a, Array a)
partitionBy f arr@Array{..} = liftIO $ do
    if arrStart >= aEnd
    then return (arr, arr)
    else do
        ptr <- go arrStart (INDEX_PREV(aEnd,a))
        let pl = Array arrContents arrStart ptr ptr
            pr = Array arrContents ptr aEnd aEnd
        return (pl, pr)

    where

    -- Invariant low < high on entry, and on return as well
    moveHigh low high = do
        h <- peekWith arrContents high
        if f h
        then
            -- Correctly classified, continue the loop
            let high1 = INDEX_PREV(high,a)
             in if low == high1
                then return Nothing
                else moveHigh low high1
        else return (Just (high, h)) -- incorrectly classified

    -- Keep a low pointer starting at the start of the array (first partition)
    -- and a high pointer starting at the end of the array (second partition).
    -- Keep incrementing the low ptr and decrementing the high ptr until both
    -- are wrongly classified, at that point swap the two and continue until
    -- the two pointer cross each other.
    --
    -- Invariants when entering this loop:
    -- low <= high
    -- Both low and high are valid locations within the array
    go low high = do
        l <- peekWith arrContents low
        if f l
        then
            -- low is wrongly classified
            if low == high
            then return low
            else do -- low < high
                r <- moveHigh low high
                case r of
                    Nothing -> return low
                    Just (high1, h) -> do -- low < high1
                        pokeWith arrContents low h
                        pokeWith arrContents high1 l
                        let low1 = INDEX_NEXT(low,a)
                            high2 = INDEX_PREV(high1,a)
                        if low1 <= high2
                        then go low1 high2
                        else return low1 -- low1 > high2

        else do
            -- low is correctly classified
            let low1 = INDEX_NEXT(low,a)
            if low == high
            then return low1
            else go low1 high

-- | Shuffle corresponding elements from two arrays using a shuffle function.
-- If the shuffle function returns 'False' then do nothing otherwise swap the
-- elements. This can be used in a bottom up fold to shuffle or reorder the
-- elements.
--
-- /Unimplemented/
{-# INLINE shuffleBy #-}
shuffleBy :: (a -> a -> m Bool) -> Array a -> Array a -> m ()
shuffleBy = undefined

-- XXX we can also make the folds partial by stopping at a certain level.
--
-- | @divideBy level partition array@  performs a top down hierarchical
-- recursive partitioning fold of items in the container using the given
-- function as the partition function.  Level indicates the level in the tree
-- where the fold would stop.
--
-- This performs a quick sort if the partition function is
-- 'partitionBy (< pivot)'.
--
-- /Unimplemented/
{-# INLINABLE divideBy #-}
divideBy ::
    Int -> (Array a -> m (Array a, Array a)) -> Array a -> m ()
divideBy = undefined

-- | @mergeBy level merge array@ performs a pairwise bottom up fold recursively
-- merging the pairs using the supplied merge function. Level indicates the
-- level in the tree where the fold would stop.
--
-- This performs a random shuffle if the merge function is random.  If we
-- stop at level 0 and repeatedly apply the function then we can do a bubble
-- sort.
--
-- /Unimplemented/
mergeBy :: Int -> (Array a -> Array a -> m ()) -> Array a -> m ()
mergeBy = undefined

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

-- | /O(1)/ Get the byte length of the array.
--
-- @since 0.7.0
{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength Array{..} =
    let len = aEnd - arrStart
    in assert (len >= 0) len

-- Note: try to avoid the use of length in performance sensitive internal
-- routines as it involves a costly 'div' operation. Instead use the end ptr
-- in the array to check the bounds etc.
--
-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- Note that 'byteLength' is less expensive than this operation, as 'length'
-- involves a costly division operation.
--
-- @since 0.7.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr =
    let elemSize = SIZE_OF(a)
        blen = byteLength arr
     in assert (blen `mod` elemSize == 0) (blen `div` elemSize)

-- | Get the total capacity of an array. An array may have space reserved
-- beyond the current used length of the array.
--
-- /Pre-release/
{-# INLINE byteCapacity #-}
byteCapacity :: Array a -> Int
byteCapacity Array{..} =
    let len = aBound - arrStart
    in assert (len >= 0) len

-- | The remaining capacity in the array for appending more elements without
-- reallocation.
--
-- /Pre-release/
{-# INLINE bytesFree #-}
bytesFree :: Array a -> Int
bytesFree Array{..} =
    let n = aBound - aEnd
    in assert (n >= 0) n

-------------------------------------------------------------------------------
-- Streams of arrays - Creation
-------------------------------------------------------------------------------

data GroupState s contents start end bound
    = GroupStart s
    | GroupBuffer s contents start end bound
    | GroupYield
        contents start end bound (GroupState s contents start end bound)
    | GroupFinish

-- | @arraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
--
-- @arraysOf n = StreamD.foldMany (Array.writeN n)@
--
-- /Pre-release/
{-# INLINE_NORMAL arraysOf #-}
arraysOf :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
-- XXX the idiomatic implementation leads to large regression in the D.reverse'
-- benchmark. It seems it has difficulty producing optimized code when
-- converting to StreamK. Investigate GHC optimizations.
-- arraysOf n = D.foldMany (writeN n)
arraysOf n (D.Stream step state) =
    D.Stream step' (GroupStart state)

    where

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Type.arraysOf: "
                    ++ "the size of arrays [" ++ show n
                    ++ "] must be a natural number"
        Array contents start end bound <- liftIO $ newArray n
        return $ D.Skip (GroupBuffer st contents start end bound)

    step' gst (GroupBuffer st contents start end bound) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                liftIO $ pokeWith contents end x
                let end1 = INDEX_NEXT(end,a)
                return $
                    if end1 >= bound
                    then D.Skip
                            (GroupYield
                                contents start end1 bound (GroupStart s))
                    else D.Skip (GroupBuffer s contents start end1 bound)
            D.Skip s ->
                return $ D.Skip (GroupBuffer s contents start end bound)
            D.Stop ->
                return
                    $ D.Skip (GroupYield contents start end bound GroupFinish)

    step' _ (GroupYield contents start end bound next) =
        return $ D.Yield (Array contents start end bound) next

    step' _ GroupFinish = return D.Stop

-- XXX buffer to a list instead?
-- | Buffer the stream into arrays in memory.
{-# INLINE arrayStreamKFromStreamD #-}
arrayStreamKFromStreamD :: forall m a. (MonadIO m, Storable a) =>
    D.Stream m a -> m (K.Stream m (Array a))
arrayStreamKFromStreamD =
    let n = allocBytesToElemCount (undefined :: a) defaultChunkSize
     in D.foldr K.cons K.nil . arraysOf n

-------------------------------------------------------------------------------
-- Streams of arrays - Flattening
-------------------------------------------------------------------------------

data FlattenState s contents a =
      OuterLoop s
    | InnerLoop s contents !Int !Int

-- | Use the "read" unfold instead.
--
-- @flattenArrays = unfoldMany read@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArrays (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield Array{..} s ->
                D.Skip (InnerLoop s arrContents arrStart aEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p end) | assert (p <= end) (p == end) =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p end) = do
        x <- liftIO $ peekWith contents p
        return $ D.Yield x (InnerLoop st contents (INDEX_NEXT(p,a)) end)

-- | Use the "readRev" unfold instead.
--
-- @flattenArrays = unfoldMany readRev@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Storable a)
    => D.Stream m (Array a) -> D.Stream m a
flattenArraysRev (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield Array{..} s ->
                let p = INDEX_PREV(aEnd,a)
                 in D.Skip (InnerLoop s arrContents p arrStart)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p start) | p < start =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p start) = do
        x <- liftIO $ do
                    r <- peekWith contents p
                    return r
        let cur = INDEX_PREV(p,a)
        return $ D.Yield x (InnerLoop st contents cur start)

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !(ArrayContents a)  -- contents
    {-# UNPACK #-} !Int                -- index 1
    {-# UNPACK #-} !Int                -- index 2

toArrayUnsafe :: Array a -> ArrayUnsafe a
toArrayUnsafe (Array contents start end _) = ArrayUnsafe contents start end

fromArrayUnsafe ::
#ifdef DEVBUILD
    Storable a =>
#endif
    ArrayUnsafe a -> Array a
fromArrayUnsafe (ArrayUnsafe contents start end) =
         Array contents start end end

{-# INLINE_NORMAL producerWith #-}
producerWith ::
       forall m a. (Monad m, Storable a)
    => (forall b. IO b -> m b) -> Producer m (Array a) a
producerWith liftio = Producer step (return . toArrayUnsafe) extract
    where

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe _ cur end)
        | assert (cur <= end) (cur == end) = return D.Stop
    step (ArrayUnsafe contents cur end) = do
            -- When we use a purely lazy Monad like Identity, we need to force a
            -- few actions for correctness and execution order sanity. We want
            -- the peek to occur right here and not lazily at some later point
            -- because we want the peek to be ordered with respect to the touch.
            !x <- liftio $ peekWith contents cur
            return $ D.Yield x (ArrayUnsafe contents (INDEX_NEXT(cur,a)) end)

    extract = return . fromArrayUnsafe

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: forall m a. (MonadIO m, Storable a) => Producer m (Array a) a
producer = producerWith liftIO

-- | Unfold an array into a stream.
--
-- @since 0.7.0
{-# INLINE_NORMAL read #-}
read :: forall m a. (MonadIO m, Storable a) => Unfold m (Array a) a
read = Producer.simplify producer

{-# INLINE_NORMAL readRevWith #-}
readRevWith ::
       forall m a. (Monad m, Storable a)
    => (forall b. IO b -> m b) -> Unfold m (Array a) a
readRevWith liftio = Unfold step inject
    where

    inject (Array contents start end _) =
        let p = INDEX_PREV(end,a)
         in return $ ArrayUnsafe contents start p

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe _ start p) | p < start = return D.Stop
    step (ArrayUnsafe contents start p) = do
        !x <- liftio $ peekWith contents p
        return $ D.Yield x (ArrayUnsafe contents start (INDEX_PREV(p,a)))

-- | Unfold an array into a stream in reverse order.
--
-- /Pre-release/
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (MonadIO m, Storable a) => Unfold m (Array a) a
readRev = readRevWith liftIO

-------------------------------------------------------------------------------
-- to Lists and streams
-------------------------------------------------------------------------------

{-
-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Storable a => (a -> b -> b) -> b -> Array a -> b
toListFB c n Array{..} = go arrStart
    where

    go p | assert (p <= aEnd) (p == aEnd) = n
    go p =
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        -- XXX
        let !x = unsafeInlineIO $ do
                    r <- peekWith arrContents p
                    return r
        in c x (go (PTR_NEXT(p,a)))
-}

-- XXX Monadic foldr/build fusion?
-- Reference: https://www.researchgate.net/publication/220676509_Monadic_augment_and_generalised_short_cut_fusion
-- | Convert an 'Array' into a list.
--
-- @since 0.7.0
{-# INLINE toList #-}
toList :: forall m a. (MonadIO m, Storable a) => Array a -> m [a]
toList Array{..} = liftIO $ go arrStart
    where

    go p | assert (p <= aEnd) (p == aEnd) = return []
    go p = do
        x <- peekWith arrContents p
        (:) x <$> go (INDEX_NEXT(p,a))

{-# INLINE_NORMAL toStreamDWith #-}
toStreamDWith ::
       forall m a. (Monad m, Storable a)
    => (forall b. IO b -> m b) -> Array a -> D.Stream m a
toStreamDWith liftio Array{..} = D.Stream step arrStart

    where

    {-# INLINE_LATE step #-}
    step _ p | assert (p <= aEnd) (p == aEnd) = return D.Stop
    step _ p = liftio $ do
        r <- peekWith arrContents p
        return $ D.Yield r (INDEX_NEXT(p,a))

-- | Use the 'read' unfold instead.
--
-- @toStreamD = D.unfold read@
--
-- We can try this if the unfold has any performance issues.
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (MonadIO m, Storable a) => Array a -> D.Stream m a
toStreamD = toStreamDWith liftIO

{-# INLINE toStreamKWith #-}
toStreamKWith ::
       forall m a. (Monad m, Storable a)
    => (forall b. IO b -> m b) -> Array a -> K.Stream m a
toStreamKWith liftio Array{..} = go arrStart

    where

    go p | assert (p <= aEnd) (p == aEnd) = K.nil
         | otherwise =
        let elemM = do
              r <- peekWith arrContents p
              return r
        in liftio elemM `K.consM` go (INDEX_NEXT(p,a))

{-# INLINE toStreamK #-}
toStreamK :: forall m a. (MonadIO m, Storable a) => Array a -> K.Stream m a
toStreamK = toStreamKWith liftIO

{-# INLINE_NORMAL toStreamDRevWith #-}
toStreamDRevWith ::
       forall m a. (Monad m, Storable a)
    => (forall b. IO b -> m b) -> Array a -> D.Stream m a
toStreamDRevWith liftio Array{..} =
    let p = INDEX_PREV(aEnd,a)
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p < arrStart = return D.Stop
    step _ p = liftio $ do
        r <- peekWith arrContents p
        return $ D.Yield r (INDEX_PREV(p,a))

-- | Use the 'readRev' unfold instead.
--
-- @toStreamDRev = D.unfold readRev@
--
-- We can try this if the unfold has any perf issues.
{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (MonadIO m, Storable a) => Array a -> D.Stream m a
toStreamDRev = toStreamDRevWith liftIO

{-# INLINE toStreamKRevWith #-}
toStreamKRevWith ::
       forall m a. (Monad m, Storable a)
    => (forall b. IO b -> m b) -> Array a -> K.Stream m a
toStreamKRevWith liftio Array {..} =
    let p = INDEX_PREV(aEnd,a)
    in go p

    where

    go p | p < arrStart = K.nil
         | otherwise =
        let elemM = do
              r <- peekWith arrContents p
              return r
        in liftio elemM `K.consM` go (INDEX_PREV(p,a))

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. (MonadIO m, Storable a) => Array a -> K.Stream m a
toStreamKRev = toStreamKRevWith liftIO

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- XXX Need something like "Array m a" enforcing monadic action to avoid the
-- possibility of such APIs.
--
-- | Strict left fold of an array.
{-# INLINE_NORMAL foldl' #-}
foldl' :: (MonadIO m, Storable a) => (b -> a -> b) -> b -> Array a -> m b
foldl' f z arr = D.foldl' f z $ toStreamD arr

-- | Right fold of an array.
{-# INLINE_NORMAL foldr #-}
foldr :: (MonadIO m, Storable a) => (a -> b -> b) -> b -> Array a -> m b
foldr f z arr = D.foldr f z $ toStreamD arr

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- Note: Arrays may be allocated with a specific alignment at the beginning of
-- the array. If you need to maintain that alignment on reallocations then you
-- can resize the array manually before append, using an aligned resize
-- operation.

-- XXX Keep the bound intact to not lose any free space? Perf impact?

-- | @appendNUnsafe alloc n@ appends up to @n@ input items to the supplied
-- array.
--
-- Unsafe: Do not drive the fold beyond @n@ elements, it will lead to memory
-- corruption or segfault.
--
-- Any free space left in the array after appending @n@ elements is lost.
--
-- /Internal/
{-# INLINE_NORMAL appendNUnsafe #-}
appendNUnsafe :: forall m a. (MonadIO m, Storable a) =>
       m (Array a)
    -> Int
    -> Fold m a (Array a)
appendNUnsafe action n =
    fmap fromArrayUnsafe $ FL.foldlM' step initial

    where

    initial = do
        assert (n >= 0) (return ())
        arr@(Array _ _ end bound) <- action
        let free = bound - end
            needed = n * SIZE_OF(a)
        -- XXX We can also reallocate if the array has too much free space,
        -- otherwise we lose that space.
        arr1 <-
            if free < needed
            then noinline reallocWith "appendNUnsafeWith" (+ needed) needed arr
            else return arr
        return $ toArrayUnsafe arr1

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeWith contents end x
        return $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

-- | Append @n@ elements to an existing array. Any free space left in the array
-- after appending @n@ elements is lost.
--
-- >>> appendN initial n = Fold.take n (Array.appendNUnsafe initial n)
--
-- /Pre-release/
{-# INLINE_NORMAL appendN #-}
appendN :: forall m a. (MonadIO m, Storable a) =>
    m (Array a) -> Int -> Fold m a (Array a)
appendN initial n = FL.take n (appendNUnsafe initial n)

-- | @appendWith realloc action@ mutates the array generated by @action@ to
-- append the input stream. If there is no reserved space available in the
-- array it is reallocated to a size in bytes  determined by @realloc oldSize@,
-- where @oldSize@ is the current size of the array in bytes.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> appendWith sizer = Fold.foldlM' (Array.snocWith sizer)
--
-- /Pre-release/
{-# INLINE appendWith #-}
appendWith :: forall m a. (MonadIO m, Storable a) =>
    (Int -> Int) -> m (Array a) -> Fold m a (Array a)
appendWith sizer = FL.foldlM' (snocWith sizer)

-- | @append action@ mutates the array generated by @action@ to append the
-- input stream. If there is no reserved space available in the array it is
-- reallocated to double the size.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> append = Array.appendWith (* 2)
--
-- /Pre-release/
{-# INLINE append #-}
append :: forall m a. (MonadIO m, Storable a) =>
    m (Array a) -> Fold m a (Array a)
append = appendWith (* 2)

-- XXX We can carry bound as well in the state to make sure we do not lose the
-- remaining capacity. Need to check perf impact.
--
-- | Like 'writeNUnsafe' but takes a new array allocator @alloc size@ function
-- as argument.
--
-- >>> writeNWithUnsafe alloc n = Array.appendNUnsafe (alloc n) n
--
-- /Pre-release/
{-# INLINE_NORMAL writeNWithUnsafe #-}
writeNWithUnsafe :: forall m a. (MonadIO m, Storable a)
    => (Int -> m (Array a)) -> Int -> Fold m a (Array a)
writeNWithUnsafe alloc n = fromArrayUnsafe <$> FL.foldlM' step initial

    where

    initial = toArrayUnsafe <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeWith contents end x
        return
          $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- >>> writeNUnsafe = Array.writeNWithUnsafe Array.newArray
--
-- @since 0.7.0
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe = writeNWithUnsafe newArray

-- | @writeNWith alloc n@ folds a maximum of @n@ elements into an array
-- allocated using the @alloc@ function.
--
-- >>> writeNWith alloc n = Fold.take n (Array.writeNWithUnsafe alloc n)
-- >>> writeNWith alloc n = Array.appendN (alloc n) n
--
{-# INLINE_NORMAL writeNWith #-}
writeNWith :: forall m a. (MonadIO m, Storable a)
    => (Int -> m (Array a)) -> Int -> Fold m a (Array a)
writeNWith alloc n = FL.take n (writeNWithUnsafe alloc n)

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- >>> writeN = Array.writeNWith Array.newArray
-- >>> writeN n = Fold.take n (Array.writeNUnsafe n)
-- >>> writeN n = Array.appendN (Array.newArray n) n
--
-- @since 0.7.0
{-# INLINE_NORMAL writeN #-}
writeN :: forall m a. (MonadIO m, Storable a) => Int -> Fold m a (Array a)
writeN = writeNWith newArray

-- | Like writeNWithUnsafe but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWithUnsafe #-}
writeRevNWithUnsafe :: forall m a. (MonadIO m, Storable a)
    => (Int -> m (Array a)) -> Int -> Fold m a (Array a)
writeRevNWithUnsafe alloc n = fromArrayUnsafe <$> FL.foldlM' step initial

    where

    toArrayUnsafeRev (Array contents _ _ bound) =
         ArrayUnsafe contents bound bound

    initial = toArrayUnsafeRev <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        let ptr = INDEX_PREV(start,a)
        liftIO $ pokeWith contents ptr x
        return
          $ ArrayUnsafe contents ptr end

-- | Like writeNWith but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWith #-}
writeRevNWith :: forall m a. (MonadIO m, Storable a)
    => (Int -> m (Array a)) -> Int -> Fold m a (Array a)
writeRevNWith alloc n = FL.take n (writeRevNWithUnsafe alloc n)

-- | Like writeN but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE_NORMAL writeRevN #-}
writeRevN :: forall m a. (MonadIO m, Storable a) => Int -> Fold m a (Array a)
writeRevN = writeRevNWith newArray

-- | @writeNAligned align n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size.
--
-- >>> writeNAligned align = Array.writeNWith (Array.newArrayAligned align)
-- >>> writeNAligned align n = Array.appendN (Array.newArrayAligned align n) n
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAligned align = writeNWith (newArrayAligned align)

-- XXX Buffer to a list instead?
--
-- | Buffer a stream into a stream of arrays.
--
-- >>> writeChunks n = Fold.many (Array.writeN n) Fold.toStreamK
--
-- Breaking an array into an array stream  can be useful to consume a large
-- array sequentially such that memory of the array is released incrementatlly.
--
-- See also: 'arrayStreamKFromStreamD'.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL writeChunks #-}
writeChunks :: (MonadIO m, Storable a) =>
    Int -> Fold m a (K.Stream n (Array a))
writeChunks n = FL.many (writeN n) FL.toStreamK

-- XXX Compare writeWith with fromStreamD which uses an array of streams
-- implementation. We can write this using writeChunks above if that is faster.
-- If writeWith is faster then we should use that to implement
-- fromStreamD.
--
-- XXX The realloc based implementation needs to make one extra copy if we use
-- shrinkToFit.  On the other hand, the stream of arrays implementation may
-- buffer the array chunk pointers in memory but it does not have to shrink as
-- we know the exact size in the end. However, memory copying does not seem to
-- be as expensive as the allocations. Therefore, we need to reduce the number
-- of allocations instead. Also, the size of allocations matters, right sizing
-- an allocation even at the cost of copying sems to help.  Should be measured
-- on a big stream with heavy calls to toArray to see the effect.
--
-- XXX check if GHC's memory allocator is efficient enough. We can try the C
-- malloc to compare against.

-- | @writeWith minCount@ folds the whole input to a single array. The array
-- starts at a size big enough to hold minCount elements, the size is doubled
-- every time the array needs to be grown.
--
-- /Caution! Do not use this on infinite streams./
--
-- >>> f n = Array.appendWith (* 2) (Array.newArray n)
-- >>> writeWith n = Fold.rmapM Array.rightSize (f n)
-- >>> writeWith n = Fold.rmapM Array.fromArrayStreamK (Array.writeChunks n)
--
-- /Pre-release/
{-# INLINE_NORMAL writeWith #-}
writeWith :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
-- writeWith n = FL.rmapM rightSize $ appendWith (* 2) (newArray n)
writeWith elemCount =
    FL.rmapM extract $ FL.foldlM' step initial

    where

    insertElem (Array contents start end bound) x = do
        liftIO $ pokeWith contents end x
        return $ Array contents start (INDEX_NEXT(end,a)) bound

    initial = do
        when (elemCount < 0) $ error "writeWith: elemCount is negative"
        liftIO $ newArrayAligned (alignment (undefined :: a)) elemCount
    step arr@(Array _ start end bound) x
        | INDEX_NEXT(end,a) > bound = do
        let oldSize = end - start
            newSize = max (oldSize * 2) 1
        arr1 <-
            liftIO
                $ reallocAligned
                    (SIZE_OF(a))
                    (alignment (undefined :: a))
                    newSize
                    arr
        insertElem arr1 x
    step arr x = insertElem arr x
    extract = liftIO . rightSize

-- | Fold the whole input to a single array.
--
-- Same as 'writeWith' using an initial array size of 'arrayChunkBytes' bytes
-- rounded up to the element size.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
write = writeWith (allocBytesToElemCount (undefined :: a) arrayChunkBytes)

-------------------------------------------------------------------------------
-- construct from streams, known size
-------------------------------------------------------------------------------

-- | Use the 'writeN' fold instead.
--
-- >>> fromStreamDN n = StreamD.fold (Array.writeN n)
--
{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
-- fromStreamDN n = D.fold (writeN n)
fromStreamDN limit str = do
    arr <- liftIO $ newArray limit
    end <- D.foldlM' (fwrite (arrContents arr)) (return $ aEnd arr) $ D.take limit str
    return $ arr {aEnd = end}

    where

    fwrite arrContents ptr x = do
        liftIO $ pokeWith arrContents ptr x
        return $ INDEX_NEXT(ptr,a)

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
-- @since 0.7.0
{-# INLINABLE fromListN #-}
fromListN :: (MonadIO m, Storable a) => Int -> [a] -> m (Array a)
fromListN n xs = fromStreamDN n $ D.fromList xs

-- | Like fromListN but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE fromListRevN #-}
fromListRevN :: (MonadIO m, Storable a) => Int -> [a] -> m (Array a)
fromListRevN n xs = D.fold (writeRevN n) $ D.fromList xs

-------------------------------------------------------------------------------
-- convert stream to a single array
-------------------------------------------------------------------------------

{-# INLINE arrayStreamKLength #-}
arrayStreamKLength :: (Monad m, Storable a) => K.Stream m (Array a) -> m Int
arrayStreamKLength as = K.foldl' (+) 0 (K.map length as)

-- | Convert an array stream to an array. Note that this requires peak memory
-- that is double the size of the array stream.
--
{-# INLINE fromArrayStreamK #-}
fromArrayStreamK :: (Storable a, MonadIO m) =>
    K.Stream m (Array a) -> m (Array a)
fromArrayStreamK as = do
    len <- arrayStreamKLength as
    fromStreamDN len $ D.unfoldMany read $ D.fromStreamK as

-- CAUTION: a very large number (millions) of arrays can degrade performance
-- due to GC overhead because we need to buffer the arrays before we flatten
-- all the arrays.
--
-- XXX Compare if this is faster or "fold write".
--
-- | We could take the approach of doubling the memory allocation on each
-- overflow. This would result in more or less the same amount of copying as in
-- the chunking approach. However, if we have to shrink in the end then it may
-- result in an extra copy of the entire data.
--
-- >>> fromStreamD = StreamD.fold Array.write
--
{-# INLINE fromStreamD #-}
fromStreamD :: (MonadIO m, Storable a) => D.Stream m a -> m (Array a)
fromStreamD m = arrayStreamKFromStreamD m >>= fromArrayStreamK

-- | Create an 'Array' from a list. The list must be of finite size.
--
-- @since 0.7.0
{-# INLINE fromList #-}
fromList :: (MonadIO m, Storable a) => [a] -> m (Array a)
fromList xs = fromStreamD $ D.fromList xs

-- XXX We are materializing the whole list first for getting the length. Check
-- if the 'fromList' like chunked implementation would fare better.

-- | Like 'fromList' but writes the contents of the list in reverse order.
{-# INLINE fromListRev #-}
fromListRev :: (MonadIO m, Storable a) => [a] -> m (Array a)
fromListRev xs = fromListRevN (Prelude.length xs) xs

-------------------------------------------------------------------------------
-- Combining
-------------------------------------------------------------------------------

-- | Put a sub range of a source array into a subrange of a destination array.
-- This is not safe as it does not check the bounds.
{-# INLINE putSliceUnsafe #-}
putSliceUnsafe :: MonadIO m => Array a -> Int -> Array a -> Int -> Int -> m ()
putSliceUnsafe src srcStartBytes dst dstStartBytes lenBytes = liftIO $ do
    assertM (lenBytes <= aBound dst - dstStartBytes)
    assertM (lenBytes <= aEnd src - srcStartBytes)
    let !(I# srcStartBytes#) = srcStartBytes
        !(I# dstStartBytes#) = dstStartBytes
        !(I# lenBytes#) = lenBytes
    let arrS# = getMutableByteArray# (arrContents src)
        arrD# = getMutableByteArray# (arrContents dst)
    IO $ \s# -> (# copyMutableByteArray#
                    arrS# srcStartBytes# arrD# dstStartBytes# lenBytes# s#
                , () #)

-- | Copy two arrays into a newly allocated array.
{-# INLINE spliceCopy #-}
spliceCopy :: forall m a. (MonadIO m, Storable a) =>
    Array a -> Array a -> m (Array a)
spliceCopy arr1 arr2 = liftIO $ do
    let start1 = arrStart arr1
        start2 = arrStart arr2
        len1 = aEnd arr1 - start1
        len2 = aEnd arr2 - start2
    newArrContents <-
        liftIO
            $ newAlignedArrayContents
                  (len1 + len2)
                  (alignment (undefined :: a))
    let len = len1 + len2
        newArr = Array newArrContents 0 len len
    putSliceUnsafe arr1 start1 newArr 0 len1
    putSliceUnsafe arr2 start2 newArr len1 len2
    return newArr

-- | Really really unsafe, appends the second array into the first array. If
-- the first array does not have enough space it may cause silent data
-- corruption or if you are lucky a segfault.
{-# INLINE spliceUnsafe #-}
spliceUnsafe :: MonadIO m =>
    Array a -> Array a -> m (Array a)
spliceUnsafe dst src =
    liftIO $ do
         let startSrc = arrStart src
             srcLen = aEnd src - startSrc
             endDst = aEnd dst
         assertM (endDst + srcLen <= aBound dst)
         putSliceUnsafe src startSrc dst endDst srcLen
         return $ dst {aEnd = endDst + srcLen}

-- | @spliceWith sizer dst src@ mutates @dst@ to append @src@. If there is no
-- reserved space available in @dst@ it is reallocated to a size determined by
-- the @sizer dstBytes srcBytes@ function, where @dstBytes@ is the size of the
-- first array and @srcBytes@ is the size of the second array, in bytes.
--
-- Note that the returned array may be a mutated version of first array.
--
-- /Pre-release/
{-# INLINE spliceWith #-}
spliceWith :: forall m a. (MonadIO m, Storable a) =>
    (Int -> Int -> Int) -> Array a -> Array a -> m (Array a)
spliceWith sizer dst@(Array _ start end bound) src = do
{-
    let f = appendWith (`sizer` byteLength src) (return dst)
     in D.fold f (toStreamD src)
-}
    assert (end <= bound) (return ())
    let srcBytes = aEnd src - arrStart src

    dst1 <-
        if end + srcBytes >= bound
        then do
            let dstBytes = end - start
                newSizeInBytes = sizer dstBytes srcBytes
            when (newSizeInBytes < dstBytes + srcBytes)
                $ error
                    $ "splice: newSize is less than the total size "
                    ++ "of arrays being appended. Please check the "
                    ++ "sizer function passed."
            liftIO $ realloc newSizeInBytes dst
        else return dst
    spliceUnsafe dst1 src

-- | The first array is mutated to append the second array. If there is no
-- reserved space available in the first array a new allocation of exact
-- required size is done.
--
-- Note that the returned array may be a mutated version of first array.
--
-- >>> splice = Array.spliceWith (+)
--
-- /Pre-release/
{-# INLINE splice #-}
splice :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
splice = spliceWith (+)

-- | Like 'append' but the growth of the array is exponential. Whenever a new
-- allocation is required the previous array size is at least doubled.
--
-- This is useful to reduce allocations when folding many arrays together.
--
-- Note that the returned array may be a mutated version of first array.
--
-- >>> spliceExp = Array.spliceWith (\l1 l2 -> max (l1 * 2) (l1 + l2))
--
-- /Pre-release/
{-# INLINE spliceExp #-}
spliceExp :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
spliceExp = spliceWith (\l1 l2 -> max (l1 * 2) (l1 + l2))

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- | Drops the separator byte
{-# INLINE breakOn #-}
breakOn :: MonadIO m
    => Word8 -> Array Word8 -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr@Array{..} = asPtrUnsafe arr $ \p -> liftIO $ do
    -- XXX Instead of using asPtrUnsafe (pinning memory) we can pass unlifted
    -- Addr# to memchr and it should be safe (from ghc 8.4).
    -- XXX We do not need memchr here, we can use a Haskell equivalent.
    loc <- c_memchr p sep (fromIntegral $ byteLength arr)
    let sepIndex = loc `minusPtr` p
    return $
        if loc == nullPtr
        then (arr, Nothing)
        else
            ( Array
                { arrContents = arrContents
                , arrStart = arrStart
                , aEnd = arrStart + sepIndex -- exclude the separator
                , aBound = arrStart + sepIndex
                }
            , Just $ Array
                    { arrContents = arrContents
                    , arrStart = arrStart + (sepIndex + 1)
                    , aEnd = aEnd
                    , aBound = aBound
                    }
            )

-- | Create two slices of an array without copying the original array. The
-- specified index @i@ is the first index of the second slice.
--
-- @since 0.7.0
splitAt :: forall a. Storable a => Int -> Array a -> (Array a, Array a)
splitAt i arr@Array{..} =
    let maxIndex = length arr - 1
    in  if i < 0
        then error "sliceAt: negative array index"
        else if i > maxIndex
             then error $ "sliceAt: specified array index " ++ show i
                        ++ " is beyond the maximum index " ++ show maxIndex
             else let off = i * SIZE_OF(a)
                      p = arrStart + off
                in ( Array
                  { arrContents = arrContents
                  , arrStart = arrStart
                  , aEnd = p
                  , aBound = p
                  }
                , Array
                  { arrContents = arrContents
                  , arrStart = p
                  , aEnd = aEnd
                  , aBound = aBound
                  }
                )

-------------------------------------------------------------------------------
-- Casting
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
    Storable b =>
#endif
    Array a -> Array b
castUnsafe (Array contents start end bound) =
    Array (castContents contents) start end bound

-- | Cast an @Array a@ into an @Array Word8@.
--
-- /Pre-release/
--
asBytes :: Array a -> Array Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
-- /Pre-release/
--
cast :: forall a b. Storable b => Array a -> Maybe (Array b)
cast arr =
    let len = byteLength arr
        r = len `mod` SIZE_OF(b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

-- XXX We can provide another API for "unsafe" FFI calls passing an unlifted
-- pointer to the FFI call. For unsafe calls we do not need to pin the array.
-- We can pass an unlifted pointer to the FFI routine to avoid GC kicking in
-- before the pointer is wrapped.
--
-- From the GHC manual:
--
-- GHC, since version 8.4, guarantees that garbage collection will never occur
-- during an unsafe call, even in the bytecode interpreter, and further
-- guarantees that unsafe calls will be performed in the calling thread. Making
-- it safe to pass heap-allocated objects to unsafe functions.

-- Unsafe because of direct pointer operations. The user must ensure that they
-- are writing within the legal bounds of the array. Should we just name it
-- asPtr, the unsafety is implicit for any pointer operations. And we are safe
-- from Haskell perspective because we will be pinning the memory.

-- | Use an @Array a@ as @Ptr a@. This is useful when we want to pass an array
-- as a pointer to some operating system call or to a "safe" FFI call.
--
-- If the array is not pinned it is copied to pinned memory before passing it
-- to the monadic action.
--
-- /Performance Notes:/ Forces a copy if the array is not pinned. It is advised
-- that the programmer keeps this in mind and creates a pinned array
-- opportunistically before this operation occurs, to avoid the cost of a copy
-- if possible.
--
-- /Unsafe/
--
-- /Pre-release/
--
asPtrUnsafe :: MonadIO m => Array a -> (Ptr a -> m b) -> m b
asPtrUnsafe arr f = do
  let contents = arrContents arr
      !ptr = Ptr (byteArrayContents#
                     (unsafeCoerce# (getMutableByteArray# contents)))
  -- XXX Check if the array is pinned, if not, copy it to a pinned array
  -- XXX We should probably pass to the IO action the byte length of the array
  -- as well so that bounds can be checked.
  r <- f (ptr `plusPtr` arrStart arr)
  liftIO $ touch contents
  return r

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- | Compare the length of the arrays. If the length is equal, compare the
-- lexicographical ordering of two underlying byte arrays
--
-- /Pre-release/
{-# INLINE cmp #-}
cmp :: MonadIO m => Array a -> Array a -> m Ordering
cmp arr1 arr2 =
    liftIO
        $ do
            let marr1 = getMutableByteArray# (arrContents arr1)
                marr2 = getMutableByteArray# (arrContents arr2)
                !(I# st1#) = arrStart arr1
                !(I# st2#) = arrStart arr2
                !(I# len#) = byteLength arr1
            case compare (byteLength arr1) (byteLength arr2) of
                EQ -> do
                    r <- liftIO $ IO $ \s# ->
                             let res =
                                     I#
                                         (compareByteArrays#
                                              (unsafeCoerce# marr1)
                                              st1#
                                              (unsafeCoerce# marr2)
                                              st2#
                                              len#)
                              in (# s#, res #)
                    return $ compare r 0
                x -> return x

-------------------------------------------------------------------------------
-- NFData
-------------------------------------------------------------------------------

-- This is a Storable array, we cannot have unevaluated data in it so this is
-- just a no op.
instance NFData (Array a) where
    {-# INLINE rnf #-}
    rnf Array {} = ()

instance NFData1 Array where
    {-# INLINE liftRnf #-}
    liftRnf _ Array{} = ()

-- | Strip elements which match with predicate from both ends.
--
-- /Pre-release/
{-# INLINE strip #-}
strip :: forall a m. (Storable a, MonadIO m) =>
    (a -> Bool) -> Array a -> m (Array a)
strip eq arr@Array{..} = liftIO $ do
    st <- getStart arrStart
    end <- getLast aEnd st
    return arr {arrStart = st, aEnd = end, aBound = end}

    where

    {-
    -- XXX This should have the same perf but it does not, investigate.
    getStart = do
        r <- liftIO $ D.head $ D.findIndices (not . eq) $ toStreamD arr
        pure $
            case r of
                Nothing -> aEnd
                Just i -> PTR_INDEX(arrStart,i,a)
    -}

    getStart cur = do
        if cur < aEnd
        then do
            r <- peekWith arrContents cur
            if eq r
            then getStart (INDEX_NEXT(cur,a))
            else return cur
        else return cur

    getLast cur low = do
        if cur > low
        then do
            let prev = INDEX_PREV(cur,a)
            r <- peekWith arrContents prev
            if eq r
            then getLast prev low
            else return cur
        else return cur

-- | Given an array sorted in ascending order except the last element being out
-- of order, use bubble sort to place the last element at the right place such
-- that the array remains sorted in ascending order.
--
-- /Pre-release/
{-# INLINE bubble #-}
bubble :: (MonadIO m, Storable a) => (a -> a -> Ordering) -> Array a -> m ()
bubble cmp0 arr =
    when (l > 1) $ do
        x <- getIndexUnsafe (l - 1) arr
        go x (l - 2)

        where

        l = length arr

        go x i =
            if i >= 0
            then do
                x1 <- getIndexUnsafe i arr
                case x `cmp0` x1 of
                    LT -> do
                        putIndexUnsafe (i + 1) x1 arr
                        go x (i - 1)
                    _ -> putIndexUnsafe (i + 1) x arr
            else putIndexUnsafe (i + 1) x arr
