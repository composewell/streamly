{-# LANGUAGE CPP #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Streamly.Internal.Data.Array.Mut.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Pinned and unpinned mutable array for 'Unboxed' types. Fulfils the following
-- goals:
--
-- * Random access (array)
-- * Efficient storage (unboxed)
-- * Performance (unboxed access)
-- * Performance - in-place operations (mutable)
-- * Performance - GC (pinned, mutable)
-- * interfacing with OS (pinned)
--
-- Stream and Fold APIs allow easy, efficient and convenient operations on
-- arrays.
--
-- Mutable arrays and file system files are quite similar, they can grow and
-- their content is mutable. Therefore, both have similar APIs as well. We
-- strive to keep the API consistent for both. Ideally, you should be able to
-- replace one with another with little changes to the code.

module Streamly.Internal.Data.Array.Mut.Type
    (
    -- * Type
    -- $arrayNotes
      MutArray (..)
    , MutableByteArray
    , touch
    , pin
    , unpin

    -- * Constructing and Writing
    -- ** Construction
    , nil

    -- *** Uninitialized Arrays
    , newPinned
    , newPinnedBytes
    , newAlignedPinned
    , new
    , newArrayWith

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
    , writeAppendNUnsafe
    , writeAppendN
    , writeAppendWith
    , writeAppend

    -- * Eliminating and Reading

    -- ** To streams
    , reader
    , readerRevWith
    , readerRev

    -- ** To containers
    , toStreamDWith
    , toStreamDRevWith
    , toStreamKWith
    , toStreamKRevWith
    , read
    , readRev
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
    , chunksOf
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

#include "assert.hs"
#include "inline.hs"
#include "ArrayMacros.h"
#include "MachDeps.h"

import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, (.|.), (.&.))
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.Ptr (plusPtr, minusPtr, nullPtr)
import Streamly.Internal.Data.Unbox
    ( MutableByteArray(..)
    , Unbox(..)
    , getMutableByteArray#
    , touch
    )
import GHC.Base
    ( IO(..)
    , Int(..)
    , byteArrayContents#
    , compareByteArrays#
    , copyMutableByteArray#
    )
import GHC.Base (noinline)
import GHC.Exts (unsafeCoerce#)
import GHC.Ptr (Ptr(..))

import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.Stream.StreamD.Type (Stream)
import Streamly.Internal.Data.Stream.StreamK.Type (StreamK)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.System.IO (arrayPayloadSize, defaultChunkSize)

import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Unbox as Unboxed
import qualified Prelude

import Prelude hiding
    (Foldable(..), read, unlines, splitAt, reverse, truncate)

#include "DocTestDataMutArray.hs"

-------------------------------------------------------------------------------
-- Foreign helpers
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- | Given an 'Unboxed' type (unused first arg) and a number of bytes, return
-- how many elements of that type will completely fit in those bytes.
--
{-# INLINE bytesToElemCount #-}
bytesToElemCount :: forall a. Unbox a => a -> Int -> Int
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
-- MutArray Data Type
-------------------------------------------------------------------------------

-- $arrayNotes
--
-- We can use an 'Unboxed' constraint in the MutArray type and the constraint
-- can be automatically provided to a function that pattern matches on the
-- MutArray type. However, it has huge performance cost, so we do not use it.
-- Investigate a GHC improvement possiblity.

-- | An unboxed mutable array. An array is created with a given length
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
data MutArray a =
#ifdef DEVBUILD
    Unbox a =>
#endif
    -- The array is a range into arrContents. arrContents may be a superset of
    -- the slice represented by the array. All offsets are in bytes.
    MutArray
    { arrContents :: {-# UNPACK #-} !MutableByteArray
    , arrStart :: {-# UNPACK #-} !Int  -- ^ index into arrContents
    , arrEnd   :: {-# UNPACK #-} !Int    -- ^ index into arrContents
                                       -- Represents the first invalid index of
                                       -- the array.
    , arrBound :: {-# UNPACK #-} !Int    -- ^ first invalid index of arrContents.
    }

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

{-# INLINE pin #-}
pin :: MutArray a -> IO (MutArray a)
pin arr@MutArray{..} = do
    contents <- Unboxed.pin arrContents
    return $ arr {arrContents = contents}

{-# INLINE unpin #-}
unpin :: MutArray a -> IO (MutArray a)
unpin arr@MutArray{..} = do
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
-- Alignment is ignored if the allocator allocates unpinned memory.
--
-- /Pre-release/
{-# INLINE newArrayWith #-}
newArrayWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> Int -> m MutableByteArray) -> Int -> Int -> m (MutArray a)
newArrayWith alloc alignSize count = do
    let size = max (count * SIZE_OF(a)) 0
    contents <- alloc size alignSize
    return $ MutArray
        { arrContents = contents
        , arrStart = 0
        , arrEnd   = 0
        , arrBound = size
        }

nil ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    MutArray a
nil = MutArray Unboxed.nil 0 0 0


-- | Allocates a pinned empty array that can hold 'count' items.  The memory of
-- the array is uninitialized and the allocation is aligned as per the
-- 'Unboxed' instance of the type.
--
-- /Pre-release/
{-# INLINE newPinnedBytes #-}
newPinnedBytes :: MonadIO m =>
#ifdef DEVBUILD
    Unbox a =>
#endif
    Int -> m (MutArray a)
newPinnedBytes bytes = do
    contents <- liftIO $ Unboxed.newPinnedBytes bytes
    return $ MutArray
        { arrContents = contents
        , arrStart = 0
        , arrEnd   = 0
        , arrBound = bytes
        }

-- | Like 'newArrayWith' but using an allocator is a pinned memory allocator and
-- the alignment is dictated by the 'Unboxed' instance of the type.
--
-- /Internal/
{-# INLINE newAlignedPinned #-}
newAlignedPinned :: (MonadIO m, Unbox a) => Int -> Int -> m (MutArray a)
newAlignedPinned =
    newArrayWith (\s a -> liftIO $ Unboxed.newAlignedPinnedBytes s a)

-- XXX can unaligned allocation be more efficient when alignment is not needed?
--
-- | Allocates an empty pinned array that can hold 'count' items.  The memory of
-- the array is uninitialized and the allocation is aligned as per the 'Unboxed'
-- instance of the type.
--
{-# INLINE newPinned #-}
newPinned :: forall m a. (MonadIO m, Unbox a) => Int -> m (MutArray a)
newPinned =
    newArrayWith
        (\s _ -> liftIO $ Unboxed.newPinnedBytes s)
        (error "newPinned: alignSize is not used")

-- | Allocates an empty unpinned array that can hold 'count' items.  The memory
-- of the array is uninitialized.
--
{-# INLINE new #-}
new :: (MonadIO m, Unbox a) => Int -> m (MutArray a)
new =
    newArrayWith
        (\s _ -> liftIO $ Unboxed.newUnpinnedBytes s)
        (error "new: alignment is not used in unpinned arrays.")

-- XXX This should create a full length uninitialzed array so that the pointer
-- can be used.

-- | Allocate a pinned MutArray of the given size and run an IO action passing
-- the array start pointer.
--
-- /Internal/
{-# INLINE withNewArrayUnsafe #-}
withNewArrayUnsafe ::
       (MonadIO m, Unbox a) => Int -> (Ptr a -> m ()) -> m (MutArray a)
withNewArrayUnsafe count f = do
    arr <- newPinned count
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
putIndexUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> MutArray a -> a -> m ()
putIndexUnsafe i MutArray{..} x = do
    let index = INDEX_OF(arrStart, i, a)
    assert (i >= 0 && INDEX_VALID(index, arrEnd, a)) (return ())
    liftIO $ pokeByteIndex index arrContents  x

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex ix arr val = MutArray.modifyIndex ix arr (const (val, ()))
-- >>> f = MutArray.putIndices
-- >>> putIndex ix arr val = Stream.fold (f arr) (Stream.fromPure (ix, val))
--
{-# INLINE putIndex #-}
putIndex :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> a -> m ()
putIndex i MutArray{..} x = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ pokeByteIndex index arrContents  x
    else invalidIndex "putIndex" i

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: forall m a. (MonadIO m, Unbox a)
    => MutArray a -> Fold m (Int, a) ()
putIndices arr = FL.foldlM' step (return ())

    where

    step () (i, x) = liftIO (putIndex i arr x)

-- | Modify a given index of an array using a modifier function.
--
-- Unsafe because it does not check the bounds of the array.
--
-- /Pre-release/
modifyIndexUnsafe :: forall m a b. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> (a -> (a, b)) -> m b
modifyIndexUnsafe i MutArray{..} f = liftIO $ do
        let index = INDEX_OF(arrStart,i,a)
        assert (i >= 0 && INDEX_NEXT(index,a) <= arrEnd) (return ())
        r <- peekByteIndex index arrContents
        let (x, res) = f r
        pokeByteIndex index arrContents  x
        return res

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndex :: forall m a b. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> (a -> (a, b)) -> m b
modifyIndex i MutArray{..} f = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ do
        r <- peekByteIndex index arrContents
        let (x, res) = f r
        pokeByteIndex index arrContents  x
        return res
    else invalidIndex "modifyIndex" i

-- | Modify the array indices generated by the supplied stream.
--
-- /Pre-release/
{-# INLINE modifyIndices #-}
modifyIndices :: forall m a . (MonadIO m, Unbox a)
    => MutArray a -> (Int -> a -> a) -> Fold m Int ()
modifyIndices arr f = FL.foldlM' step initial

    where

    initial = return ()

    step () i =
        let f1 x = (f i x, ())
         in modifyIndex i arr f1

-- | Modify each element of an array using the supplied modifier function.
--
-- This is an in-place equivalent of an immutable map operation.
--
-- /Pre-release/
modify :: forall m a. (MonadIO m, Unbox a)
    => MutArray a -> (a -> a) -> m ()
modify MutArray{..} f = liftIO $
    go arrStart

    where

    go i =
        when (INDEX_VALID(i,arrEnd,a)) $ do
            r <- peekByteIndex i arrContents
            pokeByteIndex i arrContents (f r)
            go (INDEX_NEXT(i,a))

-- XXX We could specify the number of bytes to swap instead of Proxy. Need
-- to ensure that the memory does not overlap.
{-# INLINE swapArrayByteIndices #-}
swapArrayByteIndices ::
       forall a. Unbox a
    => Proxy a
    -> MutableByteArray
    -> Int
    -> Int
    -> IO ()
swapArrayByteIndices _ arrContents i1 i2 = do
    r1 <- peekByteIndex i1 arrContents
    r2 <- peekByteIndex i2 arrContents
    pokeByteIndex i1 arrContents (r2 :: a)
    pokeByteIndex i2 arrContents (r1 :: a)

-- | Swap the elements at two indices without validating the indices.
--
-- /Unsafe/: This could result in memory corruption if indices are not valid.
--
-- /Pre-release/
{-# INLINE unsafeSwapIndices #-}
unsafeSwapIndices :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> MutArray a -> m ()
unsafeSwapIndices i1 i2 MutArray{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        swapArrayByteIndices (Proxy :: Proxy a) arrContents t1 t2

-- | Swap the elements at two indices.
--
-- /Pre-release/
swapIndices :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> MutArray a -> m ()
swapIndices i1 i2 MutArray{..} = liftIO $ do
        let t1 = INDEX_OF(arrStart,i1,a)
            t2 = INDEX_OF(arrStart,i2,a)
        when (i1 < 0 || INDEX_INVALID(t1,arrEnd,a))
            $ invalidIndex "swapIndices" i1
        when (i2 < 0 || INDEX_INVALID(t2,arrEnd,a))
            $ invalidIndex "swapIndices" i2
        swapArrayByteIndices (Proxy :: Proxy a) arrContents t1 t2

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
allocBytesToBytes :: forall a. Unbox a => a -> Int -> Int
allocBytesToBytes _ n = max (arrayPayloadSize n) (SIZE_OF(a))

-- | Given an 'Unboxed' type (unused first arg) and real allocation size
-- (including overhead), return how many elements of that type will completely
-- fit in it, returns at least 1.
--
{-# INLINE allocBytesToElemCount #-}
allocBytesToElemCount :: Unbox a => a -> Int -> Int
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
-- NOTE: we are passing elemSize explicitly to avoid an Unboxed constraint.
-- Since this is not inlined Unboxed consrraint leads to dictionary passing
-- which complicates some inspection tests.
--
{-# NOINLINE reallocExplicit #-}
reallocExplicit :: Int -> Int -> MutArray a -> IO (MutArray a)
reallocExplicit elemSize newCapacityInBytes MutArray{..} = do
    assertM(arrEnd <= arrBound)

    -- Allocate new array
    let newCapMaxInBytes = roundUpLargeArray newCapacityInBytes
    contents <- Unboxed.newPinnedBytes newCapMaxInBytes
    let !(MutableByteArray mbarrFrom#) = arrContents
        !(MutableByteArray mbarrTo#) = contents

    -- Copy old data
    let oldStart = arrStart
        !(I# oldStartInBytes#) = oldStart
        oldSizeInBytes = arrEnd - oldStart
        newCapInBytes = roundDownTo elemSize newCapMaxInBytes
        !newLenInBytes@(I# newLenInBytes#) = min oldSizeInBytes newCapInBytes
    assert (oldSizeInBytes `mod` elemSize == 0) (return ())
    assert (newLenInBytes >= 0) (return ())
    assert (newLenInBytes `mod` elemSize == 0) (return ())
    IO $ \s# -> (# copyMutableByteArray# mbarrFrom# oldStartInBytes#
                        mbarrTo# 0# newLenInBytes# s#, () #)

    return $ MutArray
        { arrStart = 0
        , arrContents = contents
        , arrEnd   = newLenInBytes
        , arrBound = newCapInBytes
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
realloc :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m (MutArray a)
realloc bytes arr = liftIO $ reallocExplicit (SIZE_OF(a)) bytes arr

-- | @reallocWith label capSizer minIncrBytes array@. The label is used
-- in error messages and the capSizer is used to determine the capacity of the
-- new array in bytes given the current byte length of the array.
reallocWith :: forall m a. (MonadIO m , Unbox a) =>
       String
    -> (Int -> Int)
    -> Int
    -> MutArray a
    -> m (MutArray a)
reallocWith label capSizer minIncrBytes arr = do
    let oldSizeBytes = arrEnd arr - arrStart arr
        newCapBytes = capSizer oldSizeBytes
        newSizeBytes = oldSizeBytes + minIncrBytes
        safeCapBytes = max newCapBytes newSizeBytes
    assertM(safeCapBytes >= newSizeBytes || error (badSize newSizeBytes))

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
resize :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m (MutArray a)
resize nElems arr@MutArray{..} = do
    let req = SIZE_OF(a) * nElems
        len = arrEnd - arrStart
    if req < len
    then return arr
    else realloc req arr

-- | Like 'resize' but if the byte capacity is more than 'largeObjectThreshold'
-- then it is rounded up to the closest power of 2.
--
-- /Pre-release/
{-# INLINE resizeExp #-}
resizeExp :: forall m a. (MonadIO m, Unbox a) =>
    Int -> MutArray a -> m (MutArray a)
resizeExp nElems arr@MutArray{..} = do
    let req = roundUpLargeArray (SIZE_OF(a) * nElems)
        req1 =
            if req > largeObjectThreshold
            then roundUpToPower2 req
            else req
        len = arrEnd - arrStart
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
rightSize :: forall m a. (MonadIO m, Unbox a) => MutArray a -> m (MutArray a)
rightSize arr@MutArray{..} = do
    assert (arrEnd <= arrBound) (return ())
    let start = arrStart
        len = arrEnd - start
        capacity = arrBound - start
        target = roundUpLargeArray len
        waste = arrBound - arrEnd
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
-- :: MutArray a -> a -> m ()), it will just modify the old reference.  The array
-- length will be mutable.  This means the length function would also be
-- monadic.  Mutable arrays would behave more like files that grow in that
-- case.

-- | Snoc using a 'Ptr'. Low level reusable function.
--
-- /Internal/
{-# INLINE snocNewEnd #-}
snocNewEnd :: (MonadIO m, Unbox a) => Int -> MutArray a -> a -> m (MutArray a)
snocNewEnd newEnd arr@MutArray{..} x = liftIO $ do
    assert (newEnd <= arrBound) (return ())
    pokeByteIndex arrEnd arrContents x
    return $ arr {arrEnd = newEnd}

-- | Really really unsafe, appends the element into the first array, may
-- cause silent data corruption or if you are lucky a segfault if the first
-- array does not have enough space to append the element.
--
-- /Internal/
{-# INLINE snocUnsafe #-}
snocUnsafe :: forall m a. (MonadIO m, Unbox a) =>
    MutArray a -> a -> m (MutArray a)
snocUnsafe arr@MutArray{..} = snocNewEnd (INDEX_NEXT(arrEnd,a)) arr

-- | Like 'snoc' but does not reallocate when pre-allocated array capacity
-- becomes full.
--
-- /Internal/
{-# INLINE snocMay #-}
snocMay :: forall m a. (MonadIO m, Unbox a) =>
    MutArray a -> a -> m (Maybe (MutArray a))
snocMay arr@MutArray{..} x = liftIO $ do
    let newEnd = INDEX_NEXT(arrEnd,a)
    if newEnd <= arrBound
    then Just <$> snocNewEnd newEnd arr x
    else return Nothing

-- NOINLINE to move it out of the way and not pollute the instruction cache.
{-# NOINLINE snocWithRealloc #-}
snocWithRealloc :: forall m a. (MonadIO m, Unbox a) =>
       (Int -> Int)
    -> MutArray a
    -> a
    -> m (MutArray a)
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
snocWith :: forall m a. (MonadIO m, Unbox a) =>
       (Int -> Int)
    -> MutArray a
    -> a
    -> m (MutArray a)
snocWith allocSize arr x = liftIO $ do
    let newEnd = INDEX_NEXT(arrEnd arr,a)
    if newEnd <= arrBound arr
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
snocLinear :: forall m a. (MonadIO m, Unbox a) => MutArray a -> a -> m (MutArray a)
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
-- >>> snoc = MutArray.snocWith (* 2)
--
-- Performs O(n * log n) copies to grow, but is liberal with memory allocation.
--
{-# INLINE snoc #-}
snoc :: forall m a. (MonadIO m, Unbox a) => MutArray a -> a -> m (MutArray a)
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
getIndexUnsafe :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m a
getIndexUnsafe i MutArray{..} = do
    let index = INDEX_OF(arrStart,i,a)
    assert (i >= 0 && INDEX_VALID(index,arrEnd,a)) (return ())
    liftIO $ peekByteIndex index arrContents

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m (Maybe a)
getIndex i MutArray{..} = do
    let index = INDEX_OF(arrStart,i,a)
    if i >= 0 && INDEX_VALID(index,arrEnd,a)
    then liftIO $ Just <$> peekByteIndex index arrContents
    else return Nothing

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: forall m a. (MonadIO m, Unbox a) => Int -> MutArray a -> m a
getIndexRev i MutArray{..} = do
    let index = RINDEX_OF(arrEnd,i,a)
    if i >= 0 && index >= arrStart
    then liftIO $ peekByteIndex index arrContents
    else invalidIndex "getIndexRev" i

data GetIndicesState contents start end st =
    GetIndicesState contents start end st

-- | Given an unfold that generates array indices, read the elements on those
-- indices from the supplied MutArray. An error is thrown if an index is out of
-- bounds.
--
-- /Pre-release/
{-# INLINE getIndicesD #-}
getIndicesD :: (Monad m, Unbox a) =>
    (forall b. IO b -> m b) -> D.Stream m Int -> Unfold m (MutArray a) a
getIndicesD liftio (D.Stream stepi sti) = Unfold step inject

    where

    inject (MutArray contents start end _) =
        return $ GetIndicesState contents start end sti

    {-# INLINE_LATE step #-}
    step (GetIndicesState contents start end st) = do
        r <- stepi defState st
        case r of
            D.Yield i s -> do
                x <- liftio $ getIndex i (MutArray contents start end undefined)
                case x of
                    Just v -> return $ D.Yield v (GetIndicesState contents start end s)
                    Nothing -> error "Invalid Index"
            D.Skip s -> return $ D.Skip (GetIndicesState contents start end s)
            D.Stop -> return D.Stop

{-# INLINE getIndices #-}
getIndices :: (MonadIO m, Unbox a) => Stream m Int -> Unfold m (MutArray a) a
getIndices = getIndicesD liftIO

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
getSliceUnsafe :: forall a. Unbox a
    => Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
getSliceUnsafe index len (MutArray contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in assert
            (index >= 0 && len >= 0 && end <= e)
            -- Note: In a slice we always use bound = end so that the slice
            -- user cannot overwrite elements beyond the end of the slice.
            (MutArray contents fp1 end end)

-- | /O(1)/ Slice an array in constant time. Throws an error if the slice
-- extends out of the array bounds.
--
-- /Pre-release/
{-# INLINE getSlice #-}
getSlice :: forall a. Unbox a =>
       Int -- ^ from index
    -> Int -- ^ length of the slice
    -> MutArray a
    -> MutArray a
getSlice index len (MutArray contents start e _) =
    let fp1 = INDEX_OF(start,index,a)
        end = fp1 + (len * SIZE_OF(a))
     in if index >= 0 && len >= 0 && end <= e
        -- Note: In a slice we always use bound = end so that the slice user
        -- cannot overwrite elements beyond the end of the slice.
        then MutArray contents fp1 end end
        else error
                $ "getSlice: invalid slice, index "
                ++ show index ++ " length " ++ show len

-------------------------------------------------------------------------------
-- In-place mutation algorithms
-------------------------------------------------------------------------------

-- XXX consider the bulk update/accumulation/permutation APIs from vector.

-- | You may not need to reverse an array because you can consume it in reverse
-- using 'readerRev'. To reverse large arrays you can read in reverse and write
-- to another array. However, in-place reverse can be useful to take adavantage
-- of cache locality and when you do not want to allocate additional memory.
--
{-# INLINE reverse #-}
reverse :: forall m a. (MonadIO m, Unbox a) => MutArray a -> m ()
reverse MutArray{..} = liftIO $ do
    let l = arrStart
        h = INDEX_PREV(arrEnd,a)
     in swap l h

    where

    swap l h = do
        when (l < h) $ do
            swapArrayByteIndices (Proxy :: Proxy a) arrContents l h
            swap (INDEX_NEXT(l,a)) (INDEX_PREV(h,a))

-- | Generate the next permutation of the sequence, returns False if this is
-- the last permutation.
--
-- /Unimplemented/
{-# INLINE permute #-}
permute :: MutArray a -> m Bool
permute = undefined

-- | Partition an array into two halves using a partitioning predicate. The
-- first half retains values where the predicate is 'False' and the second half
-- retains values where the predicate is 'True'.
--
-- /Pre-release/
{-# INLINE partitionBy #-}
partitionBy :: forall m a. (MonadIO m, Unbox a)
    => (a -> Bool) -> MutArray a -> m (MutArray a, MutArray a)
partitionBy f arr@MutArray{..} = liftIO $ do
    if arrStart >= arrEnd
    then return (arr, arr)
    else do
        ptr <- go arrStart (INDEX_PREV(arrEnd,a))
        let pl = MutArray arrContents arrStart ptr ptr
            pr = MutArray arrContents ptr arrEnd arrEnd
        return (pl, pr)

    where

    -- Invariant low < high on entry, and on return as well
    moveHigh low high = do
        h <- peekByteIndex high arrContents
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
        l <- peekByteIndex low arrContents
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
                        pokeByteIndex low arrContents h
                        pokeByteIndex high1 arrContents l
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
shuffleBy :: (a -> a -> m Bool) -> MutArray a -> MutArray a -> m ()
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
    Int -> (MutArray a -> m (MutArray a, MutArray a)) -> MutArray a -> m ()
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
mergeBy :: Int -> (MutArray a -> MutArray a -> m ()) -> MutArray a -> m ()
mergeBy = undefined

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

-- | /O(1)/ Get the byte length of the array.
--
{-# INLINE byteLength #-}
byteLength :: MutArray a -> Int
byteLength MutArray{..} =
    let len = arrEnd - arrStart
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
{-# INLINE length #-}
length :: forall a. Unbox a => MutArray a -> Int
length arr =
    let elemSize = SIZE_OF(a)
        blen = byteLength arr
     in assert (blen `mod` elemSize == 0) (blen `div` elemSize)

-- | Get the total capacity of an array. An array may have space reserved
-- beyond the current used length of the array.
--
-- /Pre-release/
{-# INLINE byteCapacity #-}
byteCapacity :: MutArray a -> Int
byteCapacity MutArray{..} =
    let len = arrBound - arrStart
    in assert (len >= 0) len

-- | The remaining capacity in the array for appending more elements without
-- reallocation.
--
-- /Pre-release/
{-# INLINE bytesFree #-}
bytesFree :: MutArray a -> Int
bytesFree MutArray{..} =
    let n = arrBound - arrEnd
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

-- | @chunksOf n stream@ groups the input stream into a stream of
-- arrays of size n.
--
-- @chunksOf n = StreamD.foldMany (MutArray.writeN n)@
--
-- /Pre-release/
{-# INLINE_NORMAL chunksOf #-}
chunksOf :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> D.Stream m (MutArray a)
-- XXX the idiomatic implementation leads to large regression in the D.reverse'
-- benchmark. It seems it has difficulty producing optimized code when
-- converting to StreamK. Investigate GHC optimizations.
-- chunksOf n = D.foldMany (writeN n)
chunksOf n (D.Stream step state) =
    D.Stream step' (GroupStart state)

    where

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.MutArray.Mut.Type.chunksOf: "
                    ++ "the size of arrays [" ++ show n
                    ++ "] must be a natural number"
        (MutArray contents start end bound :: MutArray a) <- liftIO $ newPinned n
        return $ D.Skip (GroupBuffer st contents start end bound)

    step' gst (GroupBuffer st contents start end bound) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                liftIO $ pokeByteIndex end contents  x
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
        return $ D.Yield (MutArray contents start end bound) next

    step' _ GroupFinish = return D.Stop

-- XXX buffer to a list instead?
-- | Buffer the stream into arrays in memory.
{-# INLINE arrayStreamKFromStreamD #-}
arrayStreamKFromStreamD :: forall m a. (MonadIO m, Unbox a) =>
    D.Stream m a -> m (StreamK m (MutArray a))
arrayStreamKFromStreamD =
    let n = allocBytesToElemCount (undefined :: a) defaultChunkSize
     in D.foldr K.cons K.nil . chunksOf n

-------------------------------------------------------------------------------
-- Streams of arrays - Flattening
-------------------------------------------------------------------------------

data FlattenState s contents a =
      OuterLoop s
    | InnerLoop s contents !Int !Int

-- | Use the "reader" unfold instead.
--
-- @flattenArrays = unfoldMany reader@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
flattenArrays (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield MutArray{..} s ->
                D.Skip (InnerLoop s arrContents arrStart arrEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p end) | assert (p <= end) (p == end) =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p end) = do
        x <- liftIO $ peekByteIndex p contents
        return $ D.Yield x (InnerLoop st contents (INDEX_NEXT(p,a)) end)

-- | Use the "readerRev" unfold instead.
--
-- @flattenArrays = unfoldMany readerRev@
--
-- We can try this if there are any fusion issues in the unfold.
--
{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev :: forall m a. (MonadIO m, Unbox a)
    => D.Stream m (MutArray a) -> D.Stream m a
flattenArraysRev (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield MutArray{..} s ->
                let p = INDEX_PREV(arrEnd,a)
                 in D.Skip (InnerLoop s arrContents p arrStart)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p start) | p < start =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p start) = do
        x <- liftIO $ peekByteIndex p contents
        let cur = INDEX_PREV(p,a)
        return $ D.Yield x (InnerLoop st contents cur start)

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !MutableByteArray   -- contents
    {-# UNPACK #-} !Int                -- index 1
    {-# UNPACK #-} !Int                -- index 2

toArrayUnsafe :: MutArray a -> ArrayUnsafe a
toArrayUnsafe (MutArray contents start end _) = ArrayUnsafe contents start end

fromArrayUnsafe ::
#ifdef DEVBUILD
    Unbox a =>
#endif
    ArrayUnsafe a -> MutArray a
fromArrayUnsafe (ArrayUnsafe contents start end) =
         MutArray contents start end end

{-# INLINE_NORMAL producerWith #-}
producerWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> Producer m (MutArray a) a
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
            !x <- liftio $ peekByteIndex cur contents
            return $ D.Yield x (ArrayUnsafe contents (INDEX_NEXT(cur,a)) end)

    extract = return . fromArrayUnsafe

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: forall m a. (MonadIO m, Unbox a) => Producer m (MutArray a) a
producer = producerWith liftIO

-- | Unfold an array into a stream.
--
{-# INLINE_NORMAL reader #-}
reader :: forall m a. (MonadIO m, Unbox a) => Unfold m (MutArray a) a
reader = Producer.simplify producer

{-# INLINE_NORMAL readerRevWith #-}
readerRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> Unfold m (MutArray a) a
readerRevWith liftio = Unfold step inject
    where

    inject (MutArray contents start end _) =
        let p = INDEX_PREV(end,a)
         in return $ ArrayUnsafe contents start p

    {-# INLINE_LATE step #-}
    step (ArrayUnsafe _ start p) | p < start = return D.Stop
    step (ArrayUnsafe contents start p) = do
        !x <- liftio $ peekByteIndex p contents
        return $ D.Yield x (ArrayUnsafe contents start (INDEX_PREV(p,a)))

-- | Unfold an array into a stream in reverse order.
--
{-# INLINE_NORMAL readerRev #-}
readerRev :: forall m a. (MonadIO m, Unbox a) => Unfold m (MutArray a) a
readerRev = readerRevWith liftIO

-------------------------------------------------------------------------------
-- to Lists and streams
-------------------------------------------------------------------------------

{-
-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Unbox a => (a -> b -> b) -> b -> MutArray a -> b
toListFB c n MutArray{..} = go arrStart
    where

    go p | assert (p <= arrEnd) (p == arrEnd) = n
    go p =
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        -- XXX
        let !x = unsafeInlineIO $ do
                    r <- peekByteIndex arrContents p
                    return r
        in c x (go (PTR_NEXT(p,a)))
-}

-- XXX Monadic foldr/build fusion?
-- Reference: https://www.researchgate.net/publication/220676509_Monadic_augment_and_generalised_short_cut_fusion

-- | Convert a 'MutArray' into a list.
--
{-# INLINE toList #-}
toList :: forall m a. (MonadIO m, Unbox a) => MutArray a -> m [a]
toList MutArray{..} = liftIO $ go arrStart
    where

    go p | assert (p <= arrEnd) (p == arrEnd) = return []
    go p = do
        x <- peekByteIndex p arrContents
        (:) x <$> go (INDEX_NEXT(p,a))

{-# INLINE_NORMAL toStreamDWith #-}
toStreamDWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> D.Stream m a
toStreamDWith liftio MutArray{..} = D.Stream step arrStart

    where

    {-# INLINE_LATE step #-}
    step _ p | assert (p <= arrEnd) (p == arrEnd) = return D.Stop
    step _ p = liftio $ do
        r <- peekByteIndex p arrContents
        return $ D.Yield r (INDEX_NEXT(p,a))

-- | Convert a 'MutArray' into a stream.
--
-- >>> read = Stream.unfold MutArray.reader
--
{-# INLINE_NORMAL read #-}
read :: forall m a. (MonadIO m, Unbox a) => MutArray a -> D.Stream m a
read = toStreamDWith liftIO

{-# INLINE toStreamKWith #-}
toStreamKWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> StreamK m a
toStreamKWith liftio MutArray{..} = go arrStart

    where

    go p | assert (p <= arrEnd) (p == arrEnd) = K.nil
         | otherwise =
        let elemM = peekByteIndex p arrContents
        in liftio elemM `K.consM` go (INDEX_NEXT(p,a))

{-# INLINE toStreamK #-}
toStreamK :: forall m a. (MonadIO m, Unbox a) => MutArray a -> StreamK m a
toStreamK = toStreamKWith liftIO

{-# INLINE_NORMAL toStreamDRevWith #-}
toStreamDRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> D.Stream m a
toStreamDRevWith liftio MutArray{..} =
    let p = INDEX_PREV(arrEnd,a)
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p < arrStart = return D.Stop
    step _ p = liftio $ do
        r <- peekByteIndex p arrContents
        return $ D.Yield r (INDEX_PREV(p,a))

-- | Convert a 'MutArray' into a stream in reverse order.
--
-- >>> readRev = Stream.unfold MutArray.readerRev
--
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (MonadIO m, Unbox a) => MutArray a -> D.Stream m a
readRev = toStreamDRevWith liftIO

{-# INLINE toStreamKRevWith #-}
toStreamKRevWith ::
       forall m a. (Monad m, Unbox a)
    => (forall b. IO b -> m b) -> MutArray a -> StreamK m a
toStreamKRevWith liftio MutArray {..} =
    let p = INDEX_PREV(arrEnd,a)
    in go p

    where

    go p | p < arrStart = K.nil
         | otherwise =
        let elemM = peekByteIndex p arrContents
        in liftio elemM `K.consM` go (INDEX_PREV(p,a))

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. (MonadIO m, Unbox a) => MutArray a -> StreamK m a
toStreamKRev = toStreamKRevWith liftIO

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- XXX Need something like "MutArray m a" enforcing monadic action to avoid the
-- possibility of such APIs.
--
-- | Strict left fold of an array.
{-# INLINE_NORMAL foldl' #-}
foldl' :: (MonadIO m, Unbox a) => (b -> a -> b) -> b -> MutArray a -> m b
foldl' f z arr = D.foldl' f z $ read arr

-- | Right fold of an array.
{-# INLINE_NORMAL foldr #-}
foldr :: (MonadIO m, Unbox a) => (a -> b -> b) -> b -> MutArray a -> m b
foldr f z arr = D.foldr f z $ read arr

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- Note: Arrays may be allocated with a specific alignment at the beginning of
-- the array. If you need to maintain that alignment on reallocations then you
-- can resize the array manually before append, using an aligned resize
-- operation.

-- XXX Keep the bound intact to not lose any free space? Perf impact?

-- | @writeAppendNUnsafe n arr@ appends up to @n@ input items to the supplied
-- array.
--
-- Unsafe: Do not drive the fold beyond @n@ elements, it will lead to memory
-- corruption or segfault.
--
-- Any free space left in the array after appending @n@ elements is lost.
--
-- /Internal/
{-# INLINE_NORMAL writeAppendNUnsafe #-}
writeAppendNUnsafe :: forall m a. (MonadIO m, Unbox a) =>
       Int
    -> m (MutArray a)
    -> Fold m a (MutArray a)
writeAppendNUnsafe n action =
    fmap fromArrayUnsafe $ FL.foldlM' step initial

    where

    initial = do
        assert (n >= 0) (return ())
        arr@(MutArray _ _ end bound) <- action
        let free = bound - end
            needed = n * SIZE_OF(a)
        -- XXX We can also reallocate if the array has too much free space,
        -- otherwise we lose that space.
        arr1 <-
            if free < needed
            then noinline reallocWith "writeAppendNUnsafeWith" (+ needed) needed arr
            else return arr
        return $ toArrayUnsafe arr1

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeByteIndex end contents x
        return $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

-- | Append @n@ elements to an existing array. Any free space left in the array
-- after appending @n@ elements is lost.
--
-- >>> writeAppendN n initial = Fold.take n (MutArray.writeAppendNUnsafe n initial)
--
{-# INLINE_NORMAL writeAppendN #-}
writeAppendN :: forall m a. (MonadIO m, Unbox a) =>
    Int -> m (MutArray a) -> Fold m a (MutArray a)
writeAppendN n initial = FL.take n (writeAppendNUnsafe n initial)

-- | @writeAppendWith realloc action@ mutates the array generated by @action@ to
-- append the input stream. If there is no reserved space available in the
-- array it is reallocated to a size in bytes  determined by @realloc oldSize@,
-- where @oldSize@ is the current size of the array in bytes.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> writeAppendWith sizer = Fold.foldlM' (MutArray.snocWith sizer)
--
-- /Pre-release/
{-# INLINE writeAppendWith #-}
writeAppendWith :: forall m a. (MonadIO m, Unbox a) =>
    (Int -> Int) -> m (MutArray a) -> Fold m a (MutArray a)
writeAppendWith sizer = FL.foldlM' (snocWith sizer)

-- | @append action@ mutates the array generated by @action@ to append the
-- input stream. If there is no reserved space available in the array it is
-- reallocated to double the size.
--
-- Note that the returned array may be a mutated version of original array.
--
-- >>> writeAppend = MutArray.writeAppendWith (* 2)
--
{-# INLINE writeAppend #-}
writeAppend :: forall m a. (MonadIO m, Unbox a) =>
    m (MutArray a) -> Fold m a (MutArray a)
writeAppend = writeAppendWith (* 2)

-- XXX We can carry bound as well in the state to make sure we do not lose the
-- remaining capacity. Need to check perf impact.
--
-- | Like 'writeNUnsafe' but takes a new array allocator @alloc size@ function
-- as argument.
--
-- >>> writeNWithUnsafe alloc n = MutArray.writeAppendNUnsafe (alloc n) n
--
-- /Pre-release/
{-# INLINE_NORMAL writeNWithUnsafe #-}
writeNWithUnsafe :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeNWithUnsafe alloc n = fromArrayUnsafe <$> FL.foldlM' step initial

    where

    initial = toArrayUnsafe <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        liftIO $ pokeByteIndex end contents x
        return
          $ ArrayUnsafe contents start (INDEX_NEXT(end,a))

-- | Like 'writeN' but does not check the array bounds when writing. The fold
-- driver must not call the step function more than 'n' times otherwise it will
-- corrupt the memory and crash. This function exists mainly because any
-- conditional in the step function blocks fusion causing 10x performance
-- slowdown.
--
-- >>> writeNUnsafe = MutArray.writeNWithUnsafe MutArray.newPinned
--
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
writeNUnsafe = writeNWithUnsafe newPinned

-- | @writeNWith alloc n@ folds a maximum of @n@ elements into an array
-- allocated using the @alloc@ function.
--
-- >>> writeNWith alloc n = Fold.take n (MutArray.writeNWithUnsafe alloc n)
-- >>> writeNWith alloc n = MutArray.writeAppendN (alloc n) n
--
{-# INLINE_NORMAL writeNWith #-}
writeNWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeNWith alloc n = FL.take n (writeNWithUnsafe alloc n)

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'MutArray'.
--
-- >>> writeN = MutArray.writeNWith MutArray.newPinned
-- >>> writeN n = Fold.take n (MutArray.writeNUnsafe n)
-- >>> writeN n = MutArray.writeAppendN n (MutArray.newPinned n)
--
{-# INLINE_NORMAL writeN #-}
writeN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
writeN = writeNWith newPinned

-- | Like writeNWithUnsafe but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWithUnsafe #-}
writeRevNWithUnsafe :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeRevNWithUnsafe alloc n = fromArrayUnsafe <$> FL.foldlM' step initial

    where

    toArrayUnsafeRev (MutArray contents _ _ bound) =
         ArrayUnsafe contents bound bound

    initial = toArrayUnsafeRev <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        let ptr = INDEX_PREV(start,a)
        liftIO $ pokeByteIndex ptr contents x
        return
          $ ArrayUnsafe contents ptr end

-- | Like writeNWith but writes the array in reverse order.
--
-- /Internal/
{-# INLINE_NORMAL writeRevNWith #-}
writeRevNWith :: forall m a. (MonadIO m, Unbox a)
    => (Int -> m (MutArray a)) -> Int -> Fold m a (MutArray a)
writeRevNWith alloc n = FL.take n (writeRevNWithUnsafe alloc n)

-- | Like writeN but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE_NORMAL writeRevN #-}
writeRevN :: forall m a. (MonadIO m, Unbox a) => Int -> Fold m a (MutArray a)
writeRevN = writeRevNWith newPinned

-- | @writeNAligned align n@ folds a maximum of @n@ elements from the input
-- stream to a 'MutArray' aligned to the given size.
--
-- >>> writeNAligned align = MutArray.writeNWith (MutArray.newAlignedPinned align)
-- >>> writeNAligned align n = MutArray.writeAppendN n (MutArray.newAlignedPinned align n)
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned :: forall m a. (MonadIO m, Unbox a)
    => Int -> Int -> Fold m a (MutArray a)
writeNAligned align = writeNWith (newAlignedPinned align)

-- XXX Buffer to a list instead?
--
-- | Buffer a stream into a stream of arrays.
--
-- >>> writeChunks n = Fold.many (MutArray.writeN n) Fold.toStreamK
--
-- Breaking an array into an array stream  can be useful to consume a large
-- array sequentially such that memory of the array is released incrementatlly.
--
-- See also: 'arrayStreamKFromStreamD'.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL writeChunks #-}
writeChunks :: (MonadIO m, Unbox a) =>
    Int -> Fold m a (StreamK n (MutArray a))
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
-- >>> f n = MutArray.writeAppendWith (* 2) (MutArray.newPinned n)
-- >>> writeWith n = Fold.rmapM MutArray.rightSize (f n)
-- >>> writeWith n = Fold.rmapM MutArray.fromArrayStreamK (MutArray.writeChunks n)
--
-- /Pre-release/
{-# INLINE_NORMAL writeWith #-}
writeWith :: forall m a. (MonadIO m, Unbox a)
    => Int -> Fold m a (MutArray a)
-- writeWith n = FL.rmapM rightSize $ writeAppendWith (* 2) (newPinned n)
writeWith elemCount =
    FL.rmapM extract $ FL.foldlM' step initial

    where

    initial = do
        when (elemCount < 0) $ error "writeWith: elemCount is negative"
        liftIO $ newPinned elemCount

    step arr@(MutArray _ start end bound) x
        | INDEX_NEXT(end,a) > bound = do
        let oldSize = end - start
            newSize = max (oldSize * 2) 1
        arr1 <- liftIO $ reallocExplicit (SIZE_OF(a)) newSize arr
        snocUnsafe arr1 x
    step arr x = snocUnsafe arr x

    extract = liftIO . rightSize

-- | Fold the whole input to a single array.
--
-- Same as 'writeWith' using an initial array size of 'arrayChunkBytes' bytes
-- rounded up to the element size.
--
-- /Caution! Do not use this on infinite streams./
--
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Unbox a) => Fold m a (MutArray a)
write = writeWith (allocBytesToElemCount (undefined :: a) arrayChunkBytes)

-------------------------------------------------------------------------------
-- construct from streams, known size
-------------------------------------------------------------------------------

-- | Use the 'writeN' fold instead.
--
-- >>> fromStreamDN n = Stream.fold (MutArray.writeN n)
--
{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Unbox a)
    => Int -> D.Stream m a -> m (MutArray a)
-- fromStreamDN n = D.fold (writeN n)
fromStreamDN limit str = do
    (arr :: MutArray a) <- liftIO $ newPinned limit
    end <- D.foldlM' (fwrite (arrContents arr)) (return $ arrEnd arr) $ D.take limit str
    return $ arr {arrEnd = end}

    where

    fwrite arrContents ptr x = do
        liftIO $ pokeByteIndex ptr arrContents  x
        return $ INDEX_NEXT(ptr,a)

-- | Create a 'MutArray' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
{-# INLINABLE fromListN #-}
fromListN :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
fromListN n xs = fromStreamDN n $ D.fromList xs

-- | Like fromListN but writes the array in reverse order.
--
-- /Pre-release/
{-# INLINE fromListRevN #-}
fromListRevN :: (MonadIO m, Unbox a) => Int -> [a] -> m (MutArray a)
fromListRevN n xs = D.fold (writeRevN n) $ D.fromList xs

-------------------------------------------------------------------------------
-- convert stream to a single array
-------------------------------------------------------------------------------

{-# INLINE arrayStreamKLength #-}
arrayStreamKLength :: (Monad m, Unbox a) => StreamK m (MutArray a) -> m Int
arrayStreamKLength as = K.foldl' (+) 0 (K.map length as)

-- | Convert an array stream to an array. Note that this requires peak memory
-- that is double the size of the array stream.
--
{-# INLINE fromArrayStreamK #-}
fromArrayStreamK :: (Unbox a, MonadIO m) =>
    StreamK m (MutArray a) -> m (MutArray a)
fromArrayStreamK as = do
    len <- arrayStreamKLength as
    fromStreamDN len $ D.unfoldMany reader $ D.fromStreamK as

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
-- >>> fromStreamD = StreamD.fold MutArray.write
--
{-# INLINE fromStreamD #-}
fromStreamD :: (MonadIO m, Unbox a) => D.Stream m a -> m (MutArray a)
fromStreamD m = arrayStreamKFromStreamD m >>= fromArrayStreamK

-- | Create a 'MutArray' from a list. The list must be of finite size.
--
{-# INLINE fromList #-}
fromList :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
fromList xs = fromStreamD $ D.fromList xs

-- XXX We are materializing the whole list first for getting the length. Check
-- if the 'fromList' like chunked implementation would fare better.

-- | Like 'fromList' but writes the contents of the list in reverse order.
{-# INLINE fromListRev #-}
fromListRev :: (MonadIO m, Unbox a) => [a] -> m (MutArray a)
fromListRev xs = fromListRevN (Prelude.length xs) xs

-------------------------------------------------------------------------------
-- Combining
-------------------------------------------------------------------------------

-- | Put a sub range of a source array into a subrange of a destination array.
-- This is not safe as it does not check the bounds.
{-# INLINE putSliceUnsafe #-}
putSliceUnsafe :: MonadIO m => MutArray a -> Int -> MutArray a -> Int -> Int -> m ()
putSliceUnsafe src srcStartBytes dst dstStartBytes lenBytes = liftIO $ do
    assertM(lenBytes <= arrBound dst - dstStartBytes)
    assertM(lenBytes <= arrEnd src - srcStartBytes)
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
spliceCopy :: forall m a. MonadIO m =>
#ifdef DEVBUILD
    Unbox a =>
#endif
    MutArray a -> MutArray a -> m (MutArray a)
spliceCopy arr1 arr2 = liftIO $ do
    let start1 = arrStart arr1
        start2 = arrStart arr2
        len1 = arrEnd arr1 - start1
        len2 = arrEnd arr2 - start2
    newArrContents <- liftIO $ Unboxed.newPinnedBytes (len1 + len2)
    let len = len1 + len2
        newArr = MutArray newArrContents 0 len len
    putSliceUnsafe arr1 start1 newArr 0 len1
    putSliceUnsafe arr2 start2 newArr len1 len2
    return newArr

-- | Really really unsafe, appends the second array into the first array. If
-- the first array does not have enough space it may cause silent data
-- corruption or if you are lucky a segfault.
{-# INLINE spliceUnsafe #-}
spliceUnsafe :: MonadIO m =>
    MutArray a -> MutArray a -> m (MutArray a)
spliceUnsafe dst src =
    liftIO $ do
         let startSrc = arrStart src
             srcLen = arrEnd src - startSrc
             endDst = arrEnd dst
         assertM(endDst + srcLen <= arrBound dst)
         putSliceUnsafe src startSrc dst endDst srcLen
         return $ dst {arrEnd = endDst + srcLen}

-- | @spliceWith sizer dst src@ mutates @dst@ to append @src@. If there is no
-- reserved space available in @dst@ it is reallocated to a size determined by
-- the @sizer dstBytes srcBytes@ function, where @dstBytes@ is the size of the
-- first array and @srcBytes@ is the size of the second array, in bytes.
--
-- Note that the returned array may be a mutated version of first array.
--
-- /Pre-release/
{-# INLINE spliceWith #-}
spliceWith :: forall m a. (MonadIO m, Unbox a) =>
    (Int -> Int -> Int) -> MutArray a -> MutArray a -> m (MutArray a)
spliceWith sizer dst@(MutArray _ start end bound) src = do
{-
    let f = writeAppendWith (`sizer` byteLength src) (return dst)
     in D.fold f (toStreamD src)
-}
    assert (end <= bound) (return ())
    let srcBytes = arrEnd src - arrStart src

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
-- >>> splice = MutArray.spliceWith (+)
--
-- /Pre-release/
{-# INLINE splice #-}
splice :: (MonadIO m, Unbox a) => MutArray a -> MutArray a -> m (MutArray a)
splice = spliceWith (+)

-- | Like 'append' but the growth of the array is exponential. Whenever a new
-- allocation is required the previous array size is at least doubled.
--
-- This is useful to reduce allocations when folding many arrays together.
--
-- Note that the returned array may be a mutated version of first array.
--
-- >>> spliceExp = MutArray.spliceWith (\l1 l2 -> max (l1 * 2) (l1 + l2))
--
-- /Pre-release/
{-# INLINE spliceExp #-}
spliceExp :: (MonadIO m, Unbox a) => MutArray a -> MutArray a -> m (MutArray a)
spliceExp = spliceWith (\l1 l2 -> max (l1 * 2) (l1 + l2))

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- | Drops the separator byte
{-# INLINE breakOn #-}
breakOn :: MonadIO m
    => Word8 -> MutArray Word8 -> m (MutArray Word8, Maybe (MutArray Word8))
breakOn sep arr@MutArray{..} = asPtrUnsafe arr $ \p -> liftIO $ do
    -- XXX Instead of using asPtrUnsafe (pinning memory) we can pass unlifted
    -- Addr# to memchr and it should be safe (from ghc 8.4).
    -- XXX We do not need memchr here, we can use a Haskell equivalent.
    loc <- c_memchr p sep (fromIntegral $ byteLength arr)
    let sepIndex = loc `minusPtr` p
    return $
        if loc == nullPtr
        then (arr, Nothing)
        else
            ( MutArray
                { arrContents = arrContents
                , arrStart = arrStart
                , arrEnd = arrStart + sepIndex -- exclude the separator
                , arrBound = arrStart + sepIndex
                }
            , Just $ MutArray
                    { arrContents = arrContents
                    , arrStart = arrStart + (sepIndex + 1)
                    , arrEnd = arrEnd
                    , arrBound = arrBound
                    }
            )

-- | Create two slices of an array without copying the original array. The
-- specified index @i@ is the first index of the second slice.
--
splitAt :: forall a. Unbox a => Int -> MutArray a -> (MutArray a, MutArray a)
splitAt i arr@MutArray{..} =
    let maxIndex = length arr - 1
    in  if i < 0
        then error "sliceAt: negative array index"
        else if i > maxIndex
             then error $ "sliceAt: specified array index " ++ show i
                        ++ " is beyond the maximum index " ++ show maxIndex
             else let off = i * SIZE_OF(a)
                      p = arrStart + off
                in ( MutArray
                  { arrContents = arrContents
                  , arrStart = arrStart
                  , arrEnd = p
                  , arrBound = p
                  }
                , MutArray
                  { arrContents = arrContents
                  , arrStart = p
                  , arrEnd = arrEnd
                  , arrBound = arrBound
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
    Unbox b =>
#endif
    MutArray a -> MutArray b
castUnsafe (MutArray contents start end bound) =
    MutArray contents start end bound

-- | Cast an @MutArray a@ into an @MutArray Word8@.
--
asBytes :: MutArray a -> MutArray Word8
asBytes = castUnsafe

-- | Cast an array having elements of type @a@ into an array having elements of
-- type @b@. The length of the array should be a multiple of the size of the
-- target element otherwise 'Nothing' is returned.
--
cast :: forall a b. Unbox b => MutArray a -> Maybe (MutArray b)
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

-- | Use an @MutArray a@ as @Ptr a@. This is useful when we want to pass an array
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
asPtrUnsafe :: MonadIO m => MutArray a -> (Ptr a -> m b) -> m b
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
-- lexicographical ordering of two underlying byte arrays otherwise return the
-- result of length comparison.
--
-- /Pre-release/
{-# INLINE cmp #-}
cmp :: MonadIO m => MutArray a -> MutArray a -> m Ordering
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

-- | Strip elements which match with predicate from both ends.
--
-- /Pre-release/
{-# INLINE strip #-}
strip :: forall a m. (Unbox a, MonadIO m) =>
    (a -> Bool) -> MutArray a -> m (MutArray a)
strip eq arr@MutArray{..} = liftIO $ do
    st <- getStart arrStart
    end <- getLast arrEnd st
    return arr {arrStart = st, arrEnd = end, arrBound = end}

    where

    {-
    -- XXX This should have the same perf but it does not, investigate.
    getStart = do
        r <- liftIO $ D.head $ D.findIndices (not . eq) $ toStreamD arr
        pure $
            case r of
                Nothing -> arrEnd
                Just i -> PTR_INDEX(arrStart,i,a)
    -}

    getStart cur = do
        if cur < arrEnd
        then do
            r <- peekByteIndex cur arrContents
            if eq r
            then getStart (INDEX_NEXT(cur,a))
            else return cur
        else return cur

    getLast cur low = do
        if cur > low
        then do
            let prev = INDEX_PREV(cur,a)
            r <- peekByteIndex prev arrContents
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
bubble :: (MonadIO m, Unbox a) => (a -> a -> Ordering) -> MutArray a -> m ()
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
                        putIndexUnsafe (i + 1) arr x1
                        go x (i - 1)
                    _ -> putIndexUnsafe (i + 1) arr x
            else putIndexUnsafe (i + 1) arr x
