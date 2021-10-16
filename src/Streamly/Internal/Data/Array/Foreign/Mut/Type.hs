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

    -- * Constructing and Writing
    -- ** Construction
    -- , nil

    -- *** Uninitialized Arrays
    , newArray
    , newArrayAligned
    , newArrayAlignedUnmanaged
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
    , writeNAlignedUnmanaged

    , writeWith
    , write

    -- , writeRevN
    -- , writeRev

    -- ** From containers
    , fromForeignPtrUnsafe
    , fromListN
    , fromList
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

    -- ** Truncation
    -- These are not the same as slicing the array at the beginning, they may
    -- reduce the length as well as the capacity of the array.
    , truncateWith
    , truncate
    , truncateExp

    -- * Eliminating and Reading

    -- ** To streams
    , ReadUState
    , read
    , readRev

    -- ** To containers
    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toList

    -- experimental
    , producer

    -- ** Random reads
    , getIndex
    , getIndexUnsafe
    , getIndices
    -- , getFromThenTo
    , getIndexRev

    -- * Memory Management
    , blockSize
    , arrayChunkSize
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
    , reverse
    , permute
    , partitionBy
    , shuffleBy
    , divideBy
    , mergeBy

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
    -- , putSlice
    -- , appendSlice
    -- , appendSliceFrom

    -- * Utilities
    , bytesToElemCount
    , memcpy
    , memcmp
    , c_memchr
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.DeepSeq (NFData(..))
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits ((.&.))
import Data.Functor.Identity (Identity(runIdentity))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr (touchForeignPtr, castForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (build, noinline)
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))

import Streamly.Internal.BaseCompat
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import Streamly.Internal.System.IO
    (defaultChunkSize, mkChunkSize, unsafeInlineIO)

#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Foreign.Malloc as Malloc

import Prelude hiding
    (length, foldr, read, unlines, splitAt, reverse, truncate)

#if MIN_VERSION_base(4,10,0)
import Foreign.ForeignPtr (plusForeignPtr)
#else
import GHC.Base (Int(..), plusAddr#)
import GHC.ForeignPtr (ForeignPtr(..))
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c
#endif

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as Array
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.StreamD as StreamD
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Type as Fold

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- | Given a 'Storable' type (unused first arg) and a number of bytes, return
-- how many elements of that type will fit in after rounding the bytes up to
-- the element size.
{-# INLINE bytesToElemCount #-}
bytesToElemCount :: Storable a => a -> Int -> Int
bytesToElemCount x n =
    let elemSize = sizeOf x
    in n + elemSize - 1 `div` elemSize

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

-- | Same as unsafeWithForeignPtr but lifted to any MonadIO monad.
{-# INLINE unsafeWithForeignPtrM #-}
unsafeWithForeignPtrM :: MonadIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
unsafeWithForeignPtrM fo f = do
  r <- f (unsafeForeignPtrToPtr fo)
  liftIO $ touchForeignPtr fo
  return r

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
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- ^ first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- ^ first unused address
    , aBound :: {-# UNPACK #-} !(Ptr a)        -- ^ first address beyond allocated memory
    }

-- | @fromForeignPtrUnsafe foreignPtr end bound@ creates an 'Array' that starts
-- at the memory pointed by the @foreignPtr@, @end@ is the first unused
-- address, and @bound@ is the first address beyond the allocated memory.
--
-- Unsafe: Make sure that foreignPtr <= end <= bound and (end - start) is an
-- integral multiple of the element size.
--
-- /Pre-release/
--
{-# INLINE fromForeignPtrUnsafe #-}
fromForeignPtrUnsafe ::
#ifdef DEVBUILD
    Storable a =>
#endif
    ForeignPtr a -> Ptr a -> Ptr a -> Array a
fromForeignPtrUnsafe fp end bound =
    assert (unsafeForeignPtrToPtr fp <= end && end <= bound)
           (Array fp end bound)

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
    => (Int -> Int -> m (ForeignPtr a)) -> Int -> Int -> m (Array a)
newArrayWith alloc alignSize count = do
    let size = max (count * sizeOf (undefined :: a)) 0
    fptr <- alloc size alignSize
    let p = unsafeForeignPtrToPtr fptr
    return $ Array
        { aStart = fptr
        , aEnd   = p
        , aBound = p `plusPtr` size
        }

-- | Like 'newArrayWith' but using an allocator that allocates unmanaged pinned
-- memory. The memory will never be freed by GHC.  This could be useful in
-- allocate-once global data structures. Use carefully as incorrect use can
-- lead to memory leak.
--
-- /Internal/
{-# INLINE newArrayAlignedUnmanaged #-}
newArrayAlignedUnmanaged :: (MonadIO m, Storable a) =>
    Int -> Int -> m (Array a)
newArrayAlignedUnmanaged = newArrayWith mallocForeignPtrAlignedUnmanagedBytes

    where

    mallocForeignPtrAlignedUnmanagedBytes size alignSize =
        liftIO $ Malloc.mallocForeignPtrAlignedUnmanagedBytes size alignSize

-- | Like 'newArrayWith' but using an allocator that aligns the memory to the
-- alignment dictated by the 'Storable' instance of the type.
--
-- /Internal/
{-# INLINE newArrayAligned #-}
newArrayAligned :: (MonadIO m, Storable a) => Int -> Int -> m (Array a)
newArrayAligned = newArrayWith mallocForeignPtrAlignedBytes

    where

    mallocForeignPtrAlignedBytes size alignSize =
        liftIO $ Malloc.mallocForeignPtrAlignedBytes size alignSize

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

-- | Allocate an Array of the given size and run an IO action passing the array
-- start pointer.
--
-- /Internal/
{-# INLINE withNewArrayUnsafe #-}
withNewArrayUnsafe ::
       (MonadIO m, Storable a) => Int -> (Ptr a -> m ()) -> m (Array a)
withNewArrayUnsafe count f = do
    arr <- newArray count
    unsafeWithForeignPtrM (aStart arr) $ \p -> f p >> return arr

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Unimplemented/
{-# INLINE putIndices #-}
putIndices :: Array a -> Fold m (Int, a) ()
putIndices = undefined

-- | Write the given element to the given index of the array. Does not check if
-- the index is out of bounds of the array.
--
-- /Pre-release/
{-# INLINE putIndexUnsafe #-}
putIndexUnsafe :: forall m a. (MonadIO m, Storable a)
    => Array a -> Int -> a -> m ()
putIndexUnsafe Array{..} i x =
    unsafeWithForeignPtrM aStart $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = ptr `plusPtr` (elemSize * i)
        assert (i >= 0 && elemPtr `plusPtr` elemSize <= aEnd) (return ())
        liftIO $ poke elemPtr x

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

{-# INLINE putIndexPtr #-}
putIndexPtr :: forall m a. (MonadIO m, Storable a) =>
    Ptr a -> Ptr a -> Int -> a -> m ()
putIndexPtr ptr end i x = do
    let elemSize = sizeOf (undefined :: a)
        elemPtr = ptr `plusPtr` (elemSize * i)
    if i >= 0 && elemPtr `plusPtr` elemSize <= end
    then liftIO $ poke elemPtr x
    else invalidIndex "putIndexPtr" i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex arr ix val = Array.modifyIndex arr ix (const (val, ()))
-- >>> f = Array.putIndices
-- >>> putIndex arr ix val = Stream.fold (f arr) (Stream.fromPure (ix, val))
--
-- /Pre-release/
{-# INLINE putIndex #-}
putIndex :: (MonadIO m, Storable a) => Array a -> Int -> a -> m ()
putIndex arr i x =
    unsafeWithForeignPtrM (aStart arr) $ \p -> putIndexPtr p (aEnd arr) i x

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndexUnsafe :: forall m a b. (MonadIO m, Storable a) =>
    Array a -> Int -> (a -> (a, b)) -> m b
modifyIndexUnsafe Array{..} i f = do
    liftIO $ unsafeWithForeignPtr aStart $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = ptr `plusPtr` (elemSize * i)
        assert (i >= 0 && elemPtr `plusPtr` elemSize <= aEnd) (return ())
        r <- peek elemPtr
        let (x, res) = f r
        poke elemPtr x
        return res

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndex :: forall m a b. (MonadIO m, Storable a) =>
    Array a -> Int -> (a -> (a, b)) -> m b
modifyIndex Array{..} i f = do
    liftIO $ unsafeWithForeignPtr aStart $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = ptr `plusPtr` (elemSize * i)
        if i >= 0 && elemPtr `plusPtr` elemSize <= aEnd
        then do
            r <- peek elemPtr
            let (x, res) = f r
            poke elemPtr x
            return res
        else invalidIndex "modifyIndex" i

-- | Modify the array indices generated by the supplied unfold.
--
-- /Pre-release/
modifyIndices :: -- forall m a b. (MonadIO m, Storable a) =>
    Unfold m (Array a) Int -> Array a -> (a -> a) -> m ()
modifyIndices = undefined

-- | Modify each element of an array using the supplied modifier function.
--
-- /Unimplemented/
modify :: -- forall m a b. (MonadIO m, Storable a) =>
    Array a -> (a -> a) -> m ()
modify = undefined

-- | Swap the elements at two indices.
--
-- /Pre-release/
swapIndices :: -- (MonadIO m, Storable a) =>
    Array a -> Int -> Int -> m ()
swapIndices = undefined

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

{-
roundUpToPower2 :: Int -> Int
roundUpToPower2 = undefined
-}

-- XXX remove mkChunkSize, it is not required.
-- | The default chunk size by which the array creation routines increase the
-- size of the array when the array is grown linearly.
--
{-# INLINE arrayChunkSize #-}
arrayChunkSize :: Int
arrayChunkSize = mkChunkSize 1024

-------------------------------------------------------------------------------
-- Snoc
-------------------------------------------------------------------------------

-- XXX We can possibly use a smallMutableByteArray to hold the start, end,
-- bound pointers. Or we can use the first 16 bytes of the allocation to store
-- the end, bound pointers. Using fully mutable handle will ensure that we do
-- not have multiple references to the same array of different lengths lying
-- around and potentially misused. In that case "snoc" need not return a new
-- array (snoc :: Array a -> a -> m ()), it will just modify the old reference.
-- The array length will be mutable.  This means the length function would also
-- be monadic.  Mutable arrays would behave more like files that grow in that
-- case.

-- | Snoc using a 'Ptr'. Low level reusable function.
--
-- /Internal/
{-# INLINE snocNewEnd #-}
snocNewEnd :: (MonadIO m, Storable a) => Ptr a -> Array a -> a -> m (Array a)
snocNewEnd newEnd arr@Array{..} x = liftIO $ do
    assert (newEnd <= aBound) (return ())
    poke aEnd x
    touchForeignPtr aStart
    return $ arr {aEnd = newEnd}

-- | Really really unsafe, appends the element into the first array, may
-- cause silent data corruption or if you are lucky a segfault if the first
-- array does not have enough space to append the element.
--
-- /Internal/
{-# INLINE snocUnsafe #-}
snocUnsafe :: forall m a. (MonadIO m, Storable a) =>
    Array a -> a -> m (Array a)
snocUnsafe arr@Array{..} =
    snocNewEnd (aEnd `plusPtr` sizeOf (undefined :: a)) arr

-- | Like 'snoc' but does not reallocate when pre-allocated array capacity
-- becomes full, instead it raises an error in that case.
--
-- /Internal/
{-# INLINE snocMay #-}
snocMay :: forall m a. (MonadIO m, Storable a) =>
    Array a -> a -> m (Maybe (Array a))
snocMay arr@Array{..} x = liftIO $ do
    let newEnd = aEnd `plusPtr` sizeOf (undefined :: a)
    if newEnd <= aBound
    then Just <$> snocNewEnd newEnd arr x
    else return Nothing

reallocWith :: forall m a. (MonadIO m , Storable a) =>
       String
    -> (Int -> Int)
    -> Int
    -> Array a
    -> m (Array a)
reallocWith label sizer reqSize arr = do
    let oldSize = aEnd arr `minusPtr` unsafeForeignPtrToPtr (aStart arr)
        newSize = sizer oldSize
        safeSize = max newSize (oldSize + reqSize)
        rounded = roundUpLargeArray safeSize
    assert (newSize >= oldSize + reqSize || error badSize) (return ())
    assert (rounded >= safeSize) (return ())
    realloc rounded arr

    where

    badSize = concat
        [ label
        , ": new array size is less than required size "
        , show reqSize
        , ". Please check the sizing function passed."
        ]

-- NOINLINE to move it out of the way and not pollute the instruction cache.
{-# NOINLINE snocWithRealloc #-}
snocWithRealloc :: forall m a. (MonadIO m, Storable a) =>
       (Int -> Int)
    -> Array a
    -> a
    -> m (Array a)
snocWithRealloc sizer arr x = do
    let elemSize = sizeOf (undefined :: a)
    arr1 <- liftIO $ reallocWith "snocWith" sizer elemSize arr
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
    let newEnd = aEnd arr `plusPtr` sizeOf (undefined :: a)
    if newEnd <= aBound arr
    then snocNewEnd newEnd arr x
    else snocWithRealloc allocSize arr x

-- | The array is mutated to append an additional element to it. If there
-- is no reserved space available in the array then it is reallocated to grow
-- it by 'arrayChunkSize' rounded up to 'blockSize' when the size becomes more
-- than 'largeObjectThreshold'.
--
-- Note that the returned array may be a mutated version of the original array.
--
-- Performs O(n^2) copies to grow but is thrifty on memory.
--
-- /Pre-release/
{-# INLINE snocLinear #-}
snocLinear :: forall m a. (MonadIO m, Storable a) => Array a -> a -> m (Array a)
snocLinear = snocWith (+ arrayChunkSize)

-- XXX round it to next power of 2.
--
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
snoc = snocWith (* 2)

-------------------------------------------------------------------------------
-- Resizing
-------------------------------------------------------------------------------

-- | Reallocate the array to the specified size in bytes. If the size is less
-- than the original array the array gets truncated.
{-# NOINLINE reallocAligned #-}
reallocAligned :: Int -> Int -> Int -> Array a -> IO (Array a)
reallocAligned elemSize alignSize newSize Array{..} = do
    assert (aEnd <= aBound) (return ())
    let oldStart = unsafeForeignPtrToPtr aStart
        oldSize = aEnd `minusPtr` oldStart
    assert (oldSize `mod` elemSize == 0) (return ())
    newPtr <- Malloc.mallocForeignPtrAlignedBytes newSize alignSize
    unsafeWithForeignPtr newPtr $ \pNew -> do
        let size = min oldSize newSize
        assert (size >= 0) (return ())
        assert (size `mod` elemSize == 0) (return ())
        memcpy (castPtr pNew) (castPtr oldStart) size
        touchForeignPtr aStart
        return $ Array
            { aStart = newPtr
            , aEnd   = pNew `plusPtr` (size - (size `mod` elemSize))
            , aBound = pNew `plusPtr` newSize
            }

{-# INLINABLE realloc #-}
realloc :: forall m a. (MonadIO m, Storable a) => Int -> Array a -> m (Array a)
realloc i arr =
    liftIO
        $ reallocAligned
            (sizeOf (undefined :: a)) (alignment (undefined :: a)) i arr

-- | Change the reserved memory of the array so that it is enough to hold the
-- specified number of elements.  Nothing is done if the specified capacity is
-- less than the length of the array.
--
-- If the capacity is more than 'largeObjectThreshold' then it is rounded up to
-- the block size (4K).
--
-- /Unimplemented/
{-# INLINE resize #-}
resize :: -- (MonadIO m, Storable a) =>
    Int -> Array a -> m (Array a)
resize = undefined

-- | Like 'resize' but if the capacity is more than 'largeObjectThreshold' then
-- it is rounded up to the closest power of 2.
--
-- /Unimplemented/
{-# INLINE resizeExp #-}
resizeExp :: -- (MonadIO m, Storable a) =>
    Int -> Array a -> m (Array a)
resizeExp = undefined

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
    let start = unsafeForeignPtrToPtr aStart
        len = aEnd `minusPtr` start
        capacity = aBound `minusPtr` start
        target = roundUpLargeArray len
        waste = aBound `minusPtr` aEnd
    assert (target >= len) (return ())
    assert (len `mod` sizeOf (undefined :: a) == 0) (return ())
    -- We trade off some wastage (25%) to avoid reallocations and copying.
    if target < capacity && len < 3 * waste
    then realloc target arr
    else return arr

-------------------------------------------------------------------------------
-- Reducing the length
-------------------------------------------------------------------------------

-- | Drop the last n elements of the array to reduce the length by n. The
-- capacity is reallocated using the user supplied function.
--
-- /Unimplemented/
{-# INLINE truncateWith #-}
truncateWith :: -- (MonadIO m, Storable a) =>
    Int -> (Int -> Int) -> Array a -> m (Array a)
truncateWith = undefined

-- | Drop the last n elements of the array to reduce the length by n.
--
-- The capacity is rounded to 1K or 4K if the length is more than the GHC large
-- block threshold.
--
-- /Unimplemented/
{-# INLINE truncate #-}
truncate :: -- (MonadIO m, Storable a) =>
    Int -> Array a -> m (Array a)
truncate = undefined

-- | Like 'truncate' but the capacity is rounded to the closest power of 2.
--
-- /Unimplemented/
{-# INLINE truncateExp #-}
truncateExp :: -- (MonadIO m, Storable a) =>
    Int -> Array a -> m (Array a)
truncateExp = undefined

-------------------------------------------------------------------------------
-- Random reads
-------------------------------------------------------------------------------

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe :: forall m a. (MonadIO m, Storable a) => Array a -> Int -> m a
getIndexUnsafe Array {..} i =
    unsafeWithForeignPtrM aStart $ \ptr -> do
        let elemSize = sizeOf (undefined :: a)
            elemPtr = ptr `plusPtr` (elemSize * i)
        assert (i >= 0 && elemPtr `plusPtr` elemSize <= aEnd) (return ())
        liftIO $ peek elemPtr

{-# INLINE getIndexPtr #-}
getIndexPtr :: forall m a. (MonadIO m, Storable a) =>
    Ptr a -> Ptr a -> Int -> m a
getIndexPtr ptr end i = do
    let elemSize = sizeOf (undefined :: a)
        elemPtr = ptr `plusPtr` (elemSize * i)
    if i >= 0 && elemPtr `plusPtr` elemSize <= end
    then liftIO $ peek elemPtr
    else invalidIndex "getIndexPtr" i

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: (MonadIO m, Storable a) => Array a -> Int -> m a
getIndex arr i =
    unsafeWithForeignPtrM (aStart arr) $ \p -> getIndexPtr p (aEnd arr) i

{-# INLINE getIndexPtrRev #-}
getIndexPtrRev :: forall m a. (MonadIO m, Storable a) =>
    Ptr a -> Ptr a -> Int -> m a
getIndexPtrRev ptr end i = do
    let elemSize = sizeOf (undefined :: a)
        elemPtr = end `plusPtr` negate (elemSize * (i + 1))
    if i >= 0 && elemPtr >= ptr
    then liftIO $ peek elemPtr
    else invalidIndex "getIndexPtrRev" i

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: (MonadIO m, Storable a) => Array a -> Int -> m a
getIndexRev arr i =
    unsafeWithForeignPtrM (aStart arr) $ \p -> getIndexPtrRev p (aEnd arr) i

data GetIndicesState fp end st = GetIndicesState fp end st

-- | Given an unfold that generates array indices, read the elements on those
-- indices from the supplied Array. An error is thrown if an index is out of
-- bounds.
--
-- /Pre-release/
{-# INLINE getIndices #-}
getIndices :: (MonadIO m, Storable a) =>
    Unfold m (Array a) Int -> Unfold m (Array a) a
getIndices (Unfold stepi injecti) = Unfold step inject

    where

    inject arr@(Array fp@(ForeignPtr _ _) (Ptr end) _) = do
        st <- injecti arr
        return $ GetIndicesState fp (Ptr end) st

    {-# INLINE_LATE step #-}
    step (GetIndicesState fp@(ForeignPtr start _) end st) = do
        r <- stepi st
        case r of
            D.Yield i s -> do
                x <- liftIO $ getIndexPtr (Ptr start) end i
                return $ D.Yield x (GetIndicesState fp end s)
            D.Skip s -> return $ D.Skip (GetIndicesState fp end s)
            D.Stop -> do
                liftIO $ touchForeignPtr fp
                return D.Stop

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
getSliceUnsafe index len (Array fp e _) =
    let size = sizeOf (undefined :: a)
        fp1 = fp `plusForeignPtr` (index * size)
        end = unsafeForeignPtrToPtr fp1 `plusPtr` (len * size)
     in assert (index >= 0 && len >= 0 && end <= e) (Array fp1 end end)

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
getSlice index len (Array fp e _) =
    let size = sizeOf (undefined :: a)
        fp1 = fp `plusForeignPtr` (index * size)
        end = unsafeForeignPtrToPtr fp1 `plusPtr` (len * size)
     in if index >= 0 && len >= 0 && end <= e
        then Array fp1 end end
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
-- /Unimplemented/
{-# INLINE reverse #-}
reverse :: Array a -> m Bool
reverse = undefined

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
-- /Unimplemented/
{-# INLINE partitionBy #-}
partitionBy :: (a -> Bool) -> Array a -> m (Array a, Array a)
partitionBy = undefined

-- | Shuffle corresponding elements from two arrays using a shuffle function.
-- If the shuffle function returns 'False' then do nothing otherwise swap the
-- elements. This can be used in a bottom up fold to shuffle or reorder the
-- elements.
--
-- /Unimplemented/
{-# INLINE shuffleBy #-}
shuffleBy :: (a -> a -> m Bool) -> Array a -> Array a -> m (Array a)
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
    Int -> (Array a -> Array a -> m (Array a)) -> Array a -> m (Array a)
divideBy = undefined

-- | @mergeBy level merge array@ performs a pairwise bottom up fold recursively
-- merging the pairs using the supplied merge function. Level indicates the
-- level in the tree where the fold would stop.
--
-- This performs a random shuffle if the shuffle function is random.  If we
-- stop at level 0 and repeatedly apply the function then we can do a bubble
-- sort.
--
-- /Unimplemented/
mergeBy :: Int -> (Array a -> Array a -> m (Array a)) -> Array a -> m (Array a)
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
    let p = unsafeForeignPtrToPtr aStart
        len = aEnd `minusPtr` p
    in assert (len >= 0) len

-- Note: try to avoid the use of length in performance sensitive internal
-- routines as it involves a costly 'div' operation. Instead use the end ptr
-- int he array to check the bounds etc.
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
    let elemSize = sizeOf (undefined :: a)
        blen = byteLength arr
     in assert (blen `mod` elemSize == 0) (blen `div` elemSize)

-- | Get the total capacity of an array. An array may have space reserved
-- beyond the current used length of the array.
--
-- /Pre-release/
{-# INLINE byteCapacity #-}
byteCapacity :: Array a -> Int
byteCapacity Array{..} =
    let p = unsafeForeignPtrToPtr aStart
        len = aBound `minusPtr` p
    in assert (len >= 0) len

-- | The remaining capacity in the array for appending more elements without
-- reallocation.
--
-- /Pre-release/
{-# INLINE bytesFree #-}
bytesFree :: Array a -> Int
bytesFree Array{..} =
    let n = aBound `minusPtr` aEnd
    in assert (n >= 0) n

-------------------------------------------------------------------------------
-- Streams of arrays - Creation
-------------------------------------------------------------------------------

data GroupState s start end bound
    = GroupStart s
    | GroupBuffer s start end bound
    | GroupYield start end bound (GroupState s start end bound)
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
        Array start end bound <- liftIO $ newArray n
        return $ D.Skip (GroupBuffer st start end bound)

    step' gst (GroupBuffer st start end bound) = do
        r <- step (adaptState gst) st
        case r of
            D.Yield x s -> do
                liftIO $ poke end x
                let end' = end `plusPtr` sizeOf (undefined :: a)
                return $
                    if end' >= bound
                    then D.Skip (GroupYield start end' bound (GroupStart s))
                    else D.Skip (GroupBuffer s start end' bound)
            D.Skip s -> return $ D.Skip (GroupBuffer s start end bound)
            D.Stop -> return $ D.Skip (GroupYield start end bound GroupFinish)

    step' _ (GroupYield start end bound next) =
        return $ D.Yield (Array start end bound) next

    step' _ GroupFinish = return D.Stop

-- XXX buffer to a list instead?
-- | Buffer the stream into arrays in memory.
{-# INLINE arrayStreamKFromStreamD #-}
arrayStreamKFromStreamD :: (MonadIO m, Storable a) =>
    D.Stream m a -> m (K.Stream m (Array a))
arrayStreamKFromStreamD m = D.foldr K.cons K.nil $ arraysOf defaultChunkSize m

-------------------------------------------------------------------------------
-- Streams of arrays - Flattening
-------------------------------------------------------------------------------

data FlattenState s a =
      OuterLoop s
    | InnerLoop s !(ForeignPtr a) !(Ptr a) !(Ptr a)

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
                let p = unsafeForeignPtrToPtr aStart
                in D.Skip (InnerLoop s aStart p aEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p end) | assert (p <= end) (p == end) =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st startf p end) = do
        x <- liftIO $ do
                    r <- peek p
                    touchForeignPtr startf
                    return r
        return $ D.Yield x (InnerLoop st startf
                            (p `plusPtr` sizeOf (undefined :: a)) end)

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
                let p = aEnd `plusPtr` negate (sizeOf (undefined :: a))
                -- XXX we do not need aEnd
                in D.Skip (InnerLoop s aStart p aEnd)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st start p _) | p < unsafeForeignPtrToPtr start =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st startf p end) = do
        x <- liftIO $ do
                    r <- peek p
                    touchForeignPtr startf
                    return r
        return $ D.Yield x (InnerLoop st startf
                            (p `plusPtr` negate (sizeOf (undefined :: a))) end)

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

data ReadUState a = ReadUState
    {-# UNPACK #-} !(ForeignPtr a)  -- foreign ptr with end of array pointer
    {-# UNPACK #-} !(Ptr a)         -- current pointer

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: forall m a. (Monad m, Storable a) => Producer m (Array a) a
producer = Producer step inject extract
    where

    inject (Array (ForeignPtr start contents) (Ptr end) _) =
        return $ ReadUState (ForeignPtr end contents) (Ptr start)

    {-# INLINE_LATE step #-}
    step (ReadUState fp@(ForeignPtr end _) p)
        | assert (p <= Ptr end) (p == Ptr end) =
            let x = unsafeInlineIO $ touchForeignPtr fp
            in x `seq` return D.Stop
    step (ReadUState fp p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ peek p
            return $ D.Yield x
                (ReadUState fp (p `plusPtr` sizeOf (undefined :: a)))

    extract (ReadUState (ForeignPtr end contents) (Ptr p)) =
        return $ Array (ForeignPtr p contents) (Ptr end) (Ptr end)

-- | Unfold an array into a stream.
--
-- @since 0.7.0
{-# INLINE_NORMAL read #-}
read :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
read = Producer.simplify producer

-- | Unfold an array into a stream in reverse order.
--
-- /Pre-release/
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (Monad m, Storable a) => Unfold m (Array a) a
readRev = Unfold step inject
    where

    inject (Array fp end _) =
        let p = end `plusPtr` negate (sizeOf (undefined :: a))
         in return $ ReadUState fp p

    {-# INLINE_LATE step #-}
    step (ReadUState fp@(ForeignPtr start _) p) | p < Ptr start =
        let x = unsafeInlineIO $ touchForeignPtr fp
        in x `seq` return D.Stop
    step (ReadUState fp p) = do
            -- unsafeInlineIO allows us to run this in Identity monad for pure
            -- toList/foldr case which makes them much faster due to not
            -- accumulating the list and fusing better with the pure consumers.
            --
            -- This should be safe as the array contents are guaranteed to be
            -- evaluated/written to before we peek at them.
            let !x = unsafeInlineIO $ peek p
            return $ D.Yield x
                (ReadUState fp (p `plusPtr` negate (sizeOf (undefined :: a))))

-------------------------------------------------------------------------------
-- to Lists and streams
-------------------------------------------------------------------------------

-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Storable a => (a -> b -> b) -> b -> Array a -> b
toListFB c n Array{..} = go (unsafeForeignPtrToPtr aStart)
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
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        in c x (go (p `plusPtr` sizeOf (undefined :: a)))

-- XXX Should be monadic
-- | Convert an 'Array' into a list.
--
-- @since 0.7.0
{-# INLINE toList #-}
toList :: Storable a => Array a -> [a]
toList s = build (\c n -> toListFB c n s)

-- | Use the 'read' unfold instead.
--
-- @toStreamD = D.unfold read@
--
-- We can try this if the unfold has any performance issues.
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamD Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | assert (p <= aEnd) (p == aEnd) = return D.Stop
    step _ p = do
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        --
        -- XXX This may not be safe for mutable arrays.
        --
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` sizeOf (undefined :: a))

{-# INLINE toStreamK #-}
toStreamK :: forall m a. Storable a => Array a -> K.Stream m a
toStreamK Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in go p

    where

    go p | assert (p <= aEnd) (p == aEnd) = K.nil
         | otherwise =
        -- See Note in toStreamD.
        -- XXX May not be safe for mutable arrays.
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        in x `K.cons` go (p `plusPtr` sizeOf (undefined :: a))

-- | Use the 'readRev' unfold instead.
--
-- @toStreamDRev = D.unfold readRev@
--
-- We can try this if the unfold has any perf issues.
{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamDRev Array{..} =
    let p = aEnd `plusPtr` negate (sizeOf (undefined :: a))
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p < unsafeForeignPtrToPtr aStart = return D.Stop
    step _ p = do
        -- See comments in toStreamD for why we use unsafeInlineIO
        -- XXX
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` negate (sizeOf (undefined :: a)))

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. Storable a => Array a -> K.Stream m a
toStreamKRev Array {..} =
    let p = aEnd `plusPtr` negate (sizeOf (undefined :: a))
    in go p

    where

    go p | p < unsafeForeignPtrToPtr aStart = K.nil
         | otherwise =
         -- XXX
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        in x `K.cons` go (p `plusPtr` negate (sizeOf (undefined :: a)))

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- XXX Need something like "Array m a" enforcing monadic action to avoid the
-- possibility of such APIs.
--
-- | Strict left fold of an array.
{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
-- XXX Should be monadic
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

-- | Right fold of an array.
{-# INLINE_NORMAL foldr #-}
-- XXX Should be monadic
foldr :: Storable a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !(ForeignPtr a) -- first address
    {-# UNPACK #-} !(Ptr a)        -- first unused address

-- Note: Arrays may be allocated with a specific alignment at the beginning of
-- the array. If you need to maintain that alignment on reallocations then you
-- can resize the array manually before append, using an aligned resize
-- operation.

-- XXX Keep the bound intact to not lose any free space? Perf impact?

-- | Append up to @n@ input items to the supplied array.
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
    fmap extract $ FL.foldlM' step initial

    where

    initial = do
        assert (n >= 0) (return ())
        arr@(Array _ end bound) <- action
        let free = bound `minusPtr` end
            elemSize = sizeOf (undefined :: a)
            needed = n * elemSize
        -- XXX We can also reallocate if the array has too much free space,
        -- otherwise we lose that space.
        (Array start1 end1 _) <-
            if free < needed
            then noinline reallocWith "appendNUnsafeWith" (+ needed) needed arr
            else return arr
        return $ ArrayUnsafe start1 end1

    step (ArrayUnsafe start end) x = do
        liftIO $ poke end x
        return $ ArrayUnsafe start (end `plusPtr` sizeOf (undefined :: a))

    extract (ArrayUnsafe start end) = Array start end end

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

-- | @writeNAlignedUnmanaged align n@ folds a maximum of @n@ elements from the
-- input stream to an 'Array' whose starting address is aligned to @align@
-- bytes and is allocated using unmanaged memory (never freed).  This could be
-- useful to allocate memory that we need to allocate only once in the lifetime
-- of the program.
--
-- >>> f = Array.newArrayAlignedUnmanaged
-- >>> writeNAlignedUnmanaged a = Array.writeNWith (f a)
-- >>> writeNAlignedUnmanaged a n = Array.appendN (f a n) n
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAlignedUnmanaged #-}
writeNAlignedUnmanaged :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAlignedUnmanaged align = writeNWith (newArrayAlignedUnmanaged align)

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
writeNWithUnsafe alloc n = Fold step initial extract

    where

    initial = do
        (Array start end _) <- alloc (max n 0)
        return $ FL.Partial $ ArrayUnsafe start end

    step (ArrayUnsafe start end) x = do
        liftIO $ poke end x
        return
          $ FL.Partial
          $ ArrayUnsafe start (end `plusPtr` sizeOf (undefined :: a))

    extract (ArrayUnsafe start end) = return $ Array start end end -- liftIO . shrinkToFit

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
writeWith n = FL.rmapM rightSize $ appendWith (* 2) (newArray n)

-- | Fold the whole input to a single array.
--
-- Same as 'writeWith' using an initial array size of 'arrayChunkSize' bytes
-- rounded up to the element size.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
write = writeWith (bytesToElemCount (undefined :: a) arrayChunkSize)

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
    end <- D.foldlM' fwrite (return $ aEnd arr) $ D.take limit str
    return $ arr {aEnd = end}

    where

    fwrite ptr x = do
        liftIO $ poke ptr x
        return $ ptr `plusPtr` sizeOf (undefined :: a)

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
-- @since 0.7.0
{-# INLINABLE fromListN #-}
fromListN :: (MonadIO m, Storable a) => Int -> [a] -> m (Array a)
fromListN n xs = fromStreamDN n $ D.fromList xs

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
{-# INLINABLE fromList #-}
fromList :: (MonadIO m, Storable a) => [a] -> m (Array a)
fromList xs = fromStreamD $ D.fromList xs

-------------------------------------------------------------------------------
-- Combining
-------------------------------------------------------------------------------

-- | Copy two arrays into a newly allocated array.
{-# INLINE spliceCopy #-}
spliceCopy :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
spliceCopy arr1 arr2 = do
    let src1 = unsafeForeignPtrToPtr (aStart arr1)
        src2 = unsafeForeignPtrToPtr (aStart arr2)
        len1 = aEnd arr1 `minusPtr` src1
        len2 = aEnd arr2 `minusPtr` src2

    arr <- liftIO $ newArray (len1 + len2)
    let dst = unsafeForeignPtrToPtr (aStart arr)

    -- XXX Should we use copyMutableByteArray# instead? Is there an overhead to
    -- ccall?
    liftIO $ do
        memcpy (castPtr dst) (castPtr src1) len1
        touchForeignPtr (aStart arr1)
        memcpy (castPtr (dst `plusPtr` len1)) (castPtr src2) len2
        touchForeignPtr (aStart arr2)
    return arr { aEnd = dst `plusPtr` (len1 + len2) }

-- | @spliceWith sizer dst src@ mutates @dst@ to append @src@. If there is no
-- reserved space available in @dst@ it is reallocated to a size determined by
-- the @sizer dstBytesn srcBytes@ function, where @dstBytes@ is the size of the
-- first array and @srcBytes@ is the size of the second array, in bytes.
--
-- Note that the returned array may be a mutated version of first array.
--
-- /Pre-release/
{-# INLINE spliceWith #-}
spliceWith :: forall m a. (MonadIO m, Storable a) =>
    (Int -> Int -> Int) -> Array a -> Array a -> m (Array a)
spliceWith sizer dst src =
    let f = appendWith (`sizer` byteLength src) (return dst)
     in D.fold f (toStreamD src)

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
breakOn sep arr@Array{..} = liftIO $ do
    let p = unsafeForeignPtrToPtr aStart
    loc <- c_memchr p sep (fromIntegral $ aEnd `minusPtr` p)
    return $
        if loc == nullPtr
        then (arr, Nothing)
        else
            ( Array
                { aStart = aStart
                , aEnd = loc
                , aBound = loc
                }
            , Just $ Array
                    { aStart = aStart `plusForeignPtr` (loc `minusPtr` p + 1)
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
             else let off = i * sizeOf (undefined :: a)
                      p = unsafeForeignPtrToPtr aStart `plusPtr` off
                in ( Array
                  { aStart = aStart
                  , aEnd = p
                  , aBound = p
                  }
                , Array
                  { aStart = aStart `plusForeignPtr` off
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
castUnsafe (Array start end bound) =
    Array (castForeignPtr start) (castPtr end) (castPtr bound)

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
        r = len `mod` sizeOf (undefined :: b)
     in if r /= 0
        then Nothing
        else Just $ castUnsafe arr

-- | Use an @Array a@ as @Ptr b@.
--
-- /Unsafe/
--
-- /Pre-release/
--
asPtrUnsafe :: Array a -> (Ptr b -> IO c) -> IO c
asPtrUnsafe Array{..} act = do
    unsafeWithForeignPtr aStart $ \ptr -> act (castPtr ptr)

-------------------------------------------------------------------------------
-- Equality
-------------------------------------------------------------------------------

-- | Compare if two arrays are equal.
--
-- /Pre-release/
{-# INLINE cmp #-}
cmp :: MonadIO m => Array a -> Array a -> m Bool
cmp arr1 arr2 =
    liftIO $ do
        let ptr1 = unsafeForeignPtrToPtr $ aStart arr1
        let ptr2 = unsafeForeignPtrToPtr $ aStart arr2
        let len1 = aEnd arr1 `minusPtr` ptr1
        let len2 = aEnd arr2 `minusPtr` ptr2

        if len1 == len2
        then
            if ptr1 == ptr2
            then return True
            else do
                r <- memcmp (castPtr ptr1) (castPtr ptr2) len1
                touchForeignPtr $ aStart arr1
                touchForeignPtr $ aStart arr2
                return r
        else return False

-------------------------------------------------------------------------------
-- NFData
-------------------------------------------------------------------------------

-- This is a Storable array, we cannot have unevaluated data in it so this is
-- just a no op.
instance NFData (Array a) where
    {-# INLINE rnf #-}
    rnf Array {} = ()
