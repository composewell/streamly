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
    , arrayToFptrContents
    , fptrToArrayContents
    , nilArrayContents
    , touch

    -- * Constructing and Writing
    -- ** Construction
    , nil

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
    , ReadUState(..)
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
    , roundUpToPower2
    , memcpy
    , memcmp
    , c_memchr
    )
where

#include "inline.hs"
#include "ArrayMacros.h"
#include "MachDeps.h"

#ifdef USE_C_MALLOC
#define USE_FOREIGN_PTR
#endif

import Control.Exception (assert)
import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..))
#endif
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, (.|.), (.&.))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#ifndef USE_FOREIGN_PTR
import Foreign.Marshal.Alloc (mallocBytes)
#endif
import Foreign.Ptr (plusPtr, minusPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Base
    ( touch#, IO(..), byteArrayContents#
    , Int(..), newAlignedPinnedByteArray#
    )
#ifndef USE_FOREIGN_PTR
import GHC.Base (RealWorld, MutableByteArray#)
#endif
#if __GLASGOW_HASKELL__ < 802
#define noinline
#else
import GHC.Base (noinline)
#endif
import GHC.Exts (unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..))
#ifdef USE_C_MALLOC
import GHC.ForeignPtr (mallocForeignPtrAlignedBytes)
#endif
import GHC.Ptr (Ptr(..))

import Streamly.Internal.BaseCompat
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Producer.Type (Producer (..))
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
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
#ifdef USE_FOREIGN_PTR
import qualified Streamly.Internal.Foreign.Malloc as Malloc
#endif

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
-- Array Contents
-------------------------------------------------------------------------------

-- We support using ForeignPtrContents or MutableByteArray.

#ifdef USE_FOREIGN_PTR
newtype ArrayContents = ArrayContents ForeignPtrContents
#define UNPACKIF
#else
-- XXX can use UnliftedNewtypes
data ArrayContents = ArrayContents !(MutableByteArray# RealWorld)
#define UNPACKIF {-# UNPACK #-}
#endif

{-# INLINE touch #-}
touch :: ArrayContents -> IO ()
touch (ArrayContents contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

fptrToArrayContents :: ForeignPtrContents -> ArrayContents
arrayToFptrContents :: ArrayContents -> ForeignPtrContents
#ifdef USE_FOREIGN_PTR
fptrToArrayContents = ArrayContents
arrayToFptrContents (ArrayContents contents) = contents
#else
fptrToArrayContents (PlainPtr mbarr) = ArrayContents mbarr
fptrToArrayContents _ = error "Unsupported foreign ptr"
arrayToFptrContents (ArrayContents contents) = PlainPtr contents
#endif

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
    { arrContents :: UNPACKIF !ArrayContents
    , arrStart :: {-# UNPACK #-} !(Ptr a)      -- ^ first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- ^ first unused address
    , aBound :: {-# UNPACK #-} !(Ptr a)        -- ^ first address beyond allocated memory
    }

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
    => (Int -> Int -> m (ArrayContents, Ptr a)) -> Int -> Int -> m (Array a)
newArrayWith alloc alignSize count = do
    let size = max (count * SIZE_OF(a)) 0
    (contents, p) <- alloc size alignSize
    return $ Array
        { arrContents = contents
        , arrStart = p
        , aEnd   = p
        , aBound = p `plusPtr` size
        }

newAlignedArrayContents :: Int -> Int -> IO (ArrayContents, Ptr a)
#ifdef USE_C_MALLOC
newAlignedArrayContents size align = do
    (ForeignPtr addr contents) <- mallocForeignPtrAlignedBytes size align
    return (ArrayContents contents, Ptr addr)
#else
newAlignedArrayContents size _align | size < 0 =
  errorWithoutStackTrace "newAlignedArrayContents: size must be >= 0"
newAlignedArrayContents (I# size) (I# align) = IO $ \s ->
    case newAlignedPinnedByteArray# size align s of
        (# s', mbarr# #) ->
           let p = Ptr (byteArrayContents# (unsafeCoerce# mbarr#))
#ifdef USE_FOREIGN_PTR
               c = ArrayContents (PlainPtr mbarr#)
#else
               c = ArrayContents mbarr#
#endif
            in (# s', (c, p) #)
#endif

{-# NOINLINE nilArrayContents #-}
nilArrayContents :: ArrayContents
nilArrayContents =
    fst $ unsafePerformIO $ newAlignedArrayContents 0 0

nil ::
#ifdef DEVBUILD
    Storable a =>
#endif
    Array a
nil = Array nilArrayContents nullPtr nullPtr nullPtr

-- | @fromForeignPtrUnsafe foreignPtr end bound@ creates an 'Array' that starts
-- at the memory pointed by the @foreignPtr@, @end@ is the first unused
-- address, and @bound@ is the first address beyond the allocated memory.
--
-- Unsafe: Make sure that foreignPtr <= end <= bound and (end - start) is an
-- integral multiple of the element size. Only PlainPtr type ForeignPtr is
-- supported.
--
-- /Pre-release/
--
{-# INLINE fromForeignPtrUnsafe #-}
fromForeignPtrUnsafe ::
#ifdef DEVBUILD
    Storable a =>
#endif
    ForeignPtr a -> Ptr a -> Ptr a -> Array a
fromForeignPtrUnsafe (ForeignPtr start _) _ _
    | Ptr start == nullPtr = nil
fromForeignPtrUnsafe fp@(ForeignPtr start contents) end bound =
    assert (unsafeForeignPtrToPtr fp <= end && end <= bound)
           (Array (fptrToArrayContents contents) (Ptr start) end bound)

-- | Like 'newArrayWith' but using an allocator that allocates unmanaged pinned
-- memory. The memory will never be freed by GHC.  This could be useful in
-- allocate-once global data structures. Use carefully as incorrect use can
-- lead to memory leak.
--
-- /Internal/
{-# INLINE newArrayAlignedUnmanaged #-}
newArrayAlignedUnmanaged :: forall m a. (MonadIO m, Storable a) =>
    Int -> Int -> m (Array a)
#ifdef USE_FOREIGN_PTR
newArrayAlignedUnmanaged = do
    newArrayWith mallocForeignPtrAlignedUnmanagedBytes

    where

    mallocForeignPtrAlignedUnmanagedBytes size align = do
        ForeignPtr addr contents <-
            liftIO $ Malloc.mallocForeignPtrAlignedUnmanagedBytes size align
        return (ArrayContents contents, Ptr addr)
#else
newArrayAlignedUnmanaged _align count = do
    let size = max (count * SIZE_OF(a)) 0
    p <- liftIO $ mallocBytes size
    return $ Array
        { arrContents = nilArrayContents
        , arrStart = p
        , aEnd = p
        , aBound = p `plusPtr` size
        }
#endif

-- | Like 'newArrayWith' but using an allocator that aligns the memory to the
-- alignment dictated by the 'Storable' instance of the type.
--
-- /Internal/
{-# INLINE newArrayAligned #-}
newArrayAligned :: (MonadIO m, Storable a) => Int -> Int -> m (Array a)
newArrayAligned = newArrayWith (\s a -> liftIO $ newAlignedArrayContents s a)

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
putIndexUnsafe i x arr@(Array{..}) =
    asPtrUnsafe arr $ \ptr -> do
        let elemPtr = PTR_INDEX(ptr,i,a)
        assert (i >= 0 && PTR_VALID(elemPtr,aEnd,a)) (return ())
        liftIO $ poke elemPtr x

invalidIndex :: String -> Int -> a
invalidIndex label i =
    error $ label ++ ": invalid array index " ++ show i

{-# INLINE putIndexPtr #-}
putIndexPtr :: forall m a. (MonadIO m, Storable a) =>
    Ptr a -> Ptr a -> Int -> a -> m ()
putIndexPtr start end i x = do
    let elemPtr = PTR_INDEX(start,i,a)
    if i >= 0 && PTR_VALID(elemPtr,end,a)
    then liftIO $ poke elemPtr x
    else invalidIndex "putIndexPtr" i

-- | /O(1)/ Write the given element at the given index in the array.
-- Performs in-place mutation of the array.
--
-- >>> putIndex arr ix val = Array.modifyIndex ix (const (val, ())) arr
-- >>> f = Array.putIndices
-- >>> putIndex ix val arr = Stream.fold (f arr) (Stream.fromPure (ix, val))
--
-- /Pre-release/
{-# INLINE putIndex #-}
putIndex :: (MonadIO m, Storable a) => Int -> a -> Array a -> m ()
putIndex i x arr =
    asPtrUnsafe arr
        $ \p -> putIndexPtr p (aEnd arr) i x

-- | Write an input stream of (index, value) pairs to an array. Throws an
-- error if any index is out of bounds.
--
-- /Pre-release/
{-# INLINE putIndices #-}
putIndices :: forall m a. (MonadIO m, Storable a)
    => Array a -> Fold m (Int, a) ()
putIndices Array{..} = FL.mkFoldM step initial extract

    where

    initial = return $ FL.Partial ()

    step () (i, x) = FL.Partial <$> liftIO (putIndexPtr arrStart aEnd i x)

    extract () = liftIO $ touch arrContents

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndexUnsafe :: forall m a b. (MonadIO m, Storable a) =>
    Int -> (a -> (a, b)) -> Array a -> m b
modifyIndexUnsafe i f arr@(Array{..}) = do
    liftIO $ asPtrUnsafe arr $ \ptr -> do
        let elemPtr = PTR_INDEX(ptr,i,a)
        assert (i >= 0 && PTR_NEXT(elemPtr,a) <= aEnd) (return ())
        r <- peek elemPtr
        let (x, res) = f r
        poke elemPtr x
        return res

{-# INLINE modifyIndexPtr #-}
modifyIndexPtr :: forall m a b. (MonadIO m, Storable a) =>
    Int -> (a -> (a, b)) -> Ptr a -> Ptr a -> m b
modifyIndexPtr i f start end = liftIO $ do
    let elemPtr = PTR_INDEX(start,i,a)
    if i >= 0 && PTR_VALID(elemPtr,end,a)
    then do
        r <- peek elemPtr
        let (x, res) = f r
        poke elemPtr x
        return res
    else invalidIndex "modifyIndex" i

-- | Modify a given index of an array using a modifier function.
--
-- /Pre-release/
modifyIndex :: forall m a b. (MonadIO m, Storable a) =>
    Int -> (a -> (a, b)) -> Array a -> m b
modifyIndex i f arr@(Array{..}) = do
    liftIO $ asPtrUnsafe arr $ \ptr -> do
        modifyIndexPtr i f ptr aEnd

-- | Modify the array indices generated by the supplied stream.
--
-- /Pre-release/
{-# INLINE modifyIndices #-}
modifyIndices :: forall m a. (MonadIO m, Storable a)
    => (a -> a) -> Array a -> Fold m Int ()
modifyIndices f Array{..} = Fold step initial extract

    where

    initial = return $ FL.Partial ()

    step () i =
        let f1 x = (f x, ())
         in FL.Partial <$> liftIO (modifyIndexPtr i f1 arrStart aEnd)

    extract () = liftIO $ touch arrContents

-- | Modify each element of an array using the supplied modifier function.
--
-- /Pre-release/
modify :: forall m a. (MonadIO m, Storable a)
    => (a -> a) -> Array a -> m ()
modify f arr@Array{..} = liftIO $
    asPtrUnsafe arr go

    where

    go ptr =
        when (PTR_VALID(ptr,aEnd,a)) $ do
            r <- peek ptr
            poke ptr (f r)
            go (PTR_NEXT(ptr,a))

{-# INLINE swapPtrs #-}
swapPtrs :: Storable a => Ptr a -> Ptr a -> IO ()
swapPtrs ptr1 ptr2 = do
    r1 <- peek ptr1
    r2 <- peek ptr2
    poke ptr1 r2
    poke ptr2 r1

-- | Swap the elements at two indices without validating the indices.
--
-- /Unsafe/: This could result in memory corruption if indices are not valid.
--
-- /Pre-release/
{-# INLINE unsafeSwapIndices #-}
unsafeSwapIndices :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Array a -> m ()
unsafeSwapIndices i1 i2 arr = liftIO $ do
    asPtrUnsafe arr $ \ptr -> do
        let ptr1 = PTR_INDEX(ptr,i1,a)
            ptr2 = PTR_INDEX(ptr,i2,a)
        swapPtrs ptr1 (ptr2 :: Ptr a)

-- | Swap the elements at two indices.
--
-- /Pre-release/
swapIndices :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Array a -> m ()
swapIndices i1 i2 Array{..} = liftIO $ do
        let ptr1 = PTR_INDEX(arrStart,i1,a)
            ptr2 = PTR_INDEX(arrStart,i2,a)
        when (i1 < 0 || PTR_INVALID(ptr1,aEnd,a))
            $ invalidIndex "swapIndices" i1
        when (i2 < 0 || PTR_INVALID(ptr2,aEnd,a))
            $ invalidIndex "swapIndices" i2
        swapPtrs ptr1 (ptr2 :: Ptr a)

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
{-# NOINLINE reallocAligned #-}
reallocAligned :: Int -> Int -> Int -> Array a -> IO (Array a)
reallocAligned elemSize alignSize newCapacity Array{..} = do
    assert (aEnd <= aBound) (return ())

    -- Allocate new array
    let newCapMax = roundUpLargeArray newCapacity
    (contents, pNew) <- newAlignedArrayContents newCapMax alignSize

    -- Copy old data
    let oldStart = arrStart
        oldSize = aEnd `minusPtr` oldStart
        newCap = roundDownTo elemSize newCapMax
        newLen = min oldSize newCap
    assert (oldSize `mod` elemSize == 0) (return ())
    assert (newLen >= 0) (return ())
    assert (newLen `mod` elemSize == 0) (return ())
    memcpy (castPtr pNew) (castPtr oldStart) newLen
    touch arrContents

    return $ Array
        { arrStart = pNew
        , arrContents = contents
        , aEnd   = pNew `plusPtr` newLen
        , aBound = pNew `plusPtr` newCap
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
realloc n arr =
    liftIO $ reallocAligned (SIZE_OF(a)) (alignment (undefined :: a)) n arr

-- | @reallocWith label capSizer minIncrement array@. The label is used
-- in error messages and the capSizer is used to determine the capacity of the
-- new array in bytes given the current byte length of the array.
reallocWith :: forall m a. (MonadIO m , Storable a) =>
       String
    -> (Int -> Int)
    -> Int
    -> Array a
    -> m (Array a)
reallocWith label capSizer minIncr arr = do
    let oldSize = aEnd arr `minusPtr` arrStart arr
        newCap = capSizer oldSize
        newSize = oldSize + minIncr
        safeCap = max newCap newSize
    assert (newCap >= newSize || error (badSize newSize)) (return ())
    realloc safeCap arr

    where

    badSize newSize = concat
        [ label
        , ": new array size is less than required size "
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
resize n arr@Array{..} = do
    let req = SIZE_OF(a) * n
        len = aEnd `minusPtr` arrStart
    if req < len
    then return arr
    else realloc req arr

-- | Like 'resize' but if the capacity is more than 'largeObjectThreshold' then
-- it is rounded up to the closest power of 2.
--
-- /Pre-release/
{-# INLINE resizeExp #-}
resizeExp :: forall m a. (MonadIO m, Storable a) =>
    Int -> Array a -> m (Array a)
resizeExp n arr@Array{..} = do
    let req = roundUpLargeArray (SIZE_OF(a) * n)
        req1 =
            if req > largeObjectThreshold
            then roundUpToPower2 req
            else req
        len = aEnd `minusPtr` arrStart
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
        len = aEnd `minusPtr` start
        capacity = aBound `minusPtr` start
        target = roundUpLargeArray len
        waste = aBound `minusPtr` aEnd
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
snocNewEnd :: (MonadIO m, Storable a) => Ptr a -> Array a -> a -> m (Array a)
snocNewEnd newEnd arr@Array{..} x = liftIO $ do
    assert (newEnd <= aBound) (return ())
    poke aEnd x
    touch arrContents
    return $ arr {aEnd = newEnd}

-- | Really really unsafe, appends the element into the first array, may
-- cause silent data corruption or if you are lucky a segfault if the first
-- array does not have enough space to append the element.
--
-- /Internal/
{-# INLINE snocUnsafe #-}
snocUnsafe :: forall m a. (MonadIO m, Storable a) =>
    Array a -> a -> m (Array a)
snocUnsafe arr@Array{..} = snocNewEnd (PTR_NEXT(aEnd,a)) arr

-- | Like 'snoc' but does not reallocate when pre-allocated array capacity
-- becomes full.
--
-- /Internal/
{-# INLINE snocMay #-}
snocMay :: forall m a. (MonadIO m, Storable a) =>
    Array a -> a -> m (Maybe (Array a))
snocMay arr@Array{..} x = liftIO $ do
    let newEnd = PTR_NEXT(aEnd,a)
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
    let newEnd = PTR_NEXT(aEnd arr,a)
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

-- | Return the element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL getIndexUnsafe #-}
getIndexUnsafe :: forall m a. (MonadIO m, Storable a) => Int -> Array a -> m a
getIndexUnsafe i arr@(Array {..}) =
    asPtrUnsafe arr $ \ptr -> do
        let elemPtr = PTR_INDEX(ptr,i,a)
        assert (i >= 0 && PTR_VALID(elemPtr,aEnd,a)) (return ())
        liftIO $ peek elemPtr

{-# INLINE getIndexPtr #-}
getIndexPtr :: forall m a. (MonadIO m, Storable a) =>
    Ptr a -> Ptr a -> Int -> m a
getIndexPtr start end i = do
    let elemPtr = PTR_INDEX(start,i,a)
    if i >= 0 && PTR_VALID(elemPtr,end,a)
    then liftIO $ peek elemPtr
    else invalidIndex "getIndexPtr" i

-- | /O(1)/ Lookup the element at the given index. Index starts from 0.
--
{-# INLINE getIndex #-}
getIndex :: (MonadIO m, Storable a) => Int -> Array a -> m a
getIndex i arr =
    asPtrUnsafe arr
        $ \p -> getIndexPtr p (aEnd arr) i

{-# INLINE getIndexPtrRev #-}
getIndexPtrRev :: forall m a. (MonadIO m, Storable a) =>
    Ptr a -> Ptr a -> Int -> m a
getIndexPtrRev start end i = do
    let elemPtr = PTR_RINDEX(end,i,a)
    if i >= 0 && elemPtr >= start
    then liftIO $ peek elemPtr
    else invalidIndex "getIndexPtrRev" i

-- | /O(1)/ Lookup the element at the given index from the end of the array.
-- Index starts from 0.
--
-- Slightly faster than computing the forward index and using getIndex.
--
{-# INLINE getIndexRev #-}
getIndexRev :: (MonadIO m, Storable a) => Int -> Array a -> m a
getIndexRev i arr =
    asPtrUnsafe arr
        $ \p -> getIndexPtrRev p (aEnd arr) i

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

    inject (Array contents start (Ptr end) _) =
        return $ GetIndicesState contents start (Ptr end) sti

    {-# INLINE_LATE step #-}
    step (GetIndicesState contents start end st) = do
        r <- stepi defState st
        case r of
            D.Yield i s -> do
                x <- liftio $ getIndexPtr start end i
                return $ D.Yield x (GetIndicesState contents start end s)
            D.Skip s -> return $ D.Skip (GetIndicesState contents start end s)
            D.Stop -> do
                liftio $ touch contents
                return D.Stop

{-# INLINE getIndices #-}
getIndices :: (MonadIO m, Storable a) => SerialT m Int -> Unfold m (Array a) a
getIndices (SerialT stream) = getIndicesD liftIO $ D.fromStreamK stream

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
    let fp1 = PTR_INDEX(start,index,a)
        end = fp1 `plusPtr` (len * SIZE_OF(a))
     in assert
            (index >= 0 && len >= 0 && end <= e)
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
    let fp1 = PTR_INDEX(start,index,a)
        end = fp1 `plusPtr` (len * SIZE_OF(a))
     in if index >= 0 && len >= 0 && end <= e
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
        h = PTR_PREV(aEnd,a)
     in swap l h

    where

    swap l h = do
        when (l < h) $ do
            swapPtrs l h
            swap (PTR_NEXT(l,a)) (PTR_PREV(h,a))

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
        ptr <- go arrStart (PTR_PREV(aEnd,a))
        let pl = Array arrContents arrStart ptr ptr
            pr = Array arrContents ptr aEnd aEnd
        return (pl, pr)

    where

    -- Invariant low < high on entry, and on return as well
    moveHigh low high = do
        h <- peek high
        if f h
        then
            -- Correctly classified, continue the loop
            let high1 = PTR_PREV(high,a)
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
        l <- peek low
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
                        poke low h
                        poke high1 l
                        let low1 = PTR_NEXT(low,a)
                            high2 = PTR_PREV(high1,a)
                        if low1 <= high2
                        then go low1 high2
                        else return low1 -- low1 > high2

        else do
            -- low is correctly classified
            let low1 = PTR_NEXT(low,a)
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
    let len = aEnd `minusPtr` arrStart
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
    let len = aBound `minusPtr` arrStart
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
                liftIO $ poke end x >> touch contents
                let end1 = PTR_NEXT(end,a)
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
    | InnerLoop s contents !(Ptr a) !(Ptr a)

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
        x <- liftIO $ do
                    r <- peek p
                    touch contents
                    return r
        return $ D.Yield x (InnerLoop st contents (PTR_NEXT(p,a)) end)

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
                let p = PTR_PREV(aEnd,a)
                 in D.Skip (InnerLoop s arrContents p arrStart)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ p start) | p < start =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st contents p start) = do
        x <- liftIO $ do
                    r <- peek p
                    touch contents
                    return r
        let cur = PTR_PREV(p,a)
        return $ D.Yield x (InnerLoop st contents cur start)

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

data ReadUState a = ReadUState
    UNPACKIF !ArrayContents  -- contents
    !(Ptr a)           -- end address
    !(Ptr a)           -- current address

toReadUState :: Array a -> ReadUState a
toReadUState (Array contents start end _) = ReadUState contents end start

-- | Resumable unfold of an array.
--
{-# INLINE_NORMAL producer #-}
producer :: forall m a. (MonadIO m, Storable a) => Producer m (Array a) a
producer = Producer step (return . toReadUState) extract
    where

    {-# INLINE_LATE step #-}
    step (ReadUState contents end cur)
        | assert (cur <= end) (cur == end) = do
            liftIO $ touch contents
            return D.Stop
    step (ReadUState contents end cur) = do
            !x <- liftIO $ peek cur
            return $ D.Yield x (ReadUState contents end (PTR_NEXT(cur,a)))

    extract (ReadUState contents end cur) = return $ Array contents cur end end

-- | Unfold an array into a stream.
--
-- @since 0.7.0
{-# INLINE_NORMAL read #-}
read :: forall m a. (MonadIO m, Storable a) => Unfold m (Array a) a
read = Producer.simplify producer

-- | Unfold an array into a stream in reverse order.
--
-- /Pre-release/
{-# INLINE_NORMAL readRev #-}
readRev :: forall m a. (MonadIO m, Storable a) => Unfold m (Array a) a
readRev = Unfold step inject
    where

    inject (Array contents start end _) =
        let p = PTR_PREV(end,a)
         in return $ ReadUState contents start p

    {-# INLINE_LATE step #-}
    step (ReadUState contents start p) | p < start = do
        liftIO $ touch contents
        return D.Stop
    step (ReadUState contents start p) = do
        x <- liftIO $ peek p
        return $ D.Yield x (ReadUState contents start (PTR_PREV(p,a)))

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
                    r <- peek p
                    touch arrContents
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
        x <- peek p
        touch arrContents
        (:) x <$> go (PTR_NEXT(p,a))

-- | Use the 'read' unfold instead.
--
-- @toStreamD = D.unfold read@
--
-- We can try this if the unfold has any performance issues.
{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (MonadIO m, Storable a) => Array a -> D.Stream m a
toStreamD Array{..} = D.Stream step arrStart

    where

    {-# INLINE_LATE step #-}
    step _ p | assert (p <= aEnd) (p == aEnd) = return D.Stop
    step _ p = liftIO $ do
        r <- peek p
        touch arrContents
        return $ D.Yield r (PTR_NEXT(p,a))

{-# INLINE toStreamK #-}
toStreamK :: forall m a. (MonadIO m, Storable a) => Array a -> K.Stream m a
toStreamK Array{..} = go arrStart

    where

    go p | assert (p <= aEnd) (p == aEnd) = K.nil
         | otherwise =
        let elemM = do
              r <- peek p
              touch arrContents
              return r
        in liftIO elemM `K.consM` go (PTR_NEXT(p,a))

-- | Use the 'readRev' unfold instead.
--
-- @toStreamDRev = D.unfold readRev@
--
-- We can try this if the unfold has any perf issues.
{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: forall m a. (MonadIO m, Storable a) => Array a -> D.Stream m a
toStreamDRev Array{..} =
    let p = PTR_PREV(aEnd,a)
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p < arrStart = return D.Stop
    step _ p = liftIO $ do
        r <- peek p
        touch arrContents
        return $ D.Yield r (PTR_PREV(p,a))

{-# INLINE toStreamKRev #-}
toStreamKRev :: forall m a. (MonadIO m, Storable a) => Array a -> K.Stream m a
toStreamKRev Array {..} =
    let p = PTR_PREV(aEnd,a)
    in go p

    where

    go p | p < arrStart = K.nil
         | otherwise =
        let elemM = do
              r <- peek p
              touch arrContents
              return r
        in liftIO elemM `K.consM` go (PTR_PREV(p,a))

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

data ArrayUnsafe a = ArrayUnsafe
    UNPACKIF !ArrayContents  -- contents
    {-# UNPACK #-} !(Ptr a)  -- start address
    {-# UNPACK #-} !(Ptr a)  -- first unused address

toArrayUnsafe :: Array a -> ArrayUnsafe a
toArrayUnsafe (Array contents start end _) =
    ArrayUnsafe contents start end

fromArrayUnsafe ::
#ifdef DEVBUILD
    Storable a =>
#endif
    ArrayUnsafe a -> Array a
fromArrayUnsafe (ArrayUnsafe contents start end) =
         Array contents start end end

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
    fmap fromArrayUnsafe $ FL.foldlM' step initial

    where

    initial = do
        assert (n >= 0) (return ())
        arr@(Array _ _ end bound) <- action
        let free = bound `minusPtr` end
            needed = n * SIZE_OF(a)
        -- XXX We can also reallocate if the array has too much free space,
        -- otherwise we lose that space.
        arr1 <-
            if free < needed
            then noinline reallocWith "appendNUnsafeWith" (+ needed) needed arr
            else return arr
        return $ toArrayUnsafe arr1

    step (ArrayUnsafe contents start end) x = do
        liftIO $ poke end x >> touch contents
        return $ ArrayUnsafe contents start (PTR_NEXT(end,a))

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
writeNWithUnsafe alloc n = Fold step initial (return . fromArrayUnsafe)

    where

    initial = FL.Partial . toArrayUnsafe <$> alloc (max n 0)

    step (ArrayUnsafe contents start end) x = do
        liftIO $ poke end x >> touch contents
        return
          $ FL.Partial
          $ ArrayUnsafe contents start (PTR_NEXT(end,a))

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
        liftIO $ poke end x
        return $ Array contents start (PTR_NEXT(end,a)) bound

    initial = do
        when (elemCount < 0) $ error "writeWith: elemCount is negative"
        liftIO $ newArrayAligned (alignment (undefined :: a)) elemCount
    step arr@(Array _ start end bound) x
        | PTR_NEXT(end,a) > bound = do
        let oldSize = end `minusPtr` start
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
    end <- D.foldlM' fwrite (return $ aEnd arr) $ D.take limit str
    return $ arr {aEnd = end}

    where

    fwrite ptr x = do
        liftIO $ poke ptr x
        return $ PTR_NEXT(ptr,a)

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
{-# INLINE fromList #-}
fromList :: (MonadIO m, Storable a) => [a] -> m (Array a)
fromList xs = fromStreamD $ D.fromList xs

-------------------------------------------------------------------------------
-- Combining
-------------------------------------------------------------------------------

-- | Copy two arrays into a newly allocated array.
{-# INLINE spliceCopy #-}
spliceCopy :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
spliceCopy arr1 arr2 = do
    let src1 = arrStart arr1
        src2 = arrStart arr2
        len1 = aEnd arr1 `minusPtr` src1
        len2 = aEnd arr2 `minusPtr` src2

    arr <- liftIO $ newArray (len1 + len2)
    let dst = arrStart arr

    -- XXX Should we use copyMutableByteArray# instead? Is there an overhead to
    -- ccall?
    liftIO $ do
        memcpy (castPtr dst) (castPtr src1) len1
        touch (arrContents arr1)
        memcpy (castPtr (dst `plusPtr` len1)) (castPtr src2) len2
        touch (arrContents arr2)
    return arr { aEnd = dst `plusPtr` (len1 + len2) }

-- | Really really unsafe, appends the second array into the first array. If
-- the first array does not have enough space it may cause silent data
-- corruption or if you are lucky a segfault.
{-# INLINE spliceUnsafe #-}
spliceUnsafe :: MonadIO m => Array a -> (Array a, Int) -> m (Array a)
spliceUnsafe dst (src, srcLen) =
    liftIO $ do
         let psrc = arrStart src
         let pdst = aEnd dst
         assert (pdst `plusPtr` srcLen <= aBound dst) (return ())
         memcpy (castPtr pdst) (castPtr psrc) srcLen
         touch (arrContents src)
         touch (arrContents dst)
         return $ dst {aEnd = pdst `plusPtr` srcLen}

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
spliceWith sizer dst@(Array _ start end bound) src = do
{-
    let f = appendWith (`sizer` byteLength src) (return dst)
     in D.fold f (toStreamD src)
-}
    assert (end <= bound) (return ())
    let srcLen = aEnd src `minusPtr` arrStart src

    dst1 <-
        if end `plusPtr` srcLen >= bound
        then do
            let oldSize = end `minusPtr` start
                newSize = sizer oldSize srcLen
            when (newSize < oldSize + srcLen)
                $ error
                    $ "splice: newSize is less than the total size "
                    ++ "of arrays being appended. Please check the "
                    ++ "newSize function passed."
            liftIO $ realloc newSize dst
        else return dst
    spliceUnsafe dst1 (src, srcLen)

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
    let p = arrStart
    loc <- c_memchr p sep (fromIntegral $ aEnd `minusPtr` p)
    return $
        if loc == nullPtr
        then (arr, Nothing)
        else
            ( Array
                { arrContents = arrContents
                , arrStart = arrStart
                , aEnd = loc
                , aBound = loc
                }
            , Just $ Array
                    { arrContents = arrContents
                    , arrStart = arrStart `plusPtr` (loc `minusPtr` p + 1)
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
                      p = arrStart `plusPtr` off
                in ( Array
                  { arrContents = arrContents
                  , arrStart = arrStart
                  , aEnd = p
                  , aBound = p
                  }
                , Array
                  { arrContents = arrContents
                  , arrStart = arrStart `plusPtr` off
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
    Array contents (castPtr start) (castPtr end) (castPtr bound)

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

-- | Use an @Array a@ as @Ptr a@.
--
-- /Unsafe/
--
-- /Pre-release/
--
asPtrUnsafe :: MonadIO m => Array a -> (Ptr a -> m b) -> m b
asPtrUnsafe Array{..} f = do
  r <- f arrStart
  liftIO $ touch arrContents
  return r

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
        let ptr1 = arrStart arr1
        let ptr2 = arrStart arr2
        let len1 = aEnd arr1 `minusPtr` ptr1
        let len2 = aEnd arr2 `minusPtr` ptr2

        if len1 == len2
        then
            if ptr1 == ptr2
            then return True
            else do
                r <- memcmp (castPtr ptr1) (castPtr ptr2) len1
                touch (arrContents arr1)
                touch (arrContents arr2)
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

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 Array where
    {-# INLINE liftRnf #-}
    liftRnf _ Array{} = ()
#endif
