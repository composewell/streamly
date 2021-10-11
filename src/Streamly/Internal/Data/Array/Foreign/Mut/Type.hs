-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Mut.Type
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Array.Foreign.Mut.Type
    (
    -- * Type
    -- $arrayNotes
      Array (..)

    -- * Construction
    , fromForeignPtrUnsafe
    , unsafeWithNewArray
    , newArray
    , newArrayAligned
    , newArrayAlignedUnmanaged
    , newArrayAlignedAllocWith

    -- * From containers
    , fromList
    , fromListN
    , fromStreamDN
    , fromStreamD

    -- * Resizing
    , realloc
    , rightSize

    -- * Size
    , length
    , byteLength
    , byteCapacity

    -- * Random access
    , getIndexUnsafe

    -- * Mutation
    , putIndexUnsafe
    , snocUnsafe
    , snoc
    , snocLinear

    -- * Casting
    , cast
    , castUnsafe
    , asBytes
    , asPtrUnsafe

    -- * Folding
    , foldl'
    , foldr
    , cmp

    -- * Composable Folds
    , writeWith
    , writeNAllocWith
    , writeN
    , writeNUnsafe
    , ArrayUnsafe (..)
    , writeNAligned
    , writeNAlignedUnmanaged
    , write
    , writeAligned

    -- * Unfolds
    , ReadUState
    , read
    , readRev
    , producer
    , flattenArrays
    , flattenArraysRev

    -- * To containers
    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toList

    -- * Combining
    , spliceWith
    , splice
    , spliceExp
    , spliceTwo

    -- * Splitting
    , breakOn
    , splitAt

    -- * Stream of arrays
    , arraysOf
    , writeChunks

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
import Data.Functor.Identity (runIdentity)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr (touchForeignPtr, castForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (build)
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

import Prelude hiding (length, foldr, read, unlines, splitAt)

#if MIN_VERSION_base(4,10,0)
import Foreign.ForeignPtr (plusForeignPtr)
#else
import GHC.Base (Int(..), plusAddr#)
import GHC.ForeignPtr (ForeignPtr(..))
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr c) (I# d) = ForeignPtr (plusAddr# addr d) c
#endif

-- $setup
-- >>> import qualified Streamly.Internal.Data.Stream.StreamD as StreamD
-- >>> import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as Array

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
-- how many elements of that type are required to hold those bytes.
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

-- | allocate a new empty array using the provided allocator function.
{-# INLINE newArrayAlignedAllocWith #-}
newArrayAlignedAllocWith :: forall m a. (MonadIO m, Storable a)
    => (Int -> Int -> m (ForeignPtr a)) -> Int -> Int -> m (Array a)
newArrayAlignedAllocWith alloc alignSize count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- alloc size alignSize
    let p = unsafeForeignPtrToPtr fptr
    return $ Array
        { aStart = fptr
        , aEnd   = p
        , aBound = p `plusPtr` size
        }

-- | Allocate a new empty array using the specified alignment and using
-- unmanaged pinned memory. The memory will not be automatically freed by GHC.
-- This could be useful in allocate-once global data structures. Use carefully
-- as incorrect use can lead to memory leak.
{-# INLINE newArrayAlignedUnmanaged #-}
newArrayAlignedUnmanaged :: (MonadIO m, Storable a) => Int -> Int -> m (Array a)
newArrayAlignedUnmanaged =
    newArrayAlignedAllocWith mallocForeignPtrAlignedUnmanagedBytes

    where

    mallocForeignPtrAlignedUnmanagedBytes size alignSize =
        liftIO $ Malloc.mallocForeignPtrAlignedUnmanagedBytes size alignSize

{-# INLINE newArrayAligned #-}
newArrayAligned :: (MonadIO m, Storable a) => Int -> Int -> m (Array a)
newArrayAligned = newArrayAlignedAllocWith mallocForeignPtrAlignedBytes

    where

    mallocForeignPtrAlignedBytes size alignSize =
        liftIO $ Malloc.mallocForeignPtrAlignedBytes size alignSize

-- XXX can unaligned allocation be more efficient when alignment is not needed?
--
-- | Allocate an empty array that can hold 'count' items.  The memory of the
-- array is uninitialized.
--
{-# INLINE newArray #-}
newArray :: forall m a. (MonadIO m, Storable a) => Int -> m (Array a)
newArray = newArrayAligned (alignment (undefined :: a))

-- | Allocate an Array of the given size and run an IO action passing the array
-- start pointer.
{-# INLINE unsafeWithNewArray #-}
unsafeWithNewArray ::
       (MonadIO m, Storable a) => Int -> (Ptr a -> m ()) -> m (Array a)
unsafeWithNewArray count f = do
    arr <- newArray count
    unsafeWithForeignPtrM (aStart arr) $ \p -> f p >> return arr

-------------------------------------------------------------------------------
-- Random writes
-------------------------------------------------------------------------------

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

reallocWith :: forall m a. (MonadIO m , Storable a) =>
       String
    -> (Int -> Int)
    -> Int
    -> Array a
    -> m (Array a)
reallocWith label allocSize reqSize arr = do
    let oldSize = aEnd arr `minusPtr` unsafeForeignPtrToPtr (aStart arr)
        newSize = allocSize oldSize
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
snocWithRealloc allocSize arr x = do
    let elemSize = sizeOf (undefined :: a)
    arr1 <- liftIO $ reallocWith "snocWith" allocSize elemSize arr
    snocUnsafe arr1 x

-- | @snocWith newSize arr elem@ mutates @arr@ to append @elem@, the length of
-- the array increases by 1.
--
-- If there is no reserved space available in @arr@ it is reallocated to a size
-- in bytes determined by the @newSize oldSize@ function, where @oldSize@ is
-- the original size of the array in bytes.
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
-- >>> snoc = snocWith (* 2)
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

-- XXX can unaligned allocation be more efficient when alignment is not needed?
{-# INLINABLE realloc #-}
realloc :: forall m a. (MonadIO m, Storable a) => Int -> Array a -> m (Array a)
realloc i arr =
    liftIO
        $ reallocAligned
            (sizeOf (undefined :: a)) (alignment (undefined :: a)) i arr

-- | Drop any reserved free space at the end of the array and reallocate it to
-- reduce wastage.
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
--
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

{-# INLINE_NORMAL writeNAllocWith #-}
writeNAllocWith :: forall m a. (MonadIO m, Storable a)
    => (Int -> m (Array a)) -> Int -> Fold m a (Array a)
writeNAllocWith alloc n = FL.take n (writeNUnsafeWith alloc n)

-- | @writeN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> writeN n = Fold.take n (writeNUnsafe n)
--
-- @since 0.7.0
{-# INLINE_NORMAL writeN #-}
writeN :: forall m a. (MonadIO m, Storable a) => Int -> Fold m a (Array a)
writeN = writeNAllocWith newArray

-- | @writeNAligned alignment n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size.
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAligned #-}
writeNAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAligned alignSize = writeNAllocWith (newArrayAligned alignSize)

-- | @writeNAlignedUnmanaged n@ folds a maximum of @n@ elements from the input
-- stream to an 'Array' aligned to the given size and using unmanaged memory.
-- This could be useful to allocate memory that we need to allocate only once
-- in the lifetime of the program.
--
-- /Pre-release/
--
{-# INLINE_NORMAL writeNAlignedUnmanaged #-}
writeNAlignedUnmanaged :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
writeNAlignedUnmanaged alignSize =
    writeNAllocWith (newArrayAlignedUnmanaged alignSize)

data ArrayUnsafe a = ArrayUnsafe
    {-# UNPACK #-} !(ForeignPtr a) -- first address
    {-# UNPACK #-} !(Ptr a)        -- first unused address

-- XXX We can carry bound as well in the state to make sure we do not lose the
-- remaining capacity. Need to check perf impact.
--
-- | This API is only useful when we do not want to use the remaining space in
-- the array to append anything else later as that space is lost.
--
{-# INLINE_NORMAL writeNUnsafeWith #-}
writeNUnsafeWith :: forall m a. (MonadIO m, Storable a)
    => (Int -> m (Array a)) -> Int -> Fold m a (Array a)
writeNUnsafeWith alloc n = Fold step initial extract

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
-- @since 0.7.0
{-# INLINE_NORMAL writeNUnsafe #-}
writeNUnsafe :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeNUnsafe = writeNUnsafeWith newArray

-- XXX Buffer to a list instead?
--
-- | Buffer a stream into a stream of arrays.
--
-- >>> writeChunks n = FL.many (writeN n) FL.toStreamK
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

{-# INLINE_NORMAL writeWith #-}
writeWith :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
-- writeWith n = FL.rmapM spliceArrays $ toArraysOf n
writeWith alignSize elemCount =
    FL.rmapM extract $ FL.foldlM' step initial

    where

    insertElem (Array start end bound) x = do
        liftIO $ poke end x
        return $ Array start (end `plusPtr` sizeOf (undefined :: a)) bound

    initial = do
        when (elemCount < 0) $ error "writeWith: elemCount is negative"
        liftIO $ newArrayAligned alignSize elemCount
    step arr@(Array start end bound) x
        | end `plusPtr` sizeOf (undefined :: a) > bound = do
        let p = unsafeForeignPtrToPtr start
            oldSize = end `minusPtr` p
            newSize = max (oldSize * 2) 1
        arr1 <-
            liftIO
                $ reallocAligned
                    (sizeOf (undefined :: a)) alignSize newSize arr
        insertElem arr1 x
    step arr x = insertElem arr x
    extract = liftIO . rightSize

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
write = writeWith (alignment (undefined :: a))
                        (bytesToElemCount (undefined :: a)
                        (mkChunkSize 1024))

-- | Like 'write' but the array memory is aligned according to the specified
-- alignment size. This could be useful when we have specific alignment, for
-- example, cache aligned arrays for lookup table etc.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE writeAligned #-}
writeAligned :: forall m a. (MonadIO m, Storable a)
    => Int -> Fold m a (Array a)
writeAligned alignSize =
    writeWith alignSize
                    (bytesToElemCount (undefined :: a)
                    (mkChunkSize 1024))

-------------------------------------------------------------------------------
-- construct from streams, known size
-------------------------------------------------------------------------------

-- | Use the 'writeN' fold instead.
--
-- >>> fromStreamDN n = StreamD.fold (writeN n)
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
-- @
-- fromStreamD = StreamD.fold Array.write
-- @
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
{-# INLINE spliceTwo #-}
spliceTwo :: (MonadIO m, Storable a) => Array a -> Array a -> m (Array a)
spliceTwo arr1 arr2 = do
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

-- | Really really unsafe, appends the second array into the first array. If
-- the first array does not have enough space it may cause silent data
-- corruption or if you are lucky a segfault.
{-# INLINE spliceUnsafe #-}
spliceUnsafe :: MonadIO m => Array a -> (Array a, Int) -> m (Array a)
spliceUnsafe dst (src, srcLen) =
    liftIO $ do
        unsafeWithForeignPtr (aStart dst) $ \_ ->
            unsafeWithForeignPtr (aStart src) $ \psrc -> do
                 let pdst = aEnd dst
                 assert (pdst `plusPtr` srcLen <= aBound dst) (return ())
                 memcpy (castPtr pdst) (castPtr psrc) srcLen
                 return $ dst {aEnd = pdst `plusPtr` srcLen}

-- | @spliceWith sizer dst src@ mutates @dst@ to append @src@. If there is no
-- reserved space available in the @dst@ it is reallocated to a size determined
-- by @sizer dstSize srcSize@ function, where @dstSize@ is the number of bytes
-- in the first array and @srcSize@ is the number of bytes in the second array.
--
-- Note that the returned array may be a mutated version of first array.
--
-- /Pre-release/
{-# INLINE spliceWith #-}
spliceWith :: forall m a. (MonadIO m, Storable a) =>
    (Int -> Int -> Int) -> Array a -> Array a -> m (Array a)
spliceWith allocSize dst@(Array start end bound) src = do
    assert (end <= bound) (return ())
    let srcLen = aEnd src `minusPtr` unsafeForeignPtrToPtr (aStart src)

    dst1 <-
        if end `plusPtr` srcLen >= bound
        then do
            let oldStart = unsafeForeignPtrToPtr start
                oldSize = end `minusPtr` oldStart
                newSize = allocSize oldSize srcLen
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
