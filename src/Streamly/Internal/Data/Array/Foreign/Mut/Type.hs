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
    , mutableArray
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
    , shrinkToFit

    -- * Size
    , length
    , byteLength
    , byteCapacity

    -- * Random access
    , unsafeIndex

    -- * Mutation
    , unsafeWriteIndex
    , unsafeSnoc
    , snoc

    -- * Folding
    , foldl'
    , foldr

    -- * Composable Folds
    , toArrayMinChunk
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
    , spliceWithDoubling
    , spliceTwo

    -- * Splitting
    , breakOn
    , splitAt

    -- * Stream of arrays
    , arraysOf
    , bufferChunks
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
import Data.Functor.Identity (runIdentity)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr (touchForeignPtr)
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

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

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
--
-- XXX Rename the fields to better names.
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

{-# INLINE mutableArray #-}
mutableArray ::
#ifdef DEVBUILD
    Storable a =>
#endif
    ForeignPtr a -> Ptr a -> Ptr a -> Array a
mutableArray = Array

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | allocate a new array using the provided allocator function.
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

-- | Allocate a new array aligned to the specified alignmend and using
-- unmanaged pinned memory. The memory will not be automatically freed by GHC.
-- This could be useful in allocate once global data structures. Use carefully
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
-- | Allocate an array that can hold 'count' items.  The memory of the array is
-- uninitialized.
--
-- Note that this is internal routine, the reference to this array cannot be
-- given out until the array has been written to and frozen.
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
-- Mutation
-------------------------------------------------------------------------------

{-# INLINE unsafeWriteIndex #-}
unsafeWriteIndex :: forall m a. (MonadIO m, Storable a)
    => Array a -> Int -> a -> m ()
unsafeWriteIndex Array {..} i x =
    unsafeWithForeignPtrM aStart
        $ \begin -> do
              liftIO $ poke (begin `plusPtr` (i * sizeOf (undefined :: a))) x

-- Internal routine for when the array is being created. Appends one item at
-- the end of the array. Useful when sequentially writing a stream to the
-- array.
{-# INLINE unsafeSnoc #-}
unsafeSnoc :: forall m a. (MonadIO m, Storable a) => Array a -> a -> m (Array a)
unsafeSnoc arr@Array {..} x = liftIO $ do
    when (aEnd == aBound) $ error "BUG: unsafeSnoc: writing beyond array bounds"
    poke aEnd x
    touchForeignPtr aStart
    return $ arr {aEnd = aEnd `plusPtr` sizeOf (undefined :: a)}

{-# INLINE snoc #-}
snoc :: forall m a. (MonadIO m, Storable a) => Array a -> a -> m (Array a)
snoc arr@Array {..} x =
    if aEnd == aBound
    then do
        let oldStart = unsafeForeignPtrToPtr aStart
            size = aEnd `minusPtr` oldStart
            newSize = size + sizeOf (undefined :: a)
        newPtr <- liftIO $
            Malloc.mallocForeignPtrAlignedBytes
                newSize
                (alignment (undefined :: a))
        unsafeWithForeignPtrM newPtr $ \pNew -> liftIO $ do
            memcpy (castPtr pNew) (castPtr oldStart) size
            poke (pNew `plusPtr` size) x
            touchForeignPtr aStart
            return $
                Array
                    { aStart = newPtr
                    , aEnd = pNew `plusPtr` (size + sizeOf (undefined :: a))
                    , aBound = pNew `plusPtr` newSize
                    }
    else liftIO $ do
        poke aEnd x
        touchForeignPtr aStart
        return $ arr {aEnd = aEnd `plusPtr` sizeOf (undefined :: a)}

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

-- | Remove the free space from an Array.
{-# INLINE shrinkToFit #-}
shrinkToFit :: (MonadIO m, Storable a) => Array a -> m (Array a)
shrinkToFit arr@Array{..} = do
    assert (aEnd <= aBound) (return ())
    let start = unsafeForeignPtrToPtr aStart
    let used = aEnd `minusPtr` start
        waste = aBound `minusPtr` aEnd
    -- if used == waste == 0 then do not realloc
    -- if the wastage is more than 25% of the array then realloc
    if used < 3 * waste
    then realloc used arr
    else return arr

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | Return element at the specified index without checking the bounds.
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall m a. (MonadIO m, Storable a) => Array a -> Int -> m a
unsafeIndex Array {..} i =
        unsafeWithForeignPtrM aStart $ \p -> do
        let elemSize = sizeOf (undefined :: a)
            elemOff = p `plusPtr` (elemSize * i)
        assert (i >= 0 && elemOff `plusPtr` elemSize <= aEnd)
               (return ())
        liftIO $ peek elemOff

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

-- | /O(1)/ Get the length of the array i.e. the number of elements in the
-- array.
--
-- @since 0.7.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length arr = byteLength arr `div` sizeOf (undefined :: a)

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
            error $ "Streamly.Internal.Data.Array.Foreign.Mut.Type.fromStreamDArraysOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"
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
{-# INLINE bufferChunks #-}
bufferChunks :: (MonadIO m, Storable a) =>
    D.Stream m a -> m (K.Stream m (Array a))
bufferChunks m = D.foldr K.cons K.nil $ arraysOf defaultChunkSize m

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

    step' _ (InnerLoop st _ p end) | p == end =
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
    step (ReadUState fp@(ForeignPtr end _) p) | p == Ptr end =
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

    go p | p == aEnd = n
    go p =
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        in c x (go (p `plusPtr` sizeOf (undefined :: a)))

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
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        -- unsafeInlineIO allows us to run this in Identity monad for pure
        -- toList/foldr case which makes them much faster due to not
        -- accumulating the list and fusing better with the pure consumers.
        --
        -- This should be safe as the array contents are guaranteed to be
        -- evaluated/written to before we peek at them.
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

    go p | p == aEnd = K.nil
         | otherwise =
        -- See Note in toStreamD.
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
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        in x `K.cons` go (p `plusPtr` negate (sizeOf (undefined :: a)))

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

-- | Strict left fold of an array.
{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

-- | Right fold of an array.
{-# INLINE_NORMAL foldr #-}
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
-- @writeChunks = Fold.many Fold.toStream (Array.writeN n)@
--
-- See 'bufferChunks'.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL writeChunks #-}
writeChunks :: -- (MonadIO m, Storable a) =>
    Int -> Fold m a (D.Stream m (Array a))
writeChunks = undefined -- Fold.many Fold.toStream (Array.writeN n)

-- XXX Compare toArrayMinChunk with fromStreamD which uses an array of streams
-- implementation. We can write this using writeChunks above if that is faster.
-- If toArrayMinChunk is faster then we should use that to implement
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

{-# INLINE_NORMAL toArrayMinChunk #-}
toArrayMinChunk :: forall m a. (MonadIO m, Storable a)
    => Int -> Int -> Fold m a (Array a)
-- toArrayMinChunk n = FL.rmapM spliceArrays $ toArraysOf n
toArrayMinChunk alignSize elemCount =
    FL.rmapM extract $ FL.foldlM' step initial

    where

    insertElem (Array start end bound) x = do
        liftIO $ poke end x
        return $ Array start (end `plusPtr` sizeOf (undefined :: a)) bound

    initial = do
        when (elemCount < 0) $ error "toArrayMinChunk: elemCount is negative"
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
    extract = liftIO . shrinkToFit

-- | Fold the whole input to a single array.
--
-- /Caution! Do not use this on infinite streams./
--
-- @since 0.7.0
{-# INLINE write #-}
write :: forall m a. (MonadIO m, Storable a) => Fold m a (Array a)
write = toArrayMinChunk (alignment (undefined :: a))
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
    toArrayMinChunk alignSize
                    (bytesToElemCount (undefined :: a)
                    (mkChunkSize 1024))

-------------------------------------------------------------------------------
-- construct from streams, known size
-------------------------------------------------------------------------------

-- | Use the 'writeN' fold instead.
--
-- @fromStreamDN n = D.fold (writeN n)@
--
{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
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
fromStreamD m = do
    buffered <- bufferChunks m
    len <- K.foldl' (+) 0 (K.map length buffered)
    fromStreamDN len $ D.unfoldMany read $ D.fromStreamK buffered

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

-- | Splice an array into a pre-reserved mutable array.  The user must ensure
-- that there is enough space in the mutable array, otherwise the splicing
-- fails.
{-# INLINE spliceWith #-}
spliceWith :: (MonadIO m) => Array a -> Array a -> m (Array a)
spliceWith dst@(Array _ end bound) src =
    liftIO $ do
        let srcLen = byteLength src
        if end `plusPtr` srcLen > bound
        then error
                 "Bug: spliceWith: Not enough space in the target array"
        else unsafeWithForeignPtr (aStart dst) $ \_ ->
                unsafeWithForeignPtr (aStart src) $ \psrc -> do
                     let pdst = aEnd dst
                     memcpy (castPtr pdst) (castPtr psrc) srcLen
                     return $ dst {aEnd = pdst `plusPtr` srcLen}

-- | Splice a new array into a preallocated mutable array, doubling the space
-- if there is no space in the target array.
{-# INLINE spliceWithDoubling #-}
spliceWithDoubling :: (MonadIO m, Storable a)
    => Array a -> Array a -> m (Array a)
spliceWithDoubling dst@(Array start end bound) src  = do
    assert (end <= bound) (return ())
    let srcLen = aEnd src `minusPtr` unsafeForeignPtrToPtr (aStart src)

    dst1 <-
        if end `plusPtr` srcLen >= bound
        then do
            let oldStart = unsafeForeignPtrToPtr start
                oldSize = end `minusPtr` oldStart
                newSize = max (oldSize * 2) (oldSize + srcLen)
            liftIO $ realloc newSize dst
        else return dst
    spliceWith dst1 src

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
-- Instances
-------------------------------------------------------------------------------

instance (Show a, Storable a) => Show (Array a) where
    {-# INLINE showsPrec #-}
    showsPrec _ = shows . toList

{-# INLINE arrcmp #-}
arrcmp :: Array a -> Array a -> Bool
arrcmp arr1 arr2 =
    let !res = unsafeInlineIO $ do
            let ptr1 = unsafeForeignPtrToPtr $ aStart arr1
            let ptr2 = unsafeForeignPtrToPtr $ aStart arr2
            let len1 = aEnd arr1 `minusPtr` ptr1
            let len2 = aEnd arr2 `minusPtr` ptr2

            if len1 == len2
            then do
                r <- memcmp (castPtr ptr1) (castPtr ptr2) len1
                touchForeignPtr $ aStart arr1
                touchForeignPtr $ aStart arr2
                return r
            else return False
    in res

-- XXX we are assuming that Storable equality means element equality. This may
-- or may not be correct? arrcmp is 40% faster compared to stream equality.
instance (Storable a, Eq a) => Eq (Array a) where
    {-# INLINE (==) #-}
    (==) = arrcmp
    -- arr1 == arr2 = runIdentity $ D.eqBy (==) (toStreamD arr1) (toStreamD arr2)

-------------------------------------------------------------------------------
-- NFData
-------------------------------------------------------------------------------

-- This is a Storable array, we cannot have unevaluated data in it so this is
-- just a no op.
instance NFData (Array a) where
    {-# INLINE rnf #-}
    rnf Array {} = ()

#ifdef DEVBUILD
-- Definitions using the Storable constraint from the Array type. These are to
-- make the Foldable instance possible though it is much slower (7x slower).
--
{-# INLINE_NORMAL toStreamD_ #-}
toStreamD_ :: forall m a. MonadIO m => Int -> Array a -> D.Stream m a
toStreamD_ size Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        x <- liftIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` size)
#endif
