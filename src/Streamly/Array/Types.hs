{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Array
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Array.Types
    (
      Array (..)
    , ByteArray

    , unsafeDangerousPerformIO
    , withNewArray
    , unsafeNew
    , unsafeAppend
    , shrinkToFit

    , unsafeIndex
    , fromCStringAddrUnsafe
    , length
    , foldl'

    , fromList
    , fromListN
    , toList
    )
where

import Control.Exception (assert)
import Control.Monad (when)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr
       (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length)
import Text.Read (readPrec, readListPrec, readListPrecDefault)

import GHC.Base (Addr#, realWorld#)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes, newForeignPtr_)
import GHC.IO (IO(IO), unsafeDupablePerformIO)
import GHC.Ptr (Ptr(..))

import qualified Prelude
import qualified Streamly.Streams.StreamD.Type as D

-------------------------------------------------------------------------------
-- Design Notes
-------------------------------------------------------------------------------

-- There are two goals that we need to fulfill and use arrays to fulfill them.
-- One, holding large amounts of data in non-GC memory, two, allow random
-- access to elements based on index. The first one falls in the category of
-- storage buffers while the second one falls in the category of
-- maps/multisets/hashmaps.
--
-- For the first requirement we use an array of Storables. We can have both
-- immutable and mutable variants of this array using wrappers over the same
-- underlying type.
--
-- For second requirement, we need a separate type for arrays of polymorphic
-- values, for example vectors of handler functions, lookup tables. We can call
-- this "vector".  It should not require Storable instance for the type. In
-- that case we need to use an Array# instead of a ForeignPtr. This type of
-- array would not reduce the GC overhead as much because each element of the
-- array still needs to be scanned by the GC.  However, this would allow random
-- access to the elements.  But in most cases random access means storage, and
-- it means we need to avoid GC scanning except in cases of trivially small
-- storage. One way to achieve that would be to put the array in a Compact
-- region. However, when we mutate this, we will have to use a manual GC
-- copying out to another CR and freeing the old one.

-------------------------------------------------------------------------------
-- SIMD Arrays
-------------------------------------------------------------------------------

-- XXX Try using SIMD operations where possible to combine arrays and to fold
-- arrays. For example computing checksums of streams, adding streams or
-- summing streams.

-------------------------------------------------------------------------------
-- Caching coalescing/batching
-------------------------------------------------------------------------------

-- XXX we can use address tags in IO buffers to coalesce multiple buffers into
-- fewer IO requests. Similarly we can split responses to serve them to the
-- right consumers. This will be comonadic. A common buffer cache can be
-- maintained which can be shared by many consumers.
--
-- XXX we can also have IO error monitors attached to streams. to monitor disk
-- or network errors or latencies and then take actions for example starting a
-- disk scrub or switching to a different location on the network.

-------------------------------------------------------------------------------
-- Representation notes
-------------------------------------------------------------------------------

-- XXX we can use newtype over stream for buffers. That way we can implement
-- operations like length as a fold of length of all underlying buffers.
-- A single buffer could be a singleton stream and more than one buffers would
-- be a stream of buffers.
--
-- Also, if a single buffer size is more than a threshold we can store it as a
-- linked list in the non-gc memory. This will allow unlimited size buffers to
-- be stored.
--
-- Unified Array + Stream:
-- We can use a "Array" as the unified stream structure. When we use a pure
-- cons we can increase the count of buffered elements in the stream, when we
-- use uncons we decrement the count. If the count goes beyond a threshold we
-- vectorize the buffered part. So if we are accessing an index at the end of
-- the stream and still want to hold on to the stream elements then we would be
-- buffering it by consing the elements and therefore automatically vectorizing
-- it. By consing we are joining an evaluated part with a potentially
-- unevaluated tail therefore strictizing the stream. When we uncons we are
-- actually taking out a vector (i.e. evaluated part, WHNF of course) from the
-- stream.

-------------------------------------------------------------------------------
-- Array of Arrays
-------------------------------------------------------------------------------

-- A dynamic array stored in a tree structure can be called a "store".
--
-- Storable a
-- data Store a =
--    Leaf Int (ForeignPtr a)
--  | Indirect Int Int (Store (Store a)) -- Indirect Size TreeLevel Tree
--
-- The Leaf constructor can be thought of as a stream of single array Tree can
-- make the structure hierarchical, we can have arrays inside arrays up to many
-- levels making it a tree. The level of the tree depends on the block size of
-- the array We can reduce the level by increasing the block size.
--
-- The block size of a chunk can either be constant or variable. Constant block
-- size would require compacting, whereas variable block size would require
-- more work when searching/accessing an element. To reduce the access overhead
-- we can use a B+ tree for variable sized blocks.
--
-- Use rewrite rules to rewrite array from and to stream ops to id.

-------------------------------------------------------------------------------
-- Nesting/Layers
-------------------------------------------------------------------------------

-- A stream of arrays can be grouped to create arrays of arrays i.e. a tree
-- of arrays. A tree of arrays can be concated to reduce the level of the
-- tree or turn it into a single array.

--  When converting a whole stream to a single array, we can keep adding new
--  levels to an array tree, creating arrays of arrays so that we do not have
--  to keep reallocating and copying the old data to new buffers. We can later
--  reduce the levels by compacting the tree if we want to. The 'limit'
--  argument is to raise an exception if the total size exceeds this limit,
--  this is a safety catch so that we do not vectorize infinite streams and
--  then run out of memory.
--
-- We can keep on group folding a stream until we get a singleton stream.

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- We require that an array stores only Storable. Array is used for buffering
-- while streams are used for processing. If you want something to be buffered
-- it better be Storable so that we can store it in non-GC memory.
--
-- XXX Do we need some alignment for the allocations?
-- XXX add reverse flag to reverse the contents without physically reversing.
data Array a = Storable a => Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused address
    , aBound :: {-# UNPACK #-} !(Ptr a)        -- first address beyond allocated memory
    }

type ByteArray = Array Word8

-- sizeOf :: Array a -> Int
-- sizeOf = vSize

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

-- XXX we are converting Int to CSize
memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = c_memcpy p q (fromIntegral s) >> return ()

{-# INLINE unsafeDangerousPerformIO #-}
unsafeDangerousPerformIO :: IO a -> a
unsafeDangerousPerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- XXX do we need alignment?
-- XXX what if count is 0?
--
{-# INLINE withNewArray #-}
withNewArray :: forall a. Storable a => Int -> (Ptr a -> IO ()) -> IO (Array a)
withNewArray count f = do
    let size = count * sizeOf (undefined :: a)
    fptr <- mallocPlainForeignPtrBytes size
    withForeignPtr fptr $ \p -> do
        f p
        return $ Array
            { aStart = fptr
            , aEnd   = p
            , aBound = p `plusPtr` size
            }

-- | Allocate memory for an array that can hold 'count' items. Note that this
-- is internal routine, the reference to this array cannot be given out until
-- the array has been written to and frozen. However, if we are using the array
-- as a mutable array in IO monad then the reference can be used. We should
-- remember that the array is uninitialized.
{-# INLINE unsafeNew #-}
unsafeNew :: forall a. Storable a => Int -> IO (Array a)
unsafeNew count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- mallocPlainForeignPtrBytes size
    let p = unsafeForeignPtrToPtr fptr
    return $ Array
        { aStart = fptr
        , aEnd   = p
        , aBound = p `plusPtr` size
        }

-- Internal routine for when the array is being created. Appends one item at
-- the end of the array. Useful when sequentially writing a stream to the
-- array.
-- XXX grow the array when we are beyond bound.
{-# INLINE unsafeAppend #-}
unsafeAppend :: forall a. Storable a => Array a -> a -> IO (Array a)
unsafeAppend vec@Array{..} x = do
    when (aEnd == aBound) $ error "unsafeAppend: writing beyond bounds"
    poke aEnd x
    touchForeignPtr aStart
    return $ vec {aEnd = aEnd `plusPtr` (sizeOf (undefined :: a))}

-- | Remove the free space from an Array.
shrinkToFit :: forall a. Array a -> IO (Array a)
shrinkToFit vec@Array{..} = do
    assert (aEnd <= aBound) (return ())
    if aEnd /= aBound
    then do
        let oldStart = unsafeForeignPtrToPtr aStart
        let size = aEnd `minusPtr` oldStart
        newPtr <- mallocPlainForeignPtrBytes size
        withForeignPtr newPtr $ \pNew -> do
            memcpy (castPtr pNew) (castPtr oldStart) size
            touchForeignPtr aStart
            let end = pNew `plusPtr` size
            return $ Array
                { aStart = newPtr
                , aEnd   = end
                , aBound = end
                }
    else return vec

-- Note that the address must be a read-only address (meant to be used for
-- read-only string literals) because we are sharing it, any modification to the
-- original address would change our array. That's why this function is
-- unsafe.
{-# INLINE fromCStringAddrUnsafe #-}
fromCStringAddrUnsafe :: Addr# -> IO ByteArray
fromCStringAddrUnsafe addr# = do
    ptr <- newForeignPtr_ (castPtr cstr)
    len <- c_strlen cstr
    let n = fromIntegral len
    let p = unsafeForeignPtrToPtr ptr
    let end = p `plusPtr` n
    return $ Array
        { aStart = ptr
        , aEnd   = end
        , aBound = end
        }
  where
    cstr :: CString
    cstr = Ptr addr#

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Return element at the specified index without checking the bounds.
{-# INLINE unsafeIndex #-}
unsafeIndex :: forall a. Storable a => Array a -> Int -> a
unsafeIndex Array {..} i =
    let !r = unsafeDangerousPerformIO $
             withForeignPtr aStart $ \p -> do
                let elemSize = sizeOf (undefined :: a)
                    elemOff = p `plusPtr` (elemSize * i)
                assert (i >= 0 && elemOff `plusPtr` elemSize <= aEnd) (return ())
                peek elemOff
    in r

{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length Array{..} =
    let p = unsafeForeignPtrToPtr aStart
        aLen = aEnd `minusPtr` p
    in assert (aLen >= 0) (aLen `div` sizeOf (undefined :: a))

{-# INLINE foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z Array{..} =
    unsafeDangerousPerformIO $ withForeignPtr aStart $ \p -> go z p aEnd
    where
      go !acc !p !q
        | p == q = return acc
        | otherwise = do
            x <- peek p
            go (f acc x) (p `plusPtr` sizeOf (undefined :: a)) q

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

{-# INLINE toStreamD #-}
toStreamD :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamD Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        let !x = unsafeDangerousPerformIO $ do
                    r <- peek p
                    -- XXX should we keep aStart in the state?
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` (sizeOf (undefined :: a)))

{-# INLINABLE toList #-}
toList :: Storable a => Array a -> [a]
toList = runIdentity . D.toList . toStreamD

instance (Storable a, Show a) => Show (Array a) where
    {-# INLINE showsPrec #-}
    showsPrec _ = shows . toList

-- | Create an 'Array' of a given size from a list.
{-# INLINABLE fromListN #-}
fromListN :: Storable a => Int -> [a] -> Array a
fromListN n xs = foldl step begin xs

    where

    begin      = let !x = unsafeDupablePerformIO $ unsafeNew n in x
    step arr x = let !arr' = unsafeDangerousPerformIO (unsafeAppend arr x)
                 in arr'

-- | Create an 'Array' from a list. List must be finite.
{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = fromListN (Prelude.length xs) xs

instance (Storable a, Read a, Show a) => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = do
          xs <- readPrec
          return (fromList xs)
    readListPrec = readListPrecDefault
