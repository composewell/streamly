{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Array.Types
    (
      Array (..)

    -- * Construction
    , unsafeInlineIO
    , withNewArray
    , unsafeNew
    , unsafeAppend
    , shrinkToFit
    , memcpy

    , fromCStringAddrUnsafe
    , fromList
    , fromListN
    , fromStreamDN

    -- * Elimination
    , unsafeIndex
    , length
    , foldl'
    , foldr

    , toStreamD
    , toList

    -- * Utilities
    , defaultChunkSize
    )
where

import Control.Exception (assert)
import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..), CInt(..))
import Foreign.ForeignPtr
       (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import Prelude hiding (length, foldr)
import Text.Read (readPrec, readListPrec, readListPrecDefault)

import GHC.Base (Addr#, realWorld#)
import GHC.Exts (IsList, IsString(..))
import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes, newForeignPtr_)
import GHC.IO (IO(IO), unsafeDupablePerformIO)
import GHC.Ptr (Ptr(..))

import qualified Prelude
import qualified Streamly.Streams.StreamD.Type as D
import qualified GHC.Exts as Exts

-------------------------------------------------------------------------------
-- Design Notes
-------------------------------------------------------------------------------

-- There are two goals that we want to fulfill with arrays.  One, holding large
-- amounts of data in non-GC memory and the ability to pass this data to and
-- from the operating system without an extra copy overhead. Two, allow random
-- access to elements based on their indices. The first one falls in the
-- category of storage buffers while the second one falls in the category of
-- maps/multisets/hashmaps.
--
-- For the first requirement we use an array of Storables and store it in a
-- ForeignPtr. We can have both immutable and mutable variants of this array
-- using wrappers over the same underlying type.
--
-- For the second requirement, we need a separate type for arrays of
-- polymorphic values, for example vectors of handler functions, lookup tables.
-- We can call this type a "vector" in contrast to arrays.  It should not
-- require Storable instance for the type. In that case we need to use an
-- Array# instead of a ForeignPtr. This type of array would not reduce the GC
-- overhead much because each element of the array still needs to be scanned by
-- the GC.  However, it can store polymorphic elements and allow random access
-- to those.  But in most cases random access means storage, and it means we
-- need to avoid GC scanning except in cases of trivially small storage. One
-- way to achieve that would be to put the array in a Compact region. However,
-- if and when we mutate this, we will have to use a manual GC copying out to
-- another CR and freeing the old one.

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- We require that an array stores only Storable. Arrays are used for buffering
-- data while streams are used for processing. If you want something to be
-- buffered it better be Storable so that we can store it in non-GC memory.
--
-- We can use a "Storable" constraint in the Array type and the Constraint can
-- be automatically provided to a function that pattern matches on the Array
-- type. However, it has huge performance cost, so we do not use it.
--
-- XXX Another way to not require the Storable constraint in array operations
-- is to store the elemSize in the array at construction and use that instead
-- of using sizeOf. Need to charaterize perf cost of this.
--
-- XXX add reverse flag to reverse the contents without physically reversing.
data Array a =
#ifdef DEVBUILD
    Storable a =>
#endif
    Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused address
    , aBound :: {-# UNPACK #-} !(Ptr a)        -- first address beyond allocated memory
    }

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

-- XXX we are converting Int to CSize
memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy dst src len = c_memcpy dst src (fromIntegral len) >> return ()

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- XXX we are converting Int to CSize
-- return True if the memory locations have identical contents
{-# INLINE memcmp #-}
memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
memcmp p1 p2 len = do
    r <- c_memcmp p1 p2 (fromIntegral len)
    return $ r == 0

{-# INLINE unsafeInlineIO #-}
unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Allocate an array that can hold 'count' items.  The memory of the array is
-- uninitialized.
--
-- Note that this is internal routine, the reference to this array cannot be
-- given out until the array has been written to and frozen.
{-# INLINE unsafeNew #-}
unsafeNew :: forall a. Storable a => Int -> IO (Array a)
unsafeNew count = do
    let size = count * sizeOf (undefined :: a)
    fptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: a))
    let p = unsafeForeignPtrToPtr fptr
    return $ Array
        { aStart = fptr
        , aEnd   = p
        , aBound = p `plusPtr` size
        }

-- | Allocate an Array of the given size and run an IO action passing the array
-- start pointer.
{-# INLINE withNewArray #-}
withNewArray :: forall a. Storable a => Int -> (Ptr a -> IO ()) -> IO (Array a)
withNewArray count f = do
    arr <- unsafeNew count
    withForeignPtr (aStart arr) $ \p -> f p >> return arr

-- XXX grow the array when we are beyond bound.
--
-- Internal routine for when the array is being created. Appends one item at
-- the end of the array. Useful when sequentially writing a stream to the
-- array. DOES NOT CHECK THE ARRAY BOUNDS.
{-# INLINE unsafeAppend #-}
unsafeAppend :: forall a. Storable a => Array a -> a -> IO (Array a)
unsafeAppend arr@Array{..} x = do
    when (aEnd == aBound) $
        error "BUG: unsafeAppend: writing beyond array bounds"
    poke aEnd x
    touchForeignPtr aStart
    return $ arr {aEnd = aEnd `plusPtr` (sizeOf (undefined :: a))}

-- | Remove the free space from an Array.
shrinkToFit :: forall a. Storable a => Array a -> IO (Array a)
shrinkToFit arr@Array{..} = do
    assert (aEnd <= aBound) (return ())
    if aEnd /= aBound
    then do
        let oldStart = unsafeForeignPtrToPtr aStart
        let size = aEnd `minusPtr` oldStart
        newPtr <- mallocPlainForeignPtrAlignedBytes
                    size (alignment (undefined :: a))
        withForeignPtr newPtr $ \pNew -> do
            memcpy (castPtr pNew) (castPtr oldStart) size
            touchForeignPtr aStart
            let end = pNew `plusPtr` size
            return $ Array
                { aStart = newPtr
                , aEnd   = end
                , aBound = end
                }
    else return arr

-- Note that the address must be a read-only address (meant to be used for
-- read-only string literals) because we are sharing it, any modification to
-- the original address would change our array. That's why this function is
-- unsafe.
{-# INLINE fromCStringAddrUnsafe #-}
fromCStringAddrUnsafe :: Addr# -> IO (Array Word8)
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
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall a. Storable a => Array a -> Int -> a
unsafeIndex Array {..} i =
    let !r = unsafeInlineIO $
             withForeignPtr aStart $ \p -> do
                let elemSize = sizeOf (undefined :: a)
                    elemOff = p `plusPtr` (elemSize * i)
                assert (i >= 0 && elemOff `plusPtr` elemSize <= aEnd)
                       (return ())
                peek elemOff
    in r

-- | /O(1)/ Get the length of the array.
--
-- @since 0.7.0
{-# INLINE length #-}
length :: forall a. Storable a => Array a -> Int
length Array{..} =
    let p = unsafeForeignPtrToPtr aStart
        aLen = aEnd `minusPtr` p
    in assert (aLen >= 0) (aLen `div` sizeOf (undefined :: a))

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: forall m a. (Monad m, Storable a) => Array a -> D.Stream m a
toStreamD Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` (sizeOf (undefined :: a)))

{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: Storable a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (Monad m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    let !arr = unsafeDupablePerformIO $ unsafeNew limit
    end <- D.foldl' write (aEnd arr) $ D.take limit str
    return $ arr {aEnd = end}

    where

    write ptr x =
        let !_ = unsafeInlineIO $ poke ptr x
         in ptr `plusPtr` sizeOf (undefined :: a)

-- | Convert an 'Array' into a list.
--
-- @since 0.7.0
{-# INLINABLE toList #-}
toList :: Storable a => Array a -> [a]
toList = foldr (:) []

instance (Show a, Storable a) => Show (Array a) where
    {-# INLINE showsPrec #-}
    showsPrec _ = shows . toList

-- | Create an 'Array' from the first N elements of a list. The array is
-- allocated to size N, if the list terminates before N elements then the
-- array may hold less than N elements.
--
-- @since 0.7.0
{-# INLINABLE fromListN #-}
fromListN :: Storable a => Int -> [a] -> Array a
fromListN n xs = runIdentity $ fromStreamDN n $ D.fromList xs

-- | Create an 'Array' from a list. The list must be of finite size.
--
-- @since 0.7.0
{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = fromListN (Prelude.length xs) xs

instance (Storable a, Read a, Show a) => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = do
          xs <- readPrec
          return (fromList xs)
    readListPrec = readListPrecDefault

instance (a ~ Char) => IsString (Array a) where
    {-# INLINE fromString #-}
    fromString = fromList

-- GHC versions 8.0 and below cannot derive IsList
instance Storable a => IsList (Array a) where
    type (Item (Array a)) = a
    {-# INLINE fromList #-}
    fromList = fromList
    {-# INLINE fromListN #-}
    fromListN = fromListN
    {-# INLINE toList #-}
    toList = toList

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

instance (Storable a, NFData a) => NFData (Array a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ x -> rnf x) ()

instance (Storable a, Ord a) => Ord (Array a) where
    {-# INLINE compare #-}
    compare arr1 arr2 = runIdentity $
        D.cmpBy compare (toStreamD arr1) (toStreamD arr2)

    -- Default definitions defined in base do not have an INLINE on them, so we
    -- replicate them here with an INLINE.
    {-# INLINE (<) #-}
    x <  y = case compare x y of { LT -> True;  _ -> False }

    {-# INLINE (<=) #-}
    x <= y = case compare x y of { GT -> False; _ -> True }

    {-# INLINE (>) #-}
    x >  y = case compare x y of { GT -> True;  _ -> False }

    {-# INLINE (>=) #-}
    x >= y = case compare x y of { LT -> False; _ -> True }

    -- These two default methods use '<=' rather than 'compare'
    -- because the latter is often more expensive
    {-# INLINE max #-}
    max x y = if x <= y then y else x

    {-# INLINE min #-}
    min x y = if x <= y then x else y

#ifdef DEVBUILD
-- Definitions using the Storable constraint from the Array type. These are to
-- make the Foldable instance possible though it is much slower (7x slower).
--
{-# INLINE_NORMAL toStreamD_ #-}
toStreamD_ :: forall m a. Monad m => Int -> Array a -> D.Stream m a
toStreamD_ size Array{..} =
    let p = unsafeForeignPtrToPtr aStart
    in D.Stream step p

    where

    {-# INLINE_LATE step #-}
    step _ p | p == aEnd = return D.Stop
    step _ p = do
        let !x = unsafeInlineIO $ do
                    r <- peek p
                    touchForeignPtr aStart
                    return r
        return $ D.Yield x (p `plusPtr` size)

{-# INLINE_NORMAL _foldr #-}
_foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
_foldr f z arr@Array {..} =
    let !n = sizeOf (undefined :: a)
    in runIdentity $ D.foldr f z $ toStreamD_ n arr

-- | Note that the 'Foldable' instance is 7x slower than the direct
-- operations.
instance Foldable Array where
  foldr = _foldr
#endif

-------------------------------------------------------------------------------
-- Semigroup and Monoid
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

-- | GHC memory management allocation header overhead
allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

-- | Default maximum buffer size in bytes, for reading from and writing to IO
-- devices, the value is 32KB minus GHC allocation overhead, which is a few
-- bytes, so that the actual allocation is 32KB.
defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024
