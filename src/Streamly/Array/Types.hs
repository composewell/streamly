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
    , newArray
    , unsafeAppend
    , shrinkToFit
    , memcpy
    , memcmp

    , fromList
    , fromListN
    , fromStreamDN
    , fromStreamD
    , fromStreamDArraysOf
    , flattenArrays
    , flattenArraysRev

    -- * Elimination
    , unsafeIndexIO
    , unsafeIndex
    , length
    , foldl'
    , foldr

    , toStreamD
    , toStreamDRev
    , toList
    , toArrayN

    -- * Utilities
    , defaultChunkSize
    , mkChunkSizeKB
    )
where

import Control.Exception (assert)
import Control.DeepSeq (NFData(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
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
import GHC.IO (IO(IO), unsafePerformIO)
import GHC.Ptr (Ptr(..))

import Streamly.Fold.Types (Fold(..))
import Streamly.SVar (adaptState)

import qualified Streamly.Streams.StreamD.Type as D
import qualified Streamly.Streams.StreamK as K
import qualified GHC.Exts as Exts

#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif

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
-- XXX rename the fields to "start, next, end".
--
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
{-# INLINE newArray #-}
newArray :: forall a. Storable a => Int -> IO (Array a)
newArray count = do
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
    arr <- newArray count
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

-- XXX when converting an array of Word8 from a literal string we can simply
-- refer to the literal string. Is it possible to write rules such that
-- fromList Word8 can be rewritten so that GHC does not first convert the
-- literal to [Char] and then we convert it back to an Array Word8?
--
-- Note that the address must be a read-only address (meant to be used for
-- read-only string literals) because we are sharing it, any modification to
-- the original address would change our array. That's why this function is
-- unsafe.
{-# INLINE _fromCStringAddrUnsafe #-}
_fromCStringAddrUnsafe :: Addr# -> IO (Array Word8)
_fromCStringAddrUnsafe addr# = do
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
--
-- Unsafe because it does not check the bounds of the array.
{-# INLINE_NORMAL unsafeIndexIO #-}
unsafeIndexIO :: forall a. Storable a => Array a -> Int -> IO a
unsafeIndexIO Array {..} i =
     withForeignPtr aStart $ \p -> do
        let elemSize = sizeOf (undefined :: a)
            elemOff = p `plusPtr` (elemSize * i)
        assert (i >= 0 && elemOff `plusPtr` elemSize <= aEnd)
               (return ())
        peek elemOff

-- | Return element at the specified index without checking the bounds.
{-# INLINE_NORMAL unsafeIndex #-}
unsafeIndex :: forall a. Storable a => Array a -> Int -> a
unsafeIndex arr i = let !r = unsafeInlineIO $ unsafeIndexIO arr i in r

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
        return $ D.Yield x (p `plusPtr` (sizeOf (undefined :: a)))

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

{-# INLINE_NORMAL foldl' #-}
foldl' :: forall a b. Storable a => (b -> a -> b) -> b -> Array a -> b
foldl' f z arr = runIdentity $ D.foldl' f z $ toStreamD arr

{-# INLINE_NORMAL foldr #-}
foldr :: Storable a => (a -> b -> b) -> b -> Array a -> b
foldr f z arr = runIdentity $ D.foldr f z $ toStreamD arr

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | @toArrayN n@ folds a maximum of @n@ elements from the input stream to an
-- 'Array'.
--
-- @since 0.7.0
{-# INLINE_NORMAL toArrayN #-}
toArrayN :: forall m a. (MonadIO m, Storable a) => Int -> Fold m a (Array a)
toArrayN n = Fold step initial extract

    where

    initial = liftIO $ newArray n
    step arr@(Array _ end bound) _ | end == bound = return arr
    step (Array start end bound) x = do
        liftIO $ poke end x
        return $ Array start (end `plusPtr` sizeOf (undefined :: a)) bound
    extract = return

{-
-- | Fold the input to a pure buffered stream (List) of arrays.
{-# INLINE toArrays #-}
toArrays :: (Monad m, Storable a) => Int -> Fold m a (List (Array a))
toArrays n = Fold step initial extract

-- This can be implemented by combining the List of arrays from toArrays into a
-- single array.
-- | Fold the whole input to a single array.
{-# INLINE toArray #-}
toArray :: forall m a. (Monad m, Storable a) => Fold m a (Array a)
toArray = Fold step begin done
-}

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN :: forall m a. (MonadIO m, Storable a)
    => Int -> D.Stream m a -> m (Array a)
fromStreamDN limit str = do
    arr <- liftIO $ newArray limit
    end <- D.foldlM' write (aEnd arr) $ D.take limit str
    return $ arr {aEnd = end}

    where

    write ptr x = do
        liftIO $ poke ptr x
        return $ ptr `plusPtr` sizeOf (undefined :: a)

-- | @fromStreamArraysOf n stream@ groups the input stream into a stream of
-- arrays of size n.
{-# INLINE fromStreamDArraysOf #-}
fromStreamDArraysOf :: (MonadIO m, Storable a)
    => Int -> D.Stream m a -> D.Stream m (Array a)
fromStreamDArraysOf n str = D.groupsOf n (toArrayN n) str

-- XXX concatMap does not seem to have the best possible performance so we have
-- a custom way to concat arrays.
data FlattenState s a =
      OuterLoop s
    | InnerLoop s (ForeignPtr a) (Ptr a) (Ptr a)

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
                            (p `plusPtr` (sizeOf (undefined :: a))) end)

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

-- CAUTION: a very large number (millions) of arrays can degrade performance
-- due to GC overhead because we need to buffer the arrays before we flatten
-- all the arrays.
--
-- We could take the approach of doubling the memory allocation on each
-- overflow. This would result in more or less the same amount of copying as in
-- the chunking approach. However, if we have to shrink in the end then it may
-- result in an extra copy of the entire data.
--
{-# INLINE fromStreamD #-}
fromStreamD :: (MonadIO m, Storable a) => D.Stream m a -> m (Array a)
fromStreamD m = do
    let s = fromStreamDArraysOf defaultChunkSize m
    buffered <- D.foldr K.cons K.nil s
    len <- K.foldl' (+) 0 (K.map length buffered)
    fromStreamDN len $ flattenArrays $ D.fromStreamK buffered

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
fromListN n xs = unsafePerformIO $ fromStreamDN n $ D.fromList xs

-- | Create an 'Array' from a list. The list must be of finite size.
--
-- @since 0.7.0
{-# INLINABLE fromList #-}
fromList :: Storable a => [a] -> Array a
fromList xs = unsafePerformIO $ fromStreamD $ D.fromList xs

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
    compare arr1 arr2 = unsafePerformIO $
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

{-# INLINE_NORMAL _foldr #-}
_foldr :: forall a b. (a -> b -> b) -> b -> Array a -> b
_foldr f z arr@Array {..} =
    let !n = sizeOf (undefined :: a)
    in unsafePerformIO $ D.foldr f z $ toStreamD_ n arr

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

mkChunkSizeKB :: Int -> Int
mkChunkSizeKB n = n * k - allocOverhead
   where k = 1024
