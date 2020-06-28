{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Data.Prim.Array.Types
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : streamly@composewell.com
-- Portability : non-portable
--
-- Arrays of unboxed primitive types. The function provided by this module
-- match the behavior of those provided by @Data.Primitive.ByteArray@, and
-- the underlying types and primops that back them are the same.
-- However, the type constructors 'PrimArray' and 'MutablePrimArray' take one additional
-- argument than their respective counterparts 'ByteArray' and 'MutableByteArray'.
-- This argument is used to designate the type of element in the array.
-- Consequently, all function this modules accepts length and incides in
-- terms of elements, not bytes.
--
-- @since 0.6.4.0
module Streamly.Internal.Data.Prim.Array.Types
  ( -- * Types
    PrimArray(..)
  , MutablePrimArray(..)
    -- * Allocation
  , newPrimArray
  , resizeMutablePrimArray
  , shrinkMutablePrimArray
    -- * Element Access
  , writePrimArray
  , indexPrimArray
    -- * Freezing and Thawing
  , unsafeFreezePrimArray
    -- * Information
  , sizeofPrimArray
    -- * Folding
  , foldrPrimArray
  , foldlPrimArray'
  ) where

import GHC.Exts

import Data.Primitive.Types
import Data.Primitive.ByteArray (ByteArray(..))
import Control.Monad.Primitive
import qualified Data.Primitive.ByteArray as PB

-- | Arrays of unboxed elements. This accepts types like 'Double', 'Char',
-- 'Int', and 'Word', as well as their fixed-length variants ('Word8',
-- 'Word16', etc.). Since the elements are unboxed, a 'PrimArray' is strict
-- in its elements. This differs from the behavior of 'Array', which is lazy
-- in its elements.
data PrimArray a = PrimArray ByteArray#

-- | Mutable primitive arrays associated with a primitive state token.
-- These can be written to and read from in a monadic context that supports
-- sequencing such as 'IO' or 'ST'. Typically, a mutable primitive array will
-- be built and then convert to an immutable primitive array using
-- 'unsafeFreezePrimArray'. However, it is also acceptable to simply discard
-- a mutable primitive array since it lives in managed memory and will be
-- garbage collected when no longer referenced.
data MutablePrimArray s a = MutablePrimArray (MutableByteArray# s)

sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ()) of
      r -> isTrue# r

-- | @since 0.6.4.0
instance (Eq a, Prim a) => Eq (PrimArray a) where
  a1@(PrimArray ba1#) == a2@(PrimArray ba2#)
    | sameByteArray ba1# ba2# = True
    | sz1 /= sz2 = False
    | otherwise = loop (quot sz1 (sizeOf (undefined :: a)) - 1)
    where
    -- Here, we take the size in bytes, not in elements. We do this
    -- since it allows us to defer performing the division to
    -- calculate the size in elements.
    sz1 = PB.sizeofByteArray (ByteArray ba1#)
    sz2 = PB.sizeofByteArray (ByteArray ba2#)
    loop !i
      | i < 0 = True
      | otherwise = indexPrimArray a1 i == indexPrimArray a2 i && loop (i-1)
  {-# INLINE (==) #-}

-- | Lexicographic ordering. Subject to change between major versions.
--
--   @since 0.6.4.0
instance (Ord a, Prim a) => Ord (PrimArray a) where
  compare a1@(PrimArray ba1#) a2@(PrimArray ba2#)
    | sameByteArray ba1# ba2# = EQ
    | otherwise = loop 0
    where
    cmp LT _ = LT
    cmp EQ y = y
    cmp GT _ = GT
    sz1 = PB.sizeofByteArray (ByteArray ba1#)
    sz2 = PB.sizeofByteArray (ByteArray ba2#)
    sz = quot (min sz1 sz2) (sizeOf (undefined :: a))
    loop !i
      | i < sz = compare (indexPrimArray a1 i) (indexPrimArray a2 i) `cmp` loop (i+1)
      | otherwise = compare sz1 sz2
  {-# INLINE compare #-}

-- | @since 0.6.4.0
instance (Show a, Prim a) => Show (PrimArray a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofPrimArray a) . showString " "
      . shows (primArrayToList a)

-- | Convert the primitive array to a list.
{-# INLINE primArrayToList #-}
primArrayToList :: forall a. Prim a => PrimArray a -> [a]
primArrayToList xs = build (\c n -> foldrPrimArray c n xs)

-- | Create a new mutable primitive array of the given length. The
-- underlying memory is left uninitialized.
newPrimArray :: forall m a. (PrimMonad m, Prim a) => Int -> m (MutablePrimArray (PrimState m) a)
{-# INLINE newPrimArray #-}
newPrimArray (I# n#)
  = primitive (\s# ->
      case newByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, MutablePrimArray arr# #)
    )

-- | Resize a mutable primitive array. The new size is given in elements.
--
-- This will either resize the array in-place or, if not possible, allocate the
-- contents into a new, unpinned array and copy the original array\'s contents.
--
-- To avoid undefined behaviour, the original 'MutablePrimArray' shall not be
-- accessed anymore after a 'resizeMutablePrimArray' has been performed.
-- Moreover, no reference to the old one should be kept in order to allow
-- garbage collection of the original 'MutablePrimArray' in case a new
-- 'MutablePrimArray' had to be allocated.
resizeMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m (MutablePrimArray (PrimState m) a)
{-# INLINE resizeMutablePrimArray #-}
resizeMutablePrimArray (MutablePrimArray arr#) (I# n#)
  = primitive (\s# -> case resizeMutableByteArray# arr# (n# *# sizeOf# (undefined :: a)) s# of
                        (# s'#, arr'# #) -> (# s'#, MutablePrimArray arr'# #))

-- Although it is possible to shim resizeMutableByteArray for old GHCs, this
-- is not the case with shrinkMutablePrimArray.

-- | Shrink a mutable primitive array. The new size is given in elements.
-- It must be smaller than the old size. The array will be resized in place.
-- This function is only available when compiling with GHC 7.10 or newer.
shrinkMutablePrimArray :: forall m a. (PrimMonad m, Prim a)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ new size
  -> m ()
{-# INLINE shrinkMutablePrimArray #-}
shrinkMutablePrimArray (MutablePrimArray arr#) (I# n#)
  = primitive_ (shrinkMutableByteArray# arr# (n# *# sizeOf# (undefined :: a)))

-- | Write an element to the given index.
writePrimArray ::
     (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ array
  -> Int -- ^ index
  -> a -- ^ element
  -> m ()
{-# INLINE writePrimArray #-}
writePrimArray (MutablePrimArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezePrimArray
  :: PrimMonad m => MutablePrimArray (PrimState m) a -> m (PrimArray a)
{-# INLINE unsafeFreezePrimArray #-}
unsafeFreezePrimArray (MutablePrimArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, PrimArray arr'# #))

-- | Read a primitive value from the primitive array.
indexPrimArray :: forall a. Prim a => PrimArray a -> Int -> a
{-# INLINE indexPrimArray #-}
indexPrimArray (PrimArray arr#) (I# i#) = indexByteArray# arr# i#

-- | Get the size, in elements, of the primitive array.
sizeofPrimArray :: forall a. Prim a => PrimArray a -> Int
{-# INLINE sizeofPrimArray #-}
sizeofPrimArray (PrimArray arr#) = I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

-- | Lazy right-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldrPrimArray #-}
foldrPrimArray :: forall a b. Prim a => (a -> b -> b) -> b -> PrimArray a -> b
foldrPrimArray f z arr = go 0
  where
    !sz = sizeofPrimArray arr
    go !i
      | sz > i = f (indexPrimArray arr i) (go (i+1))
      | otherwise = z

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlPrimArray' #-}
foldlPrimArray' :: forall a b. Prim a => (b -> a -> b) -> b -> PrimArray a -> b
foldlPrimArray' f z0 arr = go 0 z0
  where
    !sz = sizeofPrimArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexPrimArray arr i))
      | otherwise = acc
