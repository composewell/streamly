-- |
-- Module      : Streamly.Internal.Data.Array.Storable.Pinned.SIMD
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Storable.Pinned.SIMD
    (
    )
where


-- We could stream the array as vector types and operate on the stream.
-- However, we can also operate on the whole array dividing it into vectors and
-- using FFI to operate on it. This way we do not need to depend on llvm. The
-- FFI overhead would be lower as the array size increases.

-------------------------------------------------------------------------------
-- Elements of a vector
-------------------------------------------------------------------------------

-- An element of a vector type
class Storable a => IsElem a where

instance IsElem Word8 where
instance IsElem Word16 where
instance IsElem Word32 where
instance IsElem Word64 where

instance IsElem Int8 where
instance IsElem Int16 where
instance IsElem Int32 where
instance IsElem Int64 where

instance IsElem Float where
instance IsElem Double where

-------------------------------------------------------------------------------
-- Vector types
-------------------------------------------------------------------------------

class (Storable a, IsElem b) => IsVector a b where
    elemCount :: a -> Int
    elemSize :: a -> Int

    -- Construction
    type tuple
    packV :: tuple -> a
    unpackV :: a -> tuple

    type scalar -- broadcast scalar
    broadcastV :: scalar -> a
    insertV :: a -> scalar -> Int -> a

    -- Operations
    plusV :: a -> a -> a
    minusV :: a -> a -> a
    timesV :: a -> a -> a
    negateV :: a -> a -> a
    -- equals :: a -> a -> Bool

    -- Read from Array
    indexArray :: Array Word8 -> Int -> a
    readArray :: MArray Word8 -> Int -> m a
    readOffArray :: MArray Word8 -> Int -> m a

    -- Update in Array
    writeArray :: MArray Word8 -> Int -> a -> m ()
    indexOffArray :: Array Word8 -> Int -> a
    writeOffArray :: MArray Word8 -> Int -> a -> m ()

class IsVector a b => IsIntVector a b where
    quotV :: a -> a -> a
    remV :: a -> a -> a

class IsVector a b => IsFloatVector a b where
    divide :: a -> a -> a

-- 128 bit instructions
data Int8X16 = Int8X16 Int8X16#

instance IsVector Int8X16 where
    elemCount = 16
    elemSize = 8

-- 256 bit instructions
data Int8X32 = Int8X32 Int8X32#

-- 512 bit instructions
data Int8X64 = Int8X64 Int8X64#

-------------------------------------------------------------------------------
-- Construction of Vector types
-------------------------------------------------------------------------------

-- | Cast an array of storables into an array of vectors and a residual array.
-- We can then use the A.read to turn it into a stream of vectors.
-- We can then use vector operations on the stream to transform it and folds to
-- fold the resulting stream.
--
-- type b is statically chosen based on the instructions available or build
-- configuration.
castArray :: (IsElem a, IsVector b) => Array a -> (Array b, Array a)

-------------------------------------------------------------------------------
-- Array operations
-------------------------------------------------------------------------------

{-# INLINE map #-}
map :: (IsVector a, IsVector b) => (a -> b) -> Array a -> Array b
map f arr = undefined

-- Map a vector array to a vector stream and fold it using a Fold. For example,
-- we may want to compare the elements of a Word8 array with '\n' and fold the
-- results into a bitarray that tells where newline occurs in the input..
--
{-# INLINE foldMap #-}
foldMap :: (IsVector a, IsVector b) => Fold m b c -> (a -> b) -> Array a -> m c
foldMap fld f arr = undefined
