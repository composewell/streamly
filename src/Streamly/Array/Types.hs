{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

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
    )
where

import Control.Exception (assert)
import Control.Monad (when)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr
       (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

import GHC.Base (Addr#, nullAddr#, realWorld#)
import GHC.ForeignPtr (ForeignPtr(..), mallocPlainForeignPtrBytes, touchForeignPtr, newForeignPtr_)
import GHC.IO (IO(IO))
import GHC.Ptr (Ptr(..))

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- We require that a vector stores only Storable. Array is used for buffering
-- while streams are used for processing. If you want something to be buffered
-- it better be Storable so that we can store it in non-GC memory.
--
-- XXX We can perhaps rename a Array of Storable as a "Buffer" instead and use
-- Array for vectors of polymorphic values. So that we explicitly know that it
-- can be buffered. Though Buffer does not indicate the array/vector nature of
-- the data structure. How about SArray? Or we can call it an Array or even
-- List? Array indicates the imperative/storable nature, therefore is perhaps
-- the best term for this.
--
-- We also need a separate type for arrays of polymorphic values, for example
-- vectors of handler functions, lookup tables.
--
-- Storable a
-- data Array a =
--    VLeaf Int (ForeignPtr a)
--  | VIndirect Int Int (Array (Array a)) -- VIndirect Size TreeLevel Tree
--
-- The VLeaf constructor can be thought of as a stream of single vector.
-- VTree can make the structure hierarchical, we can have vectors inside
-- vectors up to many levels making it a tree. The level of the tree depends on
-- the block size of the vector. We can reduce the level by increasing the
-- block size.
--
-- The block size of a chunk can either be constant or variable. Constant block
-- size would require compacting, whereas variable block size would require
-- more work when searching/accessing an element. To reduce the access overhead
-- we can use a B+ tree for variable sized blocks.
--
-- Use rewrite rules to rewrite vector from and to stream ops to id.

-- XXX Do we need some alignment for the allocations?
-- XXX add reverse flag to reverse the contents without actually reversing.
data Array a = Array
    { aStart :: {-# UNPACK #-} !(ForeignPtr a) -- first address
    , aEnd   :: {-# UNPACK #-} !(Ptr a)        -- first unused address
    , aBound :: {-# UNPACK #-} !(Ptr a)        -- first address beyond allocated memory
    }

type ByteArray = Array Word8

-- XXX need Storable instance for vector
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
-- as a mutable array in IO monad then the reference can be used. However, we
-- should remeber that the array is uninitialized.
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
    -- XXX do we need a touch here?
    return $ vec {aEnd = aEnd `plusPtr` (sizeOf (undefined :: a))}

shrinkToFit :: forall a. Storable a => Array a -> IO (Array a)
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
-- read-only string literals) because we are sharing it any modification to the
-- original address would change our vector. That's why this function is
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
