{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Prim.Mut.Types
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Prim.Mut.Types
    (
      Array (..)

    -- * Construction
    , newArray
    , unsafeWriteIndex

    , spliceTwo
    , unsafeCopy

    , fromListM
    , fromListNM
    , fromStreamDN
    , fromStreamD

    -- * Streams of arrays
    , fromStreamDArraysOf

    , packArraysChunksOf
    , lpackArraysChunksOf

#if !defined(mingw32_HOST_OS)
--    , groupIOVecsOf
#endif

    -- * Elimination
    , unsafeReadIndex
    , length
    , byteLength

    , writeN
    , ArrayUnsafe(..)
    , writeNUnsafe
    , write

    -- * Utilities
    , resizeArray
    , shrinkArray
    )
where

#include "Streamly/Internal/Data/Array/Prim/MutTypesInclude.hs"

-------------------------------------------------------------------------------
-- Allocation (Unpinned)
-------------------------------------------------------------------------------

-- | Allocate an array that is unpinned and can hold 'count' items.  The memory
-- of the array is uninitialized.
--
-- Note that this is internal routine, the reference to this array cannot be
-- given out until the array has been written to and frozen.
{-# INLINE newArray #-}
newArray ::
       forall m a. (MonadIO m, Prim a)
    => Int
    -> m (Array a)
newArray (I# n#) =
    liftIO $ do
        let bytes = n# *# sizeOf# (undefined :: a)
        primitive $ \s# ->
            case newByteArray# bytes s# of
                (# s1#, arr# #) -> (# s1#, Array arr# #)

-- | Resize (unpinned) mutable byte array to new specified size (in elem
-- count). The returned array is either the original array resized in-place or,
-- if not possible, a newly allocated (unpinned) array (with the original
-- content copied over).
{-# INLINE resizeArray #-}
resizeArray ::
       forall m a. (MonadIO m, Prim a)
    => Array a
    -> Int -- ^ new size in elem count
    -> m (Array a)
resizeArray (Array arr#) (I# n#) =
    liftIO $ do
        let bytes = n# *# sizeOf# (undefined :: a)
        primitive $ \s# ->
            case resizeMutableByteArray# arr# bytes s# of
                (# s1#, arr1# #) -> (# s1#, Array arr1# #)
