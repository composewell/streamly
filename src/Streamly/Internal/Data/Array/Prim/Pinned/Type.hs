{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Prim.Pinned.Type
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Prim.Pinned.Type
    (
      Array (..)
    , unsafeFreeze
    , unsafeFreezeWithShrink
--    , unsafeThaw
    , defaultChunkSize
    , nil

    -- * Construction
    , spliceTwo

    , fromList
    , fromListN
    , fromStreamDN
    , fromStreamD

    -- * Streams of arrays
    , fromStreamDArraysOf
    , FlattenState (..) -- for inspection testing
    , flattenArrays
    , flattenArraysRev
    , SpliceState (..) -- for inspection testing
    , packArraysChunksOf
    , lpackArraysChunksOf
#if !defined(mingw32_HOST_OS)
--    , groupIOVecsOf
#endif
    , splitOn
    , breakOn

    -- * Elimination
    , unsafeIndex
    , byteLength
    , length

    , foldl'
    , foldr
    , foldr'
    , foldlM'
    , splitAt

    , toStreamD
    , toStreamDRev
    , toStreamK
    , toStreamKRev
    , toList
--    , toArrayMinChunk
    , writeN
    , MA.ArrayUnsafe(..)
    , writeNUnsafe
    , write

    , unlines

    , toPtr

    , touchArray
    , withArrayAsPtr
    )
where

import Foreign.C.Types (CSize(..))
import GHC.IO (IO(..))
import Foreign.Ptr (minusPtr, nullPtr, plusPtr)

import qualified Streamly.Internal.Data.Array.Prim.Pinned.Mut.Type as MA

#include "Streamly/Internal/Data/Array/Prim/TypesInclude.hs"

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

-------------------------------------------------------------------------------
-- Using as a Pointer
-------------------------------------------------------------------------------

-- Change name later.
{-# INLINE toPtr #-}
toPtr :: Array a -> Ptr a
toPtr (Array arr# off _) = Ptr (byteArrayContents# arr#) `plusPtr` off

{-# INLINE touchArray #-}
touchArray :: Array a -> IO ()
touchArray (Array arr# _ _) = IO $ \s -> case touch# arr# s of s1 -> (# s1, () #)

{-# INLINE withArrayAsPtr #-}
withArrayAsPtr :: Array a -> (Ptr a -> IO b) -> IO b
withArrayAsPtr arr f = do
    r <- f (toPtr arr)
    touchArray arr
    return r

-- Drops the separator byte
{-# INLINE breakOn #-}
breakOn ::
       MonadIO m
    => Word8
    -> Array Word8
    -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr@(Array arr# off len) = do
    let p = toPtr arr
        loc = unsafePerformIO $ c_memchr p sep (fromIntegral (byteLength arr))
        len1 = loc `minusPtr` p
        len2 = len - len1 - 1
    return $
        if loc == nullPtr
        then (arr, Nothing)
        else ( Array arr# off len1
             , Just $ Array arr# (off + len1 + 1) len2)
