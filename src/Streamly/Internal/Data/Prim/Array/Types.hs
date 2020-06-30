{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Prim.Array.Types
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Prim.Array.Types
    (
      Array (..)
    , unsafeFreeze
    , unsafeThaw
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
    , write

    , unlines
    )
where

import qualified Streamly.Internal.Data.Prim.Mutable.Array.Types as MA

#include "prim-array-types.hs"

-- Drops the separator byte
-- Inefficient compared to Memory Array
{-# INLINE breakOn #-}
breakOn ::
       PrimMonad m
    => Word8
    -> Array Word8
    -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr =
    case loc of
        Left _ -> return (arr, Nothing)
        Right i -> do
            let nLen = len - i - 1
            nArr <- MA.newArray nLen
            mArr <- unsafeThaw arr
            MA.unsafeCopy nArr 0 mArr (i + 1) nLen
            MA.shrinkArray mArr i
            arr1 <- unsafeFreeze mArr
            arr2 <- unsafeFreeze nArr
            return (arr1, Just arr2)

    where

    loc = foldl' chk (Left 0) arr
    len = length arr
    chk (Left i) a =
        if a == sep
            then Right i
            else Left (i + 1)
    chk r _ = r
