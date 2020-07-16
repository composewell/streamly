{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Array.Prim.Types
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Prim.Types
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
    )
where

import qualified Streamly.Internal.Data.Array.Prim.Mut.Types as MA

#include "Streamly/Internal/Data/Array/Prim/TypesInclude.hs"

-- Drops the separator byte
-- Inefficient compared to Memory Array
{-# INLINE breakOn #-}
breakOn ::
       MonadIO m
    => Word8
    -> Array Word8
    -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr@(Array arr# off len) =
    case loc of
        Left _ -> return (arr, Nothing)
        Right len1 -> do
            let len2 = len - len1 - 1
            return (Array arr# off len1, Just $ Array arr# (off + len1 + 1) len2)

    where

    loc = foldl' chk (Left 0) arr

    chk (Left i) a =
        if a == sep
            then Right i
            else Left (i + 1)
    chk r _ = r
