{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE FlexibleContexts          #-}
#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Prim.Pinned.Array.Types
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Prim.Pinned.Array.Types
    (
      Array (..)
    , unsafeFreeze
    , unsafeThaw

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

#define PINNED 1

#include "prim-array-types.hs"
