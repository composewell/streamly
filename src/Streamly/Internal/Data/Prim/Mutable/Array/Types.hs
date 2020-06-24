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
-- Module      : Streamly.Internal.Data.Prim.Mutable.Array.Types
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Prim.Mutable.Array.Types
    (
      Array (..)

    -- * Construction
    , newArray
    , writeArray

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
    , unsafeIndexM
    , length
    , byteLength

    , writeN
    , write

    -- * Utilities
    , resizeArray
    , shrinkArray
    )
where

#include "mutable-prim-array-types.hs"

-------------------------------------------------------------------------------
-- Allocation (Unpinned)
-------------------------------------------------------------------------------

{-# INLINE newArray #-}
newArray ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> m (Array (PrimState m) a)
newArray (I# n#) =
    primitive $ \s# ->
        let bytes = n# *# sizeOf# (undefined :: a)
        in case newByteArray# bytes s# of
            (# s1#, arr# #) -> (# s1#, Array arr# #)

{-# INLINE resizeArray #-}
resizeArray ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a
    -> Int -- ^ new size in elem count
    -> m (Array (PrimState m) a)
resizeArray (Array arr#) (I# n#) =
    primitive $ \s# ->
        let bytes = n# *# sizeOf# (undefined :: a)
        in case resizeMutableByteArray# arr# bytes s# of
            (# s1#, arr1# #) -> (# s1#, Array arr1# #)
