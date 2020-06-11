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
-- Module      : Streamly.Internal.Data.Prim.Pinned.Mutable.Array.Types
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Prim.Pinned.Mutable.Array.Types
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

{-# INLINE newArray #-}
newArray ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> m (Array (PrimState m) a)
newArray (I# n#)
  = primitive (\s# ->
      case newPinnedByteArray# (n# *# sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, Array arr# #)
    )

{-# INLINE resizeArray #-}
resizeArray ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a
    -> Int -- ^ new size
    -> m (Array (PrimState m) a)
resizeArray arr i =
    if len == i
    then return arr
    else if len < i
         then shrinkArray arr i >> return arr
         else do
             nArr <- newArray i
             unsafeCopy nArr 0 arr 0 len
             return nArr
  where
    len = length arr
