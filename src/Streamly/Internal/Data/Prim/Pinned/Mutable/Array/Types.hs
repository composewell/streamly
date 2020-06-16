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
    , newAlignedArray
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
    , writeNAligned
    , write

    -- * Utilities
    , resizeArray
    , shrinkArray

    , fPlainPtrToW8Array
    , touchArray
    , withArrayAsPtr
    )
where

import Data.Word (Word8)
import GHC.ForeignPtr
import GHC.IO (IO(..))
import Foreign.Ptr (Ptr(..))
import Control.Exception (assert)

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

-- Change order of args?
{-# INLINE newAlignedArray #-}
newAlignedArray ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> Int
    -> m (Array (PrimState m) a)
newAlignedArray (I# n#) (I# a#)
  = primitive (\s# ->
      case newAlignedPinnedByteArray# (n# *# sizeOf# (undefined :: a)) a# s# of
        (# s'#, arr# #) -> (# s'#, Array arr# #)
    )

{-# INLINE_NORMAL writeNAligned #-}
writeNAligned ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> Int
    -> Fold m a (Array (PrimState m) a)
writeNAligned align limit = Fold step initial extract
  where
    initial = do
        marr <- newAlignedArray limit align
        return (marr, 0)
    step (marr, i) x
        | i == limit = return (marr, i)
        | otherwise = do
            writeArray marr i x
            return (marr, i + 1)
    extract (marr, _) = return marr


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

-- Change name later.
{-# INLINE toPtr #-}
toPtr :: Array s a -> Ptr a
toPtr (Array arr#) =
    assert (I# (isMutableByteArrayPinned# arr#) == 1) (Ptr (byteArrayContents# (unsafeCoerce# arr#)))

fPlainPtrToW8Array :: ForeignPtr a -> Array RealWorld Word8
fPlainPtrToW8Array (ForeignPtr _ (PlainPtr mb)) = Array mb
fPlainPtrToW8Array _ = error "fPlainPtrToW8Array can only be used when the ForeignPtr does not have finalizers."

{-# INLINE touchArray #-}
touchArray :: Array s a -> IO ()
touchArray arr = IO $ \s -> case touch# arr s of s' -> (# s', () #)

{-# INLINE withArrayAsPtr #-}
withArrayAsPtr :: Array s a -> (Ptr a -> IO b) -> IO b
withArrayAsPtr arr f = do
    r <- f (toPtr arr)
    touchArray arr
    return r
