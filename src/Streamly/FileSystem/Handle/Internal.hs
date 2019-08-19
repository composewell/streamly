{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.FileSystem.Handle.Internal
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streaming based IO write rotuines based on file handles.

module Streamly.FileSystem.Handle.Internal
    (
    writeArray

    -- Byte stream write
    , writeS
    , writeSInChunksOf

    -- -- * Array stream Write
    , writeSArrays
    , writeSArraysInChunksOf
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr)
import Foreign.Storable (Storable(..))
import System.IO (Handle, hPutBuf)
import Prelude hiding (read)

import Streamly.Memory.Array.Types (Array(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Memory.Array.Types (defaultChunkSize)

import qualified Streamly.Memory.Array as A
import qualified Streamly.Memory.ArrayStream as AS
import qualified Streamly.Prelude.Internal as S

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an 'Array' to a file handle.
--
-- @since 0.7.0
{-# INLINABLE writeArray #-}
writeArray :: Storable a => Handle -> Array a -> IO ()
writeArray _ arr | A.length arr == 0 = return ()
writeArray h Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to a handle.
--
-- @since 0.7.0
{-# INLINE writeSArrays #-}
writeSArrays :: (MonadIO m, Storable a)
    => Handle -> SerialT m (Array a) -> m ()
writeSArrays h m = S.mapM_ (liftIO . writeArray h) m

-- | @writeArraysPackedUpto chunkSize handle stream@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @chunkSize@.
-- The chunk size is only a maximum and the actual writes could be smaller as
-- we do not split the arrays to fit exactly to the specified size.
--
-- @since 0.7.0
{-# INLINE writeSArraysInChunksOf #-}
writeSArraysInChunksOf :: (MonadIO m, Storable a)
    => Int -> Handle -> SerialT m (Array a) -> m ()
writeSArraysInChunksOf n h xs = writeSArrays h $ AS.compact n xs

-- XXX Using custom array chunking with A.arraysOf, writeS gives 2x faster
-- readStream/catStream benchmark compared to S.arraysOf based writeS as well
-- as compared to the fold IO version of cat benchmark i.e. readStream/cat.
-- However, if we use the most optimal A.writeNUnsafe fold then the fold
-- version of cat is also as fast. But with the most optimal version of
-- A.writeNUnsafe, fusion breaks. Once GHC fusion problem gets fixed we should
-- be able to use that.
--
-- Until then we are keeping the custom A.arraysOf code.
--
-- | @writeSInChunksOf chunkSize handle stream@ writes @stream@ to @handle@ in
-- chunks of @chunkSize@.  A write is performed to the IO device as soon as we
-- collect the required input size.
--
-- @since 0.7.0
{-# INLINE writeSInChunksOf #-}
writeSInChunksOf :: MonadIO m => Int -> Handle -> SerialT m Word8 -> m ()
-- writeSInChunksOf n h m = writeSArrays h $ S.arraysOf n m
writeSInChunksOf n h m = writeSArrays h $ AS.arraysOf n m

-- > write = 'writeInChunksOf' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'A.defaultChunkSize' before writing.
--
-- NOTE: This may perform better than the 'write' fold, you can try this if you
-- need some extra perf boost.
--
-- @since 0.7.0
{-# INLINE writeS #-}
writeS :: MonadIO m => Handle -> SerialT m Word8 -> m ()
writeS = writeSInChunksOf defaultChunkSize
