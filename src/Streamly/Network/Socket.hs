{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Network.Socket
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Read and write streams and arrays to and from network sockets. Socket IO
-- APIs are quite similar to "Streamly.Mem.Array" read write APIs and almost
-- identical to the sequential streaming APIs in "Streamly.FileSystem.File".
--
-- Read IO requests to the socket are performed in chunks of 32KiB, this is
-- referred to as @defaultChunkSize@ in the documentation. One IO request may
-- or may not read the full chunk.  Unless specified otherwise in the API,
-- writes are collected into chunks of @defaultChunkSize@ before they are
-- written to the socket. APIs are provided to control the chunking and
-- framing behavior.
--
-- > import qualified Streamly.Network.Socket as SK
--

module Streamly.Network.Socket
    (
    -- * Read from connection
      fromSocket
    , read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    -- , readByChunks

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf

    -- , readArraysUpto
    -- , readArraysOf
    , readArrays

    -- * Write to connection
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeByChunks

    -- -- * Array Write
    , writeArray
    , writeArrays

    -- reading/writing datagrams
    )
where

import Control.Concurrent (threadWaitWrite, rtsSupportsBoundThreads)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr, Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Network.Socket (Socket, sendBuf, recvBuf)
#if MIN_VERSION_network(3,1,0)
import Network.Socket (withFdSocket)
#else
import Network.Socket (fdSocket)
#endif
import Prelude hiding (read)

import qualified Network.Socket as Net

import Streamly.Mem.Array.Types (Array(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream, mkStream)
-- import Streamly.Fold (Fold)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)

import qualified Streamly.Mem.Array as A
import qualified Streamly.Mem.Array.Types as A hiding (flattenArrays)
import qualified Streamly.Prelude as S

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

{-# INLINABLE readArrayUptoWith #-}
readArrayUptoWith
    :: (h -> Ptr Word8 -> Int -> IO Int)
    -> Int
    -> h
    -> IO (Array Word8)
readArrayUptoWith f size h = do
    ptr <- mallocPlainForeignPtrBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    withForeignPtr ptr $ \p -> do
        n <- f h p size
        let v = Array
                { aStart = ptr
                , aEnd   = p `plusPtr` n
                , aBound = p `plusPtr` size
                }
        -- XXX shrink only if the diff is significant
        -- A.shrinkToFit v
        return v

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
{-# INLINABLE readArrayUpto #-}
readArrayUpto :: Int -> Socket -> IO (Array Word8)
readArrayUpto = readArrayUptoWith recvBuf

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

waitWhen0 :: Int -> Socket -> IO ()
waitWhen0 0 s = when rtsSupportsBoundThreads $
#if MIN_VERSION_network(3,1,0)
    withFdSocket s $ \fd -> threadWaitWrite $ fromIntegral fd
#else
    let fd = fdSocket s in threadWaitWrite $ fromIntegral fd
#endif
waitWhen0 _ _ = return ()

sendAll :: Socket -> Ptr Word8 -> Int -> IO ()
sendAll _ _ len | len <= 0 = return ()
sendAll s p len = do
    sent <- sendBuf s p len
    waitWhen0 sent s
    -- assert (sent <= len)
    when (sent >= 0) $ sendAll s (p `plusPtr` sent) (len - sent)

{-# INLINABLE writeArrayWith #-}
writeArrayWith :: Storable a
    => (h -> Ptr Word8 -> Int -> IO ())
    -> h
    -> Array a
    -> IO ()
writeArrayWith _ _ arr | A.length arr == 0 = return ()
writeArrayWith f h Array{..} = withForeignPtr aStart $ \p ->
    f h (castPtr p) aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

-- | Write an Array to a file handle.
--
-- @since 0.7.0
{-# INLINABLE writeArray #-}
writeArray :: Storable a => Socket -> Array a -> IO ()
writeArray = writeArrayWith sendAll

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

{-# INLINABLE readArraysUptoWith #-}
readArraysUptoWith :: (IsStream t, MonadIO m)
    => (Int -> h -> IO (Array Word8))
    -> Int -> h -> t m (Array Word8)
readArraysUptoWith f size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld _ stp -> do
        arr <- liftIO $ f size h
        if A.length arr == 0
        then stp
        else yld arr go

-- | @readArraysUpto size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @size@.
-- 'fromHandleArraysUpto' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
{-# INLINABLE readArraysUpto #-}
readArraysUpto :: (IsStream t, MonadIO m)
    => Int -> Socket -> t m (Array Word8)
readArraysUpto = readArraysUptoWith readArrayUpto

-- XXX read 'Array a' instead of Word8
--
-- | @readArrays h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @defaultChunkSize@.
-- 'readArrays' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
--
-- @since 0.7.0
{-# INLINE readArrays #-}
readArrays :: (IsStream t, MonadIO m) => Socket -> t m (Array Word8)
readArrays = readArraysUpto A.defaultChunkSize

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

{-
-- | @readByChunksUpto chunkSize handle@ reads a byte stream from a file
-- handle, reads are performed in chunks of up to @chunkSize@.  The stream ends
-- as soon as EOF is encountered.
--
{-# INLINE readByChunksUpto #-}
readByChunksUpto :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word8
readByChunksUpto chunkSize h = A.flattenArrays $ readArraysUpto chunkSize h
-}

-- TODO
-- read :: (IsStream t, MonadIO m, Storable a) => Handle -> t m a
--
-- > read = 'readByChunks' A.defaultChunkSize
-- | Generate a stream of elements of the given type from a file 'Handle'. The
-- stream ends when EOF is encountered.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: (IsStream t, MonadIO m) => Socket -> t m Word8
read = A.flattenArrays . readArrays

-- | Read a stream of Word8 from a socket, closing the socket when done.
fromSocket :: (MonadCatch m, MonadIO m) => Socket -> SerialT m Word8
fromSocket sk = S.finally (liftIO (Net.close sk)) (read sk)

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to a handle.
--
-- @since 0.7.0
{-# INLINE writeArrays #-}
writeArrays :: (MonadIO m, Storable a) => Socket -> SerialT m (Array a) -> m ()
writeArrays h m = S.mapM_ (liftIO . writeArray h) m

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE writeByChunks #-}
writeByChunks :: MonadIO m => Int -> Socket -> SerialT m Word8 -> m ()
writeByChunks n h m = writeArrays h $ A.arraysOf n m

-- > write = 'writeByChunks' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Combines the bytes in chunks of size
-- up to 'A.defaultChunkSize' before writing.  Note that the write behavior
-- depends on the 'IOMode' and the current seek position of the handle.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: MonadIO m => Socket -> SerialT m Word8 -> m ()
write = writeByChunks A.defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Storable a) => Handle -> SerialT m a -> m ()
write = toHandleWith A.defaultChunkSize
-}

-------------------------------------------------------------------------------
-- IO with encoding/decoding Unicode characters
-------------------------------------------------------------------------------

{-
-- |
-- > readUtf8 = decodeUtf8 . read
--
-- Read a UTF8 encoded stream of unicode characters from a file handle.
--
-- @since 0.7.0
{-# INLINE readUtf8 #-}
readUtf8 :: (IsStream t, MonadIO m) => Handle -> t m Char
readUtf8 = decodeUtf8 . read

-- |
-- > writeUtf8 h s = write h $ encodeUtf8 s
--
-- Encode a stream of unicode characters to UTF8 and write it to the given file
-- handle. Default block buffering applies to the writes.
--
-- @since 0.7.0
{-# INLINE writeUtf8 #-}
writeUtf8 :: MonadIO m => Handle -> SerialT m Char -> m ()
writeUtf8 h s = write h $ encodeUtf8 s

-- | Write a stream of unicode characters after encoding to UTF-8 in chunks
-- separated by a linefeed character @'\n'@. If the size of the buffer exceeds
-- @defaultChunkSize@ and a linefeed is not yet found, the buffer is written
-- anyway.  This is similar to writing to a 'Handle' with the 'LineBuffering'
-- option.
--
-- @since 0.7.0
{-# INLINE writeUtf8ByLines #-}
writeUtf8ByLines :: (IsStream t, MonadIO m) => Handle -> t m Char -> m ()
writeUtf8ByLines = undefined

-- | Read UTF-8 lines from a file handle and apply the specified fold to each
-- line. This is similar to reading a 'Handle' with the 'LineBuffering' option.
--
-- @since 0.7.0
{-# INLINE readLines #-}
readLines :: (IsStream t, MonadIO m) => Handle -> Fold m Char b -> t m b
readLines h f = foldLines (readUtf8 h) f

-------------------------------------------------------------------------------
-- Framing on a sequence
-------------------------------------------------------------------------------

-- | Read a stream from a file handle and split it into frames delimited by
-- the specified sequence of elements. The supplied fold is applied on each
-- frame.
--
-- @since 0.7.0
{-# INLINE readFrames #-}
readFrames :: (IsStream t, MonadIO m, Storable a)
    => Array a -> Handle -> Fold m a b -> t m b
readFrames = undefined -- foldFrames . read

-- | Write a stream to the given file handle buffering up to frames separated
-- by the given sequence or up to a maximum of @defaultChunkSize@.
--
-- @since 0.7.0
{-# INLINE writeByFrames #-}
writeByFrames :: (IsStream t, MonadIO m, Storable a)
    => Array a -> Handle -> t m a -> m ()
writeByFrames = undefined
-}
