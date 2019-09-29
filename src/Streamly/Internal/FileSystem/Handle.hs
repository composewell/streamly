{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.Handle
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.FileSystem.Handle
    (
    -- ** Read from Handle
      read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    , readInChunksOf

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf

    , readArraysOf
    -- , readArraysOf
    , readArrays

    -- ** Write to Handle
    -- Byte stream write (Folds)
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeInChunksOf

    -- Byte stream write (Streams)
    , writeS
    , writeSInChunksOf

    -- -- * Array Write
    , writeArray
    , writeArrays
    , writeArraysInChunksOf

    -- -- * Array stream Write
    , writeSArrays
    , writeSArraysInChunksOf

    -- -- * Random Access (Seek)
    -- -- | Unlike the streaming APIs listed above, these APIs apply to devices or
    -- files that have random access or seek capability.  This type of devices
    -- include disks, files, memory devices and exclude terminals, pipes,
    -- sockets and fifos.
    --
    -- , readIndex
    -- , readSlice
    -- , readSliceRev
    -- , readAt -- read from a given position to th end of file
    -- , readSliceArrayUpto
    -- , readSliceArrayOf

    -- , writeIndex
    -- , writeSlice
    -- , writeSliceRev
    -- , writeAt -- start writing at the given position
    -- , writeSliceArray
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import System.IO (Handle, hGetBufSome, hPutBuf)
import Prelude hiding (read)

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Memory.Array.Types
       (Array(..), writeNUnsafe, defaultChunkSize, shrinkToFit,
        lpackArraysChunksOf)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream, mkStream)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Memory.Array as A
import qualified Streamly.Streams.StreamD.Type as D

-------------------------------------------------------------------------------
-- References
-------------------------------------------------------------------------------
--
-- The following references may be useful to build an understanding about the
-- file API design:
--
-- http://www.linux-mag.com/id/308/ for blocking/non-blocking IO on linux.
-- https://lwn.net/Articles/612483/ Non-blocking buffered file read operations
-- https://en.wikipedia.org/wiki/C_file_input/output for C APIs.
-- https://docs.oracle.com/javase/tutorial/essential/io/file.html for Java API.
-- https://www.w3.org/TR/FileAPI/ for http file API.

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
{-# INLINABLE readArrayUpto #-}
readArrayUpto :: Int -> Handle -> IO (Array Word8)
readArrayUpto size h = do
    ptr <- mallocPlainForeignPtrBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    withForeignPtr ptr $ \p -> do
        n <- hGetBufSome h p size
        let v = Array
                { aStart = ptr
                , aEnd   = p `plusPtr` n
                , aBound = p `plusPtr` size
                }
        -- XXX shrink only if the diff is significant
        shrinkToFit v

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @readArraysOf size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
{-# INLINABLE _readArraysOf #-}
_readArraysOf :: (IsStream t, MonadIO m)
    => Int -> Handle -> t m (Array Word8)
_readArraysOf size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld _ stp -> do
        arr <- liftIO $ readArrayUpto size h
        if A.length arr == 0
        then stp
        else yld arr go

-- | @readArraysOf size handle@ reads a stream of arrays from the file
-- handle @handle@.  The maximum size of a single array is limited to @size@.
-- The actual size read may be less than or equal to @size@.
--
-- @since 0.7.0
{-# INLINE_NORMAL readArraysOf #-}
readArraysOf :: (IsStream t, MonadIO m) => Int -> Handle -> t m (Array Word8)
readArraysOf size h = D.fromStreamD (D.Stream step ())
  where
    {-# INLINE_LATE step #-}
    step _ _ = do
        arr <- liftIO $ readArrayUpto size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr ()

-- XXX read 'Array a' instead of Word8
--
-- | @readArrays handle@ reads a stream of arrays from the specified file
-- handle.  The maximum size of a single array is limited to
-- @defaultChunkSize@. The actual size read may be less than or equal to
-- @defaultChunkSize@.
--
-- > readArrays = readArraysOf defaultChunkSize
--
-- @since 0.7.0
{-# INLINE readArrays #-}
readArrays :: (IsStream t, MonadIO m) => Handle -> t m (Array Word8)
readArrays = readArraysOf defaultChunkSize

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | @readInChunksOf chunkSize handle@ reads a byte stream from a file
-- handle, reads are performed in chunks of up to @chunkSize@.
--
-- @since 0.7.0
{-# INLINE readInChunksOf #-}
readInChunksOf :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word8
readInChunksOf chunkSize h = AS.concat $ readArraysOf chunkSize h

-- TODO
-- Generate a stream of elements of the given type from a file 'Handle'.
-- read :: (IsStream t, MonadIO m, Storable a) => Handle -> t m a
--
-- > read = 'readInChunksOf' A.defaultChunkSize
-- | Generate a byte stream from a file 'Handle'.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: (IsStream t, MonadIO m) => Handle -> t m Word8
read = AS.concat . readArrays

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

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

-- | @writeSInChunksOf chunkSize handle stream@ writes @stream@ to @handle@ in
-- chunks of @chunkSize@.  A write is performed to the IO device as soon as we
-- collect the required input size.
--
-- @since 0.7.0
{-# INLINE writeSInChunksOf #-}
writeSInChunksOf :: MonadIO m => Int -> Handle -> SerialT m Word8 -> m ()
writeSInChunksOf n h m = writeSArrays h $ S.arraysOf n m
-- writeSInChunksOf n h m = writeSArrays h $ AS.arraysOf n m

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

-- | Write a stream of arrays to a handle.
--
-- @since 0.7.0
{-# INLINE writeArrays #-}
writeArrays :: (MonadIO m, Storable a) => Handle -> Fold m (Array a) ()
writeArrays h = FL.drainBy (liftIO . writeArray h)

-- | @writeArraysInChunksOf chunkSize handle@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @chunkSize@.
-- We never split an array, if a single array is bigger than the specified size
-- it emitted as it is. Multiple arrays are coalesed as long as the total size
-- remains below the specified size.
--
-- @since 0.7.0
{-# INLINE writeArraysInChunksOf #-}
writeArraysInChunksOf :: (MonadIO m, Storable a)
    => Int -> Handle -> Fold m (Array a) ()
writeArraysInChunksOf n h = lpackArraysChunksOf n (writeArrays h)

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- | @writeInChunksOf chunkSize handle@ writes the input stream to @handle@ in
-- chunks of @chunkSize@.  A write is performed to the IO device as soon as we
-- collect the required input size.
--
-- @since 0.7.0
{-# INLINE writeInChunksOf #-}
writeInChunksOf :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeInChunksOf n h = FL.lchunksOf n (writeNUnsafe n) (writeArrays h)

-- > write = 'writeInChunksOf' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'A.defaultChunkSize' before writing.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: MonadIO m => Handle -> Fold m Word8 ()
write = writeInChunksOf defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Storable a) => Handle -> SerialT m a -> m ()
write = toHandleWith A.defaultChunkSize
-}

-- XXX mmap a file into an array. This could be useful for in-place operations
-- on a file. For example, we can quicksort the contents of a file by mmapping
-- it.

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

-------------------------------------------------------------------------------
-- Framing by time
-------------------------------------------------------------------------------

-- | Write collecting the input in sessions of n seconds or if chunkSize
-- gets exceeded.
{-# INLINE writeByChunksOrSessionsOf #-}
writeByChunksOrSessionsOf :: MonadIO m
    => Int -> Double -> Handle -> SerialT m Word8 -> m ()
writeByChunksOrSessionsOf chunkSize sessionSize h m = undefined

-- | Write collecting the input in sessions of n seconds or if defaultChunkSize
-- gets exceeded.
{-# INLINE writeBySessionsOf #-}
writeBySessionsOf :: MonadIO m => Double -> Handle -> SerialT m Word8 -> m ()
writeBySessionsOf n = writeByChunksOrSessionsOf defaultChunkSize n

-------------------------------------------------------------------------------
-- Random Access IO (Seek)
-------------------------------------------------------------------------------

-- XXX handles could be shared, so we may not want to use the handle state at
-- all for these APIs. we can use pread and pwrite instead. On windows we will
-- need to use readFile/writeFile with an offset argument.

-------------------------------------------------------------------------------

-- | Read the element at the given index treating the file as an array.
--
-- @since 0.7.0
{-# INLINE readIndex #-}
readIndex :: Storable a => Handle -> Int -> Maybe a
readIndex arr i = undefined

-- NOTE: To represent a range to read we have chosen (start, size) instead of
-- (start, end). This helps in removing the ambiguity of whether "end" is
-- included in the range or not.
--
-- We could avoid specifying the range to be read and instead use "take size"
-- on the stream, but it may end up reading more and then consume it partially.

-- | @readSliceWith chunkSize handle pos len@ reads up to @len@ bytes
-- from @handle@ starting at the offset @pos@ from the beginning of the file.
--
-- Reads are performed in chunks of size @chunkSize@.  For block devices, to
-- avoid reading partial blocks @chunkSize@ must align with the block size of
-- the underlying device. If the underlying block size is unknown, it is a good
-- idea to keep it a multiple 4KiB. This API ensures that the start of each
-- chunk is aligned with @chunkSize@ from second chunk onwards.
--
{-# INLINE readSliceWith #-}
readSliceWith :: (IsStream t, MonadIO m, Storable a)
    => Int -> Handle -> Int -> Int -> t m a
readSliceWith chunkSize h pos len = undefined

-- | @readSlice h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the forward direction
-- ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE readSlice #-}
readSlice :: (IsStream t, MonadIO m, Storable a)
    => Handle -> Int -> Int -> t m a
readSlice = readSliceWith A.defaultChunkSize

-- | @readSliceRev h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the reverse direction
-- ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE readSliceRev #-}
readSliceRev :: (IsStream t, MonadIO m, Storable a)
    => Handle -> Int -> Int -> t m a
readSliceRev h i count = undefined

-- | Write the given element at the given index in the file.
--
-- @since 0.7.0
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Storable a) => Handle -> Int -> a -> m ()
writeIndex h i a = undefined

-- | @writeSlice h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the forward
-- direction ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE writeSlice #-}
writeSlice :: (IsStream t, Monad m, Storable a)
    => Handle -> Int -> Int -> t m a -> m ()
writeSlice h i len s = undefined

-- | @writeSliceRev h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE writeSliceRev #-}
writeSliceRev :: (IsStream t, Monad m, Storable a)
    => Handle -> Int -> Int -> t m a -> m ()
writeSliceRev arr i len s = undefined
-}
