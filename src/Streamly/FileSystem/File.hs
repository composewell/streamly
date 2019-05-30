{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.FileSystem.File
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Read and write streams and arrays to and from files specified by their paths
-- in the file system. Unlike the handle based APIs which can have a read/write
-- session consisting of multiple reads and writes to the handle, these APIs
-- are one shot read or write APIs. These APIs open the file handle, perform
-- the requested operation and close the handle. Thease are safer compared to
-- the handle based APIs as there is no possiblity of a file descriptor
-- leakage.
--
-- > import qualified Streamly.FileSystem.File as File
--

module Streamly.FileSystem.File
    (
    -- * Streaming IO
    -- | Stream data to or from a file or device sequentially.  When reading,
    -- the stream is lazy and generated on-demand as the consumer consumes it.
    -- Read IO requests to the IO device are performed in chunks limited to a
    -- maximum size of 32KiB, this is referred to as @defaultChunkSize@ in the
    -- documentation. One IO request may or may not read the full
    -- chunk. If the whole stream is not consumed, it is possible that we may
    -- read slightly more from the IO device than what the consumer needed.
    -- Unless specified otherwise in the API, writes are collected into chunks
    -- of @defaultChunkSize@ before they are written to the IO device.

    -- Streaming APIs work for all kind of devices, seekable or non-seekable;
    -- including disks, files, memory devices, terminals, pipes, sockets and
    -- fifos. While random access APIs work only for files or devices that have
    -- random access or seek capability for example disks, memory devices.
    -- Devices like terminals, pipes, sockets and fifos do not have random
    -- access capability.

    -- ** File IO Using Handle
      withFile

    -- ** Read From File
    , read
    -- , readShared
    -- , readTailForever

    -- , readUtf8
    -- , readLines
    -- , readFrames
    -- , readByChunks

    -- -- * Array Read
    -- , readArrayOf

    , readArraysUpto
    -- , readArraysOf
    , readArrays

    -- ** Write To File
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeByChunks

    -- -- * Array Write
    , writeArray
    , writeArrays

    -- ** Append To File
    , append
    , appendByChunks
    -- , appendShared
    , appendArray
    , appendArrays

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

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import System.IO (Handle, openFile, IOMode(..), hClose)
import Prelude hiding (read)

import qualified System.IO as SIO

import Streamly.Mem.Array.Types (Array(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)
import Streamly.SVar (MonadAsync)
-- import Streamly.Fold (Fold)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Mem.Array as A
import qualified Streamly.Mem.Array.Types as A hiding (flattenArrays)
import qualified Streamly.Prelude as S

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
-- Safe file reading
-------------------------------------------------------------------------------

-- | @'withFile' name mode act@ opens a file using 'openFile' and passes
-- the resulting handle to the computation @act@.  The handle will be
-- closed on exit from 'withFile', whether by normal termination or by
-- raising an exception.  If closing the handle raises an exception, then
-- this exception will be raised by 'withFile' rather than any exception
-- raised by 'act'.
withFile :: (IsStream t, MonadCatch m, MonadIO m)
    => FilePath -> IOMode -> (Handle -> t m a) -> t m a
withFile file mode = S.bracket (liftIO $ openFile file mode) (liftIO . hClose)

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

-- TODO readArrayOf

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an array to a file. Overwrites the file if it exists.
--
-- @since 0.7.0
{-# INLINABLE writeArray #-}
writeArray :: Storable a => FilePath -> Array a -> IO ()
writeArray file arr = SIO.withFile file WriteMode (\h -> FH.writeArray h arr)

-- | append an array to a file.
--
-- @since 0.7.0
{-# INLINABLE appendArray #-}
appendArray :: Storable a => FilePath -> Array a -> IO ()
appendArray file arr = SIO.withFile file AppendMode (\h -> FH.writeArray h arr)

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @readArraysUpto size file@ reads a stream of arrays from file @file@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
{-# INLINABLE readArraysUpto #-}
readArraysUpto :: (IsStream t, MonadCatch m, MonadIO m)
    => Int -> FilePath -> t m (Array Word8)
readArraysUpto size file = withFile file ReadMode (FH.readArraysUpto size)

-- XXX read 'Array a' instead of Word8
--
-- | @readArrays file@ reads a stream of arrays from file @file@.
-- The maximum size of a single array is limited to @defaultChunkSize@.
--
-- > readArrays = readArraysUpto defaultChunkSize
--
-- @since 0.7.0
{-# INLINE readArrays #-}
readArrays :: (IsStream t, MonadCatch m, MonadIO m)
    => FilePath -> t m (Array Word8)
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
-- | Generate a stream of elements of the given type from a file specified by
-- path. The stream ends when EOF is encountered. File is locked using multiple
-- reader and single writer locking mode.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: (IsStream t, MonadCatch m, MonadIO m) => FilePath -> t m Word8
read file = withFile file ReadMode FH.read

{-
-- | Generate a stream of elements of the given type from a file 'Handle'. The
-- stream ends when EOF is encountered. File is not locked for exclusive reads,
-- writers can keep writing to the file.
--
-- @since 0.7.0
{-# INLINE readShared #-}
readShared :: (IsStream t, MonadIO m) => Handle -> t m Word8
readShared = undefined

-- | Read a stream from a given file path. When end of file (EOF) is reached
-- this API waits for more data to be written to the file and keeps reading it
-- as it is written.
--
-- @since 0.7.0
{-# INLINE readTailForever #-}
readTailForever :: (IsStream t, MonadIO m) => Handle -> t m Word8
readTailForever = undefined
-}

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

{-# INLINE writeArraysMode #-}
writeArraysMode :: (MonadAsync m, MonadCatch m, Storable a)
    => IOMode -> FilePath -> SerialT m (Array a) -> m ()
writeArraysMode mode file xs = S.drain $
    withFile file mode (\h -> S.mapM (liftIO . FH.writeArray h) xs)

-- | Write a stream of arrays to a file. Overwrites the file if it exists.
--
-- @since 0.7.0
{-# INLINE writeArrays #-}
writeArrays :: (MonadAsync m, MonadCatch m, Storable a)
    => FilePath -> SerialT m (Array a) -> m ()
writeArrays = writeArraysMode WriteMode

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
writeByChunks :: (MonadAsync m, MonadCatch m)
    => Int -> FilePath -> SerialT m Word8 -> m ()
writeByChunks n file xs = writeArrays file $ A.arraysOf n xs

-- > write = 'writeByChunks' A.defaultChunkSize
--
-- | Write a byte stream to a file. Combines the bytes in chunks of size
-- up to 'A.defaultChunkSize' before writing. If the file exists it is
-- truncated to zero size before writing. If the file does not exist it is
-- created. File is locked using single writer locking mode.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: (MonadAsync m, MonadCatch m) => FilePath -> SerialT m Word8 -> m ()
write = writeByChunks A.defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Storable a) => Handle -> SerialT m a -> m ()
write = toHandleWith A.defaultChunkSize
-}

-- | Append a stream of arrays to a file.
--
-- @since 0.7.0
{-# INLINE appendArrays #-}
appendArrays :: (MonadAsync m, MonadCatch m, Storable a)
    => FilePath -> SerialT m (Array a) -> m ()
appendArrays = writeArraysMode AppendMode

-- | Like 'append' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE appendByChunks #-}
appendByChunks :: (MonadAsync m, MonadCatch m)
    => Int -> FilePath -> SerialT m Word8 -> m ()
appendByChunks n file xs = appendArrays file $ A.arraysOf n xs

-- | Append a byte stream to a file. Combines the bytes in chunks of size up to
-- 'A.defaultChunkSize' before writing.  If the file exists then the new data
-- is appended to the file.  If the file does not exist it is created. File is
-- locked using single writer locking mode.
--
-- @since 0.7.0
{-# INLINE append #-}
append :: (MonadAsync m, MonadCatch m) => FilePath -> SerialT m Word8 -> m ()
append = appendByChunks A.defaultChunkSize

{-
-- | Like 'append' but the file is not locked for exclusive writes.
--
-- @since 0.7.0
{-# INLINE appendShared #-}
appendShared :: MonadIO m => Handle -> SerialT m Word8 -> m ()
appendShared = undefined
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
