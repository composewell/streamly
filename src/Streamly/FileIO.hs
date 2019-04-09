{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

#include "Streams/inline.hs"

-- |
-- Module      : Streamly.FileIO
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Read and write streams to and from files.
--
-- Streamly provides a full spectrum of streaming IO operations on the stream
-- being read from a file. Full control over the behavior of the stream in term
-- of buffering, encoding, decoding is in the hands of the programmer and the
-- options of the underlying handle ('TextEncoding', 'NewLineMode',
-- 'Buffering') are not needed.
--
-- By default file IO is done using a defaultChunkSize (32K). This means
-- that when reading from a block device, a 32K IO is issued at a time. The
-- block buffering of the Handle is not required as buffering occurs in the
-- stream itself based on the chunk size.  In fact, Handle's buffering does not
-- even kick in because our default chunk size is large.  However, if you use a
-- chunk size less than 8K (GHC's default buffer size) then you are advised to
-- use NOBuffering mode on the Handle in case you do not want buffering to
-- occur at GHC level as well. Same thing applies to writes as well.
--
-- Buffering can also be achieved at any point in a stream by folding (e.g.
-- using groupsOf) the stream into a stream of strict chunks. LineBuffering
-- can be achieved by chunking the stream into lines (e.g. using foldGroupsOn).
--
-- When writing a stream to a file we can choose a chunk size and the stream
-- will be chopped into chunks of up to that size before writing. To achieve
-- line buffering we can chop the stream into lines before writing.

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.

module Streamly.FileIO
    (

    -- * Default Settings
      defaultChunkSize

    -- -- * General APIs
    -- * Reading
    , fromHandle
    , fromHandleChunksOf
    -- , fromHandleLen
    -- , fromHandleLenWith

    -- * Writing
    , toHandle
    , toHandleChunksOf

    -- -- * Seekable Devices
    -- , fromHandlePos
    -- , fromHandlePosWith
    -- , fromHandlePosLen
    -- , fromHandlePosLenWith
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import System.IO (Handle, IOMode) -- IOMode imported for haddock

import Streamly.Array.Types (Array(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)

import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Array as A
import qualified Streamly.Fold as FL

-- XXX use fdadvise (fd,0,0,FADVISE_SEQUENTIAL) when available.

-- Handles perform no buffering of their own, buffering is done explicitly
-- by the stream.

-- XXX need to test/design the API for different devices (block devices, char
-- devices, fifos, sockets).

-- | GHC memory management allocation header overhead
allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

-- | Default maximum buffer size in bytes, for reading from and writing to IO
-- devices, the value is 32KB minus GHC allocation overhead, which is a few
-- bytes, so that the actual allocation is 32KB.
defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

-------------------------------------------------------------------------------
-- File APIs exposed to users
-------------------------------------------------------------------------------

-- The following references may be useful to build an understanding about the
-- file API design:
--
-- http://www.linux-mag.com/id/308/ for blocking/non-blocking IO on linux.
-- https://lwn.net/Articles/612483/ Non-blocking buffered file read operations
-- https://en.wikipedia.org/wiki/C_file_input/output for C APIs.
-- https://docs.oracle.com/javase/tutorial/essential/io/file.html for Java API.
-- https://www.w3.org/TR/FileAPI/ for http file API.
--
-- Design Notes:
--
-- We can have handle based APIs as well as filepath based APIs to read. There
-- are a few advantages of handle based APIs:
--
-- 1) The file may get renamed or removed if not locked. Using a handle we do
-- not need to lock the namespace, we can create a file and delete it and still
-- keep using the handle.
--
-- 2) A pure "fromFile" API would require opening a handle each time the API is
-- used. If we have to read from the file many times, it requires a path lookup
-- each time and if the reads are concurrent each read would require a separate
-- OS handle, which is a limited resource.
--
-- Handles are stateful and we would like to avoid them, but they are
-- ubiquitous and in many cases we are forced to use them. Handles like 'stdin',
-- 'stdout', 'stderr' are very common. Though we could use @fromFile
-- /dev/stdin@ or @fromStdin@ etc. the former is not so elegant and the later
-- would require multiple APIs for each special handle.

-- Design Notes:
--
-- To represent the range to read we have chosen (start, size) instead of
-- (start, end). This removes the ambiguity of whether "end" is included in the
-- range or not.
--
-- We could avoid specifying the range to be read and instead use "take size"
-- on the stream, but it could be useful to avoid unnecessary readaheads beyond
-- the end point that the consumer wants. Not reading more than required is
-- especially important when reading from stdin.
--
-------------------------------------------------------------------------------
-- APIs for seekable devices
-------------------------------------------------------------------------------

-- We could use a negative start offset to represent reading from the end.
-- However, there is an awkward case, we cannot represent reading the last
-- element because we cannot use 0 as a negative offset. Though the
-- last element can be read using forward reading, but you will have to know
-- the filesize for that, defeating the purpose of negative offset.  To avoid
-- this problem, we can use an offsetType to denote offset from start or from
-- end. We can even use an Either type instead.
--
-- We could use a negative size to read backwards, this may not be very useful
-- when reading from files (and may not even apply to terminals) but could be
-- useful when reading slices from arrays, so we can keep the convention for
-- files as well treating them as byte arrays. To keep this option open we
-- currently treat negative size as an error.


-- This API applies to devices or files which have the seek capability and
-- are of finite size. Therefore, we can read from a given position or specify
-- the read position from the end of file. This type of handles include disks,
-- files, memory devices and excludes terminals, pipes, sockets and fifos.
--
-- @fromHandlePosLenWith chunkSize handle pos len@ streams up to len bytes from
-- @handle@. @pos@ with 'Left' constructor specifies an offset from the
-- beginning of the file, with 'Right' constructor it specifies a negative
-- offset from the end of the file i.e. @filesize - pos@ from the beginning.
--
-- If @size@ is specified then a total of @size@ bytes are read starting from
-- the start point, unless the file ends before @size@ bytes could be read.
-- When @size@ is not specified it keeps streaming until end of file is
-- reached.
--
-- The slice starts at the offset @start@ from the beginning of file
-- and extends up to but not including @end@. Reads are performed in chunks of
-- size @chunkSzie@.
-- when @end@ is not specified it is assumed to be the end
-- of file. If @start@ (or @end@) is negative then it is calculated as @size +
-- start@ where @size@ is the size of the file. If @end < start@ a @nil@
-- stream is returned. If @start@ or @end@ is more than the size of the file it
-- is truncated to the size of the file.
--
-- For block devices, to avoid reading partial blocks @chunkSize@ must align
-- with the block size of the underlying device. If the underlying block size
-- is unknown, it is a good idea to keep it a multiple 4KiB. This API ensures
-- that the start of each chunk is aligned with @chunkSize@ from second chunk
-- onwards.
--
{-
{-# INLINE fromHandlePosLenWith #-}
fromHandlePosLenWith :: (IsStream t, MonadIO m)
    => Int -> Handle -> Either Int -> Int -> t m Word8
fromHandlePosLenWith chunkSize h pos len = undefined

-- |
-- > fromHandlePosLen = fromHandlePosLenWith defaultChunkSize
--
{-# INLINE fromHandlePosLen #-}
fromHandlePosLen :: (IsStream t, MonadIO m)
    => Handle -> Either Int -> Int -> t m Word8
fromHandlePosLen = fromHandlePosLenWith defaultChunkSize

{-# INLINE fromHandlePosWith #-}
fromHandlePosWith :: (IsStream t, MonadIO m)
    => Int -> Handle -> Either Int -> t m Word8
fromHandlePosWith chunkSize h pos = undefined

-- |
-- > fromHandlePos = fromHandlePosWith defaultChunkSize
--
{-# INLINE fromHandlePos #-}
fromHandlePos :: (IsStream t, MonadIO m) => Handle -> Either Int -> t m Word8
fromHandlePos = fromHandlePosWith defaultChunkSize

-- XXX use RULES to seek when we drop or when we use last etc.
-- -}

-------------------------------------------------------------------------------
-- APIs for all devices
-------------------------------------------------------------------------------

-- For both non-seekable and seekable handles. For non-seekable (terminals,
-- pipes, fifo etc.) handles we cannot use an offset to read which also means
-- that we cannot specify an offset from the end. Therefore the APIs are
-- simpler.

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

{-
{-# INLINE fromHandleLenWith #-}
fromHandleLenWith :: (IsStream t, MonadIO m)
    => Int -> Handle -> Int -> t m Word8
fromHandleLenWith chunkSize h len =
    S.concatMap toWord8Stream $ A.fromHandle chunkSize h start end

-- | Like fromHandle but we can specify how much data to read. We can achieve
-- the same effect using "take len . fromHandle", however, in that case
-- fromHandle is likely to issue readaheads even beyond len because it does not
-- know where the end is.
{-# INLINE fromHandleN #-}
fromHandleLen :: (IsStream t, MonadIO m)
    => Handle -> Int -> t m Word8
fromHandleLen = fromHandleLenWith defaultChunkSize
-}

-- The stream is lazy and generated on-demand as the consumer reads.  However,
-- it may perform a readahead and buffer some data.
--
-- | @fromHandleChunksOf chunkSize handle@ reads a byte stream from a file
-- handle.  The stream ends as soon as EOF is encountered.  It will not block
-- as long as some data is available on the handle. It uses a buffer of
-- @chunkSize@ to read data from system. For seekable devices, reading starts
-- at the current seek position of the handle.
--
{-# INLINE fromHandleChunksOf #-}
fromHandleChunksOf :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word8
fromHandleChunksOf chunkSize h = A.concatArray $
    A.readHandleChunksOf chunkSize h

{-
-- XXX we need to have the chunk size aligned to 64-bit
-- | Like fromHandleChunksOf but generates a stream of 64-bit values.
{-# INLINE fromHandleChunksOf64 #-}
fromHandleChunksOf64 :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word64
fromHandleChunksOf64 chunkSize h = A.concatArray $
    A.readHandleChunksOf chunkSize h
-}

-- @
-- > fromHandle = 'fromHandleChunksOf' defaultChunkSize
-- @
--
-- | Reads a byte stream from a file handle.  The stream ends as soon as EOF is
-- encountered.  This operation does not block as long as some data is
-- available on the handle. It uses a buffer of 'defaultChunkSize' to read data
-- from system.
--
-- For seekable devices, reading starts at the current seek position of the
-- handle. 'fromHandle' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
{-# INLINE fromHandle #-}
fromHandle :: (IsStream t, MonadIO m) => Handle -> t m Word8
fromHandle = fromHandleChunksOf defaultChunkSize

-- | @bufferN n stream@ buffers every N elements of a stream into an Array and
-- returns a stream of 'Array's.
{-# INLINE bufferN #-}
bufferN :: (IsStream t, Monad m, Storable a) => Int -> t m a -> t m (Array a)
bufferN n str =
    D.fromStreamD $ D.groupsOf n (FL.toArrayN n) (D.toStreamD str)
    -- D.fromStreamD $ D.arrayGroupsOf n (D.toStreamD str)

-- | Write a byte stream to a file handle. Combine the bytes in chunks of
-- specified size before writing. Note that the write behavior depends on the
-- 'IOMode' and the current seek state of the handle.
{-# INLINE toHandleChunksOf #-}
toHandleChunksOf :: MonadIO m => Int -> Handle -> SerialT m Word8 -> m ()
toHandleChunksOf n h m = A.concatToHandle h $ bufferN n m

-- @
-- toHandle = 'toHandleChunksOf' defaultChunkSize
-- @
--
-- | Write a byte stream to a file handle. Combines the bytes in chunks of size
-- up to 'defaultChunkSize' before writing.  Note that the write behavior
-- depends on the 'IOMode' and the current seek state of the handle.
{-# INLINE toHandle #-}
toHandle :: MonadIO m => Handle -> SerialT m Word8 -> m ()
toHandle = toHandleChunksOf defaultChunkSize

-------------------------------------------------------------------------------
-- Stateless handle based APIs
-------------------------------------------------------------------------------
--
{-
-- XXX handles could be shared, so we do not want to use the handle state at
-- all for this API. we should use pread and pwrite instead. On windows we will
-- need to use readFile/writeFile with an offset argument. Note, we can do this
-- only on seekable handles.
--
-- @fromHandleChunksAt handle size at@ generates a stream of 'Chunks' from the
-- file handle @handle@, the maximum size of chunks is @size@ and the stream
-- starts at offset @at@ in the file. The stream ends when the file ends. The
-- resulting chunks may be shorter than the specified size but never more than
-- it.
fromHandleBuffersAt
    :: (IsStream t, MonadIO m, MonadIO (t m))
    => Handle -> Int -> Int -> t m Buffer
fromHandleBuffersAt handle bufSize at = do
    liftIO $ hSeek handle AbsoluteSeek (fromIntegral at)
    fromHandleBuffers handle bufSize

-- @fromHandleSizedAt handle granularity at@ generate a stream of 'Word8' from
-- the file handle @handle@, performing IO in chunks of size @granularity@ and
-- starting at the offset @at@. The stream ends when the file ends.
fromHandleWord8At
    :: (IsStream t, MonadIO m, MonadIO (t m))
    => Handle -> Int -> Int -> t m Word8
fromHandleWord8At h chunkSize offset =
    S.concatMap toWord8Stream $ fromHandleBuffersAt h chunkSize offset
-}

-- TODO:
-- interact
--
--  XXX coalesce read requests, multiple reads into the same block can be
--  combined into a single read request followed by a view/projection on the
--  read block.  similalrly for write requests. requests may be expensive e.g.
--  when you are requesting from AWS.
--
--  requests can also be reordered to batch them like elevator and then
--  reordered back on receiving the result.
