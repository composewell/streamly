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
-- By default file IO is done using a A.defaultChunkSize (32K). This means
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
--
-- For IO routines to read unicode strings and line buffered routines, see
-- "Streamly.String".
--
-- IO devices are of two types, (1) seekable devices are more like arrays and
-- can seek to a given position and read or write at that position, such
-- devices are storage devices like disks and memory and are of finite size,
-- (2) non-seekable devices are more like streams and read or write from
-- beginning to end, examples of such device are terminals, pipes, sockets and
-- fifos.

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.

module Streamly.FileIO
    (

    -- * Position Independent APIs
    -- | These APIs do not use a position offset and therefore work for both
    -- non-seekable and seekable devices.  The stream is lazy and generated
    -- on-demand as the consumer reads.  However, it may read some data in
    -- advance and buffer it.

    -- Reading
      fromHandle
    , fromHandleN

    -- Writing
    , toHandle
    , toHandleArraysOf

    -- * Position Based APIs
    -- | These APIs apply to devices or files which have seek capability.
    -- Therefore, we can read from a given position.  This type of files
    -- include disks, files, memory devices and excludes terminals, pipes,
    -- sockets and fifos.
    --
    , fromHandlePos
    , fromHandlePosN

    -- * Frame buffering
    , fromHandleFramesOn
    , toHandleFramesOn
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import System.IO (Handle, IOMode) -- IOMode imported for haddock

import Streamly.Array.Types (Array(..), defaultChunkSize)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)
import Streamly.Fold (Fold)

import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Array as A
import qualified Streamly.Fold as FL

-- XXX use fdadvise (fd,0,0,FADVISE_SEQUENTIAL) when available.
-- XXX use RULES to seek when we drop or when we use last etc.

-- Handles perform no buffering of their own, buffering is done explicitly
-- by the stream.

-- XXX need to test/design the API for different devices (block devices, char
-- devices, fifos, sockets).

-- TODO:
-- interact
--
--  XXX coalesce read requests, multiple reads into the same block can be
--  combined into a single read request followed by a view/projection on the
--  read block.  similalrly for write requests. requests may be expensive e.g.
--  when you are requesting from AWS.
--
--  read requests can also be reordered to batch them like elevator and then
--  reordered back to serve in the same order as they arrived.

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
-- API Design Notes:
--
-- We can have handle based APIs as well as filepath based APIs to read. There
-- are a few advantages of handle based APIs:
--
-- 1) The file may get renamed or removed if not locked. Using a handle we do
-- not need to lock the namespace, we can create a file and delete it and still
-- keep using the handle. Edit: this does not sound like an advantage because
-- even if we have a fromFile API we will have an OS handle as long as we are
-- streaming, and we can have an option to delete the file after opening if we
-- want to.
--
-- 2) A pure "fromFile" API would require opening a handle each time the API is
-- used. If we have to read from the file many times, it requires a path lookup
-- each time and if the reads are concurrent each read would require a separate
-- OS handle, which is a limited resource. Edit: if we are streaming the
-- underlying handle remains open until we are done. This point is valid only
-- if we are sharing the handle across multiple bursts of streaming from the
-- file.
--
-- Handles are stateful and we would like to avoid them, but they are
-- ubiquitous and in many cases we are forced to use them. For example, handles
-- like 'stdin', 'stdout', 'stderr' are very common. Though we could use
-- @fromFile /dev/stdin@ or @fromStdin@ etc. the former is not so elegant and
-- the later would require multiple APIs for each special handle. Edit: but we
-- anyway have a special name for each such special handle so why not have a
-- special API for each?

-- Ranges:
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
-- useful when reading slices from arrays, so we can keep the same convention
-- for files as well treating them as byte arrays. To keep this option open we
-- currently treat negative size as an error.
--
-- Reading in reverse order is also possible, but since that is an uncommon use
-- case we design the APIs only for the forward reading case. The reverse
-- reading APIs can be added in a separate module.
--
-- When reading in reverse we will have to read a full block and then reverse
-- it, and then read the next block from reverse and so on.

-- XXX handles could be shared, so we may not want to use the handle state at
-- all for these APIs. we can use pread and pwrite instead. On windows we will
-- need to use readFile/writeFile with an offset argument.

-------------------------------------------------------------------------------

-- | @fromHandlePosNWith chunkSize handle pos len@ reads up to @len@ bytes
-- from @handle@ starting at the offset @pos@ from the beginning of the file.
--
-- Reads are performed in chunks of size @chunkSize@.  For block devices, to
-- avoid reading partial blocks @chunkSize@ must align with the block size of
-- the underlying device. If the underlying block size is unknown, it is a good
-- idea to keep it a multiple 4KiB. This API ensures that the start of each
-- chunk is aligned with @chunkSize@ from second chunk onwards.
--
{-# INLINE fromHandlePosNWith #-}
fromHandlePosNWith :: (IsStream t, MonadIO m)
    => Int -> Handle -> Int -> Int -> t m Word8
fromHandlePosNWith chunkSize h pos len = undefined

-- |
-- > fromHandlePosN = fromHandlePosNWith A.defaultChunkSize
--
{-# INLINE fromHandlePosN #-}
fromHandlePosN :: (IsStream t, MonadIO m)
    => Handle -> Int -> Int -> t m Word8
fromHandlePosN = undefined -- fromHandlePosNWith A.defaultChunkSize

-- | Read from the given position until end of file.
--
{-# INLINE fromHandlePosWith #-}
fromHandlePosWith :: (IsStream t, MonadIO m)
    => Int -> Handle -> Int -> t m Word8
fromHandlePosWith chunkSize h pos = undefined

-- |
-- > fromHandlePos = fromHandlePosWith A.defaultChunkSize
--
{-# INLINE fromHandlePos #-}
fromHandlePos :: (IsStream t, MonadIO m) => Handle -> Int -> t m Word8
fromHandlePos = undefined -- fromHandlePosWith A.defaultChunkSize

-------------------------------------------------------------------------------
-- APIs common to seekable and non-seekable devices
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Reading
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | Like "fromHandleN" but we can specify the chunksize as well.
--
{-# INLINE fromHandleNWith #-}
fromHandleNWith :: (IsStream t, MonadIO m)
    => Int -> Handle -> Int -> t m Word8
fromHandleNWith chunkSize h len = undefined
    -- S.concatMap toWord8Stream $ A.fromHandle chunkSize h start end

-- | Like "fromHandle" but we can specify how much data to read. We can achieve
-- the same effect using "take len . fromHandle", however, in that case
-- fromHandle is likely to issue readaheads even beyond len because it does not
-- know where the end is.
{-# INLINE fromHandleN #-}
fromHandleN :: (IsStream t, MonadIO m)
    => Handle -> Int -> t m Word8
fromHandleN = undefined -- fromHandleNWith A.defaultChunkSize

{-
-- XXX we need to have the chunk size aligned to sizeOf a
--
-- | @fromHandleWith chunkSize handle@ reads a byte stream from a file
-- handle.  The stream ends as soon as EOF is encountered.  It will not block
-- as long as some data is available on the handle. It uses a buffer of
-- @chunkSize@ to read data from system. For seekable devices, reading starts
-- at the current seek position of the handle.  This API ignores the prevailing
-- 'TextEncoding' and 'NewlineMode' on the 'Handle'.
--
{-# INLINE fromHandleWith #-}
fromHandleWith :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word8
fromHandleWith chunkSize h = A.concatArrays $
    A.fromHandleArraysWith chunkSize h
-}

-- |
-- > fromHandle = 'fromHandleWith' A.defaultChunkSize
--
{-
{-# INLINE fromHandleWord8 #-}
fromHandleWord8 :: (IsStream t, MonadIO m) => Handle -> t m Word8
fromHandleWord8 = fromHandleWith A.defaultChunkSize
-}
{-# INLINE fromHandle #-}
fromHandle :: (IsStream t, MonadIO m) => Handle -> t m Word8
fromHandle = A.concatArrays . A.fromHandleArrays

{-
{-# INLINE fromHandle #-}
fromHandle :: (IsStream t, MonadIO m, Storable a) => Handle -> t m a
fromHandle = fromHandleWith A.defaultChunkSize
-}

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Buffering control
-------------------------------------------------------------------------------

-- General API to buffer by max time or max size.

-------------------------------------------------------------------------------
-- Stream element unaware: Size based buffering
-------------------------------------------------------------------------------

-- No buffering (use chunks of 1)
-- block buffering (use chunks of n)

-- | @bufferN n stream@ buffers every N elements of a stream into an Array and
-- returns a stream of 'Array's.
{-# INLINE bufferN #-}
bufferN :: (IsStream t, Monad m, Storable a) => Int -> t m a -> t m (Array a)
bufferN n str = D.fromStreamD $ D.groupsOf n (FL.toArrayN n) (D.toStreamD str)

-- | Write a byte stream to a file handle. Combine the bytes in chunks of
-- specified size before writing. Note that the write position and behavior
-- depends on the 'IOMode' and the current seek position of the handle.
--
{-# INLINE toHandleArraysOf #-}
toHandleArraysOf :: MonadIO m => Int -> Handle -> SerialT m Word8 -> m ()
toHandleArraysOf n h m = A.toHandleArrays h $ bufferN n m

-- |
-- > toHandle = 'toHandleWith' A.defaultChunkSize
--
-- Write a byte stream to a file handle. Combines the bytes in chunks of size
-- up to 'A.defaultChunkSize' before writing.  Note that the write behavior
-- depends on the 'IOMode' and the current seek position of the handle.
{-# INLINE toHandle #-}
toHandle :: MonadIO m => Handle -> SerialT m Word8 -> m ()
toHandle = toHandleArraysOf defaultChunkSize

{-
{-# INLINE toHandle #-}
toHandle :: (MonadIO m, Storable a) => Handle -> SerialT m a -> m ()
toHandle = toHandleWith A.defaultChunkSize
-}

-------------------------------------------------------------------------------
-- Stream element unaware: Time based buffering
-------------------------------------------------------------------------------

-- There are two ways in which time based buffering can be implemented.
--
-- 1) One is to use an API like groupsOf to group elements by time and then
-- flush when the group is complete. This will require checking time on each
-- element yield.
--
-- 2) Use an async thread to write. The producer thread would just queue the
-- elements to be written on a queue associated with the writer thread. When
-- the write thread is scheduled elements that have been buffered will be
-- written based on a buffering policy. This will require the producer to
-- synchronize on the queue, send a doorbell when the queue has items in it.
-- The queue can be an array which can be written directly to the IO device.
--
-- We can try both and see which one performs better.

-------------------------------------------------------------------------------
-- Stream element aware buffering control
-------------------------------------------------------------------------------
--
-- Frame buffering

{-# INLINE fromHandleFramesOn #-}
fromHandleFramesOn :: (IsStream t, MonadIO m, Storable a) => Array a -> Handle -> Fold m a b -> t m b
fromHandleFramesOn = undefined -- foldFrames . fromHandle

-- | Write to the handle as soon as the specified pattern is encountered
-- or the specified chunk size has exceeded. This can put a limit on the frame
-- size.
--
-- toHandleFramesMax ::

-- | Write to the handle as soon as the specified pattern is encountered
-- or the A.defaultChunkSize has exceeded.
--
{-# INLINE toHandleFramesOn #-}
toHandleFramesOn :: (IsStream t, MonadIO m, Storable a) => Array a -> Handle -> t m a -> m ()
toHandleFramesOn = undefined
