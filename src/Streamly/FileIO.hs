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

module Streamly.FileIO
    (

    -- * General APIs
      fromHandle
    -- , fromHandleWith
    -- , fromHandleLen
    -- , fromHandleLenWith
    , toHandle

    -- * Seekable Devices
    -- , fromHandlePos
    -- , fromHandlePosWith
    -- , fromHandlePosLen
    -- , fromHandlePosLenWith
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
{-
import GHC.ForeignPtr  (ForeignPtr(ForeignPtr)
                       ,newForeignPtr_, mallocPlainForeignPtrBytes)
-}
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (realWorld#)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (IO(IO))
import System.IO (Handle, hGetBufSome, hPutBuf) -- , hSeek, SeekMode(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Prelude as S

import Streamly.SVar (adaptState)
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream, mkStream)
import qualified Streamly.Streams.StreamD.Type as D
import qualified Streamly.Streams.StreamD as D

import qualified Streamly.Array as V
import Streamly.Array.Types (Array(..), ByteArray, unsafeDangerousPerformIO)
import qualified Streamly.Foldl as FL


-- XXX we should use overWrite/write
{-# INLINE writeHandle #-}
writeHandle :: MonadIO m => Handle -> SerialT m ByteArray -> m ()
writeHandle h m = S.mapM_ (liftIO . V.toHandle h) m

{-# INLINE fromArray #-}
fromArray :: (IsStream t, Monad m, Storable a) => Array a -> t m a
fromArray = D.fromStreamD . D.fromArray

-- XXX this should perhas go in the Prelude or another module.
-- | Convert a stream of Word8 Arrays into a stream of Word8
{-# INLINE vConcat #-}
vConcat :: (IsStream t, Monad m, Storable a) => t m (Array a) -> t m a
vConcat = S.concatMap fromArray

-- Handles perform no buffering of their own, buffering is done explicitly
-- by the stream.

-- XXX need to test/design the API for different devices (block devices, char
-- devices, fifos, sockets).

-- | GHC memory management allocation header overhead
allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

-- | Default buffer size in bytes. Account for the GHC memory allocation
-- overhead so that the actual allocation is rounded to page boundary.
defaultChunkSize :: Int
defaultChunkSize = 320 * k - allocOverhead
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
-- ubiquitous and many cases we are forced to use them. Handles like 'stdin',
-- 'stdout', 'stderr' are very common. Though we could use @fromFile
-- /dev/stdin@ or @fromStdin@ etc. the former is not so elegant and the later
-- would require multiple APIs for each special handle.

-- Design Notes:
--
-- To represent the range to read we have chosen (start, size) instead of
-- (start, end). This removes the ambiguity of whether "end" is included in the
-- range or not. Also, for non-seekable devices (e.g. terminals) "end" would
-- not make sense as start does not make sense.
--
-- We could avoid specifying the end of the stream and just use "take size" on
-- the stream, but it could be useful to avoid unnecessary readaheads beyond
-- the end point that the consumer wants. Not reading more than required is
-- especially important when reading from stdin.
--
-------------------------------------------------------------------------------
-- APIs for seekable devices
-------------------------------------------------------------------------------

-- Seekable Devices:
--
-- We could use a negative start offset to represent reading from the end.
-- However, there is an awkward case, we cannot represent reading the last
-- element because we cannot use 0 as a negative offset. Though the
-- last element can be read using forward reading, but you will have to know
-- the filesize for that, defeating the purpose of negative offset.  To avoid
-- this problem, we can use an offsetType to denote offset from start or from
-- end. We can even use an Either for that.
--
-- We could use a negative size to read backwards, this may not be very useful
-- when reading from files (and may not even apply to terminals) but could be
-- useful when reading slices from arrays, so we can keep the convention for
-- files as well treating them as byte arrays. To keep this option open we
-- currently treat negative size as an error.

-- | This API applies to devices or files which have the seek capability and
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

{-
-- For non-seekable or seekable handles. For non-seekable (terminals, pipes,
-- fifo etc.) handles we cannot use an offset to read which also means that we
-- cannot specify an offset from the end. Therefore the APIs are simpler. But
-- we can still read in chunks.
--
{-# INLINE fromHandleLenWith #-}
fromHandleLenWith :: (IsStream t, MonadIO m)
    => Int -> Handle -> Int -> t m Word8
fromHandleLenWith chunkSize h len =
    S.concatMap toWord8Stream $ V.fromHandle chunkSize h start end

-- | Like fromHandle but we can specify how much data to read. We can achieve
-- the same effect using "take len . fromHandle", however, in that case
-- fromHandle is likely to issue readaheads even beyond len because it does not
-- know where the end is.
{-# INLINE fromHandleN #-}
fromHandleLen :: (IsStream t, MonadIO m)
    => Handle -> Int -> t m Word8
fromHandleLen = fromHandleLenWith defaultChunkSize
-}

-- | Generate a byte stream reading data from a 'Handle'. For seekable devices
-- this function ignores the seek position of the Handle, and always starts
-- reading at offset 0.
--
-- The stream starts at
-- the current seek position of the 'Handle' and ends when the handle returns
-- an EOF. The stream is lazy and generated on-demand as the consumer reads.
-- However, it may perform a readahead and buffer some data. Reads are
-- performed in chunks of up to 'defaultChunkSize' size.
--
{-# INLINE fromHandleWith #-}
fromHandleWith :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word8
fromHandleWith chunkSize h = vConcat $ V.readHandleWith chunkSize h

-- XXX for concurrent streams implement readahead IO. We can send multiple read
-- requests at the same time. For serial case we can use async IO. We can also
-- control the read throughput in mbps or IOPS.
--
{-# INLINE fromHandle #-}
fromHandle :: (IsStream t, MonadIO m) => Handle -> t m Word8
fromHandle h = fromHandleWith defaultChunkSize h

-- | @bufferN n stream@ buffers every N elements of a stream into a Array and
-- return a stream of 'Array's.
{-# INLINE bufferN #-}
-- bufferN :: Int -> t m a -> t m (Array a)
bufferN :: (IsStream t, Monad m) => Int -> t m Word8 -> t m ByteArray
-- bufferN n str = D.fromStreamD $ toArrayStreamD n (D.toStreamD str)
bufferN n str =
    D.fromStreamD $ D.foldGroupsOf (FL.toArrayN n) n (D.toStreamD str)

toHandle :: MonadIO m => Handle -> SerialT m Word8 -> m ()
toHandle h m = writeHandle h $ bufferN defaultChunkSize m

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

-- interact
--  XXX caolesce read requests, multiple reads into the same block can be
--  combined into a single read request followed by a view/projection on the
--  read block.  similalrly for write requests. requests may be expensive e.g.
--  when you are requesting from AWS.
--
--  requests can also be reordered to batch them like elevator and then
--  reordered back on receiving the result.
