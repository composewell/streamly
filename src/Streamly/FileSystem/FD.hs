#include "inline.hs"

-- |
-- Module      : Streamly.FileSystem.FD
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module is a an experimental replacement for
-- "Streamly.FileSystem.Handle". The former module provides IO facilities based
-- on the GHC Handle type. The APIs in this module avoid the GHC handle layer
-- and provide more explicit control over buffering.
--
-- Read and write data as streams and arrays to and from files.
--
-- This module provides read and write APIs based on handles. Before reading or
-- writing, a file must be opened first using 'openFile'. The 'Handle' returned
-- by 'openFile' is then used to access the file. A 'Handle' is backed by an
-- operating system file descriptor. When the 'Handle' is garbage collected the
-- underlying file descriptor is automatically closed. A handle can be
-- explicitly closed using 'closeFile'.
--
-- Reading and writing APIs are divided into two categories, sequential
-- streaming APIs and random or seekable access APIs.  File IO APIs are quite
-- similar to "Streamly.Memory.Array" read write APIs. In that regard, arrays can
-- be considered as in-memory files or files can be considered as on-disk
-- arrays.
--
-- > import qualified Streamly.FileSystem.FD as FD
--

module Streamly.FileSystem.FD
    (
    -- * File Handles
      Handle
    , stdin
    , stdout
    , stderr
    , openFile

    -- TODO file path based APIs
    -- , readFile
    -- , writeFile

    -- * Streaming IO
    -- | Streaming APIs read or write data to or from a file or device
    -- sequentially, they never perform a seek to a random location.  When
    -- reading, the stream is lazy and generated on-demand as the consumer
    -- consumes it.  Read IO requests to the IO device are performed in chunks
    -- of 32KiB, this is referred to as @defaultChunkSize@ in the
    -- documentation. One IO request may or may not read the full chunk. If the
    -- whole stream is not consumed, it is possible that we may read slightly
    -- more from the IO device than what the consumer needed.  Unless specified
    -- otherwise in the API, writes are collected into chunks of
    -- @defaultChunkSize@ before they are written to the IO device.

    -- Streaming APIs work for all kind of devices, seekable or non-seekable;
    -- including disks, files, memory devices, terminals, pipes, sockets and
    -- fifos. While random access APIs work only for files or devices that have
    -- random access or seek capability for example disks, memory devices.
    -- Devices like terminals, pipes, sockets and fifos do not have random
    -- access capability.

    -- ** Read File to Stream
    , read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    , readInChunksOf

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf

    , readArrays
    , readArraysOfUpto
    -- , readArraysOf

    -- ** Write File from Stream
    , write
    -- , writeUtf8
    -- , writeUtf8Lines
    -- , writeFrames
    , writeInChunksOf

    -- -- * Array Write
    -- , writeArray
    , writeArrays
    , writeArraysPackedUpto

    -- XXX these are incomplete
    -- , writev
    -- , writevArraysPackedUpto

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
import Control.Monad.Primitive (PrimMonad(..))
import Data.Word (Word8)
-- import Foreign.ForeignPtr (withForeignPtr)
-- import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (plusPtr, castPtr)
import Data.Primitive.Types (Prim)
-- import System.IO (Handle, hGetBufSome, hPutBuf)
import System.IO (IOMode)
import Prelude hiding (read)

import qualified GHC.IO.FD as FD
import qualified GHC.IO.Device as RawIO

import Streamly.Internal.Data.Prim.Pinned.Array.Types (Array(..), byteLength, defaultChunkSize)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Stream.StreamK.Type (IsStream, mkStream)

#if !defined(mingw32_HOST_OS)
-- import Streamly.Internal.Data.Prim.Pinned.Array.Types (groupIOVecsOf)
import Streamly.Internal.Data.Stream.StreamD (toStreamD)
import Streamly.Internal.Data.Stream.StreamD.Type (fromStreamD)
import qualified Streamly.FileSystem.FDIO as RawIO hiding (write)
#endif
-- import Streamly.Data.Fold (Fold)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)

import qualified Streamly.Internal.Data.Prim.Pinned.Array.Types as A
import qualified Streamly.Internal.Data.Prim.Pinned.ArrayStream as AS
import qualified Streamly.Internal.Data.Prim.Pinned.Mutable.Array.Types as MA
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

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
-- Handles
-------------------------------------------------------------------------------

-- XXX attach a finalizer
-- | A 'Handle' is returned by 'openFile' and is subsequently used to perform
-- read and write operations on a file.
--
newtype Handle = Handle FD.FD

-- | File handle for standard input
stdin :: Handle
stdin = Handle FD.stdin

-- | File handle for standard output
stdout :: Handle
stdout = Handle FD.stdout

-- | File handle for standard error
stderr :: Handle
stderr = Handle FD.stderr

-- XXX we can support all the flags that the "open" system call supports.
-- Instead of using RTS locking mechanism can we use system provided locking
-- instead?
--
-- | Open a file that is not a directory and return a file handle.
-- 'openFile' enforces a multiple-reader single-writer locking on files. That
-- is, there may either be many handles on the same file which manage input, or
-- just one handle on the file which manages output. If any open handle is
-- managing a file for output, no new handle can be allocated for that file. If
-- any open handle is managing a file for input, new handles can only be
-- allocated if they do not manage output. Whether two files are the same is
-- implementation-dependent, but they should normally be the same if they have
-- the same absolute path name and neither has been renamed, for example.
--
openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = Handle . fst <$> FD.openFile path mode True

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
-- XXX Do we need `withForeignPtr` here?
{-# INLINABLE readArrayUpto #-}
readArrayUpto :: Int -> Handle -> IO (Array Word8)
readArrayUpto size (Handle fd) = do
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    arr <- MA.newArray size
    MA.withArrayAsPtr arr $ \p -> do
        -- n <- hGetBufSome h p size
        n <- RawIO.read fd p size
        MA.shrinkArray arr n
        A.unsafeFreeze arr

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an 'Array' to a file handle.
--
-- @since 0.7.0
{-# INLINABLE writeArray #-}
writeArray :: Prim a => Handle -> Array a -> IO ()
writeArray _ arr | A.length arr == 0 = return ()
writeArray (Handle fd) arr =
    -- RawIO.writeAll fd (castPtr p) aLen
    A.withArrayAsPtr arr $ \p ->
        RawIO.write fd (castPtr p) aLen
    {-
    -- Experiment to compare "writev" based IO with "write" based IO.
    iov <- A.newArray 1
    let iov' = iov {aEnd = aBound iov}
    A.writeIndex iov' 0 (RawIO.IOVec (castPtr p) (fromIntegral aLen))
    RawIO.writevAll fd (unsafeForeignPtrToPtr (aStart iov')) 1
    -}
    where
    aLen = byteLength arr

{-
#if !defined(mingw32_HOST_OS)
-- | Write an array of 'IOVec' to a file handle.
--
-- @since 0.7.0
{-# INLINABLE writeIOVec #-}
writeIOVec :: Handle -> Array RawIO.IOVec -> IO ()
writeIOVec _ iov | A.length iov == 0 = return ()
writeIOVec (Handle fd) iov = RawIO.writevAll fd (A.toPtr iov) (A.length iov)
#endif
-}

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @readArraysOfUpto size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
{-# INLINABLE _readArraysOfUpto #-}
_readArraysOfUpto :: (IsStream t, MonadIO m)
    => Int -> Handle -> t m (Array Word8)
_readArraysOfUpto size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld _ stp -> do
        arr <- liftIO $ readArrayUpto size h
        if A.length arr == 0
        then stp
        else yld arr go

{-# INLINE_NORMAL readArraysOfUpto #-}
readArraysOfUpto :: (IsStream t, MonadIO m)
    => Int -> Handle -> t m (Array Word8)
readArraysOfUpto size h = D.fromStreamD (D.Stream step ())
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
-- | @readArrays h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @defaultChunkSize@.
-- 'readArrays' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
--
-- > readArrays = readArraysOfUpto defaultChunkSize
--
-- @since 0.7.0
{-# INLINE readArrays #-}
readArrays :: (IsStream t, MonadIO m) => Handle -> t m (Array Word8)
readArrays = readArraysOfUpto defaultChunkSize

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | @readInChunksOf chunkSize handle@ reads a byte stream from a file handle,
-- reads are performed in chunks of up to @chunkSize@.  The stream ends as soon
-- as EOF is encountered.
--
{-# INLINE readInChunksOf #-}
readInChunksOf :: (IsStream t, MonadIO m, PrimMonad m) => Int -> Handle -> t m Word8
readInChunksOf chunkSize h = AS.concat $ readArraysOfUpto chunkSize h

-- TODO
-- read :: (IsStream t, MonadIO m, Prim a) => Handle -> t m a
--
-- > read = 'readByChunks' A.defaultChunkSize
-- | Generate a stream of elements of the given type from a file 'Handle'. The
-- stream ends when EOF is encountered.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: (IsStream t, MonadIO m, PrimMonad m) => Handle -> t m Word8
read = AS.concat . readArrays

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to a handle.
--
-- @since 0.7.0
{-# INLINE writeArrays #-}
writeArrays :: (MonadIO m, Prim a) => Handle -> SerialT m (Array a) -> m ()
writeArrays h = S.mapM_ (liftIO . writeArray h)

-- | Write a stream of arrays to a handle after coalescing them in chunks of
-- specified size. The chunk size is only a maximum and the actual writes could
-- be smaller than that as we do not split the arrays to fit them to the
-- specified size.
--
-- @since 0.7.0
{-# INLINE writeArraysPackedUpto #-}
writeArraysPackedUpto :: (PrimMonad m, Prim a, MonadIO m)
    => Int -> Handle -> SerialT m (Array a) -> m ()
writeArraysPackedUpto n h xs = writeArrays h $ AS.compact n xs

{-
#if !defined(mingw32_HOST_OS)
-- XXX this is incomplete
-- | Write a stream of 'IOVec' arrays to a handle.
--
-- @since 0.7.0
{-# INLINE writev #-}
writev :: MonadIO m => Handle -> SerialT m (Array RawIO.IOVec) -> m ()
writev h = S.mapM_ (liftIO . writeIOVec h)

{-
-- XXX UNCOMMENT THIS ONCE groupIOVecsOf IS ADDED
-- XXX this is incomplete
-- | Write a stream of arrays to a handle after grouping them in 'IOVec' arrays
-- of up to a maximum total size. Writes are performed using gather IO via
-- @writev@ system call. The maximum number of entries in each 'IOVec' group
-- limited to 512.
--
-- @since 0.7.0
{-# INLINE _writevArraysPackedUpto #-}
_writevArraysPackedUpto :: MonadIO m
    => Int -> Handle -> SerialT m (Array a) -> m ()
_writevArraysPackedUpto n h xs =
    writev h $ fromStreamD $ groupIOVecsOf n 512 (toStreamD xs)
#endif
-}
-}

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
{-# INLINE writeInChunksOf #-}
writeInChunksOf :: (PrimMonad m, MonadIO m) => Int -> Handle -> SerialT m Word8 -> m ()
writeInChunksOf n h m = writeArrays h $ AS.arraysOf n m

-- > write = 'writeInChunksOf' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Combines the bytes in chunks of size
-- up to 'A.defaultChunkSize' before writing.  Note that the write behavior
-- depends on the 'IOMode' and the current seek position of the handle.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: (MonadIO m, PrimMonad m) => Handle -> SerialT m Word8 -> m ()
write = writeInChunksOf defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Prim a) => Handle -> SerialT m a -> m ()
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
readFrames :: (IsStream t, MonadIO m, Prim a)
    => Array a -> Handle -> Fold m a b -> t m b
readFrames = undefined -- foldFrames . read

-- | Write a stream to the given file handle buffering up to frames separated
-- by the given sequence or up to a maximum of @defaultChunkSize@.
--
-- @since 0.7.0
{-# INLINE writeByFrames #-}
writeByFrames :: (IsStream t, MonadIO m, Prim a)
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
readIndex :: Prim a => Handle -> Int -> Maybe a
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
readSliceWith :: (IsStream t, MonadIO m, Prim a)
    => Int -> Handle -> Int -> Int -> t m a
readSliceWith chunkSize h pos len = undefined

-- | @readSlice h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the forward direction
-- ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE readSlice #-}
readSlice :: (IsStream t, MonadIO m, Prim a)
    => Handle -> Int -> Int -> t m a
readSlice = readSliceWith defaultChunkSize

-- | @readSliceRev h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the reverse direction
-- ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE readSliceRev #-}
readSliceRev :: (IsStream t, MonadIO m, Prim a)
    => Handle -> Int -> Int -> t m a
readSliceRev h i count = undefined

-- | Write the given element at the given index in the file.
--
-- @since 0.7.0
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Prim a) => Handle -> Int -> a -> m ()
writeIndex h i a = undefined

-- | @writeSlice h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the forward
-- direction ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE writeSlice #-}
writeSlice :: (IsStream t, Monad m, Prim a)
    => Handle -> Int -> Int -> t m a -> m ()
writeSlice h i len s = undefined

-- | @writeSliceRev h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE writeSliceRev #-}
writeSliceRev :: (IsStream t, Monad m, Prim a)
    => Handle -> Int -> Int -> t m a -> m ()
writeSliceRev arr i len s = undefined
-}
