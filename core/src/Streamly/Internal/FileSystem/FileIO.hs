-- |
-- Module      : Streamly.Internal.FileSystem.FileIO
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Read and write streams and arrays to and from files specified by their paths
-- in the file system. Unlike the handle based APIs which can have a read/write
-- session consisting of multiple reads and writes to the handle, these APIs
-- are one shot read or write APIs. These APIs open the file handle, perform
-- the requested operation and close the handle. Thease are safer compared to
-- the handle based APIs as there is no possibility of a file descriptor
-- leakage.
--
-- > import qualified Streamly.Internal.FileSystem.FileIO as File
--

module Streamly.Internal.FileSystem.FileIO
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

    -- ** Streams
    , read
    , readChunksWith
    , readChunks

    -- ** Unfolds
    , readerWith
    , reader
    -- , readShared
    -- , readUtf8
    -- , readLines
    -- , readFrames
    , chunkReaderWith
    , chunkReaderFromToWith
    , chunkReader

    -- ** Write To File
    , putChunk -- writeChunk?

    -- ** Folds
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeWith
    , writeChunks

    -- ** Writing Streams
    , fromBytes -- putBytes?
    , fromBytesWith
    , fromChunks

    -- ** Append To File
    , writeAppend
    , writeAppendWith
    -- , appendShared
    , writeAppendArray
    , writeAppendChunks

    -- * Deprecated
    , readWithBufferOf
    , readChunksWithBufferOf
    , readChunksFromToWith
    , toBytes
    , toChunks
    , toChunksWithBufferOf
    , writeWithBufferOf
    , fromBytesWithBufferOf
    )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import System.IO (Handle, IOMode(..), hClose)
import Prelude hiding (read)

import qualified Control.Monad.Catch as MC

import Streamly.Data.Fold (groupsOf, drain)
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Internal.FileSystem.Path (Path)

import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold as UF (bracketIO)
import qualified Streamly.Internal.Data.Fold.Type as FL
    (Step(..), snoc, reduce)
import qualified Streamly.Internal.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.File.Utils as FU

#include "inline.hs"

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
--
-- /Pre-release/
--
{-# INLINE withFile #-}
withFile :: (MonadIO m, MonadCatch m)
    => Path -> IOMode -> (Handle -> Stream m a) -> Stream m a
withFile file mode = S.bracketIO (FU.openFile file mode) hClose

-- | Transform an 'Unfold' from a 'Handle' to an unfold from a 'Path'.  The
-- resulting unfold opens a handle in 'ReadMode', uses it using the supplied
-- unfold and then makes sure that the handle is closed on normal termination
-- or in case of an exception.  If closing the handle raises an exception, then
-- this exception will be raised by 'usingFile'.
--
-- /Pre-release/
--
{-# INLINE usingFile #-}
usingFile :: (MonadIO m, MonadCatch m)
    => Unfold m Handle a -> Unfold m Path a
usingFile = UF.bracketIO (`FU.openFile` ReadMode) hClose

{-# INLINE usingFile2 #-}
usingFile2 :: (MonadIO m, MonadCatch m)
    => Unfold m (x, Handle) a -> Unfold m (x, Path) a
usingFile2 = UF.bracketIO before after

    where

    before (x, file) =  do
        h <- FU.openFile file ReadMode
        return (x, h)

    after (_, h) = hClose h

{-# INLINE usingFile3 #-}
usingFile3 :: (MonadIO m, MonadCatch m)
    => Unfold m (x, y, z, Handle) a -> Unfold m (x, y, z, Path) a
usingFile3 = UF.bracketIO before after

    where

    before (x, y, z, file) =  do
        h <- FU.openFile file ReadMode
        return (x, y, z, h)

    after (_, _, _, h) = hClose h

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

-- TODO readArrayOf

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an array to a file. Overwrites the file if it exists.
--
-- /Pre-release/
--
{-# INLINABLE putChunk #-}
putChunk :: Path -> Array a -> IO ()
putChunk file arr = FU.withFile file WriteMode (`FH.putChunk` arr)

-- | append an array to a file.
--
-- /Pre-release/
--
{-# INLINABLE writeAppendArray #-}
writeAppendArray :: Path -> Array a -> IO ()
writeAppendArray file arr = FU.withFile file AppendMode (`FH.putChunk` arr)

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @readChunksWith size file@ reads a stream of arrays from file @file@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
--
-- /Pre-release/
--
{-# INLINE readChunksWith #-}
readChunksWith :: (MonadIO m, MonadCatch m)
    => Int -> Path -> Stream m (Array Word8)
readChunksWith size file =
    withFile file ReadMode (FH.readChunksWith size)

{-# DEPRECATED toChunksWithBufferOf "Please use 'readChunksWith' instead" #-}
{-# INLINE toChunksWithBufferOf #-}
toChunksWithBufferOf :: (MonadIO m, MonadCatch m)
    => Int -> Path -> Stream m (Array Word8)
toChunksWithBufferOf = readChunksWith

-- XXX read 'Array a' instead of Word8
--
-- | @readChunks file@ reads a stream of arrays from file @file@.
-- The maximum size of a single array is limited to @defaultChunkSize@. The
-- actual size read may be less than @defaultChunkSize@.
--
-- > readChunks = readChunksWith defaultChunkSize
--
-- /Pre-release/
--
{-# INLINE readChunks #-}
readChunks :: (MonadIO m, MonadCatch m)
    => Path -> Stream m (Array Word8)
readChunks = readChunksWith defaultChunkSize

{-# DEPRECATED toChunks "Please use 'readChunks' instead" #-}
{-# INLINE toChunks #-}
toChunks :: (MonadIO m, MonadCatch m) => Path -> Stream m (Array Word8)
toChunks = readChunks

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | Unfold the tuple @(bufsize, filepath)@ into a stream of 'Word8' arrays.
-- Read requests to the IO device are performed using a buffer of size
-- @bufsize@. The size of an array in the resulting stream is always less than
-- or equal to @bufsize@.
--
-- /Pre-release/
--
{-# INLINE chunkReaderWith #-}
chunkReaderWith :: (MonadIO m, MonadCatch m)
    => Unfold m (Int, Path) (Array Word8)
chunkReaderWith = usingFile2 FH.chunkReaderWith

{-# DEPRECATED readChunksWithBufferOf
    "Please use 'chunkReaderWith' instead" #-}
{-# INLINE readChunksWithBufferOf #-}
readChunksWithBufferOf :: (MonadIO m, MonadCatch m)
    => Unfold m (Int, Path) (Array Word8)
readChunksWithBufferOf = chunkReaderWith

-- | Unfold the tuple @(from, to, bufsize, filepath)@ into a stream
-- of 'Word8' arrays.
-- Read requests to the IO device are performed using a buffer of size
-- @bufsize@ starting from absolute offset of @from@ till the absolute
-- position of @to@. The size of an array in the resulting stream is always
-- less than or equal to @bufsize@.
--
-- /Pre-release/
{-# INLINE chunkReaderFromToWith #-}
chunkReaderFromToWith :: (MonadIO m, MonadCatch m) =>
    Unfold m (Int, Int, Int, Path) (Array Word8)
chunkReaderFromToWith = usingFile3 FH.chunkReaderFromToWith

{-# DEPRECATED readChunksFromToWith
    "Please use 'chunkReaderFromToWith' instead" #-}
{-# INLINE readChunksFromToWith #-}
readChunksFromToWith :: (MonadIO m, MonadCatch m) =>
    Unfold m (Int, Int, Int, Path) (Array Word8)
readChunksFromToWith = chunkReaderFromToWith

-- | Unfolds a 'Path' into a stream of 'Word8' arrays. Requests to the IO
-- device are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.
--
-- /Pre-release/
{-# INLINE chunkReader #-}
chunkReader :: (MonadIO m, MonadCatch m) => Unfold m Path (Array Word8)
chunkReader = usingFile FH.chunkReader

-- | Unfolds the tuple @(bufsize, filepath)@ into a byte stream, read requests
-- to the IO device are performed using buffers of @bufsize@.
--
-- /Pre-release/
{-# INLINE readerWith #-}
readerWith :: (MonadIO m, MonadCatch m) => Unfold m (Int, Path) Word8
readerWith = usingFile2 FH.readerWith

{-# DEPRECATED readWithBufferOf "Please use 'readerWith' instead" #-}
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: (MonadIO m, MonadCatch m) =>
    Unfold m (Int, Path) Word8
readWithBufferOf = readerWith

-- | Unfolds a file path into a byte stream. IO requests to the device are
-- performed in sizes of
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.
--
-- /Pre-release/
{-# INLINE reader #-}
reader :: (MonadIO m, MonadCatch m) => Unfold m Path Word8
reader = UF.unfoldEach A.reader (usingFile FH.chunkReader)

-- | Generate a stream of bytes from a file specified by path. The stream ends
-- when EOF is encountered. File is locked using multiple reader and single
-- writer locking mode.
--
-- /Pre-release/
--
{-# INLINE read #-}
read :: (MonadIO m, MonadCatch m) => Path -> Stream m Word8
read file = A.concat $ withFile file ReadMode FH.readChunks

{-# DEPRECATED toBytes "Please use 'read' instead"  #-}
{-# INLINE toBytes #-}
toBytes :: (MonadIO m, MonadCatch m) => Path -> Stream m Word8
toBytes = read

{-
-- | Generate a stream of elements of the given type from a file 'Handle'. The
-- stream ends when EOF is encountered. File is not locked for exclusive reads,
-- writers can keep writing to the file.
--
-- @since 0.7.0
{-# INLINE readShared #-}
readShared :: MonadIO m => Handle -> Stream m Word8
readShared = undefined
-}

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

{-# INLINE fromChunksMode #-}
fromChunksMode :: (MonadIO m, MonadCatch m)
    => IOMode -> Path -> Stream m (Array a) -> m ()
fromChunksMode mode file xs = S.fold drain $
    withFile file mode (\h -> S.mapM (FH.putChunk h) xs)

-- | Write a stream of arrays to a file. Overwrites the file if it exists.
--
-- /Pre-release/
--
{-# INLINE fromChunks #-}
fromChunks :: (MonadIO m, MonadCatch m)
    => Path -> Stream m (Array a) -> m ()
fromChunks = fromChunksMode WriteMode

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
-- /Pre-release/
--
{-# INLINE fromBytesWith #-}
fromBytesWith :: (MonadIO m, MonadCatch m)
    => Int -> Path -> Stream m Word8 -> m ()
fromBytesWith n file xs = fromChunks file $ A.chunksOf' n xs

{-# DEPRECATED fromBytesWithBufferOf "Please use 'fromBytesWith' instead"  #-}
{-# INLINE fromBytesWithBufferOf #-}
fromBytesWithBufferOf :: (MonadIO m, MonadCatch m)
    => Int -> Path -> Stream m Word8 -> m ()
fromBytesWithBufferOf = fromBytesWith

-- > write = 'writeWith' defaultChunkSize
--
-- | Write a byte stream to a file. Combines the bytes in chunks of size
-- up to 'A.defaultChunkSize' before writing. If the file exists it is
-- truncated to zero size before writing. If the file does not exist it is
-- created. File is locked using single writer locking mode.
--
-- /Pre-release/
{-# INLINE fromBytes #-}
fromBytes :: (MonadIO m, MonadCatch m) => Path -> Stream m Word8 -> m ()
fromBytes = fromBytesWith defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Storable a) => Handle -> Stream m a -> m ()
write = toHandleWith A.defaultChunkSize
-}

-- | Write a stream of chunks to a handle. Each chunk in the stream is written
-- to the device as a separate IO request.
--
-- /Pre-release/
{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, MonadCatch m)
    => Path -> Fold m (Array a) ()
writeChunks path = Fold step initial extract final
    where
    initial = do
        h <- liftIO (FU.openFile path WriteMode)
        fld <- FL.reduce (FH.writeChunks h)
                `MC.onException` liftIO (hClose h)
        return $ FL.Partial (fld, h)
    step (fld, h) x = do
        r <- FL.snoc fld x `MC.onException` liftIO (hClose h)
        return $ FL.Partial (r, h)

    extract _ = return ()

    final (Fold _ initial1 _ final1, h) = do
        liftIO $ hClose h
        res <- initial1
        case res of
            FL.Partial fs -> final1 fs
            FL.Done () -> return ()

-- | @writeWith chunkSize handle@ writes the input stream to @handle@.
-- Bytes in the input stream are collected into a buffer until we have a chunk
-- of size @chunkSize@ and then written to the IO device.
--
-- /Pre-release/
{-# INLINE writeWith #-}
writeWith :: (MonadIO m, MonadCatch m)
    => Int -> Path -> Fold m Word8 ()
writeWith n path =
    groupsOf n (A.unsafeCreateOf' n) (writeChunks path)

{-# DEPRECATED writeWithBufferOf "Please use 'writeWith' instead"  #-}
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: (MonadIO m, MonadCatch m)
    => Int -> Path -> Fold m Word8 ()
writeWithBufferOf = writeWith

-- > write = 'writeWith' A.defaultChunkSize
--
-- | Write a byte stream to a file. Accumulates the input in chunks of up to
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize' before writing to
-- the IO device.
--
-- /Pre-release/
--
{-# INLINE write #-}
write :: (MonadIO m, MonadCatch m) => Path -> Fold m Word8 ()
write = writeWith defaultChunkSize

-- | Append a stream of arrays to a file.
--
-- /Pre-release/
--
{-# INLINE writeAppendChunks #-}
writeAppendChunks :: (MonadIO m, MonadCatch m)
    => Path -> Stream m (Array a) -> m ()
writeAppendChunks = fromChunksMode AppendMode

-- | Like 'append' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- /Pre-release/
--
{-# INLINE writeAppendWith #-}
writeAppendWith :: (MonadIO m, MonadCatch m)
    => Int -> Path -> Stream m Word8 -> m ()
writeAppendWith n file xs =
    writeAppendChunks file $ A.chunksOf' n xs

-- | Append a byte stream to a file. Combines the bytes in chunks of size up to
-- 'A.defaultChunkSize' before writing.  If the file exists then the new data
-- is appended to the file.  If the file does not exist it is created. File is
-- locked using single writer locking mode.
--
-- /Pre-release/
--
{-# INLINE writeAppend #-}
writeAppend :: (MonadIO m, MonadCatch m) => Path -> Stream m Word8 -> m ()
writeAppend = writeAppendWith defaultChunkSize

{-
-- | Like 'append' but the file is not locked for exclusive writes.
--
-- @since 0.7.0
{-# INLINE appendShared #-}
appendShared :: MonadIO m => Handle -> Stream m Word8 -> m ()
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
readUtf8 :: MonadIO m => Handle -> Stream m Char
readUtf8 = decodeUtf8 . read

-- |
-- > writeUtf8 h s = write h $ encodeUtf8 s
--
-- Encode a stream of unicode characters to UTF8 and write it to the given file
-- handle. Default block buffering applies to the writes.
--
-- @since 0.7.0
{-# INLINE writeUtf8 #-}
writeUtf8 :: MonadIO m => Handle -> Stream m Char -> m ()
writeUtf8 h s = write h $ encodeUtf8 s

-- | Write a stream of unicode characters after encoding to UTF-8 in chunks
-- separated by a linefeed character @'\n'@. If the size of the buffer exceeds
-- @defaultChunkSize@ and a linefeed is not yet found, the buffer is written
-- anyway.  This is similar to writing to a 'Handle' with the 'LineBuffering'
-- option.
--
-- @since 0.7.0
{-# INLINE writeUtf8ByLines #-}
writeUtf8ByLines :: MonadIO m => Handle -> Stream m Char -> m ()
writeUtf8ByLines = undefined

-- | Read UTF-8 lines from a file handle and apply the specified fold to each
-- line. This is similar to reading a 'Handle' with the 'LineBuffering' option.
--
-- @since 0.7.0
{-# INLINE readLines #-}
readLines :: MonadIO m => Handle -> Fold m Char b -> Stream m b
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
readFrames :: (MonadIO m, Storable a)
    => Array a -> Handle -> Fold m a b -> Stream m b
readFrames = undefined -- foldFrames . read

-- | Write a stream to the given file handle buffering up to frames separated
-- by the given sequence or up to a maximum of @defaultChunkSize@.
--
-- @since 0.7.0
{-# INLINE writeByFrames #-}
writeByFrames :: (MonadIO m, Storable a)
    => Array a -> Handle -> Stream m a -> m ()
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
readSliceWith :: (MonadIO m, Storable a)
    => Int -> Handle -> Int -> Int -> Stream m a
readSliceWith chunkSize h pos len = undefined

-- | @readSlice h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the forward direction
-- ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE readSlice #-}
readSlice :: (MonadIO m, Storable a)
    => Handle -> Int -> Int -> Stream m a
readSlice = readSliceWith defaultChunkSize

-- | @readSliceRev h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the reverse direction
-- ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE readSliceRev #-}
readSliceRev :: (MonadIO m, Storable a)
    => Handle -> Int -> Int -> Stream m a
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
writeSlice :: (Monad m, Storable a)
    => Handle -> Int -> Int -> Stream m a -> m ()
writeSlice h i len s = undefined

-- | @writeSliceRev h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE writeSliceRev #-}
writeSliceRev :: (Monad m, Storable a)
    => Handle -> Int -> Int -> Stream m a -> m ()
writeSliceRev arr i len s = undefined
-}
