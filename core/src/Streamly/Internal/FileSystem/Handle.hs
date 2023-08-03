#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.Handle
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The fundamental singleton IO APIs are 'getChunk' and 'putChunk' and the
-- fundamental stream IO APIs built on top of those are
-- 'readChunksWith' and 'writeChunks'. Rest of this module is just
-- combinatorial programming using these.
--
-- We can achieve line buffering by folding lines in the input stream into a
-- stream of arrays using Stream.splitOn or Fold.takeEndBy_ and similar
-- operations. One can wrap the input stream in 'Maybe' type and then use
-- 'writeMaybesWith' to achieve user controlled buffering.

-- TODO: Need a separate module for pread/pwrite based reading writing for
-- seekable devices.  Stateless read/write can be helpful in multithreaded
-- applications.
--
module Streamly.Internal.FileSystem.Handle
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup
    
    -- * Singleton APIs
      getChunk
    , getChunkOf
    , putChunk

    -- * Streams
    , read
    , readWith
    , readChunksWith
    , readChunks

    -- * Unfolds
    , reader
    -- , readUtf8
    -- , readLines
    -- , readFrames
    , readerWith
    , chunkReader
    , chunkReaderWith

    -- * Folds
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    -- , writeLines
    , writeWith
    , writeChunks
    , writeChunksWith
    , writeMaybesWith

    -- * Refolds
    , writer
    , writerWith
    , chunkWriter
    -- , chunkWriterWith

    -- * Stream writes
    , putBytes
    , putBytesWith
    , putChunksWith
    , putChunks

    -- * Random Access (Seek)
    -- | Unlike the streaming APIs listed above, these APIs apply to devices or
    -- files that have random access or seek capability.  This type of devices
    -- include disks, files, memory devices and exclude terminals, pipes,
    -- sockets and fifos.

    -- We can also generate the request pattern using a funciton.
    --
    -- , readIndex
    -- , readFrom -- read from a given position to the end of file
    -- , readFromRev -- read from a given position the beginning of file
    -- , readTo   -- read from beginning up to the given position
    -- , readToRev -- read from end to the given position in file
    -- , readFromTo
    -- , readFromThenTo

    -- , readChunksFrom
    -- , readChunksFromTo
    , chunkReaderFromToWith
    -- , readChunksFromThenToWith

    -- , writeIndex
    -- , writeFrom -- start writing at the given position
    -- , writeFromRev
    -- , writeTo   -- write from beginning up to the given position
    -- , writeToRev
    -- , writeFromTo
    -- , writeFromThenTo
    --
    -- , writeChunksFrom
    -- , writeChunksFromTo
    -- , writeChunksFromToWith
    -- , writeChunksFromThenToWith

    -- * Deprecated
    , readChunksWithBufferOf
    , readWithBufferOf
    , writeChunksWithBufferOf
    , writeWithBufferOf
    )
where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Data.Maybe (isNothing, fromJust)
import Data.Word (Word8)
import Streamly.Internal.Data.Unbox (Unbox)
import System.IO (Handle, SeekMode(..), hGetBufSome, hPutBuf, hSeek)
import Prelude hiding (read)

import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Refold.Type (Refold(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Array.Type
       (Array(..), pinnedWriteNUnsafe, unsafeFreezeWithShrink, byteLength)
import Streamly.Internal.Data.Stream.StreamD.Type (Stream)
import Streamly.Internal.Data.Stream.Chunked (lpackArraysChunksOf)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Array as A
import qualified Streamly.Internal.Data.Array.Type as A
import qualified Streamly.Internal.Data.Stream.Chunked as AS
import qualified Streamly.Internal.Data.MutArray.Type as MArray
import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Fold.Type as FL(refoldMany)
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as D
    (Stream(..), Step(..))
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K (mkStream)

#include "DocTestDataFileSystem.hs"

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

-- | Read a 'ByteArray' consisting of one or more bytes from a file handle. If
-- no data is available on the handle it blocks until at least one byte becomes
-- available. If any data is available then it immediately returns that data
-- without blocking. As a result of this behavior, it may read less than or
-- equal to the size requested.
--
{-# INLINABLE getChunk #-}
getChunk :: MonadIO m => Int -> Handle -> m (Array Word8)
getChunk size h = liftIO $ do
    arr <- MArray.pinnedNewBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    MArray.asPtrUnsafe arr $ \p -> do
        n <- hGetBufSome h p size
        -- XXX shrink only if the diff is significant
        return $
            unsafeFreezeWithShrink $
            arr { MArray.arrEnd = n, MArray.arrBound = size }

-- This could be useful in implementing the "reverse" read APIs or if you want
-- to read arrays of exact size instead of compacting them later. Compacting
-- later requires more copying.
--
-- | Read a 'ByteArray' consisting of exactly the specified number of bytes
-- from a file handle.
--
-- /Unimplemented/
{-# INLINABLE getChunkOf #-}
getChunkOf :: Int -> Handle -> IO (Array Word8)
getChunkOf = undefined

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @getChunksWith size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
--
{-# INLINE _getChunksWith #-}
_getChunksWith :: MonadIO m => Int -> Handle -> Stream m (Array Word8)
_getChunksWith size h = S.fromStreamK go
  where
    -- XXX use cons/nil instead
    go = K.mkStream $ \_ yld _ stp -> do
        arr <- getChunk size h
        if byteLength arr == 0
        then stp
        else yld arr go

-- | @readChunksWith size handle@ reads a stream of arrays from the file
-- handle @handle@.  The maximum size of a single array is limited to @size@.
-- The actual size read may be less than or equal to @size@.
--
-- >>> readChunksWith size h = Stream.unfold Handle.chunkReaderWith (size, h)
--
{-# INLINE_NORMAL readChunksWith #-}
readChunksWith :: MonadIO m => Int -> Handle -> Stream m (Array Word8)
readChunksWith size h = D.Stream step ()
  where
    {-# INLINE_LATE step #-}
    step _ _ = do
        arr <- getChunk size h
        return $
            case byteLength arr of
                0 -> D.Stop
                _ -> D.Yield arr ()

-- | Unfold the tuple @(bufsize, handle)@ into a stream of 'Word8' arrays.
-- Read requests to the IO device are performed using a buffer of size
-- @bufsize@.  The size of an array in the resulting stream is always less than
-- or equal to @bufsize@.
--
{-# INLINE_NORMAL chunkReaderWith #-}
chunkReaderWith :: MonadIO m => Unfold m (Int, Handle) (Array Word8)
chunkReaderWith =
     UF.lmap (uncurry getChunk) UF.repeatM
   & UF.takeWhile ((/= 0) . byteLength)

-- | Same as 'chunkReaderWith'
--
{-# DEPRECATED readChunksWithBufferOf "Please use chunkReaderWith instead." #-}
{-# INLINE_NORMAL readChunksWithBufferOf #-}
readChunksWithBufferOf :: MonadIO m => Unfold m (Int, Handle) (Array Word8)
readChunksWithBufferOf = chunkReaderWith

-- There are two ways to implement this.
--
-- 1. Idiomatic: use a scan on the output of readChunksWith to total
-- the array lengths and trim the last array to correct size.
-- 2. Simply implement it from scratch like readChunksWith.
--
-- XXX Change this to readChunksWithFromTo (bufferSize, from, to, h)?

-- | The input to the unfold is @(from, to, bufferSize, handle)@. It starts
-- reading from the offset `from` in the file and reads up to the offset `to`.
--
{-# INLINE_NORMAL chunkReaderFromToWith #-}
chunkReaderFromToWith :: MonadIO m =>
    Unfold m (Int, Int, Int, Handle) (Array Word8)
chunkReaderFromToWith = Unfold step inject

    where

    inject (from, to, bufSize, h) = do
        liftIO $ hSeek h AbsoluteSeek $ fromIntegral from
        -- XXX Use a strict Tuple?
        return (to - from + 1, bufSize, h)

    {-# INLINE_LATE step #-}
    step (remaining, bufSize, h) =
        if remaining <= 0
        then return D.Stop
        else do
            arr <- getChunk (min bufSize remaining) h
            return $
                case byteLength arr of
                    0 -> D.Stop
                    len ->
                        assert (len <= remaining)
                            $ D.Yield arr (remaining - len, bufSize, h)

-- | @getChunks handle@ reads a stream of arrays from the specified file
-- handle.  The maximum size of a single array is limited to
-- @defaultChunkSize@. The actual size read may be less than or equal to
-- @defaultChunkSize@.
--
-- >>> readChunks = Handle.readChunksWith IO.defaultChunkSize
--
-- /Pre-release/
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Handle -> Stream m (Array Word8)
readChunks = readChunksWith defaultChunkSize

-- | Unfolds a handle into a stream of 'Word8' arrays. Requests to the IO
-- device are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.
--
-- >>> chunkReader = Unfold.first IO.defaultChunkSize Handle.chunkReaderWith
--
{-# INLINE chunkReader #-}
chunkReader :: MonadIO m => Unfold m Handle (Array Word8)
chunkReader = UF.first defaultChunkSize chunkReaderWith

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | Unfolds the tuple @(bufsize, handle)@ into a byte stream, read requests
-- to the IO device are performed using buffers of @bufsize@.
--
-- >>> readerWith = Unfold.many Array.reader Handle.chunkReaderWith
--
{-# INLINE readerWith #-}
readerWith :: MonadIO m => Unfold m (Int, Handle) Word8
readerWith = UF.many A.reader chunkReaderWith

-- | Same as 'readerWith'
--
{-# DEPRECATED readWithBufferOf "Please use 'readerWith' instead." #-}
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: MonadIO m => Unfold m (Int, Handle) Word8
readWithBufferOf = readerWith

{-# INLINE concatChunks #-}
concatChunks :: (Monad m, Unbox a) => Stream m (Array a) -> Stream m a
concatChunks = S.unfoldMany A.reader

-- | @readWith bufsize handle@ reads a byte stream from a file
-- handle, reads are performed in chunks of up to @bufsize@.
--
-- >>> readWith size h = Stream.unfoldMany Array.reader $ Handle.readChunksWith size h
--
-- /Pre-release/
{-# INLINE readWith #-}
readWith :: MonadIO m => Int -> Handle -> Stream m Word8
readWith size h = concatChunks $ readChunksWith size h

-- | Unfolds a file handle into a byte stream. IO requests to the device are
-- performed in sizes of
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.
--
-- >>> reader = Unfold.many Array.reader chunkReader
--
{-# INLINE reader #-}
reader :: MonadIO m => Unfold m Handle Word8
reader = UF.many A.reader chunkReader

-- | Generate a byte stream from a file 'Handle'.
--
-- >>> read h = Stream.unfoldMany Array.reader $ Handle.readChunks h
--
-- /Pre-release/
{-# INLINE read #-}
read :: MonadIO m => Handle -> Stream m Word8
read = concatChunks . readChunks

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an 'Array' to a file handle.
--
{-# INLINABLE putChunk #-}
putChunk :: MonadIO m => Handle -> Array a -> m ()
putChunk _ arr | byteLength arr == 0 = return ()
putChunk h arr = A.asPtrUnsafe arr $ \ptr ->
    liftIO $ hPutBuf h ptr aLen

    where

    -- XXX We should have the length passed by asPtrUnsafe itself.
    aLen = A.byteLength arr

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- XXX use an unfold to fromObjects or fromUnfold so that we can put any object
-- | Write a stream of arrays to a handle.
--
-- >>> putChunks h = Stream.fold (Fold.drainBy (Handle.putChunk h))
--
{-# INLINE putChunks #-}
putChunks :: MonadIO m => Handle -> Stream m (Array a) -> m ()
putChunks h = S.fold (FL.drainMapM (putChunk h))

-- XXX AS.compact can be written idiomatically in terms of foldMany, just like
-- AS.concat is written in terms of foldMany. Once that is done we can write
-- idiomatic def in the docs.
--
-- | @putChunksWith bufsize handle stream@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- The chunk size is only a maximum and the actual writes could be smaller as
-- we do not split the arrays to fit exactly to the specified size.
--
{-# INLINE putChunksWith #-}
putChunksWith :: (MonadIO m, Unbox a)
    => Int -> Handle -> Stream m (Array a) -> m ()
putChunksWith n h xs = putChunks h $ AS.compact n xs

-- > putBytesWith n h m = Handle.putChunks h $ A.pinnedChunksOf n m

-- | @putBytesWith bufsize handle stream@ writes @stream@ to @handle@
-- in chunks of @bufsize@.  A write is performed to the IO device as soon as we
-- collect the required input size.
--
{-# INLINE putBytesWith #-}
putBytesWith :: MonadIO m => Int -> Handle -> Stream m Word8 -> m ()
putBytesWith n h m = putChunks h $ A.pinnedChunksOf n m

-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Type.defaultChunkSize' before writing.
--
-- NOTE: This may perform better than the 'write' fold, you can try this if you
-- need some extra perf boost.
--
-- >>> putBytes = Handle.putBytesWith IO.defaultChunkSize
--
{-# INLINE putBytes #-}
putBytes :: MonadIO m => Handle -> Stream m Word8 -> m ()
putBytes = putBytesWith defaultChunkSize

-- | Write a stream of arrays to a handle. Each array in the stream is written
-- to the device as a separate IO request.
--
-- writeChunks h = Fold.drainBy (Handle.putChunk h)
--
{-# INLINE writeChunks #-}
writeChunks :: MonadIO m => Handle -> Fold m (Array a) ()
writeChunks h = FL.drainMapM (putChunk h)

-- | Like writeChunks but uses the experimental 'Refold' API.
--
-- /Internal/
{-# INLINE chunkWriter #-}
chunkWriter :: MonadIO m => Refold m Handle (Array a) ()
chunkWriter = Refold.drainBy putChunk

-- XXX lpackArraysChunksOf should be written idiomatically

-- | @writeChunksWith bufsize handle@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- We never split an array, if a single array is bigger than the specified size
-- it emitted as it is. Multiple arrays are coalesed as long as the total size
-- remains below the specified size.
--
{-# INLINE writeChunksWith #-}
writeChunksWith :: (MonadIO m, Unbox a)
    => Int -> Handle -> Fold m (Array a) ()
writeChunksWith n h = lpackArraysChunksOf n (writeChunks h)

-- | Same as 'writeChunksWith'
--
{-# DEPRECATED writeChunksWithBufferOf "Please use writeChunksWith instead." #-}
{-# INLINE writeChunksWithBufferOf #-}
writeChunksWithBufferOf :: (MonadIO m, Unbox a)
    => Int -> Handle -> Fold m (Array a) ()
writeChunksWithBufferOf = writeChunksWith

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- XXX Maybe we should have a Fold.chunksOf like we have Stream.chunksOf

-- | @writeWith reqSize handle@ writes the input stream to @handle@.
-- Bytes in the input stream are collected into a buffer until we have a chunk
-- of @reqSize@ and then written to the IO device.
--
-- >>> writeWith n h = Fold.groupsOf n (Array.writeNUnsafe n) (Handle.writeChunks h)
--
{-# INLINE writeWith #-}
writeWith :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeWith n h = FL.groupsOf n (pinnedWriteNUnsafe n) (writeChunks h)

-- | Same as 'writeWith'
--
{-# DEPRECATED writeWithBufferOf "Please use writeWith instead." #-}
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeWithBufferOf = writeWith

-- | Write a stream of 'Maybe' values. Keep buffering the just values in an
-- array until a 'Nothing' is encountered or the buffer size exceeds the
-- specified limit, at that point flush the buffer to the handle.
--
-- /Pre-release/
{-# INLINE writeMaybesWith #-}
writeMaybesWith :: (MonadIO m )
    => Int -> Handle -> Fold m (Maybe Word8) ()
writeMaybesWith n h =
    let writeNJusts = FL.lmap fromJust $ A.pinnedWriteN n
        writeOnNothing = FL.takeEndBy_ isNothing writeNJusts
    in FL.many writeOnNothing (writeChunks h)

-- | Like 'writeWith' but uses the experimental 'Refold' API.
--
-- /Internal/
{-# INLINE writerWith #-}
writerWith :: MonadIO m => Int -> Refold m Handle Word8 ()
writerWith n =
    FL.refoldMany (FL.take n $ pinnedWriteNUnsafe n) chunkWriter

-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Type.defaultChunkSize' before writing
-- to the IO device.
--
-- >>> write = Handle.writeWith IO.defaultChunkSize
--
{-# INLINE write #-}
write :: MonadIO m => Handle -> Fold m Word8 ()
write = writeWith defaultChunkSize

-- | Like 'write'  but uses the experimental 'Refold' API.
--
-- /Internal/
{-# INLINE writer #-}
writer :: MonadIO m => Refold m Handle Word8 ()
writer = writerWith defaultChunkSize

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
{-# INLINE readUtf8 #-}
readUtf8 :: MonadIO m => Handle -> Stream m Char
readUtf8 = decodeUtf8 . read

-- |
-- > writeUtf8 h s = write h $ encodeUtf8 s
--
-- Encode a stream of unicode characters to UTF8 and write it to the given file
-- handle. Default block buffering applies to the writes.
--
{-# INLINE writeUtf8 #-}
writeUtf8 :: MonadIO m => Handle -> Stream m Char -> m ()
writeUtf8 h s = write h $ encodeUtf8 s

-- | Write a stream of unicode characters after encoding to UTF-8 in chunks
-- separated by a linefeed character @'\n'@. If the size of the buffer exceeds
-- @defaultChunkSize@ and a linefeed is not yet found, the buffer is written
-- anyway.  This is similar to writing to a 'Handle' with the 'LineBuffering'
-- option.
--
{-# INLINE writeUtf8ByLines #-}
writeUtf8ByLines :: MonadIO m => Handle -> Stream m Char -> m ()
writeUtf8ByLines = undefined

-- | Read UTF-8 lines from a file handle and apply the specified fold to each
-- line. This is similar to reading a 'Handle' with the 'LineBuffering' option.
--
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
{-# INLINE readFrames #-}
readFrames :: (MonadIO m, Unboxed a)
    => Array a -> Handle -> Fold m a b -> Stream m b
readFrames = undefined -- foldFrames . read

-- | Write a stream to the given file handle buffering up to frames separated
-- by the given sequence or up to a maximum of @defaultChunkSize@.
--
{-# INLINE writeByFrames #-}
writeByFrames :: (MonadIO m, Unboxed a)
    => Array a -> Handle -> Stream m a -> m ()
writeByFrames = undefined

-------------------------------------------------------------------------------
-- Framing by time
-------------------------------------------------------------------------------

-- | Write collecting the input in sessions of n seconds or if chunkSize
-- gets exceeded.
{-# INLINE writeByChunksOrSessionsOf #-}
writeByChunksOrSessionsOf :: MonadIO m
    => Int -> Double -> Handle -> Stream m Word8 -> m ()
writeByChunksOrSessionsOf chunkSize sessionSize h m = undefined

-- | Write collecting the input in sessions of n seconds or if defaultChunkSize
-- gets exceeded.
{-# INLINE writeBySessionsOf #-}
writeBySessionsOf :: MonadIO m => Double -> Handle -> Stream m Word8 -> m ()
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
{-# INLINE readIndex #-}
readIndex :: Unboxed a => Handle -> Int -> Maybe a
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
readSliceWith :: (MonadIO m, Unboxed a)
    => Int -> Handle -> Int -> Int -> Stream m a
readSliceWith chunkSize h pos len = undefined

-- | @readSlice h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the forward direction
-- ending at the index @i + count - 1@.
--
{-# INLINE readSlice #-}
readSlice :: (MonadIO m, Unboxed a)
    => Handle -> Int -> Int -> Stream m a
readSlice = readSliceWith A.defaultChunkSize

-- | @readSliceRev h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the reverse direction
-- ending at the index @i - count + 1@.
--
{-# INLINE readSliceRev #-}
readSliceRev :: (MonadIO m, Unboxed a)
    => Handle -> Int -> Int -> Stream m a
readSliceRev h i count = undefined

-- | Write the given element at the given index in the file.
--
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Unboxed a) => Handle -> Int -> a -> m ()
writeIndex h i a = undefined

-- | @writeSlice h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the forward
-- direction ending at the index @i + count - 1@.
--
{-# INLINE writeSlice #-}
writeSlice :: (Monad m, Unboxed a)
    => Handle -> Int -> Int -> Stream m a -> m ()
writeSlice h i len s = undefined

-- | @writeSliceRev h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
{-# INLINE writeSliceRev #-}
writeSliceRev :: (Monad m, Unboxed a)
    => Handle -> Int -> Int -> Stream m a -> m ()
writeSliceRev arr i len s = undefined
-}
