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
    -- * Singleton APIs
      getChunk
    , getChunkOf
    , putChunk

    -- * Byte Stream Read
    , read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    , readWith

    , getBytes
    , getBytesWith

    -- * Chunked Stream Read
    , readChunks
    , readChunksWith

    , getChunksWith
    , getChunks

    -- * Byte Stream Write
    -- Byte stream write (Folds)
    , write
    , consumer
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    -- , writeLines
    , writeWith
    , writeMaybesWith

    , putBytes
    , putBytesWith

    -- * Chunked Stream Write
    , writeChunks
    , writeChunksWith

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
    , readChunksFromToWith
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
import Streamly.Internal.Data.Unboxed (Unboxed)
import System.IO (Handle, SeekMode(..), hGetBufSome, hPutBuf, hSeek)
import Prelude hiding (read)

import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Refold.Type (Refold(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Array.Unboxed.Type
       (Array(..), writeNUnsafe, unsafeFreezeWithShrink, byteLength)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Stream (fromStreamD)
-- import Streamly.Internal.Data.Array.Unboxed (lpackArraysChunksOf)
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Array.Unboxed as A
import qualified Streamly.Internal.Data.Array.Unboxed.Type as A
import qualified Streamly.Internal.Data.Array.Unboxed.Mut.Type as MArray
--import qualified Streamly.Internal.Data.Array.Unboxed as AS
import qualified Streamly.Internal.Data.Array.Stream.Mut.Foreign as AS
import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Unfold as UF

-- $setup
-- >>> import qualified Streamly.Data.Array.Unboxed as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.FileSystem.Handle as Handle
-- >>> import qualified Streamly.Prelude as Stream
--
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold (first)
-- >>> import qualified Streamly.Internal.FileSystem.Handle as Handle
-- >>> import qualified Streamly.Internal.System.IO as IO (defaultChunkSize)

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
-- @since 0.8.1
{-# INLINABLE getChunk #-}
getChunk :: MonadIO m => Int -> Handle -> m (Array Word8)
getChunk size h = liftIO $ do
    arr <- MArray.newPinnedArrayBytes size
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
-- @since 0.9.0
{-# INLINE _getChunksWith #-}
_getChunksWith :: (MonadIO m)
    => Int -> Handle -> S.Stream m (Array Word8)
_getChunksWith size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld _ stp -> do
        arr <- getChunk size h
        if byteLength arr == 0
        then stp
        else yld arr go

-- | @getChunksWith size handle@ reads a stream of arrays from the file
-- handle @handle@.  The maximum size of a single array is limited to @size@.
-- The actual size read may be less than or equal to @size@.
--
-- >>> getChunksWith size h = Stream.unfold Handle.readChunksWith (size, h)
--
-- @since 0.9.0
{-# INLINE_NORMAL getChunksWith #-}
getChunksWith :: (MonadIO m) =>
    Int -> Handle -> S.Stream m (Array Word8)
getChunksWith size h = fromStreamD (D.Stream step ())
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
-- @since 0.9.0
{-# INLINE_NORMAL readChunksWith #-}
readChunksWith :: MonadIO m => Unfold m (Int, Handle) (Array Word8)
readChunksWith =
     UF.lmap (uncurry getChunk) UF.repeatM
   & UF.takeWhile ((/= 0) . byteLength)


-- | Same as 'readChunksWith'
--
-- @since 0.7.0
{-# DEPRECATED readChunksWithBufferOf "Please use readChunksWith instead." #-}
{-# INLINE_NORMAL readChunksWithBufferOf #-}
readChunksWithBufferOf :: MonadIO m => Unfold m (Int, Handle) (Array Word8)
readChunksWithBufferOf = readChunksWith

-- There are two ways to implement this.
--
-- 1. Idiomatic: use a scan on the output of readChunksWith to total
-- the array lengths and trim the last array to correct size.
-- 2. Simply implement it from scratch like readChunksWith.
--
-- XXX Change this to readChunksWithFromTo (bufferSize, from, to, h)?
--
-- | The input to the unfold is @(from, to, bufferSize, handle)@. It starts
-- reading from the offset `from` in the file and reads up to the offset `to`.
--
--
{-# INLINE_NORMAL readChunksFromToWith #-}
readChunksFromToWith :: MonadIO m =>
    Unfold m (Int, Int, Int, Handle) (Array Word8)
readChunksFromToWith = Unfold step inject

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


-- XXX read 'Array a' instead of Word8
--
-- | @getChunks handle@ reads a stream of arrays from the specified file
-- handle.  The maximum size of a single array is limited to
-- @defaultChunkSize@. The actual size read may be less than or equal to
-- @defaultChunkSize@.
--
-- >>> getChunks = Handle.getChunksWith IO.defaultChunkSize
--
-- @since 0.9.0
{-# INLINE getChunks #-}
getChunks :: (MonadIO m) => Handle -> S.Stream m (Array Word8)
getChunks = getChunksWith defaultChunkSize

-- | Unfolds a handle into a stream of 'Word8' arrays. Requests to the IO
-- device are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize'.
--
-- >>> readChunks = Unfold.first IO.defaultChunkSize Handle.readChunksWith
--
-- @since 0.7.0
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m Handle (Array Word8)
readChunks = UF.first defaultChunkSize readChunksWith

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | Unfolds the tuple @(bufsize, handle)@ into a byte stream, read requests
-- to the IO device are performed using buffers of @bufsize@.
--
-- >>> readWith = Unfold.many Array.read Handle.readChunksWith
--
-- @since 0.9.0
{-# INLINE readWith #-}
readWith :: MonadIO m => Unfold m (Int, Handle) Word8
readWith = UF.many A.read readChunksWith

-- | Same as 'readWith'
--
-- @since 0.7.0
{-# DEPRECATED readWithBufferOf "Please use readWith instead." #-}
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: MonadIO m => Unfold m (Int, Handle) Word8
readWithBufferOf = readWith

-- | @getBytesWith bufsize handle@ reads a byte stream from a file
-- handle, reads are performed in chunks of up to @bufsize@.
--
-- >>> getBytesWith size h = Stream.unfoldMany Array.read $ Handle.getChunksWith size h
--
-- /Pre-release/
{-# INLINE getBytesWith #-}
getBytesWith :: (MonadIO m) => Int -> Handle -> S.Stream m Word8
getBytesWith size h = S.concat $ getChunksWith size h

-- TODO
-- Generate a stream of elements of the given type from a file 'Handle'.
-- read :: (MonadIO m, Unboxed a) => Handle -> S.Stream m a
--
-- | Unfolds a file handle into a byte stream. IO requests to the device are
-- performed in sizes of
-- 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize'.
--
-- >>> read = Unfold.many Handle.readChunks Array.read
--
-- @since 0.7.0
{-# INLINE read #-}
read :: MonadIO m => Unfold m Handle Word8
read = UF.many A.read readChunks

-- | Generate a byte stream from a file 'Handle'.
--
-- >>> getBytes h = Stream.unfoldMany Array.read $ Handle.getChunks h
--
-- /Pre-release/
{-# INLINE getBytes #-}
getBytes :: (MonadIO m) => Handle -> S.Stream m Word8
getBytes = S.concat . getChunks

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an 'Array' to a file handle.
--
-- @since 0.8.1
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

{-# INLINE mapM'_ #-}
mapM'_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM'_ f = D.mapM_ f . S.toStreamD

-- XXX use an unfold to fromObjects or fromUnfold so that we can put any object
-- | Write a stream of arrays to a handle.
--
-- >>> putChunks h = Stream.mapM_ (Handle.putChunk h)
--
-- @since 0.7.0
{-# INLINE putChunks #-}
putChunks :: MonadIO m => Handle -> Stream m (Array a) -> m ()
putChunks h = mapM'_ (putChunk h)

-- XXX AS.compact can be written idiomatically in terms of foldMany, just like
-- AS.concat is written in terms of foldMany. Once that is done we can write
-- idiomatic def in the docs.
--
-- | @putChunksWith bufsize handle stream@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- The chunk size is only a maximum and the actual writes could be smaller as
-- we do not split the arrays to fit exactly to the specified size.
--
-- @since 0.9.0
{-# INLINE putChunksWith #-}
putChunksWith :: (MonadIO m, Unboxed a)
    => Int -> Handle -> Stream m (Array a) -> m ()
putChunksWith n h xs = putChunks h $ AS.compact n xs

-- | @putBytesWith bufsize handle stream@ writes @stream@ to @handle@
-- in chunks of @bufsize@.  A write is performed to the IO device as soon as we
-- collect the required input size.
--
-- >>> putBytesWith n h m = Handle.putChunks h $ Stream.arraysOf n m
--
-- @since 0.9.0
{-# INLINE putBytesWith #-}
putBytesWith :: MonadIO m => Int -> Handle -> Stream m Word8 -> m ()
putBytesWith n h m = putChunks h $ S.arraysOf n m

-- putBytesWith n h m = putChunks h $ AS.arraysOf n m

-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize' before writing.
--
-- NOTE: This may perform better than the 'write' fold, you can try this if you
-- need some extra perf boost.
--
-- >>> putBytes = Handle.putBytesWith IO.defaultChunkSize
--
-- @since 0.7.0
{-# INLINE putBytes #-}
putBytes :: MonadIO m => Handle -> Stream m Word8 -> m ()
putBytes = putBytesWith defaultChunkSize

-- | Write a stream of arrays to a handle. Each array in the stream is written
-- to the device as a separate IO request.
--
-- writeChunks h = Fold.drainBy (Handle.putChunk h)
--
-- @since 0.7.0
{-# INLINE writeChunks #-}
writeChunks :: MonadIO m => Handle -> Fold m (Array a) ()
writeChunks h = FL.drainBy (putChunk h)

-- | Like writeChunks but uses the experimental 'Refold' API.
--
-- /Internal/
{-# INLINE consumeChunks #-}
consumeChunks :: MonadIO m => Refold m Handle (Array a) ()
consumeChunks = Refold.drainBy putChunk

-- XXX lpackArraysChunksOf should be written idiomatically
--
-- | @writeChunksWith bufsize handle@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- We never split an array, if a single array is bigger than the specified size
-- it emitted as it is. Multiple arrays are coalesed as long as the total size
-- remains below the specified size.
--
-- @since 0.9.0
{-# INLINE writeChunksWith #-}
writeChunksWith :: (MonadIO m, Unboxed a)
    => Int -> Handle -> Fold m (Array a) ()
writeChunksWith n h = lpackArraysChunksOf n (writeChunks h)

-- | Same as 'writeChunksWith'
--
-- @since 0.7.0
{-# DEPRECATED writeChunksWithBufferOf "Please use writeChunksWith instead." #-}
{-# INLINE writeChunksWithBufferOf #-}
writeChunksWithBufferOf :: (MonadIO m, Unboxed a)
    => Int -> Handle -> Fold m (Array a) ()
writeChunksWithBufferOf = writeChunksWith

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- XXX Maybe we should have a Fold.arraysOf like we have Stream.arraysOf
--
-- | @writeWith reqSize handle@ writes the input stream to @handle@.
-- Bytes in the input stream are collected into a buffer until we have a chunk
-- of @reqSize@ and then written to the IO device.
--
-- >>> writeWith n h = Fold.chunksOf n (Array.writeNUnsafe n) (Handle.writeChunks h)
--
-- @since 0.7.0
{-# INLINE writeWith #-}
writeWith :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeWith n h = FL.chunksOf n (writeNUnsafe n) (writeChunks h)

-- | Same as 'writeWith'
--
-- @since 0.7.0
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
    let writeNJusts = FL.lmap fromJust $ A.writeN n
        writeOnNothing = FL.takeEndBy_ isNothing writeNJusts
    in FL.many writeOnNothing (writeChunks h)

-- | Like 'writeWith'  but uses the experimental 'Refold' API.
--
-- /Internal/
{-# INLINE consumerWith #-}
consumerWith :: MonadIO m => Int -> Refold m Handle Word8 ()
consumerWith n =
    FL.refoldMany (FL.take n $ writeNUnsafe n) consumeChunks

-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize' before writing
-- to the IO device.
--
-- >>> write = Handle.writeWith IO.defaultChunkSize
--
-- @since 0.7.0
{-# INLINE write #-}
write :: MonadIO m => Handle -> Fold m Word8 ()
write = writeWith defaultChunkSize

-- | Like 'write'  but uses the experimental 'Refold' API.
--
-- /Internal/
{-# INLINE consumer #-}
consumer :: MonadIO m => Refold m Handle Word8 ()
consumer = consumerWith defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Unboxed a) => Handle -> Stream m a -> m ()
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
readUtf8 :: (MonadIO m) => Handle -> S.Stream m Char
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
writeUtf8ByLines :: (MonadIO m) => Handle -> S.Stream m Char -> m ()
writeUtf8ByLines = undefined

-- | Read UTF-8 lines from a file handle and apply the specified fold to each
-- line. This is similar to reading a 'Handle' with the 'LineBuffering' option.
--
-- @since 0.7.0
{-# INLINE readLines #-}
readLines :: (MonadIO m) => Handle -> Fold m Char b -> S.Stream m b
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
readFrames :: (MonadIO m, Unboxed a)
    => Array a -> Handle -> Fold m a b -> S.Stream m b
readFrames = undefined -- foldFrames . read

-- | Write a stream to the given file handle buffering up to frames separated
-- by the given sequence or up to a maximum of @defaultChunkSize@.
--
-- @since 0.7.0
{-# INLINE writeByFrames #-}
writeByFrames :: (MonadIO m, Unboxed a)
    => Array a -> Handle -> S.Stream m a -> m ()
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
-- @since 0.7.0
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
    => Int -> Handle -> Int -> Int -> S.Stream m a
readSliceWith chunkSize h pos len = undefined

-- | @readSlice h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the forward direction
-- ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE readSlice #-}
readSlice :: (MonadIO m, Unboxed a)
    => Handle -> Int -> Int -> S.Stream m a
readSlice = readSliceWith A.defaultChunkSize

-- | @readSliceRev h i count@ streams a slice from the file handle @h@ starting
-- at index @i@ and reading up to @count@ elements in the reverse direction
-- ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE readSliceRev #-}
readSliceRev :: (MonadIO m, Unboxed a)
    => Handle -> Int -> Int -> S.Stream m a
readSliceRev h i count = undefined

-- | Write the given element at the given index in the file.
--
-- @since 0.7.0
{-# INLINE writeIndex #-}
writeIndex :: (MonadIO m, Unboxed a) => Handle -> Int -> a -> m ()
writeIndex h i a = undefined

-- | @writeSlice h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the forward
-- direction ending at the index @i + count - 1@.
--
-- @since 0.7.0
{-# INLINE writeSlice #-}
writeSlice :: (Monad m, Unboxed a)
    => Handle -> Int -> Int -> S.Stream m a -> m ()
writeSlice h i len s = undefined

-- | @writeSliceRev h i count stream@ writes a stream to the file handle @h@
-- starting at index @i@ and writing up to @count@ elements in the reverse
-- direction ending at the index @i - count + 1@.
--
-- @since 0.7.0
{-# INLINE writeSliceRev #-}
writeSliceRev :: (Monad m, Unboxed a)
    => Handle -> Int -> Int -> S.Stream m a -> m ()
writeSliceRev arr i len s = undefined
-}
