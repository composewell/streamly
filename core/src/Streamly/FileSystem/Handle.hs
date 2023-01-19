#include "inline.hs"

-- |
-- Module      : Streamly.FileSystem.Handle
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- >>> import qualified Streamly.FileSystem.Handle as Handle
--
-- Read and write byte streams and array streams to and from file handles
-- ('Handle').
--
-- The 'TextEncoding', 'NewLineMode', and 'Buffering' options of the underlying
-- GHC 'Handle' are ignored by these APIs. Please use "Streamly.Unicode.Stream"
-- module for encoding and decoding a byte stream, use stream splitting
-- operations in "Streamly.Data.Stream" to create a stream of lines or to split
-- the input stream on any other type of boundaries.
--
-- To set the read or write start position use 'hSeek' on the 'Handle', the
-- 'Streamly.Data.Stream.before' combinator may be used to do that on a
-- streaming combinator.  To restrict the length of read or write use the stream
-- trimming operations like 'Streamly.Data.Stream.take'.
--
-- Note that a 'Handle' is inherently stateful, therefore, we cannot use these
-- APIs from multiple threads without serialization; reading or writing in one
-- thread would affect the file position for other threads.
--
-- For additional, experimental APIs take a look at
-- "Streamly.Internal.FileSystem.Handle" module.

-- Design notes:
--
-- By design, file handle IO APIs are quite similar to
-- "Streamly.Data.Array" read write APIs. In that regard, arrays can be
-- considered as in-memory files or files can be considered as on-disk arrays.
--
module Streamly.FileSystem.Handle
    (
    -- * Singleton IO
    -- | Read or write a single buffer.
      getChunk
    , putChunk

    -- * Streaming IO
    -- | Read or write a stream of data to or from a file or device
    -- sequentially.
    --
    -- Read requests to the IO device are performed in chunks limited to a
    -- maximum size of 'Streamly.Internal.System.IO.defaultChunkSize'.  Note
    -- that the size of the actual chunks in the resulting stream may be less
    -- than the @defaultChunkSize@ but it can never exceed it.  If the whole
    -- stream is not consumed, it is possible that we may have read slightly
    -- more from the IO device than what the consumer needed.
    --
    -- Unless specified otherwise in the API, writes are collected into chunks
    -- of 'Streamly.Internal.System.IO.defaultChunkSize' before they are
    -- written to the IO device.

    -- Internal notes:
    --
    -- Streaming APIs work for all kind of devices, seekable or non-seekable;
    -- including disks, files, memory devices, terminals, pipes, sockets and
    -- fifos. While random access APIs work only for files or devices that have
    -- random access or seek capability for example disks, memory devices.
    -- Devices like terminals, pipes, sockets and fifos do not have random
    -- access capability.

    -- ** Reading
    -- | 'TextEncoding', 'NewLineMode', and 'Buffering' options of the
    -- underlying handle are ignored. The read occurs from the current seek
    -- position of the file handle. The stream ends as soon as EOF is
    -- encountered.

    -- -- *** Streams
    -- , read
    -- , readWith
    -- , readChunks
    -- , readChunksWith

    -- -- *** Unfolds
    , reader
    , readerWith
    , chunkReader
    , chunkReaderWith

    -- ** Folds
    -- | 'TextEncoding', 'NewLineMode', and 'Buffering' options of the
    -- underlying handle are ignored. The write occurs from the current seek
    -- position of the file handle.  The write behavior depends on the 'IOMode'
    -- of the handle.

    , write
    , writeWith
    , writeChunks

     -- * Deprecated
    , read
    , readWithBufferOf
    , readChunks
    , readChunksWithBufferOf
    , writeWithBufferOf
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Type (Array)
import Streamly.Internal.Data.Unfold.Type (Unfold)
import System.IO (Handle)

import Streamly.Internal.FileSystem.Handle hiding (read, readChunks)
import Prelude hiding (read)

-- | Same as 'reader'
--
{-# DEPRECATED read "Please use 'reader' instead" #-}
{-# INLINE read #-}
read :: MonadIO m => Unfold m Handle Word8
read = reader

-- | Same as 'chunkReader'
--
{-# DEPRECATED readChunks "Please use 'chunkReader' instead" #-}
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m Handle (Array Word8)
readChunks = chunkReader
