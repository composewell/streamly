-- |
-- Module      : Streamly.FileSystem.FileIO
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Read and write streams and arrays to and from files specified by their paths
-- in the file system. These APIs open the file handle, perform the requested
-- operation and close the handle. These are higher level and safer compared to
-- the handle based APIs as there is no possibility of a file descriptor
-- leakage.
--
-- Files are always opened in:
--
-- * __Binary mode__ — encoding, decoding, and newline translation should be
--   handled explicitly by the streaming APIs.
-- * __Unbuffered mode__ — buffering can be managed explicitly via streaming
--   APIs.
--
-- File system paths are specified using the 'Streamly.FileSystem.Path.Path'
-- type. If you want to convert between 'String' or 'FilePath' and 'Path' use
-- 'Streamly.FileSystem.Path.fromString_', 'Streamly.FileSystem.Path.toString'
-- from the "Streamly.FileSystem.Path" module..
--
-- >> import qualified Streamly.FileSystem.FileIO as File
--
module Streamly.FileSystem.FileIO
    (
    -- * Streaming IO
    -- | Stream data to or from a file or device sequentially.  When reading,
    -- the stream is lazy and generated on-demand as the consumer consumes it.
    -- Read IO requests to the IO device are performed in chunks limited to a
    -- maximum size of 32KiB, this is referred to as @defaultChunkSize@ in the
    -- documentation. One IO request may or may not read the full
    -- chunk. If the whole stream is not consumed, it is possible that we may
    -- read slightly more from the IO device than what the consumer needed.
    -- When writing, unless specified otherwise in the API, writes are
    -- collected into chunks of @defaultChunkSize@ before they are written to
    -- the IO device.

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

    -- ** Folds
    , write
    , writeWith
    , writeChunks
    )
where

import Streamly.Internal.FileSystem.FileIO
import Prelude hiding (read)
