-- |
-- Module      : Streamly.FileSystem.File
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Warning\: The API of this module is subject to change in future releases.
-- Especially the type for representing paths may change from 'FilePath' to
-- something else.
--
-- Read and write streams and arrays to and from files specified by their paths
-- in the file system. Unlike the handle based APIs which can have a read/write
-- session consisting of multiple reads and writes to the handle, these APIs
-- are one shot read or write APIs. These APIs open the file handle, perform
-- the requested operation and close the handle. These are safer compared to
-- the handle based APIs as there is no possibility of a file descriptor
-- leakage.
--
-- >>> import qualified Streamly.FileSystem.File as File
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

import Streamly.Internal.FileSystem.File
import Prelude hiding (read)
