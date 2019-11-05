{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.FileSystem.Handle
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Read and write streams and arrays to and from file handles. File handle IO
-- APIs are quite similar to "Streamly.Memory.Array" read write APIs. In that
-- regard, arrays can be considered as in-memory files or files can be
-- considered as on-disk arrays.
--
-- Control over file reading and writing behavior in terms of buffering,
-- encoding, decoding is in the hands of the programmer, the 'TextEncoding',
-- 'NewLineMode', and 'Buffering' options of the underlying handle provided by
-- GHC are not needed and ignored.
--
-- = Programmer Notes
--
-- > import qualified Streamly.FileSystem.Handle as FH
--
-- For additional, experimental APIs take a look at
-- "Streamly.Internal.FileSystem.Handle" module.
--
-- = Performance Notes
--
-- In some cases the stream type based APIs in the
-- "Streamly.Internal.FileSystem.Handle" module may be more efficient compared
-- to the unfold/fold based APIs exposed from this module because of better
-- fusion by GHC. However, with the streamly fusion GHC plugin (upcoming) these
-- APIs would perform as well as the stream based APIs in all cases.

-- IO APIs are divided into two categories, sequential streaming IO APIs and
-- random access IO APIs.

module Streamly.FileSystem.Handle
    (
    -- * Sequential/Streaming IO
    -- | Stream data to or from a file or device sequentially.  When reading,
    -- the stream is lazy and generated on-demand as the consumer consumes it.
    -- Read IO requests to the IO device are performed in chunks limited to a
    -- maximum size of 32KiB, this is referred to as
    -- 'Streamly.Internal.Memory.Array.Types.defaultChunkSize' in the
    -- documentation. One IO request may or may not read the full
    -- chunk. If the whole stream is not consumed, it is possible that we may
    -- read slightly more from the IO device than what the consumer needed.
    -- Unless specified otherwise in the API, writes are collected into chunks
    -- of 'Streamly.Internal.Memory.Array.Types.defaultChunkSize' before they
    -- are written to the IO device.

    -- Streaming APIs work for all kind of devices, seekable or non-seekable;
    -- including disks, files, memory devices, terminals, pipes, sockets and
    -- fifos. While random access APIs work only for files or devices that have
    -- random access or seek capability for example disks, memory devices.
    -- Devices like terminals, pipes, sockets and fifos do not have random
    -- access capability.

    -- ** Read From Handle
    -- | 'TextEncoding', 'NewLineMode', and 'Buffering' options of the
    -- underlying handle are ignored. The read occurs from the current seek
    -- position of the file handle. The stream ends as soon as EOF is
    -- encountered.

      read
    , readRequestsOf
    , readChunks
    , readChunksRequestsOf

    -- ** Write to Handle
    -- | 'TextEncoding', 'NewLineMode', and 'Buffering' options of the
    -- underlying handle are ignored. The write occurs from the current seek
    -- position of the file handle.  The write behavior depends on the 'IOMode'
    -- of the handle.

    , write
    , writeRequestsOf
    , writeChunks
    )
where

import Streamly.Internal.FileSystem.Handle
import Prelude hiding (read)
