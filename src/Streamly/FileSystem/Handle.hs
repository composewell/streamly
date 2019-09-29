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
-- > import qualified Streamly.FileSystem.Handle as FH
--

-- IO APIs are divided into two categories, sequential streaming IO APIs and
-- random access IO APIs.

module Streamly.FileSystem.Handle
    (
    -- * Sequential/Streaming IO
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

    -- ** Read From Handle
    -- | 'TextEncoding', 'NewLineMode', and 'Buffering' options of the
    -- underlying handle are ignored. The read occurs from the current seek
    -- position of the file handle. The stream ends as soon as EOF is
    -- encountered.

    -- XXX once we have APIs to read any type 'a' the array stream
    -- reading/writing APIs can be expressed in terms of the polymorphic API.
      read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    , readInChunksOf

    -- ** Write to Handle
    -- | 'TextEncoding', 'NewLineMode', and 'Buffering' options of the
    -- underlying handle are ignored. The write occurs from the current seek
    -- position of the file handle.  The write behavior depends on the 'IOMode'
    -- of the handle.
    --
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeInChunksOf

    -- -- * Random Access (Seek)
    -- -- | Unlike the streaming APIs listed above, these APIs apply to devices or
    -- files that have random access or seek capability.  This type of devices
    -- include disks, files, memory devices and exclude terminals, pipes,
    -- sockets and fifos.
    --
    -- , readIndex
    --  XXX we can make the names consistent with the enumerate APIs. For
    --  example, readFromTo, readFromToUp, readFromToDn, readFrom etc.
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

import Streamly.Internal.FileSystem.Handle
import Prelude hiding (read)
