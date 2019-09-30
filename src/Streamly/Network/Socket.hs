-- |
-- Module      : Streamly.Network.Socket
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Read and write streams and arrays to and from network sockets. Socket IO
-- APIs are quite similar to "Streamly.Memory.Array" read write APIs and almost
-- identical to the sequential streaming APIs in "Streamly.Internal.FileSystem.File".
--
-- Read IO requests to the socket are performed in chunks of 32KiB, this is
-- referred to as @defaultChunkSize@ in the documentation. One IO request may
-- or may not read the full chunk.  Unless specified otherwise in the API,
-- writes are collected into chunks of @defaultChunkSize@ before they are
-- written to the socket. APIs are provided to control the chunking and
-- framing behavior.
--
-- > import qualified Streamly.Network.Socket as SK
--

module Streamly.Network.Socket
    (
    -- * Use a socket
      withSocket
    , withSocketS

    -- * Read from connection
    , read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    -- , readByChunks

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf

    -- , readArraysUpto
    -- , readArraysOf
    , readArrays

    -- * Write to connection
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeInChunksOf

    -- -- * Array Write
    , writeArray
    , writeArrays

    -- reading/writing datagrams
    )
where

import Streamly.Internal.Network.Socket
import Prelude hiding (read)
