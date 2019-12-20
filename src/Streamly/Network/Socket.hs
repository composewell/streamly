-- |
-- Module      : Streamly.Network.Socket
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A socket is a handle to a protocol endpoint.
--
-- This module provides APIs to read and write streams and arrays from and to
-- network sockets. Sockets may be connected or unconnected. Connected sockets
-- can only send or recv data to/from the connected endpoint, therefore, APIs
-- for connected sockets do not need to explicitly specify the remote endpoint.
-- APIs for unconnected sockets need to explicitly specify the remote endpoint.
--
-- = Programmer Notes
--
-- Read IO requests to connected stream sockets are performed in chunks of
-- 'Streamly.Internal.Memory.Array.Types.defaultChunkSize'.  Unless specified
-- otherwise in the API, writes are collected into chunks of
-- 'Streamly.Internal.Memory.Array.Types.defaultChunkSize' before they are
-- written to the socket. APIs are provided to control the chunking behavior.
--
-- > import qualified Streamly.Network.Socket as SK
--
-- For additional, experimental APIs take a look at
-- "Streamly.Internal.Network.Socket" module.

-- By design, connected socket IO APIs are similar to
-- "Streamly.Memory.Array" read write APIs. They are almost identical to the
-- sequential streaming APIs in "Streamly.Internal.FileSystem.File".
--
module Streamly.Network.Socket
    (
    -- * Socket Specification
      SockSpec(..)

    -- * Accept Connections
    , accept

    -- * Read
    , read
    , readWithBufferOf
    , readChunks
    , readChunksWithBufferOf

    -- * Write
    , write
    , writeWithBufferOf
    , writeChunks
    )
where

import Streamly.Internal.Network.Socket
import Prelude hiding (read)
