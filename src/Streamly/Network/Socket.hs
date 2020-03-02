-- |
-- Module      : Streamly.Network.Socket
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides operations to connect to remote hosts, to receive
-- connections from remote hosts, and to read and write streams and arrays of
-- bytes to and from network sockets.
--
-- This module relies on the @network@ package for basic networking facilities.
-- You may need some types and operations from that package.
--
-- = How to Use
--
-- Use the 'accept' unfold to start listening for connections from clients.
-- 'accept' supplies a stream of connected sockets. Map an effectful action on
-- this socket stream to handle the connections. The action would typically use
-- socket reading and writing operations from this module to communicate with
-- the socket. The socket operations can read/write a stream of bytes or a
-- stream of chunks of bytes ('Array'). Following is a short example of a
-- concurrent echo server.  Please note that this example can be written more
-- succinctly by using higher level operations from
-- "Streamly.Internal.Network.Inet.TCP" module.
--
-- @
-- {-\# LANGUAGE FlexibleContexts #-}
--
-- import Data.Function ((&))
-- import Network.Socket
-- import Streamly.Internal.Network.Socket (handleWithM)
-- import Streamly.Network.Socket (SockSpec(..))
--
-- import Streamly
-- import qualified Streamly.Prelude as S
-- import qualified Streamly.Network.Socket as SK
--
-- main = do
--     let spec = SockSpec
--                { sockFamily = AF_INET
--                , sockType   = Stream
--                , sockProto  = defaultProtocol
--                , sockOpts   = []
--                }
--         addr = SockAddrInet 8090 (tupleToHostAddress (0,0,0,0))
--      in server spec addr
--
--     where
--
--     server spec addr =
--           S.unfold SK.accept (maxListenQueue, spec, addr) -- SerialT IO Socket
--         & parallely . S.mapM (handleWithM echo)           -- SerialT IO ()
--         & S.drain                                         -- IO ()
--
--     echo sk =
--           S.unfold SK.readChunks sk  -- SerialT IO (Array Word8)
--         & S.fold (SK.writeChunks sk) -- IO ()
-- @
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

-------------------------------------------------------------------------------
-- Internal Notes
-------------------------------------------------------------------------------
--
-- A socket is a handle to a protocol endpoint.
--
-- This module provides APIs to read and write streams and arrays from and to
-- network sockets. Sockets may be connected or unconnected. Connected sockets
-- can only send or recv data to/from the connected endpoint, therefore, APIs
-- for connected sockets do not need to explicitly specify the remote endpoint.
-- APIs for unconnected sockets need to explicitly specify the remote endpoint.
--
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
