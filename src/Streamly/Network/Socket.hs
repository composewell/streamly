-- |
-- Module      : Streamly.Network.Socket
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module provides socket based streaming APIs to to receive connections
-- from remote hosts, and to read and write from and to network sockets.
--
-- For basic socket types and non-streaming operations please consult the @Network.Socket@
-- module of the <http://hackage.haskell.org/package/network network> package.
--
-- = Examples
--
-- To write a server, use the 'accept' stream to start listening for
-- connections from clients.  'accept' generates a stream of connected
-- sockets. We can map an effectful action on this socket stream to handle the
-- connections. The action would typically use socket reading and writing
-- operations to communicate with the remote host. We can read/write a stream
-- of bytes or a stream of chunks of bytes ('Array').
--
-- Following is a short example of a concurrent echo server.  Please note that
-- this example can be written even more succinctly by using higher level
-- operations from "Streamly.Network.Inet.TCP" module.
--
-- >>> :set -XFlexibleContexts
-- >>>
-- >>> import Data.Function ((&))
-- >>> import Network.Socket
-- >>> import Streamly.Network.Socket (SockSpec(..))
-- >>>
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import qualified Streamly.Network.Socket as Socket
-- >>>
-- >>> :{
--  main :: IO ()
--  main = do
--       let spec = SockSpec
--                  { sockFamily = AF_INET
--                  , sockType   = Stream
--                  , sockProto  = defaultProtocol
--                  , sockOpts   = []
--                  }
--           addr = SockAddrInet 8090 (tupleToHostAddress (0,0,0,0))
--        in server spec addr
--       where
--       server spec addr =
--             Socket.accept maxListenQueue spec addr
--           & Stream.parMapM (Stream.eager True) (Socket.forSocketM echo)
--           & Stream.fold Fold.drain
--       echo sk =
--             Socket.readChunks sk -- Stream IO (Array Word8)
--           & Stream.fold (Socket.writeChunks sk) -- IO ()
-- :}
--
-- = Programmer Notes
--
-- Read IO requests to connected stream sockets are performed in chunks of
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.  Unless
-- specified otherwise in the API, writes are collected into chunks of
-- 'Streamly.Internal.System.IO.defaultChunkSize' before they are written to
-- the socket.
--
-- >>> import qualified Streamly.Network.Socket as Socket
--
-- = See Also
--
-- * "Streamly.Internal.Network.Socket"
-- * <http://hackage.haskell.org/package/network network>

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
-- "Streamly.Data.Array.Foreign" read write APIs. They are almost identical to the
-- sequential streaming APIs in "Streamly.Internal.FileSystem.File".
--
module Streamly.Network.Socket
    (
    -- * Socket Specification
      SockSpec(..)

    -- * Accept Connections
    , accept
    , acceptor

    -- * Reads
    -- ** Singleton
    , getChunk

    -- ** Streams
    , read
    , readWith
    , readChunks
    , readChunksWith

    -- ** Unfolds
    , reader
    , readerWith
    , chunkReader
    , chunkReaderWith

    -- * Writes
    -- ** Singleton
    , putChunk

    -- ** Folds
    , write
    , writeWith
    , writeChunks
    , writeChunksWith

    -- * Exceptions
    , forSocketM

    -- * Deprecated
    , readChunk
    , writeChunk
    , readWithBufferOf
    , readChunksWithBufferOf
    , writeWithBufferOf
    , writeChunksWithBufferOf
    )
where

import Data.Word (Word8)
import Network.Socket (Socket)
import Streamly.Data.Array (Array)
import Streamly.Data.MutByteArray (Unbox)

import Streamly.Internal.Network.Socket
import Prelude hiding (read)

{-# DEPRECATED readChunk "Please use 'getChunk' instead" #-}
{-# INLINABLE readChunk #-}
readChunk :: Int -> Socket -> IO (Array Word8)
readChunk = getChunk

{-# DEPRECATED writeChunk "Please use 'putChunk' instead" #-}
{-# INLINABLE writeChunk #-}
writeChunk :: Unbox a => Socket -> Array a -> IO ()
writeChunk = putChunk
