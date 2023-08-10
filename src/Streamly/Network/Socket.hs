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
-- For basic socket types and operations please consult the @Network.Socket@
-- module of the <http://hackage.haskell.org/package/network network> package.
--
-- = Examples
--
-- To write a server, use the 'acceptor' unfold to start listening for
-- connections from clients.  'acceptor' generates a stream of connected
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
-- >>> import qualified Streamly.Data.Stream as Stream
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
--             Stream.unfold Socket.acceptor (maxListenQueue, spec, addr)
--           & Stream.parMapM (Stream.eager True) (Socket.forSocketM echo)
--           & Stream.fold Fold.drain
--       echo sk =
--             Stream.unfold Socket.chunkReader sk -- Stream IO (Array Word8)
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
    , acceptor

    -- * Reads
    -- ** Singleton
    , getChunk

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
    , accept
    , readChunk
    , writeChunk
    , read
    , readWithBufferOf
    , readChunks
    , readChunksWithBufferOf
    , writeWithBufferOf
    , writeChunksWithBufferOf
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Network.Socket (Socket, SockAddr)
import Streamly.Internal.Data.Unfold (Unfold(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Unbox (Unbox)

import Streamly.Internal.Network.Socket hiding (accept, read, readChunks)
import Prelude hiding (read)

{-# DEPRECATED accept "Please use 'acceptor' instead" #-}
{-# INLINE accept #-}
accept :: MonadIO m => Unfold m (Int, SockSpec, SockAddr) Socket
accept = acceptor

{-# DEPRECATED readChunk "Please use 'getChunk' instead" #-}
{-# INLINABLE readChunk #-}
readChunk :: Int -> Socket -> IO (Array Word8)
readChunk = getChunk

{-# DEPRECATED writeChunk "Please use 'putChunk' instead" #-}
{-# INLINABLE writeChunk #-}
writeChunk :: Unbox a => Socket -> Array a -> IO ()
writeChunk = putChunk

{-# DEPRECATED read "Please use 'reader' instead" #-}
{-# INLINE read #-}
read :: MonadIO m => Unfold m Socket Word8
read = reader

{-# DEPRECATED readChunks "Please use 'chunkReader' instead" #-}
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m Socket (Array Word8)
readChunks = chunkReader
