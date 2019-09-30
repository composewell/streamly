{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Network.Server
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to build network servers.
--
-- > import qualified Streamly.Internal.Network.Server as Server
--

module Streamly.Internal.Network.Server
    (
    -- * Server Configuration
       Server(..)

    -- * TCP Servers
    -- ** Connected sockets
    -- *** Unfolds
    , listen
    , listenOnAddr
    , listenOnPort
    , listenOnPortLocal

    -- *** Streams
    , connections
    , connectionsOnAddr
    , connectionsOnAllAddrs
    , connectionsOnLocalHost

    {-
    -- ** Sink Servers

    -- These abstractions can be applied to any setting where we need to do a
    -- sink processing of multiple streams e.g. output from multiple processes
    -- or data coming from multiple files.

    -- handle connections concurrently using a specified fold
    -- , handleConnections

    -- handle frames concurrently using a specified fold
    , handleFrames

    -- merge frames from all connection into a single stream. Frames can be
    -- created by a specified fold.
    , mergeFrames

    -- * UDP Servers
    , datagrams
    , datagramsOn
    -}
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Network.Socket
       (Socket, PortNumber, SocketOption(..), Family(..), SockAddr(..),
        withSocketsDo, SocketType(..), socket, accept, bind,
        defaultProtocol, setSocketOption, maxListenQueue, tupleToHostAddress)
import Prelude hiding (read)

import qualified Network.Socket as Net

import Streamly (MonadAsync)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Streams.Serial (SerialT)

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Prelude as S
import qualified Streamly.Streams.StreamD.Type as D

-------------------------------------------------------------------------------
-- Listen
-------------------------------------------------------------------------------

-- | Specify the configuration of a server.
data Server = Server
    {
      serverAddressFamily :: !Family
    , serverAddress       :: !SockAddr
    , serverSockOpts      :: ![(SocketOption, Int)]
    }

initListener :: Int -> Server -> IO Socket
initListener tcpListenQ Server{..} =
  withSocketsDo $ do
    sock <- socket serverAddressFamily Stream defaultProtocol
    mapM_ (\(opt, val) -> setSocketOption sock opt val) serverSockOpts
    bind sock serverAddress
    Net.listen sock tcpListenQ
    return sock

{-# INLINE listenTuples #-}
listenTuples :: MonadIO m => Unfold m (Int, Server) (Socket, SockAddr)
listenTuples = Unfold step inject
    where
    inject (tcpListenQ, opts) = do
        listener <- liftIO $ initListener tcpListenQ opts
        return listener

    step listener = do
        r <- liftIO $ accept listener
        -- XXX error handling
        return $ D.Yield r listener

-- | Unfold a tuple @(listenQueue, address)@ into a stream of connected
-- TCP sockets. @listenQueue@ is the maximum number of pending connections in
-- the backlog.  @address@ is the server address specification (address
-- family, local interface IP address and port).
--
-- @since 0.7.0
{-# INLINE listen #-}
listen :: MonadIO m => Unfold m (Int, Server) Socket
listen = UF.map fst listenTuples

{-# INLINE listenTuplesOnAddr #-}
listenTuplesOnAddr
    :: MonadIO m
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) (Socket, SockAddr)
listenTuplesOnAddr = UF.lmap f listenTuples
    where
    f (addr, port) = (maxListenQueue, Server
        { serverAddressFamily = AF_INET
        , serverAddress = SockAddrInet port (tupleToHostAddress addr)
        , serverSockOpts = [(NoDelay,1), (ReuseAddr,1)]
        })

-- | Unfold a tuple @(ipAddr, port)@ into a stream of connected TCP sockets.
-- @ipAddr@ is the local IP address and @port@ is the local port on which
-- connections are accepted.
--
-- @since 0.7.0
{-# INLINE listenOnAddr #-}
listenOnAddr
    :: MonadIO m
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
listenOnAddr = UF.map fst listenTuplesOnAddr

-- | Like 'listenOnAddr' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > listenOnPort = UF.first listenOnAddr (0,0,0,0)
--
-- @since 0.7.0
{-# INLINE listenOnPort #-}
listenOnPort :: MonadIO m => Unfold m PortNumber Socket
listenOnPort = UF.first listenOnAddr (0,0,0,0)

-- | Like 'listenOnAddr' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > listenOnPortLocal = UF.first listenOnAddr (127,0,0,1)
--
-- @since 0.7.0
{-# INLINE listenOnPortLocal #-}
listenOnPortLocal :: MonadIO m => Unfold m PortNumber Socket
listenOnPortLocal = UF.first listenOnAddr (127,0,0,1)

-------------------------------------------------------------------------------
-- Listen
-------------------------------------------------------------------------------

{-# INLINE recvConnectionTuplesWith #-}
recvConnectionTuplesWith :: MonadAsync m
    => Int -> Server -> SerialT m (Socket, SockAddr)
recvConnectionTuplesWith tcpListenQ opts = S.unfoldrM step Nothing
    where
    step Nothing = do
        listener <- liftIO $ initListener tcpListenQ opts
        r <- liftIO $ accept listener
        -- XXX error handling
        return $ Just (r, Just listener)

    step (Just listener) = do
        r <- liftIO $ accept listener
        -- XXX error handling
        return $ Just (r, Just listener)

-- | Start a TCP stream server that listens for connections on the supplied
-- server address specification (address family, local interface IP address and
-- port). The server generates a stream of connected sockets.  The first
-- argument is the maximum number of pending connections in the backlog.
--
-- /Internal/
{-# INLINE connections #-}
connections :: MonadAsync m => Int -> Server -> SerialT m Socket
connections tcpListenQ opts = fmap fst $
    recvConnectionTuplesWith tcpListenQ opts

{-# INLINE connectionTuplesOnAddr #-}
connectionTuplesOnAddr
    :: MonadAsync m
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m (Socket, SockAddr)
connectionTuplesOnAddr addr port =
    recvConnectionTuplesWith maxListenQueue Server
        { serverAddressFamily = AF_INET
        , serverAddress = SockAddrInet port (tupleToHostAddress addr)
        , serverSockOpts = [(NoDelay,1), (ReuseAddr,1)]
        }

-- | Like 'connections' but binds on the specified IPv4 address of the machine
-- and listens for TCP connections on the specified port.
--
-- /Internal/
{-# INLINE connectionsOnAddr #-}
connectionsOnAddr
    :: MonadAsync m
    => (Word8, Word8, Word8, Word8) -> PortNumber -> SerialT m Socket
connectionsOnAddr addr port = fmap fst $ connectionTuplesOnAddr addr port

-- | Like 'connections' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > connectionsOnAllAddrs = connectionsOnAddr (0,0,0,0)
--
-- /Internal/
{-# INLINE connectionsOnAllAddrs #-}
connectionsOnAllAddrs :: MonadAsync m => PortNumber -> SerialT m Socket
connectionsOnAllAddrs = connectionsOnAddr (0,0,0,0)

-- | Like 'connections' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > connectionsOnLocalHost = connectionsOnAddr (127,0,0,1)
--
-- /Internal/
{-# INLINE connectionsOnLocalHost #-}
connectionsOnLocalHost :: MonadAsync m => PortNumber -> SerialT m Socket
connectionsOnLocalHost = connectionsOnAddr (127,0,0,1)
