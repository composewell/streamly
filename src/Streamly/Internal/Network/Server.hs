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
    -- * TCP Servers
    -- ** Connected sockets
    -- *** Unfolds
      listenOnAddr
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
        SocketType(..), defaultProtocol, maxListenQueue, tupleToHostAddress)
import Prelude hiding (read)

import Streamly (MonadAsync)
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Network.Socket (SockSpec(..), listen, connections)
import Streamly.Streams.Serial (SerialT)

import qualified Streamly.Internal.Data.Unfold as UF

-- | Unfold a tuple @(ipAddr, port)@ into a stream of connected TCP sockets.
-- @ipAddr@ is the local IP address and @port@ is the local port on which
-- connections are accepted.
--
-- @since 0.7.0
{-# INLINE listenOnAddr #-}
listenOnAddr
    :: MonadIO m
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
listenOnAddr = UF.lmap f listen
    where
    f (addr, port) =
        (maxListenQueue
        , SockSpec
            { sockFamily = AF_INET
            , sockType = Stream
            , sockProto = defaultProtocol -- TCP
            , sockOpts = [(NoDelay,1), (ReuseAddr,1)]
            }
        , SockAddrInet port (tupleToHostAddress addr)
        )

-- | Like 'listenOnAddr' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > listenOnPort = UF.supplyFirst listenOnAddr (0,0,0,0)
--
-- @since 0.7.0
{-# INLINE listenOnPort #-}
listenOnPort :: MonadIO m => Unfold m PortNumber Socket
listenOnPort = UF.supplyFirst listenOnAddr (0,0,0,0)

-- | Like 'listenOnAddr' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > listenOnPortLocal = UF.supplyFirst listenOnAddr (127,0,0,1)
--
-- @since 0.7.0
{-# INLINE listenOnPortLocal #-}
listenOnPortLocal :: MonadIO m => Unfold m PortNumber Socket
listenOnPortLocal = UF.supplyFirst listenOnAddr (127,0,0,1)

-------------------------------------------------------------------------------
-- Listen
-------------------------------------------------------------------------------

-- | Like 'connections' but binds on the specified IPv4 address of the machine
-- and listens for TCP connections on the specified port.
--
-- /Internal/
{-# INLINE connectionsOnAddr #-}
connectionsOnAddr
    :: MonadAsync m
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m Socket
connectionsOnAddr addr port =
    connections maxListenQueue SockSpec
        { sockFamily = AF_INET
        , sockType = Stream
        , sockProto = defaultProtocol
        , sockOpts = [(NoDelay,1), (ReuseAddr,1)]
        }
        (SockAddrInet port (tupleToHostAddress addr))

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
