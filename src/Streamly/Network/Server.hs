{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Network.Server
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to build network servers.
--
-- > import qualified Streamly.Network.Server as Server
--

module Streamly.Network.Server
    (
    -- * Server Configuration
       Server(..)

    -- * TCP Servers
    -- ** Connected sockets
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
import Streamly.Streams.Serial (SerialT)

import qualified Streamly.Prelude as S

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
-- @since 0.7.0
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
-- @since 0.7.0
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
-- @since 0.7.0
{-# INLINE connectionsOnAllAddrs #-}
connectionsOnAllAddrs :: MonadAsync m => PortNumber -> SerialT m Socket
connectionsOnAllAddrs = connectionsOnAddr (0,0,0,0)

-- | Like 'connections' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > connectionsOnLocalHost = connectionsOnAddr (127,0,0,1)
--
-- @since 0.7.0
{-# INLINE connectionsOnLocalHost #-}
connectionsOnLocalHost :: MonadAsync m => PortNumber -> SerialT m Socket
connectionsOnLocalHost = connectionsOnAddr (127,0,0,1)
