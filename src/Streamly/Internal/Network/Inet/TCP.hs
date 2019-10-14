{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Network.Inet.TCP
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to build Inet/TCP clients and servers.

module Streamly.Internal.Network.Inet.TCP
    (
    -- * TCP Servers
    -- ** Unfolds
      acceptOnAddr
    , acceptOnPort
    , acceptOnPortLocal

    -- ** Streams
    , connections
    , connectionsOnAddr
    , connectionsOnAllAddrs
    , connectionsOnLocalHost

    -- * TCP clients
    -- | Socket based reads and writes.
    , withConnection

    -- ** Source
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
    -- , readArrays

    -- ** Sink
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    -- , writeInChunksOf

    -- -- * Array Write
    -- , writeArray
    , writeArrays
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

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Network.Socket
       (Socket, PortNumber, SocketOption(..), Family(..), SockAddr(..),
        SocketType(..), defaultProtocol, maxListenQueue, tupleToHostAddress,
        socket, connect)
import Prelude hiding (read)

import Streamly (MonadAsync)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Network.Socket (SockSpec(..), listen, connections)
import Streamly.Streams.Serial (SerialT)
import Streamly.Internal.Memory.Array.Types (Array(..), defaultChunkSize, writeNUnsafe)
import Streamly.Streams.StreamK.Type (IsStream)

import qualified Network.Socket as Net
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Prelude as S
import qualified Streamly.Network.Socket as SK
import qualified Streamly.Internal.Network.Socket as ISK

-------------------------------------------------------------------------------
-- TCP Servers
-------------------------------------------------------------------------------

-- | Unfold a tuple @(ipAddr, port)@ into a stream of connected TCP sockets.
-- @ipAddr@ is the local IP address and @port@ is the local port on which
-- connections are accepted.
--
-- @since 0.7.0
{-# INLINE acceptOnAddr #-}
acceptOnAddr
    :: MonadIO m
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
acceptOnAddr = UF.lmap f listen
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

-- | Like 'acceptOnAddr' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > acceptOnPort = UF.supplyFirst acceptOnAddr (0,0,0,0)
--
-- @since 0.7.0
{-# INLINE acceptOnPort #-}
acceptOnPort :: MonadIO m => Unfold m PortNumber Socket
acceptOnPort = UF.supplyFirst acceptOnAddr (0,0,0,0)

-- | Like 'acceptOnAddr' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > acceptOnPortLocal = UF.supplyFirst acceptOnAddr (127,0,0,1)
--
-- @since 0.7.0
{-# INLINE acceptOnPortLocal #-}
acceptOnPortLocal :: MonadIO m => Unfold m PortNumber Socket
acceptOnPortLocal = UF.supplyFirst acceptOnAddr (127,0,0,1)

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

-------------------------------------------------------------------------------
-- TCP Clients
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Connect
-------------------------------------------------------------------------------

openConnection :: (Word8, Word8, Word8, Word8) -> PortNumber -> IO Socket
openConnection addr port = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock $ SockAddrInet port (Net.tupleToHostAddress addr)
    return sock

-- | @'withConnection' addr port act@ opens a connection to the specified IPv4
-- host address and port and passes the resulting socket handle to the
-- computation @act@.  The handle will be closed on exit from 'withConnection',
-- whether by normal termination or by raising an exception.  If closing the
-- handle raises an exception, then this exception will be raised by
-- 'withConnection' rather than any exception raised by 'act'.
--
-- @since 0.7.0
{-# INLINABLE withConnection #-}
withConnection :: (IsStream t, MonadCatch m, MonadIO m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> (Socket -> t m a) -> t m a
withConnection addr port =
    S.bracket (liftIO $ openConnection addr port) (liftIO . Net.close)

-------------------------------------------------------------------------------
-- Read Addr to Stream
-------------------------------------------------------------------------------

-- | Read a stream from the supplied IPv4 host address and port number.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: (IsStream t, MonadCatch m, MonadIO m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> t m Word8
read addr port = AS.concat $ withConnection addr port ISK.toStreamArrays

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

{-
-- | Write a stream of arrays to the supplied IPv4 host address and port
-- number.
--
-- @since 0.7.0
{-# INLINE writeArrays #-}
writeArrays
    :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m (Array Word8)
    -> m ()
writeArrays addr port xs =
    S.drain $ withConnection addr port (\sk -> S.yieldM $ SK.writeArrays sk xs)
-}

-- | Write a stream of arrays to the supplied IPv4 host address and port
-- number.
--
-- @since 0.7.0
{-# INLINE writeArrays #-}
writeArrays
    :: (MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Fold m (Array Word8) ()
writeArrays addr port = Fold step initial extract
    where
    initial = do
        skt <- liftIO (openConnection addr port)
        FL.initialize (SK.writeArrays skt)
    step = FL.runStep
    extract (Fold _ initial1 extract1) = initial1 >>= extract1

{-
-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE writeInChunksOf #-}
writeInChunksOf
    :: (MonadCatch m, MonadAsync m)
    => Int
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m Word8
    -> m ()
writeInChunksOf n addr port m = writeArrays addr port $ AS.arraysOf n m
-}

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.7.0
{-# INLINE writeInChunksOf #-}
writeInChunksOf
    :: (MonadAsync m)
    => Int
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Fold m Word8 ()
writeInChunksOf n addr port =
    FL.lchunksOf n (writeNUnsafe n) (writeArrays addr port)

{-
-- | Write a stream to the supplied IPv4 host address and port number.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> SerialT m Word8 -> m ()
write = writeInChunksOf defaultChunkSize
-}

-- | Write a stream to the supplied IPv4 host address and port number.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: MonadAsync m
    => (Word8, Word8, Word8, Word8) -> PortNumber -> Fold m Word8 ()
write = writeInChunksOf defaultChunkSize
