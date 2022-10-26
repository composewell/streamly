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
    -- ** Streams
      acceptOnAddr
    , acceptOnAddrWith
    , acceptOnPort
    -- , acceptOnPortWith
    , acceptOnPortLocal

    -- ** Unfolds
    , acceptorOnAddr
    , acceptorOnAddrWith
    , acceptorOnPort
    , acceptorOnPortWith
    , acceptorOnPortLocal

    -- * TCP clients
    -- | IP Address based operations.
    , connect
    , withConnectionM

    -- ** Unfolds
    , usingConnection
    , reader

    -- ** Streams
    , withConnection
    -- *** Source
    , read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    -- , readByChunks

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf

    -- , readChunksUpto
    -- , readChunksOf
    -- , readChunks

    -- *** Sink
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeWithBufferOf
    , putBytes
    , putBytesWithBufferOf

    -- -- * Array Write
    -- , writeArray
    , writeChunks
    , putChunks

    -- ** Transformation
    , pipeBytes
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

import Control.Exception (onException)
import Control.Monad.Catch (MonadCatch, MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Network.Socket
       (Socket, PortNumber, SocketOption(..), Family(..), SockAddr(..),
        SocketType(..), defaultProtocol, maxListenQueue, tupleToHostAddress,
        socket)
import Prelude hiding (read)

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Control.ForkLifted (fork)
import Streamly.Internal.Data.Array.Unboxed.Type (Array(..), writeNUnsafe)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Network.Socket (SockSpec(..), accept, acceptor)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Control.Monad.Catch as MC
import qualified Network.Socket as Net

import qualified Streamly.Data.Array.Unboxed as A
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold as UF (bracketIO, first)
import qualified Streamly.Internal.Data.Array.Unboxed.Stream as AS
import qualified Streamly.Internal.Data.Fold.Type as FL
    (initialize, snoc, Step(..))
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream.Exception.Lifted as S
import qualified Streamly.Internal.Network.Socket as ISK

-------------------------------------------------------------------------------
-- Accept (unfolds)
-------------------------------------------------------------------------------

{-# INLINE acceptorOnAddrWith #-}
acceptorOnAddrWith
    :: MonadIO m
    => [(SocketOption, Int)]
    -> Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
acceptorOnAddrWith opts = UF.lmap f acceptor
    where
    f (addr, port) =
        (maxListenQueue
        , SockSpec
            { sockFamily = AF_INET
            , sockType = Stream
            , sockProto = defaultProtocol -- TCP
            , sockOpts = opts
            }
        , SockAddrInet port (tupleToHostAddress addr)
        )

-- | Unfold a tuple @(ipAddr, port)@ into a stream of connected TCP sockets.
-- @ipAddr@ is the local IP address and @port@ is the local port on which
-- connections are accepted.
--
{-# INLINE acceptorOnAddr #-}
acceptorOnAddr
    :: MonadIO m
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
acceptorOnAddr = acceptorOnAddrWith []

{-# INLINE acceptorOnPortWith #-}
acceptorOnPortWith :: MonadIO m
    => [(SocketOption, Int)]
    -> Unfold m PortNumber Socket
acceptorOnPortWith opts = UF.first (0,0,0,0) (acceptorOnAddrWith opts)

-- | Like 'acceptorOnAddr' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > acceptorOnPort = UF.first acceptorOnAddr (0,0,0,0)
--
{-# INLINE acceptorOnPort #-}
acceptorOnPort :: MonadIO m => Unfold m PortNumber Socket
acceptorOnPort = UF.first (0,0,0,0) acceptorOnAddr

-- | Like 'acceptorOnAddr' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > acceptorOnPortLocal = UF.first acceptorOnAddr (127,0,0,1)
--
{-# INLINE acceptorOnPortLocal #-}
acceptorOnPortLocal :: MonadIO m => Unfold m PortNumber Socket
acceptorOnPortLocal = UF.first (127,0,0,1) acceptorOnAddr

-------------------------------------------------------------------------------
-- Accept (streams)
-------------------------------------------------------------------------------

{-# INLINE acceptOnAddrWith #-}
acceptOnAddrWith
    :: MonadIO m
    => [(SocketOption, Int)]
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Stream m Socket
acceptOnAddrWith opts addr port =
    accept maxListenQueue SockSpec
        { sockFamily = AF_INET
        , sockType = Stream
        , sockProto = defaultProtocol
        , sockOpts = opts
        }
        (SockAddrInet port (tupleToHostAddress addr))

-- | Like 'accept' but binds on the specified IPv4 address of the machine
-- and listens for TCP connections on the specified port.
--
-- /Pre-release/
{-# INLINE acceptOnAddr #-}
acceptOnAddr
    :: MonadIO m
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Stream m Socket
acceptOnAddr = acceptOnAddrWith []

-- | Like 'accept' but binds on the IPv4 address @0.0.0.0@ i.e.  on all
-- IPv4 addresses/interfaces of the machine and listens for TCP connections on
-- the specified port.
--
-- > acceptOnPort = acceptOnAddr (0,0,0,0)
--
-- /Pre-release/
{-# INLINE acceptOnPort #-}
acceptOnPort :: MonadIO m => PortNumber -> Stream m Socket
acceptOnPort = acceptOnAddr (0,0,0,0)

-- | Like 'accept' but binds on the localhost IPv4 address @127.0.0.1@.
-- The server can only be accessed from the local host, it cannot be accessed
-- from other hosts on the network.
--
-- > acceptOnPortLocal = acceptOnAddr (127,0,0,1)
--
-- /Pre-release/
{-# INLINE acceptOnPortLocal #-}
acceptOnPortLocal :: MonadIO m => PortNumber -> Stream m Socket
acceptOnPortLocal = acceptOnAddr (127,0,0,1)

-------------------------------------------------------------------------------
-- TCP Clients
-------------------------------------------------------------------------------

-- | Connect to the specified IP address and port number. Returns a connected
-- socket or throws an exception.
--
connect :: (Word8, Word8, Word8, Word8) -> PortNumber -> IO Socket
connect addr port = do
    sock <- socket AF_INET Stream defaultProtocol
    Net.connect sock (SockAddrInet port (Net.tupleToHostAddress addr))
        `onException` Net.close sock
    return sock

-- | Connect to a remote host using IP address and port and run the supplied
-- action on the resulting socket.  'withConnectionM' makes sure that the
-- socket is closed on normal termination or in case of an exception.  If
-- closing the socket raises an exception, then this exception will be raised
-- by 'withConnectionM'.
--
-- /Pre-release/
{-# INLINABLE withConnectionM #-}
withConnectionM :: (MonadMask m, MonadIO m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> (Socket -> m ()) -> m ()
withConnectionM addr port =
    bracket (liftIO $ connect addr port) (liftIO . Net.close)

-------------------------------------------------------------------------------
-- Connect (unfolds)
-------------------------------------------------------------------------------

-- | Transform an 'Unfold' from a 'Socket' to an unfold from a remote IP
-- address and port. The resulting unfold opens a socket, uses it using the
-- supplied unfold and then makes sure that the socket is closed on normal
-- termination or in case of an exception.  If closing the socket raises an
-- exception, then this exception will be raised by 'usingConnection'.
--
-- /Pre-release/
{-# INLINE usingConnection #-}
usingConnection :: (MonadCatch m, MonadAsync m)
    => Unfold m Socket a
    -> Unfold m ((Word8, Word8, Word8, Word8), PortNumber) a
usingConnection = UF.bracketIO (\(addr, port) -> connect addr port) Net.close

-------------------------------------------------------------------------------
-- Connect (streams)
-------------------------------------------------------------------------------

-- | @'withConnection' addr port act@ opens a connection to the specified IPv4
-- host address and port and passes the resulting socket handle to the
-- computation @act@.  The handle will be closed on exit from 'withConnection',
-- whether by normal termination or by raising an exception.  If closing the
-- handle raises an exception, then this exception will be raised by
-- 'withConnection' rather than any exception raised by 'act'.
--
-- /Pre-release/
{-# INLINE withConnection #-}
withConnection :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> (Socket -> Stream m a)
    -> Stream m a
withConnection addr port = S.bracketIO (connect addr port) Net.close

-------------------------------------------------------------------------------
-- Read Addr to Stream
-------------------------------------------------------------------------------

-- | Read a stream from the supplied IPv4 host address and port number.
--
{-# INLINE reader #-}
reader :: (MonadCatch m, MonadAsync m)
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Word8
reader = UF.many A.reader (usingConnection ISK.chunkReader)

-- | Read a stream from the supplied IPv4 host address and port number.
--
-- /Pre-release/
{-# INLINE read #-}
read :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> Stream m Word8
read addr port = AS.concat $ withConnection addr port ISK.readChunks

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to the supplied IPv4 host address and port
-- number.
--
-- /Pre-release/
{-# INLINE putChunks #-}
putChunks
    :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Stream m (Array Word8)
    -> m ()
putChunks addr port xs =
    S.fold FL.drain
        $ withConnection addr port (\sk -> S.fromEffect $ ISK.putChunks sk xs)

-- | Write a stream of arrays to the supplied IPv4 host address and port
-- number.
--
{-# INLINE writeChunks #-}
writeChunks
    :: (MonadIO m, MonadCatch m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Fold m (Array Word8) ()
writeChunks addr port = Fold step initial extract
    where
    initial = do
        skt <- liftIO (connect addr port)
        fld <- FL.initialize (ISK.writeChunks skt)
                    `MC.onException` liftIO (Net.close skt)
        return $ FL.Partial (Tuple' fld skt)
    step (Tuple' fld skt) x = do
        r <- FL.snoc fld x `MC.onException` liftIO (Net.close skt)
        return $ FL.Partial (Tuple' r skt)
    extract (Tuple' (Fold _ initial1 extract1) skt) = do
        liftIO $ Net.close skt
        res <- initial1
        case res of
            FL.Partial fs -> extract1 fs
            FL.Done fb -> return fb

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- /Pre-release/
{-# INLINE putBytesWithBufferOf #-}
putBytesWithBufferOf
    :: (MonadCatch m, MonadAsync m)
    => Int
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Stream m Word8
    -> m ()
putBytesWithBufferOf n addr port m = putChunks addr port $ AS.arraysOf n m

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf
    :: (MonadIO m, MonadCatch m)
    => Int
    -> (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Fold m Word8 ()
writeWithBufferOf n addr port =
    FL.chunksOf n (writeNUnsafe n) (writeChunks addr port)

-- | Write a stream to the supplied IPv4 host address and port number.
--
-- /Pre-release/
{-# INLINE putBytes #-}
putBytes :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> Stream m Word8 -> m ()
putBytes = putBytesWithBufferOf defaultChunkSize

-- | Write a stream to the supplied IPv4 host address and port number.
--
{-# INLINE write #-}
write :: (MonadIO m, MonadCatch m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> Fold m Word8 ()
write = writeWithBufferOf defaultChunkSize

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

{-# INLINE withInputConnect #-}
withInputConnect
    :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Stream m Word8
    -> (Socket -> Stream m a)
    -> Stream m a
withInputConnect addr port input f = S.bracket pre post handler

    where

    pre = do
        sk <- liftIO $ connect addr port
        tid <- fork (ISK.putBytes sk input)
        return (sk, tid)

    handler (sk, _) = f sk

    -- XXX kill the thread immediately?
    post (sk, _) = liftIO $ Net.close sk

-- | Send an input stream to a remote host and produce the output stream from
-- the host. The server host just acts as a transformation function on the
-- input stream.  Both sending and receiving happen asynchronously.
--
-- /Pre-release/
--
{-# INLINE pipeBytes #-}
pipeBytes
    :: (MonadAsync m, MonadCatch m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> Stream m Word8
    -> Stream m Word8
pipeBytes addr port input = withInputConnect addr port input ISK.read
