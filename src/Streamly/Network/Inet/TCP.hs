-- |
-- Module      : Streamly.Network.Server
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Combinators to build Inet\/IPv4/TCP clients and servers.
--
-- >>> import qualified Streamly.Network.Inet.TCP as TCP
--
-- = Examples
--
-- Following is a short example of a concurrent echo server.
--
-- >>> import Control.Monad.Catch (finally)
-- >>> import Data.Function ((&))
-- >>> import Network.Socket (Socket)
-- >>>
-- >>> import qualified Network.Socket as Net
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import qualified Streamly.Network.Inet.TCP as TCP
-- >>> import qualified Streamly.Network.Socket as Socket
-- >>>
-- >>> :{
-- main :: IO ()
-- main =
--       TCP.accept 8091                            -- Stream IO Socket
--     & Stream.parMapM id (handleExceptions echo)  -- Stream IO ()
--     & Stream.fold Fold.drain                     -- IO ()
--     where
--     echo :: Socket -> IO ()
--     echo sk =
--           Socket.readChunksWith 32768 sk      -- Stream IO (Array Word8)
--         & Stream.fold (Socket.writeChunks sk) -- IO ()
--     handleExceptions :: (Socket -> IO ()) -> Socket -> IO ()
--     handleExceptions f sk = finally (f sk) (Net.close sk)
-- :}

module Streamly.Network.Inet.TCP
    (
    -- * Accept Connections
    -- ** Streams
      accept
    , acceptLocal
    , acceptOnAddr
    , acceptOnAddrWith

    -- ** Unfolds
    , acceptor
    , acceptorLocal
    , acceptorOnAddr

    -- * Connect to Servers
    , connect

    {-
    -- XXX Expose this as a pipe when we have pipes.
    -- * Transformation
    -- , pipeBytes

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

    -- * Deprecated
    , acceptorOnPort
    , acceptorOnPortLocal
    )
where

import Streamly.Internal.Network.Inet.TCP
