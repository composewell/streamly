-- |
-- Module      : Streamly.Network.Server
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Combinators to build Inet/TCP clients and servers.
--
-- > import qualified Streamly.Network.Inet.TCP as TCP
--

module Streamly.Network.Inet.TCP
    (
    -- * Accept Connections
    -- ** Unfolds
      acceptorOnAddr
    , acceptorOnPort
    , acceptorOnPortLocal

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
    , acceptOnAddr
    , acceptOnPort
    , acceptOnPortLocal
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Network.Socket (Socket, PortNumber)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import Streamly.Internal.Network.Inet.TCP
    hiding (acceptOnAddr, acceptOnPort, acceptOnPortLocal)

{-# DEPRECATED acceptOnAddr "Please use 'acceptorOnAddr' instead" #-}
{-# INLINE acceptOnAddr #-}
acceptOnAddr
    :: MonadIO m
    => Unfold m ((Word8, Word8, Word8, Word8), PortNumber) Socket
acceptOnAddr = acceptorOnAddr

{-# DEPRECATED acceptOnPort "Please use 'acceptorOnPort' instead" #-}
{-# INLINE acceptOnPort #-}
acceptOnPort :: MonadIO m => Unfold m PortNumber Socket
acceptOnPort = acceptorOnPort

{-# DEPRECATED acceptOnPortLocal "Please use 'acceptorOnPortLocal' instead" #-}
{-# INLINE acceptOnPortLocal #-}
acceptOnPortLocal :: MonadIO m => Unfold m PortNumber Socket
acceptOnPortLocal = acceptorOnPortLocal
