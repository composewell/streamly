-- |
-- Module      : Streamly.Network.Server
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
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
    , listen
    , listenOnAddr
    , listenOnPort
    , listenOnPortLocal

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

import Streamly.Internal.Network.Server
