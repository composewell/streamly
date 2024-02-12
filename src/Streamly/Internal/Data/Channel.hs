-- |
-- Module      : Streamly.Internal.Data.Channel
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains operations that are common for Stream and Fold
-- channels.

module Streamly.Internal.Data.Channel
    (
    -- * Channel Config & Stats
      module Streamly.Internal.Data.Channel.Types
    -- * Worker Dispatcher
    -- | Operations used by the consumer of the channel.
    , module Streamly.Internal.Data.Channel.Dispatcher
    -- * Channel Workers
    -- | Operations used by the workers (producers) of the channel. These
    -- operations are thread-safe, these can be called concurrently by workers
    -- working in independent Haskell threads, the shared channel data
    -- structures are read or updated atomically.
    , module Streamly.Internal.Data.Channel.Worker
    )
where

import Streamly.Internal.Data.Channel.Dispatcher
import Streamly.Internal.Data.Channel.Types
import Streamly.Internal.Data.Channel.Worker
