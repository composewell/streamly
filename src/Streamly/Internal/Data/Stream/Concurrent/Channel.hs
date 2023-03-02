-- |
-- Module      : Streamly.Internal.Data.Stream.Concurrent.Channel
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Concurrent.Channel
    (
    -- * Channel
      Channel (..)
    , newChannel
    , withChannel
    , withChannelK
    , fromChannel
    , toChannel
    , toChannelK
    , stopChannel
    -- quiesceChannel -- wait for running tasks but do not schedule any more.

    -- * Configuration
    , Config
    , defaultConfig

    -- ** Limits
    , maxThreads
    , maxBuffer

    -- ** Rate Control
    , Rate(..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- ** Stop behavior
    , StopWhen (..)
    , stopWhen
    , getStopWhen

    -- ** Scheduling behavior
    , eager
    , ordered
    , interleaved

    -- ** Diagnostics
    , inspect
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.StreamD (Stream)
import Streamly.Internal.Data.Stream.Concurrent.Channel.Operations
    (fromChannel, fromChannelK, toChannel, toChannelK)

import qualified Streamly.Internal.Data.Stream.Concurrent.Channel.Append
    as Append
import qualified Streamly.Internal.Data.Stream.Concurrent.Channel.Interleave
    as Interleave
import qualified Streamly.Internal.Data.Stream.StreamK as K

import Streamly.Internal.Data.Stream.Concurrent.Channel.Type
import Streamly.Internal.Data.Stream.Channel.Types

-- | Create a new concurrent stream evaluation channel. The monad
-- state used to run the stream actions is captured from the call site of
-- newChannel.
{-# INLINE newChannel #-}
newChannel :: MonadAsync m =>
    (Config -> Config) -> m (Channel m a)
newChannel modifier =
    let cfg = modifier defaultConfig
     in if getInterleaved cfg
        then Interleave.newChannel modifier
        else Append.newChannel modifier

-- | Allocate a channel and evaluate the stream using the channel and the
-- supplied evaluator function. The evaluator is run in a worker thread.
{-# INLINE withChannelK #-}
withChannelK :: MonadAsync m =>
       (Config -> Config)
    -> K.StreamK m a
    -> (Channel m b -> K.StreamK m a -> K.StreamK m b)
    -> K.StreamK m b
withChannelK modifier input evaluator = K.concatEffect action

    where

    action = do
        chan <- newChannel modifier
        toChannelK chan (evaluator chan input)
        return $ fromChannelK chan

-- | Allocate a channel and evaluate the stream using the channel and the
-- supplied evaluator function. The evaluator is run in a worker thread.
{-# INLINE withChannel #-}
withChannel :: MonadAsync m =>
       (Config -> Config)
    -> Stream m a
    -> (Channel m b -> Stream m a -> Stream m b)
    -> Stream m b
withChannel modifier input evaluator =
    let f chan stream = K.fromStream $ evaluator chan (K.toStream stream)
     in K.toStream $ withChannelK modifier (K.fromStream input) f
