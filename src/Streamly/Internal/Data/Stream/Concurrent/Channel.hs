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
    , eagerEval
    , ordered
    , interleaved

    -- ** Diagnostics
    , inspectMode
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream (Stream, fromStreamK, toStreamK)
import Streamly.Internal.Data.Stream.Concurrent.Channel.Operations
    (fromChannel, fromChannelK, toChannel, toChannelK)

import qualified Streamly.Internal.Data.Stream.Concurrent.Channel.Append
    as Append
import qualified Streamly.Internal.Data.Stream.Concurrent.Channel.Interleave
    as Interleave
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

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
       (Channel m b -> K.Stream m a -> K.Stream m b)
    -> (Config -> Config)
    -> K.Stream m a
    -> K.Stream m b
withChannelK evaluator modifier input = K.concatEffect action

    where

    action = do
        chan <- newChannel modifier
        toChannelK chan (evaluator chan input)
        return $ fromChannelK chan

-- | Allocate a channel and evaluate the stream using the channel and the
-- supplied evaluator function. The evaluator is run in a worker thread.
{-# INLINE withChannel #-}
withChannel :: MonadAsync m =>
       (Channel m b -> Stream m a -> Stream m b)
    -> (Config -> Config)
    -> Stream m a
    -> Stream m b
withChannel evaluator modifier input =
    let f chan stream = toStreamK $ evaluator chan (fromStreamK stream)
     in fromStreamK $ withChannelK f modifier (toStreamK input)
