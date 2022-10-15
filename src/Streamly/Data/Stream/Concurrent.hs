-- |
-- Module      : Streamly.Data.Stream.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC

module Streamly.Data.Stream.Concurrent
    (
      MonadAsync

    -- * Configuration
    , Config

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

    -- ** Scheduling behavior
    , eager
    , ordered
    , interleaved

    -- ** Diagnostics
    , inspect

    -- * Combinators
    -- | Stream combinators using a concurrent channel.

    -- ** Evaluate
    -- | Evaluate a stream concurrently using a channel.
    , eval
    , evalWith

    -- ** Map
    -- | Use a single channel to evaluate mapped actions concurrently.
    , mapM
    , mapMWith
    , sequence
    , sequenceWith

    -- ** Combine two
    -- | Use one channel for each pair. When you have to chain more than two
    -- actions concat family of operations are much more efficient because they
    -- use a single channel for all streams.
    --
    -- XXX Do not expose binary operations, instead use these to concatenate n
    -- streams from a list in the given style?
    , append
    , ahead
    , parallel
    , combineWith

    -- ** Apply
    -- | Apply arguments to a function concurrently. Uses a separate channel
    -- for each application.
    , apply
    , applyWith

    -- ** Combine many
    -- | Share a single channel across many streams.
    , concatList
    , concat
    , concatWith
    , concatMap
    , concatMapWith
    )
where

import Streamly.Internal.Data.Stream.Concurrent
import Prelude hiding (mapM, sequence, concat, concatMap)
