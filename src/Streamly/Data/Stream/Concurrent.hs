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
    -- | Evaluates a stream concurrently using a channel.
    , eval
    , evalWith

    -- ** Generate
    -- | Uses a single channel to evaluate mapped actions concurrently.
    , repeatM
    , replicateM

    -- ** Map
    -- | Uses a single channel to evaluate mapped actions concurrently.
    , mapM
    , mapMWith
    , sequence
    , sequenceWith

    -- ** List of streams
    -- | Shares a single channel across many streams.
    , append
    , ahead
    , parallel
    , concatListWith
    , parallelFst
    , parallelMin
    , zipWithM
    , zipWith

    -- ** Stream of streams
    -- *** Apply
    -- | Apply argument streams to a function concurrently. Uses a separate
    -- channel for each application.
    , apply
    , applyWith

    -- *** Concat
    -- | Shares a single channel across many streams.
    , concat
    , concatWith
    , concatMap
    , concatMapWith
    )
where

import Streamly.Internal.Data.Stream.Concurrent
import Prelude hiding (mapM, sequence, concat, concatMap, zipWith)
