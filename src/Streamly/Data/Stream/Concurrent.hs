-- |
-- Module      : Streamly.Data.Stream.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Suggested idioms:
--
-- >>> joinAsync = joinUsing id
--
-- Note: this module is designed to not conflict with Data.Stream so that both
-- can be imported as Stream.

module Streamly.Data.Stream.Concurrent
    (
      MonadAsync

    -- * Configuration
    -- | Define the concurrency strategy.
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
    , evalUsing

    -- ** Map
    -- | Uses a single channel to evaluate mapped actions concurrently.
    , mapMUsing
    , sequenceUsing

    -- ** List of streams
    -- | Shares a single channel across many streams.
    , joinUsing

    -- ** Stream of streams
    -- *** Apply
    -- | Apply argument streams to a function concurrently. Uses a separate
    -- channel for each application.
    , applyUsing

    -- *** Concat
    -- | Shares a single channel across many streams.
    , concatUsing
    , concatMapUsing
    )
where

import Streamly.Internal.Data.Stream.Concurrent
import Prelude hiding (mapM, sequence, concat, concatMap, zipWith)
