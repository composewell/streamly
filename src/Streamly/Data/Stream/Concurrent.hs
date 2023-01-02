-- |
-- Module      : Streamly.Data.Stream.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module provides concurrent streaming abstractions.
--
-- == Concurrency Channels
--
-- At a lower level, concurrency is implemented using channels that support
-- concurrent evaluation of streams. We create a channel, and add one or more
-- streams to it. The channel evaluates multiple streams concurrently and then
-- generates a single output stream from the results.
--
-- == Concurrency Primitives
--
-- There are only two fundamental abstractions for concurrency, all other
-- combinators in this module can be expressed in terms of those two.
--
-- The first one is 'evalWith', it evaluates a single stream asynchronously, a
-- worker thread runs the stream and buffers the results, and the consumer of
-- the stream runs in another thread consuming it from the buffer, thus
-- decoupling the production and consumption of the stream. This can be used to
-- run different stages of a pipeline concurrently.
--
-- The second one is 'concatMapWith', unlike 'evalWith' this is used to
-- evaluate multiple streams concurrently and combine the results. A stream
-- generator function is mapped to the input stream and all the generated
-- streams are then evaluated concurrently, and the results are combined.
--
-- == Concurrency Configuration
--
-- Combinators ending with the "With" suffix ('concatMapWith') take a 'Config'
-- argument which controls the concurrent behavior. For example, maximum number
-- of threads to be used ('maxThreads') or the maxmimum size of the buffer
-- ('maxBuffer'), or how the streams are scheduled with respect to each other
-- ('interleaved'), or how the results are consumed ('ordered').
--
-- Configuration is specified as @Config -> Config@ modifier functions that can
-- be composed together using function composition. For example, to specify the
-- maximum threads we can use @concatMapWith (maxThreads 10)@ if we also want
-- to specify the maximum buffer we can compose the two options
-- @concatMapwith (maxThreads 10 . maxBuffer 100)@. To use default configuration
-- use 'id' as the config modifier e.g. @concatMapWith id@.
--
-- See the "Configuration" section and individual configuration options'
-- documentation for the default behavior and values.
--
-- == Scheduling behavior
--
-- The most important option is to control whether the output of the concurrent
-- execution is consumed in the same order as the corresponding actions in the
-- input stream or as soon as they arrive. The default is the latter, however,
-- we can enforce the original order by using the 'ordered' option.
--
-- Another important option controls whether the number of worker threads are
-- automatically increased or decreased based on the consumption rate or
-- threads are started as aggresively as possible until the maxThreads or
-- maxBuffer limits are hit. The default is the former. However, the 'eager'
-- option can be enabled to use the latter behavior. When 'eager' is on,
-- consumer thread does not make any impact on scheduling of the available
-- tasks.
--
-- == Concurrent Combinators
--
-- Using the two fundamental concurrency primitives we can implement all the
-- usual streaming combinators with concurrent behavior. Combinators like
-- 'unfoldrM', 'iterateM' that are inherently serial can be evaluated
-- concurrently with respect to the consumer pipeline using 'eval'. Combinators
-- like 'zipWithM', 'mergeByM' can also use 'evalWith' on the input streams to
-- make them concurrent.
--
-- Combinators like 'repeatM', 'replicateM', 'fromListM', 'sequence', 'mapM' in
-- which all actions are independent of each other can be made concurrent using
-- the concurrent 'concatMapWith' operation.
--
-- A concurrent 'repeatM' would repeat an action using multiple concurrent
-- executions of the action. Similarly, a concurrent 'mapM' would perform the
-- mapped action in independent threads.
--
-- == Programming Tips
--
-- The names in this module do not conflict with other stream modules,
-- therefore, you can import it in the same namespace:
--
-- >>> import qualified Streamly.Data.Stream.Concurrent as Stream

module Streamly.Data.Stream.Concurrent
    (
    -- * Concurrency
      MonadAsync

    -- ** Configuration
    , Config

    -- *** Limits
    , maxThreads
    , maxBuffer

    -- *** Rate Control
    , Rate(..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- *** Stop behavior
    , StopWhen (..)
    , stopWhen

    -- *** Scheduling behavior
    , eager
    , ordered
    , interleaved

    -- *** Diagnostics
    , inspect

    -- ** Combinators
    -- | Stream combinators using a concurrent channel.

    -- *** Evaluate
    -- | Evaluates a stream concurrently using a channel.

    -- , eval
    , parEval

    -- *** Generate
    -- | Uses a single channel to evaluate mapped actions concurrently.
    , parRepeatM
    , parReplicateM

    -- *** Map
    -- | Uses a single channel to evaluate mapped actions concurrently.

    -- , mapM
    , parMapM
    -- , sequence
    , parSequence

    -- *** List of streams
    -- | Shares a single channel across many streams.

    -- , append
    -- , ahead
    -- , parallel
    , parConcatList
    -- , parallelFst
    -- , parallelMin
    -- , zipWithM
    -- , zipWith

    -- *** Stream of streams
    -- **** Apply

    -- , apply
    , parApply

    -- **** Concat
    -- | Shares a single channel across many streams.

    -- , concat
    , parConcat
    -- , concatMap
    , parConcatMap

    -- **** ConcatIterate
    , parConcatIterate
    )
where

import Streamly.Internal.Data.Stream.Concurrent
import Prelude hiding (mapM, sequence, concat, concatMap, zipWith)
