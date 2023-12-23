-- |
-- Module      : Streamly.Data.Stream.Prelude
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- For upgrading to streamly-0.9.0+ please read the
-- <https://github.com/composewell/streamly/blob/streamly-0.10.0/docs/User/Project/Upgrading-0.8-to-0.9.md Streamly-0.9.0 upgrade guide>.
-- Also, see the "Streamly.Data.Stream.MkType" module for direct replacement of
-- stream types that have been removed in 0.9.0.
--
-- All Stream related combinators including the streamly-core
-- "Streamly.Data.Stream" module, concurrency, time and lifted
-- exception operations. For more pre-release operations also see
-- "Streamly.Internal.Data.Stream.Prelude" module.
--
module Streamly.Data.Stream.Prelude
    (
    -- * "Streamly.Data.Stream"
    -- | All "Streamly.Data.Stream" combinators are re-exported via this
    -- module. For more pre-release combinators also see
    -- "Streamly.Internal.Data.Stream" module.
      module Streamly.Data.Stream

    -- * Concurrent Operations
    -- $concurrency

    -- ** Types
    , MonadAsync

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
    -- | Evaluate a stream as a whole concurrently with respect to the consumer
    -- of the stream.

    , parEval

    -- *** Generate
    -- | Generate a stream by evaluating multiple actions concurrently.
    , parRepeatM
    , parReplicateM
    , fromCallback

    -- *** Map
    -- | Map actions on a stream such that the mapped actions are evaluated
    -- concurrently with each other.

    , parMapM
    , parSequence

    -- *** Combine two
    -- | Combine two streams such that each stream as a whole is evaluated
    -- concurrently with respect to the other stream as well as the consumer of
    -- the resulting stream.
    , parZipWithM
    , parZipWith
    , parMergeByM
    , parMergeBy

    -- *** List of streams
    -- | Shares a single channel across many streams.

    , parList
    -- , zipWithM
    -- , zipWith

    -- *** Stream of streams
    -- **** Apply

    , parApply

    -- **** Concat
    -- | Shares a single channel across many streams.

    , parConcat
    , parConcatMap

    -- **** ConcatIterate
    , parConcatIterate

    -- *** Observation
    , parTapCount

    -- * Time Related

    -- ** Timers
    , interject

    -- ** Trimming
    , takeInterval
    , dropInterval

    -- ** Chunking
    , intervalsOf

    -- ** Sampling
    , sampleIntervalEnd
    , sampleIntervalStart
    , sampleBurstEnd
    , sampleBurstStart

    -- * Lifted Exceptions
    , after
    , bracket
    -- , bracket3
    , finally

    -- ** Deprecated
    , tapCount
    )
where

import Streamly.Data.Stream
import Streamly.Internal.Data.Stream.Prelude

-- $concurrency
--
-- == Channels
--
-- At a lower level, concurrency is implemented using channels that support
-- concurrent evaluation of streams. We create a channel, and add one or more
-- streams to it. The channel evaluates multiple streams concurrently and then
-- generates a single output stream from the results. How the streams are
-- combined depends on the configuration of the channel.
--
-- == Primitives
--
-- There are only a few fundamental abstractions for concurrency, 'parEval',
-- 'parConcatMap', and 'parConcatIterate', all concurrency combinators can be
-- expressed in terms of these.
--
-- 'parEval' evaluates a stream as a whole asynchronously with respect to
-- the consumer of the stream. A worker thread evaluates multiple elements of
-- the stream ahead of time and buffers the results; the consumer of the stream
-- runs in another thread consuming the elements from the buffer, thus
-- decoupling the production and consumption of the stream. 'parEval' can be
-- used to run different stages of a pipeline concurrently.
--
-- 'parConcatMap' is used to evaluate multiple actions in a stream concurrently
-- with respect to each other or to evaluate multiple streams concurrently and
-- combine the results. A stream generator function is mapped to the input
-- stream and all the generated streams are then evaluated concurrently, and
-- the results are combined.
--
-- 'parConcatIterate' is like 'parConcatMap' but iterates a stream generator
-- function recursively over the stream. This can be used to traverse trees or
-- graphs.
--
-- == Configuration
--
-- Concurrent combinators take a 'Config' argument which controls the
-- concurrent behavior. For example, maximum number of threads to be used
-- ('maxThreads') or the maxmimum size of the buffer ('maxBuffer'), or how the
-- streams are scheduled with respect to each other ('interleaved'), or how the
-- results are consumed ('ordered').
--
-- Configuration is specified as @Config -> Config@ modifier functions that can
-- be composed together using function composition. For example, to specify the
-- maximum threads we can use @parConcatMap (maxThreads 10)@ if we also want to
-- specify the maximum buffer we can compose the two options @parConcatMap
-- (maxThreads 10 . maxBuffer 100)@. To use default configuration use 'id' as
-- the config modifier e.g. @parConcatMap id@.
--
-- See the @Configuration@ section and individual configuration options'
-- documentation for the default behavior and default values of configuration
-- parameters.
--
-- == Scheduling
--
-- The most important configuration option is to control whether the output of
-- the concurrent execution is consumed in the same order as the corresponding
-- actions in the input stream or as soon as they arrive. The default is the
-- latter, however, we can enforce the original order by using the 'ordered'
-- option.
--
-- Another important option controls whether the number of worker threads are
-- automatically increased and decreased based on the consumption rate or
-- threads are started as aggresively as possible until the 'maxThreads' or
-- 'maxBuffer' limits are hit. The default is the former. However, the 'eager'
-- option can be enabled to use the latter behavior. When 'eager' is on, even
-- if the stream consumer thread blocks it does not make any impact on the
-- scheduling of the available tasks.
--
-- == Combinators
--
-- Using the few fundamental concurrency primitives we can implement all the
-- usual streaming combinators with concurrent behavior. Combinators like
-- 'unfoldrM', 'iterateM' that are inherently serial can be evaluated
-- concurrently with respect to the consumer pipeline using 'parEval'.
-- Combinators like 'zipWithM', 'mergeByM' can also use 'parEval' on the input
-- streams to evaluate them concurrently before combining.
--
-- Combinators like 'repeatM', 'replicateM', 'fromListM', 'sequence', 'mapM' in
-- which all actions are independent of each other can be made concurrent using
-- the 'parConcatMap' operation.
--
-- A concurrent 'repeatM' repeats an action using multiple concurrent
-- executions of the action. Similarly, a concurrent 'mapM' performs the mapped
-- action in independent threads.
--
-- Some common concurrent combinators are provided in this module.
