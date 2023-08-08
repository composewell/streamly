{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Combinators
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.IsStream.Combinators {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    ( maxThreads
    , maxBuffer
    , maxYields
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate
    , inspectMode
    , printState
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Int (Int64)

import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream, mkStream, foldStreamShared)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.StreamK (Stream)

import Streamly.Internal.Data.SVar

-------------------------------------------------------------------------------
-- Concurrency control
-------------------------------------------------------------------------------
--
-- XXX need to write these in direct style otherwise they will break fusion.
--
-- | Specify the maximum number of threads that can be spawned concurrently for
-- any concurrent combinator in a stream.
-- A value of 0 resets the thread limit to default, a negative value means
-- there is no limit. The default value is 1500. 'maxThreads' does not affect
-- 'ParallelT' streams as they can use unbounded number of threads.
--
-- When the actions in a stream are IO bound, having blocking IO calls, this
-- option can be used to control the maximum number of in-flight IO requests.
-- When the actions are CPU bound this option can be used to
-- control the amount of CPU used by the stream.
--
-- /Since: 0.4.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE_NORMAL maxThreads #-}
maxThreads :: IsStream t => Int -> t m a -> t m a
maxThreads n m = mkStream $ \st stp sng yld ->
    foldStreamShared (setMaxThreads n st) stp sng yld m

{-
{-# RULES "maxThreadsSerial serial" maxThreads = maxThreadsSerial #-}
maxThreadsSerial :: Int -> SerialT m a -> SerialT m a
maxThreadsSerial _ = id
-}

-- | Specify the maximum size of the buffer for storing the results from
-- concurrent computations. If the buffer becomes full we stop spawning more
-- concurrent tasks until there is space in the buffer.
-- A value of 0 resets the buffer size to default, a negative value means
-- there is no limit. The default value is 1500.
--
-- CAUTION! using an unbounded 'maxBuffer' value (i.e. a negative value)
-- coupled with an unbounded 'maxThreads' value is a recipe for disaster in
-- presence of infinite streams, or very large streams.  Especially, it must
-- not be used when 'pure' is used in 'ZipAsyncM' streams as 'pure' in
-- applicative zip streams generates an infinite stream causing unbounded
-- concurrent generation with no limit on the buffer or threads.
--
-- /Since: 0.4.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE_NORMAL maxBuffer #-}
maxBuffer :: IsStream t => Int -> t m a -> t m a
maxBuffer n m = mkStream $ \st stp sng yld ->
    foldStreamShared (setMaxBuffer n st) stp sng yld m

{-
{-# RULES "maxBuffer serial" maxBuffer = maxBufferSerial #-}
maxBufferSerial :: Int -> SerialT m a -> SerialT m a
maxBufferSerial _ = id
-}

-- | Specify the pull rate of a stream.
-- A 'Nothing' value resets the rate to default which is unlimited.  When the
-- rate is specified, concurrent production may be ramped up or down
-- automatically to achieve the specified yield rate. The specific behavior for
-- different styles of 'Rate' specifications is documented under 'Rate'.  The
-- effective maximum production rate achieved by a stream is governed by:
--
-- * The 'maxThreads' limit
-- * The 'maxBuffer' limit
-- * The maximum rate that the stream producer can achieve
-- * The maximum rate that the stream consumer can achieve
--
-- /Since: 0.5.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE_NORMAL rate #-}
rate :: IsStream t => Maybe Rate -> t m a -> t m a
rate r m = mkStream $ \st stp sng yld ->
    case r of
        Just (Rate low goal _ _) | goal < low ->
            error "rate: Target rate cannot be lower than minimum rate."
        Just (Rate _ goal high _) | goal > high ->
            error "rate: Target rate cannot be greater than maximum rate."
        Just (Rate low _ high _) | low > high ->
            error "rate: Minimum rate cannot be greater than maximum rate."
        _ -> foldStreamShared (setStreamRate r st) stp sng yld m

-- XXX implement for serial streams as well, as a simple delay

{-
{-# RULES "rate serial" rate = yieldRateSerial #-}
yieldRateSerial :: Double -> SerialT m a -> SerialT m a
yieldRateSerial _ = id
-}

-- | Same as @rate (Just $ Rate (r/2) r (2*r) maxBound)@
--
-- Specifies the average production rate of a stream in number of yields
-- per second (i.e.  @Hertz@).  Concurrent production is ramped up or down
-- automatically to achieve the specified average yield rate. The rate can
-- go down to half of the specified rate on the lower side and double of
-- the specified rate on the higher side.
--
-- /Since: 0.5.0 ("Streamly")/
--
-- @since 0.8.0
avgRate :: IsStream t => Double -> t m a -> t m a
avgRate r = rate (Just $ Rate (r/2) r (2*r) maxBound)

-- | Same as @rate (Just $ Rate r r (2*r) maxBound)@
--
-- Specifies the minimum rate at which the stream should yield values. As
-- far as possible the yield rate would never be allowed to go below the
-- specified rate, even though it may possibly go above it at times, the
-- upper limit is double of the specified rate.
--
-- /Since: 0.5.0 ("Streamly")/
--
-- @since 0.8.0
minRate :: IsStream t => Double -> t m a -> t m a
minRate r = rate (Just $ Rate r r (2*r) maxBound)

-- | Same as @rate (Just $ Rate (r/2) r r maxBound)@
--
-- Specifies the maximum rate at which the stream should yield values. As
-- far as possible the yield rate would never be allowed to go above the
-- specified rate, even though it may possibly go below it at times, the
-- lower limit is half of the specified rate. This can be useful in
-- applications where certain resource usage must not be allowed to go
-- beyond certain limits.
--
-- /Since: 0.5.0 ("Streamly")/
--
-- @since 0.8.0
maxRate :: IsStream t => Double -> t m a -> t m a
maxRate r = rate (Just $ Rate (r/2) r r maxBound)

-- | Same as @rate (Just $ Rate r r r 0)@
--
-- Specifies a constant yield rate. If for some reason the actual rate
-- goes above or below the specified rate we do not try to recover it by
-- increasing or decreasing the rate in future.  This can be useful in
-- applications like graphics frame refresh where we need to maintain a
-- constant refresh rate.
--
-- /Since: 0.5.0 ("Streamly")/
--
-- @since 0.8.0
constRate :: IsStream t => Double -> t m a -> t m a
constRate r = rate (Just $ Rate r r r 0)

-- | Specify the average latency, in nanoseconds, of a single threaded action
-- in a concurrent composition. Streamly can measure the latencies, but that is
-- possible only after at least one task has completed. This combinator can be
-- used to provide a latency hint so that rate control using 'rate' can take
-- that into account right from the beginning. When not specified then a
-- default behavior is chosen which could be too slow or too fast, and would be
-- restricted by any other control parameters configured.
-- A value of 0 indicates default behavior, a negative value means there is no
-- limit i.e. zero latency.
-- This would normally be useful only in high latency and high throughput
-- cases.
--
{-# INLINE_NORMAL _serialLatency #-}
_serialLatency :: IsStream t => Int -> t m a -> t m a
_serialLatency n m = mkStream $ \st stp sng yld ->
    foldStreamShared (setStreamLatency n st) stp sng yld m

{-
{-# RULES "serialLatency serial" _serialLatency = serialLatencySerial #-}
serialLatencySerial :: Int -> SerialT m a -> SerialT m a
serialLatencySerial _ = id
-}

-- Stop concurrent dispatches after this limit. This is useful in API's like
-- "take" where we want to dispatch only upto the number of elements "take"
-- needs.  This value applies only to the immediate next level and is not
-- inherited by everything in enclosed scope.
{-# INLINE_NORMAL maxYields #-}
maxYields :: IsStream t => Maybe Int64 -> t m a -> t m a
maxYields n m = mkStream $ \st stp sng yld ->
    foldStreamShared (setYieldLimit n st) stp sng yld m

{-# RULES "maxYields serial" maxYields = maxYieldsSerial #-}
maxYieldsSerial :: Maybe Int64 -> SerialT m a -> SerialT m a
maxYieldsSerial _ = id

printState :: MonadIO m => State Stream m a -> m ()
printState st = liftIO $ do
    let msv = streamVar st
    case msv of
        Just sv -> dumpSVar sv >>= putStrLn
        Nothing -> putStrLn "No SVar"

-- | Print debug information about an SVar when the stream ends
--
-- /Pre-release/
--
inspectMode :: IsStream t => t m a -> t m a
inspectMode m = mkStream $ \st stp sng yld ->
     foldStreamShared (setInspectMode st) stp sng yld m
