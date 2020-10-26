-- |
-- Module      : Streamly.Internal.Data.Time
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Time utilities for reactive programming.

module Streamly.Internal.Data.Time
{-# DEPRECATED
   "Please use the \"rate\" combinator instead of the functions in this module"
  #-}
    ( periodic
    , withClock
    )
where

import Control.Monad (when)
import Control.Concurrent (threadDelay)

-- | Run an action forever periodically at the given frequency specified in per
-- second (Hz).
--
-- @since 0.1.0
{-# DEPRECATED periodic "Please use the \"rate\" combinator instead" #-}
periodic :: Int -> IO () -> IO ()
periodic freq action = do
    action
    threadDelay (1000000 `div` freq)
    periodic freq action

-- | Run a computation on every clock tick, the clock runs at the specified
-- frequency. It allows running a computation at high frequency efficiently by
-- maintaining a local clock and adjusting it with the provided base clock at
-- longer intervals.  The first argument is a base clock returning some notion
-- of time in microseconds. The second argument is the frequency in per second
-- (Hz). The third argument is the action to run, the action is provided the
-- local time as an argument.
--
-- @since 0.1.0
{-# DEPRECATED withClock "Please use the \"rate\" combinator instead" #-}
withClock :: IO Int -> Int -> (Int -> IO ()) -> IO ()
withClock clock freq action = do
    t <- clock
    go t period period t 0

    where

    period = 1000000 `div` freq

    -- Note that localTime is roughly but not exactly equal to (lastAdj + tick
    -- * n).  That is because we do not abruptly adjust the clock skew instead
    -- we adjust the tick size.
    go lastAdj delay tick localTime n = do
        action localTime
        when (delay > 0) $ threadDelay delay

        if n == freq
        then do
            (t, newTick, newDelay) <- adjustClock lastAdj localTime delay
            go t newDelay newTick (localTime + newTick) 0
        else go lastAdj delay tick (localTime + tick) (n + 1)

    -- Adjust the tick size rather than the clock to avoid abrupt changes
    -- resulting in jittery behavior at the end of every interval.
    adjustClock lastAdj localTime delay = do
        baseTime <- clock
        let newTick    = period + (baseTime - localTime) `div` freq
            lastPeriod = (baseTime - lastAdj) `div` freq
            newDelay   = max 0 (delay + period - lastPeriod)
        return (baseTime, newTick, newDelay)
