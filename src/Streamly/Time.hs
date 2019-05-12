{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Streamly.Time
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Time utilities for reactive programming.

module Streamly.Time
    ( intervalsOf

    -- * Deprecated
    , periodic
    , withClock
    )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromJust, isJust)

import Streamly (parallel)
import Streamly.SVar (MonadAsync)
import Streamly.Streams.StreamK (IsStream(..))
import Streamly.Fold (Fold)

import qualified Streamly.Fold as FL
import qualified Streamly.Prelude as S

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

-------------------------------------------------------------------------------
-- Stream element unaware: Time based buffering
-------------------------------------------------------------------------------

-- There are two ways in which time based buffering can be implemented.
--
-- 1) One is to use an API like groupsOf to group elements by time and then
-- flush when the group is complete. This will require checking time on each
-- element yield.
--
-- 2) Use an async thread to write. The producer thread would just queue the
-- elements to be written on a queue associated with the writer thread. When
-- the write thread is scheduled elements that have been buffered will be
-- written based on a buffering policy. This will require the producer to
-- synchronize on the queue, send a doorbell when the queue has items in it.
-- The queue can be an array which can be written directly to the IO device.
--
-- We can try both and see which one performs better.

-- XXX add this example after fixing the serial stream rate control
-- >>> S.toList $ S.take 5 $ intervalsOf 1 FL.sum $ constRate 2 $ S.enumerateFrom 1
-- > [3,7,11,15,19]
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- @since 0.7.0
{-# INLINE intervalsOf #-}
intervalsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
intervalsOf n f m = FL.grouped (catMaybes f) s
    where
    s = S.map (\x -> (Just x, False)) m `parallel` S.repeatM timeout
    timeout = do
        liftIO $ threadDelay (round $ n * 1000000)
        return (Nothing, True)
    catMaybes = FL.lfilter isJust . FL.lmap fromJust
