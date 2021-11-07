-- |
-- Module      : Streamly.Internal.Data.Time.Clock
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC

module Streamly.Internal.Data.Time.Clock
    (
    -- * System clock
      Clock(..)
    , getTime

    -- * Async clock
    , asyncClock
    , readClock

    -- * Adjustable Timer
    , Timer
    , timer
    , resetTimer
    , extendTimer
    , shortenTimer
    , readTimer
    , waitTimer
    )
where

import Control.Concurrent (threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (forever, when, void)
import Streamly.Internal.Data.Time.Clock.Type (Clock(..), getTime)
import Streamly.Internal.Data.Time.Units
    (MicroSecond64(..), fromAbsTime, addToAbsTime, toRelTime)
import Streamly.Internal.Control.Concurrent (forkManaged)

import qualified Streamly.Internal.Data.IORef.Prim as Prim

------------------------------------------------------------------------------
-- Async clock
------------------------------------------------------------------------------

{-# INLINE updateTimeVar #-}
updateTimeVar :: Clock -> Prim.IORef MicroSecond64 -> IO ()
updateTimeVar clock timeVar = do
    t <- fromAbsTime <$> getTime clock
    Prim.modifyIORef' timeVar (const t)

{-# INLINE updateWithDelay #-}
updateWithDelay :: RealFrac a =>
    Clock -> a -> Prim.IORef MicroSecond64 -> IO ()
updateWithDelay clock precision timeVar = do
    threadDelay (delayTime precision)
    updateTimeVar clock timeVar

    where

    -- Keep the minimum at least a millisecond to avoid high CPU usage
    {-# INLINE delayTime #-}
    delayTime g
        | g' >= fromIntegral (maxBound :: Int) = maxBound
        | g' < 1000 = 1000
        | otherwise = round g'

        where

        g' = g * 10 ^ (6 :: Int)

-- | @asyncClock g@ starts a clock thread that updates an IORef with current
-- time as a 64-bit value in microseconds, every 'g' seconds. The IORef can be
-- read asynchronously.  The thread exits automatically when the reference to
-- the returned 'ThreadId' is lost.
--
-- Minimum granularity of clock update is 1 ms. Higher is better for
-- performance.
--
-- CAUTION! This is safe only on a 64-bit machine. On a 32-bit machine a 64-bit
-- 'Var' cannot be read consistently without a lock while another thread is
-- writing to it.
asyncClock :: Clock -> Double -> IO (ThreadId, Prim.IORef MicroSecond64)
asyncClock clock g = do
    timeVar <- Prim.newIORef 0
    updateTimeVar clock timeVar
    tid <- forkManaged $ forever (updateWithDelay clock g timeVar)
    return (tid, timeVar)

{-# INLINE readClock #-}
readClock :: (ThreadId, Prim.IORef MicroSecond64) -> IO MicroSecond64
readClock (_, timeVar) = Prim.readIORef timeVar

------------------------------------------------------------------------------
-- Adjustable Timer
------------------------------------------------------------------------------

-- | Adjustable periodic timer.
data Timer = Timer ThreadId (MVar ()) (IO ())

-- Set the expiry to current time + timer period
{-# INLINE resetTimerExpiry #-}
resetTimerExpiry :: Clock -> MicroSecond64 -> Prim.IORef MicroSecond64 -> IO ()
resetTimerExpiry clock period timeVar = do
    t <- getTime clock
    let t1 = addToAbsTime t (toRelTime period)
    Prim.modifyIORef' timeVar (const (fromAbsTime t1))

{-# INLINE processTimerTick #-}
processTimerTick :: RealFrac a =>
    Clock -> a -> Prim.IORef MicroSecond64 -> MVar () -> IO () -> IO ()
processTimerTick clock precision timeVar mvar reset = do
    threadDelay (delayTime precision)
    t <- fromAbsTime <$> getTime clock
    expiry <- Prim.readIORef timeVar
    when (t >= expiry) $ do
        -- non-blocking put so that we can process multiple timers in a
        -- non-blocking manner in future.
        void $ tryPutMVar mvar ()
        reset

    where

    -- Keep the minimum at least a millisecond to avoid high CPU usage
    {-# INLINE delayTime #-}
    delayTime g
        | g' >= fromIntegral (maxBound :: Int) = maxBound
        | g' < 1000 = 1000
        | otherwise = round g'

        where

        g' = g * 10 ^ (6 :: Int)

-- XXX In future we can add a timer in a heap of timers.
--
-- | @timer clockType granularity period@ creates a timer.  The timer produces
-- timer ticks at specified time intervals that can be waited upon using
-- 'waitTimer'.  If the previous tick is not yet processed, the new tick is
-- lost.
timer :: Clock -> Double -> Double -> IO Timer
timer clock g period = do
    mvar <- newEmptyMVar
    timeVar <- Prim.newIORef 0
    let p = round (period * 1e6) :: Int
        p1 = fromIntegral p :: MicroSecond64
        reset = resetTimerExpiry clock p1 timeVar
        process = processTimerTick clock g timeVar mvar reset
    reset
    tid <- forkManaged $ forever process
    return $ Timer tid mvar reset

-- | Blocking wait for a timer tick.
{-# INLINE waitTimer #-}
waitTimer :: Timer -> IO ()
waitTimer (Timer _ mvar _) = takeMVar mvar

-- | Resets the current period.
{-# INLINE resetTimer #-}
resetTimer :: Timer -> IO ()
resetTimer (Timer _ _ reset) = reset

-- | Elongates the current period by specified amount.
--
-- /Unimplemented/
{-# INLINE extendTimer #-}
extendTimer :: Timer -> Double -> IO ()
extendTimer = undefined

-- | Shortens the current period by specified amount.
--
-- /Unimplemented/
{-# INLINE shortenTimer #-}
shortenTimer :: Timer -> Double -> IO ()
shortenTimer = undefined

-- | Show the remaining time in the current time period.
--
-- /Unimplemented/
{-# INLINE readTimer #-}
readTimer :: Timer -> IO Double
readTimer = undefined
