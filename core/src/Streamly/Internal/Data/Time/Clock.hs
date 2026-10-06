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
      module Streamly.Internal.Data.Time.Clock.Type

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

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (when, void)
import Streamly.Internal.Data.Time.Units
    (MicroSecond64(..), fromAbsTime, addToAbsTime, toRelTime)
import System.Mem.Weak (Weak, deRefWeak)

import qualified Streamly.Internal.Data.IORef as Unboxed

import Streamly.Internal.Data.Time.Clock.Type

------------------------------------------------------------------------------
-- Async clock
------------------------------------------------------------------------------

{-# INLINE updateTimeVar #-}
updateTimeVar :: Clock -> Unboxed.IORef MicroSecond64 -> IO ()
updateTimeVar clock timeVar = do
    t <- fromAbsTime <$> getTime clock
    Unboxed.modifyIORef' timeVar (const t)

-- Returns False if the IORef is no longer reachable.
{-# INLINE updateWithDelay #-}
updateWithDelay :: RealFrac a =>
    Clock -> a -> Weak (Unboxed.IORef MicroSecond64) -> IO Bool
updateWithDelay clock precision weakVar = do
    threadDelay (delayTime precision)
    r <- deRefWeak weakVar
    case r of
        Nothing -> return False
        Just timeVar -> updateTimeVar clock timeVar >> return True

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
-- read asynchronously.  The thread exits automatically when the IORef is no
-- longer reachable.
--
-- Minimum granularity of clock update is 1 ms. Higher is better for
-- performance.
--
-- CAUTION! This is safe only on a 64-bit machine. On a 32-bit machine a 64-bit
-- 'Var' cannot be read consistently without a lock while another thread is
-- writing to it.
asyncClock :: Clock -> Double -> IO (ThreadId, Unboxed.IORef MicroSecond64)
asyncClock clock g = do
    timeVar <- Unboxed.newIORef 0
    updateTimeVar clock timeVar
    -- The thread holds only a weak reference to timeVar. The consumers may
    -- drop the returned ThreadId, so a finalizer on the ThreadId may kill the
    -- thread at the next GC while timeVar is still being read.
    weakVar <- Unboxed.mkWeakIORef timeVar (return ())
    tid <- forkIO $ loop weakVar
    return (tid, timeVar)

    where

    loop weakVar = do
        continue <- updateWithDelay clock g weakVar
        when continue $ loop weakVar

{-# INLINE readClock #-}
readClock :: (ThreadId, Unboxed.IORef MicroSecond64) -> IO MicroSecond64
readClock (_, timeVar) = Unboxed.readIORef timeVar

------------------------------------------------------------------------------
-- Adjustable Timer
------------------------------------------------------------------------------

-- | Adjustable periodic timer.
data Timer = Timer ThreadId (MVar ()) (Unboxed.IORef MicroSecond64) (IO ())

-- Set the expiry to current time + timer period
{-# INLINE resetTimerExpiry #-}
resetTimerExpiry :: Clock -> MicroSecond64 -> Unboxed.IORef MicroSecond64 -> IO ()
resetTimerExpiry clock period timeVar = do
    t <- getTime clock
    let t1 = addToAbsTime t (toRelTime period)
    Unboxed.modifyIORef' timeVar (const (fromAbsTime t1))

-- Returns False if the IORef is no longer reachable.
{-# INLINE processTimerTick #-}
processTimerTick :: RealFrac a =>
       Clock
    -> a
    -> MicroSecond64
    -> Weak (Unboxed.IORef MicroSecond64)
    -> MVar ()
    -> IO Bool
processTimerTick clock precision period weakVar mvar = do
    threadDelay (delayTime precision)
    r <- deRefWeak weakVar
    case r of
        Nothing -> return False
        Just timeVar -> do
            t <- fromAbsTime <$> getTime clock
            expiry <- Unboxed.readIORef timeVar
            when (t >= expiry) $ do
                -- non-blocking put so that we can process multiple timers in
                -- a non-blocking manner in future.
                void $ tryPutMVar mvar ()
                resetTimerExpiry clock period timeVar
            return True

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
    timeVar <- Unboxed.newIORef 0
    let p = round (period * 1e6) :: Int
        p1 = fromIntegral p :: MicroSecond64
    resetTimerExpiry clock p1 timeVar
    -- The thread holds only a weak reference to timeVar, and exits when the
    -- Timer is no longer reachable. It holds mvar strongly, otherwise a thread
    -- blocked in waitTimer would be unreachable and get
    -- BlockedIndefinitelyOnMVar.
    weakVar <- Unboxed.mkWeakIORef timeVar (return ())
    tid <- forkIO $ loop p1 weakVar mvar
    return $ Timer tid mvar timeVar (resetTimerExpiry clock p1 timeVar)

    where

    loop p1 weakVar mvar = do
        continue <- processTimerTick clock g p1 weakVar mvar
        when continue $ loop p1 weakVar mvar

-- | Blocking wait for a timer tick.
{-# INLINE waitTimer #-}
waitTimer :: Timer -> IO ()
waitTimer (Timer _ mvar timeVar _) = do
    takeMVar mvar
    -- Keep timeVar reachable, so that the timer thread keeps running
    Unboxed.touchIORef timeVar

-- | Resets the current period.
{-# INLINE resetTimer #-}
resetTimer :: Timer -> IO ()
resetTimer (Timer _ _ _ reset) = reset

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
