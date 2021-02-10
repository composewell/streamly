-- |
-- Module      : Streamly.Internal.Data.Time.Clock
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Time.Clock
    (
    -- * System clock
      Clock(..)
    , getTime

    -- * Async clock
    , asyncClock
    , readClock
    )
where

import Control.Concurrent (threadDelay, ThreadId)
import Control.Monad (forever)
import Streamly.Internal.Data.Time.Clock.Type (Clock(..), getTime)
import Streamly.Internal.Data.Time.Units (MicroSecond64(..), fromAbsTime)
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
