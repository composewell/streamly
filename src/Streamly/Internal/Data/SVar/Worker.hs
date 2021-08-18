-- |
-- Module      : Streamly.Internal.Data.SVar.Worker
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.SVar.Worker
    (
    -- * Adjusting Limits
      decrementYieldLimit
    , incrementYieldLimit
    , decrementBufferLimit
    , incrementBufferLimit
    , resetBufferLimit

    -- * Rate Control
    , Work (..)
    , isBeyondMaxRate
    , estimateWorkers
    , updateYieldCount
    , minThreadDelay
    , workerRateControl
    , workerUpdateLatency

    -- * Send Events
    , send
    , ringDoorBell

    -- ** Yield
    , sendYield
    , sendToProducer

    -- ** Stop
    , sendStop
    , sendStopToProducer

    -- ** Exception
    , handleChildException
    , handleFoldException
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId, takeMVar)
import Control.Concurrent.MVar (MVar, tryPutMVar)
import Control.Exception (SomeException(..), assert)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (IORef, readIORef, writeIORef)
import Streamly.Internal.Data.Atomics
       (atomicModifyIORefCAS, atomicModifyIORefCAS_, writeBarrier,
        storeLoadBarrier)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), diffAbsTime64, fromRelTime64)

import Streamly.Internal.Data.SVar.Type

------------------------------------------------------------------------------
-- Collecting results from child workers in a streamed fashion
------------------------------------------------------------------------------

-- XXX Can we make access to remainingWork and yieldRateInfo fields in sv
-- faster, along with the fields in sv required by send?
-- XXX make it noinline
--
-- XXX we may want to employ an increment and decrement in batches when the
-- througput is high or when the cost of synchronization is high. For example
-- if the application is distributed then inc/dec of a shared variable may be
-- very costly.
--
-- A worker decrements the yield limit before it executes an action. However,
-- the action may not result in an element being yielded, in that case we have
-- to increment the yield limit.
--
-- Note that we need it to be an Int type so that we have the ability to undo a
-- decrement that takes it below zero.
{-# INLINE decrementYieldLimit #-}
decrementYieldLimit :: SVar t m a -> IO Bool
decrementYieldLimit sv =
    case remainingWork sv of
        Nothing -> return True
        Just ref -> do
            r <- atomicModifyIORefCAS ref $ \x -> (x - 1, x)
            return $ r >= 1

{-# INLINE incrementYieldLimit #-}
incrementYieldLimit :: SVar t m a -> IO ()
incrementYieldLimit sv =
    case remainingWork sv of
        Nothing -> return ()
        Just ref -> atomicModifyIORefCAS_ ref (+ 1)

-- XXX exception safety of all atomic/MVar operations

-- TBD Each worker can have their own queue and the consumer can empty one
-- queue at a time, that way contention can be reduced.

-- XXX Only yields should be counted in the buffer limit and not the Stop
-- events.

{-# INLINE decrementBufferLimit #-}
decrementBufferLimit :: SVar t m a -> IO ()
decrementBufferLimit sv =
    case maxBufferLimit sv of
        Unlimited -> return ()
        Limited _ -> do
            let ref = pushBufferSpace sv
            old <- atomicModifyIORefCAS ref $ \x ->
                        (if x >= 1 then x - 1 else x, x)
            when (old <= 0) $
                case pushBufferPolicy sv of
                    PushBufferBlock -> blockAndRetry
                    PushBufferDropNew -> do
                        -- We just drop one item and proceed. It is possible
                        -- that by the time we drop the item the consumer
                        -- thread might have run and created space in the
                        -- buffer, but we do not care about that condition.
                        -- This is not pedantically correct but it should be
                        -- fine in practice.
                        -- XXX we may want to drop only if n == maxBuf
                        -- otherwise we must have space in the buffer and a
                        -- decrement should be possible.
                        block <- atomicModifyIORefCAS (outputQueue sv) $
                            \(es, n) ->
                                case es of
                                    [] -> (([],n), True)
                                    _ : xs -> ((xs, n - 1), False)
                        when block blockAndRetry
                    -- XXX need a dequeue or ring buffer for this
                    PushBufferDropOld -> undefined

    where

    blockAndRetry = do
        let ref = pushBufferSpace sv
        liftIO $ takeMVar (pushBufferMVar sv)
        old <- atomicModifyIORefCAS ref $ \x ->
                        (if x >= 1 then x - 1 else x, x)
        -- When multiple threads sleep on takeMVar, the first thread would
        -- wakeup due to a putMVar by the consumer, but the rest of the threads
        -- would have to put back the MVar after taking it and decrementing the
        -- buffer count, otherwise all other threads will remain asleep.
        if old >= 1
        then void $ liftIO $ tryPutMVar (pushBufferMVar sv) ()
        -- We do not put the MVar back in this case, instead we
        -- wait for the consumer to put it.
        else blockAndRetry

{-# INLINE incrementBufferLimit #-}
incrementBufferLimit :: SVar t m a -> IO ()
incrementBufferLimit sv =
    case maxBufferLimit sv of
        Unlimited -> return ()
        Limited _ -> do
            atomicModifyIORefCAS_ (pushBufferSpace sv) (+ 1)
            writeBarrier
            void $ liftIO $ tryPutMVar (pushBufferMVar sv) ()

{-# INLINE resetBufferLimit #-}
resetBufferLimit :: SVar t m a -> IO ()
resetBufferLimit sv =
    case maxBufferLimit sv of
        Unlimited -> return ()
        Limited n -> atomicModifyIORefCAS_ (pushBufferSpace sv)
                                           (const (fromIntegral n))

-------------------------------------------------------------------------------
-- Yield control
-------------------------------------------------------------------------------

updateYieldCount :: WorkerInfo -> IO Count
updateYieldCount winfo = do
    cnt <- readIORef (workerYieldCount winfo)
    let cnt1 = cnt + 1
    writeIORef (workerYieldCount winfo) cnt1
    return cnt1

isBeyondMaxYield :: Count -> WorkerInfo -> Bool
isBeyondMaxYield cnt winfo =
    let ymax = workerYieldMax winfo
    in ymax /= 0 && cnt >= ymax

-------------------------------------------------------------------------------
-- Sending results from worker
-------------------------------------------------------------------------------

{-# INLINE ringDoorBell #-}
ringDoorBell :: SVar t m a -> IO ()
ringDoorBell sv = do
    storeLoadBarrier
    w <- readIORef $ needDoorBell sv
    when w $ do
        -- Note: the sequence of operations is important for correctness here.
        -- We need to set the flag to false strictly before sending the
        -- outputDoorBell, otherwise the outputDoorBell may get processed too
        -- early and then we may set the flag to False to later making the
        -- consumer lose the flag, even without receiving a outputDoorBell.
        atomicModifyIORefCAS_ (needDoorBell sv) (const False)
        void $ tryPutMVar (outputDoorBell sv) ()

{-# INLINE sendWithDoorBell #-}
sendWithDoorBell ::
    IORef ([ChildEvent a], Int) -> MVar () -> ChildEvent a -> IO Int
sendWithDoorBell q bell msg = do
    -- XXX can the access to outputQueue be made faster somehow?
    oldlen <- atomicModifyIORefCAS q $ \(es, n) ->
        ((msg : es, n + 1), n)
    when (oldlen <= 0) $ do
        -- The wake up must happen only after the store has finished otherwise
        -- we can have lost wakeup problems.
        writeBarrier
        -- Since multiple workers can try this at the same time, it is possible
        -- that we may put a spurious MVar after the consumer has already seen
        -- the output. But that's harmless, at worst it may cause the consumer
        -- to read the queue again and find it empty.
        -- The important point is that the consumer is guaranteed to receive a
        -- doorbell if something was added to the queue after it empties it.
        void $ tryPutMVar bell ()
    return oldlen

-- | This function is used by the producer threads to queue output for the
-- consumer thread to consume. Returns whether the queue has more space.
send :: SVar t m a -> ChildEvent a -> IO Int
send sv = sendWithDoorBell (outputQueue sv) (outputDoorBell sv)

-- There is no bound implemented on the buffer, this is assumed to be low
-- traffic.
sendToProducer :: SVar t m a -> ChildEvent a -> IO Int
sendToProducer sv msg = do
    -- In case the producer stream is blocked on pushing to the fold buffer
    -- then wake it up so that it can check for the stop event or exception
    -- being sent to it otherwise we will be deadlocked.
    void $ tryPutMVar (pushBufferMVar sv) ()
    sendWithDoorBell (outputQueueFromConsumer sv)
                     (outputDoorBellFromConsumer sv) msg

-------------------------------------------------------------------------------
-- Collect and update worker latency
-------------------------------------------------------------------------------

workerCollectLatency :: WorkerInfo -> IO (Maybe (Count, NanoSecond64))
workerCollectLatency winfo = do
    (cnt0, t0) <- readIORef (workerLatencyStart winfo)
    cnt1 <- readIORef (workerYieldCount winfo)
    let cnt = cnt1 - cnt0

    if cnt > 0
    then do
        t1 <- getTime Monotonic
        let period = fromRelTime64 $ diffAbsTime64 t1 t0
        writeIORef (workerLatencyStart winfo) (cnt1, t1)
        return $ Just (cnt, period)
    else return Nothing

-- XXX There are a number of gotchas in measuring latencies.
-- 1) We measure latencies only when a worker yields a value
-- 2) It is possible that a stream calls the stop continuation, in which case
-- the worker would not yield a value and we would not account that worker in
-- latencies. Even though this case should ideally be accounted we do not
-- account it because we cannot or do not distinguish it from the case
-- described next.
-- 3) It is possible that a worker returns without yielding anything because it
-- never got a chance to pick up work.
-- 4) If the system timer resolution is lower than the latency, the latency
-- computation turns out to be zero.
--
-- We can fix this if we measure the latencies by counting the work items
-- picked rather than based on the outputs yielded.
workerUpdateLatency :: YieldRateInfo -> WorkerInfo -> IO ()
workerUpdateLatency yinfo winfo = do
    r <- workerCollectLatency winfo
    case r of
        Just (cnt, period) -> do
        -- NOTE: On JS platform the timer resolution could be pretty low. When
        -- the timer resolution is low, measurement of latencies could be
        -- tricky. All the worker latencies will turn out to be zero if they
        -- are lower than the resolution. We only take into account those
        -- measurements which are more than the timer resolution.

            let ref = workerPendingLatency yinfo
                (cnt1, t1) = if period > 0 then (cnt, period) else (0, 0)
            atomicModifyIORefCAS_ ref $
                    \(fc, n, t) -> (fc + cnt, n + cnt1, t + t1)
        Nothing -> return ()

-------------------------------------------------------------------------------
-- Worker rate control
-------------------------------------------------------------------------------

-- We either block, or send one worker with limited yield count or one or more
-- workers with unlimited yield count.
data Work
    = BlockWait NanoSecond64
    | PartialWorker Count
    | ManyWorkers Int Count
    deriving Show

-- | This is a magic number and it is overloaded, and used at several places to
-- achieve batching:
--
-- 1. If we have to sleep to slowdown this is the minimum period that we
--    accumulate before we sleep. Also, workers do not stop until this much
--    sleep time is accumulated.
-- 3. Collected latencies are computed and transferred to measured latency
--    after a minimum of this period.
minThreadDelay :: NanoSecond64
minThreadDelay = 1000000

-- | Another magic number! When we have to start more workers to cover up a
-- number of yields that we are lagging by then we cannot start one worker for
-- each yield because that may be a very big number and if the latency of the
-- workers is low these number of yields could be very high. We assume that we
-- run each extra worker for at least this much time.
rateRecoveryTime :: NanoSecond64
rateRecoveryTime = 1000000

-- | Get the worker latency without resetting workerPendingLatency
-- Returns (total yield count, base time, measured latency)
-- CAUTION! keep it in sync with collectLatency
getWorkerLatency :: YieldRateInfo -> IO (Count, AbsTime, NanoSecond64)
getWorkerLatency yinfo  = do
    let cur      = workerPendingLatency yinfo
        col      = workerCollectedLatency yinfo
        longTerm = svarAllTimeLatency yinfo
        measured = workerMeasuredLatency yinfo

    (curTotalCount, curCount, curTime) <- readIORef cur
    (colTotalCount, colCount, colTime) <- readIORef col
    (lcount, ltime)     <- readIORef longTerm
    prevLat             <- readIORef measured

    let latCount = colCount + curCount
        latTime  = colTime + curTime
        totalCount = colTotalCount + curTotalCount
        newLat =
            if latCount > 0 && latTime > 0
            then let lat = latTime `div` fromIntegral latCount
                 -- XXX Give more weight to new?
                 in (lat + prevLat) `div` 2
            else prevLat
    return (lcount + totalCount, ltime, newLat)

-- XXX we can use phantom types to distinguish the duration/latency/expectedLat
estimateWorkers
    :: Limit
    -> Count
    -> Count
    -> NanoSecond64
    -> NanoSecond64
    -> NanoSecond64
    -> LatencyRange
    -> Work
estimateWorkers workerLimit svarYields gainLossYields
                svarElapsed wLatency targetLat range =
    -- XXX we can have a maxEfficiency combinator as well which runs the
    -- producer at the maximal efficiency i.e. the number of workers are chosen
    -- such that the latency is minimum or within a range. Or we can call it
    -- maxWorkerLatency.
    --
    let
        -- How many workers do we need to achieve the required rate?
        --
        -- When the workers are IO bound we can increase the throughput by
        -- increasing the number of workers as long as the IO device has enough
        -- capacity to process all the requests concurrently. If the IO
        -- bandwidth is saturated increasing the workers won't help. Also, if
        -- the CPU utilization in processing all these requests exceeds the CPU
        -- bandwidth, then increasing the number of workers won't help.
        --
        -- When the workers are purely CPU bound, increasing the workers beyond
        -- the number of CPUs won't help.
        --
        -- TODO - measure the CPU and IO requirements of the workers. Have a
        -- way to specify the max bandwidth of the underlying IO mechanism and
        -- use that to determine the max rate of workers, and also take the CPU
        -- bandwidth into account. We can also discover the IO bandwidth if we
        -- know that we are not CPU bound, then how much steady state rate are
        -- we able to achieve. Design tests for CPU bound and IO bound cases.

        -- Calculate how many yields are we ahead or behind to match the exact
        -- required rate. Based on that we increase or decrease the effective
        -- workers.
        --
        -- When the worker latency is lower than required latency we begin with
        -- a yield and then wait rather than first waiting and then yielding.
        targetYields = (svarElapsed + wLatency + targetLat - 1) `div` targetLat
        effectiveYields = svarYields + gainLossYields
        deltaYields = fromIntegral targetYields - effectiveYields

        -- We recover the deficit by running at a higher/lower rate for a
        -- certain amount of time. To keep the effective rate in reasonable
        -- limits we use rateRecoveryTime, minLatency and maxLatency.
        in  if deltaYields > 0
            then
                let deltaYieldsFreq :: Double
                    deltaYieldsFreq =
                        fromIntegral deltaYields /
                            fromIntegral rateRecoveryTime
                    yieldsFreq = 1.0 / fromIntegral targetLat
                    totalYieldsFreq = yieldsFreq + deltaYieldsFreq
                    requiredLat = NanoSecond64 $ round $ 1.0 / totalYieldsFreq
                    adjustedLat = min (max requiredLat (minLatency range))
                                      (maxLatency range)
                in  assert (adjustedLat > 0) $
                    if wLatency <= adjustedLat
                    then PartialWorker deltaYields
                    else let workers = withLimit $ wLatency `div` adjustedLat
                             limited = min workers (fromIntegral deltaYields)
                         in ManyWorkers (fromIntegral limited) deltaYields
            else
                let expectedDuration = fromIntegral effectiveYields * targetLat
                    sleepTime = expectedDuration - svarElapsed
                    maxSleepTime = maxLatency range - wLatency
                    s = min sleepTime maxSleepTime
                in assert (sleepTime >= 0) $
                    -- if s is less than 0 it means our maxSleepTime is less
                    -- than the worker latency.
                    if s > 0 then BlockWait s else ManyWorkers 1 (Count 0)
    where
        withLimit n =
            case workerLimit of
                Unlimited -> n
                Limited x -> min n (fromIntegral x)

isBeyondMaxRate :: SVar t m a -> YieldRateInfo -> IO Bool
isBeyondMaxRate sv yinfo = do
    (count, tstamp, wLatency) <- getWorkerLatency yinfo
    now <- getTime Monotonic
    let duration = fromRelTime64 $ diffAbsTime64 now tstamp
    let targetLat = svarLatencyTarget yinfo
    gainLoss <- readIORef (svarGainedLostYields yinfo)
    let work = estimateWorkers (maxWorkerLimit sv) count gainLoss duration
                               wLatency targetLat (svarLatencyRange yinfo)
    cnt <- readIORef $ workerCount sv
    return $ case work of
        -- XXX set the worker's maxYields or polling interval based on yields
        PartialWorker _yields -> cnt > 1
        ManyWorkers n _ -> cnt > n
        BlockWait _ -> True

-- XXX we should do rate control periodically based on the total yields rather
-- than based on the worker local yields as other workers may have yielded more
-- and we should stop based on the aggregate yields. However, latency update
-- period can be based on individual worker yields.
{-# NOINLINE checkRatePeriodic #-}
checkRatePeriodic :: SVar t m a
                  -> YieldRateInfo
                  -> WorkerInfo
                  -> Count
                  -> IO Bool
checkRatePeriodic sv yinfo winfo ycnt = do
    i <- readIORef (workerPollingInterval yinfo)
    -- XXX use generation count to check if the interval has been updated
    if i /= 0 && (ycnt `mod` i) == 0
    then do
        workerUpdateLatency yinfo winfo
        -- XXX not required for parallel streams
        isBeyondMaxRate sv yinfo
    else return False

-- CAUTION! this also updates the yield count and therefore should be called
-- only when we are actually yielding an element.
{-# NOINLINE workerRateControl #-}
workerRateControl :: SVar t m a -> YieldRateInfo -> WorkerInfo -> IO Bool
workerRateControl sv yinfo winfo = do
    cnt <- updateYieldCount winfo
    beyondMaxRate <- checkRatePeriodic sv yinfo winfo cnt
    return $ not (isBeyondMaxYield cnt winfo || beyondMaxRate)

-------------------------------------------------------------------------------
-- Send a yield event
-------------------------------------------------------------------------------

-- XXX we should do rate control here but not latency update in case of ahead
-- streams. latency update must be done when we yield directly to outputQueue
-- or when we yield to heap.
--
-- returns whether the worker should continue (True) or stop (False).
{-# INLINE sendYield #-}
sendYield :: SVar t m a -> Maybe WorkerInfo -> ChildEvent a -> IO Bool
sendYield sv mwinfo msg = do
    oldlen <- send sv msg
    let limit = maxBufferLimit sv
    bufferSpaceOk <- case limit of
            Unlimited -> return True
            Limited lim -> do
                active <- readIORef (workerCount sv)
                return $ (oldlen + 1) < (fromIntegral lim - active)
    rateLimitOk <-
        case mwinfo of
            Just winfo ->
                case yieldRateInfo sv of
                    Nothing -> return True
                    Just yinfo -> workerRateControl sv yinfo winfo
            Nothing -> return True
    return $ bufferSpaceOk && rateLimitOk

-------------------------------------------------------------------------------
-- Send a Stop event
-------------------------------------------------------------------------------

{-# INLINE workerStopUpdate #-}
workerStopUpdate :: WorkerInfo -> YieldRateInfo -> IO ()
workerStopUpdate winfo info = do
    i <- readIORef (workerPollingInterval info)
    when (i /= 0) $ workerUpdateLatency info winfo

{-# INLINABLE sendStop #-}
sendStop :: SVar t m a -> Maybe WorkerInfo -> IO ()
sendStop sv mwinfo = do
    atomicModifyIORefCAS_ (workerCount sv) $ \n -> n - 1
    case (mwinfo, yieldRateInfo sv) of
      (Just winfo, Just info) ->
          workerStopUpdate winfo info
      _ ->
          return ()
    myThreadId >>= \tid -> void $ send sv (ChildStop tid Nothing)

-- {-# NOINLINE sendStopToProducer #-}
sendStopToProducer :: MonadIO m => SVar t m a -> m ()
sendStopToProducer sv = liftIO $ do
    tid <- myThreadId
    void $ sendToProducer sv (ChildStop tid Nothing)

-------------------------------------------------------------------------------
-- Send exceptions
-------------------------------------------------------------------------------

{-# NOINLINE handleFoldException #-}
handleFoldException :: SVar t m a -> SomeException -> IO ()
handleFoldException sv e = do
    tid <- myThreadId
    void $ sendToProducer sv (ChildStop tid (Just e))

{-# NOINLINE handleChildException #-}
handleChildException :: SVar t m a -> SomeException -> IO ()
handleChildException sv e = do
    tid <- myThreadId
    void $ send sv (ChildStop tid (Just e))
