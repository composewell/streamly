-- |
-- Module      : Streamly.Internal.Data.Channel.Worker
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Collecting results from child workers in a streamed fashion

module Streamly.Internal.Data.Channel.Worker
    (
    -- ** Worker Rate Control
      Work (..)
    , estimateWorkers
    , isBeyondMaxRate
    , incrWorkerYieldCount

    -- ** Workers Sending Events
    , sendEvent
    , sendYield
    , sendStop
    , sendException
    )
where

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (MVar, tryPutMVar)
import Control.Exception (SomeException(..), assert)
import Control.Monad (when, void)
import Data.IORef (IORef, readIORef, writeIORef)
import Streamly.Internal.Data.Atomics
       (atomicModifyIORefCAS, atomicModifyIORefCAS_, writeBarrier)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), diffAbsTime64, fromRelTime64)

import Streamly.Internal.Data.Channel.Types

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

-- | Low level API to add an event on the channel's output queue. Atomically
-- adds the event to the queue and rings the doorbell if needed to wakeup the
-- consumer thread.
{-# INLINE sendEvent #-}
sendEvent ::
       IORef ([ChildEvent a], Int) -- ^ Queue where the event is added
    -> MVar () -- ^ Door bell to ring
    -> ChildEvent a -- ^ The event to be added
    -> IO Int -- ^ Length of the queue before adding this event
sendEvent q bell msg = do
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

-- | Describes how to pace the work based on current measurement estimates. If
-- the rate is higher than expected we may have to sleep for some time
-- ('BlockWait'), or send just one worker with limited yield count
-- ('PartialWorker') or send more than one workers with max yield count of each
-- limited to the total maximum target count.
data Work
    = BlockWait NanoSecond64 -- ^ Sleep required before next dispatch
    | PartialWorker Count -- ^ One worker is enough, total yields needed
    | ManyWorkers Int Count -- ^ Worker count, total yields needed overall
    deriving Show

-- | Another magic number! When we have to start more workers to cover up a
-- number of yields that we are lagging by then we cannot start one worker for
-- each yield because that may be a very big number and if the latency of the
-- workers is low these number of yields could be very high. We assume that we
-- run each extra worker for at least this much time.
rateRecoveryTime :: NanoSecond64
rateRecoveryTime = 1000000

-- CAUTION! keep it in sync with collectLatency

-- | Same as 'collectLatency' except that it does not update anything, this is
-- a readonly version of 'collectLatency'.
--
getWorkerLatency ::
       YieldRateInfo
    -> IO (Count, AbsTime, NanoSecond64)
    -- ^ (total yield count, base timestamp, 'workerMeasuredlatency')
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
-- XXX This should probably be inlined.
-- XXX Move this to the common Types module, since this is used by both
-- dispatcher and workers.

-- | Estimate how many workers and yield count ('Work') is required to maintian
-- the target yield rate of the channel.
--
-- This is used by the worker dispatcher to estimate how many workers to
-- dispatch. It is also used periodically by the workers to decide whether to
-- stop or continue working.
estimateWorkers
    :: Limit -- ^ Channel's max worker limit
    -> Count -- ^ Channel's yield count since start
    -> Count -- ^ 'svarGainedLostYields'
    -> NanoSecond64 -- ^ The up time of the channel
    -> NanoSecond64 -- ^ Current 'workerMeasuredLatency'
    -> NanoSecond64 -- ^ 'svarLatencyTarget'
    -> LatencyRange -- ^ 'svarLatencyRange'
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
        -- bandwidth is saturated, increasing the workers won't help. Also, if
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

-- | Using the channel worker latency and channel yield count stats from the
-- current measurement interval, estimate how many workers are needed to
-- maintain the target rate and compare that with current number of workers.
-- Returns true if we have have more than required workers.
isBeyondMaxRate ::
       Limit -- ^ Channel's max worker limit
    -> IORef Int -- ^ Current worker count
    -> YieldRateInfo -- ^ Channel's rate control info
    -> IO Bool -- ^ True if we are exceeding the specified rate
isBeyondMaxRate workerLimit workerCount rateInfo = do
    (count, tstamp, wLatency) <- getWorkerLatency rateInfo
    now <- getTime Monotonic
    let duration = fromRelTime64 $ diffAbsTime64 now tstamp
    let targetLat = svarLatencyTarget rateInfo
    gainLoss <- readIORef (svarGainedLostYields rateInfo)
    let work = estimateWorkers workerLimit count gainLoss duration
                               wLatency targetLat (svarLatencyRange rateInfo)
    cnt <- readIORef workerCount
    return $ case work of
        -- XXX set the worker's maxYields or polling interval based on yields
        PartialWorker _yields -> cnt > 1
        ManyWorkers n _ -> cnt > n
        BlockWait _ -> True

-- XXX we should do rate control periodically based on the total yields rather
-- than based on the worker local yields as other workers may have yielded more
-- and we should stop based on the aggregate yields. However, latency update
-- period can be based on individual worker yields.

-- | Update the worker latency and check the channel yield rate. Updates and
-- checks only at specified multiples of yield count to keep the overhead low.
{-# NOINLINE updateLatencyAndCheckRate #-}
updateLatencyAndCheckRate ::
       Limit
    -> IORef Int
    -> YieldRateInfo
    -> WorkerInfo
    -> Count
    -> IO Bool
updateLatencyAndCheckRate workerLimit workerCount rateInfo workerInfo ycnt = do
    i <- readIORef (workerPollingInterval rateInfo)
    -- XXX use generation count to check if the interval has been updated
    if i /= 0 && (ycnt `mod` i) == 0
    then do
        workerUpdateLatency rateInfo workerInfo
        -- XXX not required for parallel streams
        isBeyondMaxRate workerLimit workerCount rateInfo
    else return False

-- | Update the local yield count of the worker and check if:
--
-- * the channel yield rate is beyond max limit
-- * worker's yield count is beyond max limit
--
{-# NOINLINE incrWorkerYieldCount #-}
incrWorkerYieldCount ::
       Limit -- ^ Channel's max worker limit
    -> IORef Int -- ^ Current worker count
    -> YieldRateInfo -- ^ Channel's rate control info
    -> WorkerInfo -- ^ Worker's yield count info
    -> IO Bool -- ^ True means limits are ok and worker can continue
incrWorkerYieldCount workerLimit workerCount rateInfo workerInfo = do
    cnt <- updateYieldCount workerInfo
    beyondMaxRate <-
        updateLatencyAndCheckRate workerLimit workerCount rateInfo workerInfo cnt
    return $ not (isBeyondMaxYield cnt workerInfo || beyondMaxRate)

-------------------------------------------------------------------------------
-- Send a yield event
-------------------------------------------------------------------------------

-- XXX we should do rate control here but not latency update in case of ahead
-- streams. latency update must be done when we yield directly to outputQueue
-- or when we yield to heap.

-- | Add a 'ChildYield' event to the channel's output queue.
{-# INLINE sendYield #-}
sendYield ::
       Limit -- ^ Channel's max buffer limit
    -> Limit -- ^ Channel's max worker limit
    -> IORef Int -- ^ Current worker count
    -> Maybe YieldRateInfo -- ^ Channel's rate control info
    -> IORef ([ChildEvent a], Int) -- ^ Queue where the output is added
    -> MVar () -- ^ Door bell to ring
    -> Maybe WorkerInfo -- ^ Worker's yield count info
    -> a -- ^ The output to be sent
    -> IO Bool -- ^ True means worker is allowed to continue working
sendYield bufferLimit workerLimit workerCount rateInfo q bell workerInfo msg =
    do
    oldlen <- sendEvent q bell (ChildYield msg)
    bufferSpaceOk <-
        case bufferLimit of
            Unlimited -> return True
            Limited lim -> do
                active <- readIORef workerCount
                return $ (oldlen + 1) < (fromIntegral lim - active)
    rateLimitOk <-
        case workerInfo of
            Just winfo ->
                case rateInfo of
                    Nothing -> return True
                    Just yinfo ->
                        incrWorkerYieldCount workerLimit workerCount yinfo winfo
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

-- | Add a 'ChildStop' event to the channel's output queue.
{-# INLINABLE sendStop #-}
sendStop ::
       IORef Int -- ^ Channel's current worker count
    -> Maybe YieldRateInfo -- ^ Channel's rate control info
    -> IORef ([ChildEvent a], Int) -- ^ Queue where the stop event is added
    -> MVar () -- ^ Door bell to ring
    -> Maybe WorkerInfo -- ^ Worker's yield count info
    -> IO ()
sendStop workerCount rateInfo q bell workerInfo = do
    atomicModifyIORefCAS_ workerCount $ \n -> n - 1
    case (workerInfo, rateInfo) of
      (Just winfo, Just rinfo) ->
          workerStopUpdate winfo rinfo
      _ ->
          return ()
    myThreadId >>= \tid ->
        void $ sendEvent q bell (ChildStop tid Nothing)

-- | Add a 'ChildStop' event with exception to the channel's output queue.
{-# NOINLINE sendException #-}
sendException ::
       IORef ([ChildEvent a], Int) -- ^ Queue where the exception event is added
    -> MVar () -- ^ Door bell to ring
    -> SomeException -- ^ The exception to send
    -> IO ()
sendException q bell e = do
    tid <- myThreadId
    void $ sendEvent q bell (ChildStop tid (Just e))
