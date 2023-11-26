-- |
-- Module      : Streamly.Internal.Data.Stream.Concurrent.Channel.Dispatcher
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.Concurrent.Channel.Dispatcher
    (
    -- * Dispatching
      pushWorker
    , dispatchWorker
    , dispatchWorkerPaced
    , sendWorkerWait
    , startChannel
    , sendWorkerDelay
    , sendWorkerDelayPaced
    )
where

import Control.Concurrent (takeMVar, threadDelay)
import Control.Exception (assert)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromJust, fromMaybe)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Streamly.Internal.Control.Concurrent (MonadRunInIO)
import Streamly.Internal.Control.ForkLifted (doFork)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS_, storeLoadBarrier)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (MicroSecond64(..), diffAbsTime64, fromRelTime64, toRelTime64)

import Streamly.Internal.Data.Stream.Concurrent.Channel.Type
import Streamly.Internal.Data.Channel.Dispatcher
import Streamly.Internal.Data.Channel.Types
import Streamly.Internal.Data.Channel.Worker

-------------------------------------------------------------------------------
-- Dispatching workers
-------------------------------------------------------------------------------

{-# NOINLINE pushWorker #-}
pushWorker :: MonadRunInIO m => Count -> Channel m a -> m ()
pushWorker yieldMax sv = do
    liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
    when (svarInspectMode sv)
        $ recordMaxWorkers (workerCount sv) (svarStats sv)
    -- This allocation matters when significant number of workers are being
    -- sent. We allocate it only when needed.
    winfo <-
        case yieldRateInfo sv of
            Nothing -> return Nothing
            Just _ -> liftIO $ do
                cntRef <- newIORef 0
                t <- getTime Monotonic
                lat <- newIORef (0, t)
                return $ Just WorkerInfo
                    { workerYieldMax = yieldMax
                    , workerYieldCount = cntRef
                    , workerLatencyStart = lat
                    }
    -- In case of lazy dispatch we dispatch workers only from the consumer
    -- thread. In that case it is ok to use addThread here as it is guaranteed
    -- that the thread will be added to the workerSet before the thread STOP
    -- event is processed, because we do both of these actions in the same
    -- consumer thread. However, in case of eager dispatch we may dispatch
    -- workers from workers, in which case the thread Stop even may get
    -- processed before the addThread occurs, so in that case we have to use
    -- modifyThread which performs a toggle rather than adding or deleting.
    --
    -- XXX We can use addThread or modThread based on eager flag.
    doFork (workLoop sv winfo) (svarMrun sv) exception >>= modThread

    where

    modThread = modifyThread (workerThreads sv) (outputDoorBell sv)
    exception = handleChildException (outputQueue sv) (outputDoorBell sv)

-- | Determine the maximum number of workers required based on 'maxWorkerLimit'
-- and 'remainingWork'.
{-# INLINE getEffectiveWorkerLimit #-}
getEffectiveWorkerLimit :: MonadIO m => Channel m a -> m Limit
getEffectiveWorkerLimit sv = do
    let workerLimit = maxWorkerLimit sv
    case remainingWork sv of
        Nothing -> return workerLimit
        Just ref -> do
            n <- liftIO $ readIORef ref
            case yieldRateInfo sv of
                Just _ -> return workerLimit
                Nothing ->
                    return $
                        case workerLimit of
                            Unlimited -> Limited (fromIntegral n)
                            Limited lim -> Limited $ min lim (fromIntegral n)

-- | Determine whether the active threads are more than the max threads we are
-- allowed to dispatch.
{-# INLINE checkMaxThreads #-}
checkMaxThreads :: MonadIO m => Int -> Channel m a -> m Bool
checkMaxThreads active sv = do
    -- Note that we may deadlock if the previous workers (tasks in the
    -- stream) wait/depend on the future workers (tasks in the stream)
    -- executing. In that case we should either configure the maxWorker
    -- count to higher or use parallel style instead of ahead or async
    -- style.
    limit <- getEffectiveWorkerLimit sv
    return
        $ case limit of
            Unlimited -> True
            -- Note that the use of remainingWork and workerCount is not
            -- atomic and the counts may even have changed between reading
            -- and using them here, so this is just approximate logic and
            -- we cannot rely on it for correctness. We may actually
            -- dispatch more workers than required.
            Limited lim -> fromIntegral lim > active

-- | Determine whether we would exceed max buffer if we dispatch more workers
-- based on the current outputQueue size and active workers.
{-# INLINE checkMaxBuffer #-}
checkMaxBuffer :: MonadIO m => Int -> Channel m a -> m Bool
checkMaxBuffer active sv = do
    let limit = maxBufferLimit sv
    case limit of
        Unlimited -> return True
        Limited lim -> do
            (_, n) <- liftIO $ readIORef (outputQueue sv)
            return $ fromIntegral lim > n + active

dispatchWorker :: MonadRunInIO m =>
    Count -> Channel m a -> m Bool
dispatchWorker yieldCount sv = do
    -- XXX in case of Ahead streams we should not send more than one worker
    -- when the work queue is done but heap is not done.
    -- XXX Should we have a single abstraction for checking q and
    -- work instead checking the two separately?
    done <- liftIO $ isWorkDone sv
    -- Note, "done" may not mean that the work is actually finished if there
    -- are workers active, because there may be a worker which has not yet
    -- queued the leftover work.
    if not done
    then do
        qDone <- liftIO $ isQueueDone sv
        -- This count may be more until the sendStop events are processed.
        active <- liftIO $ readIORef $ workerCount sv
        when (active < 0) $ error "dispatchWorker active negative"
        if not qDone
        then do
            -- XXX for ahead streams shall we take the heap yields into account
            -- for controlling the dispatch? We should not dispatch if the heap
            -- has already got the limit covered.
            r <- checkMaxThreads active sv
            if r
            then do
                r1 <- checkMaxBuffer active sv
                if r1
                then pushWorker yieldCount sv >> return True
                else return False
            else return False
        else do
            when (active <= 0) $ do
                r <- liftIO $ isWorkDone sv
                when (not r) $ pushWorker 0 sv
            return False
    else return False

-- XXX in case of ahead style stream we need to take the heap size into account
-- because we return the workers on the basis of that which causes a condition
-- where we keep dispatching and they keep returning. So we must have exactly
-- the same logic for not dispatching and for returning.
--
-- Returns:
-- True: can dispatch more
-- False: full, no more dispatches
dispatchWorkerPaced :: MonadRunInIO m =>
    Channel m a -> m Bool
dispatchWorkerPaced sv = do
    let yinfo = fromJust $ yieldRateInfo sv
    (svarYields, svarElapsed, wLatency) <- do
        now <- liftIO $ getTime Monotonic
        (yieldCount, baseTime, lat) <-
            liftIO
                $ collectLatency
                    (svarInspectMode sv) (svarStats sv) yinfo False
        let elapsed = fromRelTime64 $ diffAbsTime64 now baseTime
        let latency =
                if lat == 0
                then fromMaybe lat (workerBootstrapLatency yinfo)
                else lat

        return (yieldCount, elapsed, latency)

    if wLatency == 0
    -- Need to measure the latency with a single worker before we can perform
    -- any computation.
    then return False
    else do
        let workerLimit = maxWorkerLimit sv
        let targetLat = svarLatencyTarget yinfo
        let range = svarLatencyRange yinfo
        gainLoss <- liftIO $ readIORef (svarGainedLostYields yinfo)
        let work = estimateWorkers workerLimit svarYields gainLoss svarElapsed
                                   wLatency targetLat range

        -- XXX we need to take yieldLimit into account here. If we are at the
        -- end of the limit as well as the time, we should not be sleeping.
        -- If we are not actually planning to dispatch any more workers we need
        -- to take that in account.
        case work of
            BlockWait s -> do
                assert (s >= 0) (return ())
                -- XXX note that when we return from here we will block waiting
                -- for the result from the existing worker. If that takes too
                -- long we won't be able to send another worker until the
                -- result arrives.
                --
                -- Sleep only if there are no active workers, otherwise we will
                -- defer the output of those. Note we cannot use workerCount
                -- here as it is not a reliable way to ensure there are
                -- definitely no active workers. When workerCount is 0 we may
                -- still have a Stop event waiting in the outputQueue.
                done <- allThreadsDone (workerThreads sv)
                when done $ void $ do
                    let us = fromRelTime64 (toRelTime64 s) :: MicroSecond64
                    liftIO $ threadDelay (fromIntegral us)
                    dispatchWorker 1 sv
                return False
            PartialWorker yields -> do
                assert (yields > 0) (return ())
                updateGainedLostYields yinfo yields

                done <- allThreadsDone (workerThreads sv)
                when done $ void $ dispatchWorker yields sv
                return False
            ManyWorkers netWorkers yields -> do
                assert (netWorkers >= 1) (return ())
                assert (yields >= 0) (return ())
                updateGainedLostYields yinfo yields

                let periodRef = workerPollingInterval yinfo
                    ycnt = max 1 $ yields `div` fromIntegral netWorkers
                    period = min ycnt (fromIntegral magicMaxBuffer)

                old <- liftIO $ readIORef periodRef
                when (period < old) $
                    liftIO $ writeIORef periodRef period

                cnt <- liftIO $ readIORef $ workerCount sv
                if cnt < netWorkers
                then do
                    let total = netWorkers - cnt
                        batch = max 1 $ fromIntegral $
                                    minThreadDelay `div` targetLat
                    -- XXX stagger the workers over a period?
                    -- XXX cannot sleep, as that would mean we cannot process
                    -- the outputs. need to try a different mechanism to
                    -- stagger.
                    -- when (total > batch) $
                       -- liftIO $ threadDelay $ nanoToMicroSecs minThreadDelay
                    dispatchN (min total batch)
                else return False

    where

    updateGainedLostYields yinfo yields = do
        let buf = fromIntegral $ svarRateBuffer yinfo
        when (yields /= 0 && abs yields > buf) $ do
            let delta =
                   if yields > 0
                   then yields - buf
                   else yields + buf
            liftIO $ modifyIORef (svarGainedLostYields yinfo) (+ delta)

    dispatchN n =
        if n == 0
        then return True
        else do
            r <- dispatchWorker 0 sv
            if r
            then dispatchN (n - 1)
            else return False

{-# NOINLINE sendWorkerWait #-}
sendWorkerWait
    :: MonadIO m
    => Bool
    -> (Channel m a -> IO ())
    -> (Channel m a -> m Bool)
    -> Channel m a
    -> m ()
sendWorkerWait eagerEval delay dispatch sv = go

    where

    go = do

        -- Note that we are guaranteed to have at least one outstanding worker
        -- when we enter this function. So if we sleep we are guaranteed to be
        -- woken up by an outputDoorBell, when the worker exits.

        liftIO $ delay sv
        (_, n) <- liftIO $ readIORef (outputQueue sv)
        when (n <= 0 || eagerEval) $ do
            -- The queue may be empty temporarily if the worker has dequeued
            -- the work item but has not enqueued the remaining part yet. For
            -- the same reason, a worker may come back if it tries to dequeue
            -- and finds the queue empty, even though the whole work has not
            -- finished yet.

            -- If we find that the queue is empty, but it may be empty
            -- temporarily, when we checked it. If that's the case we might
            -- sleep indefinitely unless the active workers produce some
            -- output. We may deadlock specially if the otuput from the active
            -- workers depends on the future workers that we may never send.
            -- So in case the queue was temporarily empty set a flag to inform
            -- the enqueue to send us a doorbell.

            -- Note that this is just a best effort mechanism to avoid a
            -- deadlock. Deadlocks may still happen if for some weird reason
            -- the consuming computation shares an MVar or some other resource
            -- with the producing computation and gets blocked on that resource
            -- and therefore cannot do any pushworker to add more threads to
            -- the producer. In such cases the programmer should use a parallel
            -- style so that all the producers are scheduled immediately and
            -- unconditionally. We can also use a separate monitor thread to
            -- push workers instead of pushing them from the consumer, but then
            -- we are no longer using pull based concurrency rate adaptation.
            --
            -- XXX update this in the tutorial.
            --
            -- Having pending active workers does not mean that we are
            -- guaranteed to be woken up if we sleep. In case of Ahead streams,
            -- there may be queued items in the heap even though the
            -- outputQueue is empty, and we may have active workers which are
            -- deadlocked on those items to be processed by the consumer. We
            -- should either guarantee that any worker, before returning,
            -- clears the heap or we send a worker to clear it. Normally we
            -- always send a worker if no output is seen, but if the thread
            -- limit is reached or we are using pacing then we may not send a
            -- worker. See the concurrentApplication test in the tests, that
            -- test case requires at least one yield from the producer to not
            -- deadlock, if the last workers output is stuck in the heap then
            -- this test fails.  This problem can be extended to n threads when
            -- the consumer may depend on the evaluation of next n items in the
            -- producer stream.

            -- register for the outputDoorBell before we check the queue so
            -- that if we sleep because the queue was empty we are guaranteed
            -- to get a doorbell on the next enqueue.

            liftIO $ atomicModifyIORefCAS_ (doorBellOnWorkQ sv) $ const True
            liftIO storeLoadBarrier
            canDoMore <- dispatch sv

            -- XXX test for the case when we miss sending a worker when the
            -- worker count is more than 1500.
            --
            -- XXX Assert here that if the heap is not empty then there is at
            -- least one outstanding worker. Otherwise we could be sleeping
            -- forever.

            if canDoMore
            then go
            else do
                liftIO
                    $ withDiagMVar
                        (svarInspectMode sv)
                        (dumpSVar sv)
                        "sendWorkerWait: nothing to do"
                    $ takeMVar (outputDoorBell sv)
                (_, len) <- liftIO $ readIORef (outputQueue sv)
                if len <= 0
                then go
                else
                    liftIO
                        $ atomicModifyIORefCAS_ (doorBellOnWorkQ sv)
                        $ const False

-- | Start the evaluation of the channel's work queue by kicking off a worker.
-- Note: Work queue must not be empty otherwise the worker will exit without
-- doing anything.
startChannel :: MonadRunInIO m =>
    Channel m a -> m ()
startChannel chan = do
    case yieldRateInfo chan of
        Nothing -> pushWorker 0 chan
        Just yinfo  ->
            if svarLatencyTarget yinfo == maxBound
            then liftIO $ threadDelay maxBound
            else pushWorker 1 chan

sendWorkerDelayPaced :: Channel m a -> IO ()
sendWorkerDelayPaced _ = return ()

sendWorkerDelay :: Channel m a -> IO ()
sendWorkerDelay _sv =
    -- XXX we need a better way to handle this than hardcoded delays. The
    -- delays may be different for different systems.
    -- If there is a usecase where this is required we can create a combinator
    -- to set it as a config in the state.
    {-
  do
    ncpu <- getNumCapabilities
    if ncpu <= 1
    then
        if (svarStyle sv == AheadVar)
        then threadDelay 100
        else threadDelay 25
    else
        if (svarStyle sv == AheadVar)
        then threadDelay 100
        else threadDelay 10
    -}
    return ()
