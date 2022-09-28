-- |
-- Module      : Streamly.Internal.Data.SVar.Dispatch
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.SVar.Dispatch
    (
    -- * Latency collection
      collectLatency

    -- * Diagnostics
    , withDiagMVar
    , dumpSVar
    , printSVar

    -- * Thread accounting
    , delThread
    , modifyThread
    , allThreadsDone

    -- * Dispatching
    , recordMaxWorkers
    , pushWorker
    , pushWorkerPar
    , dispatchWorker
    , dispatchWorkerPaced
    , sendWorkerWait
    , sendFirstWorker
    , sendWorkerDelay
    , sendWorkerDelayPaced
    )
where

#include "inline.hs"

import Control.Concurrent (takeMVar, tryReadMVar, ThreadId, threadDelay)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Exception
       (assert, catches, throwIO, Handler(..), BlockedIndefinitelyOnMVar(..),
        BlockedIndefinitelyOnSTM(..))
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromJust, fromMaybe)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Streamly.Internal.Control.Concurrent (MonadAsync, askRunInIO)
import Streamly.Internal.Control.ForkLifted (doFork)
import Streamly.Internal.Data.Atomics
       (atomicModifyIORefCAS, atomicModifyIORefCAS_, writeBarrier,
        storeLoadBarrier)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), MicroSecond64(..), diffAbsTime64,
        fromRelTime64, toRelTime64, showNanoSecond64, showRelTime64)
import System.IO (hPutStrLn, stderr)

import qualified Data.Heap as H
import qualified Data.Set as S

import Streamly.Internal.Data.SVar.Type
import Streamly.Internal.Data.SVar.Worker

-------------------------------------------------------------------------------
-- Worker latency data processing
-------------------------------------------------------------------------------

-- Every once in a while workers update the latencies and check the yield rate.
-- They return if we are above the expected yield rate. If we check too often
-- it may impact performance, if we check less often we may have a stale
-- picture. We update every minThreadDelay but we translate that into a yield
-- count based on latency so that the checking overhead is little.
--
-- XXX use a generation count to indicate that the value is updated. If the
-- value is updated an existing worker must check it again on the next yield.
-- Otherwise it is possible that we may keep updating it and because of the mod
-- worker keeps skipping it.
updateWorkerPollingInterval :: YieldRateInfo -> NanoSecond64 -> IO ()
updateWorkerPollingInterval yinfo latency = do
    let periodRef = workerPollingInterval yinfo
        cnt = max 1 $ minThreadDelay `div` latency
        period = min cnt (fromIntegral magicMaxBuffer)

    writeIORef periodRef (fromIntegral period)

{-# INLINE recordMinMaxLatency #-}
recordMinMaxLatency :: SVar t m a -> NanoSecond64 -> IO ()
recordMinMaxLatency sv new = do
    let ss = svarStats sv
    minLat <- readIORef (minWorkerLatency ss)
    when (new < minLat || minLat == 0) $
        writeIORef (minWorkerLatency ss) new

    maxLat <- readIORef (maxWorkerLatency ss)
    when (new > maxLat) $ writeIORef (maxWorkerLatency ss) new

recordAvgLatency :: SVar t m a -> (Count, NanoSecond64) -> IO ()
recordAvgLatency sv (count, time) = do
    let ss = svarStats sv
    modifyIORef (avgWorkerLatency ss) $
        \(cnt, t) -> (cnt + count, t + time)

-- Pour the pending latency stats into a collection bucket
{-# INLINE collectWorkerPendingLatency #-}
collectWorkerPendingLatency
    :: IORef (Count, Count, NanoSecond64)
    -> IORef (Count, Count, NanoSecond64)
    -> IO (Count, Maybe (Count, NanoSecond64))
collectWorkerPendingLatency cur col = do
    (fcount, count, time) <- atomicModifyIORefCAS cur $ \v -> ((0,0,0), v)

    (fcnt, cnt, t) <- readIORef col
    let totalCount = fcnt + fcount
        latCount   = cnt + count
        latTime    = t + time
    writeIORef col (totalCount, latCount, latTime)

    assert (latCount == 0 || latTime /= 0) (return ())
    let latPair =
            if latCount > 0 && latTime > 0
            then Just (latCount, latTime)
            else Nothing
    return (totalCount, latPair)

{-# INLINE shouldUseCollectedBatch #-}
shouldUseCollectedBatch
    :: Count
    -> NanoSecond64
    -> NanoSecond64
    -> NanoSecond64
    -> Bool
shouldUseCollectedBatch collectedYields collectedTime newLat prevLat =
    let r = fromIntegral newLat / fromIntegral prevLat :: Double
    in     (collectedYields > fromIntegral magicMaxBuffer)
        || (collectedTime > minThreadDelay)
        || (prevLat > 0 && (r > 2 || r < 0.5))
        || (prevLat == 0)

-- Returns a triple, (1) yield count since last collection, (2) the base time
-- when we started counting, (3) average latency in the last measurement
-- period. The former two are used for accurate measurement of the going rate
-- whereas the average is used for future estimates e.g. how many workers
-- should be maintained to maintain the rate.
-- CAUTION! keep it in sync with getWorkerLatency
collectLatency :: SVar t m a
               -> YieldRateInfo
               -> Bool
               -> IO (Count, AbsTime, NanoSecond64)
collectLatency sv yinfo drain = do
    let cur      = workerPendingLatency yinfo
        col      = workerCollectedLatency yinfo
        longTerm = svarAllTimeLatency yinfo
        measured = workerMeasuredLatency yinfo

    (newCount, newLatPair) <- collectWorkerPendingLatency cur col
    (lcount, ltime) <- readIORef longTerm
    prevLat <- readIORef measured

    let newLcount = lcount + newCount
        retWith lat = return (newLcount, ltime, lat)

    case newLatPair of
        Nothing -> retWith prevLat
        Just (count, time) -> do
            let newLat = time `div` fromIntegral count
            when (svarInspectMode sv) $ recordMinMaxLatency sv newLat
            -- When we have collected a significant sized batch we compute the
            -- new latency using that batch and return the new latency,
            -- otherwise we return the previous latency derived from the
            -- previous batch.
            if shouldUseCollectedBatch newCount time newLat prevLat || drain
            then do
                -- XXX make this NOINLINE?
                updateWorkerPollingInterval yinfo (max newLat prevLat)
                when (svarInspectMode sv) $ recordAvgLatency sv (count, time)
                writeIORef col (0, 0, 0)
                writeIORef measured ((prevLat + newLat) `div` 2)
                modifyIORef longTerm $ \(_, t) -> (newLcount, t)
                retWith newLat
            else retWith prevLat

-------------------------------------------------------------------------------
-- Dumping the SVar for debug/diag
-------------------------------------------------------------------------------

dumpSVarStats :: SVar t m a -> SVarStats -> SVarStyle -> IO String
dumpSVarStats sv ss style = do
    case yieldRateInfo sv of
        Nothing -> return ()
        Just yinfo -> do
            _ <- liftIO $ collectLatency sv yinfo True
            return ()

    dispatches <- readIORef $ totalDispatches ss
    maxWrk <- readIORef $ maxWorkers ss
    maxOq <- readIORef $ maxOutQSize ss
    maxHp <- readIORef $ maxHeapSize ss
    minLat <- readIORef $ minWorkerLatency ss
    maxLat <- readIORef $ maxWorkerLatency ss
    (avgCnt, avgTime) <- readIORef $ avgWorkerLatency ss
    (svarCnt, svarGainLossCnt, svarLat) <- case yieldRateInfo sv of
        Nothing -> return (0, 0, 0)
        Just yinfo -> do
            (cnt, startTime) <- readIORef $ svarAllTimeLatency yinfo
            if cnt > 0
            then do
                t <- readIORef (svarStopTime ss)
                gl <- readIORef (svarGainedLostYields yinfo)
                case t of
                    Nothing -> do
                        now <- getTime Monotonic
                        let interval = diffAbsTime64 now startTime
                        return (cnt, gl, interval `div` fromIntegral cnt)
                    Just stopTime -> do
                        let interval = diffAbsTime64 stopTime startTime
                        return (cnt, gl, interval `div` fromIntegral cnt)
            else return (0, 0, 0)

    return $ unlines
        [ "total dispatches = " <> show dispatches
        , "max workers = " <> show maxWrk
        , "max outQSize = " <> show maxOq
            <> (if style == AheadVar
               then "\nheap max size = " <> show maxHp
               else "")
            <> (if minLat > 0
               then "\nmin worker latency = " <> showNanoSecond64 minLat
               else "")
            <> (if maxLat > 0
               then "\nmax worker latency = " <> showNanoSecond64 maxLat
               else "")
            <> (if avgCnt > 0
                then let lat = avgTime `div` fromIntegral avgCnt
                     in "\navg worker latency = " <> showNanoSecond64 lat
                else "")
            <> (if svarLat > 0
               then "\nSVar latency = " <> showRelTime64 svarLat
               else "")
            <> (if svarCnt > 0
               then "\nSVar yield count = " <> show svarCnt
               else "")
            <> (if svarGainLossCnt > 0
               then "\nSVar gain/loss yield count = " <> show svarGainLossCnt
               else "")
        ]

{-# NOINLINE dumpSVar #-}
dumpSVar :: SVar t m a -> IO String
dumpSVar sv = do
    (oqList, oqLen) <- readIORef $ outputQueue sv
    db <- tryReadMVar $ outputDoorBell sv
    aheadDump <-
        if svarStyle sv == AheadVar
        then do
            (oheap, oheapSeq) <- readIORef $ outputHeap sv
            (wq, wqSeq) <- readIORef $ aheadWorkQueue sv
            return $ unlines
                [ "heap length = " <> show (H.size oheap)
                , "heap seqeunce = " <> show oheapSeq
                , "work queue length = " <> show (length wq)
                , "work queue sequence = " <> show wqSeq
                ]
        else return []

    let style = svarStyle sv
    waiting <-
        if style /= ParallelVar
        then readIORef $ needDoorBell sv
        else return False
    rthread <- readIORef $ workerThreads sv
    workers <- readIORef $ workerCount sv
    stats <- dumpSVarStats sv (svarStats sv) (svarStyle sv)

    return $ unlines
        [
          "Creator tid = " <> show (svarCreator sv),
          "style = " <> show (svarStyle sv)
        , "---------CURRENT STATE-----------"
        , "outputQueue length computed  = " <> show (length oqList)
        , "outputQueue length maintained = " <> show oqLen
        -- XXX print the types of events in the outputQueue, first 5
        , "outputDoorBell = " <> show db
        ]
        <> aheadDump
        <> unlines
        [ "needDoorBell = " <> show waiting
        , "running threads = " <> show rthread
        -- XXX print the status of first 5 threads
        , "running thread count = " <> show workers
        ]
        <> "---------STATS-----------\n"
        <> stats

-- MVar diagnostics has some overhead - around 5% on AsyncT null benchmark, we
-- can keep it on in production to debug problems quickly if and when they
-- happen, but it may result in unexpected output when threads are left hanging
-- until they are GCed because the consumer went away.

{-# NOINLINE mvarExcHandler #-}
mvarExcHandler :: SVar t m a -> String -> BlockedIndefinitelyOnMVar -> IO ()
mvarExcHandler sv label e@BlockedIndefinitelyOnMVar = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ label <> " " <> "BlockedIndefinitelyOnMVar\n" <> svInfo
    throwIO e

{-# NOINLINE stmExcHandler #-}
stmExcHandler :: SVar t m a -> String -> BlockedIndefinitelyOnSTM -> IO ()
stmExcHandler sv label e@BlockedIndefinitelyOnSTM = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ label <> " " <> "BlockedIndefinitelyOnSTM\n" <> svInfo
    throwIO e

withDiagMVar :: SVar t m a -> String -> IO () -> IO ()
withDiagMVar sv label action =
    if svarInspectMode sv
    then
        action `catches` [ Handler (mvarExcHandler sv label)
                         , Handler (stmExcHandler sv label)
                         ]
    else action

printSVar :: SVar t m a -> String -> IO ()
printSVar sv how = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ "\n" <> how <> "\n" <> svInfo

-------------------------------------------------------------------------------
-- Thread accounting
-------------------------------------------------------------------------------

-- Thread tracking is needed for two reasons:
--
-- 1) Killing threads on exceptions. Threads may not be left to go away by
-- themselves because they may run for significant times before going away or
-- worse they may be stuck in IO and never go away.
--
-- 2) To know when all threads are done and the stream has ended.

{-# NOINLINE addThread #-}
addThread :: MonadIO m => SVar t m a -> ThreadId -> m ()
addThread sv tid =
    liftIO $ modifyIORef (workerThreads sv) (S.insert tid)

-- This is cheaper than modifyThread because we do not have to send a
-- outputDoorBell This can make a difference when more workers are being
-- dispatched.
{-# INLINE delThread #-}
delThread :: MonadIO m => SVar t m a -> ThreadId -> m ()
delThread sv tid =
    liftIO $ modifyIORef (workerThreads sv) (S.delete tid)

-- If present then delete else add. This takes care of out of order add and
-- delete i.e. a delete arriving before we even added a thread.
-- This occurs when the forked thread is done even before the 'addThread' right
-- after the fork gets a chance to run.
{-# INLINE modifyThread #-}
modifyThread :: MonadIO m => SVar t m a -> ThreadId -> m ()
modifyThread sv tid = do
    changed <- liftIO $ atomicModifyIORefCAS (workerThreads sv) $ \old ->
        if S.member tid old
        then let new = S.delete tid old in (new, new)
        else let new = S.insert tid old in (new, old)
    when (null changed) $
         liftIO $ do
            writeBarrier
            void $ tryPutMVar (outputDoorBell sv) ()

-- | This is safe even if we are adding more threads concurrently because if
-- a child thread is adding another thread then anyway 'workerThreads' will
-- not be empty.
{-# INLINE allThreadsDone #-}
allThreadsDone :: MonadIO m => SVar t m a -> m Bool
allThreadsDone sv = liftIO $ S.null <$> readIORef (workerThreads sv)

-------------------------------------------------------------------------------
-- Dispatching workers
-------------------------------------------------------------------------------

{-# NOINLINE recordMaxWorkers #-}
recordMaxWorkers :: MonadIO m => SVar t m a -> m ()
recordMaxWorkers sv = liftIO $ do
    active <- readIORef (workerCount sv)
    maxWrk <- readIORef (maxWorkers $ svarStats sv)
    when (active > maxWrk) $ writeIORef (maxWorkers $ svarStats sv) active
    modifyIORef (totalDispatches $ svarStats sv) (+1)

{-# NOINLINE pushWorker #-}
pushWorker :: MonadAsync m => Count -> SVar t m a -> m ()
pushWorker yieldMax sv = do
    liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
    when (svarInspectMode sv) $ recordMaxWorkers sv
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
    doFork (workLoop sv winfo) (svarMrun sv) (handleChildException sv)
        >>= addThread sv

-- XXX we can push the workerCount modification in accountThread and use the
-- same pushWorker for Parallel case as well.
--
-- | In contrast to pushWorker which always happens only from the consumer
-- thread, a pushWorkerPar can happen concurrently from multiple threads on the
-- producer side. So we need to use a thread safe modification of
-- workerThreads. Alternatively, we can use a CreateThread event to avoid
-- using a CAS based modification.
{-# INLINE pushWorkerPar #-}
pushWorkerPar
    :: MonadAsync m
    => SVar t m a -> (Maybe WorkerInfo -> m ()) -> m ()
pushWorkerPar sv wloop =
    if svarInspectMode sv
    then forkWithDiag
    else doFork (wloop Nothing) (svarMrun sv) (handleChildException sv)
            >>= modifyThread sv

    where

    {-# NOINLINE forkWithDiag #-}
    forkWithDiag = do
        -- We do not use workerCount in case of ParallelVar but still there is
        -- no harm in maintaining it correctly.
        liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
        recordMaxWorkers sv
        -- This allocation matters when significant number of workers are being
        -- sent. We allocate it only when needed. The overhead increases by 4x.
        winfo <-
            case yieldRateInfo sv of
                Nothing -> return Nothing
                Just _ -> liftIO $ do
                    cntRef <- newIORef 0
                    t <- getTime Monotonic
                    lat <- newIORef (0, t)
                    return $ Just WorkerInfo
                        { workerYieldMax = 0
                        , workerYieldCount = cntRef
                        , workerLatencyStart = lat
                        }

        doFork (wloop winfo) (svarMrun sv) (handleChildException sv)
            >>= modifyThread sv

-- Returns:
-- True: can dispatch more
-- False: cannot dispatch any more
dispatchWorker :: MonadAsync m => Count -> SVar t m a -> m Bool
dispatchWorker yieldCount sv = do
    let workerLimit = maxWorkerLimit sv
    -- XXX in case of Ahead streams we should not send more than one worker
    -- when the work queue is done but heap is not done.
    done <- liftIO $ isWorkDone sv
    -- Note, "done" may not mean that the work is actually finished if there
    -- are workers active, because there may be a worker which has not yet
    -- queued the leftover work.
    if not done
    then do
        qDone <- liftIO $ isQueueDone sv
        -- This count may not be accurate as it is decremented by the workers
        -- and we have no synchronization with that decrement.
        active <- liftIO $ readIORef $ workerCount sv
        if not qDone
        then do
            -- Note that we may deadlock if the previous workers (tasks in the
            -- stream) wait/depend on the future workers (tasks in the stream)
            -- executing. In that case we should either configure the maxWorker
            -- count to higher or use parallel style instead of ahead or async
            -- style.
            limit <- case remainingWork sv of
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

            -- XXX for ahead streams shall we take the heap yields into account
            -- for controlling the dispatch? We should not dispatch if the heap
            -- has already got the limit covered.
            let dispatch = pushWorker yieldCount sv >> return True
             in case limit of
                Unlimited -> dispatch
                -- Note that the use of remainingWork and workerCount is not
                -- atomic and the counts may even have changed between reading
                -- and using them here, so this is just approximate logic and
                -- we cannot rely on it for correctness. We may actually
                -- dispatch more workers than required.
                Limited lim | lim > fromIntegral active -> dispatch
                _ -> return False
        else do
            when (active <= 0) $ pushWorker 0 sv
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
dispatchWorkerPaced :: MonadAsync m => SVar t m a -> m Bool
dispatchWorkerPaced sv = do
    let yinfo = fromJust $ yieldRateInfo sv
    (svarYields, svarElapsed, wLatency) <- do
        now <- liftIO $ getTime Monotonic
        (yieldCount, baseTime, lat) <-
            liftIO $ collectLatency sv yinfo False
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
                done <- allThreadsDone sv
                when done $ void $ do
                    let us = fromRelTime64 (toRelTime64 s) :: MicroSecond64
                    liftIO $ threadDelay (fromIntegral us)
                    dispatchWorker 1 sv
                return False
            PartialWorker yields -> do
                assert (yields > 0) (return ())
                updateGainedLostYields yinfo yields

                done <- allThreadsDone sv
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
    :: MonadAsync m
    => (SVar t m a -> IO ())
    -> (SVar t m a -> m Bool)
    -> SVar t m a
    -> m ()
sendWorkerWait delay dispatch sv = do
    -- Note that we are guaranteed to have at least one outstanding worker when
    -- we enter this function. So if we sleep we are guaranteed to be woken up
    -- by an outputDoorBell, when the worker exits.

    liftIO $ delay sv
    (_, n) <- liftIO $ readIORef (outputQueue sv)
    when (n <= 0) $ do
        -- The queue may be empty temporarily if the worker has dequeued the
        -- work item but has not enqueued the remaining part yet. For the same
        -- reason, a worker may come back if it tries to dequeue and finds the
        -- queue empty, even though the whole work has not finished yet.

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
        -- Having pending active workers does not mean that we are guaranteed
        -- to be woken up if we sleep. In case of Ahead streams, there may be
        -- queued items in the heap even though the outputQueue is empty, and
        -- we may have active workers which are deadlocked on those items to be
        -- processed by the consumer. We should either guarantee that any
        -- worker, before returning, clears the heap or we send a worker to
        -- clear it. Normally we always send a worker if no output is seen, but
        -- if the thread limit is reached or we are using pacing then we may
        -- not send a worker. See the concurrentApplication test in the tests,
        -- that test case requires at least one yield from the producer to not
        -- deadlock, if the last workers output is stuck in the heap then this
        -- test fails.  This problem can be extended to n threads when the
        -- consumer may depend on the evaluation of next n items in the
        -- producer stream.

        -- register for the outputDoorBell before we check the queue so that if
        -- we sleep because the queue was empty we are guaranteed to get a
        -- doorbell on the next enqueue.

        liftIO $ atomicModifyIORefCAS_ (needDoorBell sv) $ const True
        liftIO storeLoadBarrier
        canDoMore <- dispatch sv

        -- XXX test for the case when we miss sending a worker when the worker
        -- count is more than 1500.
        --
        -- XXX Assert here that if the heap is not empty then there is at
        -- least one outstanding worker. Otherwise we could be sleeping
        -- forever.

        if canDoMore
        then sendWorkerWait delay dispatch sv
        else do
            liftIO $ withDiagMVar sv "sendWorkerWait: nothing to do"
                             $ takeMVar (outputDoorBell sv)
            (_, len) <- liftIO $ readIORef (outputQueue sv)
            when (len <= 0) $ sendWorkerWait delay dispatch sv

sendFirstWorker :: MonadAsync m => SVar t m a -> t m a -> m (SVar t m a)
sendFirstWorker sv m = do
    -- Note: We must have all the work on the queue before sending the
    -- pushworker, otherwise the pushworker may exit before we even get a
    -- chance to push.
    runIn <- askRunInIO
    liftIO $ enqueue sv (runIn, m)
    case yieldRateInfo sv of
        Nothing -> pushWorker 0 sv
        Just yinfo  ->
            if svarLatencyTarget yinfo == maxBound
            then liftIO $ threadDelay maxBound
            else pushWorker 1 sv
    return sv

sendWorkerDelayPaced :: SVar t m a -> IO ()
sendWorkerDelayPaced _ = return ()

sendWorkerDelay :: SVar t m a -> IO ()
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
