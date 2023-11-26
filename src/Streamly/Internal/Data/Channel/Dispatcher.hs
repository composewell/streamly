-- |
-- Module      : Streamly.Internal.Data.Channel.Dispatcher
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Channel.Dispatcher
    (
    -- * Latency collection
      minThreadDelay
    , collectLatency

    -- * Thread accounting
    , addThread
    , delThread
    , modifyThread
    , allThreadsDone
    , recordMaxWorkers

    -- * Diagnostics
    , dumpSVarStats
    )
where

import Data.Set (Set)
import Control.Concurrent (MVar, ThreadId)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Exception (assert)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (IORef, modifyIORef, readIORef, writeIORef)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS, writeBarrier)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units
       ( AbsTime, NanoSecond64(..), diffAbsTime64, showNanoSecond64
       , showRelTime64)

import qualified Data.Set as S

import Streamly.Internal.Data.Channel.Types

-------------------------------------------------------------------------------
-- Worker latency data processing
-------------------------------------------------------------------------------

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
recordMinMaxLatency :: SVarStats -> NanoSecond64 -> IO ()
recordMinMaxLatency ss new = do
    minLat <- readIORef (minWorkerLatency ss)
    when (new < minLat || minLat == 0) $
        writeIORef (minWorkerLatency ss) new

    maxLat <- readIORef (maxWorkerLatency ss)
    when (new > maxLat) $ writeIORef (maxWorkerLatency ss) new

recordAvgLatency :: SVarStats -> (Count, NanoSecond64) -> IO ()
recordAvgLatency ss (count, time) = do
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
collectLatency ::
       Bool
    -> SVarStats
    -> YieldRateInfo
    -> Bool
    -> IO (Count, AbsTime, NanoSecond64)
collectLatency inspecting ss yinfo drain = do
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
            when inspecting $ recordMinMaxLatency ss newLat
            -- When we have collected a significant sized batch we compute the
            -- new latency using that batch and return the new latency,
            -- otherwise we return the previous latency derived from the
            -- previous batch.
            if shouldUseCollectedBatch newCount time newLat prevLat || drain
            then do
                -- XXX make this NOINLINE?
                updateWorkerPollingInterval yinfo (max newLat prevLat)
                when inspecting $ recordAvgLatency ss (count, time)
                writeIORef col (0, 0, 0)
                writeIORef measured ((prevLat + newLat) `div` 2)
                modifyIORef longTerm $ \(_, t) -> (newLcount, t)
                retWith newLat
            else retWith prevLat

-------------------------------------------------------------------------------
-- Dumping the SVar for debug/diag
-------------------------------------------------------------------------------

dumpSVarStats :: Bool -> Maybe YieldRateInfo -> SVarStats -> IO String
dumpSVarStats inspecting rateInfo ss = do
    case rateInfo of
        Nothing -> return ()
        Just yinfo -> do
            _ <- liftIO $ collectLatency inspecting ss yinfo True
            return ()

    dispatches <- readIORef $ totalDispatches ss
    maxWrk <- readIORef $ maxWorkers ss
    maxOq <- readIORef $ maxOutQSize ss
    -- maxHp <- readIORef $ maxHeapSize ss
    minLat <- readIORef $ minWorkerLatency ss
    maxLat <- readIORef $ maxWorkerLatency ss
    (avgCnt, avgTime) <- readIORef $ avgWorkerLatency ss
    (svarCnt, svarGainLossCnt, svarLat) <- case rateInfo of
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
addThread :: MonadIO m => IORef (Set ThreadId) -> ThreadId -> m ()
addThread workerSet tid =
    liftIO $ modifyIORef workerSet (S.insert tid)

-- This is cheaper than modifyThread because we do not have to send a
-- outputDoorBell This can make a difference when more workers are being
-- dispatched.
{-# INLINE delThread #-}
delThread :: MonadIO m => IORef (Set ThreadId) -> ThreadId -> m ()
delThread workerSet tid =
    liftIO $ modifyIORef workerSet (S.delete tid)

-- If present then delete else add. This takes care of out of order add and
-- delete i.e. a delete arriving before we even added a thread.
-- This occurs when the forked thread is done even before the 'addThread' right
-- after the fork gets a chance to run.
{-# INLINE modifyThread #-}
modifyThread :: MonadIO m => IORef (Set ThreadId) -> MVar () -> ThreadId -> m ()
modifyThread workerSet bell tid = do
    changed <- liftIO $ atomicModifyIORefCAS workerSet $ \old ->
        if S.member tid old
        then let new = S.delete tid old in (new, new)
        else let new = S.insert tid old in (new, old)
    when (null changed) $
         liftIO $ do
            writeBarrier
            void $ tryPutMVar bell ()

-- | This is safe even if we are adding more threads concurrently because if
-- a child thread is adding another thread then anyway 'workerThreads' will
-- not be empty.
{-# INLINE allThreadsDone #-}
allThreadsDone :: MonadIO m => IORef (Set ThreadId) -> m Bool
allThreadsDone ref = liftIO $ S.null <$> readIORef ref

-------------------------------------------------------------------------------
-- Dispatching workers
-------------------------------------------------------------------------------

{-# NOINLINE recordMaxWorkers #-}
recordMaxWorkers :: MonadIO m => IORef Int -> SVarStats -> m ()
recordMaxWorkers countRef ss = liftIO $ do
    active <- readIORef countRef
    maxWrk <- readIORef (maxWorkers ss)
    when (active > maxWrk) $ writeIORef (maxWorkers ss) active
    modifyIORef (totalDispatches ss) (+1)
