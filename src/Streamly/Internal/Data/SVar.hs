{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Data.SVar
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.SVar
    (
      MonadAsync
    , SVarStyle (..)
    , SVarStopStyle (..)
    , SVar (..)

    -- State threaded around the stream
    , Limit (..)
    , State (streamVar)
    , defState
    , adaptState
    , getMaxThreads
    , setMaxThreads
    , getMaxBuffer
    , setMaxBuffer
    , getStreamRate
    , setStreamRate
    , setStreamLatency
    , getYieldLimit
    , setYieldLimit
    , getInspectMode
    , setInspectMode
    , recordMaxWorkers

    , cleanupSVar
    , cleanupSVarFromWorker

    -- SVar related
    , newAheadVar
    , newParallelVar
    , captureMonadState
    , RunInIO (..)

    , WorkerInfo (..)
    , YieldRateInfo (..)
    , ThreadAbort (..)
    , ChildEvent (..)
    , AheadHeapEntry (..)
    , send
    , sendToProducer
    , sendYield
    , sendStop
    , sendStopToProducer
    , enqueueLIFO
    , enqueueFIFO
    , enqueueAhead
    , reEnqueueAhead
    , pushWorkerPar
    , handleChildException
    , handleFoldException

    , queueEmptyAhead
    , dequeueAhead

    , HeapDequeueResult(..)
    , dequeueFromHeap
    , dequeueFromHeapSeq
    , requeueOnHeapTop
    , updateHeapSeq
    , withIORef
    , heapIsSane

    , Rate (..)
    , getYieldRateInfo
    , newSVarStats
    , collectLatency
    , workerUpdateLatency
    , isBeyondMaxRate
    , workerRateControl
    , updateYieldCount
    , decrementYieldLimit
    , incrementYieldLimit
    , decrementBufferLimit
    , incrementBufferLimit
    , postProcessBounded
    , postProcessPaced
    , readOutputQBounded
    , readOutputQPaced
    , readOutputQBasic
    , dispatchWorkerPaced
    , sendFirstWorker
    , delThread
    , modifyThread
    , doFork
    , fork
    , forkManaged

    , toStreamVar
    , SVarStats (..)
    , dumpSVar
    , printSVar
    , withDiagMVar
    )
where

import Control.Concurrent
       (ThreadId, myThreadId, threadDelay, throwTo, forkIO, killThread)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, tryPutMVar, takeMVar, tryTakeMVar, newMVar,
        tryReadMVar)
import Control.Exception
       (SomeException(..), catch, mask, assert, Exception, catches,
        throwIO, Handler(..), BlockedIndefinitelyOnMVar(..),
        BlockedIndefinitelyOnSTM(..))
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control
       (MonadBaseControl, control, StM, liftBaseDiscard)
import Streamly.Internal.Data.Atomics
       (atomicModifyIORefCAS, atomicModifyIORefCAS_, writeBarrier,
        storeLoadBarrier)
import Data.Concurrent.Queue.MichaelScott (LinkedQueue, pushL)
import Data.Functor (void)
import Data.Heap (Heap, Entry(..))
import Data.Int (Int64)
import Data.Kind (Type)
import Data.IORef
       (IORef, modifyIORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.Maybe (fromJust, fromMaybe)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup ((<>))
#endif
import Data.Set (Set)
import GHC.Conc (ThreadId(..))
import GHC.Exts
import GHC.IO (IO(..))
import System.IO (hPutStrLn, stderr)
import System.Mem.Weak (addFinalizer)

import Streamly.Internal.Data.Time.Clock (Clock(..), getTime)
import Streamly.Internal.Data.Time.Units
       (AbsTime, NanoSecond64(..), MicroSecond64(..), diffAbsTime64,
        fromRelTime64, toRelTime64, showNanoSecond64, showRelTime64)

import qualified Data.Heap as H
import qualified Data.Set                    as S

newtype Count = Count Int64
    deriving ( Eq
             , Read
             , Show
             , Enum
             , Bounded
             , Num
             , Real
             , Integral
             , Ord
             )

------------------------------------------------------------------------------
-- Parent child thread communication type
------------------------------------------------------------------------------

data ThreadAbort = ThreadAbort deriving Show

instance Exception ThreadAbort

-- | Events that a child thread may send to a parent thread.
data ChildEvent a =
      ChildYield a
    | ChildStop ThreadId (Maybe SomeException)

-- | Sorting out-of-turn outputs in a heap for Ahead style streams
data AheadHeapEntry (t :: (Type -> Type) -> Type -> Type) m a =
      AheadEntryNull
    | AheadEntryPure a
    | AheadEntryStream (RunInIO m, t m a)
#undef Type

------------------------------------------------------------------------------
-- State threaded around the monad for thread management
------------------------------------------------------------------------------

-- | Identify the type of the SVar. Two computations using the same style can
-- be scheduled on the same SVar.
data SVarStyle =
      AsyncVar             -- depth first concurrent
    | WAsyncVar            -- breadth first concurrent
    | ParallelVar          -- all parallel
    | AheadVar             -- Concurrent look ahead
    deriving (Eq, Show)

-- | An SVar or a Stream Var is a conduit to the output from multiple streams
-- running concurrently and asynchronously. An SVar can be thought of as an
-- asynchronous IO handle. We can write any number of streams to an SVar in a
-- non-blocking manner and then read them back at any time at any pace.  The
-- SVar would run the streams asynchronously and accumulate results. An SVar
-- may not really execute the stream completely and accumulate all the results.
-- However, it ensures that the reader can read the results at whatever paces
-- it wants to read. The SVar monitors and adapts to the consumer's pace.
--
-- An SVar is a mini scheduler, it has an associated workLoop that holds the
-- stream tasks to be picked and run by a pool of worker threads. It has an
-- associated output queue where the output stream elements are placed by the
-- worker threads. A outputDoorBell is used by the worker threads to intimate the
-- consumer thread about availability of new results in the output queue. More
-- workers are added to the SVar by 'fromStreamVar' on demand if the output
-- produced is not keeping pace with the consumer. On bounded SVars, workers
-- block on the output queue to provide throttling of the producer  when the
-- consumer is not pulling fast enough.  The number of workers may even get
-- reduced depending on the consuming pace.
--
-- New work is enqueued either at the time of creation of the SVar or as a
-- result of executing the parallel combinators i.e. '<|' and '<|>' when the
-- already enqueued computations get evaluated. See 'joinStreamVarAsync'.

-- We measure the individual worker latencies to estimate the number of workers
-- needed or the amount of time we have to sleep between dispatches to achieve
-- a particular rate when controlled pace mode it used.
data WorkerInfo = WorkerInfo
    { workerYieldMax   :: Count -- 0 means unlimited
    -- total number of yields by the worker till now
    , workerYieldCount    :: IORef Count
    -- yieldCount at start, timestamp
    , workerLatencyStart  :: IORef (Count, AbsTime)
    }

-- | Specifies the stream yield rate in yields per second (@Hertz@).
-- We keep accumulating yield credits at 'rateGoal'. At any point of time we
-- allow only as many yields as we have accumulated as per 'rateGoal' since the
-- start of time. If the consumer or the producer is slower or faster, the
-- actual rate may fall behind or exceed 'rateGoal'.  We try to recover the gap
-- between the two by increasing or decreasing the pull rate from the producer.
-- However, if the gap becomes more than 'rateBuffer' we try to recover only as
-- much as 'rateBuffer'.
--
-- 'rateLow' puts a bound on how low the instantaneous rate can go when
-- recovering the rate gap.  In other words, it determines the maximum yield
-- latency.  Similarly, 'rateHigh' puts a bound on how high the instantaneous
-- rate can go when recovering the rate gap.  In other words, it determines the
-- minimum yield latency. We reduce the latency by increasing concurrency,
-- therefore we can say that it puts an upper bound on concurrency.
--
-- If the 'rateGoal' is 0 or negative the stream never yields a value.
-- If the 'rateBuffer' is 0 or negative we do not attempt to recover.
--
-- /Since: 0.5.0 ("Streamly")/
--
-- @since 0.8.0
data Rate = Rate
    { rateLow    :: Double -- ^ The lower rate limit
    , rateGoal   :: Double -- ^ The target rate we want to achieve
    , rateHigh   :: Double -- ^ The upper rate limit
    , rateBuffer :: Int    -- ^ Maximum slack from the goal
    }

data LatencyRange = LatencyRange
    { minLatency :: NanoSecond64
    , maxLatency :: NanoSecond64
    } deriving Show

-- Rate control.
data YieldRateInfo = YieldRateInfo
    { svarLatencyTarget    :: NanoSecond64
    , svarLatencyRange     :: LatencyRange
    , svarRateBuffer       :: Int

    -- [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    , svarGainedLostYields :: IORef Count

    -- Actual latency/througput as seen from the consumer side, we count the
    -- yields and the time it took to generates those yields. This is used to
    -- increase or decrease the number of workers needed to achieve the desired
    -- rate. The idle time of workers is adjusted in this, so that we only
    -- account for the rate when the consumer actually demands data.
    -- XXX interval latency is enough, we can move this under diagnostics build
    -- [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    , svarAllTimeLatency :: IORef (Count, AbsTime)

    -- XXX Worker latency specified by the user to be used before the first
    -- actual measurement arrives. Not yet implemented
    , workerBootstrapLatency :: Maybe NanoSecond64

    -- After how many yields the worker should update the latency information.
    -- If the latency is high, this count is kept lower and vice-versa.  XXX If
    -- the latency suddenly becomes too high this count may remain too high for
    -- long time, in such cases the consumer can change it.
    -- 0 means no latency computation
    -- XXX this is derivable from workerMeasuredLatency, can be removed.
    -- [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    , workerPollingInterval :: IORef Count

    -- This is in progress latency stats maintained by the workers which we
    -- empty into workerCollectedLatency stats at certain intervals - whenever
    -- we process the stream elements yielded in this period. The first count
    -- is all yields, the second count is only those yields for which the
    -- latency was measured to be non-zero (note that if the timer resolution
    -- is low the measured latency may be zero e.g. on JS platform).
    -- [LOCKING] Locked access. Modified by the consumer thread as well as
    -- worker threads. Workers modify it periodically based on
    -- workerPollingInterval and not on every yield to reduce the locking
    -- overhead.
    -- (allYieldCount, yieldCount, timeTaken)
    , workerPendingLatency   :: IORef (Count, Count, NanoSecond64)

    -- This is the second level stat which is an accmulation from
    -- workerPendingLatency stats. We keep accumulating latencies in this
    -- bucket until we have stats for a sufficient period and then we reset it
    -- to start collecting for the next period and retain the computed average
    -- latency for the last period in workerMeasuredLatency.
    -- [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    -- (allYieldCount, yieldCount, timeTaken)
    , workerCollectedLatency :: IORef (Count, Count, NanoSecond64)

    -- Latency as measured by workers, aggregated for the last period.
    -- [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    , workerMeasuredLatency :: IORef NanoSecond64
    }

data SVarStats = SVarStats {
      totalDispatches  :: IORef Int
    , maxWorkers       :: IORef Int
    , maxOutQSize      :: IORef Int
    , maxHeapSize      :: IORef Int
    , maxWorkQSize     :: IORef Int
    , avgWorkerLatency :: IORef (Count, NanoSecond64)
    , minWorkerLatency :: IORef NanoSecond64
    , maxWorkerLatency :: IORef NanoSecond64
    , svarStopTime     :: IORef (Maybe AbsTime)
}

-- This is essentially a 'Maybe Word' type
data Limit = Unlimited | Limited Word deriving Show

instance Eq Limit where
    Unlimited == Unlimited = True
    Unlimited == Limited _ = False
    Limited _ == Unlimited = False
    Limited x == Limited y = x == y

instance Ord Limit where
    Unlimited <= Unlimited = True
    Unlimited <= Limited _ = False
    Limited _ <= Unlimited = True
    Limited x <= Limited y = x <= y

-- When to stop the composed stream.
data SVarStopStyle =
      StopNone -- stops only when all streams are finished
    | StopAny  -- stop when any stream finishes
    | StopBy   -- stop when a specific stream finishes
    deriving (Eq, Show)

-- | Buffering policy for persistent push workers (in ParallelT).  In a pull
-- style SVar (in AsyncT, AheadT etc.), the consumer side dispatches workers on
-- demand, workers terminate if the buffer is full or if the consumer is not
-- cosuming fast enough.  In a push style SVar, a worker is dispatched only
-- once, workers are persistent and keep pushing work to the consumer via a
-- bounded buffer. If the buffer becomes full the worker either blocks, or it
-- can drop an item from the buffer to make space.
--
-- Pull style SVars are useful in lazy stream evaluation whereas push style
-- SVars are useful in strict left Folds.
--
-- XXX Maybe we can separate the implementation in two different types instead
-- of using a common SVar type.
--
data PushBufferPolicy =
      PushBufferDropNew  -- drop the latest element and continue
    | PushBufferDropOld  -- drop the oldest element and continue
    | PushBufferBlock    -- block the thread until space
                         -- becomes available

-- IMPORTANT NOTE: we cannot update the SVar after generating it as we have
-- references to the original SVar stored in several functions which will keep
-- pointing to the original data and the new updates won't reflect there.
-- Any updateable parts must be kept in mutable references (IORef).
--
data SVar t m a = SVar
    {
    -- Read only state
      svarStyle       :: SVarStyle
    , svarMrun        :: RunInIO m
    , svarStopStyle   :: SVarStopStyle
    , svarStopBy      :: IORef ThreadId

    -- Shared output queue (events, length)
    -- XXX For better efficiency we can try a preallocated array type (perhaps
    -- something like a vector) that allows an O(1) append. That way we will
    -- avoid constructing and reversing the list. Possibly we can also avoid
    -- the GC copying overhead. When the size increases we should be able to
    -- allocate the array in chunks.
    --
    -- [LOCKING] Frequent locked access. This is updated by workers on each
    -- yield and once in a while read by the consumer thread. This could have
    -- big locking overhead if the number of workers is high.
    --
    -- XXX We can use a per-CPU data structure to reduce the locking overhead.
    -- However, a per-cpu structure cannot guarantee the exact sequence in
    -- which the elements were added, though that may not be important.
    , outputQueue    :: IORef ([ChildEvent a], Int)

    -- [LOCKING] Infrequent MVar. Used when the outputQ transitions from empty
    -- to non-empty, or a work item is queued by a worker to the work queue and
    -- needDoorBell is set by the consumer.
    , outputDoorBell :: MVar ()  -- signal the consumer about output
    , readOutputQ    :: m [ChildEvent a]
    , postProcess    :: m Bool

    -- channel to send events from the consumer to the worker. Used to send
    -- exceptions from a fold driver to the fold computation running as a
    -- consumer thread in the concurrent fold cases. Currently only one event
    -- is sent by the fold so we do not really need a queue for it.
    , outputQueueFromConsumer :: IORef ([ChildEvent a], Int)
    , outputDoorBellFromConsumer :: MVar ()

    -- Combined/aggregate parameters
    -- This is truncated to maxBufferLimit if set to more than that. Otherwise
    -- potentially each worker may yield one value to the buffer in the worst
    -- case exceeding the requested buffer size.
    , maxWorkerLimit :: Limit
    , maxBufferLimit :: Limit
    -- These two are valid and used only when maxBufferLimit is Limited.
    , pushBufferSpace  :: IORef Count
    , pushBufferPolicy :: PushBufferPolicy
    -- [LOCKING] The consumer puts this MVar after emptying the buffer, workers
    -- block on it when the buffer becomes full. No overhead unless the buffer
    -- becomes full.
    , pushBufferMVar :: MVar ()

    -- [LOCKING] Read only access by consumer when dispatching a worker.
    -- Decremented by workers when picking work and undo decrement if the
    -- worker does not yield a value.
    , remainingWork  :: Maybe (IORef Count)
    , yieldRateInfo  :: Maybe YieldRateInfo

    -- Used only by bounded SVar types
    , enqueue        :: (RunInIO m, t m a) -> IO ()
    , isWorkDone     :: IO Bool
    , isQueueDone    :: IO Bool
    , needDoorBell   :: IORef Bool
    , workLoop       :: Maybe WorkerInfo -> m ()

    -- Shared, thread tracking
    -- [LOCKING] Updated unlocked only by consumer thread in case of
    -- Async/Ahead style SVars. Updated locked by worker threads in case of
    -- Parallel style.
    , workerThreads  :: IORef (Set ThreadId)
    -- [LOCKING] Updated locked by consumer thread when dispatching a worker
    -- and by the worker threads when the thread stops. This is read unsafely
    -- at several places where we want to rely on an approximate value.
    , workerCount    :: IORef Int
    , accountThread  :: ThreadId -> m ()
    , workerStopMVar :: MVar ()

    , svarStats      :: SVarStats
    -- to track garbage collection of SVar
    , svarRef        :: Maybe (IORef ())

    -- Only for diagnostics
    , svarInspectMode :: Bool
    , svarCreator    :: ThreadId
    , outputHeap     :: IORef ( Heap (Entry Int (AheadHeapEntry t m a))
                              , Maybe Int)
    -- Shared work queue (stream, seqNo)
    , aheadWorkQueue :: IORef ([t m a], Int)
    }

-------------------------------------------------------------------------------
-- State for concurrency control
-------------------------------------------------------------------------------

-- XXX we can put the resettable fields in a oneShotConfig field and others in
-- a persistentConfig field. That way reset would be fast and scalable
-- irrespective of the number of fields.
--
-- XXX make all these Limited types and use phantom types to distinguish them
data State t m a = State
    { -- one shot configuration, automatically reset for each API call
      streamVar   :: Maybe (SVar t m a)
    , _yieldLimit  :: Maybe Count

    -- persistent configuration, state that remains valid until changed by
    -- an explicit setting via a combinator.
    , _threadsHigh    :: Limit
    , _bufferHigh     :: Limit
    -- XXX these two can be collapsed into a single type
    , _streamLatency  :: Maybe NanoSecond64 -- bootstrap latency
    , _maxStreamRate  :: Maybe Rate
    , _inspectMode    :: Bool
    }

-------------------------------------------------------------------------------
-- State defaults and reset
-------------------------------------------------------------------------------

-- A magical value for the buffer size arrived at by running the smallest
-- possible task and measuring the optimal value of the buffer for that.  This
-- is obviously dependent on hardware, this figure is based on a 2.2GHz intel
-- core-i7 processor.
magicMaxBuffer :: Word
magicMaxBuffer = 1500

defaultMaxThreads, defaultMaxBuffer :: Limit
defaultMaxThreads = Limited magicMaxBuffer
defaultMaxBuffer = Limited magicMaxBuffer

-- The fields prefixed by an _ are not to be accessed or updated directly but
-- via smart accessor APIs.
defState :: State t m a
defState = State
    { streamVar = Nothing
    , _yieldLimit = Nothing
    , _threadsHigh = defaultMaxThreads
    , _bufferHigh = defaultMaxBuffer
    , _maxStreamRate = Nothing
    , _streamLatency = Nothing
    , _inspectMode = False
    }

-- XXX if perf gets affected we can have all the Nothing params in a single
-- structure so that we reset is fast. We can also use rewrite rules such that
-- reset occurs only in concurrent streams to reduce the impact on serial
-- streams.
-- We can optimize this so that we clear it only if it is a Just value, it
-- results in slightly better perf for zip/zipM but the performance of scan
-- worsens a lot, it does not fuse.
--
-- XXX This has a side effect of clearing the SVar and yieldLimit, therefore it
-- should not be used to convert from the same type to the same type, unless
-- you want to clear the SVar. For clearing the SVar you should be using the
-- appropriate unStream functions instead.
--
-- | Adapt the stream state from one type to another.
adaptState :: State t m a -> State t n b
adaptState st = st
    { streamVar = Nothing
    , _yieldLimit = Nothing
    }

-------------------------------------------------------------------------------
-- Smart get/set routines for State
-------------------------------------------------------------------------------

-- Use get/set routines instead of directly accessing the State fields
setYieldLimit :: Maybe Int64 -> State t m a -> State t m a
setYieldLimit lim st =
    st { _yieldLimit =
            case lim of
                Nothing -> Nothing
                Just n  ->
                    if n <= 0
                    then Just 0
                    else Just (fromIntegral n)
       }

getYieldLimit :: State t m a -> Maybe Count
getYieldLimit = _yieldLimit

setMaxThreads :: Int -> State t m a -> State t m a
setMaxThreads n st =
    st { _threadsHigh =
            if n < 0
            then Unlimited
            else if n == 0
                 then defaultMaxThreads
                 else Limited (fromIntegral n)
       }

getMaxThreads :: State t m a -> Limit
getMaxThreads = _threadsHigh

setMaxBuffer :: Int -> State t m a -> State t m a
setMaxBuffer n st =
    st { _bufferHigh =
            if n < 0
            then Unlimited
            else if n == 0
                 then defaultMaxBuffer
                 else Limited (fromIntegral n)
       }

getMaxBuffer :: State t m a -> Limit
getMaxBuffer = _bufferHigh

setStreamRate :: Maybe Rate -> State t m a -> State t m a
setStreamRate r st = st { _maxStreamRate = r }

getStreamRate :: State t m a -> Maybe Rate
getStreamRate = _maxStreamRate

setStreamLatency :: Int -> State t m a -> State t m a
setStreamLatency n st =
    st { _streamLatency =
            if n <= 0
            then Nothing
            else Just (fromIntegral n)
       }

getStreamLatency :: State t m a -> Maybe NanoSecond64
getStreamLatency = _streamLatency

setInspectMode :: State t m a -> State t m a
setInspectMode st = st { _inspectMode = True }

getInspectMode :: State t m a -> Bool
getInspectMode = _inspectMode

-------------------------------------------------------------------------------
-- Cleanup
-------------------------------------------------------------------------------

cleanupSVar :: SVar t m a -> IO ()
cleanupSVar sv = do
    workers <- readIORef (workerThreads sv)
    Prelude.mapM_ (`throwTo` ThreadAbort)
          workers

cleanupSVarFromWorker :: SVar t m a -> IO ()
cleanupSVarFromWorker sv = do
    workers <- readIORef (workerThreads sv)
    self <- myThreadId
    Prelude.mapM_ (`throwTo` ThreadAbort)
          (Prelude.filter (/= self) $ S.toList workers)

-------------------------------------------------------------------------------
-- Worker latency data collection
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

printSVar :: SVar t m a -> String -> IO ()
printSVar sv how = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ "\n" <> how <> "\n" <> svInfo

-- MVar diagnostics has some overhead - around 5% on asyncly null benchmark, we
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

------------------------------------------------------------------------------
-- Spawning threads
------------------------------------------------------------------------------

-- /Since: 0.8.0 ("Streamly.Prelude")/
--
-- | A monad that can perform concurrent or parallel IO operations. Streams
-- that can be composed concurrently require the underlying monad to be
-- 'MonadAsync'.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- @since 0.8.0
type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

-- When we run computations concurrently, we completely isolate the state of
-- the concurrent computations from the parent computation.  The invariant is
-- that we should never be running two concurrent computations in the same
-- thread without using the runInIO function.  Also, we should never be running
-- a concurrent computation in the parent thread, otherwise it may affect the
-- state of the parent which is against the defined semantics of concurrent
-- execution.
newtype RunInIO m = RunInIO { runInIO :: forall b. m b -> IO (StM m b) }

captureMonadState :: MonadBaseControl IO m => m (RunInIO m)
captureMonadState = control $ \run -> run (return $ RunInIO run)

-- Stolen from the async package. The perf improvement is modest, 2% on a
-- thread heavy benchmark (parallel composition using noop computations).
-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \ s ->
   case fork# action s of (# s1, tid #) -> (# s1, ThreadId tid #)

{-# INLINE doFork #-}
doFork :: MonadBaseControl IO m
    => m ()
    -> RunInIO m
    -> (SomeException -> IO ())
    -> m ThreadId
doFork action (RunInIO mrun) exHandler =
    control $ \run ->
        mask $ \restore -> do
                tid <- rawForkIO $ catch (restore $ void $ mrun action)
                                         exHandler
                run (return tid)

{-# INLINABLE fork #-}
fork :: MonadBaseControl IO m => m () -> m ThreadId
fork = liftBaseDiscard forkIO

-- | Fork a thread that is automatically killed as soon as the reference to the
-- returned threadId is garbage collected.
--
{-# INLINABLE forkManaged #-}
forkManaged :: (MonadIO m, MonadBaseControl IO m) => m () -> m ThreadId
forkManaged action = do
    tid <- fork action
    liftIO $ addFinalizer tid (killThread tid)
    return tid

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

-- {-# NOINLINE sendStopToProducer #-}
sendStopToProducer :: MonadIO m => SVar t m a -> m ()
sendStopToProducer sv = liftIO $ do
    tid <- myThreadId
    void $ sendToProducer sv (ChildStop tid Nothing)

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

-------------------------------------------------------------------------------
-- Doorbell
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

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------

-- Note: For purely right associated expressions this queue should have at most
-- one element. It grows to more than one when we have left associcated
-- expressions. Large left associated compositions can grow this to a
-- large size
{-# INLINE enqueueLIFO #-}
enqueueLIFO ::
       SVar t m a -> IORef [(RunInIO m, t m a)] -> (RunInIO m, t m a) -> IO ()
enqueueLIFO sv q m = do
    atomicModifyIORefCAS_ q $ \ms -> m : ms
    ringDoorBell sv

-------------------------------------------------------------------------------
-- WAsync
-------------------------------------------------------------------------------

-- XXX we can use the Ahead style sequence/heap mechanism to make the best
-- effort to always try to finish the streams on the left side of an expression
-- first as long as possible.

{-# INLINE enqueueFIFO #-}
enqueueFIFO ::
       SVar t m a
    -> LinkedQueue (RunInIO m, t m a)
    -> (RunInIO m, t m a)
    -> IO ()
enqueueFIFO sv q m = do
    pushL q m
    ringDoorBell sv

-------------------------------------------------------------------------------
-- Ahead
-------------------------------------------------------------------------------

-- Lookahead streams can execute multiple tasks concurrently, ahead of time,
-- but always serve them in the same order as they appear in the stream. To
-- implement lookahead streams efficiently we assign a sequence number to each
-- task when the task is picked up for execution. When the task finishes, the
-- output is tagged with the same sequence number and we rearrange the outputs
-- in sequence based on that number.
--
-- To explain the mechanism imagine that the current task at the head of the
-- stream has a "token" to yield to the outputQueue. The ownership of the token
-- is determined by the current sequence number is maintained in outputHeap.
-- Sequence number is assigned when a task is queued. When a thread dequeues a
-- task it picks up the sequence number as well and when the output is ready it
-- uses the sequence number to queue the output to the outputQueue.
--
-- The thread with current sequence number sends the output directly to the
-- outputQueue. Other threads push the output to the outputHeap. When the task
-- being queued on the heap is a stream of many elements we evaluate only the
-- first element and keep the rest of the unevaluated computation in the heap.
-- When such a task gets the "token" for outputQueue it evaluates and directly
-- yields all the elements to the outputQueue without checking for the
-- "token".
--
-- Note that no two outputs in the heap can have the same sequence numbers and
-- therefore we do not need a stable heap. We have also separated the buffer
-- for the current task (outputQueue) and the pending tasks (outputHeap) so
-- that the pending tasks cannot interfere with the current task. Note that for
-- a single task just the outputQueue is enough and for the case of many
-- threads just a heap is good enough. However we balance between these two
-- cases, so that both are efficient.
--
-- For bigger streams it may make sense to have separate buffers for each
-- stream. However, for singleton streams this may become inefficient. However,
-- if we do not have separate buffers, then the streams that come later in
-- sequence may hog the buffer, hindering the streams that are ahead. For this
-- reason we have a single element buffer limitation for the streams being
-- executed in advance.
--
-- This scheme works pretty efficiently with less than 40% extra overhead
-- compared to the Async streams where we do not have any kind of sequencing of
-- the outputs. It is especially devised so that we are most efficient when we
-- have short tasks and need just a single thread. Also when a thread yields
-- many items it can hold lockfree access to the outputQueue and do it
-- efficiently.
--
-- XXX Maybe we can start the ahead threads at a lower cpu and IO priority so
-- that they do not hog the resources and hinder the progress of the threads in
-- front of them.

-- XXX Left associated ahead expressions are expensive. We start a new SVar for
-- each left associative expression. The queue is used only for right
-- associated expression, we queue the right expression and execute the left.
-- Thererefore the queue never has more than one item in it.
--
-- XXX we can fix this. When we queue more than one item on the queue we can
-- mark the previously queued item as not-runnable. The not-runnable item is
-- not dequeued until the already running one has finished and at that time we
-- would also know the exact sequence number of the already queued item.
--
-- we can even run the already queued items but they will have to be sorted in
-- layers in the heap. We can use a list of heaps for that.
{-# INLINE enqueueAhead #-}
enqueueAhead :: SVar t m a -> IORef ([t m a], Int) -> (RunInIO m, t m a) -> IO ()
enqueueAhead sv q m = do
    atomicModifyIORefCAS_ q $ \ case
        ([], n) -> ([snd m], n + 1)  -- increment sequence
        _ -> error "enqueueAhead: queue is not empty"
    ringDoorBell sv

-- enqueue without incrementing the sequence number
{-# INLINE reEnqueueAhead #-}
reEnqueueAhead :: SVar t m a -> IORef ([t m a], Int) -> t m a -> IO ()
reEnqueueAhead sv q m = do
    atomicModifyIORefCAS_ q $ \ case
        ([], n) -> ([m], n)  -- DO NOT increment sequence
        _ -> error "reEnqueueAhead: queue is not empty"
    ringDoorBell sv

-- Normally the thread that has the token should never go away. The token gets
-- handed over to another thread, but someone or the other has the token at any
-- point of time. But if the task that has the token finds that the outputQueue
-- is full, in that case it can go away without even handing over the token to
-- another thread. In that case it sets the nextSequence number in the heap its
-- own sequence number before going away. To handle this case, any task that
-- does not have the token tries to dequeue from the heap first before
-- dequeuing from the work queue. If it finds that the task at the top of the
-- heap is the one that owns the current sequence number then it grabs the
-- token and starts with that.
--
-- XXX instead of queueing just the head element and the remaining computation
-- on the heap, evaluate as many as we can and place them on the heap. But we
-- need to give higher priority to the lower sequence numbers so that lower
-- priority tasks do not fill up the heap making higher priority tasks block
-- due to full heap. Maybe we can have a weighted space for them in the heap.
-- The weight is inversely proportional to the sequence number.
--
-- XXX review for livelock
--
{-# INLINE queueEmptyAhead #-}
queueEmptyAhead :: MonadIO m => IORef ([t m a], Int) -> m Bool
queueEmptyAhead q = liftIO $ do
    (xs, _) <- readIORef q
    return $ null xs

{-# INLINE dequeueAhead #-}
dequeueAhead :: MonadIO m
    => IORef ([t m a], Int) -> m (Maybe (t m a, Int))
dequeueAhead q = liftIO $
    atomicModifyIORefCAS q $ \case
            ([], n) -> (([], n), Nothing)
            (x : [], n) -> (([], n), Just (x, n))
            _ -> error "more than one item on queue"

-------------------------------------------------------------------------------
-- Heap manipulation
-------------------------------------------------------------------------------

withIORef :: IORef a -> (a -> IO b) -> IO b
withIORef ref f = readIORef ref >>= f

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f =
    atomicModifyIORef ref $ \x -> (f x, ())

data HeapDequeueResult t m a =
      Clearing
    | Waiting Int
    | Ready (Entry Int (AheadHeapEntry t m a))

{-# INLINE dequeueFromHeap #-}
dequeueFromHeap
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> IO (HeapDequeueResult t m a)
dequeueFromHeap hpVar =
    atomicModifyIORef hpVar $ \pair@(hp, snum) ->
        case snum of
            Nothing -> (pair, Clearing)
            Just n -> do
                let r = H.uncons hp
                case r of
                    Just (ent@(Entry seqNo _ev), hp') ->
                            if seqNo == n
                            then ((hp', Nothing), Ready ent)
                            else assert (seqNo >= n) (pair, Waiting n)
                    Nothing -> (pair, Waiting n)

{-# INLINE dequeueFromHeapSeq #-}
dequeueFromHeapSeq
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> Int
    -> IO (HeapDequeueResult t m a)
dequeueFromHeapSeq hpVar i =
    atomicModifyIORef hpVar $ \(hp, snum) ->
        case snum of
            Nothing -> do
                let r = H.uncons hp
                case r of
                    Just (ent@(Entry seqNo _ev), hp') ->
                        if seqNo == i
                        then ((hp', Nothing), Ready ent)
                        else assert (seqNo >= i) ((hp, Just i), Waiting i)
                    Nothing -> ((hp, Just i), Waiting i)
            Just _ -> error "dequeueFromHeapSeq: unreachable"

heapIsSane :: Maybe Int -> Int -> Bool
heapIsSane snum seqNo =
    case snum of
        Nothing -> True
        Just n -> seqNo >= n

{-# INLINE requeueOnHeapTop #-}
requeueOnHeapTop
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> Entry Int (AheadHeapEntry t m a)
    -> Int
    -> IO ()
requeueOnHeapTop hpVar ent seqNo =
    atomicModifyIORef_ hpVar $ \(hp, snum) ->
        assert (heapIsSane snum seqNo) (H.insert ent hp, Just seqNo)

{-# INLINE updateHeapSeq #-}
updateHeapSeq
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> Int
    -> IO ()
updateHeapSeq hpVar seqNo =
    atomicModifyIORef_ hpVar $ \(hp, snum) ->
        assert (heapIsSane snum seqNo) (hp, Just seqNo)

-------------------------------------------------------------------------------
-- WAhead
-------------------------------------------------------------------------------

-- XXX To be implemented. Use a linked queue like WAsync and put back the
-- remaining computation at the back of the queue instead of the heap, and
-- increment the sequence number.

-------------------------------------------------------------------------------
-- Dispatching workers and tracking them
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

{-# NOINLINE handleChildException #-}
handleChildException :: SVar t m a -> SomeException -> IO ()
handleChildException sv e = do
    tid <- myThreadId
    void $ send sv (ChildStop tid (Just e))

{-# NOINLINE handleFoldException #-}
handleFoldException :: SVar t m a -> SomeException -> IO ()
handleFoldException sv e = do
    tid <- myThreadId
    void $ sendToProducer sv (ChildStop tid (Just e))

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

-------------------------------------------------------------------------------
-- Dispatch workers with rate control
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

-- | Another magic number! When we have to start more workers to cover up a
-- number of yields that we are lagging by then we cannot start one worker for
-- each yield because that may be a very big number and if the latency of the
-- workers is low these number of yields could be very high. We assume that we
-- run each extra worker for at least this much time.
rateRecoveryTime :: NanoSecond64
rateRecoveryTime = 1000000

-- We either block, or send one worker with limited yield count or one or more
-- workers with unlimited yield count.
data Work
    = BlockWait NanoSecond64
    | PartialWorker Count
    | ManyWorkers Int Count
    deriving Show

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

-------------------------------------------------------------------------------
-- Worker dispatch and wait loop
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Reading from the workers' output queue/buffer
-------------------------------------------------------------------------------

{-# INLINE readOutputQBasic #-}
readOutputQBasic :: IORef ([ChildEvent a], Int) -> IO ([ChildEvent a], Int)
readOutputQBasic q = atomicModifyIORefCAS q $ \x -> (([],0), x)

{-# INLINE readOutputQRaw #-}
readOutputQRaw :: SVar t m a -> IO ([ChildEvent a], Int)
readOutputQRaw sv = do
    (list, len) <- readOutputQBasic (outputQueue sv)
    when (svarInspectMode sv) $ do
        let ref = maxOutQSize $ svarStats sv
        oqLen <- readIORef ref
        when (len > oqLen) $ writeIORef ref len
    return (list, len)

readOutputQBounded :: MonadAsync m => SVar t m a -> m [ChildEvent a]
readOutputQBounded sv = do
    (list, len) <- liftIO $ readOutputQRaw sv
    -- When there is no output seen we dispatch more workers to help
    -- out if there is work pending in the work queue.
    if len <= 0
    then blockingRead
    else do
        -- send a worker proactively, if needed, even before we start
        -- processing the output.  This may degrade single processor
        -- perf but improves multi-processor, because of more
        -- parallelism
        sendOneWorker
        return list

    where

    sendOneWorker = do
        cnt <- liftIO $ readIORef $ workerCount sv
        when (cnt <= 0) $ do
            done <- liftIO $ isWorkDone sv
            when (not done) (pushWorker 0 sv)

    {-# INLINE blockingRead #-}
    blockingRead = do
        sendWorkerWait sendWorkerDelay (dispatchWorker 0) sv
        liftIO (fst `fmap` readOutputQRaw sv)

readOutputQPaced :: MonadAsync m => SVar t m a -> m [ChildEvent a]
readOutputQPaced sv = do
    (list, len) <- liftIO $ readOutputQRaw sv
    if len <= 0
    then blockingRead
    else do
        -- XXX send a worker proactively, if needed, even before we start
        -- processing the output.
        void $ dispatchWorkerPaced sv
        return list

    where

    {-# INLINE blockingRead #-}
    blockingRead = do
        sendWorkerWait sendWorkerDelayPaced dispatchWorkerPaced sv
        liftIO (fst `fmap` readOutputQRaw sv)

postProcessBounded :: MonadAsync m => SVar t m a -> m Bool
postProcessBounded sv = do
    workersDone <- allThreadsDone sv
    -- There may still be work pending even if there are no workers pending
    -- because all the workers may return if the outputQueue becomes full. In
    -- that case send off a worker to kickstart the work again.
    --
    -- Note that isWorkDone can only be safely checked if all workers are done.
    -- When some workers are in progress they may have decremented the yield
    -- Limit and later ending up incrementing it again. If we look at the yield
    -- limit in that window we may falsely say that it is 0 and therefore we
    -- are done.
    if workersDone
    then do
        r <- liftIO $ isWorkDone sv
        -- Note that we need to guarantee a worker, therefore we cannot just
        -- use dispatchWorker which may or may not send a worker.
        when (not r) (pushWorker 0 sv)
        -- XXX do we need to dispatch many here?
        -- void $ dispatchWorker sv
        return r
    else return False

postProcessPaced :: MonadAsync m => SVar t m a -> m Bool
postProcessPaced sv = do
    workersDone <- allThreadsDone sv
    -- XXX If during consumption we figure out we are getting delayed then we
    -- should trigger dispatch there as well.  We should try to check on the
    -- workers after consuming every n item from the buffer?
    if workersDone
    then do
        r <- liftIO $ isWorkDone sv
        when (not r) $ do
            void $ dispatchWorkerPaced sv
            -- Note that we need to guarantee a worker since the work is not
            -- finished, therefore we cannot just rely on dispatchWorkerPaced
            -- which may or may not send a worker.
            noWorker <- allThreadsDone sv
            when noWorker $ pushWorker 0 sv
        return r
    else return False

-------------------------------------------------------------------------------
-- Creating an SVar
-------------------------------------------------------------------------------

getYieldRateInfo :: State t m a -> IO (Maybe YieldRateInfo)
getYieldRateInfo st = do
    -- convert rate in Hertz to latency in Nanoseconds
    let rateToLatency r = if r <= 0 then maxBound else round $ 1.0e9 / r
    case getStreamRate st of
        Just (Rate low goal high buf) ->
            let l    = rateToLatency goal
                minl = rateToLatency high
                maxl = rateToLatency low
            in mkYieldRateInfo l (LatencyRange minl maxl) buf
        Nothing -> return Nothing

    where

    mkYieldRateInfo latency latRange buf = do
        measured <- newIORef 0
        wcur     <- newIORef (0,0,0)
        wcol     <- newIORef (0,0,0)
        now      <- getTime Monotonic
        wlong    <- newIORef (0,now)
        period   <- newIORef 1
        gainLoss <- newIORef (Count 0)

        return $ Just YieldRateInfo
            { svarLatencyTarget      = latency
            , svarLatencyRange       = latRange
            , svarRateBuffer         = buf
            , svarGainedLostYields   = gainLoss
            , workerBootstrapLatency = getStreamLatency st
            , workerPollingInterval  = period
            , workerMeasuredLatency  = measured
            , workerPendingLatency   = wcur
            , workerCollectedLatency = wcol
            , svarAllTimeLatency     = wlong
            }

newSVarStats :: IO SVarStats
newSVarStats = do
    disp   <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
    avgLat <- newIORef (0, NanoSecond64 0)
    maxLat <- newIORef (NanoSecond64 0)
    minLat <- newIORef (NanoSecond64 0)
    stpTime <- newIORef Nothing

    return SVarStats
        { totalDispatches  = disp
        , maxWorkers       = maxWrk
        , maxOutQSize      = maxOq
        , maxHeapSize      = maxHs
        , maxWorkQSize     = maxWq
        , avgWorkerLatency = avgLat
        , minWorkerLatency = minLat
        , maxWorkerLatency = maxLat
        , svarStopTime     = stpTime
        }

-- XXX remove polymorphism in t, inline f
getAheadSVar :: MonadAsync m
    => State t m a
    -> (   IORef ([t m a], Int)
        -> IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
        -> State t m a
        -> SVar t m a
        -> Maybe WorkerInfo
        -> m ())
    -> RunInIO m
    -> IO (SVar t m a)
getAheadSVar st f mrun = do
    outQ    <- newIORef ([], 0)
    -- the second component of the tuple is "Nothing" when heap is being
    -- cleared, "Just n" when we are expecting sequence number n to arrive
    -- before we can start clearing the heap.
    outH    <- newIORef (H.empty, Just 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    -- Sequence number is incremented whenever something is queued, therefore,
    -- first sequence number would be 0
    q <- newIORef ([], -1)
    stopMVar <- newMVar ()
    yl <- case getYieldLimit st of
            Nothing -> return Nothing
            Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st

    stats <- newSVarStats
    tid <- myThreadId

    let getSVar sv readOutput postProc = SVar
            { outputQueue      = outQ
            , outputQueueFromConsumer = undefined
            , remainingWork  = yl
            , maxBufferLimit   = getMaxBuffer st
            , pushBufferSpace = undefined
            , pushBufferPolicy = undefined
            , pushBufferMVar   = undefined
            , maxWorkerLimit   = min (getMaxThreads st) (getMaxBuffer st)
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , outputDoorBellFromConsumer = undefined
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = f q outH st{streamVar = Just sv} sv
            , enqueue          = enqueueAhead sv q
            , isWorkDone       = isWorkDoneAhead sv q outH
            , isQueueDone      = isQueueDoneAhead sv q
            , needDoorBell     = wfw
            , svarStyle        = AheadVar
            , svarStopStyle    = StopNone
            , svarStopBy       = undefined
            , svarMrun         = mrun
            , workerCount      = active
            , accountThread    = delThread sv
            , workerStopMVar   = stopMVar
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode st
            , svarCreator      = tid
            , aheadWorkQueue   = q
            , outputHeap       = outH
            , svarStats        = stats
            }

    let sv =
            case getStreamRate st of
                Nothing -> getSVar sv readOutputQBounded postProcessBounded
                Just _  -> getSVar sv readOutputQPaced postProcessPaced
     in return sv

    where

    {-# INLINE isQueueDoneAhead #-}
    isQueueDoneAhead sv q = do
        queueDone <- checkEmpty q
        yieldsDone <-
                case remainingWork sv of
                    Just yref -> do
                        n <- readIORef yref
                        return (n <= 0)
                    Nothing -> return False
        -- XXX note that yieldsDone can only be authoritative only when there
        -- are no workers running. If there are active workers they can
        -- later increment the yield count and therefore change the result.
        return $ yieldsDone || queueDone

    {-# INLINE isWorkDoneAhead #-}
    isWorkDoneAhead sv q ref = do
        heapDone <- do
                (hp, _) <- readIORef ref
                return (H.size hp <= 0)
        queueDone <- isQueueDoneAhead sv q
        return $ heapDone && queueDone

    checkEmpty q = do
        (xs, _) <- readIORef q
        return $ null xs

getParallelSVar :: MonadIO m
    => SVarStopStyle -> State t m a -> RunInIO m -> IO (SVar t m a)
getParallelSVar ss st mrun = do
    outQ    <- newIORef ([], 0)
    outQRev <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    outQMvRev <- newEmptyMVar
    active  <- newIORef 0
    running <- newIORef S.empty
    yl <- case getYieldLimit st of
            Nothing -> return Nothing
            Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st
    let bufLim =
            case getMaxBuffer st of
                Unlimited -> undefined
                Limited x -> fromIntegral x
    remBuf <- newIORef bufLim
    pbMVar <- newMVar ()

    stats <- newSVarStats
    tid <- myThreadId

    stopBy <-
        case ss of
            StopBy -> liftIO $ newIORef undefined
            _ -> return undefined

    let sv =
            SVar { outputQueue      = outQ
                 , outputQueueFromConsumer = outQRev
                 , remainingWork    = yl
                 , maxBufferLimit   = getMaxBuffer st
                 , pushBufferSpace  = remBuf
                 , pushBufferPolicy = PushBufferBlock
                 , pushBufferMVar   = pbMVar
                 , maxWorkerLimit   = Unlimited
                 -- Used only for diagnostics
                 , yieldRateInfo    = rateInfo
                 , outputDoorBell   = outQMv
                 , outputDoorBellFromConsumer = outQMvRev
                 , readOutputQ      = readOutputQPar sv
                 , postProcess      = allThreadsDone sv
                 , workerThreads    = running
                 , workLoop         = undefined
                 , enqueue          = undefined
                 , isWorkDone       = undefined
                 , isQueueDone      = undefined
                 , needDoorBell     = undefined
                 , svarStyle        = ParallelVar
                 , svarStopStyle    = ss
                 , svarStopBy       = stopBy
                 , svarMrun         = mrun
                 , workerCount      = active
                 , accountThread    = modifyThread sv
                 , workerStopMVar   = undefined
                 , svarRef          = Nothing
                 , svarInspectMode  = getInspectMode st
                 , svarCreator      = tid
                 , aheadWorkQueue   = undefined
                 , outputHeap       = undefined
                 , svarStats        = stats
                 }
     in return sv

    where

    readOutputQPar sv = liftIO $ do
        withDiagMVar sv "readOutputQPar: doorbell"
            $ takeMVar (outputDoorBell sv)
        case yieldRateInfo sv of
            Nothing -> return ()
            Just yinfo -> void $ collectLatency sv yinfo False
        r <- fst `fmap` readOutputQRaw sv
        liftIO $ do
            void $ tryTakeMVar (pushBufferMVar sv)
            resetBufferLimit sv
            writeBarrier
            void $ tryPutMVar (pushBufferMVar sv) ()
        return r

sendFirstWorker :: MonadAsync m => SVar t m a -> t m a -> m (SVar t m a)
sendFirstWorker sv m = do
    -- Note: We must have all the work on the queue before sending the
    -- pushworker, otherwise the pushworker may exit before we even get a
    -- chance to push.
    runIn <- captureMonadState
    liftIO $ enqueue sv (runIn, m)
    case yieldRateInfo sv of
        Nothing -> pushWorker 0 sv
        Just yinfo  ->
            if svarLatencyTarget yinfo == maxBound
            then liftIO $ threadDelay maxBound
            else pushWorker 1 sv
    return sv

{-# INLINABLE newAheadVar #-}
newAheadVar :: MonadAsync m
    => State t m a
    -> t m a
    -> (   IORef ([t m a], Int)
        -> IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
        -> State t m a
        -> SVar t m a
        -> Maybe WorkerInfo
        -> m ())
    -> m (SVar t m a)
newAheadVar st m wloop = do
    mrun <- captureMonadState
    sv <- liftIO $ getAheadSVar st wloop mrun
    sendFirstWorker sv m

{-# INLINABLE newParallelVar #-}
newParallelVar :: MonadAsync m
    => SVarStopStyle -> State t m a -> m (SVar t m a)
newParallelVar ss st = do
    mrun <- captureMonadState
    liftIO $ getParallelSVar ss st mrun

-------------------------------------------------------------------------------
-- Write a stream to an SVar
-------------------------------------------------------------------------------

-- XXX this errors out for Parallel/Ahead SVars
-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toStreamVar :: MonadAsync m => SVar t m a -> t m a -> m ()
toStreamVar sv m = do
    runIn <- captureMonadState
    liftIO $ enqueue sv (runIn, m)
    done <- allThreadsDone sv
    -- XXX This is safe only when called from the consumer thread or when no
    -- consumer is present.  There may be a race if we are not running in the
    -- consumer thread.
    -- XXX do this only if the work queue is not empty. The work may have been
    -- carried out by existing workers.
    when done $
        case yieldRateInfo sv of
            Nothing -> pushWorker 0 sv
            Just _  -> pushWorker 1 sv
