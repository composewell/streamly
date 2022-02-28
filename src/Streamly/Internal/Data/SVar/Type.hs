-- |
-- Module      : Streamly.Internal.Data.SVar.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.SVar.Type
    (
    -- * Parent child communication
      ThreadAbort (..)
    , ChildEvent (..)
    , AheadHeapEntry (..)

    -- * SVar
    , Count (..)
    , Limit (..)
    , SVarStyle (..)
    , SVarStopStyle (..)
    , SVarStats (..)
    , WorkerInfo (..)
    , PushBufferPolicy(..)
    , LatencyRange (..)
    , YieldRateInfo (..)
    , SVar (..)

    -- * State threaded around the stream
    , Rate (..)
    , State (streamVar)

    -- ** Default State
    , magicMaxBuffer
    , defState

    -- ** Type cast
    , adaptState

    -- ** State accessors
    , getMaxThreads
    , setMaxThreads
    , getMaxBuffer
    , setMaxBuffer
    , getStreamRate
    , setStreamRate
    , getStreamLatency
    , setStreamLatency
    , getYieldLimit
    , setYieldLimit
    , getInspectMode
    , setInspectMode
    )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException(..), Exception)
import Data.Heap (Heap, Entry(..))
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Kind (Type)
import Data.Set (Set)

import Streamly.Internal.Data.Time.Units (AbsTime, NanoSecond64(..))
import Streamly.Internal.Control.Concurrent (RunInIO)

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
-- SVar: the state for thread management
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
-- Overall state threaded around a stream
-------------------------------------------------------------------------------

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
