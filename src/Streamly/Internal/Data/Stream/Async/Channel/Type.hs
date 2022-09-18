-- |
-- Module      : Streamly.Internal.Data.Stream.Async.Channel.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Async.Channel.Type
    (
    -- * Parent child communication
      ThreadAbort (..)
    , ChildEvent (..)

    -- * SVar
    , Count (..)
    , Limit (..)
    , SVarStats (..)
    , WorkerInfo (..)
    , LatencyRange (..)
    , YieldRateInfo (..)
    , Channel(..)

    -- * Channel configuration
    , Rate (..)
    , Config

    -- ** Default config
    , magicMaxBuffer
    , defaultConfig

    -- ** Type cast

    -- ** State accessors
    , maxThreads
    , maxBuffer
    , maxYields
    , inspectMode

    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    , getMaxThreads
    , getMaxBuffer
    , getStreamRate
    , getStreamLatency
    , setStreamLatency
    , getYieldLimit
    , getInspectMode

    , getYieldRateInfo
    , newSVarStats
    )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException(..), Exception)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef)
import Data.Set (Set)

import Streamly.Internal.Control.Concurrent (RunInIO)
import Streamly.Internal.Data.Stream.StreamK.Type (Stream)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units (AbsTime, NanoSecond64(..))

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

-- XXX We can use maxBound for unlimited?

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

------------------------------------------------------------------------------
-- Parent child thread communication type
------------------------------------------------------------------------------

data ThreadAbort = ThreadAbort deriving Show

instance Exception ThreadAbort

-- | Events that a child thread may send to a parent thread.
data ChildEvent a =
      ChildYield a
    | ChildStop ThreadId (Maybe SomeException)

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

-- IMPORTANT NOTE: we cannot update the SVar after generating it as we have
-- references to the original SVar stored in several functions which will keep
-- pointing to the original data and the new updates won't reflect there.
-- Any updateable parts must be kept in mutable references (IORef).

-- XXX Since we have stream specific channels now, we can remove functions like
-- enqueue, readOuputQ, postProcess, workLoop etc from this.

-- | A mutable channel to evaluate multiple streams concurrently and provide
-- the combined results as output stream.
data Channel m a = Channel
    {
      svarMrun        :: RunInIO m
    -- FORWARD FLOW: Flow of data from the workers to the consumer

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
    -- XXX We can send a bundle of events of one type coaleseced together in an
    -- unboxed structure.
    , outputQueue    :: IORef ([ChildEvent a], Int)

    -- [LOCKING] Infrequent MVar. Used when the outputQ transitions from empty
    -- to non-empty, or a work item is queued by a worker to the work queue and
    -- needDoorBell is set by the consumer.
    , outputDoorBell :: MVar ()  -- signal the consumer about output
    -- XXX Can we use IO instead of m here?
    , readOutputQ    :: m [ChildEvent a]
    , postProcess    :: m Bool

    -- Scheduling --

    -- Combined/aggregate parameters
    -- This is capped to maxBufferLimit if set to more than that. Otherwise
    -- potentially each worker may yield one value to the buffer in the worst
    -- case exceeding the requested buffer size.
    , maxWorkerLimit :: Limit
    , maxBufferLimit :: Limit

    -- [LOCKING] Read only access by consumer when dispatching a worker.
    -- Decremented by workers when picking work and undo decrement if the
    -- worker does not yield a value.
    , remainingWork  :: Maybe (IORef Count)
    , yieldRateInfo  :: Maybe YieldRateInfo

    , enqueue        :: (RunInIO m, Stream m a) -> IO ()
    , isWorkDone     :: IO Bool
    , isQueueDone    :: IO Bool
    , needDoorBell   :: IORef Bool
    , workLoop       :: Maybe WorkerInfo -> m ()

    -- Shared, thread tracking
    -- [LOCKING] Updated unlocked, only by consumer thread.
    , workerThreads  :: IORef (Set ThreadId)

    -- [LOCKING] Updated locked, by consumer thread when dispatching a worker
    -- and by the worker threads when the thread stops. This is read unsafely
    -- at several places where we want to rely on an approximate value.
    , workerCount    :: IORef Int
    -- XXX Can we use IO instead of m here?
    , accountThread  :: ThreadId -> m ()
    , workerStopMVar :: MVar ()

    -- cleanup: to track garbage collection of SVar --
    , svarRef        :: Maybe (IORef ())

    -- Stats --
    , svarStats      :: SVarStats

    -- Diagnostics --
    , svarInspectMode :: Bool
    , svarCreator    :: ThreadId
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

-- | An abstract type for specifying the configuration parameters of a
-- 'Channel'. Use @Config -> Config@ modifier functions to modify the
-- default configuration. See the modifiers for default values.
--
data Config = Config
    { -- one shot configuration, automatically reset for each API call
      -- streamVar   :: Maybe (SVar t m a)
      _yieldLimit  :: Maybe Count

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
defaultConfig :: Config
defaultConfig = Config
    { -- streamVar = Nothing
      _yieldLimit = Nothing
    , _threadsHigh = defaultMaxThreads
    , _bufferHigh = defaultMaxBuffer
    , _maxStreamRate = Nothing
    , _streamLatency = Nothing
    , _inspectMode = False
    }

-------------------------------------------------------------------------------
-- Smart get/set routines for State
-------------------------------------------------------------------------------

-- Use get/set routines instead of directly accessing the State fields
maxYields :: Maybe Int64 -> Config -> Config
maxYields lim st =
    st { _yieldLimit =
            case lim of
                Nothing -> Nothing
                Just n  ->
                    if n <= 0
                    then Just 0
                    else Just (fromIntegral n)
       }

getYieldLimit :: Config -> Maybe Count
getYieldLimit = _yieldLimit

-- | Specify the maximum number of threads that can be spawned by the channel.
-- A value of 0 resets the thread limit to default, a negative value means
-- there is no limit. The default value is 1500.
--
-- When the actions in a stream are IO bound, having blocking IO calls, this
-- option can be used to control the maximum number of in-flight IO requests.
-- When the actions are CPU bound this option can be used to control the amount
-- of CPU used by the stream.
--
maxThreads :: Int -> Config -> Config
maxThreads n st =
    st { _threadsHigh =
            if n < 0
            then Unlimited
            else if n == 0
                 then defaultMaxThreads
                 else Limited (fromIntegral n)
       }

getMaxThreads :: Config -> Limit
getMaxThreads = _threadsHigh

-- | Specify the maximum size of the buffer for storing the results from
-- concurrent computations. If the buffer becomes full we stop spawning more
-- concurrent tasks until there is space in the buffer.
-- A value of 0 resets the buffer size to default, a negative value means
-- there is no limit. The default value is 1500.
--
-- CAUTION! using an unbounded 'maxBuffer' value (i.e. a negative value)
-- coupled with an unbounded 'maxThreads' value is a recipe for disaster in
-- presence of infinite streams, or very large streams.  Especially, it must
-- not be used when 'pure' is used in 'ZipAsyncM' streams as 'pure' in
-- applicative zip streams generates an infinite stream causing unbounded
-- concurrent generation with no limit on the buffer or threads.
--
maxBuffer :: Int -> Config -> Config
maxBuffer n st =
    st { _bufferHigh =
            if n < 0
            then Unlimited
            else if n == 0
                 then defaultMaxBuffer
                 else Limited (fromIntegral n)
       }

getMaxBuffer :: Config -> Limit
getMaxBuffer = _bufferHigh

-- | Specify the pull rate of a channel.
-- A 'Nothing' value resets the rate to default which is unlimited.  When the
-- rate is specified, concurrent production may be ramped up or down
-- automatically to achieve the specified yield rate. The specific behavior for
-- different styles of 'Rate' specifications is documented under 'Rate'.  The
-- effective maximum production rate achieved by a channel is governed by:
--
-- * The 'maxThreads' limit
-- * The 'maxBuffer' limit
-- * The maximum rate that the stream producer can achieve
-- * The maximum rate that the stream consumer can achieve
--
rate :: Maybe Rate -> Config -> Config
rate r st = st { _maxStreamRate = r }

getStreamRate :: Config -> Maybe Rate
getStreamRate = _maxStreamRate

setStreamLatency :: Int -> Config -> Config
setStreamLatency n st =
    st { _streamLatency =
            if n <= 0
            then Nothing
            else Just (fromIntegral n)
       }

getStreamLatency :: Config -> Maybe NanoSecond64
getStreamLatency = _streamLatency

inspectMode :: Config -> Config
inspectMode st = st { _inspectMode = True }

getInspectMode :: Config -> Bool
getInspectMode = _inspectMode

-------------------------------------------------------------------------------
-- Creating an SVar
-------------------------------------------------------------------------------

getYieldRateInfo :: Config -> IO (Maybe YieldRateInfo)
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

-------------------------------------------------------------------------------
-- Rate
-------------------------------------------------------------------------------

-- | Same as @rate (Just $ Rate (r/2) r (2*r) maxBound)@
--
-- Specifies the average production rate of a stream in number of yields
-- per second (i.e.  @Hertz@).  Concurrent production is ramped up or down
-- automatically to achieve the specified average yield rate. The rate can
-- go down to half of the specified rate on the lower side and double of
-- the specified rate on the higher side.
--
avgRate :: Double -> Config -> Config
avgRate r = rate (Just $ Rate (r/2) r (2*r) maxBound)

-- | Same as @rate (Just $ Rate r r (2*r) maxBound)@
--
-- Specifies the minimum rate at which the stream should yield values. As
-- far as possible the yield rate would never be allowed to go below the
-- specified rate, even though it may possibly go above it at times, the
-- upper limit is double of the specified rate.
--
minRate :: Double -> Config -> Config
minRate r = rate (Just $ Rate r r (2*r) maxBound)

-- | Same as @rate (Just $ Rate (r/2) r r maxBound)@
--
-- Specifies the maximum rate at which the stream should yield values. As
-- far as possible the yield rate would never be allowed to go above the
-- specified rate, even though it may possibly go below it at times, the
-- lower limit is half of the specified rate. This can be useful in
-- applications where certain resource usage must not be allowed to go
-- beyond certain limits.
--
maxRate :: Double -> Config -> Config
maxRate r = rate (Just $ Rate (r/2) r r maxBound)

-- | Same as @rate (Just $ Rate r r r 0)@
--
-- Specifies a constant yield rate. If for some reason the actual rate
-- goes above or below the specified rate we do not try to recover it by
-- increasing or decreasing the rate in future.  This can be useful in
-- applications like graphics frame refresh where we need to maintain a
-- constant refresh rate.
--
constRate :: Double -> Config -> Config
constRate r = rate (Just $ Rate r r r 0)
