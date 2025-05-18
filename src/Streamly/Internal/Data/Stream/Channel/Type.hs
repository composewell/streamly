-- |
-- Module      : Streamly.Internal.Data.Stream.Channel.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Channel.Type
    (
    -- ** Type
      Channel(..)

    -- ** Configuration
    , Config

    -- *** Default config
    , defaultConfig

    -- *** Limits
    , maxThreads
    , maxBuffer
    , maxYields

    -- *** Rate Control
    , Rate(..)
    , newRateInfo
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- *** Stop behavior
    , StopWhen (..)
    , stopWhen

    -- *** Scheduling behavior
    , eager
    , ordered
    , interleaved
    , boundThreads

    -- *** Diagnostics
    , inspect

    -- *** Get config
    , getMaxBuffer
    , getMaxThreads
    , getYieldLimit
    , getInspectMode
    , getStreamRate
    , getEagerDispatch
    , getOrdered
    , getStopWhen
    , getInterleaved

    -- ** Sending Worker Events
    , yieldWith
    , stopWith
    , exceptionWith
    , shutdown

    -- ** Diagnostics
    , dumpChannel
    )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.IORef (IORef, newIORef)
import Data.List (intersperse)
import Data.Set (Set)
import Streamly.Internal.Control.Concurrent (RunInIO)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS_)
import Streamly.Internal.Data.Channel.Dispatcher (dumpSVarStats)
import Streamly.Internal.Data.Channel.Worker
    (sendYield, sendStop, sendEvent, sendException)
import Streamly.Internal.Data.StreamK (StreamK)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units (NanoSecond64(..))

import Streamly.Internal.Data.Channel.Types

-- IMPORTANT NOTE: we cannot update the SVar after generating it as we have
-- references to the original SVar stored in several functions which will keep
-- pointing to the original data and the new updates won't reflect there.
-- Any updateable parts must be kept in mutable references (IORef).

-- XXX Since we have stream specific channels now, we can remove functions like
-- enqueue, readOuputQ, postProcess, workLoop etc from this.

-- XXX Add an option in channel for minthreads.
-- dispatch tail worker from the worker itself up to min threads or based on
-- pace data. min threads can be increased dynamically by the event loop.
-- for eager minthreads = maxthreads

-- | A mutable channel to evaluate multiple streams concurrently and provide
-- the combined results as output stream.
--
-- There are only two actors working on the channel data structure, the event
-- processing loop (single thread), and the workers (multiple threads). Locking
-- notes are provided below for concurrent access.
data Channel m a = Channel
    {
     -- XXX Do we need this? We store the runner in the work q, is that enough?
     -- This seems to be used only by the 'ordered' stream as of now.

     -- | Runner for the monadic actions in the stream. Captures the monad
     -- state at the point where the channel was created and uses the same
     -- state to run all actions.
      svarMrun :: RunInIO m

    ---------------------------------------------------------------------------
    -- Output queue related
    ---------------------------------------------------------------------------

    -- | Maximum size of the 'outputQueue'. The actual worst case buffer could
    -- be double of this as the event loop may read the queue and the workers
    -- may fill it up even before the event loop has started consuming.
    , maxBufferLimit :: Limit

    -- XXX For better efficiency we can try a preallocated array type (perhaps
    -- something like a vector) that allows an O(1) append. That way we will
    -- avoid constructing and reversing the list. Possibly we can also avoid
    -- the GC copying overhead. When the size increases we should be able to
    -- allocate the array in chunks.
    --
    -- XXX We can use a per-CPU data structure to reduce the locking overhead.
    -- However, a per-cpu structure cannot guarantee the exact sequence in
    -- which the elements were added, though that may not be important.
    --
    -- XXX We can send a bundle of events of one type coaleseced together in an
    -- unboxed structure.

    -- | (events, count): worker event queue of the channel. This is where the
    -- workers queue the results and other events.
    --
    -- [LOCKING] Frequently locked. This is locked and updated by workers on
    -- each yield, and locked, updated by the event loop thread once in a while
    -- for reading. Workers' locking contention may be high if there are a
    -- large number of workers.
    , outputQueue :: IORef ([ChildEvent a], Int)

    -- | Door bell for workers to wakeup the event loop.
    --
    -- [LOCKING] Infrequently locked. Used only when the 'outputQueue'
    -- transitions from empty to non-empty, or a work item is queued by a
    -- worker to the work queue and 'doorBellOnWorkQ' is set by the event loop.
    , outputDoorBell :: MVar ()

    -- XXX Can we use IO instead of m here?

    -- | Function to read the output queue of the channel, depends on the rate
    -- control option.
    , readOutputQ :: m [ChildEvent a]

    -- | Function to invoke after all the events read in a batch are processed
    -- i.e. before we go on to read the next batch, depends on the rate control
    -- option.
    , postProcess :: m Bool

    ---------------------------------------------------------------------------
    -- Work and rate control
    ---------------------------------------------------------------------------

    -- | Tracks how many yields are remaining before the channel stops, used
    -- when 'maxYields' option is enabled.
    --
    -- [LOCKING] Read only access by event loop when dispatching a worker.
    -- Decremented by workers when picking work and undo decrement if the
    -- worker does not yield a value.
    , remainingWork :: Maybe (IORef Count)

    -- XXX We make this isChannelDone which should not include isQueueDone.
    --
    -- | Determine if there is no more work to do. When 'maxYields' is set for
    -- the channel we may be done even if the work queue still has work.
    , isWorkDone :: IO Bool

    -- | Rate control information for the channel used when 'rate' control is
    -- enabled,
    , yieldRateInfo :: Maybe YieldRateInfo

    ---------------------------------------------------------------------------
    -- Work queue related
    ---------------------------------------------------------------------------

    -- | When set to True, ring 'outputDoorBell' when a work item is queued on
    -- the work queue. This is set by the dispatcher before going to sleep. It
    -- wants to be woken up whenever the work queue got more work to do so that
    -- it can dispatch a worker.
    , doorBellOnWorkQ :: IORef Bool

    -- XXX instead of this we should use a dispatcher setting.

    -- | This is a hook which is invoked whenever the tail of the stream is
    -- re-enqueued on the work queue. Normally, this is set to a noop. When
    -- 'eager' option is enabled this is set to an unconditional worker
    -- dispatch function. This ensures that we eagerly sends a worker as long
    -- as there is work to do.
    , eagerDispatch :: m ()

    -- | Enqueue a stream for evaluation on the channel. The first element of
    -- the tuple is the runner function which is used to run the stream actions
    -- in a specific monadic context.
    , enqueue :: (RunInIO m, StreamK m a) -> IO ()

    -- | Determine if the work queue is empty, therefore, there is no more work
    -- to do.
    , isQueueDone :: IO Bool

    -- | Worker function. It is implicitly aware of the work queue. It dequeues
    -- a work item from the queue and runs it. It keeps on doing this in a loop
    -- until it determines that it needs to stop.
    --
    -- Normally, the worker stops when the work queue becomes empty or the work
    -- rate is higher than the target rate when rate control is enabled. It
    -- stops by sending a 'ChildStop' event to the channel
    --
    -- When rate control is enabled, the worker is dispatched with a
    -- 'WorkerInfo' record which is used by the worker to maintain rate control
    -- information and communicate it to the channel.
    , workLoop :: Maybe WorkerInfo -> m ()

    ---------------------------------------------------------------------------
    -- Worker thread accounting
    ---------------------------------------------------------------------------
    --
    -- | This is capped to 'maxBufferLimit' if set to more than that. Otherwise
    -- potentially each worker may yield one value to the buffer in the worst
    -- case exceeding the requested buffer size.
    , maxWorkerLimit :: Limit

    -- | Tracks all active worker threads. An entry is added by the dispatcher
    -- when a worker is dispatched, and removed whenever the event processing
    -- loop receives a 'ChildStop' event.
    --
    -- [LOCKING] Updated unlocked, only by the event loop thread.
    , workerThreads :: IORef (Set ThreadId)

    -- | Total number of active worker threads.
    --
    -- [LOCKING] Updated locked, by the event loop thread when dispatching a
    -- worker and by a worker thread when the thread stops. This is read
    -- without lock at several places where we want to rely on an approximate
    -- value.
    , workerCount :: IORef Int

    -- XXX Can we use IO instead of m here?
    , accountThread  :: ThreadId -> m ()

    -- | Used when 'ordered' is enabled. This is a lock to stop the workers one
    -- at a time. Stopping one might affect whether the other should stop.
    , workerStopMVar :: MVar ()

    ---------------------------------------------------------------------------
    -- Channel cleanup --
    ---------------------------------------------------------------------------
    -- | A weak IORef to call a cleanup function when the channel is garbage
    -- collected.
    , svarRef :: Maybe (IORef ())

    ---------------------------------------------------------------------------
    -- Channel Stats --
    ---------------------------------------------------------------------------
    -- | Stats collection.
    , svarStats :: SVarStats

    ---------------------------------------------------------------------------
    -- Diagnostics --
    ---------------------------------------------------------------------------
    -- | When 'inspect' mode is enabled we report diagnostic data about the
    -- channel at certain points.
    , svarInspectMode :: Bool
    -- | threadId of the thread that created the channel
    , svarCreator :: ThreadId
    }

-------------------------------------------------------------------------------
-- Channel Config
-------------------------------------------------------------------------------

-- XXX we can put the resettable fields in a oneShotConfig field and others in
-- a persistentConfig field. That way reset would be fast and scalable
-- irrespective of the number of fields.
--
-- XXX make all these Limited types and use phantom types to distinguish them

-- | An abstract type for specifying the configuration parameters of a
-- 'Channel'. Use @Config -> Config@ modifier functions to modify the default
-- configuration. See the individual modifier documentation for default values.
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
    , _inspect    :: Bool
    , _eagerDispatch  :: Bool
    , _stopWhen :: StopWhen
    , _ordered :: Bool
    , _interleaved :: Bool
    , _bound :: Bool
    }

-------------------------------------------------------------------------------
-- State defaults and reset
-------------------------------------------------------------------------------

defaultMaxThreads, defaultMaxBuffer :: Limit
defaultMaxThreads = Limited magicMaxBuffer
defaultMaxBuffer = Limited magicMaxBuffer

-- | The fields prefixed by an _ are not to be accessed or updated directly but
-- via smart accessor APIs. Use get/set routines instead of directly accessing
-- the Config fields
defaultConfig :: Config
defaultConfig = Config
    { -- streamVar = Nothing
      _yieldLimit = Nothing
    , _threadsHigh = defaultMaxThreads
    , _bufferHigh = defaultMaxBuffer
    , _maxStreamRate = Nothing
    , _streamLatency = Nothing
    , _inspect = False
    -- XXX Set it to True when Rate is not set?
    , _eagerDispatch = False
    , _stopWhen = AllStop
    , _ordered = False
    , _interleaved = False
    , _bound = False
    }

-------------------------------------------------------------------------------
-- Smart get/set routines for State
-------------------------------------------------------------------------------

-- | The maximum number of yields that this channel would produce. The Channel
-- automatically stops after that. This could be used to limit the speculative
-- execution beyond the limit.
--
-- 'Nothing' means there is no limit.
--
-- Keep in mind that checking this limit all the time has a performance
-- overhead.
--
-- Known Bugs: currently this works only when rate is specified.
-- Known Bugs: for ordered streams sometimes the actual count is less than
-- expected.
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

-- | Specify the stream evaluation rate of a channel.
--
-- A 'Nothing' value means there is no smart rate control, concurrent execution
-- blocks only if 'maxThreads' or 'maxBuffer' is reached, or there are no more
-- concurrent tasks to execute. This is the default.
--
-- When rate (throughput) is specified, concurrent production may be ramped
-- up or down automatically to achieve the specified stream throughput. The
-- specific behavior for different styles of 'Rate' specifications is
-- documented under 'Rate'.  The effective maximum production rate achieved by
-- a channel is governed by:
--
-- * The 'maxThreads' limit
-- * The 'maxBuffer' limit
-- * The maximum rate that the stream producer can achieve
-- * The maximum rate that the stream consumer can achieve
--
-- Maximum production rate is given by:
--
-- \(rate = \frac{maxThreads}{latency}\)
--
-- If we know the average latency of the tasks we can set 'maxThreads'
-- accordingly.
--
rate :: Maybe Rate -> Config -> Config
rate r st = st { _maxStreamRate = r }

getStreamRate :: Config -> Maybe Rate
getStreamRate = _maxStreamRate

_setStreamLatency :: Int -> Config -> Config
_setStreamLatency n st =
    st { _streamLatency =
            if n <= 0
            then Nothing
            else Just (fromIntegral n)
       }

getStreamLatency :: Config -> Maybe NanoSecond64
getStreamLatency = _streamLatency

-- XXX Rename to "inspect"

-- | Print debug information about the 'Channel' when the stream ends. When the
-- stream does not end normally, the channel debug information is printed when
-- the channel is garbage collected. If you are expecting but not seeing the
-- debug info try adding a 'performMajorGC' before the program ends.
--
inspect :: Bool -> Config -> Config
inspect flag st = st { _inspect = flag }

getInspectMode :: Config -> Bool
getInspectMode = _inspect

-- | By default, processing of output from the worker threads is given priority
-- over dispatching new workers. More workers are dispatched only when there is
-- no output to process. When 'eager' is set to 'True', workers are dispatched
-- aggresively as long as there is more work to do irrespective of whether
-- there is output pending to be processed by the stream consumer. However,
-- dispatching may stop if 'maxThreads' or 'maxBuffer' is reached.
--
-- /Note:/ This option has no effect when rate has been specified.
--
-- /Note:/ Not supported with 'interleaved'.
--
eager :: Bool -> Config -> Config
eager flag st = st { _eagerDispatch = flag }

getEagerDispatch :: Config -> Bool
getEagerDispatch = _eagerDispatch

-- | Specify when the 'Channel' should stop.
stopWhen :: StopWhen -> Config -> Config
stopWhen cond st = st { _stopWhen = cond }

getStopWhen :: Config -> StopWhen
getStopWhen = _stopWhen

-- | When enabled the streams may be evaluated cocnurrently but the results are
-- produced in the same sequence as a serial evaluation would produce.
--
-- /Note:/ Not supported with 'interleaved'.
--
ordered :: Bool -> Config -> Config
ordered flag st = st { _ordered = flag }

getOrdered :: Config -> Bool
getOrdered = _ordered

-- | Interleave the streams fairly instead of prioritizing the left stream.
-- This schedules all streams in a round robin fashion over limited number of
-- threads.
--
-- /Note:/ Can only be used on finite number of streams.
--
-- /Note:/ Not supported with 'ordered'.
--
interleaved :: Bool -> Config -> Config
interleaved flag st = st { _interleaved = flag }

getInterleaved :: Config -> Bool
getInterleaved = _interleaved

-- | Spawn bound threads (i.e., spawn threads using 'forkOS' instead of
-- 'forkIO'). The default value is 'False'.
--
-- /Unimplemented/
boundThreads :: Bool -> Config -> Config
boundThreads flag st = st { _bound = flag }

_getBound :: Config -> Bool
_getBound = _bound

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

newRateInfo :: Config -> IO (Maybe YieldRateInfo)
newRateInfo st = do
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

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Used by workers to send a value to the channel's output stream.
--
-- When a worker is dispatched, a 'WorkerInfo' record is supplied to it by the
-- dispatcher. This record contains the timestamp at the time of dispatch.
-- Whenever the worker yields a value, the yield count in the 'WorkerInfo' is
-- incremented. If the channel has rate control enabled, the yield count and
-- time duration is periodically (based on 'workerPollingInterval') pushed to
-- the channel's 'workerPendingLatency' stat. It is done only if the
-- 'workerPollingInterval' is non-zero.
--
-- Queues the event but returns 'False' if:
--
-- * the buffer limit is exceeding
-- * channel yield rate is exceeding (when rate control is enabled and
-- 'WorkerInfo' is available)
--
-- This is a thread-safe API and can be called by anyone from anywhere. Even a
-- thread that is not registered as a worker with the channel can use it but
-- when rate control is enabled, it might confuse the rate control mechanism if
-- we use workers beyond the knowledge of dispatcher.
--
{-# INLINE yieldWith #-}
yieldWith ::
       Maybe WorkerInfo -- ^ Rate control info for the worker
    -> Channel m a
    -> a
    -> IO Bool -- ^ True means the worker can continue otherwise stop.
yieldWith winfo chan =
    sendYield
        (maxBufferLimit chan)
        (maxWorkerLimit chan)
        (workerCount chan)
        (yieldRateInfo chan)
        (outputQueue chan)
        (outputDoorBell chan)
        winfo

-- | Send a 'ChildStop' event to the channel, used when the worker stops
-- yielding and exits. The final update of the collected latency stats in
-- 'WorkerInfo' is pushed to the channel. Upon receiving the 'ChildStop' event
-- the channel would remove the worker from its set of registered workers.
--
-- A worker that uses this API must have been registered on the Channel prior
-- to invoking this API. This is usually done by the dispatcher  when the
-- worker is dispatched.
{-# INLINE stopWith #-}
stopWith :: Maybe WorkerInfo -> Channel m a -> IO ()
stopWith winfo chan =
    sendStop
        (workerCount chan)
        (yieldRateInfo chan)
        (outputQueue chan)
        (outputDoorBell chan)
        winfo

-- | Like 'stopWith' but marks the stop event with the specified exception.
{-# INLINE exceptionWith #-}
exceptionWith :: Maybe WorkerInfo -> Channel m a -> SomeException -> IO ()
exceptionWith _winfo chan =
    sendException (outputQueue chan) (outputDoorBell chan)

-- | Send a 'ChildStopChannel' event to shutdown the channel. Upon receiving
-- the event the event processing loop kills all the registered worker threads
-- and stops the channel.
{-# INLINABLE shutdown #-}
shutdown :: MonadIO m => Channel m a -> m ()
shutdown chan = liftIO $ do
    atomicModifyIORefCAS_ (workerCount chan) $ \n -> n - 1
    void
        $ sendEvent
            (outputQueue chan)
            (outputDoorBell chan)
            ChildStopChannel

-- | Dump the channel stats for diagnostics. Used when 'inspect' option is
-- enabled.
{-# NOINLINE dumpChannel #-}
dumpChannel :: Channel m a -> IO String
dumpChannel sv = do
    xs <- sequence $ intersperse (return "\n")
        [ return (dumpCreator (svarCreator sv))
        , return "---------CURRENT STATE-----------"
        , dumpOutputQ (outputQueue sv)
        -- XXX print the types of events in the outputQueue, first 5
        , dumpDoorBell (outputDoorBell sv)
        , dumpNeedDoorBell (doorBellOnWorkQ sv)
        , dumpRunningThreads (workerThreads sv)
        -- XXX print the status of first 5 threads
        , dumpWorkerCount (workerCount sv)
        , return "---------STATS-----------\n"
        , dumpSVarStats (svarInspectMode sv) (yieldRateInfo sv) (svarStats sv)
        ]
    return $ concat xs
