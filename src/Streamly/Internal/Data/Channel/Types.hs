-- |
-- Module      : Streamly.Internal.Data.Channel.Types
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A Channel is a place where streams join and new streams start. This module
-- defines low level data structures and functions to build channels. For
-- concrete Channels see the Channel modules of specific stream types.
--
-- A Channel is a conduit to the output from multiple streams running
-- concurrently and asynchronously. A channel can be thought of as an
-- asynchronous IO handle. We can write any number of streams to a channel in a
-- non-blocking manner and then read them back at any time at any pace.  The
-- channel would run the streams asynchronously and accumulate results. A
-- channel may not really execute the stream completely and accumulate all the
-- results. However, it ensures that the reader can read the results at
-- whatever pace it wants to read. The channel monitors and adapts to the
-- consumer's pace.
--
-- A channel is a mini scheduler, it has an associated workLoop that holds the
-- stream tasks to be picked and run by a pool of worker threads. It has an
-- associated output queue where the output stream elements are placed by the
-- worker threads. An outputDoorBell is used by the worker threads to intimate the
-- consumer thread about availability of new results in the output queue. More
-- workers are added to the channel by 'fromChannel' on demand if the output
-- produced is not keeping pace with the consumer. On bounded channels, workers
-- block on the output queue to provide throttling of the producer  when the
-- consumer is not pulling fast enough.  The number of workers may even get
-- reduced depending on the consuming pace.
--
module Streamly.Internal.Data.Channel.Types
    (
    -- * Types
      Count (..)
    , Limit (..)
    , ThreadAbort (..)
    , ChildEvent (..)

    -- * Stats
    , SVarStats (..)
    , newSVarStats

    -- * Rate Control
    , WorkerInfo (..)
    , LatencyRange (..)
    , YieldRateInfo (..)
    , newRateInfo

    -- * Output queue
    , readOutputQRaw
    , readOutputQBasic
    , ringDoorBell

    -- * Yield Limit
    , decrementYieldLimit
    , incrementYieldLimit

    -- * Configuration
    , Rate (..)
    , StopWhen (..)
    , Config

    -- ** Default config
    , magicMaxBuffer
    , defaultConfig

    -- ** Set config
    , maxThreads
    , maxBuffer
    , maxYields
    , inspect
    , eager
    , stopWhen
    , ordered
    , interleaved
    , boundThreads

    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- ** Get config
    , getMaxThreads
    , getMaxBuffer
    , getStreamRate
    , getStreamLatency
    , setStreamLatency
    , getYieldLimit
    , getInspectMode
    , getEagerDispatch
    , getStopWhen
    , getOrdered
    , getInterleaved
    , getBound

    -- * Cleanup
    , cleanupSVar

    -- * Diagnostics
    , dumpCreator
    , dumpOutputQ
    , dumpDoorBell
    , dumpNeedDoorBell
    , dumpRunningThreads
    , dumpWorkerCount

    , withDiagMVar
    , printSVar
    )
where

import Control.Concurrent (ThreadId, throwTo, MVar, tryReadMVar)
import Control.Concurrent.MVar (tryPutMVar)
import Control.Exception
    ( SomeException(..), Exception, catches, throwIO, Handler(..)
    , BlockedIndefinitelyOnMVar(..), BlockedIndefinitelyOnSTM(..))
import Control.Monad (void, when)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import Streamly.Internal.Data.Atomics
    (atomicModifyIORefCAS, atomicModifyIORefCAS_, storeLoadBarrier)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units (AbsTime, NanoSecond64(..))
import System.IO (hPutStrLn, stderr)

import qualified Data.Set as Set

------------------------------------------------------------------------------
-- Common types
------------------------------------------------------------------------------

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

-- | Channel driver throws this exception to all active workers to clean up
-- the channel.
data ThreadAbort = ThreadAbort deriving Show

instance Exception ThreadAbort

-- XXX Use a ChildSingle event to speed up mapM?
-- | Events that a child thread may send to a parent thread.
data ChildEvent a =
      ChildYield a
    | ChildStopChannel
    | ChildStop ThreadId (Maybe SomeException)

-- | We measure the individual worker latencies to estimate the number of workers
-- needed or the amount of time we have to sleep between dispatches to achieve
-- a particular rate when controlled pace mode it used.
data WorkerInfo = WorkerInfo
    {
    -- | 0 means unlimited
      workerYieldMax   :: Count
    -- | total number of yields by the worker till now
    , workerYieldCount    :: IORef Count
    -- | yieldCount at start, timestamp
    , workerLatencyStart  :: IORef (Count, AbsTime)
    }

data LatencyRange = LatencyRange
    { minLatency :: NanoSecond64
    , maxLatency :: NanoSecond64
    } deriving Show

-- | Rate control.
data YieldRateInfo = YieldRateInfo
    { svarLatencyTarget    :: NanoSecond64
    , svarLatencyRange     :: LatencyRange
    , svarRateBuffer       :: Int

    -- | [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    , svarGainedLostYields :: IORef Count

    -- | Actual latency/througput as seen from the consumer side, we count the
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

    -- | After how many yields the worker should update the latency information.
    -- If the latency is high, this count is kept lower and vice-versa.  XXX If
    -- the latency suddenly becomes too high this count may remain too high for
    -- long time, in such cases the consumer can change it.
    -- 0 means no latency computation
    -- XXX this is derivable from workerMeasuredLatency, can be removed.
    -- [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    , workerPollingInterval :: IORef Count

    -- | This is in progress latency stats maintained by the workers which we
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

    -- | This is the second level stat which is an accmulation from
    -- workerPendingLatency stats. We keep accumulating latencies in this
    -- bucket until we have stats for a sufficient period and then we reset it
    -- to start collecting for the next period and retain the computed average
    -- latency for the last period in workerMeasuredLatency.
    -- [LOCKING] Unlocked access. Modified by the consumer thread and unsafely
    -- read by the worker threads
    -- (allYieldCount, yieldCount, timeTaken)
    , workerCollectedLatency :: IORef (Count, Count, NanoSecond64)

    -- | Latency as measured by workers, aggregated for the last period.
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

-------------------------------------------------------------------------------
-- Config
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
data Rate = Rate
    { rateLow    :: Double -- ^ The lower rate limit
    , rateGoal   :: Double -- ^ The target rate we want to achieve
    , rateHigh   :: Double -- ^ The upper rate limit
    , rateBuffer :: Int    -- ^ Maximum slack from the goal
    }

-- | Specify when the 'Channel' should stop.
data StopWhen =
      FirstStops -- ^ Stop when the first stream ends.
    | AllStop    -- ^ Stop when all the streams end.
    | AnyStops   -- ^ Stop when any one stream ends.

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

-- | A magical value for the buffer size arrived at by running the smallest
-- possible task and measuring the optimal value of the buffer for that.  This
-- is obviously dependent on hardware, this figure is based on a 2.2GHz intel
-- core-i7 processor.
magicMaxBuffer :: Word
magicMaxBuffer = 1500

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

setStreamLatency :: Int -> Config -> Config
setStreamLatency n st =
    st { _streamLatency =
            if n <= 0
            then Nothing
            else Just (fromIntegral n)
       }

getStreamLatency :: Config -> Maybe NanoSecond64
getStreamLatency = _streamLatency

-- XXX Rename to "inspect"

-- | Print debug information about the 'Channel' when the stream ends.
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

-- TODO: Make it consistently take effect everywhere.

-- | Spawn bound threads (i.e., spawn threads using 'forkOS' instead of
-- 'forkIO'). The default value is 'False'.
--
-- Currently, this only takes effect only for concurrent folds.
boundThreads :: Bool -> Config -> Config
boundThreads flag st = st { _bound = flag }

getBound :: Config -> Bool
getBound = _bound

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

-------------------------------------------------------------------------------
-- Channel yield count
-------------------------------------------------------------------------------

-- XXX Can we make access to remainingWork and yieldRateInfo fields in sv
-- faster, along with the fields in sv required by send?
-- XXX make it noinline
--
-- XXX we may want to employ an increment and decrement in batches when the
-- througput is high or when the cost of synchronization is high. For example
-- if the application is distributed then inc/dec of a shared variable may be
-- very costly.

-- | A worker decrements the yield limit before it executes an action. However,
-- the action may not result in an element being yielded, in that case we have
-- to increment the yield limit.
--
-- Note that we need it to be an Int type so that we have the ability to undo a
-- decrement that takes it below zero.
{-# INLINE decrementYieldLimit #-}
decrementYieldLimit :: Maybe (IORef Count) -> IO Bool
decrementYieldLimit remaining =
    case remaining of
        Nothing -> return True
        Just ref -> do
            r <- atomicModifyIORefCAS ref $ \x -> (x - 1, x)
            return $ r >= 1

{-# INLINE incrementYieldLimit #-}
incrementYieldLimit :: Maybe (IORef Count) -> IO ()
incrementYieldLimit remaining =
    case remaining of
        Nothing -> return ()
        Just ref -> atomicModifyIORefCAS_ ref (+ 1)

-------------------------------------------------------------------------------
-- Output queue
-------------------------------------------------------------------------------

{-# INLINE readOutputQBasic #-}
readOutputQBasic :: IORef ([ChildEvent a], Int) -> IO ([ChildEvent a], Int)
readOutputQBasic q = atomicModifyIORefCAS q $ \x -> (([],0), x)

{-# INLINE readOutputQRaw #-}
readOutputQRaw ::
    IORef ([ChildEvent a], Int) -> Maybe SVarStats -> IO ([ChildEvent a], Int)
readOutputQRaw q stats = do
    (list, len) <- readOutputQBasic q
    case stats of
        Just ss -> do
            let ref = maxOutQSize ss
            oqLen <- readIORef ref
            when (len > oqLen) $ writeIORef ref len
        Nothing -> return ()
    return (list, len)

{-# INLINE ringDoorBell #-}
ringDoorBell :: IORef Bool -> MVar () -> IO ()
ringDoorBell needBell bell = do
    storeLoadBarrier
    w <- readIORef needBell
    when w $ do
        -- Note: the sequence of operations is important for correctness here.
        -- We need to set the flag to false strictly before sending the
        -- outputDoorBell, otherwise the outputDoorBell may get processed too
        -- early and then we may set the flag to False to later making the
        -- consumer lose the flag, even without receiving a outputDoorBell.
        atomicModifyIORefCAS_ needBell (const False)
        void $ tryPutMVar bell ()

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

dumpCreator :: Show a => a -> String
dumpCreator tid = "Creator tid = " <> show tid

dumpOutputQ :: (Foldable t, Show a1) => IORef (t a2, a1) -> IO String
dumpOutputQ q = do
    (oqList, oqLen) <- readIORef q
    return $ unlines
        [ "outputQueue length computed  = " <> show (length oqList)
        , "outputQueue length maintained = " <> show oqLen
        ]

dumpDoorBell :: Show a => MVar a -> IO String
dumpDoorBell mvar =  do
    db <- tryReadMVar mvar
    return $ "outputDoorBell = " <> show db

dumpNeedDoorBell :: Show a => IORef a -> IO String
dumpNeedDoorBell ref = do
    waiting <- readIORef ref
    return $ "needDoorBell = " <> show waiting

dumpRunningThreads :: Show a => IORef a -> IO String
dumpRunningThreads ref = do
    rthread <- readIORef ref
    return $ "running threads = " <> show rthread

dumpWorkerCount :: Show a => IORef a -> IO String
dumpWorkerCount ref = do
    workers <- readIORef ref
    return $ "running thread count = " <> show workers

{-# NOINLINE mvarExcHandler #-}
mvarExcHandler :: IO String -> String -> BlockedIndefinitelyOnMVar -> IO ()
mvarExcHandler dump label e@BlockedIndefinitelyOnMVar = do
    svInfo <- dump
    hPutStrLn stderr $ label <> " " <> "BlockedIndefinitelyOnMVar\n" <> svInfo
    throwIO e

{-# NOINLINE stmExcHandler #-}
stmExcHandler :: IO String -> String -> BlockedIndefinitelyOnSTM -> IO ()
stmExcHandler dump label e@BlockedIndefinitelyOnSTM = do
    svInfo <- dump
    hPutStrLn stderr $ label <> " " <> "BlockedIndefinitelyOnSTM\n" <> svInfo
    throwIO e

-- | MVar diagnostics has some overhead - around 5% on AsyncT null benchmark, we
-- can keep it on in production to debug problems quickly if and when they
-- happen, but it may result in unexpected output when threads are left hanging
-- until they are GCed because the consumer went away.
withDiagMVar :: Bool -> IO String -> String -> IO () -> IO ()
withDiagMVar inspecting dump label action =
    if inspecting
    then
        action `catches` [ Handler (mvarExcHandler dump label)
                         , Handler (stmExcHandler dump label)
                         ]
    else action

printSVar :: IO String -> String -> IO ()
printSVar dump how = do
    svInfo <- dump
    hPutStrLn stderr $ "\n" <> how <> "\n" <> svInfo

-------------------------------------------------------------------------------
-- Cleanup
-------------------------------------------------------------------------------

-- | Never called from a worker thread.
cleanupSVar :: IORef (Set ThreadId) -> IO ()
cleanupSVar workerSet = do
    workers <- readIORef workerSet
    -- self <- myThreadId
    Prelude.mapM_ (`throwTo` ThreadAbort)
          -- (Prelude.filter (/= self) $ Set.toList workers)
          (Set.toList workers)
