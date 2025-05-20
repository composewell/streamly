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
    -- ** Types
      Count (..)
    , Limit (..)
    , ThreadAbort (..)
    , ChildEvent (..)

    -- ** Stats
    , SVarStats (..)
    , newSVarStats

    -- ** Rate Control
    , WorkerInfo (..)
    , LatencyRange (..)
    , YieldRateInfo (..)

    -- ** Output queue
    , readOutputQRaw
    , readOutputQBasic
    , ringDoorBell

    -- ** Yield Limit
    , decrementYieldLimit
    , incrementYieldLimit

    -- ** Configuration
    , Rate (..)
    , StopWhen (..)
    , magicMaxBuffer

    -- ** Cleanup
    , cleanupSVar

    -- ** Diagnostics
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
    -- | Yields allowed for this worker. 0 means unlimited.
      workerYieldMax   :: Count
    -- | total number of yields by the worker till now
    , workerYieldCount    :: IORef Count
    -- | (yield count at start of collection interval, collection start timestamp)
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

    -- | Number of yields beyond which we will not try to recover the rate.
    , svarRateBuffer :: Int

    -- | Yields that we have permanently gained or lost since the start of the
    -- channel i.e. we do not want to adjust the rate to make up for this
    -- deficit or gain.
    --
    -- [LOCKING] Unlocked access. Modified by the consumer thread and snapshot
    -- read by the worker threads
    , svarGainedLostYields :: IORef Count

    -- XXX interval latency is enough, we can move this under diagnostics build

    -- | (channel yields from start till now, channel start timestamp) as
    -- recorded by the consumer side of the channel.
    --
    -- [LOCKING] Unlocked access. Modified by the consumer thread, snapshot
    -- read by the worker threads.
    , svarAllTimeLatency :: IORef (Count, AbsTime)

    -- | TODO. Not yet implemented. Worker latency specified by the user to be
    -- used as a guide before the first actual measurement arrives.
    , workerBootstrapLatency :: Maybe NanoSecond64

    -- XXX If the latency suddenly becomes too high this count may remain too
    -- high for long time, in such cases the consumer can change it. 0 means no
    -- latency computation
    -- XXX this is derivable from workerMeasuredLatency, can be removed.

    -- | After how many yields the worker should update the latency
    -- information. If the 'workerMeasuredLatency' is high, this count is kept
    -- lower and vice-versa.
    --
    -- [LOCKING] Unlocked access. Modified by the consumer thread and snapshot
    -- read by the worker threads
    , workerPollingInterval :: IORef Count

    -- | (total yields, measured yields, time taken by measured yields).
    -- This is first level collection bucket which is continuously updated by
    -- workers and periodically emptied and collected into
    -- 'workerCollectedLatency' by the consumer thread.
    --
    -- "Measured yields" are only those yields for which the latency was
    -- measured to be non-zero (note that if the timer resolution is low the
    -- measured latency may be zero e.g. on JS platform).
    --
    -- [LOCKING] Locked access. Atomically modified by the consumer thread as
    -- well as worker threads. Workers modify it periodically based on
    -- workerPollingInterval and not on every yield to reduce the locking
    -- overhead.
    , workerPendingLatency   :: IORef (Count, Count, NanoSecond64)

    -- | 'workerPendingLatency' is periodically reset and aggregated into this
    -- by the consumer thread. This itself is reset periodically and
    -- 'svarAllTimeLatency', 'workerMeasuredLatency' are updated using it.
    --
    -- [LOCKING] Unlocked access. Modified by the consumer thread and snapshot
    -- read by the worker threads
    , workerCollectedLatency :: IORef (Count, Count, NanoSecond64)

    -- | Weighted average of worker latencies in previous measurement periods.
    --
    -- [LOCKING] Unlocked access. Modified by the consumer thread and snapshot
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
-- However, if the yield count gap becomes more than 'rateBuffer' (specified as
-- a yield count) we try to recover only as much as 'rateBuffer'.
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
    { rateLow    :: Double -- ^ The lower rate limit (yields per sec)
    , rateGoal   :: Double -- ^ The target rate we want to achieve
    , rateHigh   :: Double -- ^ The upper rate limit
    , rateBuffer :: Int    -- ^ Maximum yield count slack from the goal
    }

-- | Specify when the 'Channel' should stop.
data StopWhen =
      FirstStops -- ^ Stop when the first stream ends.
    | AllStop    -- ^ Stop when all the streams end.
    | AnyStops   -- ^ Stop when any one stream ends.

-- | A magical value for the buffer size arrived at by running the smallest
-- possible task and measuring the optimal value of the buffer for that.  This
-- is obviously dependent on hardware, this figure is based on a 2.2GHz intel
-- core-i7 processor.
magicMaxBuffer :: Word
magicMaxBuffer = 1500

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

-- | Read the output queue of the channel. After reading set it to empty list
-- and 0 count.
{-# INLINE readOutputQBasic #-}
readOutputQBasic ::
       IORef ([a], Int) -- ^ The channel output queue
    -> IO ([a], Int) -- ^ (events, count)
readOutputQBasic q = atomicModifyIORefCAS q $ \x -> (([],0), x)

-- | Same as 'readOutputQBasic' but additionally update the max output queue
-- size channel stat if the new size is more than current max.
{-# INLINE readOutputQRaw #-}
readOutputQRaw ::
       IORef ([ChildEvent a], Int) -- ^ Channel output queue
    -> Maybe SVarStats -- ^ Channel stats
    -> IO ([ChildEvent a], Int) -- ^ (events, count)
readOutputQRaw q stats = do
    (list, len) <- readOutputQBasic q
    case stats of
        Just ss -> do
            let ref = maxOutQSize ss
            oqLen <- readIORef ref
            when (len > oqLen) $ writeIORef ref len
        Nothing -> return ()
    return (list, len)

-- | RingArray door bell. The IORef is read after adding a store-load barrier. If
-- the IORef was set to 'True' it is atomically reset to 'False'.
{-# INLINE ringDoorBell #-}
ringDoorBell ::
       IORef Bool -- ^ If 'True' only then ring the door bell
    -> MVar () -- ^ Door bell, put () to ring
    -> IO ()
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
    writeIORef workerSet Set.empty
