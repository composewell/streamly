{-# LANGUAGE CPP                        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UnboxedTuples              #-}

-- |
-- Module      : Streamly.SVar
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.SVar
    (
      MonadAsync
    , SVarStyle (..)
    , SVar (..)

    -- State threaded around the stream
    , Limit (..)
    , State (streamVar)
    , defState
    , rstState
    , getMaxThreads
    , setMaxThreads
    , getMaxBuffer
    , setMaxBuffer
    , getMaxStreamRate
    , setMaxStreamRate
    , setStreamLatency
    , getYieldLimit
    , setYieldLimit

    -- SVar related
    , newAheadVar
    , newParallelVar

    , atomicModifyIORefCAS
    , WorkerInfo (..)
    , YieldRateInfo (..)
    , ChildEvent (..)
    , AheadHeapEntry (..)
    , send
    , sendYield
    , sendStop
    , enqueueLIFO
    , enqueueFIFO
    , enqueueAhead
    , reEnqueueAhead
    , pushWorkerPar

    , queueEmptyAhead
    , dequeueAhead
    , dequeueFromHeap

    , getYieldRateInfo
    , collectLatency
    , workerUpdateLatency
    , isBeyondMaxRate
    , workerRateControl
    , updateYieldCount
    , decrementYieldLimit
    , incrementYieldLimit
    , postProcessBounded
    , postProcessPaced
    , readOutputQBounded
    , readOutputQPaced
    , dispatchWorkerPaced
    , sendFirstWorker
    , delThread

    , toStreamVar
    )
where

import Control.Concurrent
       (ThreadId, myThreadId, threadDelay, getNumCapabilities)
import Control.Concurrent.MVar
       (MVar, newEmptyMVar, tryPutMVar, takeMVar, newMVar)
import Control.Exception (SomeException(..), catch, mask, assert)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Atomics
       (casIORef, readForCAS, peekTicket, atomicModifyIORefCAS_,
        writeBarrier, storeLoadBarrier)
import Data.Concurrent.Queue.MichaelScott (LinkedQueue, pushL)
import Data.Functor (void)
import Data.Heap (Heap, Entry(..))
import Data.Int (Int64)
import Data.IORef
       (IORef, modifyIORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Word (Word64)
import GHC.Conc (ThreadId(..))
import GHC.Exts
import GHC.IO (IO(..))
import System.Clock
       (TimeSpec, Clock(Monotonic), getTime, toNanoSecs, fromNanoSecs)

import qualified Data.Heap as H
import qualified Data.Set                    as S

-- MVar diagnostics has some overhead - around 5% on asyncly null benchmark, we
-- can keep it on in production to debug problems quickly if and when they
-- happen, but it may result in unexpected output when threads are left hanging
-- until they are GCed because the consumer went away.

#ifdef DIAGNOSTICS
import Control.Concurrent.MVar (tryTakeMVar)
import Control.Exception
       (catches, throwIO, Handler(..), BlockedIndefinitelyOnMVar(..),
        BlockedIndefinitelyOnSTM(..))
import System.IO (hPutStrLn, stderr)
#endif

newtype NanoSecs = NanoSecs Word64
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

newtype Count = Count Word64
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

-- | Events that a child thread may send to a parent thread.
data ChildEvent a =
      ChildYield a
    | ChildStop ThreadId (Maybe SomeException)

-- | Sorting out-of-turn outputs in a heap for Ahead style streams
data AheadHeapEntry (t :: (* -> *) -> * -> *) m a =
      AheadEntryPure a
    | AheadEntryStream (t m a)

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
    , workerLatencyStart  :: IORef (Count, TimeSpec)
    }

-- Rate control.
data YieldRateInfo = YieldRateInfo
    { expectedYieldLatency :: NanoSecs

    -- XXX Worker latency specified by the user to be used before the first
    -- actual measurement arrives. Not yet implemented
    , workerBootstrapLatency :: Maybe NanoSecs

    -- After how many yields the worker should update the latency information.
    -- If the latency is high, this count is kept lower and vice-versa.  XXX If
    -- the latency suddenly becomes too high this count may remain too high for
    -- long time, in such cases the consumer can change it.
    -- 0 means no latency computation
    -- XXX this is derivable from workerMeasuredLatency, can be removed.
    , workerLatencyPeriod :: IORef Count

    -- This is in progress latency stats maintained by the workers which we
    -- empty into workerCollectedLatency stats at certain intervals - whenever
    -- we process the stream elements yielded in this period.
    -- (yieldCount, timeTaken)
    , workerCurrentLatency   :: IORef (Count, NanoSecs)

    -- This is the second level stat which is an accmulation from
    -- workerCurrentLatency stats. We keep accumulating latencies in this
    -- bucket until we have stats for a sufficient period and then we reset it
    -- to start collecting for the next period and retain the computed average
    -- latency for the last period in workerMeasuredLatency.
    -- (yieldCount, timeTaken)
    , workerCollectedLatency :: IORef (Count, NanoSecs)

    -- Latency as measured by workers, aggregated for the last period.
    , workerMeasuredLatency :: IORef NanoSecs

    -- Actual latency/througput as seen from the consumer side, we count the
    -- yields and the time it took to generates those yields. This is used to
    -- increase or decrease the number of workers needed to achieve the desired
    -- rate. The idle time of workers is adjusted in this, so that we only
    -- account for the rate when the consumer actually demands data.
    , workerLongTermLatency :: IORef (Count, TimeSpec)

    -- When the last worker exits it updates this timestamp. We use this to
    -- adjust workerLongTermLatency so that we do not account the idle time in
    -- that. XXX Note that not all workers stop at the same time therefore
    -- there is a certain inaccuracy in this mechanism.
    , workerStopTimeStamp :: IORef TimeSpec
    }

data Limit = Unlimited | Limited Word deriving Show

data SVar t m a = SVar
    {
    -- Read only state
      svarStyle      :: SVarStyle

    -- Shared output queue (events, length)
    , outputQueue    :: IORef ([ChildEvent a], Int)
    , outputDoorBell :: MVar ()  -- signal the consumer about output
    , readOutputQ    :: m [ChildEvent a]
    , postProcess    :: m Bool

    -- Combined/aggregate parameters
    , maxWorkerLimit :: Limit
    , maxBufferLimit :: Limit
    , remainingYields :: Maybe (IORef Count)
    , yieldRateInfo  :: Maybe YieldRateInfo

    -- Used only by bounded SVar types
    , enqueue        :: t m a -> IO ()
    , isWorkDone     :: IO Bool
    , needDoorBell   :: IORef Bool
    , workLoop       :: WorkerInfo -> m ()

    -- Shared, thread tracking
    , workerThreads  :: IORef (Set ThreadId)
    , workerCount    :: IORef Int
    , accountThread  :: ThreadId -> m ()
    , workerStopMVar :: MVar ()

#ifdef DIAGNOSTICS
    , outputHeap     :: IORef (Heap (Entry Int (AheadHeapEntry t m a)) , Int)
    -- Shared work queue (stream, seqNo)
    , aheadWorkQueue  :: IORef ([t m a], Int)
    , totalDispatches :: IORef Int
    , maxWorkers      :: IORef Int
    , maxOutQSize     :: IORef Int
    , maxHeapSize     :: IORef Int
    , maxWorkQSize    :: IORef Int
#endif
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
    , _streamLatency  :: Maybe NanoSecs
    , _maxStreamRate  :: Maybe Double -- yields per second
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
    }

-- XXX if perf gets affected we can have all the Nothing params in a single
-- structure so that we reset is fast. We can also use rewrite rules such that
-- reset occurs only in concurrent streams to reduce the impact on serial
-- streams.
-- We can optimize this so that we clear it only if it is a Just value, it
-- results in slightly better perf for zip/zipM but the performance of scan
-- worsens a lot, it does not fuse.
rstState :: State t m a -> State t m b
rstState st = st
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

setMaxStreamRate :: Double -> State t m a -> State t m a
setMaxStreamRate n st =
    st { _maxStreamRate =
            if n < 0
            then Nothing
            else if n == 0
                 then Nothing
                 else Just n
       }

getMaxStreamRate :: State t m a -> Maybe Double
getMaxStreamRate = _maxStreamRate

setStreamLatency :: Int -> State t m a -> State t m a
setStreamLatency n st =
    st { _streamLatency =
            if n < 0
            then Nothing
            else if n == 0
                 then Nothing
                 else Just (fromIntegral n)
       }

getStreamLatency :: State t m a -> Maybe NanoSecs
getStreamLatency = _streamLatency

-------------------------------------------------------------------------------
-- Dumping the SVar for debug/diag
-------------------------------------------------------------------------------

#ifdef DIAGNOSTICS
{-# NOINLINE dumpSVar #-}
dumpSVar :: SVar t m a -> IO String
dumpSVar sv = do
    tid <- myThreadId
    (oqList, oqLen) <- readIORef $ outputQueue sv
    db <- tryTakeMVar $ outputDoorBell sv
    aheadDump <-
        if svarStyle sv == AheadVar
        then do
            (oheap, oheapSeq) <- readIORef $ outputHeap sv
            (wq, wqSeq) <- readIORef $ aheadWorkQueue sv
            maxHp <- readIORef $ maxHeapSize sv
            return $ unlines
                [ "heap length = " ++ show (H.size oheap)
                , "heap seqeunce = " ++ show oheapSeq
                , "work queue length = " ++ show (length wq)
                , "work queue sequence = " ++ show wqSeq
                , "heap max size = " ++ show maxHp
                ]
        else return []

    waiting <- readIORef $ needDoorBell sv
    rthread <- readIORef $ workerThreads sv
    workers <- readIORef $ workerCount sv
    maxWrk <- readIORef $ maxWorkers sv
    dispatches <- readIORef $ totalDispatches sv
    maxOq <- readIORef $ maxOutQSize sv

    return $ unlines
        [ "tid = " ++ show tid
        , "style = " ++ show (svarStyle sv)
        , "outputQueue length computed  = " ++ show (length oqList)
        , "outputQueue length maintained = " ++ show oqLen
        , "output outputDoorBell = " ++ show db
        , "total dispatches = " ++ show dispatches
        , "max workers = " ++ show maxWrk
        , "max outQSize = " ++ show maxOq
        ]
        ++ aheadDump ++ unlines
        [ "needDoorBell = " ++ show waiting
        , "running threads = " ++ show rthread
        , "running thread count = " ++ show workers
        ]

{-# NOINLINE mvarExcHandler #-}
mvarExcHandler :: SVar t m a -> String -> BlockedIndefinitelyOnMVar -> IO ()
mvarExcHandler sv label e@BlockedIndefinitelyOnMVar = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ label ++ " " ++ "BlockedIndefinitelyOnMVar\n" ++ svInfo
    throwIO e

{-# NOINLINE stmExcHandler #-}
stmExcHandler :: SVar t m a -> String -> BlockedIndefinitelyOnSTM -> IO ()
stmExcHandler sv label e@BlockedIndefinitelyOnSTM = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ label ++ " " ++ "BlockedIndefinitelyOnSTM\n" ++ svInfo
    throwIO e

withDBGMVar :: SVar t m a -> String -> IO () -> IO ()
withDBGMVar sv label action =
    action `catches` [ Handler (mvarExcHandler sv label)
                     , Handler (stmExcHandler sv label)
                     ]
#else
withDBGMVar :: SVar t m a -> String -> IO () -> IO ()
withDBGMVar _ _ action = action
#endif

-------------------------------------------------------------------------------
-- CAS
-------------------------------------------------------------------------------

-- Slightly faster version of CAS. Gained some improvement by avoiding the use
-- of "evaluate" because we know we do not have exceptions in fn.
{-# INLINE atomicModifyIORefCAS #-}
atomicModifyIORefCAS :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORefCAS ref fn = do
    tkt <- readForCAS ref
    loop tkt retries

    where

    retries = 25 :: Int
    loop _   0     = atomicModifyIORef ref fn
    loop old tries = do
        let (new, result) = fn $ peekTicket old
        (success, tkt) <- casIORef ref old new
        if success
        then return result
        else loop tkt (tries - 1)

------------------------------------------------------------------------------
-- Spawning threads and collecting result in streamed fashion
------------------------------------------------------------------------------

-- | A monad that can perform concurrent or parallel IO operations. Streams
-- that can be composed concurrently require the underlying monad to be
-- 'MonadAsync'.
--
-- @since 0.1.0
type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

-- Stolen from the async package. The perf improvement is modest, 2% on a
-- thread heavy benchmark (parallel composition using noop computations).
-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \ s ->
   case (fork# action s) of (# s1, tid #) -> (# s1, ThreadId tid #)

{-# INLINE doFork #-}
doFork :: MonadBaseControl IO m
    => m ()
    -> (SomeException -> IO ())
    -> m ThreadId
doFork action exHandler =
    control $ \runInIO ->
        mask $ \restore -> do
                tid <- rawForkIO $ catch (restore $ void $ runInIO action)
                                         exHandler
                runInIO (return tid)

-- XXX Can we make access to remainingYields and yieldRateInfo fields in sv
-- faster, along with the fields in sv required by send?
-- XXX make it noinline
--
-- XXX we may want to employ an increment and decrement in batches when the
-- througput is high or when the cost of synchronization is high. For example
-- if the application is distributed then inc/dec of a shared variable may be
-- very costly.
{-# INLINE decrementYieldLimit #-}
decrementYieldLimit :: SVar t m a -> IO Bool
decrementYieldLimit sv =
    case remainingYields sv of
        Nothing -> return True
        Just ref -> do
            r <- atomicModifyIORefCAS ref $ \x ->
                    (if x >= 1 then x - 1 else 0, x)
            return $ r >= 1

-- XXX increment is not safe when the yield limit was already 0
-- and therefore was not decremented. We need yield limit to be
-- int and negative if we want this to work.
{-# INLINE incrementYieldLimit #-}
incrementYieldLimit :: SVar t m a -> IO ()
incrementYieldLimit sv =
    case remainingYields sv of
        Nothing -> return ()
        Just ref -> atomicModifyIORefCAS_ ref (+ 1)

-- XXX exception safety of all atomic/MVar operations

-- TBD Each worker can have their own queue and the consumer can empty one
-- queue at a time, that way contention can be reduced.

-- | This function is used by the producer threads to queue output for the
-- consumer thread to consume. Returns whether the queue has more space.
send :: SVar t m a -> ChildEvent a -> IO Bool
send sv msg = do
    -- XXX can the access to outputQueue and maxBufferLimit be made faster
    -- somehow?
    len <- atomicModifyIORefCAS (outputQueue sv) $ \(es, n) ->
        ((msg : es, n + 1), n)
    when (len <= 0) $ do
        -- The wake up must happen only after the store has finished otherwise
        -- we can have lost wakeup problems.
        writeBarrier
        -- Since multiple workers can try this at the same time, it is possible
        -- that we may put a spurious MVar after the consumer has already seen
        -- the output. But that's harmless, at worst it may cause the consumer
        -- to read the queue again and find it empty.
        -- The important point is that the consumer is guaranteed to receive a
        -- doorbell if something was added to the queue after it empties it.
        void $ tryPutMVar (outputDoorBell sv) ()

    -- XXX we should reserve the buffer when we pick up the work from the
    -- queue, instead of checking it here when it is too late.
    let limit = maxBufferLimit sv
    case limit of
        Unlimited -> return True
        Limited lim -> do
            active <- readIORef (workerCount sv)
            return $ len < ((fromIntegral lim) - active)

-- XXX We assume that a worker always yields a value. If we can have
-- workers that return without yielding anything our computations to
-- determine the number of workers may be off.
workerUpdateLatency :: YieldRateInfo -> WorkerInfo -> IO ()
workerUpdateLatency yinfo winfo = do
    cnt1 <- readIORef (workerYieldCount winfo)
    (cnt0, t0) <- readIORef (workerLatencyStart winfo)
    t1 <- getTime Monotonic
    writeIORef (workerLatencyStart winfo) (cnt1, t1)
    let period = fromInteger $ toNanoSecs (t1 - t0)
    let ref = workerCurrentLatency yinfo
    atomicModifyIORefCAS ref $ \(ycnt, ytime) ->
        ((ycnt + cnt1 - cnt0, ytime + period), ())

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
    i <- readIORef (workerLatencyPeriod yinfo)
    if (i /= 0 && (ycnt `mod` i) == 0)
    then do
        workerUpdateLatency yinfo winfo
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
{-# INLINE sendYield #-}
sendYield :: SVar t m a -> WorkerInfo -> ChildEvent a -> IO Bool
sendYield sv winfo msg = do
    r <- send sv msg
    rateLimitOk <-
        case yieldRateInfo sv of
            Nothing -> return True
            Just yinfo -> workerRateControl sv yinfo winfo
    return $ r && rateLimitOk

{-# INLINE workerStopUpdate #-}
workerStopUpdate :: WorkerInfo -> YieldRateInfo -> Int -> IO ()
workerStopUpdate winfo info n = do
    i <- readIORef (workerLatencyPeriod info)
    when (i /= 0) $ workerUpdateLatency info winfo
    when (n == 1) $ do
        t <- getTime Monotonic
        writeIORef (workerStopTimeStamp info) t

{-# INLINABLE sendStop #-}
sendStop :: SVar t m a -> WorkerInfo -> IO ()
sendStop sv winfo = do
    n <- liftIO $ atomicModifyIORefCAS (workerCount sv) $ \n -> (n - 1, n)
    case yieldRateInfo sv of
        Nothing -> return ()
        Just info -> workerStopUpdate winfo info n
    myThreadId >>= \tid -> void $ send sv (ChildStop tid Nothing)

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------

-- Note: For purely right associated expressions this queue should have at most
-- one element. It grows to more than one when we have left associcated
-- expressions. Large left associated compositions can grow this to a
-- large size
{-# INLINE enqueueLIFO #-}
enqueueLIFO :: SVar t m a -> IORef [t m a] -> t m a -> IO ()
enqueueLIFO sv q m = do
    atomicModifyIORefCAS_ q $ \ms -> m : ms
    storeLoadBarrier
    w <- readIORef $ needDoorBell sv
    when w $ do
        -- Note: the sequence of operations is important for correctness here.
        -- We need to set the flag to false strictly before sending the
        -- outputDoorBell, otherwise the outputDoorBell may get processed too early and
        -- then we may set the flag to False to later making the consumer lose
        -- the flag, even without receiving a outputDoorBell.
        atomicModifyIORefCAS_ (needDoorBell sv) (const False)
        void $ tryPutMVar (outputDoorBell sv) ()

-------------------------------------------------------------------------------
-- WAsync
-------------------------------------------------------------------------------

-- XXX we can use the Ahead style sequence/heap mechanism to make the best
-- effort to always try to finish the streams on the left side of an expression
-- first as long as possible.

{-# INLINE enqueueFIFO #-}
enqueueFIFO :: SVar t m a -> LinkedQueue (t m a) -> t m a -> IO ()
enqueueFIFO sv q m = do
    pushL q m
    storeLoadBarrier
    w <- readIORef $ needDoorBell sv
    when w $ do
        -- Note: the sequence of operations is important for correctness here.
        -- We need to set the flag to false strictly before sending the
        -- outputDoorBell, otherwise the outputDoorBell may get processed too early and
        -- then we may set the flag to False to later making the consumer lose
        -- the flag, even without receiving a outputDoorBell.
        atomicModifyIORefCAS_ (needDoorBell sv) (const False)
        void $ tryPutMVar (outputDoorBell sv) ()

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
enqueueAhead :: SVar t m a -> IORef ([t m a], Int) -> t m a -> IO ()
enqueueAhead sv q m = do
    atomicModifyIORefCAS_ q $ \ case
        ([], n) -> ([m], n + 1)  -- increment sequence
        _ -> error "not empty"
    storeLoadBarrier
    w <- readIORef $ needDoorBell sv
    when w $ do
        -- Note: the sequence of operations is important for correctness here.
        -- We need to set the flag to false strictly before sending the
        -- outputDoorBell, otherwise the outputDoorBell may get processed too early and
        -- then we may set the flag to False to later making the consumer lose
        -- the flag, even without receiving a outputDoorBell.
        atomicModifyIORefCAS_ (needDoorBell sv) (const False)
        void $ tryPutMVar (outputDoorBell sv) ()

-- enqueue without incrementing the sequence number
{-# INLINE reEnqueueAhead #-}
reEnqueueAhead :: SVar t m a -> IORef ([t m a], Int) -> t m a -> IO ()
reEnqueueAhead sv q m = do
    atomicModifyIORefCAS_ q $ \ case
        ([], n) -> ([m], n)  -- DO NOT increment sequence
        _ -> error "not empty"
    storeLoadBarrier
    w <- readIORef $ needDoorBell sv
    when w $ do
        atomicModifyIORefCAS_ (needDoorBell sv) (const False)
        void $ tryPutMVar (outputDoorBell sv) ()

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
dequeueAhead q = liftIO $ do
    atomicModifyIORefCAS q $ \case
            ([], n) -> (([], n), Nothing)
            (x : [], n) -> (([], n), Just (x, n))
            _ -> error "more than one item on queue"

{-# INLINE dequeueFromHeap #-}
dequeueFromHeap
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Int)
    -> IO (Maybe (Entry Int (AheadHeapEntry t m a)))
dequeueFromHeap hpRef = do
    atomicModifyIORef hpRef $ \hp@(h, snum) -> do
        let r = H.uncons h
        case r of
            Nothing -> (hp, Nothing)
            Just (ent@(Entry seqNo _ev), hp') ->
                if (seqNo == snum)
                then ((hp', seqNo), Just ent)
                else (hp, Nothing)

-------------------------------------------------------------------------------
-- WAhead
-------------------------------------------------------------------------------

-- XXX To be implemented. Use a linked queue like WAsync and put back the
-- remaining computation at the back of the queue instead of the heap, and
-- increment the sequence number.

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
    liftIO $ modifyIORef (workerThreads sv) $ (\s -> S.delete tid s)

-- If present then delete else add. This takes care of out of order add and
-- delete i.e. a delete arriving before we even added a thread.
-- This occurs when the forked thread is done even before the 'addThread' right
-- after the fork gets a chance to run.
{-# INLINE modifyThread #-}
modifyThread :: MonadIO m => SVar t m a -> ThreadId -> m ()
modifyThread sv tid = do
    changed <- liftIO $ atomicModifyIORefCAS (workerThreads sv) $ \old ->
        if (S.member tid old)
        then let new = (S.delete tid old) in (new, new)
        else let new = (S.insert tid old) in (new, old)
    if null changed
    then liftIO $ do
        writeBarrier
        void $ tryPutMVar (outputDoorBell sv) ()
    else return ()

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

#ifdef DIAGNOSTICS
recordMaxWorkers :: MonadIO m => SVar t m a -> m ()
recordMaxWorkers sv = liftIO $ do
    active <- readIORef (workerCount sv)
    maxWrk <- readIORef (maxWorkers sv)
    when (active > maxWrk) $ writeIORef (maxWorkers sv) active
    modifyIORef (totalDispatches sv) (+1)
#endif

{-# NOINLINE pushWorker #-}
pushWorker :: MonadAsync m => Count -> SVar t m a -> m ()
pushWorker yieldMax sv = do
    liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
#ifdef DIAGNOSTICS
    recordMaxWorkers sv
#endif
    -- XXX we can make this allocation conditional, it might matter when
    -- significant number of workers are being sent.
    winfo <- do
            cntRef <- liftIO $ newIORef 0
            t <- liftIO $ getTime Monotonic
            lat <- liftIO $ newIORef (0, t)
            return $ WorkerInfo
                { workerYieldMax = yieldMax
                , workerYieldCount = cntRef
                , workerLatencyStart = lat
                }
    doFork (workLoop sv winfo) (handleChildException sv) >>= addThread sv

-- XXX we can push the workerCount modification in accountThread and use the
-- same pushWorker for Parallel case as well.
--
-- | In contrast to pushWorker which always happens only from the consumer
-- thread, a pushWorkerPar can happen concurrently from multiple threads on the
-- producer side. So we need to use a thread safe modification of
-- workerThreads. Alternatively, we can use a CreateThread event to avoid
-- using a CAS based modification.
{-# NOINLINE pushWorkerPar #-}
pushWorkerPar :: MonadAsync m => SVar t m a -> m () -> m ()
pushWorkerPar sv wloop = do
    -- We do not use workerCount in case of ParallelVar but still there is no
    -- harm in maintaining it correctly.
#ifdef DIAGNOSTICS
    liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
    recordMaxWorkers sv
#endif
    doFork wloop (handleChildException sv) >>= modifyThread sv

-- Returns:
-- True: can dispatch more
-- False: cannot dispatch any more
dispatchWorker :: MonadAsync m => Count -> SVar t m a -> m Bool
dispatchWorker yieldCount sv = do
    let workerLimit = maxWorkerLimit sv
    -- XXX in case of Ahead streams we should not send more than one worker
    -- when the work queue is done but heap is not done.
    done <- liftIO $ isWorkDone sv
    if (not done)
    then do
        -- Note that the worker count is only decremented during event
        -- processing in fromStreamVar and therefore it is safe to read and
        -- use it without a lock.
        active <- liftIO $ readIORef $ workerCount sv
        -- Note that we may deadlock if the previous workers (tasks in the
        -- stream) wait/depend on the future workers (tasks in the stream)
        -- executing. In that case we should either configure the maxWorker
        -- count to higher or use parallel style instead of ahead or async
        -- style.
        limit <- case remainingYields sv of
            Nothing -> return workerLimit
            Just x -> do
                n <- liftIO $ readIORef x
                return $
                    case workerLimit of
                        Unlimited -> Limited (fromIntegral n)
                        Limited lim -> Limited $ min lim (fromIntegral n)

        -- XXX for ahead streams shall we take the heap yields into account for
        -- controlling the dispatch? We should not dispatch if the heap has
        -- already got the limit covered.
        let dispatch = pushWorker yieldCount sv >> return True
         in case limit of
            Unlimited -> dispatch
            -- Note that the use of remainingYields and workerCount is not
            -- atomic and the counts may even have changed between reading and
            -- using them here, so this is just approximate logic and we cannot
            -- rely on it for correctness. We may actually dispatch more
            -- workers than required.
            Limited lim | active < (fromIntegral lim) -> dispatch
            _ -> return False
    else return False

-- | This is a magic number and it is overloaded, and used at several places to
-- achieve batching:
--
-- 1. If we have to sleep to slowdown this is the minimum period that we
--    accumulate before we sleep. Also, workers do not stop until this much
--    sleep time is accumulated.
-- 3. Collected latencies are computed and transferred to measured latency
--    after a minimum of this period.
minThreadDelay :: NanoSecs
minThreadDelay = 10^(6 :: Int)

-- | Another magic number! When we have to start more workers to cover up a
-- number of yields that we are lagging by then we cannot start one worker for
-- each yield because that may be a very big number and if the latency of the
-- workers is low these number of yields could be very high. We use this number
-- as the duration in which we want cover any lag. In other words we accumulate
-- the lag (lagging number of yields) such that it covers this much duration
-- for a single worker to produce those yields, before we start an extra
-- worker.
coverUpLatency :: NanoSecs
coverUpLatency = 1000000

nanoToMicroSecs :: NanoSecs -> Int
nanoToMicroSecs s = (fromIntegral s) `div` 1000

-- XXX we can use phantom types to distinguish the duration/latency/expectedLat
countDispatchWorkers
    :: Limit -> Count -> NanoSecs -> NanoSecs -> NanoSecs -> Int
countDispatchWorkers workerLimit count duration wLatency expectedLat =
    -- Calculate how many workers do we need to maintain the rate.

    -- XXX Increasing the workers may reduce the overall latency per
    -- yield of the workers, sometimes quite a bit. We can keep a
    -- latency histogram for 1,2,4..2^20 workers so that we can decide
    -- which bucket is optimal and decide the number of workers based
    -- on that.
    --
    -- XXX put a cap on extraworkers? Should not be more than nWorkers.
    -- Bridge the gap slowly rather than in a burst in one go.
    --
    -- XXX we can have a maxEfficiency combinator as well which runs the
    -- producer at the maximal efficiency i.e. the number of workers are chosen
    -- such that the latency is minimum or within a range. Or we can call it
    -- maxLatency.
    --
    let nWorkers = (fromIntegral wLatency) / (fromIntegral expectedLat)
        -- Calculate how many yields are we ahead or behind to match the exact
        -- required rate. Based on that we increase or decrease the effective
        -- workers.
        d = fromIntegral duration
        e = fromIntegral expectedLat
        n = max 1 (fromIntegral coverUpLatency / fromIntegral wLatency)
                :: Double
        extraYields = (d / e) - (fromIntegral count)
        extraWorkers = extraYields / n
        requiredWorkers = round $ nWorkers + extraWorkers
    in case workerLimit of
        Unlimited -> requiredWorkers
        Limited x -> min requiredWorkers (fromIntegral x)

-- | Get the worker latency without resetting workerCurrentLatency
-- Returns (total yield count, base time, measured latency)
getWorkerLatency :: YieldRateInfo -> IO (Count, TimeSpec, NanoSecs)
getWorkerLatency yinfo  = do
    let cur      = workerCurrentLatency yinfo
        col      = workerCollectedLatency yinfo
        longTerm = workerLongTermLatency yinfo
        measured = workerMeasuredLatency yinfo

    (count, time)       <- readIORef cur
    (colCount, colTime) <- readIORef col
    (lcount, ltime)     <- readIORef longTerm
    prev                <- readIORef measured

    let pendingCount = colCount + count
        pendingTime  = colTime + time
        -- XXX Take the average of previous and the latest
        new =
            if pendingCount > 0
            then ((pendingTime `div` (fromIntegral pendingCount)) + prev)
                    `div` 2
            else prev
    return (lcount + pendingCount, ltime, new)

isBeyondMaxRate :: SVar t m a -> YieldRateInfo -> IO Bool
isBeyondMaxRate sv yinfo = do
    (count, tstamp, wLatency) <- getWorkerLatency yinfo
    now <- getTime Monotonic
    let duration = fromInteger $ toNanoSecs $ now - tstamp
    let expectedLat = expectedYieldLatency yinfo
    let netWorkers = countDispatchWorkers (maxWorkerLimit sv) count duration
                                          wLatency expectedLat
    cnt <- readIORef $ workerCount sv
    if netWorkers <= 0
    then do
        let sleepTime = ((fromIntegral count) * expectedLat) - duration
        if sleepTime >= minThreadDelay
        then return True
        else return False
    else if cnt > netWorkers
         then return True
         else return False

useCollectedBatch :: YieldRateInfo
                  -> NanoSecs
                  -> IORef (Count, NanoSecs)
                  -> IORef NanoSecs
                  -> IO ()
useCollectedBatch yinfo latency colRef measRef = do
    let periodRef = workerLatencyPeriod yinfo
        cnt = max 1 $ minThreadDelay `div` latency
        period = min cnt (fromIntegral magicMaxBuffer)

    -- XXX we should use some weight for the old latency as well in arriving at
    -- the new latency, we have the counts, we can use that for some weighting.
    writeIORef periodRef (fromIntegral period)
    writeIORef colRef (0, 0)
    writeIORef measRef latency

-- Returns a triple, (1) yield count since last collection, (2) the base time
-- when we started counting, (3) average latency in the last measurement
-- period. The former two are used for accurate measurement of the going rate
-- whereas the average is used for future estimates e.g. how many workers
-- should be maintained to maintain the rate.
collectLatency :: YieldRateInfo -> IO (Count, TimeSpec, NanoSecs)
collectLatency yinfo = do
    let cur      = workerCurrentLatency yinfo
        col      = workerCollectedLatency yinfo
        longTerm = workerLongTermLatency yinfo
        measured = workerMeasuredLatency yinfo

    (count, time)       <- atomicModifyIORefCAS cur $ \v -> ((0,0), v)
    (colCount, colTime) <- readIORef col
    (lcount, ltime)     <- readIORef longTerm
    prev                <- readIORef measured

    let pendingCount = colCount + count
        pendingTime  = colTime + time

        lcount' = lcount + pendingCount
        notUpdated = (lcount', ltime, prev)

    if (pendingCount > 0)
    then do
        let new = pendingTime `div` (fromIntegral pendingCount)
        -- To avoid minor fluctuations update in batches
        if     (pendingCount > fromIntegral magicMaxBuffer)
            || (pendingTime > minThreadDelay)
            || (let r = (fromIntegral new) / (fromIntegral prev) :: Double
                 in prev > 0 && (r > 2 || r < 0.5))
            || (prev == 0)
        then do
            useCollectedBatch yinfo new col measured
            modifyIORef longTerm $ \(_, t) -> (lcount', t)
            return (lcount', ltime, new)
        else do
            writeIORef col (pendingCount, pendingTime)
            return notUpdated
    else return notUpdated

adjustLatencies :: YieldRateInfo -> IO ()
adjustLatencies yinfo = do
    let longTerm = workerLongTermLatency yinfo
    t0 <- readIORef (workerStopTimeStamp yinfo)
    t1 <- getTime Monotonic
    modifyIORef longTerm $ \(c, t) -> (c, t + t1 - t0)
    return ()

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
    let workerLimit = maxWorkerLimit sv
    let yinfo = fromJust $ yieldRateInfo sv
    (count, tstamp, wLatency) <- liftIO $ collectLatency yinfo
    -- XXX use workerBootstrapLatency
    if wLatency == 0
    -- Need to measure the latency with a single worker before we can perform
    -- any computation.
    then return False
    else do
        now <- liftIO $ getTime Monotonic
        let duration = fromInteger $ toNanoSecs $ now - tstamp
        let expectedLat = expectedYieldLatency yinfo
        let netWorkers = countDispatchWorkers workerLimit count duration
                                              wLatency expectedLat

        -- XXX we need to take yieldLimit into account here. If we are at the
        -- end of the limit as well as the time, we should not be sleeping.
        -- If we are not actually planning to dispatch any more workers we need
        -- to take that in account.
        if netWorkers <= 0
        then do
            let sleepTime = ((fromIntegral count) * expectedLat) - duration

            when (sleepTime >= minThreadDelay) $ do
                liftIO $ threadDelay $ nanoToMicroSecs sleepTime

            cnt <- liftIO $ readIORef $ workerCount sv
            when (cnt < 1) $ void $ do
                let n = countDispatchYields (expectedLat - wLatency)
                 in dispatchWorker (fromIntegral (max n 1)) sv
            return False
        else do
            -- XXX stagger the workers over a period
            cnt <- liftIO $ readIORef $ workerCount sv
            if (cnt < netWorkers)
            then dispatchWorker 0 sv
            else return False

    where

    countDispatchYields :: NanoSecs -> Int
    countDispatchYields idleTime =
        assert (idleTime >= 0) $
        -- we do not want to sleep for less than minThreadDelay
        let maxYieldsPerWorker = fromIntegral magicMaxBuffer
         in if idleTime < minThreadDelay
            then
                if idleTime == 0
                then maxYieldsPerWorker
                else
                    let n = (minThreadDelay + idleTime) `div` idleTime
                     in min (fromIntegral n) maxYieldsPerWorker
            else 1

sendWorkerDelayPaced :: SVar t m a -> IO ()
sendWorkerDelayPaced _ = return ()

sendWorkerDelay :: SVar t m a -> IO ()
sendWorkerDelay sv = do
    -- XXX we need a better way to handle this than hardcoded delays. The
    -- delays may be different for different systems.
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
    -- by a outputDoorBell, when the worker exits.

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
        -- worker, before returning, clears the heap or we send a worker to clear
        -- it. Normally we always send a worker if no output is seen, but if
        -- the thread limit is reached or we are using pacing then we may not
        -- send a worker. See the concurrentApplication test in the tests, that
        -- test case requires at least one yield from the producer to not
        -- deadlock, if the last workers output is stuck in the heap then this
        -- test fails.  This problem can be extended to n threads when the
        -- consumer may depend on the evaluation of next n items in the
        -- producer stream.

        -- register for the outputDoorBell before we check the queue so that if we
        -- sleep because the queue was empty we are guaranteed to get a
        -- doorbell on the next enqueue.

        liftIO $ atomicModifyIORefCAS_ (needDoorBell sv) $ const True
        liftIO $ storeLoadBarrier
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
            liftIO $ withDBGMVar sv "sendWorkerWait: nothing to do"
                             $ takeMVar (outputDoorBell sv)
            (_, len) <- liftIO $ readIORef (outputQueue sv)
            when (len <= 0) $ sendWorkerWait delay dispatch sv

{-# INLINE readOutputQRaw #-}
readOutputQRaw :: SVar t m a -> IO ([ChildEvent a], Int)
readOutputQRaw sv = do
    (list, len) <- atomicModifyIORefCAS (outputQueue sv) $ \x -> (([],0), x)
#ifdef DIAGNOSTICS
    oqLen <- readIORef (maxOutQSize sv)
    when (len > oqLen) $ writeIORef (maxOutQSize sv) len
#endif
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
            when (not done) $ pushWorker 0 sv

    {-# INLINE blockingRead #-}
    blockingRead = do
        sendWorkerWait sendWorkerDelay (dispatchWorker 0) sv
        liftIO $ (readOutputQRaw sv >>= return . fst)

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
        liftIO $ (readOutputQRaw sv >>= return . fst)

postProcessBounded :: MonadAsync m => SVar t m a -> m Bool
postProcessBounded sv = do
    workersDone <- allThreadsDone sv
    -- There may still be work pending even if there are no workers pending
    -- because all the workers may return if the outputQueue becomes full. In
    -- that case send off a worker to kickstart the work again.
    if workersDone
    then do
        r <- liftIO $ isWorkDone sv
        -- XXX do we need to dispatch many here?
        when (not r) $ pushWorker 0 sv
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
            let yinfo = fromJust $ yieldRateInfo sv
            liftIO $ adjustLatencies yinfo
            void $ dispatchWorkerPaced sv
        return r
    else return False

getYieldRateInfo :: State t m a -> IO (Maybe YieldRateInfo)
getYieldRateInfo st = do
    case getMaxStreamRate st of
        Just rate -> do
            let latency = round $ 1.0e9 / rate
            measured <- newIORef 0
            wcur     <- newIORef (0,0)
            wcol     <- newIORef (0,0)
            now      <- getTime Monotonic
            wlong    <- newIORef (0,now)
            period   <- newIORef 1
            stopTime <- newIORef $ fromNanoSecs 0

            return $ Just YieldRateInfo
                { expectedYieldLatency   = latency
                , workerBootstrapLatency = getStreamLatency st
                , workerLatencyPeriod    = period
                , workerMeasuredLatency  = measured
                , workerCurrentLatency   = wcur
                , workerStopTimeStamp    = stopTime
                , workerCollectedLatency = wcol
                , workerLongTermLatency  = wlong
                }
        Nothing -> return Nothing

getAheadSVar :: MonadAsync m
    => State t m a
    -> (   IORef ([t m a], Int)
        -> IORef (Heap (Entry Int (AheadHeapEntry t m a)), Int)
        -> State t m a
        -> SVar t m a
        -> WorkerInfo
        -> m ())
    -> IO (SVar t m a)
getAheadSVar st f = do
    outQ    <- newIORef ([], 0)
    outH    <- newIORef (H.empty, 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q <- newIORef ([], -1)
    stopMVar <- newMVar ()
    yl <- case getYieldLimit st of
            Nothing -> return Nothing
            Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st

#ifdef DIAGNOSTICS
    disp   <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif

    let getSVar sv readOutput postProc = SVar
            { outputQueue      = outQ
            , remainingYields  = yl
            , maxBufferLimit   = getMaxBuffer st
            , maxWorkerLimit   = getMaxThreads st
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = f q outH st{streamVar = Just sv} sv
            , enqueue          = enqueueAhead sv q
            , isWorkDone       = isWorkDoneAhead sv q outH
            , needDoorBell     = wfw
            , svarStyle        = AheadVar
            , workerCount      = active
            , accountThread    = delThread sv
            , workerStopMVar   = stopMVar
#ifdef DIAGNOSTICS
            , aheadWorkQueue   = q
            , outputHeap       = outH
            , totalDispatches  = disp
            , maxWorkers       = maxWrk
            , maxOutQSize      = maxOq
            , maxHeapSize      = maxHs
            , maxWorkQSize     = maxWq
#endif
            }

    let sv =
            case getMaxStreamRate st of
                Nothing -> getSVar sv readOutputQBounded postProcessBounded
                Just _  -> getSVar sv readOutputQPaced postProcessPaced
     in return sv

    where

    {-# INLINE isWorkDoneAhead #-}
    isWorkDoneAhead sv q ref = do
        heapDone <- do
                (hp, _) <- readIORef ref
                return (H.size hp <= 0)
        queueDone <- checkEmpty q
        yieldsDone <-
                case remainingYields sv of
                    Just yref -> do
                        n <- readIORef yref
                        return (n <= 0)
                    Nothing -> return False
        -- XXX note that yieldsDone can only be authoritative only when there
        -- are no workers running. If there are active workers they can
        -- increment the yield count and therefore change the result.
        return $ yieldsDone || (queueDone && heapDone)

    checkEmpty q = do
        (xs, _) <- readIORef q
        return $ null xs

getParallelSVar :: MonadIO m => IO (SVar t m a)
getParallelSVar = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    running <- newIORef S.empty
#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif
    let sv =
            SVar { outputQueue      = outQ
                 , remainingYields  = Nothing
                 , maxBufferLimit   = Unlimited
                 , maxWorkerLimit   = undefined
                 , yieldRateInfo    = Nothing
                 , outputDoorBell   = outQMv
                 , readOutputQ      = readOutputQPar sv
                 , postProcess      = allThreadsDone sv
                 , workerThreads    = running
                 , workLoop         = undefined
                 , enqueue          = undefined
                 , isWorkDone       = undefined
                 , needDoorBell     = undefined
                 , svarStyle        = ParallelVar
                 , workerCount      = active
                 , accountThread    = modifyThread sv
                 , workerStopMVar   = undefined
#ifdef DIAGNOSTICS
                 , aheadWorkQueue   = undefined
                 , outputHeap       = undefined
                 , totalDispatches  = disp
                 , maxWorkers       = maxWrk
                 , maxOutQSize      = maxOq
                 , maxHeapSize      = maxHs
                 , maxWorkQSize     = maxWq
#endif
                 }
     in return sv

    where

    readOutputQPar sv = liftIO $ do
        withDBGMVar sv "readOutputQPar: doorbell" $ takeMVar (outputDoorBell sv)
        readOutputQRaw sv >>= return . fst

sendFirstWorker :: MonadAsync m => SVar t m a -> t m a -> m (SVar t m a)
sendFirstWorker sv m = do
    -- Note: We must have all the work on the queue before sending the
    -- pushworker, otherwise the pushworker may exit before we even get a
    -- chance to push.
    liftIO $ enqueue sv m
    case yieldRateInfo sv of
        Nothing -> pushWorker 0 sv
        Just _  -> pushWorker 1 sv
    return sv

{-# INLINABLE newAheadVar #-}
newAheadVar :: MonadAsync m
    => State t m a
    -> t m a
    -> (   IORef ([t m a], Int)
        -> IORef (Heap (Entry Int (AheadHeapEntry t m a)), Int)
        -> State t m a
        -> SVar t m a
        -> WorkerInfo
        -> m ())
    -> m (SVar t m a)
newAheadVar st m wloop = do
    sv <- liftIO $ getAheadSVar st wloop
    sendFirstWorker sv m

{-# INLINABLE newParallelVar #-}
newParallelVar :: MonadAsync m => m (SVar t m a)
newParallelVar = liftIO $ getParallelSVar

-- XXX this errors out for Parallel/Ahead SVars
-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toStreamVar :: MonadAsync m => SVar t m a -> t m a -> m ()
toStreamVar sv m = do
    liftIO $ (enqueue sv) m
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
