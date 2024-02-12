-- |
-- Module      : Streamly.Internal.Data.Stream.Channel.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Channel.Type
    (
      Channel(..)
    , yieldWith
    , stopWith
    , exceptionWith
    , shutdown
    , dumpChannel
    )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException(..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (IORef)
import Data.List (intersperse)
import Data.Set (Set)
import Streamly.Internal.Control.Concurrent (RunInIO)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS_)
import Streamly.Internal.Data.Channel.Dispatcher (dumpSVarStats)
import Streamly.Internal.Data.Channel.Worker
    (sendYield, sendStop, sendEvent, sendException)
import Streamly.Internal.Data.StreamK (StreamK)

import Streamly.Internal.Data.Channel.Types

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
     -- | Runner for the monadic actions in the stream. Captures the monad
     -- state at the point where the channel was created and uses the same
     -- state to run all actions.
      svarMrun :: RunInIO m

    ---------------------------------------------------------------------------
    -- FORWARD FLOW: Flow of data from the workers to the consumer
    ---------------------------------------------------------------------------

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

    -- | (events, count): output queue of the channel. This is where the
    -- workers queue the results.
    --
    -- [LOCKING] Frequently locked. This is locked and updated by workers
    -- on each yield, and locked, updated by the consumer thread once in a
    -- while for reading. Workers' locking contention may be high if there are
    -- a large number of workers.
    , outputQueue :: IORef ([ChildEvent a], Int)

    -- | Door bell for workers to wakeup the consumer.
    --
    -- [LOCKING] Infrequently locked. Used only when the 'outputQueue'
    -- transitions from empty to non-empty, or a work item is queued by a
    -- worker to the work queue and 'doorBellOnWorkQ' is set by the consumer.
    , outputDoorBell :: MVar ()

    -- XXX Can we use IO instead of m here?
    , readOutputQ :: m [ChildEvent a] -- XXX remove
    , postProcess :: m Bool -- XXX remove

    ---------------------------------------------------------------------------
    -- Scheduling --
    ---------------------------------------------------------------------------

    -- Combined/aggregate parameters

    -- | This is capped to 'maxBufferLimit' if set to more than that. Otherwise
    -- potentially each worker may yield one value to the buffer in the worst
    -- case exceeding the requested buffer size.
    , maxWorkerLimit :: Limit

    -- | Maximum size of the 'outputQueue'. The actual worst case buffer could
    -- be double of this as the consumer may read the queue and the workers may
    -- fill it up even before the consumer has started consuming.
    , maxBufferLimit :: Limit

    -- | Tracks how many yields are remaining before the channel stops, used
    -- when 'maxYields' option is enabled.
    --
    -- [LOCKING] Read only access by consumer when dispatching a worker.
    -- Decremented by workers when picking work and undo decrement if the
    -- worker does not yield a value.
    , remainingWork :: Maybe (IORef Count)

    -- | Rate control information for the channel used when 'rate' control is
    -- enabled,
    , yieldRateInfo :: Maybe YieldRateInfo

    , enqueue        :: Bool -> (RunInIO m, StreamK m a) -> IO () -- XXX remove
    , eagerDispatch  :: m ()
    , isWorkDone     :: IO Bool
    , isQueueDone    :: IO Bool

    -- | When set to True, ring 'outputDoorBell' when a work item is queued on
    -- the work queue.
    , doorBellOnWorkQ :: IORef Bool
    , workLoop       :: Maybe WorkerInfo -> m () -- XXX remove

    -- | Tracks all active worker threads.
    --
    -- [LOCKING] Updated unlocked, only by consumer thread.
    , workerThreads :: IORef (Set ThreadId)

    -- | Total number of active worker threads.
    --
    -- [LOCKING] Updated locked, by consumer thread when dispatching a worker
    -- and by a worker threads when the thread stops. This is read without lock
    -- at several places where we want to rely on an approximate value.
    , workerCount :: IORef Int

    -- XXX Can we use IO instead of m here?
    , accountThread  :: ThreadId -> m ()

    -- | Used when 'ordered' is enabled.
    , workerStopMVar :: MVar ()

    ---------------------------------------------------------------------------
    -- cleanup --
    ---------------------------------------------------------------------------
    -- | IORef to call a cleanup function when the channel is garbage
    -- collected.
    , svarRef :: Maybe (IORef ())

    ---------------------------------------------------------------------------
    -- Stats --
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

-- | Yield a value to the channel. Worker latencies are collected in the
-- supplied 'WorkerInfo' record, and periodically pushed to the channel's
-- 'workerPendingLatency' stat. Worker latencies are updated in the channel
-- only if the channel has a 'YieldRateInfo' attached and the
-- 'workerPollingInterval' is non-zero.
--
-- Even unregistered workers can use this API.
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

-- | The worker stops yielding and exits. The final update of the collected
-- latency stats in 'WorkerInfo' are pushed to the channel. Upon receiving the
-- 'ChildStop' event the channel would remove the worker from its set of
-- registered workers.
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

-- | Like 'stopWith' but stops with the specified exception.
{-# INLINE exceptionWith #-}
exceptionWith :: Maybe WorkerInfo -> Channel m a -> SomeException -> IO ()
exceptionWith _winfo chan =
    sendException (outputQueue chan) (outputDoorBell chan)

-- | Shutdown the channel. Kill all the registered worker threads.
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
