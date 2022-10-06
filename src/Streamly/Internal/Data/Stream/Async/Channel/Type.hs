-- |
-- Module      : Streamly.Internal.Data.Stream.Async.Channel.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Async.Channel.Type
    (
      Channel(..)
    , yield
    , stop
    , dumpSVar
    )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import Data.List (intersperse)
import Data.Set (Set)

import Streamly.Internal.Control.Concurrent (RunInIO)
import Streamly.Internal.Data.Stream.Channel.Dispatcher (dumpSVarStats)
import Streamly.Internal.Data.Stream.Channel.Worker (sendYield, sendStop)
import Streamly.Internal.Data.Stream.StreamK.Type (Stream)

import Streamly.Internal.Data.Stream.Channel.Types

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

{-# INLINE yield #-}
yield :: Channel m a -> Maybe WorkerInfo -> a -> IO Bool
yield sv winfo x =
    sendYield
        (maxBufferLimit sv)
        (maxWorkerLimit sv)
        (workerCount sv)
        winfo
        (yieldRateInfo sv)
        (outputQueue sv)
        (outputDoorBell sv)
        (ChildYield x)

{-# INLINE stop #-}
stop :: Channel m a -> Maybe WorkerInfo -> IO ()
stop sv winfo =
    sendStop
        (workerCount sv)
        winfo
        (yieldRateInfo sv)
        (outputQueue sv)
        (outputDoorBell sv)

{-# NOINLINE dumpSVar #-}
dumpSVar :: Channel m a -> IO String
dumpSVar sv = do
    xs <- sequence $ intersperse (return "\n")
        [ return (dumpCreator (svarCreator sv))
        , return "---------CURRENT STATE-----------"
        , dumpOutputQ (outputQueue sv)
        -- XXX print the types of events in the outputQueue, first 5
        , dumpDoorBell (outputDoorBell sv)
        , dumpNeedDoorBell (needDoorBell sv)
        , dumpRunningThreads (workerThreads sv)
        -- XXX print the status of first 5 threads
        , dumpWorkerCount (workerCount sv)
        , return "---------STATS-----------\n"
        , dumpSVarStats (svarInspectMode sv) (yieldRateInfo sv) (svarStats sv)
        ]
    return $ concat xs
