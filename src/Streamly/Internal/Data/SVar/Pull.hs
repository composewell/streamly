-- |
-- Module      : Streamly.Internal.Data.SVar.Pull
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.SVar.Pull
    (
    -- * Read Output
      readOutputQBasic
    , readOutputQRaw
    , readOutputQPaced
    , readOutputQBounded

    -- * Postprocess Hook After Reading
    , postProcessPaced
    , postProcessBounded

    -- * Release Resources
    , cleanupSVar
    , cleanupSVarFromWorker
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId, throwTo)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (readIORef, writeIORef)
import Data.IORef (IORef)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS)

import qualified Data.Set as S

import Streamly.Internal.Data.SVar.Type
import Streamly.Internal.Data.SVar.Dispatch

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
