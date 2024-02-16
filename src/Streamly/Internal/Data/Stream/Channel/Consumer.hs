-- |
-- Module      : Streamly.Internal.Data.Stream.Channel.Consumer
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Channel.Consumer
    (
    -- *** Reading Events
    -- | Low level functions used to build 'fromChannelK'.
      readOutputQBounded
    , readOutputQPaced
    , postProcessBounded
    , postProcessPaced
    )
where

import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (readIORef)
import Streamly.Internal.Control.Concurrent (MonadRunInIO)

import Streamly.Internal.Data.Channel.Dispatcher
import Streamly.Internal.Data.Channel.Types
import Streamly.Internal.Data.Stream.Channel.Dispatcher
import Streamly.Internal.Data.Stream.Channel.Type

-------------------------------------------------------------------------------
-- Reading from the workers' output queue/buffer
-------------------------------------------------------------------------------

{-# INLINE readOutputQChan #-}
readOutputQChan :: Channel m a -> IO ([ChildEvent a], Int)
readOutputQChan sv = do
    let ss = if svarInspectMode sv then Just (svarStats sv) else Nothing
     in readOutputQRaw (outputQueue sv) ss

-- | Read the channel's output queue. When there is no output dispatches
-- workers and waits for output (using 'sendWorkerWait'). Always ensures that
-- there is at least one outstanding worker.
--
-- To be used as 'readOutputQ' function for the channel.
readOutputQBounded :: MonadRunInIO m => Bool -> Channel m a -> m [ChildEvent a]
readOutputQBounded eagerEval sv = do
    (list, len) <- liftIO $ readOutputQChan sv
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
            when (not done) (forkWorker 0 sv)

    {-# INLINE blockingRead #-}
    blockingRead = do
        dispatchAllWait eagerEval sendWorkerDelay (dispatchWorker 0) sv
        liftIO (fst `fmap` readOutputQChan sv)

-- | Same as 'readOutputQBounded' but uses 'dispatchWorkerPaced' to
-- dispatch workers with rate control.
--
-- To be used as 'readOutputQ' function for the channel when rate control is
-- on.
readOutputQPaced :: MonadRunInIO m => Channel m a -> m [ChildEvent a]
readOutputQPaced sv = do
    (list, len) <- liftIO $ readOutputQChan sv
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
        dispatchAllWait False sendWorkerDelayPaced dispatchWorkerPaced sv
        liftIO (fst `fmap` readOutputQChan sv)

-- | If there is work to do dispatch as many workers as the target rate
-- requires.
--
-- To be used as 'postProcess' function for the channel when rate control is
-- enabled.
postProcessPaced :: MonadRunInIO m => Channel m a -> m Bool
postProcessPaced sv = do
    workersDone <- allThreadsDone (workerThreads sv)
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
            noWorker <- allThreadsDone (workerThreads  sv)
            when noWorker $ forkWorker 0 sv
        return r
    else return False

-- | If there is work to do ensure that we have at least one worker disptached.
--
-- To be used as 'postProcess' function for the channel.
postProcessBounded :: MonadRunInIO m => Channel m a -> m Bool
postProcessBounded sv = do
    workersDone <- allThreadsDone (workerThreads sv)
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
        when (not r) (forkWorker 0 sv)
        -- XXX do we need to dispatch many here?
        -- void $ dispatchWorker sv
        return r
    else return False
