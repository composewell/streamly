{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.SVar.Eliminate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Eliminate a stream by distributing it to multiple SVars concurrently.
--
module Streamly.Internal.Data.Stream.SVar.Eliminate
    (
    -- * Concurrent Function Application
      toSVarParallel

    -- * Concurrent folds
    -- $concurrentFolds
    , newFoldSVar
    , newFoldSVarF

    , fromConsumer
    , pushToFold
    , teeToSVar
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId, takeMVar)
import Control.Monad (when, void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, writeIORef)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Control.ForkLifted (doFork)
import Streamly.Internal.Data.Atomics (atomicModifyIORefCAS_)
import Streamly.Internal.Data.Fold.SVar (write, writeLimited)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import qualified Streamly.Internal.Data.Stream as D
    (Stream(..), Step(..), fold)
import qualified Streamly.Internal.Data.StreamK as K
    (Stream, mkStream, foldStream, foldStreamShared, nilM)
import qualified Streamly.Internal.Data.Stream.Serial as Stream
    (fromStreamK, toStreamK)

import Streamly.Internal.Data.SVar

-------------------------------------------------------------------------------
-- Concurrent function application
-------------------------------------------------------------------------------

-- Using StreamD the worker stream producing code can fuse with the code to
-- queue output to the SVar giving some perf boost.
--
-- Note that StreamD can only be used in limited situations, specifically, we
-- cannot implement joinStreamVarPar using this.
--
-- XXX make sure that the SVar passed is a Parallel style SVar.

-- | Fold the supplied stream to the SVar asynchronously using Parallel
-- concurrency style.
-- {-# INLINE_NORMAL toSVarParallel #-}
{-# INLINE toSVarParallel #-}
toSVarParallel :: MonadAsync m
    => State t m a -> SVar t m a -> D.Stream m a -> m ()
toSVarParallel st sv xs =
    if svarInspectMode sv
    then forkWithDiag
    else do
        tid <-
                case getYieldLimit st of
                    Nothing -> doFork (work Nothing)
                                      (svarMrun sv)
                                      (handleChildException sv)
                    Just _  -> doFork (workLim Nothing)
                                      (svarMrun sv)
                                      (handleChildException sv)
        modifyThread sv tid

    where

    {-# NOINLINE work #-}
    work info = D.fold (write sv info) xs

    {-# NOINLINE workLim #-}
    workLim info = D.fold (writeLimited sv info) xs

    {-# NOINLINE forkWithDiag #-}
    forkWithDiag = do
        -- We do not use workerCount in case of ParallelVar but still there is
        -- no harm in maintaining it correctly.
        liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
        recordMaxWorkers sv
        -- This allocation matters when significant number of workers are being
        -- sent. We allocate it only when needed. The overhead increases by 4x.
        winfo <-
            case yieldRateInfo sv of
                Nothing -> return Nothing
                Just _ -> liftIO $ do
                    cntRef <- newIORef 0
                    t <- getTime Monotonic
                    lat <- newIORef (0, t)
                    return $ Just WorkerInfo
                        { workerYieldMax = 0
                        , workerYieldCount = cntRef
                        , workerLatencyStart = lat
                        }
        tid <-
            case getYieldLimit st of
                Nothing -> doFork (work winfo)
                                  (svarMrun sv)
                                  (handleChildException sv)
                Just _  -> doFork (workLim winfo)
                                  (svarMrun sv)
                                  (handleChildException sv)
        modifyThread sv tid

-------------------------------------------------------------------------------
-- Support for running folds concurrently
-------------------------------------------------------------------------------

-- $concurrentFolds
--
-- To run folds concurrently, we need to decouple the fold execution from the
-- stream production. We use the SVar to do that, we have a single worker
-- pushing the stream elements to the SVar and on the consumer side a fold
-- driver pulls the values and folds them.
--
-- @
--
-- Fold worker <------SVar<------input stream
--     |  exceptions  |
--     --------------->
--
-- @
--
-- We need a channel for pushing exceptions from the fold worker to the stream
-- pusher. The stream may be pushed to multiple folds at the same time. For
-- that we need one SVar per fold:
--
-- @
--
-- Fold worker <------SVar<---
--                    |       |
-- Fold worker <------SVar<------input stream
--                    |       |
-- Fold worker <------SVar<---
--
-- @
--
-- Unlike in case concurrent stream evaluation, the puller does not drive the
-- scheduling and concurrent execution of the stream. The stream is simply
-- pushed by the stream producer at its own rate. The fold worker just pulls it
-- and folds it.
--
-- Note: If the stream pusher terminates due to an exception, we do not
-- actively terminate the fold. It gets cleaned up by the GC.

-------------------------------------------------------------------------------
-- Process events received by a fold consumer from a stream producer
-------------------------------------------------------------------------------

-- | Pull a stream from an SVar to fold it. Like 'fromSVar' except that it does
-- not drive the evaluation of the stream. It just pulls whatever is available
-- on the SVar. Also, when the fold stops it sends a notification to the stream
-- pusher/producer. No exceptions are expected to be propagated from the stream
-- pusher to the fold puller.
--
{-# NOINLINE fromProducer #-}
fromProducer :: forall m a . MonadAsync m => SVar K.Stream m a -> K.Stream m a
fromProducer sv = K.mkStream $ \st yld sng stp -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    K.foldStream st yld sng stp $ processEvents $ reverse list

    where

    allDone :: m r -> m r
    allDone stp = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        sendStopToProducer sv
        stp

    {-# INLINE processEvents #-}
    processEvents :: [ChildEvent a] -> K.Stream m a
    processEvents [] = K.mkStream $ \st yld sng stp -> do
        K.foldStream st yld sng stp $ fromProducer sv

    processEvents (ev : es) = K.mkStream $ \_ yld _ stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> allDone stp
                    Just _ -> error "Bug: fromProducer: received exception"

-- | Create a Fold style SVar that runs a supplied fold function as the
-- consumer.  Any elements sent to the SVar are consumed by the supplied fold
-- function.
--
{-# INLINE newFoldSVar #-}
newFoldSVar :: MonadAsync m
    => State K.Stream m a -> (SerialT m a -> m b) -> m (SVar K.Stream m a)
newFoldSVar stt f = do
    -- Buffer size for the SVar is derived from the current state
    sv <- newParallelVar StopAny (adaptState stt)

    -- Add the producer thread-id to the SVar.
    liftIO myThreadId >>= modifyThread sv

    void $ doFork (void $ f $ Stream.fromStreamK $ fromProducer sv)
                  (svarMrun sv)
                  (handleFoldException sv)
    return sv

data FromSVarState t m a =
      FromSVarInit
    | FromSVarRead (SVar t m a)
    | FromSVarLoop (SVar t m a) [ChildEvent a]
    | FromSVarDone (SVar t m a)

-- | Like 'fromProducer' but generates a StreamD style stream instead of
-- StreamK.
--
{-# INLINE_NORMAL fromProducerD #-}
fromProducerD :: (MonadAsync m) => SVar t m a -> D.Stream m a
fromProducerD svar = D.Stream step (FromSVarRead svar)
    where

    {-# INLINE_LATE step #-}
    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ D.Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = return $ D.Skip $ FromSVarRead sv
    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ D.Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        sendStopToProducer sv
                        return $ D.Skip (FromSVarDone sv)
                    Just _ -> error "Bug: fromProducer: received exception"

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return D.Stop

    step _ FromSVarInit = undefined

-- | Like 'newFoldSVar' except that it uses a 'Fold' instead of a fold
-- function.
--
{-# INLINE newFoldSVarF #-}
newFoldSVarF :: MonadAsync m => State t m a -> Fold m a b -> m (SVar t m a)
newFoldSVarF stt f = do
    -- Buffer size for the SVar is derived from the current state
    sv <- newParallelVar StopAny (adaptState stt)
    -- Add the producer thread-id to the SVar.
    liftIO myThreadId >>= modifyThread sv
    void $ doFork (work sv) (svarMrun sv) (handleFoldException sv)
    return sv

    where

    {-# NOINLINE work #-}
    work sv = void $ D.fold f $ fromProducerD sv

-------------------------------------------------------------------------------
-- Process events received by the producer thread from the consumer side
-------------------------------------------------------------------------------

-- XXX currently only one event is sent by a fold consumer to the stream
-- producer. But we can potentially have multiple events e.g. the fold step can
-- generate exception more than once and the producer can ignore those
-- exceptions or handle them and still keep driving the fold.
--
-- | Poll for events sent by the fold consumer to the stream pusher. The fold
-- consumer can send a "Stop" event or an exception. When a "Stop" is received
-- this function returns 'True'. If an exception is recieved then it throws the
-- exception.
--
{-# NOINLINE fromConsumer #-}
fromConsumer :: MonadAsync m => SVar K.Stream m a -> m Bool
fromConsumer sv = do
    (list, _) <- liftIO $ readOutputQBasic (outputQueueFromConsumer sv)
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    processEvents $ reverse list

    where

    {-# INLINE processEvents #-}
    processEvents [] = return False
    processEvents (ev : _) = do
        case ev of
            ChildStop _ e -> do
                case e of
                    Nothing -> return True
                    Just ex -> throwM ex
            ChildYield _ -> error "Bug: fromConsumer: invalid ChildYield event"

-- | Push values from a stream to a fold worker via an SVar. Before pushing a
-- value to the SVar it polls for events received from the fold consumer.  If a
-- stop event is received then it returns 'True' otherwise false.  Propagates
-- exceptions received from the fold consumer.
--
{-# INLINE pushToFold #-}
pushToFold :: MonadAsync m => SVar K.Stream m a -> a -> m Bool
pushToFold sv a = do
    -- Check for exceptions before decrement so that we do not
    -- block forever if the child already exited with an exception.
    --
    -- We avoid a race between the consumer fold sending an event and we
    -- blocking on decrementBufferLimit by waking up the producer thread in
    -- sendToProducer before any event is sent by the fold to the producer
    -- stream.
    let qref = outputQueueFromConsumer sv
    done <- do
        (_, n) <- liftIO $ readIORef qref
        if n > 0
        then fromConsumer sv
        else return False
    if done
    then return True
    else liftIO $ do
        decrementBufferLimit sv
        void $ send sv (ChildYield a)
        return False

------------------------------------------------------------------------------
-- Clone and distribute a stream in parallel
------------------------------------------------------------------------------

-- XXX this could be written in StreamD style for better efficiency with fusion.
--
-- | Tap a stream and send the elements to the specified SVar in addition to
-- yielding them again. The SVar runs a fold consumer. Elements are tapped and
-- sent to the SVar until the fold finishes. Any exceptions from the fold
-- evaluation are propagated in the current thread.
--
-- @
--
-- ------input stream---------output stream----->
--                    /|\\   |
--         exceptions  |    |  input
--                     |   \\|/
--                     ----SVar
--                          |
--                         Fold
--
-- @
--
{-# INLINE teeToSVar #-}
teeToSVar :: MonadAsync m =>
    SVar K.Stream m a -> SerialT m a -> SerialT m a
teeToSVar svr m = Stream.fromStreamK $ K.mkStream $ \st yld sng stp -> do
    K.foldStreamShared st yld sng stp (go False $ Stream.toStreamK m)

    where

    go False m0 = K.mkStream $ \st yld _ stp -> do
        let drain = do
                -- In general, a Stop event would come equipped with the result
                -- of the fold. It is not used here but it would be useful in
                -- applicative and distribute.
                done <- fromConsumer svr
                when (not done) $ do
                    liftIO $ withDiagMVar svr "teeToSVar: waiting to drain"
                           $ takeMVar (outputDoorBellFromConsumer svr)
                    drain

            stopFold = do
                liftIO $ sendStop svr Nothing
                -- drain/wait until a stop event arrives from the fold.
                drain

            stop       = stopFold >> stp
            single a   = do
                done <- pushToFold svr a
                yld a (go done (K.nilM stopFold))
            yieldk a r = pushToFold svr a >>= \done -> yld a (go done r)
         in K.foldStreamShared st yieldk single stop m0

    go True m0 = m0
