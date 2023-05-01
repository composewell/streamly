-- |
-- Module      : Streamly.Internal.Data.Fold.Concurrent.Channel.Type
-- Copyright   : (c) 2017, 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Fold.Concurrent.Channel.Type
    ( Channel (..)
    , newChannel
    , Config
    , sendToWorker
    , checkFoldStatus
    , dumpSVar
    )
where

#include "inline.hs"

import Control.Concurrent (ThreadId, myThreadId, tryPutMVar)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Exception (SomeException(..))
import Control.Monad (void)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (intersperse)
import Streamly.Internal.Control.Concurrent
    (MonadAsync, MonadRunInIO, askRunInIO)
import Streamly.Internal.Control.ForkLifted (doFork')
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Stream.Channel.Dispatcher (dumpSVarStats)
import Streamly.Internal.Data.Stream.Channel.Worker (sendWithDoorBell)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

import Streamly.Internal.Data.Stream.Channel.Types

data Channel m a b = Channel
    {
    -- FORWARD FLOW: Flow of data from the driver to the consuming fold

    -- XXX This is inputQueue instead.

    -- Shared output queue (events, length)
    --
    -- [LOCKING] Frequent locked access. This is updated by the driver on each
    -- yield and once in a while read by the consumer fold thread.
    --
    -- XXX Use a different type than ChildEvent. We can do with a simpler type
    -- in folds.
      outputQueue :: IORef ([ChildEvent a], Int)
    -- This is capped to maxBufferLimit if set to more than that. Otherwise
    -- potentially each worker may yield one value to the buffer in the worst
    -- case exceeding the requested buffer size.
    , maxBufferLimit :: Limit

    -- [LOCKING] Infrequent MVar. Used when the outputQ transitions from empty
    -- to non-empty.
    , outputDoorBell :: MVar ()  -- signal the consumer about output
    , readOutputQ :: m [ChildEvent a]

    -- receive async events from the fold consumer to the driver.
    , outputQueueFromConsumer :: IORef ([ChildEvent b], Int)
    , outputDoorBellFromConsumer :: MVar ()
    , bufferSpaceDoorBell :: MVar ()

    -- cleanup: to track garbage collection of SVar --
    , svarRef :: Maybe (IORef ())

    -- Stats --
    , svarStats :: SVarStats

    -- Diagnostics --
    , svarInspectMode :: Bool
    , svarCreator :: ThreadId
    }

{-# NOINLINE dumpSVar #-}
dumpSVar :: Channel m a b -> IO String
dumpSVar sv = do
    xs <- sequence $ intersperse (return "\n")
        [ return (dumpCreator (svarCreator sv))
        , return "---------CURRENT STATE-----------"
        , dumpOutputQ (outputQueue sv)
        -- XXX print the types of events in the outputQueue, first 5
        , dumpDoorBell (outputDoorBell sv)
        , return "---------STATS-----------\n"
        , dumpSVarStats (svarInspectMode sv) Nothing (svarStats sv)
        ]
    return $ concat xs

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
-- Fold worker <------Channel<------Fold driver
--     |  exceptions  |
--     --------------->
--
-- @
--
-- We need a channel for pushing exceptions from the fold worker to the fold
-- driver. The stream may be pushed to multiple folds at the same time. For
-- that we need one Channel per fold:
--
-- @
--
-- Fold worker <------Channel--
--                    |        |
-- Fold worker <------Channel------Driver
--                    |        |
-- Fold worker <------Channel--
--
-- @
--
-- Note: If the stream pusher terminates due to an exception, we do not
-- actively terminate the fold. It gets cleaned up by the GC.

-------------------------------------------------------------------------------
-- Process events received by a fold worker from a fold driver
-------------------------------------------------------------------------------

sendToDriver :: Channel m a b -> ChildEvent b -> IO Int
sendToDriver sv msg = do
    -- In case the producer stream is blocked on pushing to the fold buffer
    -- then wake it up so that it can check for the stop event or exception
    -- being sent to it otherwise we will be deadlocked.
    -- void $ tryPutMVar (pushBufferMVar sv) ()
    sendWithDoorBell (outputQueueFromConsumer sv)
                     (outputDoorBellFromConsumer sv) msg

sendYieldToDriver :: MonadIO m => Channel m a b -> b -> m ()
sendYieldToDriver sv res = liftIO $ do
    void $ sendToDriver sv (ChildYield res)

{-# NOINLINE sendExceptionToDriver #-}
sendExceptionToDriver :: Channel m a b -> SomeException -> IO ()
sendExceptionToDriver sv e = do
    tid <- myThreadId
    void $ sendToDriver sv (ChildStop tid (Just e))

data FromSVarState m a b =
      FromSVarRead (Channel m a b)
    | FromSVarLoop (Channel m a b) [ChildEvent a]

{-# INLINE_NORMAL fromProducerD #-}
fromProducerD :: MonadIO m => Channel m a b -> D.Stream m a
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
            ChildStopChannel -> return D.Stop
            _ -> undefined

{-# INLINE readOutputQChan #-}
readOutputQChan :: Channel m a b -> IO ([ChildEvent a], Int)
readOutputQChan chan = do
    let ss = if svarInspectMode chan then Just (svarStats chan) else Nothing
    r@(_, n) <- readOutputQRaw (outputQueue chan) ss
    if n <= 0
    then do
        liftIO
            $ withDiagMVar
                (svarInspectMode chan)
                (dumpSVar chan)
                "readOutputQChan: nothing to do"
            $ takeMVar (outputDoorBell chan)
        readOutputQRaw (outputQueue chan) ss
    else return r

{-# INLINE readOutputQDB #-}
readOutputQDB :: Channel m a b -> IO ([ChildEvent a], Int)
readOutputQDB chan = do
    r <- readOutputQChan chan
    -- XXX We can do this only if needed, if someone sleeps because of buffer
    -- then they can set a flag and we ring the doorbell only if the flag is
    -- set. Like we do in sendWorkerWait for streams.
    _ <- tryPutMVar (bufferSpaceDoorBell chan) ()
    return r

mkNewChannel :: forall m a b. MonadIO m => Config -> IO (Channel m a b)
mkNewChannel cfg = do
    outQ <- newIORef ([], 0)
    outQMv <- newEmptyMVar
    outQRev <- newIORef ([], 0)
    outQMvRev <- newEmptyMVar
    bufferMv <- newEmptyMVar

    stats <- newSVarStats
    tid <- myThreadId

    let getSVar :: Channel m a b -> Channel m a b
        getSVar sv = Channel
            { outputQueue      = outQ
            , outputDoorBell   = outQMv
            , outputQueueFromConsumer = outQRev
            , outputDoorBellFromConsumer = outQMvRev
            , bufferSpaceDoorBell = bufferMv
            , maxBufferLimit   = getMaxBuffer cfg
            , readOutputQ      = liftIO $ fmap fst (readOutputQDB sv)
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode cfg
            , svarCreator      = tid
            , svarStats        = stats
            }

    let sv = getSVar sv in return sv

{-# INLINABLE newChannel #-}
{-# SPECIALIZE newChannel ::
    (Config -> Config) -> Fold IO a b -> IO (Channel IO a b) #-}
newChannel :: (MonadRunInIO m) =>
    (Config -> Config) -> Fold m a b -> m (Channel m a b)
newChannel modifier f = do
    let config = modifier defaultConfig
    sv <- liftIO $ mkNewChannel config
    mrun <- askRunInIO
    void $ doFork' (getBound config) (work sv) mrun (sendExceptionToDriver sv)
    return sv

    where

    {-# NOINLINE work #-}
    work chan =
        let f1 = Fold.rmapM (void . sendYieldToDriver chan) f
         in D.fold f1 $ fromProducerD chan

-------------------------------------------------------------------------------
-- Process events received by the driver thread from the fold worker side
-------------------------------------------------------------------------------

-- XXX currently only one event is sent by a fold consumer to the stream
-- producer. But we can potentially have multiple events e.g. the fold step can
-- generate exception more than once and the producer can ignore those
-- exceptions or handle them and still keep driving the fold.

-- XXX In case of scan this could be a stream.

-- | Poll for events sent by the fold worker to the fold driver. The fold
-- consumer can send a "Stop" event or an exception. When a "Stop" is received
-- this function returns 'True'. If an exception is recieved then it throws the
-- exception.
--
{-# NOINLINE checkFoldStatus #-}
checkFoldStatus :: MonadAsync m => Channel m a b -> m (Maybe b)
checkFoldStatus sv = do
    (list, _) <- liftIO $ readOutputQBasic (outputQueueFromConsumer sv)
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    processEvents $ reverse list

    where

    {-# INLINE processEvents #-}
    processEvents [] = return Nothing
    processEvents (ev : _) = do
        case ev of
            ChildStop _ e -> maybe undefined throwM e
            ChildStopChannel -> undefined
            ChildYield b -> return (Just b)

{-# INLINE isBufferAvailable #-}
isBufferAvailable :: MonadIO m => Channel m a b -> m Bool
isBufferAvailable sv = do
    let limit = maxBufferLimit sv
    case limit of
        Unlimited -> return True
        Limited lim -> do
            (_, n) <- liftIO $ readIORef (outputQueue sv)
            return $ fromIntegral lim > n

-- | Push values from a driver to a fold worker via a Channel. Before pushing a
-- value to the Channel it polls for events received from the fold worker.  If a
-- stop event is received then it returns 'True' otherwise false.  Propagates
-- exceptions received from the fold wroker.
--
{-# INLINE sendToWorker #-}
sendToWorker :: MonadAsync m => Channel m a b -> a -> m (Maybe b)
sendToWorker chan a = go

    where

    -- Recursive function, should we use SPEC?
    go = do
        let qref = outputQueueFromConsumer chan
        status <- do
            (_, n) <- liftIO $ readIORef qref
            if n > 0
            then checkFoldStatus chan
            else return Nothing
        case status of
            Just _ -> return status
            Nothing -> do
                    r <- isBufferAvailable chan
                    if r
                    then do
                        liftIO
                            $ void
                            $ sendWithDoorBell
                                (outputQueue chan)
                                (outputDoorBell chan)
                                (ChildYield a)
                        return Nothing
                    else do
                        () <- liftIO $ takeMVar (bufferSpaceDoorBell chan)
                        go
