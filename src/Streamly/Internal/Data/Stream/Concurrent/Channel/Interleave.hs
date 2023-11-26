-- |
-- Module      : Streamly.Internal.Data.Stream.Concurrent.Channel.Interleave
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Concurrent.Channel.Interleave
    (
      newInterleaveChannel
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, nullQ, tryPopR, pushL)
import Data.IORef (newIORef, readIORef)
import Streamly.Internal.Control.Concurrent
    (MonadRunInIO, MonadAsync, RunInIO(..), askRunInIO, restoreM)
import Streamly.Internal.Data.Channel.Dispatcher (delThread)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.StreamK as K

import Streamly.Internal.Data.Stream.Concurrent.Channel.Consumer
import Streamly.Internal.Data.Stream.Concurrent.Channel.Type
import Streamly.Internal.Data.Channel.Types

------------------------------------------------------------------------------
-- Creating a channel
------------------------------------------------------------------------------

data WorkerStatus = Continue | Suspend

-- XXX This is not strictly round-robin as the streams that are faster may
-- yield more elements than the ones that are slower. Also, when streams
-- suspend due to buffer getting full they get added to the queue in a random
-- order.

{-# INLINE enqueueFIFO #-}
enqueueFIFO ::
       Channel m a
    -> LinkedQueue (RunInIO m, K.StreamK m a)
    -> (RunInIO m, K.StreamK m a)
    -> IO ()
enqueueFIFO sv q m = do
    pushL q m
    ringDoorBell (doorBellOnWorkQ sv) (outputDoorBell sv)

{-# INLINE workLoopFIFO #-}
workLoopFIFO
    :: MonadRunInIO m
    => LinkedQueue (RunInIO m, K.StreamK m a)
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
workLoopFIFO q sv winfo = run

    where

    run = do
        work <- liftIO $ tryPopR q
        case work of
            Nothing -> liftIO $ stop sv winfo
            Just (RunInIO runin, m) -> do
                r <- liftIO
                        $ runin
                        $ K.foldStreamShared
                            undefined yieldk single (return Continue) m
                res <- restoreM r
                case res of
                    Continue -> run
                    Suspend -> liftIO $ stop sv winfo

    single a = do
        res <- liftIO $ yield sv winfo a
        return $ if res then Continue else Suspend

    -- XXX in general we would like to yield "n" elements from a single stream
    -- before moving on to the next. Single element granularity could be too
    -- expensive in certain cases. Similarly, we can use time limit for
    -- yielding.
    yieldk a r = do
        res <- liftIO $ yield sv winfo a
        runInIO <- askRunInIO
        -- XXX If the queue is empty we do not need to enqueue. We can just
        -- continue evaluating the stream.
        liftIO $ enqueueFIFO sv q (runInIO, r)
        return $ if res then Continue else Suspend

{-# INLINE workLoopFIFOLimited #-}
workLoopFIFOLimited
    :: forall m a. MonadRunInIO m
    => LinkedQueue (RunInIO m, K.StreamK m a)
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
workLoopFIFOLimited q sv winfo = run

    where

    incrContinue =
        liftIO (incrementYieldLimit (remainingWork sv)) >> return Continue

    run = do
        work <- liftIO $ tryPopR q
        case work of
            Nothing -> liftIO $ stop sv winfo
            Just (RunInIO runin, m) -> do
                yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
                if yieldLimitOk
                then do
                    r <- liftIO
                            $ runin
                            $ K.foldStreamShared
                                undefined yieldk single incrContinue m
                    res <- restoreM r
                    case res of
                        Continue -> run
                        Suspend -> liftIO $ stop sv winfo
                else liftIO $ do
                    enqueueFIFO sv q (RunInIO runin, m)
                    incrementYieldLimit (remainingWork sv)
                    stop sv winfo

    single a = do
        res <- liftIO $ yield sv winfo a
        return $ if res then Continue else Suspend

    yieldk a r = do
        res <- liftIO $ yield sv winfo a
        runInIO <- askRunInIO
        liftIO $ enqueueFIFO sv q (runInIO, r)
        yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
        if res && yieldLimitOk
        then return Continue
        else liftIO $ do
            incrementYieldLimit (remainingWork sv)
            return Suspend

-------------------------------------------------------------------------------
-- SVar creation
-------------------------------------------------------------------------------

-- XXX we have this function in this file because passing runStreamLIFO as a
-- function argument to this function results in a perf degradation of more
-- than 10%.  Need to investigate what the root cause is.
-- Interestingly, the same thing does not make any difference for Ahead.
getFifoSVar :: forall m a. MonadRunInIO m =>
    RunInIO m -> Config -> IO (Channel m a)
getFifoSVar mrun cfg = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef Set.empty
    q       <- newQ
    yl      <- case getYieldLimit cfg of
                Nothing -> return Nothing
                Just x -> Just <$> newIORef x
    rateInfo <- newRateInfo cfg

    stats <- newSVarStats
    tid <- myThreadId

    let isWorkFinished _ = nullQ q
    let isWorkFinishedLimited sv = do
            yieldsDone <-
                    case remainingWork sv of
                        Just ref -> do
                            n <- readIORef ref
                            return (n <= 0)
                        Nothing -> return False
            qEmpty <- nullQ q
            return $ qEmpty || yieldsDone

    let getSVar :: Channel m a
            -> (Channel m a -> m [ChildEvent a])
            -> (Channel m a -> m Bool)
            -> (Channel m a -> IO Bool)
            -> (LinkedQueue (RunInIO m, K.StreamK m a)
                -> Channel m a
                -> Maybe WorkerInfo
                -> m())
            -> Channel m a
        getSVar sv readOutput postProc workDone wloop = Channel
            { outputQueue      = outQ
            , remainingWork    = yl
            , maxBufferLimit   = getMaxBuffer cfg
            , maxWorkerLimit   = min (getMaxThreads cfg) (getMaxBuffer cfg)
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = wloop q sv
            , enqueue          = \_ -> enqueueFIFO sv q
            , eagerDispatch    = return ()
            , isWorkDone       = workDone sv
            , isQueueDone      = workDone sv
            , doorBellOnWorkQ  = wfw
            , svarMrun         = mrun
            , workerCount      = active
            , accountThread    = delThread running
            , workerStopMVar   = undefined
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode cfg
            , svarCreator      = tid
            , svarStats        = stats
            }

    let sv =
            case getStreamRate cfg of
                Nothing ->
                    case getYieldLimit cfg of
                        Nothing -> getSVar sv (readOutputQBounded False)
                                              postProcessBounded
                                              isWorkFinished
                                              workLoopFIFO
                        Just _  -> getSVar sv (readOutputQBounded False)
                                              postProcessBounded
                                              isWorkFinishedLimited
                                              workLoopFIFOLimited
                Just _  ->
                    case getYieldLimit cfg of
                        Nothing -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinished
                                              workLoopFIFO
                        Just _  -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinishedLimited
                                              workLoopFIFOLimited
     in return sv

-- XXX GHC: If instead of MonadAsync we use (MonadIO m, MonadBaseControl IO m)
-- constraint we get a 2x perf regression. Need to look into that.
--
-- | Create a new async style concurrent stream evaluation channel. The monad
-- state used to run the stream actions is taken from the call site of
-- newInterleaveChannel.
{-# INLINABLE newInterleaveChannel #-}
{-# SPECIALIZE newInterleaveChannel :: (Config -> Config) -> IO (Channel IO a) #-}
newInterleaveChannel :: MonadAsync m =>
    (Config -> Config) -> m (Channel m a)
newInterleaveChannel modifier = do
    mrun <- askRunInIO
    liftIO $ getFifoSVar mrun (modifier defaultConfig)
