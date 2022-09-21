-- |
-- Module      : Streamly.Internal.Data.Stream.Async.Channel.Interleave
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Async.Channel.Interleave
    (
      newChannel
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl, restoreM)
import Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, nullQ, tryPopR, pushL)
import Data.IORef (newIORef, readIORef)
import Streamly.Internal.Control.Concurrent
    (MonadRunInIO, MonadAsync, RunInIO(..), askRunInIO)
import Streamly.Internal.Data.Stream.Channel.Dispatcher (delThread)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Async.Channel.Consumer
import Streamly.Internal.Data.Stream.Async.Channel.Type
import Streamly.Internal.Data.Stream.Channel.Types

------------------------------------------------------------------------------
-- Creating a channel
------------------------------------------------------------------------------

data WorkerStatus = Continue | Suspend

-- XXX we can use the Ahead style sequence/heap mechanism to make the best
-- effort to always try to finish the streams on the left side of an expression
-- first as long as possible.

{-# INLINE enqueueFIFO #-}
enqueueFIFO ::
       Channel m a
    -> LinkedQueue (RunInIO m, K.Stream m a)
    -> (RunInIO m, K.Stream m a)
    -> IO ()
enqueueFIFO sv q m = do
    pushL q m
    ringDoorBell (needDoorBell sv) (outputDoorBell sv)

-- XXX we can remove sv as it is derivable from st

{-# INLINE workLoopFIFO #-}
workLoopFIFO
    :: MonadRunInIO m
    => LinkedQueue (RunInIO m, K.Stream m a)
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
        liftIO $ enqueueFIFO sv q (runInIO, r)
        return $ if res then Continue else Suspend

{-# INLINE workLoopFIFOLimited #-}
workLoopFIFOLimited
    :: forall m a. MonadRunInIO m
    => LinkedQueue (RunInIO m, K.Stream m a)
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
getFifoSVar :: forall m a. (MonadIO m, MonadBaseControl IO m) =>
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
            -> (LinkedQueue (RunInIO m, K.Stream m a)
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
            , enqueue          = enqueueFIFO sv q
            , isWorkDone       = workDone sv
            , isQueueDone      = workDone sv
            , needDoorBell     = wfw
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
                        Nothing -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinished
                                              workLoopFIFO
                        Just _  -> getSVar sv readOutputQBounded
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

-- | Create a new async style concurrent stream evaluation channel. The monad
-- state used to run the stream actions is taken from the call site of
-- newChannel.
{-# INLINABLE newChannel #-}
{-# SPECIALIZE newChannel :: (Config -> Config) -> IO (Channel IO a) #-}
newChannel :: (MonadAsync m) =>
    (Config -> Config) -> m (Channel m a)
newChannel modifier = do
    mrun <- askRunInIO
    liftIO $ getFifoSVar mrun (modifier defaultConfig)
