-- |
-- Module      : Streamly.Internal.Data.Stream.Async.Channel.Append
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The functions in this module are separated from the combinators using
-- these because of a GHC issue. We need to have newChannel specialized but
-- not inlined. If we keep it in the same module as its users we cannot achieve
-- that and the code becomes bloated. But if it is in a separate module we can
-- use INLINABLE and SPECIALIZE on it which makes it specialized but it is not
-- actually inlined.

module Streamly.Internal.Data.Stream.Async.Channel.Append
    (
      newChannel
    )
where

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl, restoreM)
import Data.IORef (IORef, newIORef, readIORef)
import Streamly.Internal.Control.Concurrent
    (MonadRunInIO, MonadAsync, RunInIO(..), askRunInIO)
import Streamly.Internal.Data.Atomics
    (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Streamly.Internal.Data.Stream.Channel.Dispatcher (delThread)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Async.Channel.Consumer
import Streamly.Internal.Data.Stream.Async.Channel.Type
import Streamly.Internal.Data.Stream.Channel.Types

------------------------------------------------------------------------------
-- Creating a channel
------------------------------------------------------------------------------

-- Note: For purely right associated expressions this queue should have at most
-- one element. It grows to more than one when we have left associcated
-- expressions. Large left associated compositions can grow this to a
-- large size
{-# INLINE enqueueLIFO #-}
enqueueLIFO ::
      Channel m a
   -> IORef [(RunInIO m, K.Stream m a)]
   -> (RunInIO m, K.Stream m a)
   -> IO ()
enqueueLIFO sv q m = do
    atomicModifyIORefCAS_ q $ \ms -> m : ms
    ringDoorBell (needDoorBell sv) (outputDoorBell sv)

data WorkerStatus = Continue | Suspend

{-# INLINE workLoopLIFO #-}
workLoopLIFO
    :: MonadRunInIO m
    => IORef [(RunInIO m, K.Stream m a)]
    -- -> State Stream m a
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
workLoopLIFO q sv winfo = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> liftIO $ stop sv winfo
            Just (RunInIO runin, m) -> do
                -- XXX when we finish we need to send the monadic state back to
                -- the parent so that the state can be merged back. We capture
                -- and return the state in the stop continuation.
                --
                -- Instead of using the run function we can just restore the
                -- monad state here. That way it can work easily for
                -- distributed case as well.
                r <- liftIO $ runin $
                        K.foldStreamShared
                            undefined
                            yieldk
                            single
                            (return Continue)
                            m
                res <- restoreM r
                case res of
                    Continue -> run
                    Suspend -> liftIO $ stop sv winfo

    single a = do
        res <- liftIO $ yield sv winfo a
        return $ if res then Continue else Suspend

    yieldk a r = do
        res <- liftIO $ yield sv winfo a
        if res
        then K.foldStreamShared undefined yieldk single (return Continue) r
        else do
            runInIO <- askRunInIO
            liftIO $ enqueueLIFO sv q (runInIO, r)
            return Suspend

    dequeue = liftIO $ atomicModifyIORefCAS q $ \case
                [] -> ([], Nothing)
                x : xs -> (xs, Just x)

-- We duplicate workLoop for yield limit and no limit cases because it has
-- around 40% performance overhead in the worst case.
--
-- XXX we can pass yinfo directly as an argument here so that we do not have to
-- make a check every time.
{-# INLINE workLoopLIFOLimited #-}
workLoopLIFOLimited
    :: forall m a. MonadRunInIO m
    => IORef [(RunInIO m, K.Stream m a)]
    -- -> State Stream m a
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
workLoopLIFOLimited q sv winfo = run

    where

    incrContinue =
        liftIO (incrementYieldLimit (remainingWork sv)) >> return Continue

    run = do
        work <- dequeue
        case work of
            Nothing -> liftIO $ stop sv winfo
            Just (RunInIO runin, m) -> do
                -- XXX This is just a best effort minimization of concurrency
                -- to the yield limit. If the stream is made of concurrent
                -- streams we do not reserve the yield limit in the constituent
                -- streams before executing the action. This can be done
                -- though, by sharing the yield limit ref with downstream
                -- actions via state passing. Just a todo.
                yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
                if yieldLimitOk
                then do
                    r <- liftIO $ runin $
                            K.foldStreamShared
                                undefined
                                yieldk
                                single
                                incrContinue
                                m
                    res <- restoreM r
                    case res of
                        Continue -> run
                        Suspend -> liftIO $ stop sv winfo
                -- Avoid any side effects, undo the yield limit decrement if we
                -- never yielded anything.
                else liftIO $ do
                    enqueueLIFO sv q (RunInIO runin, m)
                    incrementYieldLimit (remainingWork sv)
                    stop sv winfo

    single a = do
        res <- liftIO $ yield sv winfo a
        return $ if res then Continue else Suspend

    -- XXX can we pass on the yield limit downstream to limit the concurrency
    -- of constituent streams.
    yieldk a r = do
        res <- liftIO $ yield sv winfo a
        yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
        if res && yieldLimitOk
        then K.foldStreamShared undefined yieldk single incrContinue r
        else do
            runInIO <- askRunInIO
            liftIO $ incrementYieldLimit (remainingWork sv)
            liftIO $ enqueueLIFO sv q (runInIO, r)
            return Suspend

    dequeue = liftIO $ atomicModifyIORefCAS q $ \case
                [] -> ([], Nothing)
                x : xs -> (xs, Just x)

-------------------------------------------------------------------------------
-- SVar creation
-- This code belongs in SVar.hs but is kept here for perf reasons
-------------------------------------------------------------------------------

-- XXX we have this function in this file because passing runStreamLIFO as a
-- function argument to this function results in a perf degradation of more
-- than 10%.  Need to investigate what the root cause is.
-- Interestingly, the same thing does not make any difference for Ahead.
-- {-# INLINABLE getLifoSVar #-}
getLifoSVar :: forall m a. (MonadIO m, MonadBaseControl IO m) =>
    RunInIO m -> Config -> IO (Channel m a)
getLifoSVar mrun cfg = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef Set.empty
    q       <- newIORef ([] :: [(RunInIO m, K.Stream m a)])
    yl      <- case getYieldLimit cfg of
                Nothing -> return Nothing
                Just x -> Just <$> newIORef x
    rateInfo <- newRateInfo cfg

    stats <- newSVarStats
    tid <- myThreadId

    let isWorkFinished _ = null <$> readIORef q

    let isWorkFinishedLimited sv = do
            yieldsDone <-
                    case remainingWork sv of
                        Just ref -> do
                            n <- readIORef ref
                            return (n <= 0)
                        Nothing -> return False
            qEmpty <- null <$> readIORef q
            return $ qEmpty || yieldsDone

    let getSVar :: Channel m a
            -> (Channel m a -> m [ChildEvent a])
            -> (Channel m a -> m Bool)
            -> (Channel m a -> IO Bool)
            -> (IORef [(RunInIO m, K.Stream m a)]
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
            , enqueue          = enqueueLIFO sv q
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
                                              workLoopLIFO
                        Just _  -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
                Just _  ->
                    case getYieldLimit cfg of
                        Nothing -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinished
                                              workLoopLIFO
                        Just _  -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
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
    liftIO $ getLifoSVar mrun (modifier defaultConfig)
