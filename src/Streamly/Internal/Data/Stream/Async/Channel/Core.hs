#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- |
-- Module      : Streamly.Internal.Data.Stream.Async.Channel.Core
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

module Streamly.Internal.Data.Stream.Async.Channel.Core
    (
      newChannel
    , toChannel
    , toChannelK
    , fromChannel
    , fromChannelK
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Exception (fromException)
import Control.Monad (when)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl, restoreM)
import Data.IORef (IORef, newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import Streamly.Internal.Control.Concurrent
    (MonadRunInIO, MonadAsync, RunInIO(..), askRunInIO)
import Streamly.Internal.Data.Atomics
    (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import System.Mem (performMajorGC)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Async.Channel.Consumer
import Streamly.Internal.Data.Stream.Async.Channel.Dispatcher
import Streamly.Internal.Data.Stream.Async.Channel.Type
import Streamly.Internal.Data.Stream.Async.Channel.Worker

import Prelude hiding (map, concat, concatMap)

#if __GLASGOW_HASKELL__ < 810
#ifdef INSPECTION
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable (Typeable)
import Test.Inspection (inspect, hasNoTypeClassesExcept)
#endif
#endif

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
    ringDoorBell sv

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

    stop = liftIO $ sendStop sv winfo
    run = do
        work <- dequeue
        case work of
            Nothing -> stop
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
                    Suspend -> stop

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        return $ if res then Continue else Suspend

    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
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

    incrContinue = liftIO (incrementYieldLimit sv) >> return Continue
    stop = liftIO $ sendStop sv winfo
    run = do
        work <- dequeue
        case work of
            Nothing -> stop
            Just (RunInIO runin, m) -> do
                -- XXX This is just a best effort minimization of concurrency
                -- to the yield limit. If the stream is made of concurrent
                -- streams we do not reserve the yield limit in the constituent
                -- streams before executing the action. This can be done
                -- though, by sharing the yield limit ref with downstream
                -- actions via state passing. Just a todo.
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
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
                        Suspend -> stop
                -- Avoid any side effects, undo the yield limit decrement if we
                -- never yielded anything.
                else liftIO $ do
                    enqueueLIFO sv q (RunInIO runin, m)
                    incrementYieldLimit sv
                    sendStop sv winfo

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        return $ if res then Continue else Suspend

    -- XXX can we pass on the yield limit downstream to limit the concurrency
    -- of constituent streams.
    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if res && yieldLimitOk
        then K.foldStreamShared undefined yieldk single incrContinue r
        else do
            runInIO <- askRunInIO
            liftIO $ incrementYieldLimit sv
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
    rateInfo <- getYieldRateInfo cfg

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
                -- -> State Stream m a
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
            , accountThread    = delThread sv
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

-- type Channel m a = SVar K.Stream m a
-- data Config = Config

-- XXX openChannel?

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

------------------------------------------------------------------------------
-- Generating streams from a channel
------------------------------------------------------------------------------

-- $concurrentEval
--
-- Usually a channel is used to concurrently evaluate multiple actions in a
-- stream using many worker threads that push the results to the channel and a
-- single puller that pulls them from channel generating the evaluated stream.
--
-- @
--                  input stream
--                       |
--     <-----------------|<--------worker
--     |  exceptions     |
-- output stream <---Channel<------worker
--                       |
--                       |<--------worker
--
-- @
--
-- The puller itself schedules the worker threads based on demand.
-- Exceptions are propagated from the worker threads to the puller.

-------------------------------------------------------------------------------
-- Write a stream to a channel
-------------------------------------------------------------------------------

-- XXX Should be a Fold, singleton API could be called joinChannel, or the fold
-- can be called joinChannel.

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toChannelK :: (MonadIO m, MonadBaseControl IO m) =>
    Channel m a -> K.Stream m a -> m ()
toChannelK sv m = do
    runIn <- askRunInIO
    liftIO $ enqueue sv (runIn, m)

-- INLINE for fromStreamK/toStreamK fusion

-- | Send a stream to a given channel for concurrent evaluation.
{-# INLINE toChannel #-}
toChannel :: (MonadIO m, MonadBaseControl IO m) =>
    Channel m a -> Stream m a -> m ()
toChannel chan = toChannelK chan . Stream.toStreamK

{-
-- | Send a stream of streams to a concurrent channel for evaluation.
{-# INLINE joinChannel #-}
joinChannel :: Channel m a -> Fold m (Stream m a) ()
joinChannel = undefined
-}

-------------------------------------------------------------------------------
-- Read a stream from a channel
-------------------------------------------------------------------------------

-- | Pull a stream from an SVar.
{-# NOINLINE fromChannelRaw #-}
fromChannelRaw :: (MonadIO m, MonadThrow m) => Channel m a -> K.Stream m a
fromChannelRaw sv = K.MkStream $ \st yld sng stp -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    K.foldStream st yld sng stp $ processEvents $ reverse list

    where

    allDone stp = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        stp

    {-# INLINE processEvents #-}
    processEvents [] = K.MkStream $ \st yld sng stp -> do
        done <- postProcess sv
        if done
        then allDone stp
        else K.foldStream st yld sng stp $ fromChannelRaw sv

    processEvents (ev : es) = K.MkStream $ \st yld sng stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> K.foldStream st yld sng stp rest
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                K.foldStream st yld sng stp rest
                            Nothing -> liftIO (cleanupSVar sv) >> throwM ex

#if __GLASGOW_HASKELL__ < 810
#ifdef INSPECTION
-- Use of GHC constraint tuple (GHC.Classes.(%,,%)) in fromStreamVar leads to
-- space leak because the tuple gets allocated in every recursive call and each
-- allocation holds on to the previous allocation. This test is to make sure
-- that we do not use the constraint tuple type class.
--
inspect $ hasNoTypeClassesExcept 'fromStreamVar
    [ ''Monad
    , ''Applicative
    , ''MonadThrow
    , ''Exception
    , ''MonadIO
    , ''MonadBaseControl
    , ''Typeable
    , ''Functor
    ]
#endif
#endif

-- XXX fromChannel Should not be called multiple times, we can add a
-- safeguard for that. Or we can replicate the stream so that we can distribute
-- it to multiple consumers. or should we use an explicit dupChannel for that?

{-# INLINE fromChannelK #-}
fromChannelK :: MonadAsync m => Channel m a -> K.Stream m a
fromChannelK sv =
    K.mkStream $ \st yld sng stp -> do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook

        startChannel sv
        -- We pass a copy of sv to fromStreamVar, so that we know that it has
        -- no other references, when that copy gets garbage collected "ref"
        -- will get garbage collected and our hook will be called.
        K.foldStreamShared st yld sng stp $
            fromChannelRaw sv{svarRef = Just ref}
    where

    hook = do
        when (svarInspectMode sv) $ do
            r <- liftIO $ readIORef (svarStopTime (svarStats sv))
            when (isNothing r) $
                printSVar sv "SVar Garbage Collected"
        cleanupSVar sv
        -- If there are any SVars referenced by this SVar a GC will prompt
        -- them to be cleaned up quickly.
        when (svarInspectMode sv) performMajorGC

-- | Generate a stream of results from concurrent evaluations from a channel.
-- Evaluation of the channel does not start until this API is called. This API
-- must not be called more than once on a channel. It kicks off evaluation of
-- the channel by dispatching concurrent workers and ensures that as long there
-- is work queued on the channel workers are dispatched proportional to the
-- demand by the consumer.
--
{-# INLINE fromChannel #-}
fromChannel :: MonadAsync m => Channel m a -> Stream m a
fromChannel = Stream.fromStreamK . fromChannelK

data FromSVarState t m a =
      FromSVarInit
    | FromSVarRead (Channel m a)
    | FromSVarLoop (Channel m a) [ChildEvent a]
    | FromSVarDone (Channel m a)

-- | Like 'fromSVar' but generates a StreamD style stream instead of CPS.
--
{-# INLINE_NORMAL _fromChannelD #-}
_fromChannelD :: (MonadIO m, MonadThrow m) => Channel m a -> D.Stream m a
_fromChannelD svar = D.Stream step FromSVarInit
    where

    {-# INLINE_LATE step #-}
    step _ FromSVarInit = do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- when this copy of svar gets garbage collected "ref" will get
        -- garbage collected and our GC hook will be called.
        let sv = svar{svarRef = Just ref}
        return $ D.Skip (FromSVarRead sv)

        where

        {-# NOINLINE hook #-}
        hook = do
            when (svarInspectMode svar) $ do
                r <- liftIO $ readIORef (svarStopTime (svarStats svar))
                when (isNothing r) $
                    printSVar svar "SVar Garbage Collected"
            cleanupSVar svar
            -- If there are any SVars referenced by this SVar a GC will prompt
            -- them to be cleaned up quickly.
            when (svarInspectMode svar) performMajorGC

    step _ (FromSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ D.Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = do
        done <- postProcess sv
        return $ D.Skip $ if done
                      then FromSVarDone sv
                      else FromSVarRead sv

    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ D.Yield a (FromSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> return $ D.Skip (FromSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ D.Skip (FromSVarLoop sv es)
                            Nothing -> liftIO (cleanupSVar sv) >> throwM ex

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar sv "SVar Done"
        return D.Stop
