#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

-- |
-- Module      : Streamly.Internal.Data.Stream.Channel.Operations
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Channel.Operations
    (
    -- *** Reading Stream
      fromChannelK
    , fromChannel

    -- ** Enqueuing Work
    , toChannelK
    , toChannel
    )
where

#include "inline.hs"

import Control.Exception (fromException)
import Control.Monad (when)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import Streamly.Internal.Control.Concurrent
    (MonadAsync, MonadRunInIO, askRunInIO)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import System.Mem (performMajorGC)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.StreamK as K

import Streamly.Internal.Data.Channel.Types hiding (inspect)
import Streamly.Internal.Data.Stream.Channel.Dispatcher
import Streamly.Internal.Data.Stream.Channel.Type hiding (inspect)

import Prelude hiding (map, concat, concatMap)

#ifdef INSPECTION
import Control.Exception (Exception)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Typeable (Typeable)
import Test.Inspection (inspect, hasNoTypeClassesExcept)
#endif

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
-- XXX If we use toChannelK multiple times on a channel make sure the channel
-- does not go away before we use the subsequent ones.

-- | High level function to enqueue a work item on the channel. The fundamental
-- unit of work is a stream. Each stream enqueued on the channel is picked up
-- and evaluated by a worker thread. The worker evaluates the stream it picked
-- up serially. When multiple streams are queued on the channel each stream can
-- be evaluated concurrently by different workers.
--
-- Note that the items in each stream are not concurrently evaluated, streams
-- are fundamentally serial, therefore, elements in one particular stream will
-- be generated serially one after the other. Only two or more streams can be
-- run concurrently with each other.
--
-- See 'chanConcatMapK' for concurrent evaluation of each element of a stream.
-- Alternatively, you can wrap each element of the original stream into a
-- stream generating action and queue all those streams on the channel. Then
-- all of them would be evaluated concurrently. However, that would not be
-- streaming in nature, it would require buffering space for the entire
-- original stream. Prefer 'chanConcatMapK' for larger streams.
--
-- Items from each evaluated streams are queued to the same output queue of the
-- channel which can be read using 'fromChannelK'. 'toChannelK' can be called
-- multiple times to enqueue multiple streams on the channel.
--
{-# INLINE toChannelK #-}
toChannelK :: MonadRunInIO m => Channel m a -> K.StreamK m a -> m ()
toChannelK chan m = do
    runIn <- askRunInIO
    liftIO $ enqueue chan (runIn, m)

-- INLINE for fromStreamK/toStreamK fusion

-- | A wrapper over 'toChannelK' for 'Stream' type.
{-# INLINE toChannel #-}
toChannel :: MonadRunInIO m => Channel m a -> Stream m a -> m ()
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
fromChannelRaw :: (MonadIO m, MonadThrow m) => Channel m a -> K.StreamK m a
fromChannelRaw sv = K.MkStream $ \st yld sng stp -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    K.foldStream st yld sng stp $ processEvents $ reverse list

    where

    cleanup = do
        when (svarInspectMode sv) $ liftIO $ do
            t <- getTime Monotonic
            writeIORef (svarStopTime (svarStats sv)) (Just t)
            printSVar (dumpChannel sv) "SVar Done"

    {-# INLINE processEvents #-}
    processEvents [] = K.MkStream $ \st yld sng stp -> do
        done <- postProcess sv
        if done
        then cleanup >> stp
        else K.foldStream st yld sng stp $ fromChannelRaw sv

    processEvents (ev : es) = K.MkStream $ \st yld sng stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStopChannel -> do
                liftIO (cleanupSVar (workerThreads sv))
                cleanup >> stp
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> K.foldStream st yld sng stp rest
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                -- We terminate the loop after sending
                                -- ThreadAbort to workers so we should never
                                -- get it unless it is thrown from inside a
                                -- worker thread or by someone else to our
                                -- thread.
                                error "processEvents: got ThreadAbort"
                                -- K.foldStream st yld sng stp rest
                            Nothing -> do
                                liftIO (cleanupSVar (workerThreads sv))
                                cleanup >> throwM ex

#ifdef INSPECTION
-- Use of GHC constraint tuple (GHC.Classes.(%,,%)) in fromStreamVar leads to
-- space leak because the tuple gets allocated in every recursive call and each
-- allocation holds on to the previous allocation. This test is to make sure
-- that we do not use the constraint tuple type class.
--
inspect $ hasNoTypeClassesExcept 'fromChannelRaw
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

-- XXX Add a lock in the channel so that fromChannel cannot be called multiple
-- times.
--
-- XXX Add an option to block the consumer rather than stopping the stream if
-- the work queue gets over.

chanCleanupOnGc :: Channel m a -> IO ()
chanCleanupOnGc chan = do
    when (svarInspectMode chan) $ do
        r <- liftIO $ readIORef (svarStopTime (svarStats chan))
        when (isNothing r) $
            printSVar (dumpChannel chan) "Channel Garbage Collected"
    cleanupSVar (workerThreads chan)
    -- If there are any other channels referenced by this channel a GC will
    -- prompt them to be cleaned up quickly.
    when (svarInspectMode chan) performMajorGC

-- | Draw a stream from a concurrent channel. The stream consists of the
-- evaluated values from the input streams that were enqueued on the channel
-- using 'toChannelK'.
--
-- This is the event processing loop for the channel which does two
-- things, (1) dispatch workers, (2) process the events sent by the workers.
-- Workers are dispatched based on the channel's configuration settings.
--
-- The stream stops and the channel is shutdown if any of the following occurs:
--
-- * the work queue becomes empty
-- * channel's max yield limit is reached
-- * an exception is thrown by a worker
-- * 'shutdown' is called on the channel
--
-- Before the channel stops, all the workers are drained and no more workers
-- are dispatched. When the channel is garbage collected a 'ThreadAbort'
-- exception is thrown to all pending workers. If 'inspect' option is enabled
-- then channel's stats are printed on stdout when the channel stops.
--
-- CAUTION! This API must not be called more than once on a channel.
{-# INLINE fromChannelK #-}
fromChannelK :: MonadAsync m => Channel m a -> K.StreamK m a
fromChannelK chan =
    K.mkStream $ \st yld sng stp -> do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref (chanCleanupOnGc chan)

        startChannel chan
        -- We pass a copy of sv to fromStreamVar, so that we know that it has
        -- no other references, when that copy gets garbage collected "ref"
        -- will get garbage collected and our hook will be called.
        K.foldStreamShared st yld sng stp $
            fromChannelRaw chan{svarRef = Just ref}

-- | A wrapper over 'fromChannelK' for 'Stream' type.
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
                    printSVar (dumpChannel svar) "SVar Garbage Collected"
            cleanupSVar (workerThreads svar)
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
            ChildStopChannel -> do
                liftIO (cleanupSVar (workerThreads sv))
                return $ D.Skip (FromSVarDone sv)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> return $ D.Skip (FromSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ D.Skip (FromSVarLoop sv es)
                            Nothing -> do
                                liftIO (cleanupSVar (workerThreads sv))
                                throwM ex

    step _ (FromSVarDone sv) = do
        when (svarInspectMode sv) $ do
            t <- liftIO $ getTime Monotonic
            liftIO $ writeIORef (svarStopTime (svarStats sv)) (Just t)
            liftIO $ printSVar (dumpChannel sv) "SVar Done"
        return D.Stop
