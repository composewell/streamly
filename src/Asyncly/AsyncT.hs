{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Asyncly.AsyncT
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Asyncly.AsyncT
    ( AsyncT (..)
    , MonadAsync
--    , async
--    , makeAsync
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Concurrent          (ThreadId, forkIO, myThreadId)
import           Control.Concurrent.STM      (TChan, atomically, newTChan,
                                              readTChan, writeTChan)
import           Control.Exception           (fromException, try,
                                              SomeException (..),
                                              BlockedIndefinitelyOnSTM(..))
import qualified Control.Exception.Lifted    as EL
import           Control.Monad               (ap, liftM, MonadPlus(..), mzero)
--import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
{-
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM, liftBaseWith)
                                              -}
import           Data.Maybe                  (maybe)
import           Data.Monoid                 ((<>))

import           Control.Monad.Trans.Recorder (MonadRecorder(..))


------------------------------------------------------------------------------
-- Concurrency Semantics
------------------------------------------------------------------------------
--
-- Asyncly is essentially a concurrent list transformer. To concatenate the
-- lists it provides three different ways of composing them. The Monoid
-- instance concatenates lists in a non-concurrent in-order fashion. The
-- Alternative instance concatenates list in a concurrent manner potentially
-- running each action in parallel and therefore the order of the resulting
-- items is not deterministic. Computations in Alternative composition may or
-- may not run in parallel. Thirdly, the <||> operator provides a way to
-- compose computations that are guaranteed to run in parallel. This provides a
-- way to run infinite loops or long running computations without blocking
-- or starving other computations.

------------------------------------------------------------------------------
-- Parent child thread communication types
------------------------------------------------------------------------------

-- XXX for threads that yield a single value we can pack yield and done in
-- ChildDone (Either exception value) so that we do not use two messages over
-- the channel where we can use just one.
data ChildEvent a = ChildYield a | ChildDone ThreadId (Maybe SomeException)

------------------------------------------------------------------------------
-- State threaded around the monad for thread management
------------------------------------------------------------------------------

newtype Context a = Context { childChannel :: TChan (ChildEvent a) }

-- The 'Maybe (AsyncT m a)' is redundant as we can use 'stop' value for the
-- Nothing case, but it makes the fold using '<|>' 25% faster. Need to try
-- again as the code has gone through many changes after this was tested.
-- With the Maybe, 'stop' is required only to represent 'empty' in an
-- Alternative composition.
--
-- Currently the only state we need is the thread context, For generality we
-- can parameterize the type with a state type 's'.
newtype AsyncT m a =
    AsyncT {
        runAsyncT :: forall r.
               Maybe (Context a)                            -- state
            -> m r                                          -- stop
            -> (a -> Maybe (Context a) -> Maybe (AsyncT m a) -> m r)  -- yield
            -> m r
    }

type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

-- | Appends the results of two AsyncT computations in order.
instance Monad m => Monoid (AsyncT m a) where
    mempty = AsyncT $ \_ stp _ -> stp
    mappend (AsyncT m1) m2 = AsyncT $ \ctx stp yld ->
        m1 ctx ((runAsyncT m2) ctx stp yld) $ \a c r ->
            let yield x = yld a c (Just x)
            in maybe (yield m2) (\rx -> yield $ mappend rx m2) r

-- We do not use bind for parallelism. That is, we do not start each iteration
-- of the list in parallel. That will introduce too much uncontrolled
-- parallelism. Instead we control parallelism using <|>, it is used for
-- actions that 'may' be run in parallel. The library decides whether they will
-- be started in parallel. This puts the parallel semantics in the hands of the
-- user and operational semantics in the hands of the library. Ideally we would
-- want parallelism to be completely hidden from the user, for example we can
-- automatically decide where to start an action in parallel so that it is most
-- efficient. We can decide whether a particular bind should fork a parallel
-- thread or not based on the level of the bind in the tree. Maybe at some
-- point we shall be able to do that?

instance Monad m => Monad (AsyncT m) where
    return a = AsyncT $ \ctx _ yld -> yld a ctx Nothing

    -- | A thread context is valid only until the next bind. Upon a bind we
    -- reset the context to Nothing.
    AsyncT m >>= f = AsyncT $ \_ stp yld ->
        m Nothing stp $ \a _ r ->
            let run x = (runAsyncT x) Nothing stp yld
                next = f a
            in maybe (run next) (\rx -> run $ next <> (rx >>= f)) r

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (AsyncT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (AsyncT m) where
    pure  = return
    (<*>) = ap

------------------------------------------------------------------------------
-- Alternative
------------------------------------------------------------------------------

{-# NOINLINE doFork #-}
doFork :: (MonadIO m, MonadBaseControl IO m)
    => m ()
    -> (SomeException -> IO ())
    -> m ThreadId
doFork action preExit =
    EL.mask $ \restore ->
        liftBaseWith $ \runInIO -> forkIO $ do
            _ <- runInIO $ EL.catch (restore action) (liftIO . preExit)
            -- XXX restore state here?
            return ()

{-# NOINLINE push #-}
push :: MonadIO m => Context a -> AsyncT m a -> m ()
push context action = run (Just context) action

    where

    run ctx m = (runAsyncT m) ctx channelDone yield

    -- XXX make this a bounded channel so that we block if the previous value
    -- is not consumed yet.
    chan           = childChannel context
    channelYield a = liftIO $ atomically $ writeTChan chan (ChildYield a)
    channelDone    = do
        tid <- liftIO myThreadId
        liftIO $ atomically $ writeTChan chan (ChildDone tid Nothing)

    done a           = channelYield a >> channelDone
    continue a ctx m = channelYield a >> run ctx m
    yield a ctx r    = maybe (done a) (\rx -> continue a ctx rx) r

-- If an exception occurs we push it to the channel so that it can handled by
-- the parent.  'Paused' exceptions are to be collected at the top level.
-- XXX Paused exceptions should only bubble up to the runRecorder computation
{-# NOINLINE handlePushException #-}
handlePushException :: TChan (ChildEvent a) -> SomeException -> IO ()
handlePushException pchan e = do
    tid <- myThreadId
    atomically $ writeTChan pchan (ChildDone tid (Just e))

-- | run m1 in a new thread, pushing its results to a pull channel and then run
-- m2 in the parent thread. Any exceptions are also pushed to the channel.
{-# INLINE pushFork #-}
pushFork :: (MonadIO m, MonadBaseControl IO m)
    => Context a -> AsyncT m a -> AsyncT m a -> AsyncT m a
pushFork c m1 m2 = AsyncT $ \ctx stp yld -> do
    _ <- doFork (push c m1) (handlePushException (childChannel c))
    -- XXX run m2 only when the channel becomes empty
    (runAsyncT m2) ctx stp yld

-- We re-raise any exceptions received from the child threads, that way
-- exceptions get propagated to the top level computation and can be handled
-- there.
{-# NOINLINE pull #-}
pull :: (MonadIO m, MonadThrow m) => Context a -> AsyncT m a
pull ctx = AsyncT $ \_ stp yld -> do
    -- XXX push the try out of the pull loop
    res <- liftIO $ try (atomically $ readTChan (childChannel ctx))
    case res of
        Left e ->
            case fromException e of
                Just BlockedIndefinitelyOnSTM -> stp
                Nothing -> throwM e
        Right ev ->
            case ev of
                ChildYield a -> yld a Nothing (Just (pull ctx))
                ChildDone _ e -> do
                    let continue = (runAsyncT (pull ctx)) (Just ctx) stp yld
                    maybe continue throwM e

-- | Split the original computation in a pull-push pair. The original
-- computation pulls from a Channel while m1 and m2 push to the channel.
{-# INLINE pullFork #-}
pullFork :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
pullFork m1 m2 = AsyncT $ \_ stp yld -> do
    -- XXX assert ctx is Nothing
    c <- liftIO $ newContext
    _ <- doFork (pushBoth c m1 m2) (handlePushException (childChannel c))
    (runAsyncT (pull c)) (Just c) stp yld

    where

    newContext = do
        childChan  <- atomically newTChan
        return $ Context { childChannel  = childChan }

    pushBoth c ma mb = do
        _ <- doFork (push c ma) (handlePushException (childChannel c))
        -- XXX push from m2 only when the channel becomes empty
        push c mb

canFork :: IO Bool
canFork = return True

-- Concurrency rate control. Our objective is to create more threads on
-- demand if the consumer is running faster than us. As soon as we
-- encounter an Alternative composition we create a push pull pair of
-- threads. We use a channel for communication between the consumer that
-- pulls from the channel and the producer that pushes to the channel. The
-- producer creates more threads if the channel becomes empty at times,
-- that is the consumer is running faster. However this mechanism can be
-- problematic if the initial production latency is high, we may end up
-- creating too many threads. So we need some way to monitor and use the
-- latency as well.
-- TBD For quick response we may have to increase the rate in the middle of
-- a serially running computation. For that we can use a state flag to fork
-- the rest of the computation at any point of time inside the Monad bind
-- operation if the consumer is running at a faster speed.
--
{-# NOINLINE fork #-}
fork :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
fork m1 m2 = AsyncT $ \ctx stp yld -> do
    case ctx of
        Nothing -> (runAsyncT (pullFork m1 m2)) ctx stp yld
        Just c -> (runAsyncT (pushFork c m1 m2)) ctx stp yld

instance MonadAsync m => Alternative (AsyncT m) where
    empty = mempty

    m1 <|> m2 = AsyncT $ \ctx stp yld -> do
        doAsync <- liftIO $ canFork
        if doAsync then
            (runAsyncT $ fork m1 m2) ctx stp yld
        else (runAsyncT (m1 <> m2)) ctx stp yld

instance MonadAsync m => MonadPlus (AsyncT m) where
    mzero = empty
    mplus = (<|>)

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Num a, Monad (AsyncT m)) => Num (AsyncT m a) where
  fromInteger = return . fromInteger
  mf + mg     = (+) <$> mf <*> mg
  mf * mg     = (*) <$> mf <*> mg
  negate f    = f >>= return . negate
  abs f       = f >>= return . abs
  signum f    = f >>= return . signum

-------------------------------------------------------------------------------
-- AsyncT transformer
-------------------------------------------------------------------------------

instance MonadTrans AsyncT where
    lift mx = AsyncT $ \ctx _ yld -> mx >>= (\a -> (yld a ctx Nothing))

{-
instance (MonadBase b m, Monad m) => MonadBase b (AsyncT m) where
    liftBase = liftBaseDefault

-------------------------------------------------------------------------------
-- monad-control
-------------------------------------------------------------------------------

instance MonadTransControl AsyncT where
    type StT AsyncT a = (Maybe a, Context)
    liftWith f = AsyncT $ StateT $ \s ->
                   liftM (\x -> (Just x, s))
                         (f $ \t -> runStateT (runAsyncT t) s)
    restoreT = AsyncT . StateT . const
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m, Monad m) => MonadBaseControl b (AsyncT m) where
    type StM (AsyncT m) a = ComposeSt AsyncT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}
-}

------------------------------------------------------------------------------
-- Standard transformer instances
------------------------------------------------------------------------------

instance MonadIO m => MonadIO (AsyncT m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (AsyncT m) where
    throwM = lift . throwM

------------------------------------------------------------------------------
-- MonadRecorder
------------------------------------------------------------------------------

instance MonadRecorder m => MonadRecorder (AsyncT m) where
    getJournal = lift getJournal
    putJournal = lift . putJournal
    play = lift . play

------------------------------------------------------------------------------
-- Async primitives
------------------------------------------------------------------------------
--
-- Only those actions that are marked with 'async' are guaranteed to be
-- asynchronous. Asyncly is free to run other actions synchronously or
-- asynchronously and it should not matter to the semantics of the program, if
-- it does then use async to force.
--
-- Why not make async as default and ask the programmer to use a 'sync'
-- primitive to force an action to run synchronously? But then we would not
-- have the freedom to convert the async actions to sync dynamically. Note that
-- it is safe to convert a sync action to async but vice-versa is not true.
-- Converting an async to sync can cause change in semantics if the async
-- action was an infinite loop for example.
--
-- | In an 'Alternative' composition, force the action to run asynchronously.
-- The @\<|\>@ operator implies "can be parallel", whereas 'async' implies
-- "must be parallel". Note that outside an 'Alternative' composition 'async'
-- is not useful and should not be used.  Even in an 'Alternative' composition
-- 'async' is not useful as the last action as the last action always runs in
-- the current thread.
{-
async :: Monad m => AsyncT m a -> AsyncT m a
async action = AsyncT $ runAsyncTask True (runAsyncT action)

makeAsync :: Monad m => ((a -> m ()) -> m ()) -> AsyncT m a
makeAsync = AsyncT . makeCont
-}
