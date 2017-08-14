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
import           Data.Maybe                  (fromJust, maybe)

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

-- The 'Maybe' is redundant as we can use 'stop' value in the Nothing case,
-- but it makes the fold using '<|>' 25% faster. Need to try again as the code
-- has gone through many changes after this was tested.
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

-- XXX make this (<>)
serially :: AsyncT m a -> AsyncT m a -> AsyncT m a
serially (AsyncT m1) m2 = AsyncT $ \ctx stp yld ->
    m1 ctx ((runAsyncT m2) ctx stp yld) $ \a c r ->
        let yield x = yld a c (Just x)
        in maybe (yield m2) (\rx -> yield $ serially rx m2) r

instance Monad m => Monad (AsyncT m) where
    return a = AsyncT $ \ctx _ yld -> yld a ctx Nothing

    -- | On bind the new computation does not inherit the parent thread
    -- context, it starts a new node in the tree.
    AsyncT m >>= f = AsyncT $ \_ stp yld ->
        m Nothing stp $ \a _ r ->
            let run x = (runAsyncT x) Nothing stp yld
                next = f a
            in maybe (run next) (\rx -> run $ serially next (rx >>= f)) r

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

newContext :: IO (Context a)
newContext = do
    childChan  <- atomically newTChan
    return $ Context { childChannel  = childChan }

doFork :: (MonadIO m, MonadBaseControl IO m)
    => m ()
    -> (Either SomeException () -> IO ())
    -> m ThreadId
doFork action preExit =
    EL.mask $ \restore ->
        liftBaseWith $ \runInIO -> forkIO $ do
            _ <- runInIO $ EL.try (restore action) >>= liftIO . preExit
            -- XXX restore state here
            return ()

-- XXX make this a bounded channel so that we block if the previous value is
-- not consumed yet.
{-# INLINE channelDone #-}
channelDone :: MonadIO m => TChan (ChildEvent a) -> m ()
channelDone chan = do
    tid <- liftIO myThreadId
    liftIO $ atomically $ writeTChan chan (ChildDone tid Nothing)

{-# INLINE channelYield #-}
channelYield :: MonadIO m => TChan (ChildEvent a) -> a -> m ()
channelYield chan a = liftIO $ atomically $ writeTChan chan (ChildYield a)

push :: MonadIO m => Context a -> AsyncT m a -> m ()
push ctx m = go (Just ctx) m

    where

    go c mx = (runAsyncT mx) c (channelDone pchan) yield
    pchan = childChannel ctx

    done a = do
        channelYield pchan a
        channelDone pchan

    -- XXX If other threads are running, we should yield to the channel only
    -- when the channel becomes empty so that we do not start more threads
    -- unnecessarily.
    {-# INLINE continue #-}
    continue a c mx = do
        channelYield pchan a
        go c mx

    {-# INLINE yield #-}
    yield a c r = maybe (done a) (\x -> continue a c x) r

-- If an exception occurs we push it to the channel so that it can handled by
-- the parent.  'Paused' exceptions are to be collected at the top level.
-- XXX Paused exceptions should only bubble up to the runRecorder computation
handlePushException :: TChan (ChildEvent a) -> Either SomeException () -> IO ()
handlePushException pchan res = do
    let r = case res of
                Left e -> Just e
                Right _ -> Nothing

    tid <- myThreadId
    atomically $ writeTChan pchan (ChildDone tid r)

canFork :: IO Bool
canFork = return True

-- XXX do not fork if the channel is never empty
-- We can fork if the channel goes empty sometimes
fork :: (MonadIO m, MonadBaseControl IO m) => AsyncT m a -> AsyncT m a
fork m = AsyncT $ \ctx stp yld -> do
    doAsync <- liftIO $ canFork
    let c = fromJust ctx -- XXX partial
    if doAsync then do
        _ <- doFork (push c m) (handlePushException (childChannel c))
        stp
    else (runAsyncT m) ctx stp yld

-- We re-raise any exceptions received from the child threads, that way
-- exceptions get propagated to the top level computation and can be handled
-- there.
pull :: (MonadIO m, MonadThrow m) => Context a -> AsyncT m a
pull ctx = AsyncT $ \_ stp yld -> do
    ethr <- liftIO $ try $ atomically $ readTChan (childChannel ctx)
    case ethr of
        -- XXX for immediate cleanup and for killing any blocked threads we
        -- need to track the children
        Left e -> case fromException e of
                    Just BlockedIndefinitelyOnSTM -> stp
                    Nothing -> throwM e
        Right ev ->
            case ev of
                ChildYield a -> yld a Nothing (Just (pull ctx))
                ChildDone _ e -> do
                    let continue = (runAsyncT (pull ctx)) (Just ctx) stp yld
                    maybe continue throwM e

pushpull :: MonadAsync m => AsyncT m a -> AsyncT m a
pushpull m = AsyncT $ \ctx stp yld ->
    case ctx of
        Nothing -> do
            c <- liftIO $ newContext
            -- XXX m must be a push type computation
            let comp = serially (fork m) (pull c)
            (runAsyncT comp) (Just c) stp yld
        Just _ -> (runAsyncT m) ctx stp yld

-- Child threads yield values to the parent's channel. The parent reads them
-- from there and yields to its consumer.
instance MonadAsync m => Alternative (AsyncT m) where
    empty = AsyncT $ \_ stp _ -> stp

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
    m1 <|> m2 = AsyncT $ \ctx stp yld ->
        let comp = pushpull $ serially (fork m1) m2
        in (runAsyncT comp) ctx stp yld

instance MonadAsync m => MonadPlus (AsyncT m) where
    mzero = empty
    mplus = (<|>)

-- | Appends the results of two AsyncT computations in order.
instance MonadAsync m => Monoid (AsyncT m a) where
    mempty      = empty
    mappend x y = serially x y

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

instance (Monad m, MonadRecorder m) => MonadRecorder (AsyncT m) where
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
