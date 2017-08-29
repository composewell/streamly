{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE EmptyCase                 #-}
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
    , runAsyncly
    , toList
    , interleave
    , (<=>)
    , parAhead
    , (<>|)
    , parLeft
    , (<|)
    , foldWith
    , foldMapWith
    , forEachWith
    , fromCallback
    )
where

import           Control.Applicative         (Alternative (..), liftA2)
import           Control.Concurrent          (ThreadId, forkIO, killThread,
                                              myThreadId, threadDelay)
import           Control.Concurrent.STM      (TBQueue, atomically, newTBQueue,
                                              tryReadTBQueue, writeTBQueue,
                                              isEmptyTBQueue, isFullTBQueue,
                                              peekTBQueue)
import           Control.Exception           (SomeException (..))
import qualified Control.Exception.Lifted    as EL
import           Control.Monad               (ap, liftM, MonadPlus(..), mzero,
                                              when)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.Error.Class   (MonadError(..))
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Reader.Class  (MonadReader(..))
import           Control.Monad.State.Class   (MonadState(..))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import           Data.Functor                (void)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              writeIORef, readIORef)
import           Data.Monoid                 ((<>))
import           Data.Set                    (Set)
import qualified Data.Set                    as S

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

data ChildEvent a =
      ChildYield a
    | ChildDone ThreadId a
    | ChildStop ThreadId (Maybe SomeException)
    | ChildCreate ThreadId

------------------------------------------------------------------------------
-- State threaded around the monad for thread management
------------------------------------------------------------------------------

data Context m a =
    Context { outputQueue    :: TBQueue (ChildEvent a)
            , workQueue      :: TBQueue (AsyncT m a)
            , runningThreads :: IORef (Set ThreadId)
            , doneThreads    :: IORef (Set ThreadId)
            }

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
               Maybe (Context m a)                          -- state
            -> m r                                          -- stop
            -> (a -> Maybe (Context m a) -> Maybe (AsyncT m a) -> m r)  -- yield
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
        let stop = (runAsyncT m2) ctx stp yld
            yield a c Nothing  = yld a c (Just m2)
            yield a c (Just r) = yld a c (Just (mappend r m2))
        in m1 ctx stop yield

-- | Interleaves the results of two 'AsyncT' streams
interleave :: AsyncT m a -> AsyncT m a -> AsyncT m a
interleave m1 m2 = AsyncT $ \ctx stp yld -> do
    let stop = (runAsyncT m2) ctx stp yld
        yield a c Nothing  = yld a c (Just m2)
        yield a c (Just r) = yld a c (Just (interleave m2 r))
    (runAsyncT m1) ctx stop yield

infixr 5 <=>

(<=>) :: AsyncT m a -> AsyncT m a -> AsyncT m a
(<=>) = interleave

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
        let run x = (runAsyncT x) Nothing stp yld
            yield a _ Nothing  = run $ f a
            yield a _ (Just r) = run $ f a <> (r >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (AsyncT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- Note: We cannot have the applicative instance run the two actions in
-- parallel. This is because the first action can always return empty and in
-- case it returns empty we should not be starting the second action. Therefore
-- we can only start the second action after getting a result from the first.
-- For running applicative actions in parallel we can use the async package and
-- lift the result into AsyncT.

instance Monad m => Applicative (AsyncT m) where
    pure  = return
    (<*>) = ap

------------------------------------------------------------------------------
-- Alternative
------------------------------------------------------------------------------

{-# INLINE doFork #-}
doFork :: (MonadIO m, MonadBaseControl IO m)
    => m ()
    -> (SomeException -> IO ())
    -> m ThreadId
doFork action exHandler =
    EL.mask $ \restore ->
        liftBaseWith $ \runInIO -> forkIO $ do
            -- XXX test the exception handling
            _ <- runInIO $ EL.catch (restore action) (liftIO . exHandler)
            -- XXX restore state here?
            return ()

{-# NOINLINE push #-}
push :: MonadAsync m => Context m a -> m ()
push ctx = run (Just ctx) (dequeueLoop ctx)

    where

    send msg = atomically $ writeTBQueue (outputQueue ctx) msg
    stop = do
        workQEmpty <- liftIO $ atomically $ isEmptyTBQueue (workQueue ctx)
        if (not workQEmpty) then push ctx
        else liftIO $ myThreadId >>= \tid -> send (ChildStop tid Nothing)
    yield a _ Nothing  = liftIO $ myThreadId >>= \tid -> send (ChildDone tid a)
    yield a c (Just r) = liftIO (send (ChildYield a)) >> run c r
    run c m            = (runAsyncT m) c stop yield

-- Thread tracking has a significant performance overhead (~20% on empty
-- threads, it will be lower for heavy threads). It is needed for two reasons:
--
-- 1) Killing threads on exceptions. Threads may not be allowed to go away by
-- themselves because they may run for significant times before going away or
-- worse they may be stuck in IO and never go away.
--
-- 2) To know when all threads are done. This can be acheived by detecting a
-- BlockedIndefinitelyOnSTM exception too. But we will have to trigger a GC to
-- make sure that we detect it promptly.
--
-- This is a bit messy because ChildCreate and ChildDone events can arrive out
-- of order in case of pushSideDispatch. Returns whether we are done draining
-- threads.
{-# INLINE accountThread #-}
accountThread :: MonadIO m
    => ThreadId -> IORef (Set ThreadId) -> IORef (Set ThreadId) -> m Bool
accountThread tid ref1 ref2 = liftIO $ do
    s1 <- readIORef ref1
    s2 <- readIORef ref2

    if (S.member tid s1) then do
        let r = S.delete tid s1
        writeIORef ref1 r
        return $ S.null r && S.null s2
    else do
        liftIO $ writeIORef ref2 (S.insert tid s2)
        return False

{-# NOINLINE addThread #-}
addThread :: MonadIO m => Context m a -> ThreadId -> m Bool
addThread ctx tid = accountThread tid (doneThreads ctx) (runningThreads ctx)

{-# INLINE delThread #-}
delThread :: MonadIO m => Context m a -> ThreadId -> m Bool
delThread ctx tid = accountThread tid (runningThreads ctx) (doneThreads ctx)

handleException :: (MonadIO m, MonadThrow m)
    => SomeException -> Context m a -> ThreadId -> m r
handleException e ctx tid = do
    void (delThread ctx tid)
    liftIO $ readIORef (runningThreads ctx) >>= mapM_ killThread
    throwM e

{-# NOINLINE sendWorkerWait #-}
sendWorkerWait :: MonadAsync m => Context m a -> m ()
sendWorkerWait ctx = do
    liftIO $ threadDelay 4
    let workQ = workQueue ctx
        outQ  = outputQueue ctx
    workQEmpty <- liftIO $ atomically $ isEmptyTBQueue workQ
    outQEmpty  <- liftIO $ atomically $ isEmptyTBQueue outQ
    when (not workQEmpty && outQEmpty) $ pushWorker ctx
    void $ liftIO $ atomically $ peekTBQueue (outputQueue ctx)

-- We re-raise any exceptions received from the child threads, that way
-- exceptions get propagated to the top level computation and can be handled
-- there.
{-# NOINLINE pullWorker #-}
pullWorker :: MonadAsync m => Context m a -> AsyncT m a
pullWorker ctx = AsyncT $ \pctx stp yld -> do
    let continue = (runAsyncT (pullWorker ctx)) pctx stp yld
        yield a  = yld a pctx (Just (pullWorker ctx))
        threadOp tid f finish cont = do
            done <- f ctx tid
            if done then finish else cont

    res <- liftIO $ atomically $ tryReadTBQueue (outputQueue ctx)
    case res of
        Nothing -> sendWorkerWait ctx >> continue
        Just ev ->
            case ev of
                ChildYield a -> yield a
                ChildDone tid a ->
                    threadOp tid delThread (yld a pctx Nothing) (yield a)
                ChildStop tid e ->
                    case e of
                        Nothing -> threadOp tid delThread stp continue
                        Just ex -> handleException ex ctx tid
                ChildCreate tid -> threadOp tid addThread stp continue

-- If an exception occurs we push it to the channel so that it can handled by
-- the parent.  'Paused' exceptions are to be collected at the top level.
-- XXX Paused exceptions should only bubble up to the runRecorder computation
{-# NOINLINE handleChildException #-}
handleChildException :: TBQueue (ChildEvent a) -> SomeException -> IO ()
handleChildException pchan e = do
    tid <- myThreadId
    atomically $ writeTBQueue pchan (ChildStop tid (Just e))

-- This function is different than "forkWorker" because we have to directly
-- insert the threadIds here and cannot use the channel to send ChildCreate
-- unlike on the push side.  If we do that, the first thread's done message
-- may arrive even before the second thread is forked, in that case
-- pullWorker will falsely detect that all threads are over.
{-# INLINE pushWorker #-}
pushWorker :: MonadAsync m => Context m a -> m ()
pushWorker ctx = do
    tid <- doFork (push ctx) (handleChildException (outputQueue ctx))
    liftIO $ modifyIORef (runningThreads ctx) $ (\s -> S.insert tid s)

-- | Split the original computation in a pull-push pair. The original
-- computation pulls from a Channel while m1 and m2 push to the channel.
{-# NOINLINE pullFork #-}
pullFork :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
pullFork m1 m2 = AsyncT $ \_ stp yld -> do
    ctx <- liftIO $ newContext
    queueWork ctx m1 >> queueWork ctx m2 >> pushWorker ctx
    (runAsyncT (pullWorker ctx)) Nothing stp yld

    where

    newContext = do
        outQ    <- atomically $ newTBQueue 32
        workQ   <- atomically $ newTBQueue 32
        running <- newIORef S.empty
        done    <- newIORef S.empty
        return $ Context { outputQueue   = outQ
                         , runningThreads = running
                         , doneThreads    = done
                         , workQueue      = workQ
                         }

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
--
-- TBD For quick response we may have to increase the rate in the middle of
-- a serially running computation. For that we can use a state flag to fork
-- the rest of the computation at any point of time inside the Monad bind
-- operation if the consumer is running at a faster speed.
--
-- TBD the alternative composition allows us to dispatch a chunkSize of only 1.
-- If we have to dispatch in arbitrary chunksizes we will need to compose the
-- parallel actions using a data constructor instead so that we can divide it
-- in chunks of arbitrary size before dispatch. When batching we can convert
-- the structure into Alternative batches of Monoid composition. That will also
-- allow us to dispatch more work to existing threads rather than creating new
-- threads always.
--
-- TBD for pure work (when we are not in the IO monad) we can divide it into
-- just the number of CPUs.
--
-- XXX to rate control left folded structrues we will have to return the
-- residual work back to the dispatcher. It will also consume a lot of
-- memory due to queueing of all the work before execution starts.

{-# INLINE forkWorker #-}
forkWorker :: MonadAsync m => Context m a -> m ()
forkWorker ctx = do
    let q = outputQueue ctx
    tid <- doFork (push ctx) (handleChildException q)
    liftIO $ atomically $ writeTBQueue q (ChildCreate tid)

{-# INLINE queueWork #-}
queueWork :: MonadAsync m => Context m a -> AsyncT m a -> m ()
queueWork ctx m = do
    -- To guarantee deadlock avoidance we need to dispatch a worker when the
    -- workQueue goes full. Otherwise all the worker threads might be waiting
    -- on the queue and never wakeup.
    --
    -- TBD If we run out of threads we can also evaluate the action completely
    -- right here, disallowing any further child workers and turning the
    -- parallel composition into interleaved serial composition.
    workQFull <- liftIO $ atomically $ isFullTBQueue (workQueue ctx)
    when (workQFull) $ forkWorker ctx
    liftIO $ atomically $ writeTBQueue (workQueue ctx) m

{-# INLINE dequeueLoop #-}
dequeueLoop :: MonadAsync m => Context m a -> AsyncT m a
dequeueLoop ctx = AsyncT $ \_ stp yld -> do
    work <- liftIO $ atomically $ tryReadTBQueue (workQueue ctx)
    case work of
        Nothing -> stp
        Just m -> do
            let stop = (runAsyncT (dequeueLoop ctx)) Nothing stp yld
                yield a c Nothing = yld a c (Just (dequeueLoop ctx))
                yield a c r = yld a c r
            (runAsyncT m) (Just ctx) stop yield

instance MonadAsync m => Alternative (AsyncT m) where
    empty = mempty

    -- Note: This is designed to scale for right associated compositions,
    -- therefore always use a right fold for folding bigger structures.
    {-# INLINE (<|>) #-}
    m1 <|> m2 = AsyncT $ \ctx stp yld -> do
        case ctx of
            Nothing -> (runAsyncT (pullFork m1 m2)) Nothing stp yld
            Just  c -> do
                -- we open up both the branches fairly but on a given level we
                -- go left to right. If the left one keeps producing results we
                -- may or may not run the right one.
                queueWork c m1 >> queueWork c m2
                (runAsyncT (dequeueLoop c)) Nothing stp yld

-- | Just like '<>' except that it can execute the action on the right in
-- parallel ahead of time. Returns the results in serial order like '<>' from
-- left to right.
parAhead :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
parAhead = undefined

(<>|) :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
(<>|) = parAhead

-- | Left biased parallel execution. Actions may run in parallel but not
-- necessarily, the action on the left is executed before the one on the right
-- when parallelism is not needed. This combinator is useful when fairness is
-- not required.
{-# INLINE parLeft #-}
parLeft :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
parLeft m1 m2 = AsyncT $ \ctx stp yld -> do
    case ctx of
        Nothing -> (runAsyncT (pullFork m1 m2)) Nothing stp yld
        Just  c -> do
            -- we open up both the branches fairly but on a given level we
            -- go left to right. If the left one keeps producing results we
            -- may or may not run the right one.
            queueWork c m1 >> queueWork c m2
            (runAsyncT (dequeueLoop c)) Nothing stp yld

-- | Same as 'parLeft'.
{-# INLINE (<|) #-}
(<|) :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
(<|) = parLeft

instance MonadAsync m => MonadPlus (AsyncT m) where
    mzero = empty
    mplus = (<|>)

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Monad m, Num a) => Num (AsyncT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (AsyncT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (AsyncT m a) where
    pi = pure pi

    exp  = fmap exp
    sqrt = fmap sqrt
    log  = fmap log
    sin  = fmap sin
    tan  = fmap tan
    cos  = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    tanh = fmap tanh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

-------------------------------------------------------------------------------
-- AsyncT transformer
-------------------------------------------------------------------------------

instance MonadTrans AsyncT where
    lift mx = AsyncT $ \ctx _ yld -> mx >>= (\a -> (yld a ctx Nothing))

instance (MonadBase b m, Monad m) => MonadBase b (AsyncT m) where
    liftBase = liftBaseDefault

------------------------------------------------------------------------------
-- Standard transformer instances
------------------------------------------------------------------------------

instance MonadIO m => MonadIO (AsyncT m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (AsyncT m) where
    throwM = lift . throwM

-- XXX handle and test cross thread state transfer
instance MonadError e m => MonadError e (AsyncT m) where
    throwError     = lift . throwError
    catchError m h = AsyncT $ \ctx stp yld ->
        let handle r = r `catchError` \e -> (runAsyncT (h e)) ctx stp yld
            yield a c Nothing = yld a c Nothing
            yield a c (Just r) = yld a c (Just (catchError r h))
        in handle $ (runAsyncT m) ctx stp yield

instance MonadReader r m => MonadReader r (AsyncT m) where
    ask = lift ask
    local f m = AsyncT $ \ctx stp yld ->
        let yield a c Nothing  = local f $ yld a c Nothing
            yield a c (Just r) = local f $ yld a c (Just (local f r))
        in (runAsyncT m) ctx (local f stp) yield

instance MonadState s m => MonadState s (AsyncT m) where
    get     = lift get
    put x   = lift (put x)
    state k = lift (state k)

------------------------------------------------------------------------------
-- MonadRecorder
------------------------------------------------------------------------------

instance MonadRecorder m => MonadRecorder (AsyncT m) where
    getJournal = lift getJournal
    putJournal = lift . putJournal
    play = lift . play

------------------------------------------------------------------------------
-- Running the monad
------------------------------------------------------------------------------

-- | Run an 'AsyncT m' computation, wait for it to finish and discard the
-- results.
runAsyncly :: MonadAsync m => AsyncT m a -> m ()
runAsyncly m = run Nothing m

    where

    stop = return ()

    {-# INLINE yield #-}
    yield _ _ Nothing  = stop
    yield _ c (Just x) = run c x

    run ct mx = (runAsyncT mx) ct stop yield

-- | Run an 'AsyncT m' computation and collect the results generated by each
-- thread of the computation in a list.
{-# INLINABLE toList #-}
toList :: MonadAsync m => AsyncT m a -> m [a]
toList m = run Nothing m

    where

    stop = return []

    {-# INLINE yield #-}
    yield a _ Nothing  = return [a]
    yield a c (Just x) = liftM (a :) (run c x)

    {-# INLINE run #-}
    run ctx mx = (runAsyncT mx) ctx stop yield

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Fold a 'Foldable' container using the given function.
{-# INLINABLE foldWith #-}
foldWith :: (Monoid b, Foldable t) => (a -> b -> b) -> t a -> b
foldWith f = foldr f mempty

-- | Fold a 'Foldable' container using a function that is a composition of the
-- two arguments.
{-# INLINABLE foldMapWith #-}
foldMapWith :: (Monoid b, Foldable t) =>
    (b1 -> b -> b) -> (a -> b1) -> t a -> b
foldMapWith f g = foldr (f . g) mempty

-- | Fold a 'Foldable' container using a function that is a compostioin of the
-- first and the third argument.
{-# INLINABLE forEachWith #-}
forEachWith :: (Monoid b, Foldable t) =>
    (b1 -> b -> b) -> t a -> (a -> b1) -> b
forEachWith f xs g = foldr (f . g) mempty xs

------------------------------------------------------------------------------
-- Convert a callback into an 'AsyncT' computation
------------------------------------------------------------------------------

fromCallback :: Monad m => (forall r. (a -> m r) -> m r) -> AsyncT m a
fromCallback k = AsyncT $ \ctx _ yld -> k (\a -> yld a ctx Nothing)
