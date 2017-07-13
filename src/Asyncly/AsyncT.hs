{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Asyncly.AsyncT
    ( AsyncT (..)
    , MonadAsync
    , ChildEvent(..)
    , Context(..)
    , initContext
    , Location(..)
    , getLocation
    , setLocation
    , threads
    , waitForChildren
    , handleResult

    , async
    , makeAsync
    , each

    , discardAndThen
    , (*>>)
    , thenDiscard
    , (>>*)
    , afterFirst
    , (>>|)

    -- internal
    , dbg
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Concurrent          (ThreadId, forkIO, killThread,
                                              myThreadId)
import           Control.Concurrent.STM      (TChan, atomically, newTChan,
                                              readTChan, tryReadTChan,
                                              writeTChan)
import           Control.Exception           (fromException,
                                              SomeException (..))
import qualified Control.Exception.Lifted    as EL
import           Control.Monad               (ap, mzero, when)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.State         (MonadIO (..), MonadPlus (..),
                                              StateT (..), liftM, runStateT,
                                              get, gets, modify)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM, liftBaseWith)
import           Data.IORef                  (IORef, atomicModifyIORef,
                                              modifyIORef, newIORef, readIORef,
                                              writeIORef)
import           Data.List                   (delete)
import           Data.Maybe                  (isJust)
import           GHC.Prim                    (Any)
import           Unsafe.Coerce               (unsafeCoerce)

import           Control.Monad.Trans.Recorder (MonadRecorder(..), Paused(..),
                                               Recording)
--import           Debug.Trace (traceM)

newtype AsyncT m a = AsyncT { runAsyncT :: StateT Context m (Maybe a) }

------------------------------------------------------------------------------
-- Parent child thread communication types
------------------------------------------------------------------------------

data ChildEvent = ChildDone ThreadId (Maybe SomeException)

------------------------------------------------------------------------------
-- State threaded around the monad
------------------------------------------------------------------------------

data Context = Context
  {
    ---------------------------------------------------------------------------
    -- Execution state
    ---------------------------------------------------------------------------

    -- a -> AsyncT m b
    continuation   :: Any

  -- When we suspend we save the logs in this IORef and exit.
  , logsRef :: Maybe (IORef [Recording])
    -- XXX this functionality can be in a separate layer?
  , location :: Location

    ---------------------------------------------------------------------------
    -- Thread creation, communication and cleanup
    ---------------------------------------------------------------------------
    -- When a new thread is created the parent records it in the
    -- 'pendingThreads' field and 'threadCredit' is decremented.  When a child
    -- thread is done it sends a done event to the parent on an unbounded
    -- channel and goes away.  Before starting a new thread the parent always
    -- processes the unbounded channel to clear up the pending zombies. This
    -- strategy ensures that the zombie queue will never grow more than the
    -- number of running threads.  Before exiting, the parent thread waits on
    -- the channel until all its children are cleaned up.
    ---------------------------------------------------------------------------

    -- XXX setup a cleanup computation to run rather than passing all these
    -- params.

  , childChannel    :: TChan ChildEvent
    -- ^ A channel for the immediate children to communicate to us when they
    -- die.  Each thread has its own dedicated channel for its children

    -- We always track the child threads, otherwise the programmer needs to
    -- worry about if some threads may remain hanging because they are stuck in
    -- an infinite loop or a forever blocked IO. Also, it is not clear whether
    -- there is a significant gain by disabling tracking. Instead of using
    -- threadIds we can also keep a count instead, that will allow us to wait
    -- for all children to drain but we cannot kill them.
    --
    -- We need these as IORefs since we need any changes to these to be
    -- reflected back in the calling code that waits for the children.

  , pendingThreads :: IORef [ThreadId]
    -- ^ Active immediate child threads spawned by this thread, modified only
    -- by this thread, therefore atomic modification is not needed.

    -- By default this limit is shared by the current tree of threads. But at
    -- any point the 'threads' primitive can be used to start a new limit for
    -- the computation under that primitive.
    --
    -- Shared across many threads, therefore atomic access is required.
    -- This is incremented by the child thread itelf before it exits.
    --
  , threadCredit   :: IORef Int
    -- ^ How many more threads are allowed to be created?
  }

initContext
    :: TChan ChildEvent
    -> IORef [ThreadId]
    -> IORef Int
    -> (a -> AsyncT m a)
    -> Maybe (IORef [Recording])
    -> Context
initContext childChan pending credit finalizer lref =
  Context { continuation    = unsafeCoerce finalizer
          , logsRef         = lref
          , location        = Worker
          , childChannel    = unsafeCoerce childChan
          , pendingThreads  = pending
          , threadCredit    = credit }

------------------------------------------------------------------------------
-- Where is the computation running?
------------------------------------------------------------------------------

-- RemoteNode is used in the Alternative instance. We stop the computation when
-- the location is RemoteNode, we do not execute any alternatives.
--
data Location = Worker | WaitingParent | RemoteNode
  deriving (Eq, Show)

getLocation :: Monad m => StateT Context m Location
getLocation = gets location

setLocation :: Monad m => Location -> StateT Context m ()
setLocation loc = modify $ \Context { .. } -> Context { location = loc, .. }

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Toggle the dbg definition for debug traces
--dbg :: Monad m => String -> m ()
--dbg = traceM
dbg :: Monad m => a -> m ()
dbg _ = return ()

type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

------------------------------------------------------------------------------
-- Pick up from where we left in the previous thread
------------------------------------------------------------------------------

-- | Run the continuation of an action
runContext :: MonadAsync m => Context -> AsyncT m a -> StateT Context m ()
runContext ctx action = do
    let s = runAsyncT $ action >>= unsafeCoerce (continuation ctx)
    _ <- lift $ runStateT s ctx
    return ()

------------------------------------------------------------------------------
-- Thread Management (creation, reaping and killing)
------------------------------------------------------------------------------

-- XXX We are using unbounded channels so this will not block on writing to
-- pchan. We can use bounded channels to throttle the creation of threads based
-- on consumption rate.
processOneEvent :: MonadIO m
    => ChildEvent
    -> [ThreadId]
    -> m ([ThreadId], Maybe SomeException)
processOneEvent (ChildDone tid e) pending = do
    when (isJust e) $ liftIO $ mapM_ killThread pending
    return (delete tid pending, Nothing)

drainChildren :: MonadIO m
    => TChan ChildEvent
    -> [ThreadId]
    -> m ([ThreadId], Maybe SomeException)
drainChildren cchan pending =
    case pending of
        [] -> return (pending, Nothing)
        _  ->  do
            ev <- liftIO $ atomically $ readTChan cchan
            (p, e) <- processOneEvent ev pending
            maybe (drainChildren cchan p) (const $ return (p, e)) e

waitForChildren :: MonadIO m => Context -> m (Maybe SomeException)
waitForChildren ctx = do
    let pendingRef = pendingThreads ctx
    pending <- liftIO $ readIORef pendingRef
    (p, e) <- drainChildren (childChannel ctx) pending
    liftIO $ writeIORef pendingRef p
    return e

tryReclaimZombies :: (MonadIO m, MonadThrow m) => Context -> m ()
tryReclaimZombies ctx = do
    let cchan = childChannel ctx
        pendingRef = pendingThreads ctx

    pending <- liftIO $ readIORef pendingRef
    case pending of
        [] -> return ()
        _ ->  do
            mev <- liftIO $ atomically $ tryReadTChan cchan
            case mev of
                Nothing -> return ()
                Just ev -> do
                    (p, e) <- processOneEvent ev pending
                    liftIO $ writeIORef pendingRef p
                    maybe (return ()) throwM e
                    tryReclaimZombies ctx

waitForOneEvent :: (MonadIO m, MonadThrow m) => Context -> m ()
waitForOneEvent ctx = do
    -- XXX assert pending must have at least one element
    -- assert that the tid is found in our list
    let cchan = childChannel ctx
        pendingRef = pendingThreads ctx

    ev <- liftIO $ atomically $ readTChan cchan
    pending <- liftIO $ readIORef pendingRef
    (p, e) <- processOneEvent ev pending
    liftIO $ writeIORef pendingRef p
    maybe (return ()) throwM e

-- XXX this is not a real semaphore as it does not really block on wait,
-- instead it returns whether the value is zero or non-zero.
--
waitQSemB :: IORef Int -> IO Bool
waitQSemB   sem = atomicModifyIORef sem $ \n ->
                    if n > 0
                    then (n - 1, True)
                    else (n, False)

signalQSemB :: IORef Int -> IO ()
signalQSemB sem = atomicModifyIORef sem $ \n -> (n + 1, ())

-- Allocation of threads
--
-- global thread limit
-- thread fan-out i.e. per thread children limit
-- min per thread allocation to avoid starvation
--
-- dynamic adjustment based on the cost, speed of consumption, cpu utilization
-- etc. We need to adjust the limits based on cost, throughput and latencies.
--
-- The event producer thread must put the work on a work-queue and the child
-- threads can pick it up from there. But if there is just one consumer then it
-- may not make sense to have a separate producer unless the producing cost is
-- high.
--

handleResult :: MonadIO m
    => Context -> Either SomeException a -> m (Maybe SomeException)
handleResult ctx res =
    case res of
        Left e -> do
            case fromException e of
                Just (Paused recording) ->
                    maybe (passException e)
                          (handleRecording recording) (logsRef ctx)
                Nothing -> passException e
        Right _ -> waitForChildren ctx

    where

    handleRecording recording ref = do
        liftIO $ atomicModifyIORef ref $ \logs -> (recording : logs, ())
        waitForChildren ctx

    passException e = do
        liftIO $ readIORef (pendingThreads ctx) >>= mapM_ killThread
        return (Just e)

forkFinally1 :: MonadAsync m
    => Context
    -> AsyncT m a
    -> (Either SomeException () -> IO ())
    -> StateT Context m ThreadId
forkFinally1 ctx action preExit =
    EL.mask $ \restore ->
        liftBaseWith $ \runInIO -> forkIO $ do
            _ <- runInIO $ EL.try (restore (runContext ctx action))
                           >>= liftIO . preExit
            -- XXX restore state here
            return ()

-- | Run a given context in a new thread.
--
forkContext :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
    => AsyncT m a -> StateT Context m ()
forkContext action = do
    parentCtx <- get
    childCtx <- childContext parentCtx
    tid <- forkFinally1 childCtx action
                (beforeExit childCtx (childChannel parentCtx))
    updatePendingThreads parentCtx tid

    where

    updatePendingThreads :: (MonadIO m, MonadThrow m)
        => Context -> ThreadId -> m ()
    updatePendingThreads ctx tid = do
        -- update the new thread before reclaiming zombies so that if it exited
        -- already reclaim finds it in the list and does not panic.
        liftIO $ modifyIORef (pendingThreads ctx) $ (\ts -> tid:ts)
        tryReclaimZombies ctx

    childContext ctx = do
        pendingRef <- liftIO $ newIORef []
        chan <- liftIO $ atomically newTChan
        -- shares the threadCredit of the parent by default
        return $ ctx
            { pendingThreads = pendingRef
            , childChannel = chan
            }

    beforeExit ctx pchan res = do
        r <- handleResult ctx res
        signalQSemB (threadCredit ctx)
        tid <- myThreadId
        liftIO $ atomically $ writeTChan pchan (ChildDone tid r)

-- | Decide whether to resume the context in the same thread or a new thread
--
canFork :: Context -> IO Bool
canFork context = do
    gotCredit <- liftIO $ waitQSemB (threadCredit context)
    case gotCredit of
        False -> do
            pending <- liftIO $ readIORef $ pendingThreads context
            case pending of
                [] -> return False
                _ -> do
                        -- XXX If we have unreclaimable child threads e.g.
                        -- infinite loop, this is going to deadlock us. We need
                        -- special handling for those cases. Add those to
                        -- unreclaimable list? And always execute them in an
                        -- async thread, cannot use sync for those.
                        --
                        waitForOneEvent context
                        canFork context
        True -> return True

-- Housekeeping, invoked after spawning of all child tasks is done and the
-- parent task needs to terminate. Either the task is fully done or we handed
-- it over to another thread, in any case the current thread is done.

spawningParentDone :: MonadIO m => StateT Context m (Maybe a)
spawningParentDone = do
    loc <- getLocation
    when (loc /= RemoteNode) $ setLocation WaitingParent
    return Nothing

-- | The task may be run in the same thread or in a new thread depending on the
-- forceAsync parameter and the current thread quota.
--
runAsyncTask :: MonadAsync m
    => Bool
    -> AsyncT m a
    -> StateT Context m (Maybe a)
runAsyncTask forceAsync action = do
    if forceAsync
    then forkContext action
    else do
        context <- get
        can <- liftIO $ canFork context
        case can of
            False -> runContext context action
            True -> forkContext action
    spawningParentDone

-- The current model is to start a new thread for every task. The input is
-- provided at the time of the creation and therefore no synchronization is
-- needed compared to a pool of threads contending to get the input from a
-- channel. However the thread creation overhead may be more than the
-- synchronization cost?
--
-- When the task is over the outputs need to be collected and that requires
-- synchronization irrespective of a thread pool model or per task new thread
-- model.
--
-- XXX instead of starting a new thread every time, reuse the existing child
-- threads and send them work via a shared channel. When there is no more work
-- available we need a way to close the channel and wakeup all waiters so that
-- they can go away rather than waiting indefinitely.
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
-- The <|> operator implies "can be parallel", whereas async implies "must be
-- parallel". Note that async is not useful and should never be used outside an
-- 'Alternative' composition. Even in an 'Alternative' composition 'async' is
-- not useful in the last action.
async :: MonadAsync m => AsyncT m a -> AsyncT m a
async action = AsyncT $ runAsyncTask True action

-- XXX After setting the callback we should wait for the callback otherwise we
-- will just go away. We should use the same finalization mechanism that we use
-- in fork.
--
-- | Makes an asyncly action from a callback setter function; can be used to
-- convert existing callback style code into asyncly style code.  The first
-- parameter is a callback setter function.  When we are called back,
-- 'makeAsync' behaves as if it was an async computation that just returned a
-- value of type 'a'.  After the computation is done the result of the action
-- in second parameter is returned to the callback.
--
makeAsync :: MonadAsync m => ((a -> m ()) -> m ()) -> AsyncT m a
makeAsync cbsetter = AsyncT $ do
    -- XXX should we force fork a thread or keep running in the context of the
    -- callback?
    ctx <- get
    lift $ cbsetter $ \a -> do
            let s = runAsyncTask False (return a)
            _ <- runStateT s ctx
            return ()
    spawningParentDone

-- scatter
each :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
    => [a] -> AsyncT m a
each xs = foldl (<|>) empty $ map return xs

------------------------------------------------------------------------------
-- Controlling thread quota
------------------------------------------------------------------------------

-- XXX Should n be Word32 instead?
-- | Runs a computation under a given thread limit.  A limit of 0 means new
-- tasks start synchronously in the current thread.  New threads are created by
-- 'parallel', and APIs that use parallel.
threads :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
threads n process = AsyncT $ do
   oldCr <- gets threadCredit
   newCr <- liftIO $ newIORef n
   modify $ \s -> s { threadCredit = newCr }
   r <- runAsyncT $ process
        >>* (AsyncT $ do
            modify $ \s -> s { threadCredit = oldCr }
            return (Just ())
            ) -- restore old credit
   return r

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance MonadAsync m => Functor (AsyncT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------


instance MonadAsync m => Applicative (AsyncT m) where
    pure a  = AsyncT . return $ Just a
    (<*>) = ap

------------------------------------------------------------------------------
-- Alternative
------------------------------------------------------------------------------

instance MonadAsync m => Alternative (AsyncT m) where
    empty = AsyncT $ return Nothing
    (<|>) m1 m2 = AsyncT $ do
        r <- runAsyncTask False m1
        loc <- getLocation
        case loc of
            RemoteNode -> return Nothing
            _          -> maybe (runAsyncT m2) (return . Just) r

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (AsyncT m) where
    return = pure
    m >>= f = AsyncT $ do
        modify $ \Context { continuation = g, .. } ->
            let k x = f x >>= unsafeCoerce g
            in Context { continuation = unsafeCoerce k, .. }

        x <- runAsyncT m
        Context { continuation = g } <- get
        runAsyncT $ maybe mzero (unsafeCoerce g) x

instance MonadAsync m => MonadPlus (AsyncT m) where
    mzero = empty
    mplus = (<|>)

instance (Monoid a, MonadAsync m) => Monoid (AsyncT m a) where
    mappend x y = mappend <$> x <*> y
    mempty      = return mempty

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

------------------------------------------------------------------------------
-- Special compositions
------------------------------------------------------------------------------

infixr 1 >>*, *>>, >>|

-- XXX This can be moved to utility functions as it is purely app level
-- | Run @b@ once, discarding its result when the first task in task set @a@
-- has finished. Useful to start a singleton task after the first task has been
-- setup.
afterFirst :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
afterFirst ma mb = do
    ref <- liftIO $ newIORef False
    x <- ma
    done <- liftIO $ readIORef ref
    when (not done) $ (liftIO $ writeIORef ref True) >>* mb
    return x

(>>|) :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
(>>|) = afterFirst

-- | Run 'm a' in "isolation" and discard its result, and then run 'm b' and
-- return its result.  Isolation means that any alternative actions inside 'm
-- a' are not continued to 'm b'.
discardAndThen :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m b
discardAndThen ma mb = AsyncT $ do
    _ <- runAsyncT (ma >> mzero)
    runAsyncT mb

-- | Same as 'discardAndThen'.
(*>>) :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m b
(*>>) = discardAndThen

-- | Run 'm a' and then run 'm b' in "isolation" and return the result of 'm
-- a'. Isolation means that any alternative actions inside 'm a' are not
-- continued to 'm b' and the results of 'm b' are discarded.
thenDiscard :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
thenDiscard ma mb = AsyncT $ do
    a <- runAsyncT ma
    _ <- runAsyncT (mb >> mzero)
    return a

-- | Same as 'thenDiscard'.
(>>*) :: MonadAsync m => AsyncT m a -> AsyncT m b -> AsyncT m a
(>>*) = thenDiscard

-------------------------------------------------------------------------------
-- AsyncT transformer
-------------------------------------------------------------------------------

instance MonadTrans AsyncT where
    lift mx = AsyncT $ lift mx >>= return . Just

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

instance (MonadBase b m, MonadAsync m) => MonadBase b (AsyncT m) where
    liftBase = liftBaseDefault

instance (MonadBaseControl b m, MonadAsync m) => MonadBaseControl b (AsyncT m) where
    type StM (AsyncT m) a = ComposeSt AsyncT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

------------------------------------------------------------------------------
-- Standard transformer instances
------------------------------------------------------------------------------

instance MonadAsync m => MonadIO (AsyncT m) where
    liftIO = lift . liftIO

instance MonadAsync m => MonadThrow (AsyncT m) where
    throwM = lift . throwM

------------------------------------------------------------------------------
-- MonadRecorder
------------------------------------------------------------------------------

instance (MonadAsync m, MonadRecorder m) => MonadRecorder (AsyncT m) where
    getJournal = lift getJournal
    putJournal = lift . putJournal
    play = lift . play
