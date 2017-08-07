{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RecordWildCards           #-}

-- |
-- Module      : Asyncly.Threads
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module Asyncly.Threads
    ( -- ChildEvent(..)
      MonadAsync
    , Context(..)
    , Step (..)
    , AsyncT (..)
    , initContext
    --, runAsyncTask
    --, makeCont
    , threadCtl
    -- , Location(..)
    -- , getLocation
    -- , setLocation

    -- , waitForChildren
    --, handleResult
    )
where

import           Control.Concurrent          (ThreadId, forkIO, killThread,
                                              myThreadId)
import           Control.Concurrent.STM      (TChan, atomically, newTChan,
                                              readTChan, tryReadTChan,
                                              writeTChan)
import           Control.Exception           (fromException,
                                              SomeException (..))
import qualified Control.Exception.Lifted    as EL
import           Control.Monad               (when)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.State         (MonadIO (..),
                                              StateT (..), runStateT,
                                              get, gets, modify)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import           Data.Atomics                (atomicModifyIORefCAS)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef, writeIORef)
import           Data.List                   (delete)
import           Data.Maybe                  (isJust)
import           GHC.Prim                    (Any)
import           Unsafe.Coerce               (unsafeCoerce)

import           Control.Monad.Trans.Recorder (Paused (..), Recording)

type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

data Step a m = Stop | Yield a (AsyncT m a)
newtype AsyncT m a = AsyncT { runAsyncT :: StateT Context m (Step a m) }

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
    continuations :: [Any]

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
    -> (a -> AsyncT m b)
    -> Maybe (IORef [Recording])
    -> Context
initContext childChan pending credit finalizer lref =
  Context { continuations   = [unsafeCoerce finalizer]
          , logsRef         = lref
          , location        = Worker
          , childChannel    = childChan
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

{-
------------------------------------------------------------------------------
-- Pick up from where we left in the previous thread
------------------------------------------------------------------------------

{-# SPECIALIZE continue :: [Any] -> a -> StateT Context IO (Maybe a) #-}
continue :: Monad m => [Any] -> a -> StateT Context m (Maybe a)
continue [] x = do
    return (Just x)

continue [f] x = do
    modify $ \ctx -> ctx {continuations = []}
    (unsafeCoerce f) x

continue (f:fs) x = do
    modify $ \ctx -> ctx {continuations = fs}
    y <- (unsafeCoerce f) x
    case y of
        Nothing -> return y
        Just z -> continue fs z

{-# SPECIALIZE runContext :: Context -> StateT Context IO (Maybe a) -> StateT Context IO () #-}
runContext :: MonadAsync m => Context -> StateT Context m (Maybe a) -> StateT Context m ()
runContext ctx action = do
    _ <- lift $ runStateT m ctx
    return ()

    where

    m = do
        let conts = continuations ctx
        x <- action
        case x of
            Just y -> continue conts y >> return ()
            Nothing -> return ()

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

{-# NOINLINE waitForOneEvent #-}
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
{-# INLINE waitQSemB #-}
waitQSemB :: IORef Int -> IO Bool
waitQSemB   sem = atomicModifyIORefCAS sem $ \n ->
                    if n > 0
                    then (n - 1, True)
                    else (n, False)

signalQSemB :: IORef Int -> IO ()
signalQSemB sem = atomicModifyIORefCAS sem $ \n -> (n + 1, ())

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

{-# INLINE handleResult #-}
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
        liftIO $ atomicModifyIORefCAS ref $ \logs -> (recording : logs, ())
        waitForChildren ctx

    passException e = do
        liftIO $ readIORef (pendingThreads ctx) >>= mapM_ killThread
        return (Just e)

forkFinally1 :: MonadAsync m
    => Context
    -> StateT Context m (Maybe a)
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
    => StateT Context m (Maybe a) -> StateT Context m ()
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
    gotCredit <- waitQSemB (threadCredit context)
    case gotCredit of
        False -> do
            pending <- readIORef $ pendingThreads context
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
{-# SPECIALIZE runAsyncTask :: Bool -> StateT Context IO (Maybe a)
   -> StateT Context IO (Maybe a) #-}
{-# INLINE runAsyncTask #-}
runAsyncTask :: MonadAsync m
    => Bool
    -> StateT Context m (Maybe a)
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
    return Nothing

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
makeCont :: MonadAsync m => ((a -> m ()) -> m ()) -> StateT Context m (Maybe a)
makeCont cbsetter = do
    -- XXX should we force fork a thread or keep running in the context of the
    -- callback?
    ctx <- get
    lift $ cbsetter $ \a -> do
            let s = runAsyncTask False (return (Just a))
            _ <- runStateT s ctx
            return ()
    spawningParentDone

-}
------------------------------------------------------------------------------
-- Controlling thread quota
------------------------------------------------------------------------------

-- XXX Should n be Word32 instead?
-- | Runs a computation under a given thread limit.  A limit of 0 means all new
-- tasks start synchronously in the current thread unless overridden by
-- 'async'.
threadCtl :: MonadAsync m
    => Int
    -> StateT Context m a
    -> StateT Context m a
threadCtl n action = do
   oldCr <- gets threadCredit
   newCr <- liftIO $ newIORef n
   modify $ \s -> s { threadCredit = newCr }
   r <- action
   modify $ \s -> s { threadCredit = oldCr }
   return r
