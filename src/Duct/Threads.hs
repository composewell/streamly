{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Duct.Threads
    ( parallel
    , waitEvents
    , async
    , sample
    , sync
    , react
    , threads
    )
where

import           Control.Applicative         ((<|>))
import           Control.Concurrent          (ThreadId, forkIO, killThread,
                                              myThreadId, threadDelay)
import           Control.Concurrent.STM      (TChan, atomically, newTChan,
                                              readTChan, tryReadTChan,
                                              writeTChan)
import           Control.Exception           (ErrorCall (..),
                                              SomeException (..), catch)
import qualified Control.Exception.Lifted    as EL
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.State         (StateT, get, gets, modify, put,
                                              runStateT, when)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import           Data.Dynamic                (Typeable)
import           Data.IORef                  (IORef, atomicModifyIORef,
                                              modifyIORef, newIORef, readIORef,
                                              writeIORef)
import           Data.List                   (delete)
import           Data.Maybe                  (fromJust)
import           Unsafe.Coerce               (unsafeCoerce)

import           Duct.AsyncT
import           Duct.Context

------------------------------------------------------------------------------
-- Model of computation
------------------------------------------------------------------------------

-- A computation starts in a top level thread. A thread can either finish the
-- computation or it can wait for events to occur before it proceeds further. A
-- thread can spawn more threads to process an event. It is a way to compose
-- asynchronous events in a concurrent manner. Since it is a transformer we can
-- use things like pipe, conduit or any other transformer monads inside the
-- computations to utilize single threaded composition or data flow techniques.
--
-- Usually none of the computations in this monad need to return a result as
-- the computation can be handed over to the next one until the final effect is
-- produced. However, if needed to be embedded in a different programming model
-- the monad can return values in a stream like a ListT.

------------------------------------------------------------------------------
-- Pick up from where we left in the previous thread
------------------------------------------------------------------------------

-- | Continue execution of the closure that we were executing when we migrated
-- to a new thread.

resume :: Monad m => Context -> StateT Context m (Maybe a)
resume = runAsyncT . resumeContext

------------------------------------------------------------------------------
-- Thread Management (creation, reaping and killing)
------------------------------------------------------------------------------

tryReclaimZombies :: MonadIO m => TChan ThreadId -> IORef [ThreadId] -> m ()
tryReclaimZombies chan pendingRef = do
    pending <- liftIO $ readIORef pendingRef
    case pending of
        [] -> return ()
        _ ->  do
            mtid <- liftIO $ atomically $ tryReadTChan chan
            case mtid of
                Nothing -> return ()
                Just tid -> do
                    liftIO $ writeIORef pendingRef $ delete tid pending
                    tryReclaimZombies chan pendingRef

waitForOneChild :: MonadIO m => TChan ThreadId -> IORef [ThreadId] -> m ()
waitForOneChild chan pendingRef = do
    -- XXX assert pending must have at least one element
    -- assert that the tid is found in our list
    tid <- liftIO $ atomically $ readTChan chan
    liftIO $ modifyIORef pendingRef $ delete tid

-- | kill all the child threads associated with the continuation context
killChildren :: Context -> IO ()
killChildren ctx  = do
    ths <- readIORef (pendingThreads ctx)
    mapM_ killThread ths

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

instance Read SomeException where
  readsPrec _n str = [(SomeException $ ErrorCall s, r)]
    where [(s , r)] = read str

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

forkFinally1 :: (MonadIO m, MonadBaseControl IO m) =>
    m a -> (Either SomeException a -> IO ()) -> m ThreadId
forkFinally1 action and_then =
    EL.mask $ \restore ->
        liftBaseWith $ \runInIO -> forkIO $ do
            _ <- runInIO $ EL.try (restore action) >>= liftIO . and_then
            return ()

forkIt :: (MonadBaseControl IO m, MonadIO m) => Context -> (Context -> m t) -> m ()
forkIt context runCtx = do
    child <- childContext context
    tid <- forkFinally1 (runCtx child) (beforeExit child)
    updatePendingThreads context tid

    where

    updatePendingThreads :: MonadIO m => Context -> ThreadId -> m ()
    updatePendingThreads cur tid = do
        -- update the new thread before reclaiming zombies so that if it exited
        -- already reclaim finds it in the list and does not panic.
        liftIO $ modifyIORef (pendingThreads cur) $ (\ts -> tid:ts)
        tryReclaimZombies (childChannel cur) (pendingThreads cur)

    childContext ctx = do
        pendingRef <- liftIO $ newIORef []
        chan <- liftIO $ atomically newTChan
        -- shares the threadCredit of the parent by default
        return $ ctx
            { parentChannel  = Just (childChannel ctx)
            , pendingThreads = pendingRef
            , childChannel = chan
            }

    beforeExit cur res = do
        case res of
            Left _exc -> liftIO $ killChildren cur
                        -- propagate the exception to the parent
            Right _ -> -- collect the results?
                        return ()

        waitForChildren (childChannel cur) (pendingThreads cur)
        signalQSemB (threadCredit cur)
        -- We are guaranteed to have a parent because we have been explicitly
        -- forked by some parent.
        let p = fromJust (parentChannel cur)
        tid <- myThreadId
        liftIO $ atomically $ writeTChan p tid

forkMaybe :: (MonadBaseControl IO m, MonadIO m) => Context -> (Context -> m ()) -> m ()
forkMaybe context runCtx = do
    gotCredit <- liftIO $ waitQSemB (threadCredit context)
    pending <- liftIO $ readIORef $ pendingThreads context
    case gotCredit of
        False -> case pending of
                [] -> runCtx context -- run synchronously
                _ -> do
                        -- XXX If we have unreclaimable child threads e.g.
                        -- infinite loop, this is going to deadlock us. We need
                        -- special handling for those cases. Add those to
                        -- unreclaimable list? And always execute them in an
                        -- async thread, cannot use sync for those.
                        waitForOneChild (childChannel context)
                                        (pendingThreads context)
                        forkMaybe context runCtx
        True -> forkIt context runCtx

-- | 'StreamData' represents a task in a task stream being generated.
data StreamData a =
      SMore a               -- ^ More tasks to come
    | SLast a               -- ^ This is the last task
    | SDone                 -- ^ No more tasks, we are done
    | SError SomeException  -- ^ An error occurred
    deriving (Typeable, Show,Read)

-- The current model is to start a new thread for every task. The input is
-- provided at the time of the creation and therefore no synchronization is
-- needed compared to a pool of threads contending to get the input from a
-- channel. However the thread creation overhead may be more than that?
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
-- | Execute the IO action and the continuation
genAsyncEvents ::  (MonadIO m, MonadBaseControl IO m) => Context -> IO (StreamData t) -> m ()
genAsyncEvents context rec = forkMaybe context loop

    where

    -- Execute the IO computation and then the closure-continuation
    loop ctx = do
        streamData <- liftIO $ rec `catch`
                \(e :: SomeException) -> return $ SError e

        let run = runContWith streamData
        case streamData of
            SMore _ -> do
                forkMaybe ctx run
                loop ctx
            _ -> run ctx

    -- resume the context with the result of the IO computation
    runContWith dat ctx = do
        let newCtx = ctx { event = Just $ unsafeCoerce dat }
        _ <- runStateT (resume newCtx) newCtx
        return ()

-- | Run an IO action one or more times to generate a stream of tasks. The IO
-- action returns a 'StreamData'. When it returns an 'SMore' or 'SLast' a new
-- task is triggered with the result value. If the return value is 'SMore', the
-- action is run again to generate the next task, otherwise task creation
-- stops.
--
-- Unless the maximum number of threads (set with 'threads') has been reached,
-- the task is generated in a new thread and the current thread returns a void
-- task.
parallel  :: (Monad m, MonadIO m, MonadBaseControl IO m)
    => IO (StreamData a) -> AsyncT m (StreamData a)
parallel ioaction = AsyncT $ do
  -- We retrieve the context here and pass it on so that we can resume it
  -- later. Control resumes at this place when we resume this context after
  -- generating the event data from the ioaction.
  context <- get
  case event context of
    -- we have already executed the ioaction and now we are continuing in a new
    -- thread. Just return the result of the ioaction stored in 'event' field.
    j@(Just _) -> do
      put context { event = Nothing }
      return $ unsafeCoerce j
    -- we have to execute the io action and generate an event and then continue
    -- in this thread or a new thread.
    Nothing    -> do
      lift $ genAsyncEvents context ioaction
      loc <- getLocation
      when (loc /= RemoteNode) $ setLocation WaitingParent
      return Nothing

-- | An task stream generator that produces an infinite stream of tasks by
-- running an IO computation in a loop. A task is triggered carrying the output
-- of the computation. See 'parallel' for notes on the return value.
waitEvents :: (MonadIO m, MonadBaseControl IO m) => IO a -> AsyncT m a
waitEvents io = do
  mr <- parallel (SMore <$> io)
  case mr of
    SMore  x -> return x
 --   SError e -> back e

-- | Run an IO computation asynchronously and generate a single task carrying
-- the result of the computation when it completes. See 'parallel' for notes on
-- the return value.
async  :: (MonadIO m, MonadBaseControl IO m) => IO a -> AsyncT m a
async io = do
  mr <- parallel (SLast <$> io)
  case mr of
    SLast  x -> return x
  --  SError e -> back   e

-- | Force an async computation to run synchronously. It can be useful in an
-- 'Alternative' composition to run the alternative only after finishing a
-- computation.  Note that in Applicatives it might result in an undesired
-- serialization.
sync :: MonadIO m => AsyncT m a -> AsyncT m a
sync x = AsyncT $ do
  setLocation RemoteNode
  r <- runAsyncT x
  setLocation Worker
  return r

-- | An task stream generator that produces an infinite stream of tasks by
-- running an IO computation periodically at the specified time interval. The
-- task carries the result of the computation.  A new task is generated only if
-- the output of the computation is different from the previous one.  See
-- 'parallel' for notes on the return value.
sample :: (Eq a, MonadIO m, MonadBaseControl IO m) => IO a -> Int -> AsyncT m a
sample action interval = do
  v    <- liftIO action
  prev <- liftIO $ newIORef v
  waitEvents (loop action prev) <|> async (return v)
  where loop act prev = loop'
          where loop' = do
                  threadDelay interval
                  v  <- act
                  v' <- readIORef prev
                  if v /= v' then writeIORef prev v >> return v else loop'

-- | Make a transient task generator from an asynchronous callback handler.
--
-- The first parameter is a callback. The second parameter is a value to be
-- returned to the callback; if the callback expects no return value it
-- can just be a @return ()@. The callback expects a setter function taking the
-- @eventdata@ as an argument and returning a value to the callback; this
-- function is supplied by 'react'.
--
-- Callbacks from foreign code can be wrapped into such a handler and hooked
-- into the transient monad using 'react'. Every time the callback is called it
-- generates a new task for the transient monad.
--
react
  :: (Monad m, MonadIO m)
  => ((eventdata ->  m response) -> m ())
  -> IO  response
  -> AsyncT m eventdata
react setHandler iob = AsyncT $ do
        cont    <- get
        case event cont of
          Nothing -> do
            lift $ setHandler $ \dat ->do
              runStateT (resume cont) cont{event= Just $ unsafeCoerce dat}
              liftIO iob
            loc <- getLocation
            when (loc /= RemoteNode) $ setLocation WaitingParent
            return Nothing

          j@(Just _) -> do
            put cont{event=Nothing}
            return $ unsafeCoerce j

------------------------------------------------------------------------------
-- Controlling thread quota
------------------------------------------------------------------------------

-- XXX Should n be Word32 instead?
-- | Runs a computation under a given thread limit.  A limit of 0 means new
-- tasks start synchronously in the current thread.  New threads are created by
-- 'parallel', and APIs that use parallel.
threads :: MonadIO m => Int -> AsyncT m a -> AsyncT m a
threads n process = do
   oldCr <- gets threadCredit
   newCr <- liftIO $ newIORef n
   modify $ \s -> s { threadCredit = newCr }
   r <- process
        <** (modify $ \s -> s { threadCredit = oldCr }) -- restore old credit
   return r

{-
-- | Run a "non transient" computation within the underlying state monad, so it
-- is guaranteed that the computation neither can stop nor can trigger
-- additional events/threads.
noTrans :: Monad m => StateM m x -> AsyncT m x
noTrans x = AsyncT $ x >>= return . Just

-- This can be used to set, increase or decrease the existing limit. The limit
-- is shared by multiple threads and therefore needs to modified atomically.
-- Note that when there is no limit the limit is set to maxBound it can
-- overflow with an increment and get reduced instead of increasing.
-- XXX should we use a Maybe instead? Or use separate inc/dec/set APIs to
-- handle overflow properly?
--
-- modifyThreads :: MonadIO m => (Int -> Int) -> AsyncT m ()
-- modifyThreads f =
-}
