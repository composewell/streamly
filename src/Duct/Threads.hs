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

import Control.Applicative ((<|>))
import Control.Concurrent
       (ThreadId, forkIO, myThreadId, threadDelay, killThread)
import Control.Concurrent.STM
       (TChan, atomically, newTChan, readTChan, tryReadTChan, writeTChan)
import Control.Exception (SomeException(..), ErrorCall(..), catch)
import qualified Control.Exception.Lifted as E
import Data.Dynamic (Typeable)
import Data.IORef
       (IORef, atomicModifyIORef, readIORef, writeIORef, modifyIORef,
        newIORef)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
       (StateT, get, gets, modify, put, runStateT, when)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import Unsafe.Coerce (unsafeCoerce)

import Duct.AsyncT
import Duct.Event

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Run the closure and the continuation using the state data of the calling thread
runCont :: (MonadIO m, Monad (AsyncT m)) => EventF -> StateT EventF m (Maybe a)
runCont EventF { xcomp = x, fcomp = fs } = runAsyncT $ do
  r <- unsafeCoerce x
  compose (unsafeCoerce fs) r

------------------------------------------------------------------------------
-- Thread Management (creation, reaping and killing)
------------------------------------------------------------------------------

tryReclaimZombies :: MonadIO m => TChan ThreadId -> IORef [EventF] -> m ()
tryReclaimZombies chan pendingRef = do
    pending <- liftIO $ readIORef pendingRef
    if length pending == 0
        then return ()
        else do
            mtid <- liftIO $ atomically $ tryReadTChan chan
            case mtid of
                Nothing -> return ()
                Just tid -> do
                    liftIO $ writeIORef pendingRef $ filter (\x -> threadId x /= tid) pending
                    tryReclaimZombies chan pendingRef

waitForOneChild :: MonadIO m => TChan ThreadId -> IORef [EventF] -> m ()
waitForOneChild chan pendingRef = do
    -- XXX assert pending must have at least one element
    -- assert that the tid is found in our list
    tid <- liftIO $ atomically $ readTChan chan
    liftIO $ modifyIORef pendingRef $ filter (\x -> threadId x /= tid)

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
    E.mask $ \restore ->
        liftBaseWith $ \runInIO -> forkIO $ do
            runInIO $ E.try (restore action) >>= liftIO . and_then
            return ()

forkIt :: (MonadBaseControl IO m, MonadIO m) => EventF -> (EventF -> m t) -> m ()
forkIt current proc = do
    child <- newChildCont current
    tid <- forkFinally1 (runInChild proc child)
         $ \res -> do
            thId <- myThreadId
            beforeExit child{threadId = thId} res
    updatePendingThreads current child{threadId = tid}
    return ()

    where

    updatePendingThreads :: MonadIO m => EventF -> EventF -> m ()
    updatePendingThreads cur child = do
        -- update the new thread first so that if it exited already reclaim finds
        -- it in the list and does not panic.
        liftIO $ modifyIORef (pendingThreads cur) $ (\ts -> child:ts)
        tryReclaimZombies (zombieChannel cur) (pendingThreads cur)

    newChildCont cur = do
        pendingRef <- liftIO $ newIORef []
        chan <- liftIO $ atomically newTChan
        -- shares the threadCredit of the parent by default
        return $ cur
            { parent   = Just cur
            , pendingThreads = pendingRef
            , zombieChannel = chan
            }

    runInChild m child = do
        th <- liftIO $ myThreadId
        dbg $ "Forked: child " ++ show th
        m child{threadId = th}

    beforeExit cur res = do
        case res of
            Left _exc -> -- kill all child threads
                        -- propagate the exception to the parent
                        return ()
            Right _ -> -- collect the results?
                        return ()

        waitForChildren (zombieChannel cur) (pendingThreads cur)
        signalQSemB (threadCredit cur)
        let p = fromJust (parent cur)
        liftIO $ atomically $ writeTChan (zombieChannel p) (threadId cur)

forkMaybe :: (MonadBaseControl IO m, MonadIO m) => EventF -> (EventF -> m ()) -> m ()
forkMaybe current toRun = do
    gotCredit <- liftIO $ waitQSemB (threadCredit current)
    pending <- liftIO $ readIORef $ pendingThreads current
    case gotCredit of
        False -> case pending of
                [] -> toRun current -- run synchronously
                _ -> do
                        -- XXX If we have unreclaimable child threads e.g.
                        -- infinite loop, this is going to deadlock us. We need
                        -- special handling for those cases. Add those to
                        -- unreclaimable list? And always execute them in an
                        -- async thread, cannot use sync for those.
                        waitForOneChild (zombieChannel current)
                                        (pendingThreads current)
                        forkMaybe current toRun
        True -> do
            dbg $ "Forking: parent " ++ show (threadId current)
            forkIt current toRun

-- | 'StreamData' represents a task in a task stream being generated.
data StreamData a =
      SMore a               -- ^ More tasks to come
    | SLast a               -- ^ This is the last task
    | SDone                 -- ^ No more tasks, we are done
    | SError SomeException  -- ^ An error occurred
    deriving (Typeable, Show,Read)

-- | Execute the IO action and the continuation
genAsyncEvents ::  (MonadIO m, MonadBaseControl IO m) => EventF -> IO (StreamData t) -> m ()
genAsyncEvents parentc rec =
    forkMaybe parentc $ \childcont -> loop childcont >> return ()

    where

    -- Execute the IO computation and then the closure-continuation
    loop curcont = do
        streamData <- liftIO $ rec `catch`
                \(e :: SomeException) -> return $ SError e

        let run = runContWith streamData
        case streamData of
            SMore _ -> do
                forkMaybe curcont run
                loop curcont
            _ -> run curcont

         where

        -- pass on the io result and then run the continuation
         runContWith dat cont = do
              let cont' = cont { event = Just $ unsafeCoerce dat }
              _ <- runStateT (runCont cont')  cont'
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
  cont <- get
  case event cont of
    -- we have already exceuted the ioaction and now we are continuing in a new
    -- thread. Just return the result of the ioaction stored in 'event' field.
    j@(Just _) -> do
      put cont { event = Nothing }
      return $ unsafeCoerce j
    -- we have to execute the io action and generate an event and then continue
    -- in this thread or a new thread.
    Nothing    -> do
      lift $ genAsyncEvents cont ioaction
      was <- getData `onNothing` return NoRemote
      when (was /= WasRemote) $ setData WasParallel
      return Nothing

-- | An task stream generator that produces an infinite stream of tasks by
-- running an IO computation in a loop. A task is triggered carrying the output
-- of the computation. See 'parallel' for notes on the return value.
waitEvents :: (MonadIO m, MonadBaseControl IO m) => IO a -> AsyncT m a
waitEvents io = do
  mr <- parallel (SMore <$> io)
  case mr of
    SMore  x -> return x
    SError e -> back e

-- | Run an IO computation asynchronously and generate a single task carrying
-- the result of the computation when it completes. See 'parallel' for notes on
-- the return value.
async  :: (MonadIO m, MonadBaseControl IO m) => IO a -> AsyncT m a
async io = do
  mr <- parallel (SLast <$> io)
  case mr of
    SLast  x -> return x
    SError e -> back   e

-- | Force an async computation to run synchronously. It can be useful in an
-- 'Alternative' composition to run the alternative only after finishing a
-- computation.  Note that in Applicatives it might result in an undesired
-- serialization.
sync :: MonadIO m => AsyncT m a -> AsyncT m a
sync x = do
  setData WasRemote
  r <- x
  delData WasRemote
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
              runStateT (runCont cont) cont{event= Just $ unsafeCoerce dat}
              liftIO iob
            was <- getData `onNothing` return NoRemote
            when (was /= WasRemote) $ setData WasParallel
            return Nothing

          j@(Just _) -> do
            put cont{event=Nothing}
            return $ unsafeCoerce j

-- Need to propagate the exception via kill exception handler
-- | kill all the child threads associated with the continuation context
killChildren :: IORef [EventF] -> IO ()
killChildren pendingRef  = do
    ths <- readIORef pendingRef
    mapM_ (killThread . threadId) ths

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
