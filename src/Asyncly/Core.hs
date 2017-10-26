{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Asyncly.Core
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Asyncly.Core
    ( Stream (..)
    , MonadAsync
    , Streaming (..)
    , ContextUsedAfterEOF (..)

    -- * Running the Monad
    , runStreaming

    -- * Interleaving
    , interleave
    , (<=>)

    -- * Asynchronous Operations
    , CtxType (..)
    , SchedPolicy (..)
    , Dimension (..)
    , yielding
    , parallel
    , parAlt
    , parLeft
    , (<|)
    , async

    -- * Fold Utilities
    , foldWith
    , foldMapWith
    , forEachWith
    )
where

import           Control.Applicative         (Alternative (..))
import           Control.Concurrent          (ThreadId, forkIO,
                                              myThreadId, threadDelay)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, tryTakeMVar,
                                              tryPutMVar, takeMVar)
import           Control.Exception           (SomeException (..), Exception)
import qualified Control.Exception.Lifted    as EL
import           Control.Monad               (MonadPlus(..), mzero, when)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.Error.Class   (MonadError(..))
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Reader.Class  (MonadReader(..))
import           Control.Monad.State.Class   (MonadState(..))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import           Data.Atomics                (atomicModifyIORefCAS,
                                              atomicModifyIORefCAS_)
import           Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, pushL,
                                                     tryPopR, nullQ)
import           Data.Functor                (void)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef)
import           Data.Maybe                  (isNothing)
import           Data.Semigroup              (Semigroup(..))
import           Data.Set                    (Set)
import qualified Data.Set                    as S

------------------------------------------------------------------------------
-- Parent child thread communication type
------------------------------------------------------------------------------

-- | Events that a child thread may send to a parent thread.
data ChildEvent a =
      ChildYield a
    | ChildStop ThreadId (Maybe SomeException)

------------------------------------------------------------------------------
-- State threaded around the monad for thread management
------------------------------------------------------------------------------

-- | Conjunction is used for monadic/product style composition. Disjunction is
-- used for fold/sum style composition. We need to distiguish the two types of
-- contexts so that the scheduing of the two is independent.
data Dimension   = Conjunction | Disjunction deriving Eq

-- | For fairly interleaved parallel composition the sched policy is FIFO
-- whereas for left biased parallel composition it is LIFO.
data SchedPolicy = LIFO | FIFO deriving Eq

-- | Identify the type of the context used for parallel composition.
data CtxType     = CtxType Dimension SchedPolicy deriving Eq

-- | A Context represents a parallel composition. It has a runqueue which holds
-- the tasks that are to be picked by a pool of worker threads. It has an
-- output queue where the results are placed by the worker threads. A
-- doorBell is used by the worker threads to intimate the consumer thread
-- about availability of new results. New work is usually enqueued as a result
-- of executing the parallel combinators i.e. '<|' and '<|>'.
data Context m a =
    Context { outputQueue    :: IORef [ChildEvent a]
            , doorBell       :: MVar Bool -- wakeup mechanism for outQ
            , enqueue        :: Stream m a -> IO ()
            , runqueue       :: m ()
            , runningThreads :: IORef (Set ThreadId)
            , queueEmpty     :: m Bool
            , ctxType        :: CtxType
            }

------------------------------------------------------------------------------
-- The stream type
------------------------------------------------------------------------------

-- | Represents a monadic stream of values of type 'a' constructed using
-- actions in monad 'm'. Streams can be composed sequentially or in parallel in
-- product style compositions (monadic bind multiplies streams in a ListT
-- fashion) and using sum style compositions like 'Semigroup', 'Monoid',
-- 'Alternative' or variants of these.
newtype Stream m a =
    Stream {
        runStream :: forall r.
               Maybe (Context m a)               -- local state
            -> m r                               -- stop
            -> (a -> Maybe (Stream m a) -> m r)  -- yield
            -> m r
    }

-- | A monad that can perform asynchronous/concurrent IO operations.
type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

-- | Yield a singleton value in a stream.
yielding :: a -> Stream m a
yielding a = Stream $ \_ _ yld -> yld a Nothing

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | '<>' concatenates two streams sequentially i.e. the first stream is
-- exhausted completely before yielding any element from the second stream.
instance Semigroup (Stream m a) where
    m1 <> m2 = go m1
        where
        go (Stream m) = Stream $ \_ stp yld ->
                let stop = (runStream m2) Nothing stp yld
                    yield a Nothing  = yld a (Just m2)
                    yield a (Just r) = yld a (Just (go r))
                in m Nothing stop yield

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (Stream m a) where
    mempty = Stream $ \_ stp _ -> stp
    mappend = (<>)

------------------------------------------------------------------------------
-- Interleave
------------------------------------------------------------------------------

-- | Same as '<=>'.
interleave :: Stream m a -> Stream m a -> Stream m a
interleave m1 m2 = Stream $ \_ stp yld -> do
    let stop = (runStream m2) Nothing stp yld
        yield a Nothing  = yld a (Just m2)
        yield a (Just r) = yld a (Just (interleave m2 r))
    (runStream m1) Nothing stop yield

------------------------------------------------------------------------------
-- Spawning threads and collecting result in streamed fashion
------------------------------------------------------------------------------

{-# INLINE doFork #-}
doFork :: MonadBaseControl IO m
    => m ()
    -> (SomeException -> m ())
    -> m ThreadId
doFork action exHandler =
    EL.mask $ \restore ->
        liftBaseWith $ \runInIO -> forkIO $ do
            -- XXX test the exception handling
            _ <- runInIO $ EL.catch (restore action) exHandler
            -- XXX restore state here?
            return ()

-- XXX exception safety of all atomic/MVar operations

{-# INLINE send #-}
send :: MonadIO m => Context m a -> ChildEvent a -> m ()
send ctx msg = liftIO $ do
    atomicModifyIORefCAS_ (outputQueue ctx) $ \es -> msg : es
    -- XXX need a memory barrier? The wake up must happen only after the
    -- store has finished otherwise we can have lost wakeup problems.
    void $ tryPutMVar (doorBell ctx) True

{-# INLINE sendStop #-}
sendStop :: MonadIO m => Context m a -> m ()
sendStop ctx = liftIO myThreadId >>= \tid -> send ctx (ChildStop tid Nothing)

-- Note: Left associated operations can grow this queue to a large size
{-# INLINE enqueueLIFO #-}
enqueueLIFO :: IORef [Stream m a] -> Stream m a -> IO ()
enqueueLIFO q m = atomicModifyIORefCAS_ q $ \ ms -> m : ms

runqueueLIFO :: MonadIO m => Context m a -> IORef [Stream m a] -> m ()
runqueueLIFO ctx q = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> sendStop ctx
            Just m -> (runStream m) (Just ctx) run yield

    sendit a = send ctx (ChildYield a)
    yield a Nothing  = sendit a >> run
    yield a (Just r) = sendit a >> (runStream r) (Just ctx) run yield

    dequeue = liftIO $ atomicModifyIORefCAS q $ \ ms ->
        case ms of
            [] -> ([], Nothing)
            x : xs -> (xs, Just x)

{-# INLINE enqueueFIFO #-}
enqueueFIFO :: LinkedQueue (Stream m a) -> Stream m a -> IO ()
enqueueFIFO = pushL

runqueueFIFO :: MonadIO m => Context m a -> LinkedQueue (Stream m a) -> m ()
runqueueFIFO ctx q = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> sendStop ctx
            Just m -> (runStream m) (Just ctx) run yield

    dequeue = liftIO $ tryPopR q
    sendit a = send ctx (ChildYield a)
    yield a Nothing  = sendit a >> run
    yield a (Just r) = sendit a >> liftIO (enqueueFIFO q r) >> run

-- Thread tracking is needed for two reasons:
--
-- 1) Killing threads on exceptions. Threads may not be allowed to go away by
-- themselves because they may run for significant times before going away or
-- worse they may be stuck in IO and never go away.
--
-- 2) To know when all threads are done.

{-# NOINLINE addThread #-}
addThread :: MonadIO m => Context m a -> ThreadId -> m ()
addThread ctx tid =
    liftIO $ modifyIORef (runningThreads ctx) $ (\s -> S.insert tid s)

{-# INLINE delThread #-}
delThread :: MonadIO m => Context m a -> ThreadId -> m ()
delThread ctx tid =
    liftIO $ modifyIORef (runningThreads ctx) $ (\s -> S.delete tid s)

{-# INLINE allThreadsDone #-}
allThreadsDone :: MonadIO m => Context m a -> m Bool
allThreadsDone ctx = liftIO $ do
    readIORef (runningThreads ctx) >>= return . S.null

{-# NOINLINE handleChildException #-}
handleChildException :: MonadIO m => Context m a -> SomeException -> m ()
handleChildException ctx e = do
    tid <- liftIO myThreadId
    send ctx (ChildStop tid (Just e))

{-# NOINLINE pushWorker #-}
pushWorker :: MonadAsync m => Context m a -> m ()
pushWorker ctx =
    doFork (runqueue ctx) (handleChildException ctx) >>= addThread ctx

-- XXX When the queue is LIFO we can put a limit on the number of dispatches.
-- Also, if a worker blocks on the output queue we can decide if we want to
-- block or make it go away entirely, depending on the number of workers and
-- the type of the queue.
{-# INLINE sendWorkerWait #-}
sendWorkerWait :: MonadAsync m => Context m a -> m ()
sendWorkerWait ctx = do
    case ctxType ctx of
        CtxType _ LIFO -> liftIO $ threadDelay 200
        CtxType _ FIFO -> liftIO $ threadDelay 0

    output <- liftIO $ readIORef (outputQueue ctx)
    when (null output) $ do
        done <- queueEmpty ctx
        if (not done)
        then (pushWorker ctx) >> sendWorkerWait ctx
        else void (liftIO $ takeMVar (doorBell ctx))

-- | An 'async' stream has finished but is still being used.
data ContextUsedAfterEOF = ContextUsedAfterEOF deriving Show
instance Exception ContextUsedAfterEOF

-- | Pull a stream from a context
{-# NOINLINE pullFromCtx #-}
pullFromCtx :: MonadAsync m => Context m a -> Stream m a
pullFromCtx ctx = Stream $ \_ stp yld -> do
    -- When using an async handle to the context, one may keep using a stale
    -- context even after it has been fully drained. To detect it gracefully we
    -- raise an explicit exception.
    -- XXX if reading the IORef is costly we can use a flag in the context to
    -- indicate we are done.
    done <- allThreadsDone ctx
    when done $ throwM ContextUsedAfterEOF

    res <- liftIO $ tryTakeMVar (doorBell ctx)
    when (isNothing res) $ sendWorkerWait ctx
    list <- liftIO $ atomicModifyIORefCAS (outputQueue ctx) $ \x -> ([], x)
    -- To avoid lock overhead we read all events at once instead of reading one
    -- at a time. We just reverse the list to process the events in the order
    -- they arrived. Maybe we can use a queue instead?
    (runStream $ processEvents (reverse list)) Nothing stp yld

    where

    handleException e tid = do
        delThread ctx tid
        -- XXX implement kill async exception handling
        -- liftIO $ readIORef (runningThreads ctx) >>= mapM_ killThread
        throwM e

    {-# INLINE processEvents #-}
    processEvents [] = Stream $ \_ stp yld -> do
        done <- allThreadsDone ctx
        if not done
        then (runStream (pullFromCtx ctx)) Nothing stp yld
        else stp

    processEvents (ev : es) = Stream $ \_ stp yld -> do
        let continue = (runStream (processEvents es)) Nothing stp yld
            yield a  = yld a (Just (processEvents es))

        case ev of
            ChildYield a -> yield a
            ChildStop tid e ->
                case e of
                    Nothing -> delThread ctx tid >> continue
                    Just ex -> handleException ex tid

getFifoCtx :: MonadIO m => CtxType -> IO (Context m a)
getFifoCtx ctype = do
    outQ    <- newIORef []
    outQMv  <- newEmptyMVar
    running <- newIORef S.empty
    q       <- newQ
    let ctx =
            Context { outputQueue    = outQ
                    , doorBell       = outQMv
                    , runningThreads = running
                    , runqueue       = runqueueFIFO ctx q
                    , enqueue        = pushL q
                    , queueEmpty     = liftIO $ nullQ q
                    , ctxType        = ctype
                    }
     in return ctx

getLifoCtx :: MonadIO m => CtxType -> IO (Context m a)
getLifoCtx ctype = do
    outQ    <- newIORef []
    outQMv  <- newEmptyMVar
    running <- newIORef S.empty
    q <- newIORef []
    let checkEmpty = liftIO (readIORef q) >>= return . null
    let ctx =
            Context { outputQueue    = outQ
                    , doorBell       = outQMv
                    , runningThreads = running
                    , runqueue       = runqueueLIFO ctx q
                    , enqueue        = enqueueLIFO q
                    , queueEmpty     = checkEmpty
                    , ctxType        = ctype
                    }
     in return ctx

-- | Split the original computation in a pull-push pair. The original
-- computation pulls from a Channel while m1 and m2 push to the channel.
pushPairToCtx :: MonadAsync m
    => CtxType -> Stream m a -> Stream m a -> m (Context m a)
pushPairToCtx ctype m1 m2 = do
    -- Note: We must have all the work on the queue before sending the
    -- pushworker, otherwise the pushworker may exit before we even get a
    -- chance to push.
    ctx <- liftIO $
        case ctype of
            CtxType _ FIFO -> do
                c <- getFifoCtx ctype
                (enqueue c) m1 >> (enqueue c) m2
                return c
            CtxType _ LIFO -> do
                c <- getLifoCtx ctype
                (enqueue c) m2 >> (enqueue c) m1
                return c
    pushWorker ctx
    return ctx

-- | The computation specified in the argument is pushed to a new thread and
-- the context is returned. The returned context can be used to pull the output
-- of the computation.
pushOneToCtx :: MonadAsync m => CtxType -> Stream m a -> m (Context m a)
pushOneToCtx ctype m = do
    ctx <- liftIO $
        case ctype of
            CtxType _ FIFO -> do
                c <- getFifoCtx ctype
                return c
            CtxType _ LIFO -> do
                c <- getLifoCtx ctype
                return c
    -- Note: We must have all the work on the queue before sending the
    -- pushworker, otherwise the pushworker may exit before we even get a
    -- chance to push.
    liftIO $ (enqueue ctx) m
    pushWorker ctx
    return ctx

{-# NOINLINE pushPullFork #-}
pushPullFork :: MonadAsync m
    => CtxType -> Stream m a -> Stream m a -> Stream m a
pushPullFork ct m1 m2 = Stream $ \_ stp yld -> do
    ctx <- pushPairToCtx ct m1 m2
    (runStream (pullFromCtx ctx)) Nothing stp yld

-- Concurrency rate control. Our objective is to create more threads on demand
-- if the consumer is running faster than us. As soon as we encounter an
-- Alternative composition we create a push pull pair of threads. We use a
-- channel for communication between the consumer pulling from the channel and
-- the producer who pushing to the channel. The producer creates more threads
-- if no output is seen on the channel, that is the consumer is running faster.
-- However this mechanism can be problematic if the initial production latency
-- is high, we may end up creating too many threads. So we need some way to
-- monitor and use the latency as well.
--
-- TBD We may run computations at the lower level of the composition tree
-- serially even if they are composed using a parallel combinator. We can use
-- <> in place of <| and <=> in place of <|>. If we find that a parallel
-- channel immediately above a computation becomes empty we can switch to
-- parallelizing the computation.  For that we can use a state flag to fork the
-- rest of the computation at any point of time inside the Monad bind operation
-- if the consumer is running at a faster speed.
--
-- TBD the alternative composition allows us to dispatch a chunkSize of only 1.
-- If we have to dispatch in arbitrary chunksizes we will need to compose the
-- parallel actions using a data constructor (Free Alternative) instead so that
-- we can divide it in chunks of arbitrary size before dispatch. If the stream
-- is composed of hierarchically composed grains of different sizes then we can
-- always switch to a desired granularity depending on the consumer speed.
--
-- TBD for pure work (when we are not in the IO monad) we can divide it into
-- just the number of CPUs.
--
-- XXX to rate control left folded structrues we will have to return the
-- residual work back to the dispatcher. It will also consume a lot of
-- memory due to queueing of all the work before execution starts.

-- | Compose two streams in parallel using a scheduling policy specified by
-- 'CtxType'.  Note: This is designed to scale for right associated
-- compositions, therefore always use a right fold for folding bigger
-- structures.
{-# INLINE parallel #-}
parallel :: MonadAsync m => CtxType -> Stream m a -> Stream m a -> Stream m a
parallel ct m1 m2 = Stream $ \ctx stp yld -> do
    case ctx of
        Nothing -> (runStream (pushPullFork ct m1 m2)) Nothing stp yld
        Just c | ctxType c /= ct ->
            (runStream (pushPullFork ct m1 m2)) Nothing stp yld
        Just c -> liftIO ((enqueue c) m2) >> (runStream m1) ctx stp yld

------------------------------------------------------------------------------
-- Semigroup and Monoid style compositions for parallel actions
------------------------------------------------------------------------------

{-
-- | Same as '<>|'.
parAhead :: Stream m a -> Stream m a -> Stream m a
parAhead = undefined

-- | Sequential composition similar to '<>' except that it can execute the
-- action on the right in parallel ahead of time. Returns the results in
-- sequential order like '<>' from left to right.
(<>|) :: Stream m a -> Stream m a -> Stream m a
(<>|) = parAhead
-}

-- | Same as '<|>'.
{-# INLINE parAlt #-}
parAlt :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parAlt = parallel (CtxType Disjunction FIFO)

-- | Same as '<|'.
{-# INLINE parLeft #-}
parLeft :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parLeft = parallel (CtxType Disjunction LIFO)

-------------------------------------------------------------------------------
-- Instances (only used for deriving newtype instances)
-------------------------------------------------------------------------------

-- Stream type is not exposed, these instances are only for deriving instances
-- for the newtype wrappers based on Stream.

-- Dummy Instances, defined to enable the definition of other instances that
-- require a Monad constraint.  Must be defined by the newtypes.

instance Monad m => Functor (Stream m) where
    fmap = undefined

instance Monad m => Applicative (Stream m) where
    pure = undefined
    (<*>) = undefined

instance Monad m => Monad (Stream m) where
    return = pure
    (>>=) = undefined

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

-- | `empty` represents an action that takes non-zero time to complete.  Since
-- all actions take non-zero time, an `Alternative` composition ('<|>') is a
-- monoidal composition executing all actions in parallel, it is similar to
-- '<>' except that it runs all the actions in parallel and interleaves their
-- results fairly.
instance MonadAsync m => Alternative (Stream m) where
    empty = mempty
    (<|>) = parAlt

instance MonadAsync m => MonadPlus (Stream m) where
    mzero = empty
    mplus = (<|>)

-------------------------------------------------------------------------------
-- Transformer
-------------------------------------------------------------------------------

instance MonadTrans Stream where
    lift mx = Stream $ \_ _ yld -> mx >>= (\a -> (yld a Nothing))

instance (MonadBase b m, Monad m) => MonadBase b (Stream m) where
    liftBase = liftBaseDefault

------------------------------------------------------------------------------
-- Standard transformer instances
------------------------------------------------------------------------------

instance MonadIO m => MonadIO (Stream m) where
    liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (Stream m) where
    throwM = lift . throwM

-- XXX handle and test cross thread state transfer
instance MonadError e m => MonadError e (Stream m) where
    throwError     = lift . throwError
    catchError m h = Stream $ \ctx stp yld ->
        let handle r = r `catchError` \e -> (runStream (h e)) ctx stp yld
            yield a Nothing = yld a Nothing
            yield a (Just r) = yld a (Just (catchError r h))
        in handle $ (runStream m) ctx stp yield

instance MonadReader r m => MonadReader r (Stream m) where
    ask = lift ask
    local f m = Stream $ \ctx stp yld ->
        let yield a Nothing  = local f $ yld a Nothing
            yield a (Just r) = local f $ yld a (Just (local f r))
        in (runStream m) ctx (local f stp) yield

instance MonadState s m => MonadState s (Stream m) where
    get     = lift get
    put x   = lift (put x)
    state k = lift (state k)

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

-- | Types that can represent a stream of elements.
class Streaming t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a

------------------------------------------------------------------------------
-- Some basic stream type agnostic APIs
------------------------------------------------------------------------------

-- XXX The async API is useful for exploring each stream arbitrarily when
-- zipping or merging two streams. We can use a newtype wrapper with a monad
-- instance that composes like regular streaming libraries to facilitate linear
-- composition.  We will also need a yield API for that.

-- | Run a computation asynchronously, triggers the computation and returns
-- another computation that when executed produces the output generated by the
-- original computation. Note that the returned action must be executed exactly
-- once and drained completely. If not executed or not drained fully we may
-- have a thread blocked forever and if executed more than once a
-- 'ContextUsedAfterEOF' exception will be raised.

async :: (Streaming t, MonadAsync m) => t m a -> m (t m a)
async m = do
    ctx <- pushOneToCtx (CtxType Disjunction LIFO) (toStream m)
    return $ fromStream $ pullFromCtx ctx

infixr 5 <=>

-- | Sequential interleaved composition, in contrast to '<>' this operator
-- fairly interleaves the two streams instead of appending them; yielding one
-- element from each stream alternately. Unlike '<>' it cannot be used to fold
-- an infinite container of streams.
{-# INLINE (<=>) #-}
(<=>) :: Streaming t => t m a -> t m a -> t m a
m1 <=> m2 = fromStream $ interleave (toStream m1) (toStream m2)

-- | Parallel interleaved composition, in contrast to '<|>' this operator
-- "merges" streams in a left biased manner rather than fairly interleaving
-- them.  It keeps yielding from the stream on the left as long as it can. If
-- the left stream blocks or cannot keep up with the pace of the consumer it
-- can yield from the stream on the right in parallel.  Unlike '<|>' it can be
-- used to fold infinite containers of streams.
{-# INLINE (<|) #-}
(<|) :: (Streaming t, MonadAsync m) => t m a -> t m a -> t m a
m1 <| m2 = fromStream $ parLeft (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Running the monad
------------------------------------------------------------------------------

-- | Run a composed streaming computation, wait for it to finish and discard
-- the results.
runStreaming :: (Monad m, Streaming t) => t m a -> m ()
runStreaming m = go (toStream m)

    where

    go m1 = (runStream m1) Nothing stop yield

    stop = return ()

    {-# INLINE yield #-}
    yield _ Nothing  = stop
    yield _ (Just x) = go x

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

-- | Fold a 'Foldable' container using a function that is a composition of the
-- first and the third argument.
{-# INLINABLE forEachWith #-}
forEachWith :: (Monoid b, Foldable t) =>
    (b1 -> b -> b) -> t a -> (a -> b1) -> b
forEachWith f xs g = foldr (f . g) mempty xs
