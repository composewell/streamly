{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Asyncly.AsyncT
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
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
    , uncons
    , async
    , unfoldr

    , take
    , drop

    , zipWithM
    , zipWith
    , ZipSerial (..)
    , zipAsyncWithM
    , zipAsyncWith
    , ZipAsync (..)

    , interleave
    , (<=>)
    , parAhead
    , (<>|)
    , parLeft
    , (<|)

    , (>->)
    , (>>|)
    , (>|>)

    , foldWith
    , foldMapWith
    , forEachWith
    , fromCallback
    )
where

import           Control.Applicative         (Alternative (..), liftA2)
import           Control.Concurrent          (ThreadId, forkIO,
                                              myThreadId, threadDelay)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, tryTakeMVar,
                                              tryPutMVar, takeMVar)
import           Control.Exception           (SomeException (..), Exception)
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
import           Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, pushL,
                                                     tryPopR, nullQ)
import           Data.Functor                (void)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef)
import           Data.Atomics                (atomicModifyIORefCAS,
                                              atomicModifyIORefCAS_)
import           Data.Maybe                  (isNothing)
import           Data.Semigroup              (Semigroup(..), cycle1)
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Prelude                     hiding (take, drop, zipWith)

------------------------------------------------------------------------------
-- Concurrency Semantics
------------------------------------------------------------------------------
--
-- Asyncly is essentially a concurrent list transformer. To fold the lists
-- (AsyncT m a) it provides many different ways of composing them. The Monoid
-- instance folds lists in a sequential fashion. The Alternative instance folds
-- a list in a concurrent manner running each action in parallel and therefore
-- the order of results in the fold is not deterministic, but the results are
-- fairly interleaved. There are other ways to bind and compose. The
-- combinations of different types of binds and monoidal compositions can be
-- used to achieve the desired results.

------------------------------------------------------------------------------
-- Parent child thread communication types
------------------------------------------------------------------------------

data ChildEvent a =
      ChildYield a
    | ChildStop ThreadId (Maybe SomeException)

------------------------------------------------------------------------------
-- State threaded around the monad for thread management
------------------------------------------------------------------------------

data Dimension   = Conjunction | Disjunction deriving Eq
data SchedPolicy = LIFO | FIFO deriving Eq
data CtxType     = CtxType Dimension SchedPolicy deriving Eq

data Context m a =
    Context { outputQueue    :: IORef [ChildEvent a]
            , doorBell       :: MVar Bool -- wakeup mechanism for outQ
            , enqueue        :: AsyncT m a -> IO ()
            , runqueue       :: m ()
            , runningThreads :: IORef (Set ThreadId)
            , queueEmpty     :: m Bool
            , ctxType        :: CtxType
            }

-- | Represents a monadic stream of values of type 'a' resulting from actions
-- in monad 'm'. 'AsyncT' streams can be composed sequentially or in parallel
-- in various ways using monadic bind or variants of it and using monoidal
-- compositions like 'Monoid', 'Alternative' or variants of these.
newtype AsyncT m a =
    AsyncT {
        runAsyncT :: forall r.
               Maybe (Context m a)               -- local state
            -> m r                               -- stop
            -> (a -> Maybe (AsyncT m a) -> m r)  -- yield
            -> m r
    }

-- | A monad that can do asynchronous (or parallel) IO operations.
type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

-- | '<>' concatenates two 'AsyncT' streams sequentially i.e. the first stream
-- is exhausted completely before yielding any element from the second stream.
instance Semigroup (AsyncT m a) where
    m1 <> m2 = go m1
        where
        go (AsyncT m) = AsyncT $ \_ stp yld ->
                let stop = (runAsyncT m2) Nothing stp yld
                    yield a Nothing  = yld a (Just m2)
                    yield a (Just r) = yld a (Just (go r))
                in m Nothing stop yield

instance Monoid (AsyncT m a) where
    mempty = AsyncT $ \_ stp _ -> stp
    mappend = (<>)

-- | Same as '<=>'.
interleave :: AsyncT m a -> AsyncT m a -> AsyncT m a
interleave m1 m2 = AsyncT $ \_ stp yld -> do
    let stop = (runAsyncT m2) Nothing stp yld
        yield a Nothing  = yld a (Just m2)
        yield a (Just r) = yld a (Just (interleave m2 r))
    (runAsyncT m1) Nothing stop yield

infixr 5 <=>

-- | Sequential interleaved composition, similar to '<>' except that it
-- fairly interleaves the two 'AsyncT' streams, yielding an element from each
-- stream alternately.
(<=>) :: AsyncT m a -> AsyncT m a -> AsyncT m a
(<=>) = interleave

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (AsyncT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

-- The newtype ZipAsync provides a parallel zip applicative instance

instance Monad m => Applicative (AsyncT m) where
    pure a = AsyncT $ \_ _ yld -> yld a Nothing
    (<*>) = ap

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

-- | A thread context is valid only until the next bind. Upon a bind we
-- reset the context to Nothing.
{-# INLINE bindWith #-}
bindWith
    :: (forall c. AsyncT m c -> AsyncT m c -> AsyncT m c)
    -> AsyncT m a
    -> (a -> AsyncT m b)
    -> AsyncT m b
bindWith k m f = go m
    where
        go (AsyncT g) =
            AsyncT $ \_ stp yld ->
            let run x = (runAsyncT x) Nothing stp yld
                yield a Nothing  = run $ f a
                yield a (Just r) = run $ f a `k` (go r)
            in g Nothing stp yield

-- | Execute a monadic action sequentially for each element in the 'AsyncT'
-- stream, i.e. an iteration finishes completely before the next one starts.
instance Monad m => Monad (AsyncT m) where
    return = pure

    AsyncT m >>= f = AsyncT $ \_ stp yld ->
        let run x = (runAsyncT x) Nothing stp yld
            yield a Nothing  = run $ f a
            yield a (Just r) = run $ f a <> (r >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Alternative ways to bind
------------------------------------------------------------------------------

-- XXX Do we need newtype wrappers for these?

-- | Execute a monadic action for each element in the stream, in a fairly
-- interleaved manner i.e. iterations yield alternately.
(>->) :: AsyncT m a -> (a -> AsyncT m b) -> AsyncT m b
(>->) = bindWith (<=>)

{-# INLINE parbind #-}
parbind
    :: (forall c. AsyncT m c -> AsyncT m c -> AsyncT m c)
    -> AsyncT m a
    -> (a -> AsyncT m b)
    -> AsyncT m b
parbind k m f = go m
    where
        go (AsyncT g) =
            AsyncT $ \ctx stp yld ->
            let run x = (runAsyncT x) ctx stp yld
                yield a Nothing  = run $ f a
                yield a (Just r) = run $ f a `k` (go r)
            in g Nothing stp yield

-- | Execute a monadic action for each element in the stream, running
-- iterations in parallel, but giving preference to iterations started earlier.
(>>|) :: MonadAsync m => AsyncT m a -> (a -> AsyncT m b) -> AsyncT m b
(>>|) = parbind (parallel (CtxType Conjunction LIFO))

-- | Execute a monadic action for each element in the stream, running
-- iterations in a fairly parallel manner, i.e. all iterations are equally
-- likely to run.
(>|>) :: MonadAsync m => AsyncT m a -> (a -> AsyncT m b) -> AsyncT m b
(>|>) = parbind (parallel (CtxType Conjunction FIFO))

-- TBD Ideally we would want parallelism to be completely hidden from the user,
-- for example we can automatically decide where to start an action in parallel
-- so that it is most efficient. We can decide whether a particular bind should
-- fork a parallel thread or not based on the level of the bind in the tree.
-- This is equivalent to dynamically deciding (maybe based user policies
-- applied using combinators) which type of bind or monoidal composition we
-- should use. We can have another bind operator (maybe the standard >>= should
-- be used for that) for this type of behavior.

------------------------------------------------------------------------------
-- Alternative
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
enqueueLIFO :: IORef [AsyncT m a] -> AsyncT m a -> IO ()
enqueueLIFO q m = atomicModifyIORefCAS_ q $ \ ms -> m : ms

runqueueLIFO :: MonadIO m => Context m a -> IORef [AsyncT m a] -> m ()
runqueueLIFO ctx q = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> sendStop ctx
            Just m -> (runAsyncT m) (Just ctx) run yield

    sendit a = send ctx (ChildYield a)
    yield a Nothing  = sendit a >> run
    yield a (Just r) = sendit a >> (runAsyncT r) (Just ctx) run yield

    dequeue = liftIO $ atomicModifyIORefCAS q $ \ ms ->
        case ms of
            [] -> ([], Nothing)
            x : xs -> (xs, Just x)

{-# INLINE enqueueFIFO #-}
enqueueFIFO :: LinkedQueue (AsyncT m a) -> AsyncT m a -> IO ()
enqueueFIFO = pushL

runqueueFIFO :: MonadIO m => Context m a -> LinkedQueue (AsyncT m a) -> m ()
runqueueFIFO ctx q = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> sendStop ctx
            Just m -> (runAsyncT m) (Just ctx) run yield

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
    liftIO $ threadDelay 200
    output <- liftIO $ readIORef (outputQueue ctx)
    when (null output) $ do
        done <- queueEmpty ctx
        if (not done)
        then (pushWorker ctx) >> sendWorkerWait ctx
        else void (liftIO $ takeMVar (doorBell ctx))

data ContextUsedAfterEOF = ContextUsedAfterEOF deriving Show
instance Exception ContextUsedAfterEOF

-- | Pull an AsyncT stream from a context
{-# NOINLINE pullFromCtx #-}
pullFromCtx :: MonadAsync m => Context m a -> AsyncT m a
pullFromCtx ctx = AsyncT $ \_ stp yld -> do
    -- When using an async handle to the context, one may keep using a stale
    -- context even after it has been fullt drained. To detect it gracefully we
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
    (runAsyncT $ processEvents (reverse list)) Nothing stp yld

    where

    handleException e tid = do
        delThread ctx tid
        -- XXX implement kill async exception handling
        -- liftIO $ readIORef (runningThreads ctx) >>= mapM_ killThread
        throwM e

    {-# INLINE processEvents #-}
    processEvents [] = AsyncT $ \_ stp yld -> do
        done <- allThreadsDone ctx
        if not done
        then (runAsyncT (pullFromCtx ctx)) Nothing stp yld
        else stp

    processEvents (ev : es) = AsyncT $ \_ stp yld -> do
        let continue = (runAsyncT (processEvents es)) Nothing stp yld
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
    => CtxType -> AsyncT m a -> AsyncT m a -> m (Context m a)
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

{-# NOINLINE pushPullFork #-}
pushPullFork :: MonadAsync m
    => CtxType -> AsyncT m a -> AsyncT m a -> AsyncT m a
pushPullFork ct m1 m2 = AsyncT $ \_ stp yld -> do
    ctx <- pushPairToCtx ct m1 m2
    (runAsyncT (pullFromCtx ctx)) Nothing stp yld

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

-- Note: This is designed to scale for right associated compositions,
-- therefore always use a right fold for folding bigger structures.
{-# INLINE parallel #-}
parallel :: MonadAsync m => CtxType -> AsyncT m a -> AsyncT m a -> AsyncT m a
parallel ct m1 m2 = AsyncT $ \ctx stp yld -> do
    case ctx of
        Nothing -> (runAsyncT (pushPullFork ct m1 m2)) Nothing stp yld
        Just c | ctxType c /= ct ->
            (runAsyncT (pushPullFork ct m1 m2)) Nothing stp yld
        Just c -> liftIO ((enqueue c) m2) >> (runAsyncT m1) ctx stp yld

-- | `empty` represents an action that takes non-zero time to complete.  Since
-- all actions take non-zero time, an `Alternative` composition ('<|>') is a
-- monoidal composition executing all actions in parallel, it is similar to
-- '<>' except that it runs all the actions in parallel and interleaves their
-- results fairly.
instance MonadAsync m => Alternative (AsyncT m) where
    empty = mempty

    {-# INLINE (<|>) #-}
    (<|>) = parallel (CtxType Disjunction FIFO)

------------------------------------------------------------------------------
-- Other monoidal compositions for parallel actions
------------------------------------------------------------------------------

-- | Same as '<>|'.
parAhead :: AsyncT m a -> AsyncT m a -> AsyncT m a
parAhead = undefined

-- | Sequential composition similar to '<>' except that it can execute the
-- action on the right in parallel ahead of time. Returns the results in
-- sequential order like '<>' from left to right.
(<>|) :: AsyncT m a -> AsyncT m a -> AsyncT m a
(<>|) = parAhead

-- | Same as '<|'.
{-# INLINE parLeft #-}
parLeft :: MonadAsync m => AsyncT m a -> AsyncT m a -> AsyncT m a
parLeft = parallel (CtxType Disjunction LIFO)

-- | Parallel composition similar to `<|>` except that it is left-biased
-- instead of being fair.  Action on the left is likely to be given a chance to
-- execute before the action on the right. If the left action keeps yielding
-- results without blocking it continues running until it finishes and only
-- then the right action runs.
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
    lift mx = AsyncT $ \_ _ yld -> mx >>= (\a -> (yld a Nothing))

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
            yield a Nothing = yld a Nothing
            yield a (Just r) = yld a (Just (catchError r h))
        in handle $ (runAsyncT m) ctx stp yield

instance MonadReader r m => MonadReader r (AsyncT m) where
    ask = lift ask
    local f m = AsyncT $ \ctx stp yld ->
        let yield a Nothing  = local f $ yld a Nothing
            yield a (Just r) = local f $ yld a (Just (local f r))
        in (runAsyncT m) ctx (local f stp) yield

instance MonadState s m => MonadState s (AsyncT m) where
    get     = lift get
    put x   = lift (put x)
    state k = lift (state k)

------------------------------------------------------------------------------
-- Running the monad
------------------------------------------------------------------------------

-- | Run an 'AsyncT' computation, wait for it to finish and discard the
-- results.
runAsyncly :: MonadAsync m => AsyncT m a -> m ()
runAsyncly m = (runAsyncT m) Nothing stop yield

    where

    stop = return ()

    {-# INLINE yield #-}
    yield _ Nothing  = stop
    yield _ (Just x) = runAsyncly x


-- | Collect the results of an 'AsyncT' stream into a list.
{-# INLINABLE toList #-}
toList :: MonadAsync m => AsyncT m a -> m [a]
toList m = (runAsyncT m) Nothing stop yield

    where

    stop = return []

    {-# INLINE yield #-}
    yield a Nothing  = return [a]
    yield a (Just x) = liftM (a :) (toList x)

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: MonadAsync m => AsyncT m a -> m (Maybe (a, AsyncT m a))
uncons m = (runAsyncT m) Nothing stop yield

    where

    stop = return Nothing

    {-# INLINE yield #-}
    yield a Nothing  = return (Just (a, empty))
    yield a (Just x) = return (Just (a, x))

-- | Build a Stream by unfolding steps starting from a seed.
unfoldr :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> AsyncT m a
unfoldr step = go
    where
    go s = AsyncT $ \_ stp yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
take :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
take n m = AsyncT $ \ctx stp yld -> do
    let yield a Nothing  = yld a Nothing
        yield a (Just x) = yld a (Just (take (n - 1) x))
    if (n <= 0)
    then stp
    else (runAsyncT m) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
drop n m = AsyncT $ \ctx stp yld -> do
    let yield _ Nothing  = stp
        yield _ (Just x) = (runAsyncT $ drop (n - 1) x) ctx stp yld
    if (n <= 0)
    then (runAsyncT m) ctx stp yld
    else (runAsyncT m) ctx stp yield

------------------------------------------------------------------------------
-- Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams serially using a monadic zipping function.
zipWithM :: MonadAsync m =>
    (a -> b -> AsyncT m c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipWithM f m1 m2 = AsyncT $ \_ stp yld -> do
    let merge a ra =
            let yield2 b Nothing   = (runAsyncT (f a b)) Nothing stp yld
                yield2 b (Just rb) =
                    (runAsyncT ((f a b) <> (zipWithM f ra rb))) Nothing stp yld
             in (runAsyncT m2) Nothing stp yield2
    let yield1 a Nothing   = merge a empty
        yield1 a (Just ra) = merge a ra
    (runAsyncT m1) Nothing stp yield1

-- | Zip two AsyncT streams serially using a pure zipping function.
zipWith :: MonadAsync m
    => (a -> b -> c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipWith f m1 m2 = AsyncT $ \_ stp yld -> do
    let merge a ra =
            let yield2 b Nothing   = yld (f a b) Nothing
                yield2 b (Just rb) = yld (f a b) (Just (zipWith f ra rb))
             in (runAsyncT m2) Nothing stp yield2
    let yield1 a Nothing   = merge a empty
        yield1 a (Just ra) = merge a ra
    (runAsyncT m1) Nothing stp yield1

-- | Wrapper around AsyncT type with a serial zipping Applicative instance.
-- Note that the binary function interleave (\<=>) is a special case of
-- ZipSerial Applicative.
--
-- > f <$> ZipSerial xs1 <*> ... <*> ZipSerial xsN
newtype ZipSerial m a = ZipSerial {getZipSerial :: AsyncT m a}
        deriving (Functor)

instance MonadAsync m => Applicative (ZipSerial m) where
    pure a = ZipSerial $ cycle1 (pure a)
    (ZipSerial xs) <*> (ZipSerial ys) = ZipSerial (zipWith id xs ys)

-- | The computation specified in the argument is pushed to a new thread and
-- the context is computation pulls output from the thread.
pushOneToCtx :: MonadAsync m => CtxType -> AsyncT m a -> m (Context m a)
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

-- XXX The async API is useful for exploring each stream arbitrarily when
-- zipping or merging two streams. We can use a newtype wrapper with a monad
-- instance that composes like regular streaming libraries to facilitate linear
-- composition.  We will also need a yield API for that.

-- | Run a computation asynchronously, triggers the computation and returns
-- another computation (i.e. a promise) that when executed produces the output
-- from the original computation. Note that the returned action must be
-- executed exactly once and drained completely. If not executed or not drained
-- fully we will may have a thread blocked forever and if executed more than
-- once a ContextUsedAfterEOF exception will be raised.

async :: MonadAsync m => AsyncT m a -> m (AsyncT m a)
async m = do
    ctx <- pushOneToCtx (CtxType Disjunction LIFO) m
    return $ pullFromCtx ctx

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a monadic zipping function.
zipAsyncWithM :: MonadAsync m
    => (a -> b -> AsyncT m c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipAsyncWithM f m1 m2 = AsyncT $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runAsyncT (zipWithM f ma mb)) Nothing stp yld

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a pure zipping function.
zipAsyncWith :: MonadAsync m
    => (a -> b -> c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipAsyncWith f m1 m2 = AsyncT $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runAsyncT (zipWith f ma mb)) Nothing stp yld

-- | Wrapper around AsyncT type with a parallel zipping Applicative instance.
-- Note that the binary operator (\<|>) from the Alternative instance of AsyncT
-- is a special case of ZipAsync Applicative.
--
-- > f <$> ZipAsync xs1 <*> ... <*> ZipAsync xsN
newtype ZipAsync m a = ZipAsync {getZipAsync :: AsyncT m a}
        deriving (Functor)

instance MonadAsync m => Applicative (ZipAsync m) where
    pure a = ZipAsync $ cycle1 (pure a)
    (ZipAsync xs) <*> (ZipAsync ys) = ZipAsync (zipAsyncWith id xs ys)

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

------------------------------------------------------------------------------
-- Convert a callback into an 'AsyncT' computation
------------------------------------------------------------------------------

-- | Convert a callback into an 'AsyncT' stream.
fromCallback :: (forall r. (a -> m r) -> m r) -> AsyncT m a
fromCallback k = AsyncT $ \_ _ yld -> k (\a -> yld a Nothing)
