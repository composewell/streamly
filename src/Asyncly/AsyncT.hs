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
    , take
    , drop

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
import           Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, pushL,
                                                     tryPopR, nullQ)
import           Data.Functor                (void)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef)
import           Data.Atomics                (atomicModifyIORefCAS,
                                              atomicModifyIORefCAS_)
import           Data.Maybe                  (isNothing)
import           Data.Semigroup              (Semigroup(..))
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Prelude                     hiding (take, drop)

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

data Context m a =
    Context { outputQueue    :: IORef [ChildEvent a]
            , doorBell       :: MVar Bool -- wakeup mechanism for outQ
            , enqueue        :: AsyncT m a -> IO ()
            , runqueue       :: m ()
            , runningThreads :: IORef (Set ThreadId)
            , queueEmpty     :: m Bool
            }

-- | Represents a monadic stream of values of type 'a' resulting from actions
-- in monad 'm'. 'AsyncT' streams can be composed sequentially or in parallel
-- in various ways using monadic bind or variants of it and using monoidal
-- compositions like 'Monoid', 'Alternative' or variants of these.
newtype AsyncT m a =
    AsyncT {
        runAsyncT :: forall r.
               Maybe (Context m a)                          -- state
            -> m r                                          -- stop
            -> (a -> Maybe (Context m a) -> Maybe (AsyncT m a) -> m r)  -- yield
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
        go (AsyncT m) = AsyncT $ \ctx stp yld ->
                let stop = (runAsyncT m2) ctx stp yld
                    yield a c Nothing  = yld a c (Just m2)
                    yield a c (Just r) = yld a c (Just (go r))
                in m ctx stop yield

instance Monoid (AsyncT m a) where
    mempty = AsyncT $ \_ stp _ -> stp
    mappend = (<>)

-- | Same as '<=>'.
interleave :: AsyncT m a -> AsyncT m a -> AsyncT m a
interleave m1 m2 = AsyncT $ \ctx stp yld -> do
    let stop = (runAsyncT m2) ctx stp yld
        yield a c Nothing  = yld a c (Just m2)
        yield a c (Just r) = yld a c (Just (interleave m2 r))
    (runAsyncT m1) ctx stop yield

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

-- Note: We cannot have the applicative instance run the two actions in
-- parallel. This is because the first action can always return empty and in
-- case it returns empty we should not be starting the second action. Therefore
-- we can only start the second action after getting a result from the first.
-- For running applicative actions in parallel we can use the async package and
-- lift the result into AsyncT.

instance Monad m => Applicative (AsyncT m) where
    pure a = AsyncT $ \ctx _ yld -> yld a ctx Nothing
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
                yield a _ Nothing  = run $ f a
                yield a _ (Just r) = run $ f a `k` (go r)
            in g Nothing stp yield

-- | '>>=' execute a monadic action for each element generated by the 'AsyncT'
-- stream, running nested loops sequentially, in a depth first manner i.e.  the
-- inner loop finishes completely before the next iteration of the outer loop
-- can run.
instance Monad m => Monad (AsyncT m) where
    return = pure

    AsyncT m >>= f = AsyncT $ \_ stp yld ->
        let run x = (runAsyncT x) Nothing stp yld
            yield a _ Nothing  = run $ f a
            yield a _ (Just r) = run $ f a <> (r >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Alternative ways to bind
------------------------------------------------------------------------------

-- | Execute a monadic action for each element in the stream, running nested
-- loops sequentially, but in a fairly interleaved manner i.e. both inner and
-- outer loop yield alternately.
(>->) :: AsyncT m a -> (a -> AsyncT m b) -> AsyncT m b
(>->) = bindWith (<=>)

-- | Execute a monadic action for each element in the stream, running nested
-- loops in parallel, but with a depth first preference.
(>>|) :: MonadAsync m => AsyncT m a -> (a -> AsyncT m b) -> AsyncT m b
(>>|) = bindWith (<|)

-- | Execute a monadic action for each element in the stream, running nested
-- loops in parallel, but in a fairly interleaved manner i.e. both inner and
-- outer loops yield alternately.
(>|>) :: MonadAsync m => AsyncT m a -> (a -> AsyncT m b) -> AsyncT m b
(>|>) = bindWith (<|>)

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
    yield a _ Nothing  = sendit a >> run
    yield a c (Just r) = sendit a >> (runAsyncT r) c run yield

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
    yield a _ Nothing  = sendit a >> run
    -- XXX do we need to save the context as well?
    -- we can enqueue "(runAsyncT m) ctx run yield" instead
    yield a _ (Just r) = sendit a >> liftIO (enqueueFIFO q r) >> run

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

{-# INLINE sendWorkerWait #-}
sendWorkerWait :: MonadAsync m => Context m a -> m ()
sendWorkerWait ctx = dispatch >> void (liftIO $ takeMVar (doorBell ctx))

    where

    dispatch = do
        liftIO $ threadDelay 200
        output <- liftIO $ readIORef (outputQueue ctx)
        when (null output) $ do
            done <- queueEmpty ctx
            when (not done) $ (pushWorker ctx) >> dispatch


-- Note: This is performance sensitive code.
{-# NOINLINE pullWorker #-}
pullWorker :: MonadAsync m => Context m a -> AsyncT m a
pullWorker ctx = AsyncT $ \pctx stp yld -> do
    res <- liftIO $ tryTakeMVar (doorBell ctx)
    when (isNothing res) $ sendWorkerWait ctx
    list <- liftIO $ atomicModifyIORefCAS (outputQueue ctx) $ \x -> ([], x)
    (runAsyncT $ processEvents list) pctx stp yld

    where

    handleException e tid = do
        delThread ctx tid
        -- XXX implement kill async exception handling
        -- liftIO $ readIORef (runningThreads ctx) >>= mapM_ killThread
        throwM e

    {-# INLINE processEvents #-}
    processEvents [] = AsyncT $ \pctx stp yld -> do
        done <- allThreadsDone ctx
        if not done
        then (runAsyncT (pullWorker ctx)) pctx stp yld
        else stp

    processEvents (ev : es) = AsyncT $ \pctx stp yld -> do
        let continue = (runAsyncT (processEvents es)) pctx stp yld
            yield a  = yld a pctx (Just (processEvents es))

        case ev of
            ChildYield a -> yield a
            ChildStop tid e ->
                case e of
                    Nothing -> delThread ctx tid >> continue
                    Just ex -> handleException ex tid

-- | Split the original computation in a pull-push pair. The original
-- computation pulls from a Channel while m1 and m2 push to the channel.
{-# NOINLINE pullFork #-}
pullFork :: MonadAsync m => AsyncT m a -> AsyncT m a -> Bool -> AsyncT m a
pullFork m1 m2 fifo = AsyncT $ \_ stp yld -> do
    ctx <- liftIO $ newContext
    pushWorker ctx >> (runAsyncT (pullWorker ctx)) Nothing stp yld

    where

    newContext = do
        outQ    <- newIORef []
        outQMv  <- newEmptyMVar
        running <- newIORef S.empty

        case fifo of
            True -> do
                q <- newQ
                pushL q m1 >> pushL q m2
                let ctx =
                        Context { outputQueue    = outQ
                                , doorBell       = outQMv
                                , runningThreads = running
                                , runqueue       = runqueueFIFO ctx q
                                , enqueue        = pushL q
                                , queueEmpty     = liftIO $ nullQ q
                                }
                 in return ctx
            False -> do
                q <- newIORef []
                enqueueLIFO q m2 >> enqueueLIFO q m1
                let checkEmpty = liftIO (readIORef q) >>= return . null
                let ctx =
                        Context { outputQueue    = outQ
                                , doorBell       = outQMv
                                , runningThreads = running
                                , runqueue       = runqueueLIFO ctx q
                                , enqueue        = enqueueLIFO q
                                , queueEmpty     = checkEmpty
                                }
                 in return ctx

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
parallel :: MonadAsync m => AsyncT m a -> AsyncT m a -> Bool -> AsyncT m a
parallel m1 m2 fifo = AsyncT $ \ctx stp yld -> do
    case ctx of
        Nothing -> (runAsyncT (pullFork m1 m2 fifo)) Nothing stp yld
        Just  c -> liftIO ((enqueue c) m2) >> (runAsyncT m1) ctx stp yld

-- | `empty` represents an action that takes non-zero time to complete.  Since
-- all actions take non-zero time, an `Alternative` composition ('<|>') is a
-- monoidal composition executing all actions in parallel, it is similar to
-- '<>' except that it runs all the actions in parallel and interleaves their
-- results fairly.
instance MonadAsync m => Alternative (AsyncT m) where
    empty = mempty

    {-# INLINE (<|>) #-}
    m1 <|> m2 = parallel m1 m2 True

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
parLeft m1 m2 = parallel m1 m2 False

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
-- Running the monad
------------------------------------------------------------------------------

-- | Run an 'AsyncT' computation, wait for it to finish and discard the
-- results.
runAsyncly :: MonadAsync m => AsyncT m a -> m ()
runAsyncly m = run Nothing m

    where

    stop = return ()

    {-# INLINE yield #-}
    yield _ _ Nothing  = stop
    yield _ c (Just x) = run c x

    run ct mx = (runAsyncT mx) ct stop yield

-- | Collect the results of an 'AsyncT' stream into a list.
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
-- Transformation
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
take :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
take n m = AsyncT $ \ctx stp yld -> do
    let yield a c Nothing  = yld a c Nothing
        yield a c (Just x) = yld a c (Just (take (n - 1) x))
    if (n <= 0)
    then stp
    else (runAsyncT m) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
drop n m = AsyncT $ \ctx stp yld -> do
    let yield _ _ Nothing  = stp
        yield _ c (Just x) = (runAsyncT $ drop (n - 1) x) c stp yld
    if (n <= 0)
    then (runAsyncT m) ctx stp yld
    else (runAsyncT m) ctx stp yield

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
fromCallback k = AsyncT $ \ctx _ yld -> k (\a -> yld a ctx Nothing)
