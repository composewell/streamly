{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.CPS.Concurrent
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.CPS.Concurrent
    (
    -- * Construction (monadic)
      consMAhead
    , consMAsync
    , consMWAsync
    , consMParallel

    -- * Semigroup Style Composition
    , ahead
    , async
    , wAsync
    , parallel

    -- * applications
    , applyWith
    , runWith

    -- * zip
    , zipAsyncWith

    -- * Concurrent Stream Vars (SVars)
    , fromStreamVar
    , mkParallel

    )
where

--
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Atomics (atomicModifyIORefCAS_)
import Data.Concurrent.Queue.MichaelScott
       (LinkedQueue, newQ, nullQ)
import Data.Functor (void)
import Data.Heap (Heap, Entry(..))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromJust)
import GHC.IO (IO(..))
import Prelude hiding (repeat, zipWith)

import qualified Data.Set as S
import qualified Data.Heap as H

import Streamly.SVar
import Streamly.CPS.Stream

-- MVar diagnostics has some overhead - around 5% on asyncly null benchmark, we
-- can keep it on in production to debug problems quickly if and when they
-- happen, but it may result in unexpected output when threads are left hanging
-- until they are GCed because the consumer went away.

#ifdef DIAGNOSTICS
import Control.Monad (when)
import Data.IORef (writeIORef)
#endif

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------

{-# INLINE runStreamLIFO #-}
runStreamLIFO :: MonadIO m
    => SVar Stream m a -> IORef [Stream m a] -> Stream m a -> m () -> m ()
runStreamLIFO sv q m stop = runStream m (Just sv) stop single yield
    where
    single a = do
        res <- liftIO $ send sv (ChildYield a)
        if res then stop else liftIO $ sendStop sv
    yield a r = do
        res <- liftIO $ send sv (ChildYield a)
        if res
        then (runStream r) (Just sv) stop single yield
        else liftIO $ enqueueLIFO sv q r >> sendStop sv

-------------------------------------------------------------------------------
-- WAsync
-------------------------------------------------------------------------------

{-# INLINE runStreamFIFO #-}
runStreamFIFO :: MonadIO m
    => SVar Stream m a -> LinkedQueue (Stream m a) -> Stream m a -> m () -> m ()
runStreamFIFO sv q m stop = runStream m (Just sv) stop single yield
    where
    single a = do
        res <- liftIO $ send sv (ChildYield a)
        if res then stop else liftIO $ sendStop sv
    yield a r = do
        res <- liftIO $ send sv (ChildYield a)
        liftIO (enqueueFIFO sv q r)
        if res then stop else liftIO $ sendStop sv

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

{-# NOINLINE runOne #-}
runOne :: MonadIO m => SVar Stream m a -> Stream m a -> m ()
runOne sv m = (runStream m) (Just sv) stop single yield

    where

    stop = liftIO $ sendStop sv
    sendit a = liftIO $ send sv (ChildYield a)
    single a = sendit a >> stop
    -- XXX there is no flow control in parallel case. We should perhaps use a
    -- queue and queue it back on that and exit the thread when the outputQueue
    -- overflows. Parallel is dangerous because it can accumulate unbounded
    -- output in the buffer.
    yield a r = void (sendit a) >> runOne sv r

-------------------------------------------------------------------------------
-- Ahead
-------------------------------------------------------------------------------

-- Lookahead streams can execute multiple tasks concurrently, ahead of time,
-- but always serve them in the same order as they appear in the stream. To
-- implement lookahead streams efficiently we assign a sequence number to each
-- task when the task is picked up for execution. When the task finishes, the
-- output is tagged with the same sequence number and we rearrange the outputs
-- in sequence based on that number.
--
-- To explain the mechanism imagine that the current task at the head of the
-- stream has a "token" to yield to the outputQueue. The ownership of the token
-- is determined by the current sequence number is maintained in outputHeap.
-- Sequence number is assigned when a task is queued. When a thread dequeues a
-- task it picks up the sequence number as well and when the output is ready it
-- uses the sequence number to queue the output to the outputQueue.
--
-- The thread with current sequence number sends the output directly to the
-- outputQueue. Other threads push the output to the outputHeap. When the task
-- being queued on the heap is a stream of many elements we evaluate only the
-- first element and keep the rest of the unevaluated computation in the heap.
-- When such a task gets the "token" for outputQueue it evaluates and directly
-- yields all the elements to the outputQueue without checking for the
-- "token".
--
-- Note that no two outputs in the heap can have the same sequence numbers and
-- therefore we do not need a stable heap. We have also separated the buffer
-- for the current task (outputQueue) and the pending tasks (outputHeap) so
-- that the pending tasks cannot interfere with the current task. Note that for
-- a single task just the outputQueue is enough and for the case of many
-- threads just a heap is good enough. However we balance between these two
-- cases, so that both are efficient.
--
-- For bigger streams it may make sense to have separate buffers for each
-- stream. However, for singleton streams this may become inefficient. However,
-- if we do not have separate buffers, then the streams that come later in
-- sequence may hog the buffer, hindering the streams that are ahead. For this
-- reason we have a single element buffer limitation for the streams being
-- executed in advance.
--
-- This scheme works pretty efficiently with less than 40% extra overhead
-- compared to the Async streams where we do not have any kind of sequencing of
-- the outputs. It is especially devised so that we are most efficient when we
-- have short tasks and need just a single thread. Also when a thread yields
-- many items it can hold lockfree access to the outputQueue and do it
-- efficiently.
--
-- XXX Maybe we can start the ahead threads at a lower cpu and IO priority so
-- that they do not hog the resources and hinder the progress of the threads in
-- front of them.

-- Left associated ahead expressions are expensive. We start a new SVar for
-- each left associative expression. The queue is used only for right
-- associated expression, we queue the right expression and execute the left.
-- Thererefore the queue never has more than on item in it.

workLoopAhead :: MonadIO m
    => SVar Stream m a
    -> IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Int)
    -> m ()
workLoopAhead sv q heap = runHeap

    where

    toHeap seqNo ent = do
        hp <- liftIO $ atomicModifyIORefCAS heap $ \(h, snum) ->
            ((H.insert (Entry seqNo ent) h, snum), h)
        if H.size hp <= maxHeap
        then runHeap
        else liftIO $ sendStop sv

    singleToHeap seqNo a = toHeap seqNo (AheadEntryPure a)
    yieldToHeap seqNo a r = toHeap seqNo (AheadEntryStream (a `cons` r))

    singleOutput seqNo a = do
        continue <- liftIO $ send sv (ChildYield a)
        if continue
        then runQueueToken seqNo
        else liftIO $ do
            atomicModifyIORefCAS_ heap $ \(h, _) -> (h, seqNo + 1)
            sendStop sv

    yieldOutput seqNo a r = do
        continue <- liftIO $ send sv (ChildYield a)
        if continue
        then (runStream r) (Just sv) (runQueueToken seqNo)
                                     (singleOutput seqNo)
                                     (yieldOutput seqNo)
        else liftIO $ do
            atomicModifyIORefCAS_ heap $ \(h, _) ->
                (H.insert (Entry seqNo (AheadEntryStream r)) h, seqNo)
            sendStop sv

    {-# INLINE runQueueToken #-}
    runQueueToken prevSeqNo = do
        work <- dequeueAhead q
        case work of
            Nothing -> do
                liftIO $ atomicModifyIORefCAS_ heap $ \(h, _) ->
                    (h, prevSeqNo + 1)
                runHeap
            Just (m, seqNo) -> do
                if seqNo == prevSeqNo + 1
                then
                    (runStream m) (Just sv) (runQueueToken seqNo)
                                            (singleOutput seqNo)
                                            (yieldOutput seqNo)
                else do
                    liftIO $ atomicModifyIORefCAS_ heap $ \(h, _) ->
                        (h, prevSeqNo + 1)
                    (runStream m) (Just sv) runHeap
                                            (singleToHeap seqNo)
                                            (yieldToHeap seqNo)
    runQueueNoToken = do
        work <- dequeueAhead q
        case work of
            Nothing -> runHeap
            Just (m, seqNo) -> do
                if seqNo == 0
                then
                    (runStream m) (Just sv) (runQueueToken seqNo)
                                            (singleOutput seqNo)
                                            (yieldOutput seqNo)
                else
                    (runStream m) (Just sv) runHeap
                                            (singleToHeap seqNo)
                                            (yieldToHeap seqNo)

    {-# NOINLINE runHeap #-}
    runHeap = do
#ifdef DIAGNOSTICS
        liftIO $ do
            maxHp <- readIORef (maxHeapSize sv)
            (hp, _) <- readIORef heap
            when (H.size hp > maxHp) $ writeIORef (maxHeapSize sv) (H.size hp)
#endif
        ent <- liftIO $ dequeueFromHeap heap
        case ent of
            Nothing -> do
                done <- queueEmptyAhead q
                if done
                then liftIO $ sendStop sv
                else runQueueNoToken
            Just (Entry seqNo hent) -> do
                case hent of
                    AheadEntryPure a -> singleOutput seqNo a
                    AheadEntryStream r ->
                        (runStream r) (Just sv) (runQueueToken seqNo)
                                                (singleOutput seqNo)
                                                (yieldOutput seqNo)

-------------------------------------------------------------------------------
-- WAhead
-------------------------------------------------------------------------------

-- XXX To be implemented. Use a linked queue like WAsync and put back the
-- remaining computation at the back of the queue instead of the heap, and
-- increment the sequence number.

-- | Pull a stream from an SVar.
{-# NOINLINE fromStreamVar #-}
fromStreamVar :: MonadAsync m => SVar Stream m a -> Stream m a
fromStreamVar sv = Stream $ \_ stp sng yld -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    runStream (processEvents $ reverse list) Nothing stp sng yld

    where

    allDone stp = do
#ifdef DIAGNOSTICS
#ifdef DIAGNOSTICS_VERBOSE
            svInfo <- liftIO $ dumpSVar sv
            liftIO $ hPutStrLn stderr $ "fromStreamVar done\n" ++ svInfo
#endif
#endif
            stp

    {-# INLINE processEvents #-}
    processEvents [] = Stream $ \_ stp sng yld -> do
        done <- postProcess sv
        if done
        then allDone stp
        else runStream (fromStreamVar sv) Nothing stp sng yld

    processEvents (ev : es) = Stream $ \_ stp sng yld -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> runStream rest Nothing stp sng yld
                    Just ex -> throwM ex

------------------------------------------------------------------------------
-- Running streams concurrently
------------------------------------------------------------------------------

-- Concurrency rate control.
--
-- Our objective is to create more threads on demand if the consumer is running
-- faster than us. As soon as we encounter a concurrent composition we create a
-- push pull pair of threads. We use an SVar for communication between the
-- consumer, pulling from the SVar and the producer who is pushing to the SVar.
-- The producer creates more threads if the SVar drains and becomes empty, that
-- is the consumer is running faster.
--
-- XXX Note 1: This mechanism can be problematic if the initial production
-- latency is high, we may end up creating too many threads. So we need some
-- way to monitor and use the latency as well. Having a limit on the dispatches
-- (programmer controlled) may also help.
--
-- TBD Note 2: We may want to run computations at the lower level of the
-- composition tree serially even when they are composed using a parallel
-- combinator. We can use 'serial' in place of 'async' and 'wSerial' in
-- place of 'wAsync'. If we find that an SVar immediately above a computation
-- gets drained empty we can switch to parallelizing the computation.  For that
-- we can use a state flag to fork the rest of the computation at any point of
-- time inside the Monad bind operation if the consumer is running at a faster
-- speed.
--
-- TBD Note 3: the binary operation ('parallel') composition allows us to
-- dispatch a chunkSize of only 1.  If we have to dispatch in arbitrary
-- chunksizes we will need to compose the parallel actions using a data
-- constructor (A Free container) instead so that we can divide it in chunks of
-- arbitrary size before dispatching. If the stream is composed of
-- hierarchically composed grains of different sizes then we can always switch
-- to a desired granularity depending on the consumer speed.
--
-- TBD Note 4: for pure work (when we are not in the IO monad) we can divide it
-- into just the number of CPUs.

-- | Join two computations on the currently running 'SVar' queue for concurrent
-- execution.  When we are using parallel composition, an SVar is passed around
-- as a state variable. We try to schedule a new parallel computation on the
-- SVar passed to us. The first time, when no SVar exists, a new SVar is
-- created.  Subsequently, 'joinStreamVarAsync' may get called when a computation
-- already scheduled on the SVar is further evaluated. For example, when (a
-- `parallel` b) is evaluated it calls a 'joinStreamVarAsync' to put 'a' and 'b' on
-- the current scheduler queue.
--
-- The 'SVarStyle' required by the current composition context is passed as one
-- of the parameters.  If the scheduling and composition style of the new
-- computation being scheduled is different than the style of the current SVar,
-- then we create a new SVar and schedule it on that.  The newly created SVar
-- joins as one of the computations on the current SVar queue.
--
-- Cases when we need to switch to a new SVar:
--
-- * (x `parallel` y) `parallel` (t `parallel` u) -- all of them get scheduled on the same SVar
-- * (x `parallel` y) `parallel` (t `async` u) -- @t@ and @u@ get scheduled on a new child SVar
--   because of the scheduling policy change.
-- * if we 'adapt' a stream of type 'async' to a stream of type
--   'Parallel', we create a new SVar at the transitioning bind.
-- * When the stream is switching from disjunctive composition to conjunctive
--   composition and vice-versa we create a new SVar to isolate the scheduling
--   of the two.

forkSVarAsync :: MonadAsync m => SVarStyle -> Stream m a -> Stream m a -> Stream m a
forkSVarAsync style m1 m2 = Stream $ \_ stp sng yld -> do
    sv <- case style of
        AsyncVar -> newAsyncVar (concurrently m1 m2)
        WAsyncVar -> newWAsyncVar (concurrently m1 m2)
        _ -> error "illegal svar type"
    runStream (fromStreamVar sv) Nothing stp sng yld
    where
    concurrently ma mb = Stream $ \svr stp sng yld -> do
        liftIO $ enqueue (fromJust svr) mb
        (runStream ma) svr stp sng yld

{-# INLINE joinStreamVarAsync #-}
joinStreamVarAsync :: MonadAsync m
    => SVarStyle -> Stream m a -> Stream m a -> Stream m a
joinStreamVarAsync style m1 m2 = Stream $ \svr stp sng yld ->
    case svr of
        Just sv | svarStyle sv == style ->
            liftIO (enqueue sv m2) >> (runStream m1) svr stp sng yld
        _ -> runStream (forkSVarAsync style m1 m2) Nothing stp sng yld

-- The only difference between forkSVarAsync and this is that we run the left
-- computation without a shared SVar.
forkSVarAhead :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
forkSVarAhead m1 m2 = Stream $ \_ stp sng yld -> do
        sv <- newAheadVar (concurrently m1 m2) workLoopAhead
        (runStream (fromStreamVar sv)) Nothing stp sng yld
    where
    concurrently ma mb = Stream $ \svr stp sng yld -> do
        liftIO $ enqueue (fromJust svr) mb
        (runStream ma) Nothing stp sng yld

{-# INLINE ahead #-}
ahead :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
ahead m1 m2 = Stream $ \svr stp sng yld -> do
    case svr of
        Just sv | svarStyle sv == AheadVar -> do
            liftIO $ enqueue sv m2
            -- Always run the left side on a new SVar to avoid complexity in
            -- sequencing results. This means the left side cannot further
            -- split into more ahead computations on the same SVar.
            (runStream m1) Nothing stp sng yld
        _ -> runStream (forkSVarAhead m1 m2) Nothing stp sng yld

{-# NOINLINE forkSVarPar #-}
forkSVarPar :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
forkSVarPar m r = Stream $ \_ stp sng yld -> do
    sv <- newParallelVar
    pushWorkerPar sv (runOne sv m)
    pushWorkerPar sv (runOne sv r)
    (runStream (fromStreamVar sv)) Nothing stp sng yld

{-# INLINE joinStreamVarPar #-}
joinStreamVarPar :: MonadAsync m
    => SVarStyle -> Stream m a -> Stream m a -> Stream m a
joinStreamVarPar style m1 m2 = Stream $ \svr stp sng yld ->
    case svr of
        Just sv | svarStyle sv == style -> do
            pushWorkerPar sv (runOne sv m1) >> (runStream m2) svr stp sng yld
        _ -> runStream (forkSVarPar m1 m2) Nothing stp sng yld

------------------------------------------------------------------------------
-- Semigroup and Monoid style compositions for parallel actions
------------------------------------------------------------------------------

{-# INLINE async #-}
async :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
async = joinStreamVarAsync AsyncVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using async.
{-# INLINE consMAsync #-}
consMAsync :: MonadAsync m => m a -> Stream m a -> Stream m a
consMAsync m r = once m `async` r

{-# INLINE wAsync #-}
wAsync :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
wAsync = joinStreamVarAsync WAsyncVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using wAsync.
{-# INLINE consMWAsync #-}
consMWAsync :: MonadAsync m => m a -> Stream m a -> Stream m a
consMWAsync m r = once m `wAsync` r

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using ahead.
{-# INLINE consMAhead #-}
consMAhead :: MonadAsync m => m a -> Stream m a -> Stream m a
consMAhead m r = once m `ahead` r

{-# INLINE parallel #-}
parallel :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallel = joinStreamVarPar ParallelVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using parallel.
{-# INLINE consMParallel #-}
consMParallel :: MonadAsync m => m a -> Stream m a -> Stream m a
consMParallel m r = once m `parallel` r

------------------------------------------------------------------------------
-- Stream to stream function application
------------------------------------------------------------------------------

mkParallel :: MonadAsync m => Stream m a -> m (Stream m a)
mkParallel m = do
    sv <- newParallelVar
    pushWorkerPar sv (runOne sv m)
    return $ fromStreamVar sv

applyWith :: MonadAsync m
    => (Stream m a -> Stream m b) -> Stream m a -> Stream m b
applyWith f m = Stream $ \svr stp sng yld -> do
    sv <- newParallelVar
    pushWorkerPar sv (runOne sv m)
    runStream (f $ fromStreamVar sv) svr stp sng yld

------------------------------------------------------------------------------
-- Stream runner function application
------------------------------------------------------------------------------

runWith :: MonadAsync m => (Stream m a -> m b) -> Stream m a -> m b
runWith f m = do
    sv <- newParallelVar
    pushWorkerPar sv (runOne sv m)
    f $ fromStreamVar sv

{-# INLINE zipAsyncWith #-}
zipAsyncWith :: MonadAsync m
    => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipAsyncWith f m1 m2 = Stream $ \_ stp sng yld -> do
    ma <- mkAsync m1
    mb <- mkAsync m2
    (runStream (zipWith f ma mb)) Nothing stp sng yld

    where

    mkAsync :: MonadAsync m => Stream m a -> m (Stream m a)
    mkAsync m = newAsyncVar m
        >>= return . fromStreamVar

-------------------------------------------------------------------------------
-- SVar creation
-- This code belongs in SVar.hs but is kept here for perf reasons
-------------------------------------------------------------------------------

-- XXX we have this function in this file because passing runStreamLIFO as a
-- function argument to this function results in a perf degradation of more
-- than 10%.  Need to investigate what the root cause is.
-- Interestingly, the same thing does not make any difference for Ahead.
getLifoSVar :: MonadAsync m => IO (SVar Stream m a)
getLifoSVar = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q <- newIORef []
#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif
    let checkEmpty = null <$> readIORef q
    let sv =
            SVar { outputQueue      = outQ
                 , outputDoorBell   = outQMv
                 , readOutputQ      = readOutputQBounded sv
                 , postProcess      = postProcessBounded sv
                 , workerThreads    = running
                 , workLoop         = workLoopLIFO runStreamLIFO sv q
                 , enqueue          = enqueueLIFO sv q
                 , isWorkDone       = checkEmpty
                 , needDoorBell     = wfw
                 , svarStyle        = AsyncVar
                 , workerCount      = active
                 , accountThread    = delThread sv
#ifdef DIAGNOSTICS
                 , aheadWorkQueue   = undefined
                 , outputHeap       = undefined
                 , maxWorkers       = maxWrk
                 , totalDispatches  = disp
                 , maxOutQSize      = maxOq
                 , maxHeapSize      = maxHs
                 , maxWorkQSize     = maxWq
#endif
                 }
     in return sv

getFifoSVar :: MonadAsync m => IO (SVar Stream m a)
getFifoSVar = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q       <- newQ
#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif
    let sv =
           SVar { outputQueue      = outQ
                , outputDoorBell   = outQMv
                , readOutputQ      = readOutputQBounded sv
                , postProcess      = postProcessBounded sv
                , workerThreads    = running
                , workLoop         = workLoopFIFO runStreamFIFO sv q
                , enqueue          = enqueueFIFO sv q
                , isWorkDone       = nullQ q
                , needDoorBell     = wfw
                , svarStyle        = WAsyncVar
                , workerCount      = active
                , accountThread    = delThread sv
#ifdef DIAGNOSTICS
                , aheadWorkQueue   = undefined
                , outputHeap       = undefined
                , totalDispatches  = disp
                , maxWorkers       = maxWrk
                , maxOutQSize      = maxOq
                , maxHeapSize      = maxHs
                , maxWorkQSize     = maxWq
#endif
                 }
     in return sv

{-# INLINABLE newAsyncVar #-}
newAsyncVar :: MonadAsync m => Stream m a -> m (SVar Stream m a)
newAsyncVar m = do
    sv <- liftIO getLifoSVar
    sendWorker sv m

-- | Create a new SVar and enqueue one stream computation on it.
{-# INLINABLE newWAsyncVar #-}
newWAsyncVar :: MonadAsync m => Stream m a -> m (SVar Stream m a)
newWAsyncVar m = do
    sv <- liftIO getFifoSVar
    sendWorker sv m
