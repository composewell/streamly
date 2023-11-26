-- |
-- Module      : Streamly.Internal.Data.Stream.Concurrent.Channel.Append
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The functions in this module are separated from the combinators using
-- these because of a GHC issue. We need to have newAppendChannel specialized but
-- not inlined. If we keep it in the same module as its users we cannot achieve
-- that and the code becomes bloated. But if it is in a separate module we can
-- use INLINABLE and SPECIALIZE on it which makes it specialized but it is not
-- actually inlined.

module Streamly.Internal.Data.Stream.Concurrent.Channel.Append
    (
      newAppendChannel
    )
where

import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (assert)
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Heap (Heap, Entry(..))
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef, writeIORef)
import Data.Kind (Type)
import GHC.Exts (inline)
import Streamly.Internal.Control.Concurrent
    (MonadRunInIO, RunInIO(..), askRunInIO, restoreM)
import Streamly.Internal.Data.Atomics
    (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import Streamly.Internal.Data.Channel.Dispatcher (modifyThread)

import qualified Data.Heap as H
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.StreamK as K

import Streamly.Internal.Data.Stream.Concurrent.Channel.Consumer
import Streamly.Internal.Data.Stream.Concurrent.Channel.Dispatcher
import Streamly.Internal.Data.Stream.Concurrent.Channel.Type
import Streamly.Internal.Data.Channel.Types
import Streamly.Internal.Data.Channel.Worker

------------------------------------------------------------------------------
-- Concurrent streams with first-come-first serve results
------------------------------------------------------------------------------

-- Note: For purely right associated expressions this queue should have at most
-- one element. It grows to more than one when we have left associcated
-- expressions. Large left associated compositions can grow this to a
-- large size
{-# INLINE enqueueLIFO #-}
enqueueLIFO ::
      Channel m a
   -> IORef ([(RunInIO m, K.StreamK m a)], [(RunInIO m, K.StreamK m a)])
   -> Bool
   -> (RunInIO m, K.StreamK m a)
   -> IO ()
enqueueLIFO sv q inner m = do
    atomicModifyIORefCAS_ q $ \(xs, ys) ->
        if inner then (xs, m : ys) else (m : xs, ys)
    ringDoorBell (doorBellOnWorkQ sv) (outputDoorBell sv)

data QResult a = QEmpty | QOuter a | QInner a

{-# INLINE dequeue #-}
dequeue :: MonadIO m =>
       IORef ([(RunInIO m, K.StreamK m a)], [(RunInIO m, K.StreamK m a)])
    -> m (QResult (RunInIO m, K.StreamK m a))
dequeue qref =
    liftIO
        $ atomicModifyIORefCAS qref
        $ \case
            (xs, y : ys) -> ((xs, ys), QInner y)
            (x : xs, ys) -> ((xs, ys), QOuter x)
            x -> (x, QEmpty)

data WorkerStatus = Continue | Suspend

{-# INLINE workLoopLIFO #-}
workLoopLIFO
    :: MonadRunInIO m
    => IORef ([(RunInIO m, K.StreamK m a)], [(RunInIO m, K.StreamK m a)])
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
workLoopLIFO qref sv winfo = run

    where

    run = do
        work <- dequeue qref
        case work of
            QEmpty ->
                liftIO $ stop sv winfo
            QInner (RunInIO runin, m) ->
                process runin m True
            QOuter (RunInIO runin, m) ->
                process runin m False

    process runin m inner = do
        -- XXX when we finish we need to send the monadic state back to
        -- the parent so that the state can be merged back. We capture
        -- and return the state in the stop continuation.
        --
        -- Instead of using the run function we can just restore the
        -- monad state here. That way it can work easily for
        -- distributed case as well.
        r <- liftIO $ runin $
                K.foldStreamShared
                    undefined
                    yieldk
                    single
                    (return Continue)
                    m
        res <- restoreM r
        case res of
            Continue -> run
            Suspend -> liftIO $ stop sv winfo

        where

        single a = do
            res <- liftIO $ yield sv winfo a
            return $ if res then Continue else Suspend

        yieldk a r = do
            res <- liftIO $ yield sv winfo a
            if res
            then K.foldStreamShared undefined yieldk single (return Continue) r
            else do
                runInIO <- askRunInIO
                liftIO $ enqueueLIFO sv qref inner (runInIO, r)
                return Suspend

-- We duplicate workLoop for yield limit and no limit cases because it has
-- around 40% performance overhead in the worst case.
--
-- XXX we can pass yinfo directly as an argument here so that we do not have to
-- make a check every time.
{-# INLINE workLoopLIFOLimited #-}
workLoopLIFOLimited
    :: forall m a. MonadRunInIO m
    => IORef ([(RunInIO m, K.StreamK m a)], [(RunInIO m, K.StreamK m a)])
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
workLoopLIFOLimited qref sv winfo = run

    where

    incrContinue =
        liftIO (incrementYieldLimit (remainingWork sv)) >> return Continue

    run = do
        work <- dequeue qref
        case work of
            QEmpty ->
                liftIO $ stop sv winfo
            QInner item ->
                process item True
            QOuter item ->
                process item False

    process item@(RunInIO runin, m) inner = do
        -- XXX This is just a best effort minimization of concurrency
        -- to the yield limit. If the stream is made of concurrent
        -- streams we do not reserve the yield limit in the constituent
        -- streams before executing the action. This can be done
        -- though, by sharing the yield limit ref with downstream
        -- actions via state passing. Just a todo.
        yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
        if yieldLimitOk
        then do
            r <- liftIO $ runin $
                    K.foldStreamShared
                        undefined
                        yieldk
                        single
                        incrContinue
                        m
            res <- restoreM r
            case res of
                Continue -> run
                Suspend -> liftIO $ stop sv winfo
        -- Avoid any side effects, undo the yield limit decrement if we
        -- never yielded anything.
        else liftIO $ do
            enqueueLIFO sv qref inner item
            incrementYieldLimit (remainingWork sv)
            stop sv winfo

        where

        single a = do
            res <- liftIO $ yield sv winfo a
            return $ if res then Continue else Suspend

        -- XXX can we pass on the yield limit downstream to limit the
        -- concurrency of constituent streams.
        yieldk a r = do
            res <- liftIO $ yield sv winfo a
            yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
            if res && yieldLimitOk
            then K.foldStreamShared undefined yieldk single incrContinue r
            else do
                runInIO <- askRunInIO
                liftIO $ incrementYieldLimit (remainingWork sv)
                liftIO $ enqueueLIFO sv qref inner (runInIO, r)
                return Suspend

-------------------------------------------------------------------------------
-- Ahead Channel Data Structures
-------------------------------------------------------------------------------

-- XXX Left associated ahead expressions are expensive. We start a new SVar for
-- each left associative expression. The queue is used only for right
-- associated expression, we queue the right expression and execute the left.
-- Therefore the queue never has more than one item in it. However, in case of
-- parIterateConcatMap the iteration may add more items at the end of the
-- queue.
--
-- XXX we can fix this. When we queue more than one item on the queue we can
-- mark the previously queued item as not-runnable. The not-runnable item is
-- not dequeued until the already running one has finished and at that time we
-- would also know the exact sequence number of the already queued item.
--
-- we can even run the already queued items but they will have to be sorted in
-- layers in the heap. We can use a list of heaps for that.
{-# ANN enqueueAhead "HLint: ignore" #-}
{-# INLINE enqueueAhead #-}
enqueueAhead ::
       Channel m a
    -> IORef ([K.StreamK m a], Int)
    -> (RunInIO m, K.StreamK m a)
    -> IO ()
enqueueAhead sv q m = do
    -- XXX The queue is LIFO. When parConcatIterate queues more than one items
    -- to the queue it will perform a DFS style traversal. For BFS we will have
    -- to use a FIFO data structure here. That would require another Config
    -- option.
    atomicModifyIORefCAS_ q $ \(xs, n) -> (snd m:xs, n)
    ringDoorBell (doorBellOnWorkQ sv) (outputDoorBell sv)

-- Normally the thread that has the token should never go away. The token gets
-- handed over to another thread, but someone or the other has the token at any
-- point of time. But if the task that has the token finds that the outputQueue
-- is full, in that case it can go away without even handing over the token to
-- another thread. In that case it sets the nextSequence number in the heap its
-- own sequence number before going away. To handle this case, any task that
-- does not have the token tries to dequeue from the heap first before
-- dequeuing from the work queue. If it finds that the task at the top of the
-- heap is the one that owns the current sequence number then it grabs the
-- token and starts with that.
--
-- XXX instead of queueing just the head element and the remaining computation
-- on the heap, evaluate as many as we can and place them on the heap. But we
-- need to give higher priority to the lower sequence numbers so that lower
-- priority tasks do not fill up the heap making higher priority tasks block
-- due to full heap. Maybe we can have a weighted space for them in the heap.
-- The weight is inversely proportional to the sequence number.
--
-- XXX review for livelock

{-# INLINE dequeueAhead #-}
dequeueAhead :: MonadIO m
    => IORef ([t m a], Int) -> m (Maybe (t m a, Int))
dequeueAhead q = liftIO $
    atomicModifyIORefCAS q $ \case
            ([], n) -> (([], n), Nothing)
            (x : xs, n) -> ((xs, n + 1), Just (x, n + 1))

-- Dequeue only if the seq number matches the expected seq number.
{-# INLINE dequeueAheadSeqCheck #-}
dequeueAheadSeqCheck :: MonadIO m
    => IORef ([t m a], Int) -> Int -> m (Maybe (t m a))
dequeueAheadSeqCheck q seqNo = liftIO $
    atomicModifyIORefCAS q $ \case
            ([], n) -> (([], n), Nothing)
            (x : xs, n) ->
                if n + 1 == seqNo
                then ((xs, n + 1), Just x)
                else ((x : xs, n), Nothing)

-------------------------------------------------------------------------------
-- Heap manipulation
-------------------------------------------------------------------------------

withIORef :: IORef a -> (a -> IO b) -> IO b
withIORef ref f = readIORef ref >>= f

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f =
    atomicModifyIORef ref $ \x -> (f x, ())

data AheadHeapEntry (t :: (Type -> Type) -> Type -> Type) m a =
      AheadEntryNull
    | AheadEntryPure a
    | AheadEntryStream (RunInIO m, t m a)

data HeapDequeueResult t m a =
      Clearing
    | Waiting Int
    | Ready (Entry Int (AheadHeapEntry t m a))

{-# INLINE dequeueFromHeap #-}
dequeueFromHeap
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> IO (HeapDequeueResult t m a)
dequeueFromHeap hpVar =
    atomicModifyIORef hpVar $ \pair@(hp, snum) ->
        case snum of
            Nothing -> (pair, Clearing)
            Just n -> do
                let r = H.uncons hp
                case r of
                    Just (ent@(Entry seqNo _ev), hp') ->
                            if seqNo == n
                            then ((hp', Nothing), Ready ent)
                            else assert (seqNo >= n) (pair, Waiting n)
                    Nothing -> (pair, Waiting n)

{-# INLINE dequeueFromHeapSeq #-}
dequeueFromHeapSeq
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> Int
    -> IO (HeapDequeueResult t m a)
dequeueFromHeapSeq hpVar i =
    atomicModifyIORef hpVar $ \(hp, snum) ->
        case snum of
            Nothing -> do
                let r = H.uncons hp
                case r of
                    Just (ent@(Entry seqNo _ev), hp') ->
                        if seqNo == i
                        then ((hp', Nothing), Ready ent)
                        else assert (seqNo >= i) ((hp, Just i), Waiting i)
                    Nothing -> ((hp, Just i), Waiting i)
            Just _ -> error "dequeueFromHeapSeq: unreachable"

heapIsSane :: Maybe Int -> Int -> Bool
heapIsSane snum seqNo =
    case snum of
        Nothing -> True
        Just n -> seqNo >= n

{-# INLINE requeueOnHeapTop #-}
requeueOnHeapTop
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> Entry Int (AheadHeapEntry t m a)
    -> Int
    -> IO ()
requeueOnHeapTop hpVar ent seqNo =
    atomicModifyIORef_ hpVar $ \(hp, snum) ->
        assert (heapIsSane snum seqNo) (H.insert ent hp, Just seqNo)

{-# INLINE updateHeapSeq #-}
updateHeapSeq
    :: IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
    -> Int
    -> IO ()
updateHeapSeq hpVar seqNo =
    atomicModifyIORef_ hpVar $ \(hp, snum) ->
        assert (heapIsSane snum seqNo) (hp, Just seqNo)

------------------------------------------------------------------------------
-- Ahead: Concurrent streams with ordered results
------------------------------------------------------------------------------

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
--
-- XXX Also note that limiting concurrency for cases like "take 10" would not
-- work well with left associative expressions, because we have no visibility
-- about how much the left side of the expression would yield.
--
-- XXX It may be a good idea to increment sequence numbers for each yield,
-- currently a stream on the left side of the expression may yield many
-- elements with the same sequene number. We can then use the seq number to
-- enforce yieldMax and yieldLImit as well.

-- Invariants:
--
-- * A worker should always ensure that it pushes all the consecutive items in
-- the heap to the outputQueue especially the items on behalf of the workers
-- that have already left when we were holding the token. This avoids deadlock
-- conditions when the later workers completion depends on the consumption of
-- earlier results. For more details see comments in the consumer pull side
-- code.

{-# INLINE underMaxHeap #-}
underMaxHeap ::
       Channel m a
    -> Heap (Entry Int (AheadHeapEntry K.StreamK m a))
    -> IO Bool
underMaxHeap sv hp = do
    (_, len) <- readIORef (outputQueue sv)

    -- XXX simplify this
    let maxHeap = case maxBufferLimit sv of
            Limited lim -> Limited $
                max 0 (lim - fromIntegral len)
            Unlimited -> Unlimited

    case maxHeap of
        Limited lim -> do
            active <- readIORef (workerCount sv)
            return $ H.size hp + active <= fromIntegral lim
        Unlimited -> return True

-- Return value:
-- True => stop
-- False => continue
preStopCheck ::
       Channel m a
    -> IORef (Heap (Entry Int (AheadHeapEntry K.StreamK m a)) , Maybe Int)
    -> IO Bool
preStopCheck sv heap =
    -- check the stop condition under a lock before actually
    -- stopping so that the whole herd does not stop at once.
    withIORef heap $ \(hp, _) -> do
        heapOk <- underMaxHeap sv hp
        takeMVar (workerStopMVar sv)
        let stopping = do
                putMVar (workerStopMVar sv) ()
                return True
            continue = do
                putMVar (workerStopMVar sv) ()
                return False
        if heapOk
        then
            case yieldRateInfo sv of
                Nothing -> continue
                Just yinfo -> do
                    rateOk <-
                        isBeyondMaxRate
                            (maxWorkerLimit sv) (workerCount sv) yinfo
                    if rateOk then continue else stopping
        else stopping

abortExecution :: Channel m a -> Maybe WorkerInfo -> IO ()
abortExecution sv winfo = do
    incrementYieldLimit (remainingWork sv)
    stop sv winfo

-- XXX In absence of a "noyield" primitive (i.e. do not pre-empt inside a
-- critical section) from GHC RTS, we have a difficult problem. Assume we have
-- a 100,000 threads producing output and queuing it to the heap for
-- sequencing. The heap can be drained only by one thread at a time, any thread
-- that finds that heap can be drained now, takes a lock and starts draining
-- it, however the thread may get prempted in the middle of it holding the
-- lock. Since that thread is holding the lock, the other threads cannot pick
-- up the draining task, therefore they proceed to picking up the next task to
-- execute. If the draining thread could yield voluntarily at a point where it
-- has released the lock, then the next threads could pick up the draining
-- instead of executing more tasks. When there are 100,000 threads the drainer
-- gets a cpu share to run only 1:100000 of the time. This makes the heap
-- accumulate a lot of output when we the buffer size is large.
--
-- The solutions to this problem are:
-- 1) make the other threads wait in a queue until the draining finishes
-- 2) make the other threads queue and go away if draining is in progress
--
-- In both cases we give the drainer a chance to run more often.
--
processHeap
    :: MonadRunInIO m
    => IORef ([K.StreamK m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry K.StreamK m a)), Maybe Int)
    -> Channel m a
    -> Maybe WorkerInfo
    -> AheadHeapEntry K.StreamK m a
    -> Int
    -> Bool -- we are draining the heap before we stop
    -> m ()
processHeap q heap sv winfo entry sno stopping = loopHeap sno entry

    where

    stopIfNeeded ent seqNo r = do
        stopIt <- liftIO $ preStopCheck sv heap
        if stopIt
        then liftIO $ do
            -- put the entry back in the heap and stop
            requeueOnHeapTop heap (Entry seqNo ent) seqNo
            stop sv winfo
        else runStreamWithYieldLimit True seqNo r

    loopHeap seqNo ent =
        case ent of
            AheadEntryNull -> nextHeap seqNo
            AheadEntryPure a -> do
                -- Use 'send' directly so that we do not account this in worker
                -- latency as this will not be the real latency.
                -- Don't stop the worker in this case as we are just
                -- transferring available results from heap to outputQueue.
                void
                    $ liftIO
                    $ sendWithDoorBell
                        (outputQueue sv) (outputDoorBell sv) (ChildYield a)
                nextHeap seqNo
            AheadEntryStream (RunInIO runin, r) -> do
                if stopping
                then stopIfNeeded ent seqNo r
                else do
                    res <- liftIO $ runin (runStreamWithYieldLimit True seqNo r)
                    restoreM res

    nextHeap prevSeqNo = do
        res <- liftIO $ dequeueFromHeapSeq heap (prevSeqNo + 1)
        case res of
            Ready (Entry seqNo hent) -> loopHeap seqNo hent
            Clearing -> liftIO $ stop sv winfo
            Waiting _ ->
                if stopping
                then do
                    r <- liftIO $ preStopCheck sv heap
                    if r
                    then liftIO $ stop sv winfo
                    else processWorkQueue prevSeqNo
                else inline processWorkQueue prevSeqNo

    processWorkQueue prevSeqNo = do
        yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
        if yieldLimitOk
        then do
            work <- dequeueAhead q
            case work of
                Nothing -> liftIO $ stop sv winfo
                Just (m, seqNo) -> do
                    if seqNo == prevSeqNo + 1
                    then processWithToken q heap sv winfo m seqNo
                    else processWithoutToken q heap sv winfo m seqNo
        else liftIO $ abortExecution sv winfo

    -- We do not stop the worker on buffer full here as we want to proceed to
    -- nextHeap anyway so that we can clear any subsequent entries. We stop
    -- only in yield continuation where we may have a remaining stream to be
    -- pushed on the heap.
    singleStreamFromHeap seqNo a = do
        void $ liftIO $ yield sv winfo a
        nextHeap seqNo

    -- XXX when we have an unfinished stream on the heap we cannot account all
    -- the yields of that stream until it finishes, so if we have picked up
    -- and executed more actions beyond that in the parent stream and put them
    -- on the heap then they would eat up some yield limit which is not
    -- correct, we will think that our yield limit is over even though we have
    -- to yield items from unfinished stream before them. For this reason, if
    -- there are pending items in the heap we drain them unconditionally
    -- without considering the yield limit.
    runStreamWithYieldLimit continue seqNo r = do
        _ <- liftIO $ decrementYieldLimit (remainingWork sv)
        if continue -- see comment above -- && yieldLimitOk
        then do
            let stopk = do
                  liftIO (incrementYieldLimit (remainingWork sv))
                  nextHeap seqNo
            K.foldStreamShared undefined
                          (yieldStreamFromHeap seqNo)
                          (singleStreamFromHeap seqNo)
                          stopk
                          r
        else do
            runIn <- askRunInIO
            let ent = Entry seqNo (AheadEntryStream (runIn, r))
            liftIO $ do
                requeueOnHeapTop heap ent seqNo
                incrementYieldLimit (remainingWork sv)
                stop sv winfo

    yieldStreamFromHeap seqNo a r = do
        continue <- liftIO $ yield sv winfo a
        runStreamWithYieldLimit continue seqNo r

{-# NOINLINE drainHeap #-}
drainHeap
    :: MonadRunInIO m
    => IORef ([K.StreamK m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry K.StreamK m a)), Maybe Int)
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
drainHeap q heap sv winfo = do
    r <- liftIO $ dequeueFromHeap heap
    case r of
        Ready (Entry seqNo hent) ->
            processHeap q heap sv winfo hent seqNo True
        _ -> liftIO $ stop sv winfo

data HeapStatus = HContinue | HStop

processWithoutToken
    :: MonadRunInIO m
    => IORef ([K.StreamK m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry K.StreamK m a)), Maybe Int)
    -> Channel m a
    -> Maybe WorkerInfo
    -> K.StreamK m a
    -> Int
    -> m ()
processWithoutToken q heap sv winfo m seqNo = do
    -- we have already decremented the yield limit for m
    let stopk = do
            liftIO (incrementYieldLimit (remainingWork sv))
            -- If the stream stops without yielding anything, and we do not put
            -- anything on heap, but if heap was waiting for this seq number
            -- then it will keep waiting forever, because we are never going to
            -- put it on heap. So we have to put a null entry on heap even when
            -- we stop.
            toHeap AheadEntryNull
        mrun = runInIO $ svarMrun sv

    r <- liftIO $ mrun $
            K.foldStreamShared undefined
                (\a r -> do
                    runIn <- askRunInIO
                    toHeap $ AheadEntryStream (runIn, K.cons a r))
                (toHeap . AheadEntryPure)
                stopk
                m
    res <- restoreM r
    case res of
        Continue -> workLoopAhead q heap sv winfo
        Suspend -> drainHeap q heap sv winfo

    where

    -- XXX to reduce contention each CPU can have its own heap
    toHeap ent = do
        -- Heap insertion is an expensive affair so we use a non CAS based
        -- modification, otherwise contention and retries can make a thread
        -- context switch and throw it behind other threads which come later in
        -- sequence.
        newHp <- liftIO $ atomicModifyIORef heap $ \(hp, snum) ->
            let hp' = H.insert (Entry seqNo ent) hp
            in assert (heapIsSane snum seqNo) ((hp', snum), hp')

        when (svarInspectMode sv) $
            liftIO $ do
                maxHp <- readIORef (maxHeapSize $ svarStats sv)
                when (H.size newHp > maxHp) $
                    writeIORef (maxHeapSize $ svarStats sv) (H.size newHp)

        heapOk <- liftIO $ underMaxHeap sv newHp
        status <-
            case yieldRateInfo sv of
                Nothing -> return HContinue
                Just yinfo ->
                    case winfo of
                        Just info -> do
                            rateOk <-
                                liftIO
                                    $ workerRateControl
                                        (maxWorkerLimit sv)
                                        (workerCount sv)
                                        yinfo
                                        info
                            if rateOk
                            then return HContinue
                            else return HStop
                        Nothing -> return HContinue

        if heapOk
        then
            case status of
                HContinue -> return Continue
                HStop -> return Suspend
        else return Suspend

data TokenWorkerStatus = TokenContinue Int | TokenSuspend

processWithToken
    :: MonadRunInIO m
    => IORef ([K.StreamK m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry K.StreamK m a)), Maybe Int)
    -> Channel m a
    -> Maybe WorkerInfo
    -> K.StreamK m a
    -> Int
    -> m ()
processWithToken q heap sv winfo action sno = do
    -- Note, we enter this function with yield limit already decremented
    -- XXX deduplicate stop in all invocations
    let stopk = do
            liftIO (incrementYieldLimit (remainingWork sv))
            return $ TokenContinue (sno + 1)
        mrun = runInIO $ svarMrun sv

    r <-
        liftIO
            $ mrun
            $ K.foldStreamShared
                undefined (yieldOutput sno) (singleOutput sno) stopk action

    res <- restoreM r
    case res of
        TokenContinue seqNo -> loopWithToken seqNo
        TokenSuspend -> drainHeap q heap sv winfo

    where

    singleOutput seqNo a = do
        continue <- liftIO $ yield sv winfo a
        if continue
        then return $ TokenContinue (seqNo + 1)
        else do
            liftIO $ updateHeapSeq heap (seqNo + 1)
            return TokenSuspend

    -- XXX use a wrapper function around stop so that we never miss
    -- incrementing the yield in a stop continuation. Essentiatlly all
    -- "unstream" calls in this function must increment yield limit on stop.
    yieldOutput seqNo a r = do
        continue <- liftIO $ yield sv winfo a
        yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
        if continue && yieldLimitOk
        then do
            let stopk = do
                    liftIO (incrementYieldLimit (remainingWork sv))
                    return $ TokenContinue (seqNo + 1)
            K.foldStreamShared undefined
                          (yieldOutput seqNo)
                          (singleOutput seqNo)
                          stopk
                          r
        else do
            runIn <- askRunInIO
            let ent = Entry seqNo (AheadEntryStream (runIn, r))
            liftIO $ requeueOnHeapTop heap ent seqNo
            liftIO $ incrementYieldLimit (remainingWork sv)
            return TokenSuspend

    loopWithToken nextSeqNo = do
        let preExit = liftIO $ do
                updateHeapSeq heap nextSeqNo
                incrementYieldLimit (remainingWork sv)
        yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
        -- To avoid a race when another thread puts something
        -- on the heap and goes away, the consumer will not get
        -- a doorBell and we will not clear the heap before
        -- executing the next action. If the consumer depends
        -- on the output that is stuck in the heap then this
        -- will result in a deadlock. So we always clear the
        -- heap before executing the next action.
        if yieldLimitOk
        then do
            -- XXX Instead of checking seqno inside dequeue we can dequeue
            -- unconditionally and if the seqNo is not the same as nextSeqNo
            -- then release the token and call processWithoutToken. Need
            -- to check the performance though.
            work <- dequeueAheadSeqCheck q nextSeqNo
            case work of
                Nothing -> preExit >> workLoopAhead q heap sv winfo
                Just m -> do
                    let stopk = do
                            liftIO (incrementYieldLimit (remainingWork sv))
                            return $ TokenContinue (nextSeqNo + 1)
                        mrun = runInIO $ svarMrun sv
                    r <- liftIO $ mrun $
                        K.foldStreamShared undefined
                                      (yieldOutput nextSeqNo)
                                      (singleOutput nextSeqNo)
                                      stopk
                                      m
                    res <- restoreM r
                    case res of
                        TokenContinue seqNo -> loopWithToken seqNo
                        TokenSuspend -> drainHeap q heap sv winfo
        else preExit >> drainHeap q heap sv winfo

-- XXX the yield limit changes increased the performance overhead by 30-40%.
-- Just like AsyncT we can use an implementation without yeidlimit and even
-- without pacing code to keep the performance higher in the unlimited and
-- unpaced case.
--
-- XXX The yieldLimit stuff is pretty invasive. We can instead do it by using
-- three hooks, a pre-execute hook, a yield hook and a stop hook. In fact these
-- hooks can be used for a more general implementation to even check predicates
-- and not just yield limit.

workLoopAhead
    :: MonadRunInIO m
    => IORef ([K.StreamK m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry K.StreamK m a)), Maybe Int)
    -> Channel m a
    -> Maybe WorkerInfo
    -> m ()
workLoopAhead q heap sv winfo = do
        r <- liftIO $ dequeueFromHeap heap
        case r of
            Ready (Entry seqNo hent) ->
                processHeap q heap sv winfo hent seqNo False
            Clearing -> liftIO $ stop sv winfo
            Waiting _ -> do
                -- Before we execute the next item from the work queue we check
                -- if we are beyond the yield limit. It is better to check the
                -- yield limit before we pick up the next item. Otherwise we
                -- may have already started more tasks even though we may have
                -- reached the yield limit.  We can avoid this by taking active
                -- workers into account, but that is not as reliable, because
                -- workers may go away without picking up work and yielding a
                -- value.
                --
                -- Rate control can be done either based on actual yields in
                -- the output queue or based on any yield either to the heap or
                -- to the output queue. In both cases we may have one issue or
                -- the other. We chose to do this based on actual yields to the
                -- output queue because it makes the code common to both async
                -- and ahead streams.
                --
                yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork sv)
                if yieldLimitOk
                then do
                    work <- dequeueAhead q
                    case work of
                        Nothing -> liftIO $ stop sv winfo
                        Just (m, seqNo) -> do
                            if seqNo == 0
                            then processWithToken q heap sv winfo m seqNo
                            else processWithoutToken q heap sv winfo m seqNo
                else liftIO $ abortExecution sv winfo

-------------------------------------------------------------------------------
-- SVar creation
-- This code belongs in SVar.hs but is kept here for perf reasons
-------------------------------------------------------------------------------

-- XXX we have this function in this file because passing runStreamLIFO as a
-- function argument to this function results in a perf degradation of more
-- than 10%.  Need to investigate what the root cause is.
-- Interestingly, the same thing does not make any difference for Ahead.
-- {-# INLINABLE getLifoSVar #-}
getLifoSVar :: forall m a. MonadRunInIO m =>
    RunInIO m -> Config -> IO (Channel m a)
getLifoSVar mrun cfg = do
    outQ    <- newIORef ([], 0)
    -- the second component of the tuple is "Nothing" when heap is being
    -- cleared, "Just n" when we are expecting sequence number n to arrive
    -- before we can start clearing the heap.
    outH    <- newIORef (H.empty, Just 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef Set.empty
    q       <- newIORef
                ( [] :: [(RunInIO m, K.StreamK m a)]
                , [] :: [(RunInIO m, K.StreamK m a)]
                )
    -- Sequence number is incremented whenever something is de-queued,
    -- therefore, first sequence number would be 0
    aheadQ <- newIORef ([], -1)
    stopMVar <- newMVar ()
    yl <-
        case getYieldLimit cfg of
            Nothing -> return Nothing
            Just x -> Just <$> newIORef x
    rateInfo <- newRateInfo cfg

    stats <- newSVarStats
    tid <- myThreadId

    -- We are reading it without lock, the result would be reliable only if no
    -- worker is pending.
    let isWorkFinished _ = do
            (xs, ys) <- readIORef q
            return (null xs && null ys)

    let isWorkFinishedLimited sv = do
            yieldsDone <-
                    case remainingWork sv of
                        Just ref -> do
                            n <- readIORef ref
                            return (n <= 0)
                        Nothing -> return False
            qEmpty <- isWorkFinished sv
            return $ qEmpty || yieldsDone

    let eagerEval = getEagerDispatch cfg
        inOrder = getOrdered cfg

    let getSVar :: Channel m a
            -> (Channel m a -> m [ChildEvent a])
            -> (Channel m a -> m Bool)
            -> (Channel m a -> IO Bool)
            -> (IORef ([(RunInIO m, K.StreamK m a)], [(RunInIO m, K.StreamK m a)])
                -> Channel m a
                -> Maybe WorkerInfo
                -> m())
            -> Channel m a
        getSVar sv readOutput postProc workDone wloop = Channel
            { outputQueue      = outQ
            , remainingWork    = yl
            , maxBufferLimit   = getMaxBuffer cfg
            , maxWorkerLimit   = min (getMaxThreads cfg) (getMaxBuffer cfg)
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running

            , workLoop =
                if inOrder
                then workLoopAhead aheadQ outH sv
                else wloop q sv
            , enqueue =
                \inner ->
                    if inOrder
                    then enqueueAhead sv aheadQ
                    else enqueueLIFO sv q inner
            , eagerDispatch = when eagerEval $ void $ dispatchWorker 0 sv
            , isWorkDone =
                if inOrder
                then isWorkDoneAhead sv aheadQ outH
                else workDone sv
            , isQueueDone =
                if inOrder
                then isQueueDoneAhead sv aheadQ
                else workDone sv

            , doorBellOnWorkQ  = wfw
            , svarMrun         = mrun
            , workerCount      = active
            -- XXX We can use delThread or modThread based on eager flag.
            , accountThread    = modifyThread running outQMv
            , workerStopMVar   = stopMVar
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode cfg
            , svarCreator      = tid
            , svarStats        = stats
            }

    let sv =
            case getStreamRate cfg of
                Nothing ->
                    case getYieldLimit cfg of
                        Nothing -> getSVar sv (readOutputQBounded eagerEval)
                                              postProcessBounded
                                              isWorkFinished
                                              workLoopLIFO
                        Just _  -> getSVar sv (readOutputQBounded eagerEval)
                                              postProcessBounded
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
                Just _  ->
                    case getYieldLimit cfg of
                        Nothing -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinished
                                              workLoopLIFO
                        Just _  -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
     in return sv

    where

    {-# INLINE isQueueDoneAhead #-}
    isQueueDoneAhead sv q = do
        queueDone <- checkEmpty q
        yieldsDone <-
                case remainingWork sv of
                    Just yref -> do
                        n <- readIORef yref
                        return (n <= 0)
                    Nothing -> return False
        -- XXX note that yieldsDone can only be authoritative only when there
        -- are no workers running. If there are active workers they can
        -- later increment the yield count and therefore change the result.
        return $ yieldsDone || queueDone

    {-# INLINE isWorkDoneAhead #-}
    isWorkDoneAhead sv q ref = do
        heapDone <- do
                (hp, _) <- readIORef ref
                return (H.size hp <= 0)
        queueDone <- isQueueDoneAhead sv q
        return $ heapDone && queueDone

    checkEmpty q = do
        (xs, _) <- readIORef q
        return $ null xs

-- | Create a new async style concurrent stream evaluation channel. The monad
-- state used to run the stream actions is taken from the call site of
-- newAppendChannel.
{-# INLINABLE newAppendChannel #-}
{-# SPECIALIZE newAppendChannel :: (Config -> Config) -> IO (Channel IO a) #-}
newAppendChannel :: MonadRunInIO m => (Config -> Config) -> m (Channel m a)
newAppendChannel modifier = do
    mrun <- askRunInIO
    liftIO $ getLifoSVar mrun (modifier defaultConfig)
