-- |
-- Module      : Streamly.Internal.Data.SVar
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.SVar
    {-# DEPRECATED "SVar is replaced by Channel." #-}
    (
      module Streamly.Internal.Data.SVar.Type
    , module Streamly.Internal.Data.SVar.Worker
    , module Streamly.Internal.Data.SVar.Dispatch
    , module Streamly.Internal.Data.SVar.Pull

    -- * New SVar
    , getYieldRateInfo
    , newSVarStats

    -- ** Parallel
    , newParallelVar

    -- ** Ahead
    , enqueueAhead
    , reEnqueueAhead
    , queueEmptyAhead
    , dequeueAhead
    , HeapDequeueResult(..)
    , dequeueFromHeap
    , dequeueFromHeapSeq
    , requeueOnHeapTop
    , updateHeapSeq
    , withIORef
    , heapIsSane
    , newAheadVar
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId, takeMVar)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, tryTakeMVar, newMVar)
import Control.Exception (assert)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Heap (Heap, Entry(..))
import Data.IORef (newIORef, readIORef)
import Data.IORef (IORef, atomicModifyIORef)
import Streamly.Internal.Control.Concurrent (MonadAsync, askRunInIO)
import Streamly.Internal.Data.Atomics
       (atomicModifyIORefCAS, atomicModifyIORefCAS_, writeBarrier)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Time.Units (NanoSecond64(..))

import qualified Data.Heap as H
import qualified Data.Set as S

import Streamly.Internal.Data.SVar.Dispatch
import Streamly.Internal.Data.SVar.Pull
import Streamly.Internal.Data.SVar.Type
import Streamly.Internal.Data.SVar.Worker

-------------------------------------------------------------------------------
-- Creating an SVar
-------------------------------------------------------------------------------

getYieldRateInfo :: State t m a -> IO (Maybe YieldRateInfo)
getYieldRateInfo st = do
    -- convert rate in Hertz to latency in Nanoseconds
    let rateToLatency r = if r <= 0 then maxBound else round $ 1.0e9 / r
    case getStreamRate st of
        Just (Rate low goal high buf) ->
            let l    = rateToLatency goal
                minl = rateToLatency high
                maxl = rateToLatency low
            in mkYieldRateInfo l (LatencyRange minl maxl) buf
        Nothing -> return Nothing

    where

    mkYieldRateInfo latency latRange buf = do
        measured <- newIORef 0
        wcur     <- newIORef (0,0,0)
        wcol     <- newIORef (0,0,0)
        now      <- getTime Monotonic
        wlong    <- newIORef (0,now)
        period   <- newIORef 1
        gainLoss <- newIORef (Count 0)

        return $ Just YieldRateInfo
            { svarLatencyTarget      = latency
            , svarLatencyRange       = latRange
            , svarRateBuffer         = buf
            , svarGainedLostYields   = gainLoss
            , workerBootstrapLatency = getStreamLatency st
            , workerPollingInterval  = period
            , workerMeasuredLatency  = measured
            , workerPendingLatency   = wcur
            , workerCollectedLatency = wcol
            , svarAllTimeLatency     = wlong
            }

newSVarStats :: IO SVarStats
newSVarStats = do
    disp   <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
    avgLat <- newIORef (0, NanoSecond64 0)
    maxLat <- newIORef (NanoSecond64 0)
    minLat <- newIORef (NanoSecond64 0)
    stpTime <- newIORef Nothing

    return SVarStats
        { totalDispatches  = disp
        , maxWorkers       = maxWrk
        , maxOutQSize      = maxOq
        , maxHeapSize      = maxHs
        , maxWorkQSize     = maxWq
        , avgWorkerLatency = avgLat
        , minWorkerLatency = minLat
        , maxWorkerLatency = maxLat
        , svarStopTime     = stpTime
        }

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

-- XXX Left associated ahead expressions are expensive. We start a new SVar for
-- each left associative expression. The queue is used only for right
-- associated expression, we queue the right expression and execute the left.
-- Thererefore the queue never has more than one item in it.
--
-- XXX we can fix this. When we queue more than one item on the queue we can
-- mark the previously queued item as not-runnable. The not-runnable item is
-- not dequeued until the already running one has finished and at that time we
-- would also know the exact sequence number of the already queued item.
--
-- we can even run the already queued items but they will have to be sorted in
-- layers in the heap. We can use a list of heaps for that.
{-# INLINE enqueueAhead #-}
enqueueAhead :: SVar t m a -> IORef ([t m a], Int) -> (RunInIO m, t m a) -> IO ()
enqueueAhead sv q m = do
    atomicModifyIORefCAS_ q $ \ case
        ([], n) -> ([snd m], n + 1)  -- increment sequence
        _ -> error "enqueueAhead: queue is not empty"
    ringDoorBell sv

-- enqueue without incrementing the sequence number
{-# INLINE reEnqueueAhead #-}
reEnqueueAhead :: SVar t m a -> IORef ([t m a], Int) -> t m a -> IO ()
reEnqueueAhead sv q m = do
    atomicModifyIORefCAS_ q $ \ case
        ([], n) -> ([m], n)  -- DO NOT increment sequence
        _ -> error "reEnqueueAhead: queue is not empty"
    ringDoorBell sv

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
--
{-# INLINE queueEmptyAhead #-}
queueEmptyAhead :: MonadIO m => IORef ([t m a], Int) -> m Bool
queueEmptyAhead q = liftIO $ do
    (xs, _) <- readIORef q
    return $ null xs

{-# INLINE dequeueAhead #-}
dequeueAhead :: MonadIO m
    => IORef ([t m a], Int) -> m (Maybe (t m a, Int))
dequeueAhead q = liftIO $
    atomicModifyIORefCAS q $ \case
            ([], n) -> (([], n), Nothing)
            (x : [], n) -> (([], n), Just (x, n))
            _ -> error "more than one item on queue"

-------------------------------------------------------------------------------
-- Heap manipulation
-------------------------------------------------------------------------------

withIORef :: IORef a -> (a -> IO b) -> IO b
withIORef ref f = readIORef ref >>= f

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f =
    atomicModifyIORef ref $ \x -> (f x, ())

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

-- XXX remove polymorphism in t, inline f
getAheadSVar :: MonadAsync m
    => State t m a
    -> (   IORef ([t m a], Int)
        -> IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
        -> State t m a
        -> SVar t m a
        -> Maybe WorkerInfo
        -> m ())
    -> RunInIO m
    -> IO (SVar t m a)
getAheadSVar st f mrun = do
    outQ    <- newIORef ([], 0)
    -- the second component of the tuple is "Nothing" when heap is being
    -- cleared, "Just n" when we are expecting sequence number n to arrive
    -- before we can start clearing the heap.
    outH    <- newIORef (H.empty, Just 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    -- Sequence number is incremented whenever something is queued, therefore,
    -- first sequence number would be 0
    q <- newIORef ([], -1)
    stopMVar <- newMVar ()
    yl <- case getYieldLimit st of
            Nothing -> return Nothing
            Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st

    stats <- newSVarStats
    tid <- myThreadId

    let getSVar sv readOutput postProc = SVar
            { outputQueue      = outQ
            , outputQueueFromConsumer = undefined
            , remainingWork  = yl
            , maxBufferLimit   = getMaxBuffer st
            , pushBufferSpace = undefined
            , pushBufferPolicy = undefined
            , pushBufferMVar   = undefined
            , maxWorkerLimit   = min (getMaxThreads st) (getMaxBuffer st)
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , outputDoorBellFromConsumer = undefined
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = f q outH st{streamVar = Just sv} sv
            , enqueue          = enqueueAhead sv q
            , isWorkDone       = isWorkDoneAhead sv q outH
            , isQueueDone      = isQueueDoneAhead sv q
            , needDoorBell     = wfw
            , svarStyle        = AheadVar
            , svarStopStyle    = StopNone
            , svarStopBy       = undefined
            , svarMrun         = mrun
            , workerCount      = active
            , accountThread    = delThread sv
            , workerStopMVar   = stopMVar
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode st
            , svarCreator      = tid
            , aheadWorkQueue   = q
            , outputHeap       = outH
            , svarStats        = stats
            }

    let sv =
            case getStreamRate st of
                Nothing -> getSVar sv readOutputQBounded postProcessBounded
                Just _  -> getSVar sv readOutputQPaced postProcessPaced
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

{-# INLINABLE newAheadVar #-}
newAheadVar :: MonadAsync m
    => State t m a
    -> t m a
    -> (   IORef ([t m a], Int)
        -> IORef (Heap (Entry Int (AheadHeapEntry t m a)), Maybe Int)
        -> State t m a
        -> SVar t m a
        -> Maybe WorkerInfo
        -> m ())
    -> m (SVar t m a)
newAheadVar st m wloop = do
    mrun <- askRunInIO
    sv <- liftIO $ getAheadSVar st wloop mrun
    sendFirstWorker sv m

-------------------------------------------------------------------------------
-- WAhead
-------------------------------------------------------------------------------

-- XXX To be implemented. Use a linked queue like WAsync and put back the
-- remaining computation at the back of the queue instead of the heap, and
-- increment the sequence number.

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

getParallelSVar :: MonadIO m
    => SVarStopStyle -> State t m a -> RunInIO m -> IO (SVar t m a)
getParallelSVar ss st mrun = do
    outQ    <- newIORef ([], 0)
    outQRev <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    outQMvRev <- newEmptyMVar
    active  <- newIORef 0
    running <- newIORef S.empty
    yl <- case getYieldLimit st of
            Nothing -> return Nothing
            Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st
    let bufLim =
            case getMaxBuffer st of
                Unlimited -> undefined
                Limited x -> fromIntegral x
    remBuf <- newIORef bufLim
    pbMVar <- newMVar ()

    stats <- newSVarStats
    tid <- myThreadId

    stopBy <-
        case ss of
            StopBy -> liftIO $ newIORef undefined
            _ -> return undefined

    let sv =
            SVar { outputQueue      = outQ
                 , outputQueueFromConsumer = outQRev
                 , remainingWork    = yl
                 , maxBufferLimit   = getMaxBuffer st
                 , pushBufferSpace  = remBuf
                 , pushBufferPolicy = PushBufferBlock
                 , pushBufferMVar   = pbMVar
                 , maxWorkerLimit   = Unlimited
                 -- Used only for diagnostics
                 , yieldRateInfo    = rateInfo
                 , outputDoorBell   = outQMv
                 , outputDoorBellFromConsumer = outQMvRev
                 , readOutputQ      = readOutputQPar sv
                 , postProcess      = allThreadsDone sv
                 , workerThreads    = running
                 , workLoop         = undefined
                 , enqueue          = undefined
                 , isWorkDone       = undefined
                 , isQueueDone      = undefined
                 , needDoorBell     = undefined
                 , svarStyle        = ParallelVar
                 , svarStopStyle    = ss
                 , svarStopBy       = stopBy
                 , svarMrun         = mrun
                 , workerCount      = active
                 , accountThread    = modifyThread sv
                 , workerStopMVar   = undefined
                 , svarRef          = Nothing
                 , svarInspectMode  = getInspectMode st
                 , svarCreator      = tid
                 , aheadWorkQueue   = undefined
                 , outputHeap       = undefined
                 , svarStats        = stats
                 }
     in return sv

    where

    readOutputQPar sv = liftIO $ do
        withDiagMVar sv "readOutputQPar: doorbell"
            $ takeMVar (outputDoorBell sv)
        case yieldRateInfo sv of
            Nothing -> return ()
            Just yinfo -> void $ collectLatency sv yinfo False
        r <- fst `fmap` readOutputQRaw sv
        liftIO $ do
            void $ tryTakeMVar (pushBufferMVar sv)
            resetBufferLimit sv
            writeBarrier
            void $ tryPutMVar (pushBufferMVar sv) ()
        return r

{-# INLINABLE newParallelVar #-}
newParallelVar :: MonadAsync m
    => SVarStopStyle -> State t m a -> m (SVar t m a)
newParallelVar ss st = do
    mrun <- askRunInIO
    liftIO $ getParallelSVar ss st mrun
