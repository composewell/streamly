{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.Ahead
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.Ahead
    (
      AheadT
    , Ahead
    , aheadly
    , ahead
    )
where

import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Exception (assert)
import Control.Monad (ap, void, when)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Heap (Heap, Entry(..))
import Data.IORef (IORef, readIORef, atomicModifyIORef, writeIORef)
import Data.Maybe (fromJust)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (inline)

import qualified Data.Heap as H

import Streamly.Streams.SVar (fromSVar)
import Streamly.Streams.Serial (map)
import Streamly.Internal.Data.SVar
import Streamly.Streams.StreamK
       (IsStream(..), Stream, mkStream, foldStream, foldStreamShared,
        foldStreamSVar)
import qualified Streamly.Streams.StreamK as K

import Prelude hiding (map)

#include "Instances.hs"

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
       SVar Stream m a
    -> Heap (Entry Int (AheadHeapEntry Stream m a))
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
       SVar Stream m a
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Maybe Int)
    -> IO Bool
preStopCheck sv heap =
    -- check the stop condition under a lock before actually
    -- stopping so that the whole herd does not stop at once.
    withIORef heap $ \(hp, _) -> do
        heapOk <- underMaxHeap sv hp
        takeMVar (workerStopMVar sv)
        let stop = do
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
                    rateOk <- isBeyondMaxRate sv yinfo
                    if rateOk then continue else stop
        else stop

abortExecution ::
       IORef ([Stream m a], Int)
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> Stream m a
    -> IO ()
abortExecution q sv winfo m = do
    reEnqueueAhead sv q m
    incrementYieldLimit sv
    sendStop sv winfo

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
processHeap :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)), Maybe Int)
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> AheadHeapEntry Stream m a
    -> Int
    -> Bool -- we are draining the heap before we stop
    -> m ()
processHeap q heap st sv winfo entry sno stopping = loopHeap sno entry

    where

    stopIfNeeded ent seqNo r = do
        stopIt <- liftIO $ preStopCheck sv heap
        if stopIt
        then liftIO $ do
            -- put the entry back in the heap and stop
            requeueOnHeapTop heap (Entry seqNo ent) seqNo
            sendStop sv winfo
        else runStreamWithYieldLimit True seqNo r

    loopHeap seqNo ent =
        case ent of
            AheadEntryNull -> nextHeap seqNo
            AheadEntryPure a -> do
                -- Use 'send' directly so that we do not account this in worker
                -- latency as this will not be the real latency.
                -- Don't stop the worker in this case as we are just
                -- transferring available results from heap to outputQueue.
                void $ liftIO $ send sv (ChildYield a)
                nextHeap seqNo
            AheadEntryStream r ->
                if stopping
                then stopIfNeeded ent seqNo r
                else runStreamWithYieldLimit True seqNo r

    nextHeap prevSeqNo = do
        res <- liftIO $ dequeueFromHeapSeq heap (prevSeqNo + 1)
        case res of
            Ready (Entry seqNo hent) -> loopHeap seqNo hent
            Clearing -> liftIO $ sendStop sv winfo
            Waiting _ ->
                if stopping
                then do
                    r <- liftIO $ preStopCheck sv heap
                    if r
                    then liftIO $ sendStop sv winfo
                    else processWorkQueue prevSeqNo
                else inline processWorkQueue prevSeqNo

    processWorkQueue prevSeqNo = do
        work <- dequeueAhead q
        case work of
            Nothing -> liftIO $ sendStop sv winfo
            Just (m, seqNo) -> do
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                if yieldLimitOk
                then
                    if seqNo == prevSeqNo + 1
                    then processWithToken q heap st sv winfo m seqNo
                    else processWithoutToken q heap st sv winfo m seqNo
                else liftIO $ abortExecution q sv winfo m

    -- We do not stop the worker on buffer full here as we want to proceed to
    -- nextHeap anyway so that we can clear any subsequent entries. We stop
    -- only in yield continuation where we may have a remaining stream to be
    -- pushed on the heap.
    singleStreamFromHeap seqNo a = do
        void $ liftIO $ sendYield sv winfo (ChildYield a)
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
        _ <- liftIO $ decrementYieldLimit sv
        if continue -- see comment above -- && yieldLimitOk
        then do
            let stop = do
                  liftIO (incrementYieldLimit sv)
                  nextHeap seqNo
            foldStreamSVar sv st
                          (yieldStreamFromHeap seqNo)
                          (singleStreamFromHeap seqNo)
                          stop
                          r
        else liftIO $ do
            let ent = Entry seqNo (AheadEntryStream r)
            liftIO $ requeueOnHeapTop heap ent seqNo
            incrementYieldLimit sv
            sendStop sv winfo

    yieldStreamFromHeap seqNo a r = do
        continue <- liftIO $ sendYield sv winfo (ChildYield a)
        runStreamWithYieldLimit continue seqNo r

{-# NOINLINE drainHeap #-}
drainHeap :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)), Maybe Int)
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> m ()
drainHeap q heap st sv winfo = do
    r <- liftIO $ dequeueFromHeap heap
    case r of
        Ready (Entry seqNo hent) ->
            processHeap q heap st sv winfo hent seqNo True
        _ -> liftIO $ sendStop sv winfo

data HeapStatus = HContinue | HStop

processWithoutToken :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)), Maybe Int)
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> Stream m a
    -> Int
    -> m ()
processWithoutToken q heap st sv winfo m seqNo = do
    -- we have already decremented the yield limit for m
    let stop = do
            liftIO (incrementYieldLimit sv)
            -- If the stream stops without yielding anything, and we do not put
            -- anything on heap, but if heap was waiting for this seq number
            -- then it will keep waiting forever, because we are never going to
            -- put it on heap. So we have to put a null entry on heap even when
            -- we stop.
            toHeap AheadEntryNull

    foldStreamSVar sv st
        (\a r -> toHeap $ AheadEntryStream $ K.cons a r)
        (toHeap . AheadEntryPure)
        stop
        m

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
        let drainAndStop = drainHeap q heap st sv winfo
            mainLoop = workLoopAhead q heap st sv winfo
        status <-
            case yieldRateInfo sv of
                Nothing -> return HContinue
                Just yinfo ->
                    case winfo of
                        Just info -> do
                            rateOk <- liftIO $ workerRateControl sv yinfo info
                            if rateOk
                            then return HContinue
                            else return HStop
                        Nothing -> return HContinue

        if heapOk
        then
            case status of
                HContinue -> mainLoop
                HStop -> drainAndStop
        else drainAndStop

processWithToken :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)), Maybe Int)
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> Stream m a
    -> Int
    -> m ()
processWithToken q heap st sv winfo action sno = do
    -- Note, we enter this function with yield limit already decremented
    -- XXX deduplicate stop in all invocations
    let stop = do
            liftIO (incrementYieldLimit sv)
            loopWithToken (sno + 1)

    foldStreamSVar sv st (yieldOutput sno) (singleOutput sno) stop action

    where

    singleOutput seqNo a = do
        continue <- liftIO $ sendYield sv winfo (ChildYield a)
        if continue
        then loopWithToken (seqNo + 1)
        else do
            liftIO $ updateHeapSeq heap (seqNo + 1)
            drainHeap q heap st sv winfo

    -- XXX use a wrapper function around stop so that we never miss
    -- incrementing the yield in a stop continuation. Essentiatlly all
    -- "unstream" calls in this function must increment yield limit on stop.
    yieldOutput seqNo a r = do
        continue <- liftIO $ sendYield sv winfo (ChildYield a)
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if continue && yieldLimitOk
        then do
            let stop = do
                    liftIO (incrementYieldLimit sv)
                    loopWithToken (seqNo + 1)
            foldStreamSVar sv st
                          (yieldOutput seqNo)
                          (singleOutput seqNo)
                          stop
                          r
        else do
            let ent = Entry seqNo (AheadEntryStream r)
            liftIO $ requeueOnHeapTop heap ent seqNo
            liftIO $ incrementYieldLimit sv
            drainHeap q heap st sv winfo

    loopWithToken nextSeqNo = do
        work <- dequeueAhead q
        case work of
            Nothing -> do
                liftIO $ updateHeapSeq heap nextSeqNo
                workLoopAhead q heap st sv winfo

            Just (m, seqNo) -> do
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                let undo = liftIO $ do
                        updateHeapSeq heap nextSeqNo
                        reEnqueueAhead sv q m
                        incrementYieldLimit sv
                if yieldLimitOk
                then
                    if seqNo == nextSeqNo
                    then do
                        let stop = do
                                liftIO (incrementYieldLimit sv)
                                loopWithToken (seqNo + 1)
                        foldStreamSVar sv st
                                      (yieldOutput seqNo)
                                      (singleOutput seqNo)
                                      stop
                                      m
                    else
                        -- To avoid a race when another thread puts something
                        -- on the heap and goes away, the consumer will not get
                        -- a doorBell and we will not clear the heap before
                        -- executing the next action. If the consumer depends
                        -- on the output that is stuck in the heap then this
                        -- will result in a deadlock. So we always clear the
                        -- heap before executing the next action.
                        undo >> workLoopAhead q heap st sv winfo
                else undo >> drainHeap q heap st sv winfo

-- XXX the yield limit changes increased the performance overhead by 30-40%.
-- Just like AsyncT we can use an implementation without yeidlimit and even
-- without pacing code to keep the performance higher in the unlimited and
-- unpaced case.
--
-- XXX The yieldLimit stuff is pretty invasive. We can instead do it by using
-- three hooks, a pre-execute hook, a yield hook and a stop hook. In fact these
-- hooks can be used for a more general implementation to even check predicates
-- and not just yield limit.

-- XXX we can remove the sv parameter as it can be derived from st

workLoopAhead :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)), Maybe Int)
    -> State Stream m a
    -> SVar Stream m a
    -> Maybe WorkerInfo
    -> m ()
workLoopAhead q heap st sv winfo = do
        r <- liftIO $ dequeueFromHeap heap
        case r of
            Ready (Entry seqNo hent) ->
                processHeap q heap st sv winfo hent seqNo False
            Clearing -> liftIO $ sendStop sv winfo
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
                work <- dequeueAhead q
                case work of
                    Nothing -> liftIO $ sendStop sv winfo
                    Just (m, seqNo) -> do
                        yieldLimitOk <- liftIO $ decrementYieldLimit sv
                        if yieldLimitOk
                        then
                            if seqNo == 0
                            then processWithToken q heap st sv winfo m seqNo
                            else processWithoutToken q heap st sv winfo m seqNo
                        -- If some worker decremented the yield limit but then
                        -- did not yield anything and therefore incremented it
                        -- later, then if we did not requeue m here we may find
                        -- the work queue empty and therefore miss executing
                        -- the remaining action.
                        else liftIO $ abortExecution q sv winfo m

-------------------------------------------------------------------------------
-- WAhead
-------------------------------------------------------------------------------

-- XXX To be implemented. Use a linked queue like WAsync and put back the
-- remaining computation at the back of the queue instead of the heap, and
-- increment the sequence number.

-- The only difference between forkSVarAsync and this is that we run the left
-- computation without a shared SVar.
forkSVarAhead :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
forkSVarAhead m1 m2 = mkStream $ \st stp sng yld -> do
        sv <- newAheadVar st (concurrently (toStream m1) (toStream m2))
                          workLoopAhead
        foldStream st stp sng yld (fromSVar sv)
    where
    concurrently ma mb = mkStream $ \st stp sng yld -> do
        liftIO $ enqueue (fromJust $ streamVar st) mb
        foldStream st stp sng yld ma

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'AheadT'.
-- Merges two streams sequentially but with concurrent lookahead.
--
-- @since 0.3.0
{-# INLINE ahead #-}
ahead :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
ahead m1 m2 = mkStream $ \st stp sng yld ->
    case streamVar st of
        Just sv | svarStyle sv == AheadVar -> do
            liftIO $ enqueue sv (toStream m2)
            -- Always run the left side on a new SVar to avoid complexity in
            -- sequencing results. This means the left side cannot further
            -- split into more ahead computations on the same SVar.
            foldStream st stp sng yld m1
        _ -> foldStreamShared st stp sng yld (forkSVarAhead m1 m2)

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using ahead.
{-# INLINE consMAhead #-}
{-# SPECIALIZE consMAhead :: IO a -> AheadT IO a -> AheadT IO a #-}
consMAhead :: MonadAsync m => m a -> AheadT m a -> AheadT m a
consMAhead m r = fromStream $ K.yieldM m `ahead` (toStream r)

------------------------------------------------------------------------------
-- AheadT
------------------------------------------------------------------------------

-- | The 'Semigroup' operation for 'AheadT' appends two streams. The combined
-- stream behaves like a single stream with the actions from the second stream
-- appended to the first stream. The combined stream is evaluated in the
-- speculative style.  This operation can be used to fold an infinite lazy
-- container of streams.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = do
--  xs \<- S.'toList' . 'aheadly' $ (p 1 |: p 2 |: nil) <> (p 3 |: p 4 |: nil)
--  print xs
--  where p n = threadDelay 1000000 >> return n
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream.
--
-- The monad instance of 'AheadT' may run each monadic continuation (bind)
-- concurrently in a speculative manner, performing side effects in a partially
-- ordered manner but producing the outputs in an ordered manner like
-- 'SerialT'.
--
-- @
-- main = S.drain . 'aheadly' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.yieldM $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- @since 0.3.0
newtype AheadT m a = AheadT {getAheadT :: Stream m a}
    deriving (MonadTrans)

-- | A serial IO stream of elements of type @a@ with concurrent lookahead.  See
-- 'AheadT' documentation for more details.
--
-- @since 0.3.0
type Ahead = AheadT IO

-- | Fix the type of a polymorphic stream as 'AheadT'.
--
-- @since 0.3.0
aheadly :: IsStream t => AheadT m a -> t m a
aheadly = K.adapt

instance IsStream AheadT where
    toStream = getAheadT
    fromStream = AheadT
    consM = consMAhead
    (|:) = consMAhead

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

{-# INLINE mappendAhead #-}
{-# SPECIALIZE mappendAhead :: AheadT IO a -> AheadT IO a -> AheadT IO a #-}
mappendAhead :: MonadAsync m => AheadT m a -> AheadT m a -> AheadT m a
mappendAhead m1 m2 = fromStream $ ahead (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (AheadT m a) where
    (<>) = mappendAhead

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (AheadT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

{-# INLINE concatMapAhead #-}
{-# SPECIALIZE concatMapAhead :: (a -> AheadT IO b) -> AheadT IO a -> AheadT IO b #-}
concatMapAhead :: MonadAsync m => (a -> AheadT m b) -> AheadT m a -> AheadT m b
concatMapAhead f m = fromStream $
    K.concatMapBy ahead (\a -> K.adapt $ f a) (K.adapt m)

instance MonadAsync m => Monad (AheadT m) where
    return = pure
    {-# INLINE (>>=) #-}
    (>>=) = flip concatMapAhead

instance (Monad m, MonadAsync m) => Applicative (AheadT m) where
    pure = AheadT . K.yield
    {-# INLINE (<*>) #-}
    (<*>) = ap

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_COMMON_INSTANCES(AheadT, MONADPARALLEL)
