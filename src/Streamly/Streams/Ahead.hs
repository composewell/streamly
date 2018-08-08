{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
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
import Control.Monad (ap, void)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Heap (Heap, Entry(..))
import Data.IORef (IORef, readIORef, atomicModifyIORef)
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup(..))
import GHC.Exts (inline)

import qualified Data.Heap as H

import Streamly.Streams.SVar (fromSVar)
import Streamly.Streams.Serial (map)
import Streamly.SVar
import Streamly.Streams.StreamK (IsStream(..), Stream(..))
import qualified Streamly.Streams.StreamK as K

#ifdef DIAGNOSTICS
import Control.Monad (when)
import Data.IORef (writeIORef)
#endif
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

-- XXX the only difference between this and workerRateControl is that this
-- updates the heap count and the other updates the yield count.
-- we can pass that as an update function instead.

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
                if (fromIntegral lim) >= len
                then lim - (fromIntegral len)
                else 0
            Unlimited -> Unlimited

    case maxHeap of
        Limited lim -> do
            active <- readIORef (workerCount sv)
            return $ H.size hp + active <= (fromIntegral lim)
        Unlimited -> return True

-- Return value:
-- True => stop
-- False => continue
preStopCheck ::
       SVar Stream m a
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Int)
    -> IO Bool
preStopCheck sv heap = do
    -- check the stop condition under a lock before actually
    -- stopping so that the whole herd does not stop at once.
    takeMVar (workerStopMVar sv)
    let stop = do
            putMVar (workerStopMVar sv) ()
            return True
        continue = do
            putMVar (workerStopMVar sv) ()
            return False
    (hp, _) <- readIORef heap
    heapOk <- underMaxHeap sv hp
    if heapOk
    then
        case yieldRateInfo sv of
            Nothing -> continue
            Just yinfo -> do
                rateOk <- isBeyondMaxRate sv yinfo
                if rateOk then continue else stop
    else stop

processHeap :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Int)
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
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
            atomicModifyIORef heap $ \(h, _) ->
                ((H.insert (Entry seqNo ent) h, seqNo), ())
            sendStop sv winfo
        else runStreamWithYieldLimit True seqNo r

    loopHeap seqNo ent = do
#ifdef DIAGNOSTICS
        liftIO $ do
            maxHp <- readIORef (maxHeapSize sv)
            (hp, _) <- readIORef heap
            when (H.size hp > maxHp) $ writeIORef (maxHeapSize sv) (H.size hp)
#endif
        case ent of
            AheadEntryPure a -> do
                -- Use 'send' directly so that we do not account this in worker
                -- latency as this will not be the real latency.
                -- Don't stop the worker in this case as we are just
                -- transferring available results from heap to outputQueue.
                void $ liftIO $ send sv (ChildYield a)
                nextHeap seqNo
            AheadEntryStream r -> do
                if stopping
                then stopIfNeeded ent seqNo r
                else runStreamWithYieldLimit True seqNo r

    nextHeap prevSeqNo = do
        -- XXX use "dequeueIfSeqential prevSeqNo" instead of always
        -- updating the sequence number in heap.
        liftIO $ atomicModifyIORef heap $ \(h, _) -> ((h, prevSeqNo + 1), ())
        ent <- liftIO $ dequeueFromHeap heap
        case ent of
            Just (Entry seqNo hent) -> loopHeap seqNo hent
            Nothing -> do
                if stopping
                then do
                    r <- liftIO $ preStopCheck sv heap
                    if r
                    then liftIO $ sendStop sv winfo
                    else processWorkQueue prevSeqNo
                else (inline processWorkQueue) prevSeqNo

    processWorkQueue prevSeqNo = do
        work <- dequeueAhead q
        case work of
            Nothing -> liftIO $ sendStop sv winfo
            Just (m, seqNo) -> do
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                if yieldLimitOk
                then do
                    if seqNo == prevSeqNo + 1
                    then processWithToken q heap st sv winfo m seqNo
                    else processWithoutToken q heap st sv winfo m seqNo
                else liftIO $ do
                    incrementYieldLimit sv
                    sendStop sv winfo

    -- We do not stop the worker on buffer full here as we want to proceed to
    -- nextHeap anyway so that we can clear any subsequent entries. We stop
    -- only in yield continuation where we may have a remaining stream to be
    -- pushed on the heap.
    singleStreamFromHeap seqNo a = do
        void $ liftIO $ sendYield sv winfo (ChildYield a)
        nextHeap seqNo

    runStreamWithYieldLimit continue seqNo r = do
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if continue && yieldLimitOk
        then do
            let stop = do
                  liftIO (incrementYieldLimit sv)
                  nextHeap seqNo
            unStream r st stop
                          (singleStreamFromHeap seqNo)
                          (yieldStreamFromHeap seqNo)
        else liftIO $ do
            atomicModifyIORef heap $ \(h, _) ->
                 ((H.insert (Entry seqNo (AheadEntryStream r)) h, seqNo), ())
            incrementYieldLimit sv
            sendStop sv winfo

    yieldStreamFromHeap seqNo a r = do
        continue <- liftIO $ sendYield sv winfo (ChildYield a)
        runStreamWithYieldLimit continue seqNo r

{-# NOINLINE drainHeap #-}
drainHeap :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Int)
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> m ()
drainHeap q heap st sv winfo = do
    ent <- liftIO $ dequeueFromHeap heap
    case ent of
        Nothing -> liftIO $ sendStop sv winfo
        Just (Entry seqNo hent) ->
            processHeap q heap st sv winfo hent seqNo True

processWithoutToken :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Int)
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> Stream m a
    -> Int
    -> m ()
processWithoutToken q heap st sv winfo m sno = do
    -- we have already decremented the yield limit for m
    let stop = do
            liftIO (incrementYieldLimit sv)
            workLoopAhead q heap st sv winfo

    unStream m st stop (singleToHeap sno) (yieldToHeap sno)

    where

    -- XXX to reduce contention each CPU can have its own heap
    toHeap seqNo ent = do
        -- Heap insertion is an expensive affair so we use a non CAS based
        -- modification, otherwise contention and retries can make a thread
        -- context switch and throw it behind other threads which come later in
        -- sequence.
        hp <- liftIO $ atomicModifyIORef heap $ \(h, snum) ->
            ((H.insert (Entry seqNo ent) h, snum), h)

        heapOk <- liftIO $ underMaxHeap sv hp
        if heapOk
        then
            case yieldRateInfo sv of
                Nothing -> workLoopAhead q heap st sv winfo
                Just yinfo -> do
                    rateOk <- liftIO $ workerRateControl sv yinfo winfo
                    if rateOk
                    then workLoopAhead q heap st sv winfo
                    else drainHeap q heap st sv winfo
        else drainHeap q heap st sv winfo

    singleToHeap seqNo a = toHeap seqNo (AheadEntryPure a)
    yieldToHeap seqNo a r = toHeap seqNo (AheadEntryStream (a `K.cons` r))

processWithToken :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Int)
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> Stream m a
    -> Int
    -> m ()
processWithToken q heap st sv winfo action sno = do
    let stop = do
            liftIO (incrementYieldLimit sv)
            loopWithToken sno

    unStream action st stop
                  (singleOutput sno)
                  (yieldOutput sno)

    where

    singleOutput seqNo a = do
        continue <- liftIO $ sendYield sv winfo (ChildYield a)
        if continue
        then loopWithToken seqNo
        else do
            liftIO $ atomicModifyIORef heap $ \(h, _) -> ((h, seqNo + 1), ())
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
                    loopWithToken seqNo
            unStream r st stop
                          (singleOutput seqNo)
                          (yieldOutput seqNo)
        else do
            liftIO $ atomicModifyIORef heap $ \(h, _) ->
                 ((H.insert (Entry seqNo (AheadEntryStream r)) h, seqNo), ())
            liftIO $ incrementYieldLimit sv
            drainHeap q heap st sv winfo

    loopWithToken prevSeqNo = do
        work <- dequeueAhead q
        case work of
            Nothing -> do
                liftIO $ atomicModifyIORef heap $ \(h, _) ->
                    ((h, prevSeqNo + 1), ())
                workLoopAhead q heap st sv winfo

            Just (m, seqNo) -> do
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                if yieldLimitOk
                then do
                    if seqNo == prevSeqNo + 1
                    then do
                        let stop = do
                                liftIO (incrementYieldLimit sv)
                                loopWithToken seqNo
                        unStream m st stop
                                      (singleOutput seqNo)
                                      (yieldOutput seqNo)
                    else do
                        liftIO $ atomicModifyIORef heap $ \(h, _) ->
                             ((h, prevSeqNo + 1), ())
                        -- To avoid a race when another thread puts something
                        -- on the heap and goes away, the consumer will not get
                        -- a doorBell and we will not clear the heap before
                        -- executing the next action. If the consumer depends
                        -- on the output that is stuck in the heap then this
                        -- will result in a deadlock. So we always clear the
                        -- heap before executing the next action.
                        -- processWithoutToken q heap st sv winfo m seqNo
                        liftIO $ reEnqueueAhead sv q m
                        workLoopAhead q heap st sv winfo
                else do
                    liftIO $ atomicModifyIORef heap $ \(h, _) ->
                         ((h, prevSeqNo + 1), ())
                    liftIO $ incrementYieldLimit sv
                    drainHeap q heap st sv winfo

-- XXX the yield limit changes increased the performance overhead by 30-40%.
-- Just like AsyncT we can use an implementation without yeidlimit and even
-- without pacing code to keep the performance higher in the unlimited and
-- unpaced case.
workLoopAhead :: MonadIO m
    => IORef ([Stream m a], Int)
    -> IORef (Heap (Entry Int (AheadHeapEntry Stream m a)) , Int)
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> m ()
workLoopAhead q heap st sv winfo = do
#ifdef DIAGNOSTICS
        liftIO $ do
            maxHp <- readIORef (maxHeapSize sv)
            (hp, _) <- readIORef heap
            when (H.size hp > maxHp) $ writeIORef (maxHeapSize sv) (H.size hp)
#endif
        ent <- liftIO $ dequeueFromHeap heap
        case ent of
            Nothing -> do
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
                        then do
                            if seqNo == 0
                            then processWithToken q heap st sv winfo m seqNo
                            else processWithoutToken q heap st sv winfo m seqNo
                        else liftIO $ do
                            incrementYieldLimit sv
                            sendStop sv winfo
            Just (Entry seqNo hent) ->
                processHeap q heap st sv winfo hent seqNo False

-------------------------------------------------------------------------------
-- WAhead
-------------------------------------------------------------------------------

-- XXX To be implemented. Use a linked queue like WAsync and put back the
-- remaining computation at the back of the queue instead of the heap, and
-- increment the sequence number.

-- The only difference between forkSVarAsync and this is that we run the left
-- computation without a shared SVar.
forkSVarAhead :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
forkSVarAhead m1 m2 = Stream $ \st stp sng yld -> do
        sv <- newAheadVar st (concurrently m1 m2) workLoopAhead
        unStream (fromSVar sv) (rstState st) stp sng yld
    where
    concurrently ma mb = Stream $ \st stp sng yld -> do
        liftIO $ enqueue (fromJust $ streamVar st) mb
        unStream ma (rstState st) stp sng yld

{-# INLINE aheadS #-}
aheadS :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
aheadS m1 m2 = Stream $ \st stp sng yld -> do
    case streamVar st of
        Just sv | svarStyle sv == AheadVar -> do
            liftIO $ enqueue sv m2
            -- Always run the left side on a new SVar to avoid complexity in
            -- sequencing results. This means the left side cannot further
            -- split into more ahead computations on the same SVar.
            unStream m1 (rstState st) stp sng yld
        _ -> unStream (forkSVarAhead m1 m2) st stp sng yld

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using ahead.
{-# INLINE consMAhead #-}
consMAhead :: MonadAsync m => m a -> Stream m a -> Stream m a
consMAhead m r = K.yieldM m `aheadS` r

------------------------------------------------------------------------------
-- AheadT
------------------------------------------------------------------------------

-- | Deep ahead composition or ahead composition with depth first traversal.
-- The semigroup composition of 'AheadT' appends streams in a depth first
-- manner just like 'SerialT' except that it can produce elements concurrently
-- ahead of time. It is like 'AsyncT' except that 'AsyncT' produces the output
-- as it arrives whereas 'AheadT' orders the output in the traversal order.
--
-- @
-- main = ('toList' . 'aheadly' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream.
--
-- Similarly, the monad instance of 'AheadT' may run each iteration
-- concurrently ahead of time but presents the results in the same order as
-- 'SerialT'.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'runStream' . 'aheadly' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.once $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- All iterations may run in the same thread if they do not block.
--
-- Note that ahead composition with depth first traversal can be used to
-- combine infinite number of streams as it explores only a bounded number of
-- streams at a time.
--
-- @since 0.3.0
newtype AheadT m a = AheadT {getAheadT :: Stream m a}
    deriving (MonadTrans)

-- | A serial IO stream of elements of type @a@ with concurrent lookahead.  See
-- 'AheadT' documentation for more details.
--
-- @since 0.3.0
type Ahead a = AheadT IO a

-- | Fix the type of a polymorphic stream as 'AheadT'.
--
-- @since 0.3.0
aheadly :: IsStream t => AheadT m a -> t m a
aheadly = K.adapt

instance IsStream AheadT where
    toStream = getAheadT
    fromStream = AheadT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> AheadT IO a -> AheadT IO a #-}
    consM m r = fromStream $ consMAhead m (toStream r)

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> AheadT IO a -> AheadT IO a #-}
    (|:) = consM

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'AheadT'.
-- Merges two streams sequentially but with concurrent lookahead.
--
-- @since 0.3.0
{-# INLINE ahead #-}
ahead :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
ahead m1 m2 = fromStream $ aheadS (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (AheadT m a) where
    (<>) = ahead

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (AheadT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

{-# INLINE aheadbind #-}
aheadbind
    :: MonadAsync m
    => Stream m a
    -> (a -> Stream m b)
    -> Stream m b
aheadbind m f = go m
    where
        go (Stream g) =
            Stream $ \st stp sng yld ->
            let run x = unStream x st stp sng yld
                single a   = run $ f a
                yieldk a r = run $ f a `aheadS` go r
            in g (rstState st) stp single yieldk

instance MonadAsync m => Monad (AheadT m) where
    return = pure
    (AheadT m) >>= f = AheadT $ aheadbind m (getAheadT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(AheadT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(AheadT, MONADPARALLEL)
