-- |
-- Module      : Streamly.Internal.Data.Stream.Parallel.Channel.Type
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Parallel.Channel.Type
    (
      Channel(..)
    , SVarStopStyle (..)
    , newChannel
    , toChannel
    , toChannelD
    , toChannelK
    , runOne
    , runOneLimited
    , fromChannel
    , readChannel
    , fromChannelK
    , newCallbackStream
    )
where

#include "inline.hs"

import Control.Concurrent (ThreadId, myThreadId, takeMVar)
import Control.Concurrent.MVar
    (MVar, newEmptyMVar, tryPutMVar, tryTakeMVar, newMVar)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadThrow, throwM, fromException)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef (IORef, newIORef, readIORef, mkWeakIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Set (Set)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Control.Concurrent
    (MonadAsync, RunInIO(..), askRunInIO)
import Streamly.Internal.Control.ForkLifted (doFork)
import Streamly.Internal.Data.Atomics
       (atomicModifyIORefCAS, atomicModifyIORefCAS_, writeBarrier)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import System.Mem (performMajorGC)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Fold as Fold (Step(..))
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Channel.Dispatcher
import Streamly.Internal.Data.Stream.Channel.Worker
import Streamly.Internal.Data.Stream.Channel.Types

-- | When to stop the composed stream.
data SVarStopStyle =
      StopNone -- stops only when all streams are finished
    | StopAny  -- stop when any stream finishes
    | StopBy   -- stop when a specific stream finishes
    deriving (Eq, Show)

-- | Buffering policy for persistent push workers (in Parallel).  In a pull
-- style Channel (in Async, Ahead etc.), the consumer side dispatches workers
-- on demand, workers terminate if the buffer is full or if the consumer is not
-- cosuming fast enough.  In a push style SVar, a worker is dispatched only
-- once, workers are persistent and keep pushing work to the consumer via a
-- bounded buffer. If the buffer becomes full the worker either blocks, or it
-- can drop an item from the buffer to make space.
--
data PushBufferPolicy =
      PushBufferDropNew  -- drop the latest element and continue
    | PushBufferDropOld  -- drop the oldest element and continue
    | PushBufferBlock    -- block the thread until space
                         -- becomes available

-- | See the "Streamly.Internal.Data.Stream.Async" module for documentation of
-- common fields.
data Channel m a = Channel
    {
      svarMrun        :: RunInIO m
    , outputQueue    :: IORef ([ChildEvent a], Int)
    , outputDoorBell :: MVar ()  -- signal the consumer about output

    -- XXX Can we use IO instead of m here?
    , readOutputQ    :: m [ChildEvent a]
    , postProcess    :: m Bool

    -- Scheduling --
    , maxBufferLimit :: Limit

    , remainingWork  :: Maybe (IORef Count)
    , yieldRateInfo  :: Maybe YieldRateInfo
    , workerThreads  :: IORef (Set ThreadId)
    , workerCount    :: IORef Int
    -- XXX Can we use IO instead of m here?
    , accountThread  :: ThreadId -> m ()

    -- cleanup: to track garbage collection of SVar --
    , svarRef        :: Maybe (IORef ())

    -- Stats --
    , svarStats      :: SVarStats

    -- Diagnostics --
    , svarInspectMode :: Bool
    , svarCreator    :: ThreadId

    ----- Parallel stream specific fields------

    , svarStopStyle   :: SVarStopStyle
    , svarStopBy      :: IORef ThreadId

    -- These two are valid and used only when maxBufferLimit is Limited.
    , pushBufferSpace  :: IORef Count
    , pushBufferPolicy :: PushBufferPolicy
    -- [LOCKING] The consumer puts this MVar after emptying the buffer, workers
    -- block on it when the buffer becomes full. No overhead unless the buffer
    -- becomes full.
    , pushBufferMVar :: MVar ()
    }

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

-- XXX This only differs in one field from async/ahead dump
{-# NOINLINE dumpSVar #-}
dumpSVar :: Channel m a -> IO String
dumpSVar sv = do
    xs <- sequence
        [ return (dumpCreator (svarCreator sv))
        , return "---------CURRENT STATE-----------"
        , dumpOutputQ (outputQueue sv)
        -- XXX print the types of events in the outputQueue, first 5
        , dumpDoorBell (outputDoorBell sv)
        -- , dumpNeedDoorBell (needDoorBell sv)
        -- , dumpRunningThreads (workerThreads sv)
        -- XXX print the status of first 5 threads
        , dumpWorkerCount (workerCount sv)
        , return "---------STATS-----------\n"
        , dumpSVarStats (svarInspectMode sv) (yieldRateInfo sv) (svarStats sv)
        ]
    return $ concat xs

-------------------------------------------------------------------------------
-- Consumer side of the channel
-------------------------------------------------------------------------------

{-# INLINE resetBufferLimit #-}
resetBufferLimit :: Channel m a -> IO ()
resetBufferLimit sv =
    case maxBufferLimit sv of
        Unlimited -> return ()
        Limited n -> do
            void $ tryTakeMVar (pushBufferMVar sv)
            atomicModifyIORefCAS_ (pushBufferSpace sv) (const (fromIntegral n))
            writeBarrier
            void $ tryPutMVar (pushBufferMVar sv) ()

-- | Needs to synchronize with the producer code running concurrently.
readOutputQPar :: MonadIO m => Channel m a -> m [ChildEvent a]
readOutputQPar sv = liftIO $ do
    withDiagMVar
        (svarInspectMode sv)
        (dumpSVar sv)
        "readOutputQPar: doorbell"
        $ takeMVar (outputDoorBell sv)
    case yieldRateInfo sv of
        Nothing -> return ()
        Just yinfo ->
            void $ collectLatency
                (svarInspectMode sv) (svarStats sv) yinfo False
    let stats = if svarInspectMode sv then Just (svarStats sv) else Nothing
    r <- fst `fmap` readOutputQRaw (outputQueue sv) stats

    liftIO $ resetBufferLimit sv
    return r

-------------------------------------------------------------------------------
-- Producer side of the channel: StreamK based worker routines
-------------------------------------------------------------------------------

decrementSlowPath ::
    IORef ([a], Int) -> PushBufferPolicy -> IORef Count -> MVar () -> IO ()
decrementSlowPath q policy spaceRef lock = do
    -- putStrLn "decrementSlowPath"
    case policy of
        PushBufferBlock -> blockAndRetry
        PushBufferDropNew -> do
            -- We just drop one item and proceed. It is possible
            -- that by the time we drop the item the consumer
            -- thread might have run and created space in the
            -- buffer, but we do not care about that condition.
            -- This is not pedantically correct but it should be
            -- fine in practice.
            -- XXX we may want to drop only if n == maxBuf
            -- otherwise we must have space in the buffer and a
            -- decrement should be possible.
            block <- atomicModifyIORefCAS q $
                \(es, n) ->
                    case es of
                        [] -> (([],n), True)
                        _ : xs -> ((xs, n - 1), False)
            when block blockAndRetry
        -- XXX need a dequeue or ring buffer for this
        PushBufferDropOld -> undefined

    where

    blockAndRetry = do
        liftIO $ takeMVar lock
        old <- atomicModifyIORefCAS spaceRef $ \x ->
                        (if x >= 1 then x - 1 else x, x)
        -- When multiple threads sleep on takeMVar, the first thread would
        -- wakeup due to a putMVar by the consumer, but the rest of the threads
        -- would have to put back the MVar after taking it and decrementing the
        -- buffer count, otherwise all other threads will remain asleep.
        if old >= 1
        then void $ liftIO $ tryPutMVar lock ()
        -- We do not put the MVar back in this case, instead we
        -- wait for the consumer to put it.
        else blockAndRetry

{-# INLINE decrementBufferLimit #-}
decrementBufferLimit :: Channel m a -> IO ()
decrementBufferLimit sv =
    case maxBufferLimit sv of
        Unlimited -> return ()
        Limited _ -> do
            let ref = pushBufferSpace sv
            old <- atomicModifyIORefCAS ref $ \x ->
                        (if x >= 1 then x - 1 else x, x)
            when (old <= 0)
                $ decrementSlowPath
                    (outputQueue sv)
                    (pushBufferPolicy sv)
                    (pushBufferSpace sv)
                    (pushBufferMVar sv)

{-# INLINE incrementBufferLimit #-}
incrementBufferLimit :: Channel m a -> IO ()
incrementBufferLimit sv =
    case maxBufferLimit sv of
        Unlimited -> return ()
        Limited _ -> do
            atomicModifyIORefCAS_ (pushBufferSpace sv) (+ 1)
            writeBarrier
            void $ liftIO $ tryPutMVar (pushBufferMVar sv) ()

{-# INLINE yield #-}
yield :: Channel m a -> a -> IO ()
yield chan x =
    void
        $ sendWithDoorBell
            (outputQueue chan) (outputDoorBell chan) (ChildYield x)

{-# INLINE stop #-}
stop :: Channel m a -> Maybe WorkerInfo -> IO ()
stop chan winfo =
    sendStop
        (workerCount chan)
        winfo
        (yieldRateInfo chan)
        (outputQueue chan)
        (outputDoorBell chan)

runOneLimited
    :: MonadIO m
    => Channel m a -> K.Stream m a -> Maybe WorkerInfo -> m ()
runOneLimited chan m0 winfo = go m0

    where

    go m = do
        yieldLimitOk <- liftIO $ decrementYieldLimit (remainingWork chan)
        if yieldLimitOk
        then do
            liftIO $ decrementBufferLimit chan
            K.foldStreamShared undefined yieldk single stopk m
        else do
            liftIO $ cleanupSVar (workerThreads chan)
            liftIO $ stop chan winfo

    stopk = liftIO $ do
        incrementBufferLimit chan
        incrementYieldLimit (remainingWork chan)
        stop chan winfo

    single a = liftIO (yield chan a >> stop chan winfo)

    yieldk a r = do
        -- XXX This code is same as go except the yield, reproduced here to
        -- avoid mutual recursion between go and yieldk. We can possibly pass
        -- yieldk to "go" to use the same go routine without mutual recursion.
        yieldLimitOk <-
            liftIO $ do
                yield chan a
                decrementYieldLimit (remainingWork chan)
        if yieldLimitOk
        then do
            liftIO $ decrementBufferLimit chan
            K.foldStreamShared undefined yieldk single stopk r
        else liftIO $ do
            cleanupSVar (workerThreads chan)
            stop chan winfo

-- | Needs to synchronize with other producers and the consumer running
-- concurrently.
{-# INLINE runOne #-}
runOne
    :: MonadIO m
    => Channel m a -> K.Stream m a -> Maybe WorkerInfo -> m ()
runOne chan m0 winfo =
    -- XXX This can be done statically when creating the channel
    case remainingWork chan of
        Nothing -> go m0
        Just _  -> runOneLimited chan m0 winfo

    where

    go m = do
        liftIO $ decrementBufferLimit chan
        K.foldStreamShared undefined yieldk single stopk m

    stopk = liftIO $ do
        incrementBufferLimit chan
        stop chan winfo

    single a = liftIO $ yield chan a >> stop chan winfo

    yieldk a r = do
        liftIO $ do
            yield chan a
            -- XXX this is expensive because of atomicModifyIORefCAS, this
            -- alone has 3x perf impact. We can use sendYield here and then
            -- sleep only if it returns False in which case we definitively
            -- check if we will be woken up before sleeping.
            decrementBufferLimit chan
        K.foldStreamShared undefined yieldk single stopk r

-------------------------------------------------------------------------------
-- Producer side of the channel: Fold based workers
-------------------------------------------------------------------------------

-- | A fold to write a stream to a channel.
{-# INLINE write #-}
write :: MonadIO m => Channel m a -> Maybe WorkerInfo -> Fold m a ()
write svar winfo = Fold step initial extract

    where

    initial = return $ Fold.Partial ()

    step () x =
        liftIO $ do
            decrementBufferLimit svar
            yield svar x
            return $ Fold.Partial ()

    extract () = liftIO $ stop svar winfo

-- | Like write, but applies a yield limit.
--
{-# INLINE writeLimited #-}
writeLimited :: MonadIO m
    => Channel m a -> Maybe WorkerInfo -> Fold m a ()
writeLimited svar winfo = Fold step initial extract

    where

    initial = return $ Fold.Partial True

    step True x =
        liftIO $ do
            yieldLimitOk <- decrementYieldLimit (remainingWork svar)
            if yieldLimitOk
            then do
                decrementBufferLimit svar
                yield svar x
                return $ Fold.Partial True
            else do
                cleanupSVar (workerThreads svar)
                stop svar winfo
                return $ Fold.Done ()
    step False _ = return $ Fold.Done ()

    extract True = liftIO $ stop svar winfo
    extract False = return ()

-------------------------------------------------------------------------------
-- Adding a stream to channel: StreamK version
-------------------------------------------------------------------------------

-- XXX We can reuse code in this and toChannelD

-- | In contrast to pushWorker which always happens only from the consumer
-- thread, a pushWorkerPar can happen concurrently from multiple threads on the
-- producer side. So we need to use a thread safe modification of
-- workerThreads. Alternatively, we can use a CreateThread event to avoid
-- using a CAS based modification.
{-# INLINE pushWorkerPar #-}
pushWorkerPar
    :: (MonadIO m, MonadBaseControl IO m)
    => Channel m a -> (Maybe WorkerInfo -> m ()) -> m ()
pushWorkerPar sv wloop =
    if svarInspectMode sv
    then forkWithDiag
    else doFork (wloop Nothing) (svarMrun sv) exception >>= modThread

    where

    modThread = modifyThread (workerThreads sv) (outputDoorBell sv)
    exception = handleChildException (outputQueue sv) (outputDoorBell sv)

    {-# NOINLINE forkWithDiag #-}
    forkWithDiag = do
        -- We do not use workerCount in case of ParallelVar but still there is
        -- no harm in maintaining it correctly.
        liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
        recordMaxWorkers (workerCount sv) (svarStats sv)
        -- This allocation matters when significant number of workers are being
        -- sent. We allocate it only when needed. The overhead increases by 4x.
        winfo <-
            case yieldRateInfo sv of
                Nothing -> return Nothing
                Just _ -> liftIO $ do
                    cntRef <- newIORef 0
                    t <- getTime Monotonic
                    lat <- newIORef (0, t)
                    return $ Just WorkerInfo
                        { workerYieldMax = 0
                        , workerYieldCount = cntRef
                        , workerLatencyStart = lat
                        }

        doFork (wloop winfo) (svarMrun sv) exception >>= modThread

-- XXX Instead of sending the worker right away we should just queue the work
-- if the channel has not yet started fromChannel. We should not start any
-- evaluation until we use fromChannel.
{-# INLINE toChannelK #-}
toChannelK :: (MonadIO m, MonadBaseControl IO m) =>
    Channel m a -> K.Stream m a -> m ()
toChannelK chan xs = pushWorkerPar chan (runOne chan xs)

-------------------------------------------------------------------------------
-- Adding a stream to channel: StreamD/fold version
-------------------------------------------------------------------------------

-- Using StreamD the worker stream producing code can fuse with the code to
-- queue output to the SVar giving some perf boost.

-- XXX Instead of sending the worker right away we should just queue the work
-- if the channel has not yet invoked fromChannel. We should not start any
-- evaluation until we use fromChannel.

-- | Fold the supplied stream to the SVar asynchronously using Parallel
-- concurrency style.
{-# INLINE toChannelD #-}
toChannelD :: (MonadIO m, MonadBaseControl IO m) =>
    Channel m a -> D.Stream m a -> m ()
toChannelD sv xs =
    if svarInspectMode sv
    then forkWithDiag
    else do
        tid <-
            case remainingWork sv of
                Nothing -> doFork (work Nothing) (svarMrun sv) exception
                Just _ -> doFork (workLim Nothing) (svarMrun sv) exception
        modThread tid

    where

    modThread = modifyThread (workerThreads sv) (outputDoorBell sv)
    exception = handleChildException (outputQueue sv) (outputDoorBell sv)

    {-# NOINLINE work #-}
    work info = D.fold (write sv info) xs

    {-# NOINLINE workLim #-}
    workLim info = D.fold (writeLimited sv info) xs

    {-# NOINLINE forkWithDiag #-}
    forkWithDiag = do
        liftIO $ atomicModifyIORefCAS_ (workerCount sv) $ \n -> n + 1
        recordMaxWorkers (workerCount sv) (svarStats sv)
        -- This allocation matters when significant number of workers are being
        -- sent. We allocate it only when needed. The overhead increases by 4x.
        winfo <-
            case yieldRateInfo sv of
                Nothing -> return Nothing
                Just _ -> liftIO $ do
                    cntRef <- newIORef 0
                    t <- getTime Monotonic
                    lat <- newIORef (0, t)
                    return $ Just WorkerInfo
                        { workerYieldMax = 0
                        , workerYieldCount = cntRef
                        , workerLatencyStart = lat
                        }
        tid <-
            case remainingWork sv of
                Nothing -> doFork (work winfo) (svarMrun sv) exception
                Just _  -> doFork (workLim winfo) (svarMrun sv) exception
        modThread tid

-- | Send a stream to a given channel for concurrent evaluation.
toChannel :: (MonadIO m, MonadBaseControl IO m) =>
    Channel m a -> Stream m a -> m ()
-- toChannel chan = toChannelD chan . Stream.toStreamD
toChannel chan = toChannelK chan . Stream.toStreamK

-------------------------------------------------------------------------------
-- Read a stream from a channel
-------------------------------------------------------------------------------

-- | Pull a stream from an SVar.
{-# NOINLINE fromChannelRaw #-}
fromChannelRaw :: (MonadIO m, MonadThrow m) => Channel m a -> K.Stream m a
fromChannelRaw sv = K.MkStream $ \st yld sng stp -> do
    list <- readOutputQ sv
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    K.foldStream st yld sng stp $ processEvents $ reverse list

    where

    allDone stp = do
        when (svarInspectMode sv) $ liftIO $ do
            t <- getTime Monotonic
            writeIORef (svarStopTime (svarStats sv)) (Just t)
            printSVar (dumpSVar sv) "SVar Done"
        stp

    {-# INLINE processEvents #-}
    processEvents [] = K.MkStream $ \st yld sng stp -> do
        done <- postProcess sv
        if done
        then allDone stp
        else K.foldStream st yld sng stp $ fromChannelRaw sv

    processEvents (ev : es) = K.MkStream $ \st yld sng stp -> do
        let rest = processEvents es
        case ev of
            ChildYield a -> yld a rest
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> do
                        r <- shouldStop tid
                        if r
                        then do
                            liftIO (cleanupSVar (workerThreads sv))
                            allDone stp
                        else K.foldStream st yld sng stp rest
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                K.foldStream st yld sng stp rest
                            Nothing -> do
                                liftIO (cleanupSVar (workerThreads sv))
                                throwM ex
    shouldStop tid =
        case svarStopStyle sv of
            StopNone -> return False
            StopAny -> return True
            StopBy -> do
                sid <- liftIO $ readIORef (svarStopBy sv)
                return $ tid == sid

#if __GLASGOW_HASKELL__ < 810
#ifdef INSPECTION
-- Use of GHC constraint tuple (GHC.Classes.(%,,%)) in fromStreamVar leads to
-- space leak because the tuple gets allocated in every recursive call and each
-- allocation holds on to the previous allocation. This test is to make sure
-- that we do not use the constraint tuple type class.
--
inspect $ hasNoTypeClassesExcept 'fromStreamVar
    [ ''Monad
    , ''Applicative
    , ''MonadThrow
    , ''Exception
    , ''MonadIO
    , ''MonadBaseControl
    , ''Typeable
    , ''Functor
    ]
#endif
#endif

-- | Generate a stream from an SVar.  An unevaluated stream can be pushed to an
-- SVar using 'toSVar'.  As we pull a stream from the SVar the input stream
-- gets evaluated concurrently. The evaluation depends on the SVar style and
-- the configuration parameters e.g. using the maxBuffer/maxThreads
-- combinators.
--
{-# INLINE fromChannelK #-}
fromChannelK :: (MonadIO m, MonadThrow m) => Channel m a -> K.Stream m a
fromChannelK sv =
    K.mkStream $ \st yld sng stp -> do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- We pass a copy of sv to fromStreamVar, so that we know that it has
        -- no other references, when that copy gets garbage collected "ref"
        -- will get garbage collected and our hook will be called.
        K.foldStreamShared st yld sng stp $
            fromChannelRaw sv{svarRef = Just ref}
    where

    hook = do
        when (svarInspectMode sv) $ do
            r <- liftIO $ readIORef (svarStopTime (svarStats sv))
            when (isNothing r) $
                printSVar (dumpSVar sv) "SVar Garbage Collected"
        cleanupSVar (workerThreads sv)
        -- If there are any SVars referenced by this SVar a GC will prompt
        -- them to be cleaned up quickly.
        when (svarInspectMode sv) performMajorGC

{-# ANN type ReadSVarState Fuse #-}
data ReadSVarState t m a =
      ReadSVarInit (Channel m a)
    | ReadSVarRead (Channel m a)
    | ReadSVarLoop (Channel m a) [ChildEvent a]
    | ReadSVarDone (Channel m a)

{-# INLINE_NORMAL readChannel #-}
readChannel :: (MonadIO m, MonadThrow m) => Unfold m (Channel m a) a
readChannel = Unfold step inject

    where

    inject = return . ReadSVarInit

    {-# INLINE_LATE step #-}
    step (ReadSVarInit svar) = do
        ref <- liftIO $ newIORef ()
        _ <- liftIO $ mkWeakIORef ref hook
        -- when this copy of svar gets garbage collected "ref" will get
        -- garbage collected and our GC hook will be called.
        let sv = svar{svarRef = Just ref}
        return $ D.Skip (ReadSVarRead sv)

        where

        {-# NOINLINE hook #-}
        hook = do
            when (svarInspectMode svar) $ do
                r <- liftIO $ readIORef (svarStopTime (svarStats svar))
                when (isNothing r) $
                    printSVar (dumpSVar svar) "SVar Garbage Collected"
            cleanupSVar (workerThreads svar)
            -- If there are any SVars referenced by this SVar a GC will prompt
            -- them to be cleaned up quickly.
            when (svarInspectMode svar) performMajorGC

    step (ReadSVarRead sv) = do
        list <- readOutputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ D.Skip $ ReadSVarLoop sv (Prelude.reverse list)

    step (ReadSVarLoop sv []) = do
        done <- postProcess sv
        return
            $ D.Skip
            $ if done
              then ReadSVarDone sv
              else ReadSVarRead sv

    step (ReadSVarLoop sv (ev : es)) = do
        case ev of
            ChildYield a -> return $ D.Yield a (ReadSVarLoop sv es)
            ChildStop tid e -> do
                accountThread sv tid
                case e of
                    Nothing -> return $ D.Skip (ReadSVarLoop sv es)
                    Just ex ->
                        case fromException ex of
                            Just ThreadAbort ->
                                return $ D.Skip (ReadSVarLoop sv es)
                            Nothing -> do
                                liftIO (cleanupSVar (workerThreads sv))
                                throwM ex

    step (ReadSVarDone sv) = liftIO $ do
        when (svarInspectMode sv) $ do
            t <- getTime Monotonic
            writeIORef (svarStopTime (svarStats sv)) (Just t)
            printSVar (dumpSVar sv) "SVar Done"
        return D.Stop

-- | Generate a stream of results from concurrent evaluations from a channel.
-- Evaluation of the channel does not start until this API is called. This API
-- must not be called more than once on a channel. It kicks off evaluation of
-- the channel by dispatching concurrent workers and ensures that as long there
-- is work queued on the channel workers are dispatched proportional to the
-- demand by the consumer.
--
{-# INLINE fromChannel #-}
fromChannel :: (MonadIO m, MonadThrow m) => Channel m a -> Stream m a
fromChannel = Stream.fromStreamK . fromChannelK

-------------------------------------------------------------------------------
-- Create a channel
-------------------------------------------------------------------------------

getParallelSVar :: MonadIO m
    => SVarStopStyle -> RunInIO m -> Config -> IO (Channel m a)
getParallelSVar ss mrun cfg = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    running <- newIORef Set.empty
    yl <- case getYieldLimit cfg of
            Nothing -> return Nothing
            Just x -> Just <$> newIORef x
    rateInfo <- newRateInfo cfg
    let bufLim =
            case getMaxBuffer cfg of
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
            Channel { outputQueue      = outQ
                 , remainingWork    = yl
                 , maxBufferLimit   = getMaxBuffer cfg
                 , pushBufferSpace  = remBuf
                 , pushBufferPolicy = PushBufferBlock
                 , pushBufferMVar   = pbMVar
                 -- Used only for diagnostics
                 , yieldRateInfo    = rateInfo
                 , outputDoorBell   = outQMv
                 , readOutputQ      = readOutputQPar sv
                 , postProcess      = allThreadsDone running
                 , workerThreads    = running
                 , svarStopStyle    = ss
                 , svarStopBy       = stopBy
                 , svarMrun         = mrun
                 , workerCount      = active
                 , accountThread    = modifyThread running outQMv
                 , svarRef          = Nothing
                 , svarInspectMode  = getInspectMode cfg
                 , svarCreator      = tid
                 , svarStats        = stats
                 }
     in return sv

{-# INLINABLE newChannel #-}
{-# SPECIALIZE newChannel ::
    SVarStopStyle -> (Config -> Config) -> IO (Channel IO a) #-}
newChannel :: MonadAsync m =>
    SVarStopStyle -> (Config -> Config) -> m (Channel m a)
newChannel ss modifier = do
    mrun <- askRunInIO
    liftIO $ getParallelSVar ss mrun (modifier defaultConfig)

-------------------------------------------------------------------------------
-- From callback
-------------------------------------------------------------------------------

-- XXX This basically nothing but a toChannel (queuing a single element) and
-- fromChannel. We can just use a channel directly instead
--
-- Note: we can use another API with two callbacks stop and yield if we want
-- the callback to be able to indicate end of stream.

-- | Generates a callback and a stream pair. The callback returned is used to
-- queue values to the stream.  The stream is infinite, there is no way for the
-- callback to indicate that it is done now.
--
-- /Pre-release/
--
{-# INLINE_NORMAL newCallbackStream #-}
newCallbackStream :: MonadAsync m => m (a -> m (), Stream m a)
newCallbackStream = do
    chan <- newChannel StopNone id

    -- XXX Add our own thread-id to the SVar as we can not know the callback's
    -- thread-id and the callback is not run in a managed worker. We need to
    -- handle this better.
    liftIO myThreadId
        >>= modifyThread (workerThreads chan) (outputDoorBell chan)

    let callback a =
            liftIO
                $ void
                $ sendWithDoorBell
                    (outputQueue chan) (outputDoorBell chan) (ChildYield a)
    -- XXX Use fromChannelD?
    return (callback, fromChannel chan)
