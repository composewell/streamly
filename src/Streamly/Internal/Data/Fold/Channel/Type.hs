-- |
-- Module      : Streamly.Internal.Data.Fold.Channel.Type
-- Copyright   : (c) 2017, 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Fold.Channel.Type
    (
    -- ** Type
      Channel (..)
    , OutEvent (..)

    -- ** Configuration
    , Config
    , defaultConfig
    , maxBuffer
    , boundThreads
    , inspect

    -- ** Operations
    , newChannelWith
    , newChannelWithScan
    , newChannel
    , newScanChannel
    , sendToWorker
    , sendToWorker_
    , checkFoldStatus -- XXX collectFoldOutput
    , dumpChannel
    , cleanup
    , finalize
    )
where

#include "inline.hs"

import Control.Concurrent (ThreadId, myThreadId, tryPutMVar)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Exception (SomeException(..))
import Control.Monad (void, when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import Streamly.Internal.Control.Concurrent
    (MonadAsync, MonadRunInIO, askRunInIO)
import Streamly.Internal.Control.ForkLifted (doForkWith)
import Streamly.Internal.Data.Atomics (writeBarrier)
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Internal.Data.Scanl (Scanl(..))
import Streamly.Internal.Data.Channel.Dispatcher (dumpSVarStats)
import Streamly.Internal.Data.Channel.Worker (sendEvent)
import Streamly.Internal.Data.Time.Clock (Clock(Monotonic), getTime)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as D

import Streamly.Internal.Data.Channel.Types

-- XXX We can make the fold evaluation concurrent by using a monoid for the
-- accumulator. It will then work in the same way as the stream evaluation, in
-- stream evaluation we dequeue the head and queue the tail, in folds we will
-- queue the accumulator and it will be picked by the next worker to accumulate
-- the next value.

data OutEvent b =
      FoldException ThreadId SomeException
    | FoldPartial b
    | FoldDone ThreadId b
    | FoldEOF ThreadId

-- | The fold driver thread queues the input of the fold in the 'inputQueue'
-- The driver rings the doorbell when the queue transitions from empty to
-- non-empty state.
--
-- The fold consumer thread dequeues the input items from the 'inputQueue' and
-- supplies them to the fold. When the fold is done the output of the fold is
-- placed in 'inputQueue' and 'outputDoorBell' is rung.
--
-- The fold driver thread keeps watching the 'outputQueue', if the fold has
-- terminated, it stops queueing the input to the 'inputQueue'
--
-- If the fold driver runs out of input it stops and waits for the fold to
-- drain the buffered input.
--
-- Driver thread ------>------Input Queue and Doorbell ----->-----Fold thread
--
-- Driver thread ------<------Output Queue and Doorbell-----<-----Fold thread
--
data Channel m a b = Channel
    {
    -- FORWARD FLOW: Flow of data from the driver to the consuming fold

    -- XXX Use a different type than ChildEvent. We can do with a simpler type
    -- in folds.

    -- | Input queue (messages, length).
    --
    -- [LOCKING] Frequent, locked access. Input is queued frequently by the
    -- driver and infrequently dequeued in chunks by the fold.
    --
      inputQueue :: IORef ([ChildEvent a], Int)

      -- | The maximum size of the inputQueue allowed.
    , maxInputBuffer :: Limit

    -- | Doorbell is rung by the driver when 'inputQueue' transitions from
    -- empty to non-empty.
    --
    -- [LOCKING] Infrequent, MVar.
    , inputItemDoorBell :: MVar ()
    , closedForInput :: IORef Bool

    -- | Doorbell to tell the driver that there is now space available in the
    -- 'inputQueue' and more items can be queued.
    , inputSpaceDoorBell :: MVar ()

    , readInputQ :: m [ChildEvent a]

    -- | Final output and exceptions, if any, queued by the fold and read by
    -- the fold driver.
    --
    -- [LOCKING] atomicModifyIORef. Output is queued infrequently by the fold
    -- and read frequently by the driver.
    , outputQueue :: IORef ([OutEvent b], Int)

    -- | Doorbell for the 'outputQueue', rung by the fold when the queue
    -- transitions from empty to non-empty.
    --
    -- [LOCKING] Infrequent, MVar.
    , outputDoorBell :: MVar ()

    -- cleanup: to track garbage collection of SVar --
    , svarRef :: Maybe (IORef ())

    -- Stats --
    , svarStats :: SVarStats

    -- Diagnostics --
    , svarInspectMode :: Bool
    , svarCreator :: ThreadId
    }

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

-- | An abstract type for specifying the configuration parameters of a
-- 'Channel'. Use @Config -> Config@ modifier functions to modify the default
-- configuration. See the individual modifier documentation for default values.
--
data Config = Config
    {
      _bufferHigh :: Limit
    , _inspect    :: Bool
    , _bound :: Bool
    }

-------------------------------------------------------------------------------
-- State defaults and reset
-------------------------------------------------------------------------------

defaultMaxBuffer :: Limit
defaultMaxBuffer = Limited magicMaxBuffer

-- | The fields prefixed by an _ are not to be accessed or updated directly but
-- via smart accessor APIs. Use get/set routines instead of directly accessing
-- the Config fields
defaultConfig :: Config
defaultConfig = Config
    {
      _bufferHigh = defaultMaxBuffer
    , _inspect = False
    , _bound = False
    }

-------------------------------------------------------------------------------
-- Smart get/set routines for State
-------------------------------------------------------------------------------

-- | Specify the maximum size of the buffer for storing the results from
-- concurrent computations. If the buffer becomes full we stop spawning more
-- concurrent tasks until there is space in the buffer.
-- A value of 0 resets the buffer size to default, a negative value means
-- there is no limit. The default value is 1500.
--
-- CAUTION! using an unbounded 'maxBuffer' value (i.e. a negative value)
-- coupled with an unbounded 'maxThreads' value is a recipe for disaster in
-- presence of infinite streams, or very large streams.  Especially, it must
-- not be used when 'pure' is used in 'ZipAsyncM' streams as 'pure' in
-- applicative zip streams generates an infinite stream causing unbounded
-- concurrent generation with no limit on the buffer or threads.
--
maxBuffer :: Int -> Config -> Config
maxBuffer n st =
    st { _bufferHigh =
            if n < 0
            then Unlimited
            else if n == 0
                 then defaultMaxBuffer
                 else Limited (fromIntegral n)
       }

getMaxBuffer :: Config -> Limit
getMaxBuffer = _bufferHigh

-- | Print debug information about the 'Channel' when the stream ends. When the
-- stream does not end normally, the channel debug information is printed when
-- the channel is garbage collected. If you are expecting but not seeing the
-- debug info try adding a 'performMajorGC' before the program ends.
--
inspect :: Bool -> Config -> Config
inspect flag st = st { _inspect = flag }

getInspectMode :: Config -> Bool
getInspectMode = _inspect

-- | Spawn bound threads (i.e., spawn threads using 'forkOS' instead of
-- 'forkIO'). The default value is 'False'.
--
-- Currently, this only takes effect only for concurrent folds.
boundThreads :: Bool -> Config -> Config
boundThreads flag st = st { _bound = flag }

getBound :: Config -> Bool
getBound = _bound

-------------------------------------------------------------------------------
-- Inspection
-------------------------------------------------------------------------------

-- | Dump the channel stats for diagnostics. Used when 'inspect' option is
-- enabled.
{-# NOINLINE dumpChannel #-}
dumpChannel :: Channel m a b -> IO String
dumpChannel sv = do
    xs <- sequence $ intersperse (return "\n")
        [ return (dumpCreator (svarCreator sv))
        , return "---------CURRENT STATE-----------"
        , dumpOutputQ (inputQueue sv)
        -- XXX print the types of events in the outputQueue, first 5
        , dumpDoorBell (inputItemDoorBell sv)
        , return "---------STATS-----------\n"
        , dumpSVarStats (svarInspectMode sv) Nothing (svarStats sv)
        ]
    return $ concat xs

-------------------------------------------------------------------------------
-- Support for running folds concurrently
-------------------------------------------------------------------------------

-- $concurrentFolds
--
-- To run folds concurrently, we need to decouple the fold execution from the
-- stream production. We use the SVar to do that, we have a single worker
-- pushing the stream elements to the SVar and on the consumer side a fold
-- driver pulls the values and folds them.
--
-- @
--
-- Fold worker <------Channel<------Fold driver
--     |  exceptions  |
--     --------------->
--
-- @
--
-- We need a channel for pushing exceptions from the fold worker to the fold
-- driver. The stream may be pushed to multiple folds at the same time. For
-- that we need one Channel per fold:
--
-- @
--
-- Fold worker <------Channel--
--                    |        |
-- Fold worker <------Channel------Driver
--                    |        |
-- Fold worker <------Channel--
--
-- @
--
-- Note: If the stream pusher terminates due to an exception, we do not
-- actively terminate the fold. It gets cleaned up by the GC.

-------------------------------------------------------------------------------
-- Process events received by a fold worker from a fold driver
-------------------------------------------------------------------------------

sendToDriver :: Channel m a b -> OutEvent b -> IO Int
sendToDriver sv msg = do
    -- In case the producer stream is blocked on pushing to the fold buffer
    -- then wake it up so that it can check for the stop event or exception
    -- being sent to it otherwise we will be deadlocked.
    -- void $ tryPutMVar (pushBufferMVar sv) ()
    sendEvent (outputQueue sv)
                     (outputDoorBell sv) msg

sendYieldToDriver :: MonadIO m => Channel m a b -> b -> m ()
sendYieldToDriver sv res = liftIO $ do
    tid <- myThreadId
    void $ sendToDriver sv (FoldDone tid res)

sendPartialToDriver :: MonadIO m => Channel m a b -> b -> m ()
sendPartialToDriver sv res = liftIO $ do
    void $ sendToDriver sv (FoldPartial res)

sendEOFToDriver :: MonadIO m => Channel m a b -> m ()
sendEOFToDriver sv = liftIO $ do
    tid <- myThreadId
    void $ sendToDriver sv (FoldEOF tid)

{-# NOINLINE sendExceptionToDriver #-}
sendExceptionToDriver :: Channel m a b -> SomeException -> IO ()
sendExceptionToDriver sv e = do
    tid <- myThreadId
    void $ sendToDriver sv (FoldException tid e)

data FromSVarState m a b =
      FromSVarRead (Channel m a b)
    | FromSVarLoop (Channel m a b) [ChildEvent a]

{-# INLINE_NORMAL fromInputQueue #-}
fromInputQueue :: MonadIO m => Channel m a b -> D.Stream m a
fromInputQueue svar = D.Stream step (FromSVarRead svar)

    where

    {-# INLINE_LATE step #-}
    step _ (FromSVarRead sv) = do
        list <- readInputQ sv
        -- Reversing the output is important to guarantee that we process the
        -- outputs in the same order as they were generated by the constituent
        -- streams.
        return $ D.Skip $ FromSVarLoop sv (Prelude.reverse list)

    step _ (FromSVarLoop sv []) = return $ D.Skip $ FromSVarRead sv
    step _ (FromSVarLoop sv (ev : es)) = do
        case ev of
            -- XXX Separate input and output events. Input events cannot have
            -- Stop event and output events cannot have ChildStopChannel
            -- event.
            ChildYield a -> return $ D.Yield a (FromSVarLoop sv es)
            ChildStopChannel -> return D.Stop
            _ -> undefined

{-# INLINE readInputQChan #-}
readInputQChan :: Channel m a b -> IO ([ChildEvent a], Int)
readInputQChan chan = do
    let ss = if svarInspectMode chan then Just (svarStats chan) else Nothing
    r@(_, n) <- readOutputQRaw (inputQueue chan) ss
    if n <= 0
    then do
        liftIO
            $ withDiagMVar
                (svarInspectMode chan)
                (dumpChannel chan)
                "readInputQChan: nothing to do"
            $ takeMVar (inputItemDoorBell chan)
        readOutputQRaw (inputQueue chan) ss
    else return r

{-# INLINE readInputQWithDB #-}
readInputQWithDB :: Channel m a b -> IO ([ChildEvent a], Int)
readInputQWithDB chan = do
    r <- readInputQChan chan
    -- XXX We can do this only if needed, if someone sleeps because of buffer
    -- then they can set a flag and we ring the doorbell only if the flag is
    -- set. Like we do in sendWorkerWait for streams.
    _ <- tryPutMVar (inputSpaceDoorBell chan) ()
    return r

mkNewChannelWith :: forall m a b. MonadIO m =>
       IORef ([OutEvent b], Int)
    -> MVar ()
    -> Config
    -> IO (Channel m a b)
mkNewChannelWith outQRev outQMvRev cfg = do
    outQ <- newIORef ([], 0)
    outQMv <- newEmptyMVar
    bufferMv <- newEmptyMVar
    ref <- newIORef False

    stats <- newSVarStats
    tid <- myThreadId

    let getSVar :: Channel m a b -> Channel m a b
        getSVar sv = Channel
            { inputQueue      = outQ
            , inputItemDoorBell   = outQMv
            , outputQueue = outQRev
            , outputDoorBell = outQMvRev
            , inputSpaceDoorBell = bufferMv
            , closedForInput = ref
            , maxInputBuffer   = getMaxBuffer cfg
            , readInputQ      = liftIO $ fmap fst (readInputQWithDB sv)
            , svarRef          = Nothing
            , svarInspectMode  = getInspectMode cfg
            , svarCreator      = tid
            , svarStats        = stats
            }

    let sv = getSVar sv in return sv

{-# INLINABLE newChannelWith #-}
{-# SPECIALIZE newChannelWith ::
       IORef ([OutEvent b], Int)
    -> MVar ()
    -> (Config -> Config)
    -> Fold IO a b
    -> IO (Channel IO a b, ThreadId) #-}
newChannelWith :: (MonadRunInIO m) =>
       IORef ([OutEvent b], Int)
    -> MVar ()
    -> (Config -> Config)
    -> Fold m a b
    -> m (Channel m a b, ThreadId)
newChannelWith outq outqDBell modifier f = do
    let config = modifier defaultConfig
    sv <- liftIO $ mkNewChannelWith outq outqDBell config
    mrun <- askRunInIO
    tid <- doForkWith
        (getBound config) (work sv) mrun (sendExceptionToDriver sv)
    return (sv, tid)

    where

    {-# NOINLINE work #-}
    work chan =
        let f1 = Fold.rmapM (void . sendYieldToDriver chan) f
         in D.fold f1 $ fromInputQueue chan

-- | Returns True if the fold terminated due to completion and False when due
-- to end-of-stream.
{-# INLINE scanToChannel #-}
scanToChannel :: MonadIO m => Channel m a b -> Scanl m a b -> Fold m a Bool
scanToChannel chan (Scanl step initial extract final) =
    Fold step1 initial1 extract1 final1

    where

    initial1 = do
        r <- initial
        case r of
            Fold.Partial s -> do
                b <- extract s
                void $ sendPartialToDriver chan b
                return $ Fold.Partial s
            Fold.Done b -> do
                sendYieldToDriver chan b
                return $ Fold.Done True

    step1 st x = do
        r <- step st x
        case r of
            Fold.Partial s -> do
                b <- extract s
                void $ sendPartialToDriver chan b
                return $ Fold.Partial s
            Fold.Done b -> do
                sendYieldToDriver chan b
                return $ Fold.Done True

    extract1 _ = error "extract: not supported by folds"

    -- XXX Should we not discard the result?
    final1 st = do
        void (final st)
        return False

{-# INLINABLE newChannelWithScan #-}
{-# SPECIALIZE newChannelWithScan ::
       IORef ([OutEvent b], Int)
    -> MVar ()
    -> (Config -> Config)
    -> Scanl IO a b
    -> IO (Channel IO a b, ThreadId) #-}
newChannelWithScan :: (MonadRunInIO m) =>
       IORef ([OutEvent b], Int)
    -> MVar ()
    -> (Config -> Config)
    -> Scanl m a b
    -> m (Channel m a b, ThreadId)
newChannelWithScan outq outqDBell modifier f = do
    let config = modifier defaultConfig
    sv <- liftIO $ mkNewChannelWith outq outqDBell config
    mrun <- askRunInIO
    tid <- doForkWith
        (getBound config) (work sv) mrun (sendExceptionToDriver sv)
    return (sv, tid)

    where

    {-# NOINLINE work #-}
    work chan = do
        completed <- D.fold (scanToChannel chan f) (fromInputQueue chan)
        -- We check for only one item in the outputqueue, for example in
        -- parTeeWith, multiple messages can make that complicated. Therefore,
        -- we first check if we already sent a FoldDone.
        when (not completed) $ sendEOFToDriver chan
        liftIO $ writeIORef (closedForInput chan) True
        liftIO writeBarrier
        void $ liftIO $ tryPutMVar (inputSpaceDoorBell chan) ()

{-# INLINABLE newChannel #-}
{-# SPECIALIZE newChannel ::
    (Config -> Config) -> Fold IO a b -> IO (Channel IO a b) #-}
newChannel :: (MonadRunInIO m) =>
    (Config -> Config) -> Fold m a b -> m (Channel m a b)
newChannel modifier f = do
    outQRev <- liftIO $ newIORef ([], 0)
    outQMvRev <- liftIO newEmptyMVar
    fmap fst (newChannelWith outQRev outQMvRev modifier f)

{-# INLINABLE newScanChannel #-}
{-# SPECIALIZE newScanChannel ::
    (Config -> Config) -> Scanl IO a b -> IO (Channel IO a b) #-}
newScanChannel :: (MonadRunInIO m) =>
    (Config -> Config) -> Scanl m a b -> m (Channel m a b)
newScanChannel modifier f = do
    outQRev <- liftIO $ newIORef ([], 0)
    outQMvRev <- liftIO newEmptyMVar
    fmap fst (newChannelWithScan outQRev outQMvRev modifier f)

-------------------------------------------------------------------------------
-- Process events received by the driver thread from the fold worker side
-------------------------------------------------------------------------------

-- XXX currently only one event is sent by a fold consumer to the stream
-- producer. But we can potentially have multiple events e.g. the fold step can
-- generate exception more than once and the producer can ignore those
-- exceptions or handle them and still keep driving the fold.

-- XXX In case of scan this could be a stream.

-- | Poll for events sent by the fold worker to the fold driver. The fold
-- consumer can send a "Stop" event or an exception. When a "Stop" is received
-- this function returns 'True'. If an exception is recieved then it throws the
-- exception.
--
{-# NOINLINE checkFoldStatus #-}
checkFoldStatus :: MonadAsync m => Channel m a b -> m (Maybe b)
checkFoldStatus sv = do
    (list, _) <- liftIO $ readOutputQBasic (outputQueue sv)
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    processEvents $ reverse list

    where

    {-# INLINE processEvents #-}
    processEvents [] = return Nothing
    processEvents (ev : _) = do
        case ev of
            FoldException _ e -> throwM e
            FoldDone _ b -> return (Just b)
            FoldPartial _ ->
                error "checkFoldStatus: FoldPartial can occur only for scans"
            FoldEOF _ ->
                error "checkFoldStatus: FoldEOF can occur only for scans"

{-# INLINE isBufferAvailable #-}
isBufferAvailable :: MonadIO m => Channel m a b -> m Bool
isBufferAvailable sv = do
    let limit = maxInputBuffer sv
    case limit of
        Unlimited -> return True
        Limited lim -> do
            (_, n) <- liftIO $ readIORef (inputQueue sv)
            return $ fromIntegral lim > n

-- | Push values from a driver to a fold worker via a Channel. Blocks if no
-- space is available in the buffer. Before pushing a value to the Channel it
-- polls for events received from the fold worker.  If a stop event is received
-- then it returns 'True' otherwise false.  Propagates exceptions received from
-- the fold worker.
--
{-# INLINE sendToWorker #-}
sendToWorker :: MonadAsync m => Channel m a b -> a -> m (Maybe b)
sendToWorker chan a = go

    where

    -- Recursive function, should we use SPEC?
    go = do
        let qref = outputQueue chan
        status <- do
            (_, n) <- liftIO $ readIORef qref
            if n > 0
            then checkFoldStatus chan
            else return Nothing
        case status of
            Just _ -> return status
            Nothing -> do
                    r <- isBufferAvailable chan
                    if r
                    then do
                        liftIO
                            $ void
                            $ sendEvent
                                (inputQueue chan)
                                (inputItemDoorBell chan)
                                (ChildYield a)
                        return Nothing
                    else do
                        () <- liftIO $ takeMVar (inputSpaceDoorBell chan)
                        go

-- | Like sendToWorker but only sends, does not receive any events from the
-- fold.
{-# INLINE sendToWorker_ #-}
sendToWorker_ :: MonadAsync m => Channel m a b -> a -> m ()
sendToWorker_ chan a = go

    where

    -- Recursive function, should we use SPEC?
    go = do
        r <- isBufferAvailable chan
        if r
        then do
            liftIO
                $ void
                $ sendEvent
                    (inputQueue chan)
                    (inputItemDoorBell chan)
                    (ChildYield a)
        else do
            -- Block for space
            () <- liftIO $ takeMVar (inputSpaceDoorBell chan)
            closed <- liftIO $ readIORef (closedForInput chan)
            when (not closed) go

-- XXX Cleanup the fold if the stream is interrupted. Add a GC hook.

cleanup :: MonadIO m => Channel m a b -> m ()
cleanup chan = do
    when (svarInspectMode chan) $ liftIO $ do
        t <- getTime Monotonic
        writeIORef (svarStopTime (svarStats chan)) (Just t)
        printSVar (dumpChannel chan) "Scan channel done"

finalize :: MonadIO m => Channel m a b -> m ()
finalize chan = do
    liftIO $ void
        $ sendEvent
            (inputQueue chan)
            (inputItemDoorBell chan)
            ChildStopChannel
