{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.Async
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.Async
    (
      AsyncT
    , Async
    , asyncly
    , async
    , (<|)             --deprecated
    , mkAsync
    , mkAsync'

    , WAsyncT
    , WAsync
    , wAsyncly
    , wAsync
    )
where

import Control.Monad (ap)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Concurrent.MVar (newEmptyMVar)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, nullQ, tryPopR)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup(..))

import Prelude hiding (map)
import qualified Data.Set as S

import Streamly.Streams.SVar (fromSVar)
import Streamly.Streams.Serial (map)
import Streamly.SVar
import Streamly.Streams.StreamK (IsStream(..), Stream(..), adapt)
import qualified Streamly.Streams.StreamK as K

#include "Instances.hs"

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------

{-# INLINE workLoopLIFO #-}
workLoopLIFO
    :: MonadIO m
    => IORef [Stream m a]
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> m ()
workLoopLIFO q st sv winfo = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> liftIO $ sendStop sv winfo
            Just m -> unStream m st run single yieldk

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        if res then run else liftIO $ sendStop sv winfo

    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        if res
        then unStream r st run single yieldk
        else liftIO $ do
            enqueueLIFO sv q r
            sendStop sv winfo

    dequeue = liftIO $ atomicModifyIORefCAS q $ \case
                [] -> ([], Nothing)
                x : xs -> (xs, Just x)

-- We duplicate workLoop for yield limit and no limit cases because it has
-- around 40% performance overhead in the worst case.
--
-- XXX we can pass yinfo directly as an argument here so that we do not have to
-- make a check every time.
{-# INLINE workLoopLIFOLimited #-}
workLoopLIFOLimited
    :: MonadIO m
    => IORef [Stream m a]
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> m ()
workLoopLIFOLimited q st sv winfo = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> liftIO $ sendStop sv winfo
            Just m -> do
                -- XXX This is just a best effort minimization of concurrency
                -- to the yield limit. If the stream is made of concurrent
                -- streams we do not reserve the yield limit in the constituent
                -- streams before executing the action. This can be done
                -- though, by sharing the yield limit ref with downstream
                -- actions via state passing. Just a todo.
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                if yieldLimitOk
                then do
                -- XXX increment is not safe when the yield limit was already 0
                -- and therefore was not decremented. We need yield limit to be
                -- int and negative if we want this to work.
                    let stop = liftIO (incrementYieldLimit sv) >> run
                    unStream m st stop single yieldk
                -- Avoid any side effects, undo the yield limit decrement if we
                -- never yielded anything.
                else liftIO $ do
                    incrementYieldLimit sv
                    sendStop sv winfo

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        if res then run else liftIO $ sendStop sv winfo

    -- XXX can we pass on the yield limit downstream to limit the concurrency
    -- of constituent streams.
    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        let stop = liftIO (incrementYieldLimit sv) >> run
        if res && yieldLimitOk
        then unStream r st stop single yieldk
        else liftIO $ do
            incrementYieldLimit sv
            enqueueLIFO sv q r
            sendStop sv winfo

    dequeue = liftIO $ atomicModifyIORefCAS q $ \case
                [] -> ([], Nothing)
                x : xs -> (xs, Just x)

-------------------------------------------------------------------------------
-- WAsync
-------------------------------------------------------------------------------

{-# INLINE workLoopFIFO #-}
workLoopFIFO
    :: MonadIO m
    => LinkedQueue (Stream m a)
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> m ()
workLoopFIFO q st sv winfo = run

    where

    run = do
        work <- liftIO $ tryPopR q
        case work of
            Nothing -> liftIO $ sendStop sv winfo
            Just m -> unStream m st run single yieldk

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        if res then run else liftIO $ sendStop sv winfo

    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        if res
        then unStream r st run single yieldk
        else liftIO $ do
            enqueueFIFO sv q r
            sendStop sv winfo

{-# INLINE workLoopFIFOLimited #-}
workLoopFIFOLimited
    :: MonadIO m
    => LinkedQueue (Stream m a)
    -> State Stream m a
    -> SVar Stream m a
    -> WorkerInfo
    -> m ()
workLoopFIFOLimited q st sv winfo = run

    where

    run = do
        work <- liftIO $ tryPopR q
        case work of
            Nothing -> liftIO $ sendStop sv winfo
            Just m -> do
                yieldLimitOk <- liftIO $ decrementYieldLimit sv
                if yieldLimitOk
                then do
                    let stop = liftIO (incrementYieldLimit sv) >> run
                    unStream m st stop single yieldk
                else liftIO $ do
                    incrementYieldLimit sv
                    sendStop sv winfo

    single a = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        if res then run else liftIO $ sendStop sv winfo

    yieldk a r = do
        res <- liftIO $ sendYield sv winfo (ChildYield a)
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        let stop = liftIO (incrementYieldLimit sv) >> run
        if res && yieldLimitOk
        then unStream r st stop single yieldk
        else liftIO $ do
            incrementYieldLimit sv
            enqueueFIFO sv q r
            sendStop sv winfo

-------------------------------------------------------------------------------
-- SVar creation
-- This code belongs in SVar.hs but is kept here for perf reasons
-------------------------------------------------------------------------------

-- XXX we have this function in this file because passing runStreamLIFO as a
-- function argument to this function results in a perf degradation of more
-- than 10%.  Need to investigate what the root cause is.
-- Interestingly, the same thing does not make any difference for Ahead.
getLifoSVar :: MonadAsync m => State Stream m a -> IO (SVar Stream m a)
getLifoSVar st = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q       <- newIORef []
    yl      <- case getYieldLimit st of
                Nothing -> return Nothing
                Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st

#ifdef DIAGNOSTICS
    disp   <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif
    let isWorkFinished _ = null <$> readIORef q

    let isWorkFinishedLimited sv = do
            yieldsDone <-
                    case remainingYields sv of
                        Just ref -> do
                            n <- readIORef ref
                            return (n <= 0)
                        Nothing -> return False
            qEmpty <- null <$> readIORef q
            return $ qEmpty || yieldsDone

    let getSVar sv readOutput postProc workDone wloop = SVar
            { outputQueue      = outQ
            , remainingYields    = yl
            , maxBufferLimit   = getMaxBuffer st
            , maxWorkerLimit   = getMaxThreads st
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = wloop q st{streamVar = Just sv} sv
            , enqueue          = enqueueLIFO sv q
            , isWorkDone       = workDone sv
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

    let sv =
            case getMaxStreamRate st of
                Nothing ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinished
                                              workLoopLIFO
                        Just _  -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
                Just _  ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinished
                                              workLoopLIFO
                        Just _  -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinishedLimited
                                              workLoopLIFOLimited
     in return sv

getFifoSVar :: MonadAsync m => State Stream m a -> IO (SVar Stream m a)
getFifoSVar st = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q       <- newQ
    yl      <- case getYieldLimit st of
                Nothing -> return Nothing
                Just x -> Just <$> newIORef x
    rateInfo <- getYieldRateInfo st

#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif

    let isWorkFinished _ = nullQ q
    let isWorkFinishedLimited sv = do
            yieldsDone <-
                    case remainingYields sv of
                        Just ref -> do
                            n <- readIORef ref
                            return (n <= 0)
                        Nothing -> return False
            qEmpty <- nullQ q
            return $ qEmpty || yieldsDone

    let getSVar sv readOutput postProc workDone wloop = SVar
            { outputQueue      = outQ
            , remainingYields  = yl
            , maxBufferLimit   = getMaxBuffer st
            , maxWorkerLimit   = getMaxThreads st
            , yieldRateInfo    = rateInfo
            , outputDoorBell   = outQMv
            , readOutputQ      = readOutput sv
            , postProcess      = postProc sv
            , workerThreads    = running
            , workLoop         = wloop q st{streamVar = Just sv} sv
            , enqueue          = enqueueFIFO sv q
            , isWorkDone       = workDone sv
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

    let sv =
            case getMaxStreamRate st of
                Nothing ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinished
                                              workLoopFIFO
                        Just _  -> getSVar sv readOutputQBounded
                                              postProcessBounded
                                              isWorkFinishedLimited
                                              workLoopFIFOLimited
                Just _  ->
                    case getYieldLimit st of
                        Nothing -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinished
                                              workLoopFIFO
                        Just _  -> getSVar sv readOutputQPaced
                                              postProcessPaced
                                              isWorkFinishedLimited
                                              workLoopFIFOLimited
     in return sv

{-# INLINABLE newAsyncVar #-}
newAsyncVar :: MonadAsync m
    => State Stream m a -> Stream m a -> m (SVar Stream m a)
newAsyncVar st m = do
    sv <- liftIO $ getLifoSVar st
    sendFirstWorker sv m

-- XXX Get rid of this?
-- | Make a stream asynchronous, triggers the computation and returns a stream
-- in the underlying monad representing the output generated by the original
-- computation. The returned action is exhaustible and must be drained once. If
-- not drained fully we may have a thread blocked forever and once exhausted it
-- will always return 'empty'.
--
-- @since 0.2.0
{-# INLINABLE mkAsync #-}
mkAsync :: (IsStream t, MonadAsync m) => t m a -> m (t m a)
mkAsync m = newAsyncVar defState (toStream m) >>= return . fromSVar

{-# INLINABLE mkAsync' #-}
mkAsync' :: (IsStream t, MonadAsync m) => State Stream m a -> t m a -> m (t m a)
mkAsync' st m = newAsyncVar st (toStream m) >>= return . fromSVar

-- | Create a new SVar and enqueue one stream computation on it.
{-# INLINABLE newWAsyncVar #-}
newWAsyncVar :: MonadAsync m
    => State Stream m a -> Stream m a -> m (SVar Stream m a)
newWAsyncVar st m = do
    sv <- liftIO $ getFifoSVar st
    sendFirstWorker sv m

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

forkSVarAsync :: MonadAsync m
    => SVarStyle -> Stream m a -> Stream m a -> Stream m a
forkSVarAsync style m1 m2 = Stream $ \st stp sng yld -> do
    sv <- case style of
        AsyncVar -> newAsyncVar st (concurrently m1 m2)
        WAsyncVar -> newWAsyncVar st (concurrently m1 m2)
        _ -> error "illegal svar type"
    unStream (fromSVar sv) (rstState st) stp sng yld
    where
    concurrently ma mb = Stream $ \st stp sng yld -> do
        liftIO $ enqueue (fromJust $ streamVar st) mb
        unStream ma st stp sng yld

{-# INLINE joinStreamVarAsync #-}
joinStreamVarAsync :: MonadAsync m
    => SVarStyle -> Stream m a -> Stream m a -> Stream m a
joinStreamVarAsync style m1 m2 = Stream $ \st stp sng yld -> do
    case streamVar st of
        Just sv | svarStyle sv == style ->
            liftIO (enqueue sv m2) >> unStream m1 st stp sng yld
        _ -> unStream (forkSVarAsync style m1 m2) st stp sng yld

------------------------------------------------------------------------------
-- Semigroup and Monoid style compositions for parallel actions
------------------------------------------------------------------------------

{-# INLINE asyncS #-}
asyncS :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
asyncS = joinStreamVarAsync AsyncVar

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'AsyncT'.
-- Merges two streams possibly concurrently, preferring the
-- elements from the left one when available.
--
-- @since 0.2.0
{-# INLINE async #-}
async :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
async m1 m2 = fromStream $
    joinStreamVarAsync AsyncVar (toStream m1) (toStream m2)

-- | Same as 'async'.
--
-- @since 0.1.0
{-# DEPRECATED (<|) "Please use 'async' instead." #-}
{-# INLINE (<|) #-}
(<|) :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
(<|) = async

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using async.
{-# INLINE consMAsync #-}
consMAsync :: MonadAsync m => m a -> Stream m a -> Stream m a
consMAsync m r = K.yieldM m `asyncS` r

------------------------------------------------------------------------------
-- AsyncT
------------------------------------------------------------------------------

-- | Deep async composition or async composition with depth first traversal. In
-- a left to right 'Semigroup' composition it tries to yield elements from the
-- left stream as long as it can, but it can run the right stream in parallel
-- if it needs to, based on demand. The right stream can be run if the left
-- stream blocks on IO or cannot produce elements fast enough for the consumer.
--
-- @
-- main = ('toList' . 'asyncly' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the monad instance of 'AsyncT' /may/ run each iteration
-- concurrently based on demand.  More concurrent iterations are started only
-- if the previous iterations are not able to produce enough output for the
-- consumer.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'runStream' . 'asyncly' $ do
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
-- Note that async composition with depth first traversal can be used to
-- combine infinite number of streams as it explores only a bounded number of
-- streams at a time.
--
-- @since 0.1.0
newtype AsyncT m a = AsyncT {getAsyncT :: Stream m a}
    deriving (MonadTrans)

-- | A demand driven left biased parallely composing IO stream of elements of
-- type @a@.  See 'AsyncT' documentation for more details.
--
-- @since 0.2.0
type Async a = AsyncT IO a

-- | Fix the type of a polymorphic stream as 'AsyncT'.
--
-- @since 0.1.0
asyncly :: IsStream t => AsyncT m a -> t m a
asyncly = adapt

instance IsStream AsyncT where
    toStream = getAsyncT
    fromStream = AsyncT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> AsyncT IO a -> AsyncT IO a #-}
    consM m r = fromStream $ consMAsync m (toStream r)

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> AsyncT IO a -> AsyncT IO a #-}
    (|:) = consM

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

instance MonadAsync m => Semigroup (AsyncT m a) where
    (<>) = async

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (AsyncT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (AsyncT m) where
    return = pure
    (AsyncT m) >>= f = AsyncT $ K.bindWith asyncS m (getAsyncT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(AsyncT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(AsyncT, MONADPARALLEL)

------------------------------------------------------------------------------
-- WAsyncT
------------------------------------------------------------------------------

{-# INLINE wAsyncS #-}
wAsyncS :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
wAsyncS = joinStreamVarAsync WAsyncVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using wAsync.
{-# INLINE consMWAsync #-}
consMWAsync :: MonadAsync m => m a -> Stream m a -> Stream m a
consMWAsync m r = K.yieldM m `wAsyncS` r

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'WAsyncT'.
-- Merges two streams concurrently choosing elements from both fairly.
--
-- @since 0.2.0
{-# INLINE wAsync #-}
wAsync :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
wAsync m1 m2 = fromStream $ wAsyncS (toStream m1) (toStream m2)

-- | Wide async composition or async composition with breadth first traversal.
-- The Semigroup instance of 'WAsyncT' concurrently /traverses/ the composed
-- streams using a depth first travesal or in a round robin fashion, yielding
-- elements from both streams alternately.
--
-- @
-- main = ('toList' . 'wAsyncly' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the 'Monad' instance of 'WAsyncT' runs /all/ iterations fairly
-- concurrently using a round robin scheduling.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'runStream' . 'wAsyncly' $ do
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
-- Unlike 'AsyncT' all iterations are guaranteed to run fairly
-- concurrently, unconditionally.
--
-- Note that async composition with breadth first traversal can only combine a
-- finite number of streams as it needs to retain state for each unfinished
-- stream.
--
-- @since 0.2.0
newtype WAsyncT m a = WAsyncT {getWAsyncT :: Stream m a}
    deriving (MonadTrans)

-- | A round robin parallely composing IO stream of elements of type @a@.
-- See 'WAsyncT' documentation for more details.
--
-- @since 0.2.0
type WAsync a = WAsyncT IO a

-- | Fix the type of a polymorphic stream as 'WAsyncT'.
--
-- @since 0.2.0
wAsyncly :: IsStream t => WAsyncT m a -> t m a
wAsyncly = adapt

instance IsStream WAsyncT where
    toStream = getWAsyncT
    fromStream = WAsyncT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> WAsyncT IO a -> WAsyncT IO a #-}
    consM m r = fromStream $ consMWAsync m (toStream r)

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> WAsyncT IO a -> WAsyncT IO a #-}
    (|:) = consM

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

instance MonadAsync m => Semigroup (WAsyncT m a) where
    (<>) = wAsync

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (WAsyncT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (WAsyncT m) where
    return = pure
    (WAsyncT m) >>= f =
        WAsyncT $ K.bindWith wAsyncS m (getWAsyncT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(WAsyncT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(WAsyncT, MONADPARALLEL)
