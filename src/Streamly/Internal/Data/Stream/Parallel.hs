{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Parallel
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Control.Concurrent (threadDelay)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}
--
module Streamly.Internal.Data.Stream.Parallel {-# DEPRECATED "Please use \"Streamly.Internal.Data.Stream.Concurrent\" instead." #-}
    (
    -- * Parallel Stream Type
      ParallelT(..)
    , Parallel
    , consM

    -- * Merge Concurrently
    , parallelK
    , parallelFstK
    , parallelMinK

    -- * Evaluate Concurrently
    , mkParallelD
    , mkParallelK

    -- * Tap Concurrently
    , tapAsyncK
    , tapAsyncF

    -- * Callbacks
    , newCallbackStream
    )
where

import Control.Concurrent (myThreadId, takeMVar)
import Control.Monad (when)
#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
#endif
import Control.Monad.Catch (MonadThrow, throwM)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.Class (MonadTrans(lift))
#endif
import Data.Functor (void)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromJust)

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream (Step(..))

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.StreamK as K
    (StreamK, foldStreamShared, mkStream, foldStream, fromEffect
    , nil, concatMapWith, fromPure, bindWith)
import qualified Streamly.Internal.Data.Stream as D
    (Stream(..), mapM, toStreamK, fromStreamK)
import qualified Streamly.Internal.Data.Stream.SVar.Generate as SVar
import qualified Streamly.Internal.Data.Stream.SVar.Eliminate as SVar
import qualified Streamly.Internal.Data.Stream.Serial as Stream

import Streamly.Internal.Data.SVar
import Prelude hiding (map)

#include "inline.hs"
#include "Instances.hs"

--
-- $setup
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Control.Concurrent (threadDelay)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

{-# INLINABLE withLocal #-}
withLocal :: MonadReader r m => (r -> r) -> K.StreamK m a -> K.StreamK m a
withLocal f m =
    K.mkStream $ \st yld sng stp ->
        let single = local f . sng
            yieldk a r = local f $ yld a (withLocal f r)
        in K.foldStream st yieldk single (local f stp) m

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- StreamK based worker routines
-------------------------------------------------------------------------------

{-# NOINLINE runOne #-}
runOne
    :: MonadIO m
    => State K.StreamK m a -> K.StreamK m a -> Maybe WorkerInfo -> m ()
runOne st m0 winfo =
    case getYieldLimit st of
        Nothing -> go m0
        Just _  -> runOneLimited st m0 winfo

    where

    go m = do
        liftIO $ decrementBufferLimit sv
        K.foldStreamShared st yieldk single stop m

    sv = fromJust $ streamVar st

    stop = liftIO $ do
        incrementBufferLimit sv
        sendStop sv winfo
    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> liftIO (sendStop sv winfo)
    yieldk a r = sendit a >> go r

runOneLimited
    :: MonadIO m
    => State K.StreamK m a -> K.StreamK m a -> Maybe WorkerInfo -> m ()
runOneLimited st m0 winfo = go m0

    where

    go m = do
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if yieldLimitOk
        then do
            liftIO $ decrementBufferLimit sv
            K.foldStreamShared st yieldk single stop m
        else do
            liftIO $ cleanupSVarFromWorker sv
            liftIO $ sendStop sv winfo

    sv = fromJust $ streamVar st

    stop = liftIO $ do
        incrementBufferLimit sv
        incrementYieldLimit sv
        sendStop sv winfo
    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> liftIO (sendStop sv winfo)
    yieldk a r = sendit a >> go r

-------------------------------------------------------------------------------
-- Consing and appending a stream in parallel style
-------------------------------------------------------------------------------

-- Note that consing and appending requires StreamK as it would not scale well
-- with StreamD unless we are only consing a very small number of streams or
-- elements in a stream. StreamK allows us to manipulate control flow in a way
-- which StreamD cannot allow. StreamK can make a jump without having to
-- remember the past state.

{-# NOINLINE forkSVarPar #-}
forkSVarPar :: MonadAsync m
    => SVarStopStyle -> K.StreamK m a -> K.StreamK m a -> K.StreamK m a
forkSVarPar ss m r = K.mkStream $ \st yld sng stp -> do
    sv <- newParallelVar ss st
    pushWorkerPar sv (runOne st{streamVar = Just sv} m)
    case ss of
        StopBy -> liftIO $ do
            set <- readIORef (workerThreads sv)
            writeIORef (svarStopBy sv) $ Set.elemAt 0 set
        _ -> return ()
    pushWorkerPar sv (runOne st{streamVar = Just sv} r)
    K.foldStream st yld sng stp $ Stream.toStreamK (SVar.fromSVar sv)

{-# INLINE joinStreamVarPar #-}
joinStreamVarPar ::
       MonadAsync m
    => SVarStyle
    -> SVarStopStyle
    -> K.StreamK m a
    -> K.StreamK m a
    -> K.StreamK m a
joinStreamVarPar style ss m1 m2 = K.mkStream $ \st yld sng stp ->
    case streamVar st of
        Just sv | svarStyle sv == style && svarStopStyle sv == ss -> do
            -- Here, WE ARE IN THE WORKER/PRODUCER THREAD, we know that because
            -- the SVar exists. We are running under runOne and the output we
            -- produce ultimately will be sent to the SVar by runOne.
            --
            -- If we came here the worker/runOne is evaluating a `parallel`
            -- combinator. In this case, we always fork a new worker for the
            -- first component (m1) in the parallel composition and continue to
            -- evaluate the second component (m2) in the current worker thread.
            --
            -- When m1 is serially composed, the worker would evaluate it
            -- without any further forks and the resulting output is sent to
            -- the SVar and the evaluation terminates. If m1 is a `parallel`
            -- composition of two streams the worker would again recurses here.
            --
            -- Similarly, when m2 is serially composed it gets evaluated here
            -- and the resulting output is sent to the SVar by the runOne
            -- wrapper. When m2 is composed with `parallel` it will again
            -- recurse here and so on until it finally terminates.
            --
            -- When we create a right associated expression using `parallel`,
            -- then m1 would always terminate without further forks or
            -- recursion into this routine, therefore, the worker returns
            -- immediately after evaluating it. And m2 would continue to
            -- fork/recurse, therefore, the current thread always recurses and
            -- forks new workers one after the other.  This is a tail recursive
            -- style execution, m2, the recursive expression always executed at
            -- the tail.
            --
            -- When the expression is left associated, the worker spawned would
            -- get the forking/recursing responsibility and then again the
            -- worker spawned by that worker would fork, thus creating layer
            -- over layer of workers and a chain of threads leading to a very
            -- inefficient execution.
            pushWorkerPar sv (runOne st m1)
            K.foldStreamShared st yld sng stp m2
        _ ->
            -- Here WE ARE IN THE CONSUMER THREAD, we create a new SVar, fork
            -- worker threads to execute m1 and m2 and this thread starts
            -- pulling the stream from the SVar.
            K.foldStreamShared st yld sng stp (forkSVarPar ss m1 m2)

-------------------------------------------------------------------------------
-- User facing APIs
-------------------------------------------------------------------------------

{-# INLINE parallelK #-}
parallelK :: MonadAsync m => K.StreamK m a -> K.StreamK m a -> K.StreamK m a
parallelK = joinStreamVarPar ParallelVar StopNone

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using parallel.
{-# INLINE consM #-}
{-# SPECIALIZE consM :: IO a -> ParallelT IO a -> ParallelT IO a #-}
consM :: MonadAsync m => m a -> ParallelT m a -> ParallelT m a
consM m (ParallelT r) = ParallelT $ parallelK (K.fromEffect m) r

-- This is a co-parallel like combinator for streams, where first stream is the
-- main stream and the rest are just supporting it, when the first ends
-- everything ends.
--
-- | Like `parallel` but stops the output as soon as the first stream stops.
--
-- /Pre-release/
{-# INLINE parallelFstK #-}
parallelFstK :: MonadAsync m => K.StreamK m a -> K.StreamK m a -> K.StreamK m a
parallelFstK = joinStreamVarPar ParallelVar StopBy

-- This is a race like combinator for streams.
--
-- | Like `parallel` but stops the output as soon as any of the two streams
-- stops.
--
-- /Pre-release/
{-# INLINE parallelMinK #-}
parallelMinK :: MonadAsync m => K.StreamK m a -> K.StreamK m a -> K.StreamK m a
parallelMinK = joinStreamVarPar ParallelVar StopAny

------------------------------------------------------------------------------
-- Convert a stream to parallel
------------------------------------------------------------------------------

-- | Like 'mkParallel' but uses StreamK internally.
--
-- /Pre-release/
--
mkParallelK :: MonadAsync m => K.StreamK m a -> K.StreamK m a
mkParallelK m = K.mkStream $ \st yld sng stp -> do
    sv <- newParallelVar StopNone (adaptState st)
    -- pushWorkerPar sv (runOne st{streamVar = Just sv} $ toStream m)
    SVar.toSVarParallel st sv $ D.fromStreamK m
    K.foldStream st yld sng stp $ Stream.toStreamK $ SVar.fromSVar sv

-- | Same as 'mkParallel' but for StreamD stream.
--
{-# INLINE_NORMAL mkParallelD #-}
mkParallelD :: MonadAsync m => D.Stream m a -> D.Stream m a
mkParallelD m = D.Stream step Nothing
    where

    step gst Nothing = do
        sv <- newParallelVar StopNone gst
        SVar.toSVarParallel gst sv m
        -- XXX use unfold instead?
        return $ Skip $ Just $ SVar.fromSVarD sv

    step gst (Just (D.UnStream step1 st)) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (Just $ D.Stream step1 s)
            Skip s    -> Skip (Just $ D.Stream step1 s)
            Stop      -> Stop

-------------------------------------------------------------------------------
-- Concurrent tap
-------------------------------------------------------------------------------

-- NOTE: In regular pull style streams, the consumer stream is pulling elements
-- from the SVar and we have several workers producing elements and pushing to
-- SVar. In case of folds, we, the parent stream driving the fold, are the
-- stream producing worker, we start an SVar and start pushing to the SVar, the
-- fold on the other side of the SVar is the consumer stream.
--
-- In the pull stream case exceptions are propagated from the producing workers
-- to the consumer stream, the exceptions are propagated on the same channel as
-- the produced stream elements. However, in case of push style folds the
-- current stream itself is the worker and the fold is the consumer, in this
-- case we have to propagate the exceptions from the consumer to the producer.
-- This is reverse of the pull case and we need a reverse direction channel
-- to propagate the exception.
--
-- | Redirect a copy of the stream to a supplied fold and run it concurrently
-- in an independent thread. The fold may buffer some elements. The buffer size
-- is determined by the prevailing 'Streamly.Prelude.maxBuffer' setting.
--
-- @
--               StreamK m a -> m b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- @
-- > S.drain $ S.tapAsync (S.mapM_ print) (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Exceptions from the concurrently running fold are propagated to the current
-- computation.  Note that, because of buffering in the fold, exceptions may be
-- delayed and may not correspond to the current element being processed in the
-- parent stream, but we guarantee that before the parent stream stops the tap
-- finishes and all exceptions from it are drained.
--
--
-- Compare with 'tap'.
--
-- /Pre-release/
{-# INLINE tapAsyncK #-}
tapAsyncK ::
       MonadAsync m => (K.StreamK m a -> m b) -> K.StreamK m a -> K.StreamK m a
tapAsyncK f m = K.mkStream $ \st yld sng stp -> do
    sv <- SVar.newFoldSVar st (f . Stream.toStreamK)
    K.foldStreamShared st yld sng stp
        $ Stream.toStreamK (SVar.teeToSVar sv $ Stream.fromStreamK m)

data TapState fs st a = TapInit | Tapping !fs st | TapDone st

-- | Like 'tapAsync' but uses a 'Fold' instead of a fold function.
--
{-# INLINE_NORMAL tapAsyncF #-}
tapAsyncF :: MonadAsync m => Fold m a b -> D.Stream m a -> D.Stream m a
tapAsyncF f (D.Stream step1 state1) = D.Stream step TapInit
    where

    drainFold svr = do
            -- In general, a Stop event would come equipped with the result
            -- of the fold. It is not used here but it would be useful in
            -- applicative and distribute.
            done <- SVar.fromConsumer svr
            when (not done) $ do
                liftIO $ withDiagMVar svr "teeToSVar: waiting to drain"
                       $ takeMVar (outputDoorBellFromConsumer svr)
                drainFold svr

    stopFold svr = do
            liftIO $ sendStop svr Nothing
            -- drain/wait until a stop event arrives from the fold.
            drainFold svr

    {-# INLINE_LATE step #-}
    step gst TapInit = do
        sv <- SVar.newFoldSVarF gst f
        return $ Skip (Tapping sv state1)

    step gst (Tapping sv st) = do
        r <- step1 gst st
        case r of
            Yield a s ->  do
                done <- SVar.pushToFold sv a
                if done
                then do
                    -- XXX we do not need to wait synchronously here
                    stopFold sv
                    return $ Yield a (TapDone s)
                else return $ Yield a (Tapping sv s)
            Skip s -> return $ Skip (Tapping sv s)
            Stop -> do
                stopFold sv
                return Stop

    step gst (TapDone st) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (TapDone s)
            Skip s    -> Skip (TapDone s)
            Stop      -> Stop

------------------------------------------------------------------------------
-- ParallelT
------------------------------------------------------------------------------

-- | For 'ParallelT' streams:
--
-- @
-- (<>) = 'Streamly.Prelude.parallel'
-- (>>=) = flip . 'Streamly.Prelude.concatMapWith' 'Streamly.Prelude.parallel'
-- @
--
-- See 'Streamly.Prelude.AsyncT', 'ParallelT' is similar except that all
-- iterations are strictly concurrent while in 'AsyncT' it depends on the
-- consumer demand and available threads. See 'parallel' for more details.
--
-- /Since: 0.1.0 ("Streamly")/
--
-- /Since: 0.7.0 (maxBuffer applies to ParallelT streams)/
--
-- @since 0.8.0
newtype ParallelT m a = ParallelT {getParallelT :: K.StreamK m a}

#if !(MIN_VERSION_transformers(0,6,0))
instance MonadTrans ParallelT where
    {-# INLINE lift #-}
    lift = ParallelT . K.fromEffect
#endif

-- | A parallely composing IO stream of elements of type @a@.
-- See 'ParallelT' documentation for more details.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type Parallel = ParallelT IO

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

{-# INLINE append #-}
{-# SPECIALIZE append :: ParallelT IO a -> ParallelT IO a -> ParallelT IO a #-}
append :: MonadAsync m => ParallelT m a -> ParallelT m a -> ParallelT m a
append (ParallelT m1) (ParallelT m2) = ParallelT $ parallelK m1 m2

instance MonadAsync m => Semigroup (ParallelT m a) where
    (<>) = append

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (ParallelT m a) where
    mempty = ParallelT K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE apParallel #-}
{-# SPECIALIZE apParallel ::
    ParallelT IO (a -> b) -> ParallelT IO a -> ParallelT IO b #-}
apParallel :: MonadAsync m =>
    ParallelT m (a -> b) -> ParallelT m a -> ParallelT m b
apParallel (ParallelT m1) (ParallelT m2) =
    let f x1 = K.concatMapWith parallelK (K.fromPure . x1) m2
    in ParallelT $ K.concatMapWith parallelK f m1

instance (Monad m, MonadAsync m) => Applicative (ParallelT m) where
    {-# INLINE pure #-}
    pure = ParallelT . K.fromPure

    {-# INLINE (<*>) #-}
    (<*>) = apParallel

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

{-# INLINE bind #-}
{-# SPECIALIZE bind ::
    ParallelT IO a -> (a -> ParallelT IO b) -> ParallelT IO b #-}
bind :: MonadAsync m => ParallelT m a -> (a -> ParallelT m b) -> ParallelT m b
bind (ParallelT m) f = ParallelT $ K.bindWith parallelK m (getParallelT . f)

instance MonadAsync m => Monad (ParallelT m) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = bind

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

#if !(MIN_VERSION_transformers(0,6,0))
instance (MonadBase b m, Monad m, MonadAsync m) => MonadBase b (ParallelT m) where
    liftBase = liftBaseDefault
#endif

MONAD_COMMON_INSTANCES(ParallelT, MONADPARALLEL)

-------------------------------------------------------------------------------
-- From callback
-------------------------------------------------------------------------------

-- Note: we can use another API with two callbacks stop and yield if we want
-- the callback to be able to indicate end of stream.
--
-- | Generates a callback and a stream pair. The callback returned is used to
-- queue values to the stream.  The stream is infinite, there is no way for the
-- callback to indicate that it is done now.
--
-- /Pre-release/
--
{-# INLINE_NORMAL newCallbackStream #-}
newCallbackStream :: MonadAsync m => m (a -> m (), K.StreamK m a)
newCallbackStream = do
    sv <- newParallelVar StopNone defState

    -- XXX Add our own thread-id to the SVar as we can not know the callback's
    -- thread-id and the callback is not run in a managed worker. We need to
    -- handle this better.
    liftIO myThreadId >>= modifyThread sv

    let callback a = liftIO $ void $ send sv (ChildYield a)
    -- XXX we can return an SVar and then the consumer can unfold from the
    -- SVar?
    return (callback, D.toStreamK (SVar.fromSVarD sv))
