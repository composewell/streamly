{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.Parallel
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Internal.Data.Stream.Parallel
    (
      ParallelT
    , Parallel
    , parallely
    , parallel
    , parallelFst
    , parallelMin
    , tapAsync
    , mkParallel
    , applyParallel
    , foldParallel
    , sample
    , sampleWithDefault
    )
where

import Control.Concurrent (myThreadId, takeMVar, tryPutMVar)
import Control.Monad (when)
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow, throwM)
-- import Control.Monad.Error.Class   (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Functor (void)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromJust)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Prelude hiding (map)

import qualified Data.Set as Set

import Streamly.Internal.Data.Stream.SVar
       (fromSVar, fromProducer, fromProducerPeek, fromConsumer, pushToFold)
import Streamly.Internal.Data.Stream.StreamK
       (IsStream(..), Stream, mkStream, foldStream, foldStreamShared, adapt)

import Streamly.Internal.Data.SVar

import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D

#include "Instances.hs"

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- StreamK based worker routines
-------------------------------------------------------------------------------

{-# NOINLINE runOne #-}
runOne
    :: MonadIO m
    => State Stream m a -> Stream m a -> Maybe WorkerInfo -> m ()
runOne st m0 winfo =
    case getYieldLimit st of
        Nothing -> go m0
        Just _  -> runOneLimited st m0 winfo

    where

    go m = do
         case maxBufferLimit sv of
            BufferUnlimited ->
                foldStreamShared st yieldk single stopUnlim m
            BufferLast ->
                foldStreamShared st yieldkRep singleRep stopUnlim m
            BufferLimited _ policy -> do
                liftIO $ decrementBufferLimit sv policy
                foldStreamShared st yieldk single stopLim m

    sv = fromJust $ streamVar st

    stopUnlim = liftIO $ sendStop sv winfo
    stopLim = liftIO $ do
        incrementBufferLimit sv
        sendStop sv winfo

    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> (liftIO $ sendStop sv winfo)
    yieldk a r = sendit a >> go r

    sendReplaceIt a = liftIO $ void $ sendReplace sv (ChildYield a)
    singleRep a = sendReplaceIt a >> (liftIO $ sendStop sv winfo)
    yieldkRep a r = sendReplaceIt a >> go r

runOneLimited
    :: MonadIO m
    => State Stream m a -> Stream m a -> Maybe WorkerInfo -> m ()
runOneLimited st m0 winfo = go m0

    where

    go m = do
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if yieldLimitOk
        then do
            case maxBufferLimit sv of
                BufferUnlimited ->
                    foldStreamShared st yieldk single stopUnlim m
                BufferLast ->
                    foldStreamShared st yieldkRep singleRep stopUnlim m
                BufferLimited _ policy -> do
                    liftIO $ decrementBufferLimit sv policy
                    foldStreamShared st yieldk single stopLim m
        else do
            liftIO $ cleanupSVarFromWorker sv
            liftIO $ sendStop sv winfo

    sv = fromJust $ streamVar st

    stopUnlim = liftIO $ sendStop sv winfo
    stopLim = liftIO $ do
        incrementBufferLimit sv
        incrementYieldLimit sv
        sendStop sv winfo

    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> (liftIO $ sendStop sv winfo)
    yieldk a r = sendit a >> go r

    sendReplaceIt a = liftIO $ void $ sendReplace sv (ChildYield a)
    singleRep a = sendReplaceIt a >> (liftIO $ sendStop sv winfo)
    yieldkRep a r = sendReplaceIt a >> go r

-------------------------------------------------------------------------------
-- Consing and appending a stream in parallel style
-------------------------------------------------------------------------------

-- Note that consing and appending requires StreamK as it would not scale well
-- with StreamD unless we are only consing a very small number of streams or
-- elements in a stream. StreamK allows us to manipulate control flow in a way
-- which StreamD cannot allow. StreamK can make a jump without having to
-- remember the past state.

{-# NOINLINE forkSVarPar #-}
forkSVarPar :: (IsStream t, MonadAsync m)
    => SVarStopStyle -> t m a -> t m a -> t m a
forkSVarPar ss m r = mkStream $ \st yld sng stp -> do
    sv <- newParallelVar ss st
    pushWorkerPar sv (runOne st{streamVar = Just sv} $ toStream m)
    case ss of
        StopBy -> liftIO $ do
            set <- readIORef (workerThreads sv)
            writeIORef (svarStopBy sv) $ Set.elemAt 0 set
        _ -> return ()
    pushWorkerPar sv (runOne st{streamVar = Just sv} $ toStream r)
    foldStream st yld sng stp (fromSVar sv)

{-# INLINE joinStreamVarPar #-}
joinStreamVarPar :: (IsStream t, MonadAsync m)
    => SVarStyle -> SVarStopStyle -> t m a -> t m a -> t m a
joinStreamVarPar style ss m1 m2 = mkStream $ \st yld sng stp ->
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
            pushWorkerPar sv (runOne st $ toStream m1)
            foldStreamShared st yld sng stp m2
        _ ->
            -- Here WE ARE IN THE CONSUMER THREAD, we create a new SVar, fork
            -- worker threads to execute m1 and m2 and this thread starts
            -- pulling the stream from the SVar.
            foldStreamShared st yld sng stp (forkSVarPar ss m1 m2)

-------------------------------------------------------------------------------
-- User facing APIs
-------------------------------------------------------------------------------

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using parallel.
{-# INLINE consMParallel #-}
{-# SPECIALIZE consMParallel :: IO a -> ParallelT IO a -> ParallelT IO a #-}
consMParallel :: MonadAsync m => m a -> ParallelT m a -> ParallelT m a
consMParallel m r = fromStream $ K.yieldM m `parallel` (toStream r)

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'ParallelT'
-- Merges two streams concurrently.
--
-- @since 0.2.0
{-# INLINE parallel #-}
parallel :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallel = joinStreamVarPar ParallelVar StopNone

-- This is a co-parallel like combinator for streams, where first stream is the
-- main stream and the rest are just supporting it, when the first ends
-- everything ends.
--
-- | Like `parallel` but stops the output as soon as the first stream stops.
--
-- @since 0.7.0
{-# INLINE parallelFst #-}
parallelFst :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelFst = joinStreamVarPar ParallelVar StopBy

-- This is a race like combinator for streams.
--
-- | Like `parallel` but stops the output as soon as any of the two streams
-- stops.
--
-- @since 0.7.0
{-# INLINE parallelMin #-}
parallelMin :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelMin = joinStreamVarPar ParallelVar StopAny

------------------------------------------------------------------------------
-- Convert a stream to parallel
------------------------------------------------------------------------------

mkParallel :: (IsStream t, MonadAsync m)
    => State Stream m a -> t m a -> m (t m a)
mkParallel st m = do
    sv <- newParallelVar StopNone defState
    -- pushWorkerPar sv (runOne st{streamVar = Just sv} $ toStream m)
    D.toSVarParallel st sv $ D.toStreamD m
    return $ fromSVar sv

------------------------------------------------------------------------------
-- Stream to stream concurrent function application
------------------------------------------------------------------------------

{-# INLINE applyParallel #-}
applyParallel :: (IsStream t, MonadAsync m)
    => (t m a -> t m b) -> t m a -> t m b
applyParallel f m = mkStream $ \st yld sng stp -> do
    sv <- newParallelVar StopNone (adaptState st)
    -- pushWorkerPar sv (runOne st{streamVar = Just sv} (toStream m))
    D.toSVarParallel (adaptState st) sv $ D.toStreamD m
    foldStream st yld sng stp $ f $ fromSVar sv

------------------------------------------------------------------------------
-- Stream runner concurrent function application
------------------------------------------------------------------------------

{-# INLINE foldParallel #-}
foldParallel :: (IsStream t, MonadAsync m) => (t m a -> m b) -> t m a -> m b
foldParallel f m = do
    sv <- newParallelVar StopNone defState
    -- pushWorkerPar sv (runOne defState{streamVar = Just sv} $ toStream m)
    D.toSVarParallel defState sv $ D.toStreamD m
    f $ fromSVar sv

------------------------------------------------------------------------------
-- Clone and distribute a stream in parallel
------------------------------------------------------------------------------

-- Tap a stream and send the elements to the specified SVar in addition to
-- yielding them again.
--
-- XXX this could be written in StreamD style for better efficiency with fusion.
--
{-# INLINE teeToSVar #-}
teeToSVar :: (IsStream t, MonadAsync m) => SVar Stream m a -> t m a -> t m a
teeToSVar svr m = mkStream $ \st yld sng stp -> do
    foldStreamShared st yld sng stp (go False m)

    where

    go False m0 = mkStream $ \st yld _ stp -> do
        let drain = do
                -- In general, a Stop event would come equipped with the result
                -- of the fold. It is not used here but it would be useful in
                -- applicative and distribute.
                done <- fromConsumer svr
                when (not done) $ do
                    liftIO $ withDiagMVar svr "teeToSVar: waiting to drain"
                           $ takeMVar (outputDoorBellFromConsumer svr)
                    drain

            stopFold = do
                liftIO $ sendStop svr Nothing
                -- drain/wait until a stop event arrives from the fold.
                drain

            stop       = stopFold >> stp
            single a   = do
                done <- pushToFold svr a
                yld a (go done (K.nilM stopFold))
            yieldk a r = pushToFold svr a >>= \done -> yld a (go done r)
         in foldStreamShared st yieldk single stop m0

    go True m0 = m0

-- In case of folds the roles of worker and parent on an SVar are reversed. The
-- parent stream pushes values to an SVar instead of pulling from it and a
-- worker thread running the fold pulls from the SVar and folds the stream. We
-- keep a separate channel for pushing exceptions in the reverse direction i.e.
-- from the fold to the parent stream.
--
-- Note: If we terminate due to an exception, we do not actively terminate the
-- fold. It gets cleaned up by the GC.

-- | Create an SVar with a fold consumer that will fold any elements sent to it
-- using the supplied fold function.
{-# INLINE newFoldSVar #-}
newFoldSVar :: (IsStream t, MonadAsync m)
    => State Stream m a -> (t m a -> m b) -> m (SVar Stream m a)
newFoldSVar stt f = do
    -- Buffer size for the SVar is derived from the current state
    sv <- newParallelVar StopAny (adaptState stt)

    -- Add the producer thread-id to the SVar.
    liftIO myThreadId >>= modifyThread sv

    void $ doFork (void $ f $ fromStream $ fromProducer sv)
                  (svarMrun sv)
                  (handleFoldException sv)
    return sv

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
-- is determined by the prevailing 'maxBuffer' setting.
--
-- @
--               Stream m a -> m b
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
-- @since 0.7.0
{-# INLINE tapAsync #-}
tapAsync :: (IsStream t, MonadAsync m) => (t m a -> m b) -> t m a -> t m a
tapAsync f m = mkStream $ \st yld sng stp -> do
    sv <- newFoldSVar st f
    foldStreamShared st yld sng stp (teeToSVar sv m)

------------------------------------------------------------------------------
-- Sampling
------------------------------------------------------------------------------

-- | The input stream of 'sample' is asynchronously evaluated in a loop, the
-- latest value from the evaluation is retained, whenever the output stream
-- produced by 'sample' is evaluated it supplies the latest sample available
-- from the input stream. If the input has not yet generated any output
-- 'sample' waits for the first sample to arrive.  Any exceptions from the
-- input stream are propagated to the output stream.
--
-- @
--      S.mapM_ (\x -> print x >> threadDelay 100000)
--    $ S.sample
--    $ S.fromList [1..]
-- @
--
-- /Internal/
--
{-# INLINE sample #-}
sample :: (IsStream t, MonadAsync m) => t m a -> t m a
sample m = mkStream $ \s yld sng stp -> do
    let st = setBufferStyle BufferLast s
    sv <- newParallelVar StopNone (adaptState st)
    -- pushWorkerPar sv (runOne st{streamVar = Just sv} (toStream m))
    D.toSVarParallel (adaptState st) sv $ D.toStreamD m
    -- Hack! drain the putMVar from modifyThread in D.toSVarParallel
    -- so that we can wait for the first event in fromProducerPeek
    liftIO $ takeMVar (outputDoorBell sv)
    foldStream st yld sng stp $ fromProducerPeek sv

-- This can be implemented by merging the sampled stream with a default stream
-- using a priority merge giving higher priority to the sampled stream.
--
-- | Like 'sample' but uses a default sample when no output is yet generated by
-- the input stream.
--
-- @
--      S.mapM_ (\x -> print x >> threadDelay 100000)
--    $ Par.sampleWithDefault 0
--    $ S.mapM (\x ->
--          if x == 1
--          then threadDelay 1000000 >> return x
--          else return x)
--    $ S.fromList [1..]
-- @
--
-- /Internal/
--
{-# INLINE sampleWithDefault #-}
sampleWithDefault :: (IsStream t, MonadAsync m) => a -> t m a -> t m a
sampleWithDefault def m = mkStream $ \s yld sng stp -> do
    let st = setBufferStyle BufferLast s
    sv <- newParallelVar StopNone (adaptState st)
    -- initialize the buffer with the default value
    liftIO $ writeIORef (outputQueue sv) ([ChildYield def],1)
    liftIO $ void $ tryPutMVar (outputDoorBell sv) ()
    -- pushWorkerPar sv (runOne st{streamVar = Just sv} (toStream m))
    D.toSVarParallel (adaptState st) sv $ D.toStreamD m
    foldStream st yld sng stp $ fromProducerPeek sv

------------------------------------------------------------------------------
-- ParallelT
------------------------------------------------------------------------------

-- | Async composition with strict concurrent execution of all streams.
--
-- The 'Semigroup' instance of 'ParallelT' executes both the streams
-- concurrently without any delay or without waiting for the consumer demand
-- and /merges/ the results as they arrive. If the consumer does not consume
-- the results, they are buffered upto a configured maximum, controlled by the
-- 'maxBuffer' primitive. If the buffer becomes full the concurrent tasks will
-- block until there is space in the buffer.
--
-- Both 'WAsyncT' and 'ParallelT', evaluate the constituent streams fairly in a
-- round robin fashion. The key difference is that 'WAsyncT' might wait for the
-- consumer demand before it executes the tasks whereas 'ParallelT' starts
-- executing all the tasks immediately without waiting for the consumer demand.
-- For 'WAsyncT' the 'maxThreads' limit applies whereas for 'ParallelT' it does
-- not apply. In other words, 'WAsyncT' can be lazy whereas 'ParallelT' is
-- strict.
--
-- 'ParallelT' is useful for cases when the streams are required to be
-- evaluated simultaneously irrespective of how the consumer consumes them e.g.
-- when we want to race two tasks and want to start both strictly at the same
-- time or if we have timers in the parallel tasks and our results depend on
-- the timers being started at the same time. If we do not have such
-- requirements then 'AsyncT' or 'AheadT' are recommended as they can be more
-- efficient than 'ParallelT'.
--
-- @
-- main = ('toList' . 'parallely' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- When streams with more than one element are merged, it yields whichever
-- stream yields first without any bias, unlike the 'Async' style streams.
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the 'Monad' instance of 'ParallelT' runs /all/ iterations
-- of the loop concurrently.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'drain' . 'parallely' $ do
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
-- Note that parallel composition can only combine a finite number of
-- streams as it needs to retain state for each unfinished stream.
--
-- /Since: 0.7.0 (maxBuffer applies to ParallelT streams)/
--
-- /Since: 0.1.0/
newtype ParallelT m a = ParallelT {getParallelT :: Stream m a}
    deriving (MonadTrans)

-- | A parallely composing IO stream of elements of type @a@.
-- See 'ParallelT' documentation for more details.
--
-- @since 0.2.0
type Parallel = ParallelT IO

-- | Fix the type of a polymorphic stream as 'ParallelT'.
--
-- @since 0.1.0
parallely :: IsStream t => ParallelT m a -> t m a
parallely = adapt

instance IsStream ParallelT where
    toStream = getParallelT
    fromStream = ParallelT

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ParallelT IO a -> ParallelT IO a #-}
    consM = consMParallel

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ParallelT IO a -> ParallelT IO a #-}
    (|:) = consM

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

{-# INLINE mappendParallel #-}
{-# SPECIALIZE mappendParallel :: ParallelT IO a -> ParallelT IO a -> ParallelT IO a #-}
mappendParallel :: MonadAsync m => ParallelT m a -> ParallelT m a -> ParallelT m a
mappendParallel m1 m2 = fromStream $ parallel (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (ParallelT m a) where
    (<>) = mappendParallel

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (ParallelT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE apParallel #-}
{-# SPECIALIZE apParallel :: ParallelT IO (a -> b) -> ParallelT IO a -> ParallelT IO b #-}
apParallel :: MonadAsync m => ParallelT m (a -> b) -> ParallelT m a -> ParallelT m b
apParallel (ParallelT m1) (ParallelT m2) =
    let f x1 = K.concatMapBy parallel (pure . x1) m2
    in ParallelT $ K.concatMapBy parallel f m1

instance (Monad m, MonadAsync m) => Applicative (ParallelT m) where
    {-# INLINE pure #-}
    pure = ParallelT . K.yield
    {-# INLINE (<*>) #-}
    (<*>) = apParallel

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

{-# INLINE bindParallel #-}
{-# SPECIALIZE bindParallel :: ParallelT IO a -> (a -> ParallelT IO b) -> ParallelT IO b #-}
bindParallel :: MonadAsync m => ParallelT m a -> (a -> ParallelT m b) -> ParallelT m b
bindParallel m f = fromStream $ K.bindWith parallel (K.adapt m) (\a -> K.adapt $ f a)

instance MonadAsync m => Monad (ParallelT m) where
    return = pure
    (>>=) = bindParallel

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_COMMON_INSTANCES(ParallelT, MONADPARALLEL)
