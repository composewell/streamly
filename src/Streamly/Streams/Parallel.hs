{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.Parallel
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.Parallel
    (
      ParallelT
    , Parallel
    , parallely
    , parallel
    , parallelFst
    , parallelMin
    , tapAsync

    -- * Function application
    , mkParallel
    , (|$)
    , (|&)
    , (|$.)
    , (|&.)
    )
where

import Control.Concurrent (myThreadId)
import Control.Exception (SomeException(..), throwIO)
import Control.Monad (ap)
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

import Streamly.Streams.SVar (fromSVar, fromStreamVar)
import Streamly.Streams.Serial (map)
import Streamly.Internal.Data.SVar
import Streamly.Streams.StreamK (IsStream(..), Stream, mkStream, foldStream,
                                 foldStreamShared, adapt)
import qualified Streamly.Streams.StreamK as K

#include "Instances.hs"

-------------------------------------------------------------------------------
-- Parallel
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
        liftIO $ decrementBufferLimit sv
        foldStreamShared st yieldk single stop m

    sv = fromJust $ streamVar st

    stop = liftIO $ do
        incrementBufferLimit sv
        sendStop sv winfo
    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> (liftIO $ sendStop sv winfo)
    yieldk a r = sendit a >> go r

runOneLimited
    :: MonadIO m
    => State Stream m a -> Stream m a -> Maybe WorkerInfo -> m ()
runOneLimited st m0 winfo = go m0

    where

    go m = do
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if yieldLimitOk
        then do
            liftIO $ decrementBufferLimit sv
            foldStreamShared st yieldk single stop m
        else do
            liftIO $ cleanupSVarFromWorker sv
            liftIO $ sendStop sv winfo

    sv = fromJust $ streamVar st

    stop = liftIO $ do
        incrementBufferLimit sv
        incrementYieldLimit sv
        sendStop sv winfo
    sendit a = liftIO $ void $ send sv (ChildYield a)
    single a = sendit a >> (liftIO $ sendStop sv winfo)
    yieldk a r = sendit a >> go r

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
            pushWorkerPar sv (runOne st $ toStream m1)
            foldStreamShared st yld sng stp m2
        _ -> foldStreamShared st yld sng stp (forkSVarPar ss m1 m2)

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

mkParallel :: (IsStream t, MonadAsync m) => t m a -> m (t m a)
mkParallel m = do
    sv <- newParallelVar StopNone defState
    pushWorkerPar sv (runOne defState{streamVar = Just sv} $ toStream m)
    return $ fromSVar sv

------------------------------------------------------------------------------
-- Stream to stream concurrent function application
------------------------------------------------------------------------------

{-# INLINE applyParallel #-}
applyParallel :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> t m a -> t m b
applyParallel f m = mkStream $ \st yld sng stp -> do
    sv <- newParallelVar StopNone (adaptState st)
    pushWorkerPar sv (runOne st{streamVar = Just sv} (toStream m))
    foldStream st yld sng stp $ f $ fromSVar sv

------------------------------------------------------------------------------
-- Stream runner concurrent function application
------------------------------------------------------------------------------

{-# INLINE foldParallel #-}
foldParallel :: (IsStream t, MonadAsync m) => (t m a -> m b) -> t m a -> m b
foldParallel f m = do
    sv <- newParallelVar StopNone defState
    pushWorkerPar sv (runOne defState{streamVar = Just sv} $ toStream m)
    f $ fromSVar sv

------------------------------------------------------------------------------
-- Clone and distribute a stream in parallel
------------------------------------------------------------------------------

-- push values to a fold worker via an SVar. Returns whether the fold is done.
{-# INLINE pushToFold #-}
pushToFold :: SVar Stream m a -> a -> IO Bool
pushToFold sv a = do
    -- Check for exceptions before decrement so that we do not
    -- block forever if the child already exited with an exception.
    let qref = outputQueueRev sv
    done <- do
        (_, n) <- readIORef qref
        if (n > 0)
        then fromStreamVarRev sv
        else return False
    if done
    then return True
    else do
        decrementBufferLimit sv
        void $ send sv (ChildYield a)
        return False

{-# INLINE teeToSVar #-}
teeToSVar :: (IsStream t, MonadAsync m) => SVar Stream m a -> t m a -> t m a
teeToSVar svr m = mkStream $ \st yld sng stp -> do
    foldStreamShared st yld sng stp (go svr False m)

    where

    go sv False m0 = mkStream $ \st yld sng stp -> do
        let goDrain = do
                -- In general, a Stop event would come equipped with the result
                -- of the fold. It is not used here but it would be useful in
                -- applicative and distribute.
                done <- fromStreamVarRev sv
                when (not done) $
                    liftIO $ withDiagMVar sv "teeToSVar: waiting to drain"
                                     $ takeMVar (outputDoorBellRev sv)
                    goDrain

            stopFold = liftIO $ do
                sendStop sv Nothing
                goDrain

            stop = stopFold >> stp
            single a = do
                r <- pushToFold a
                when (not r) $ stopFold
                sng a
            yieldk a r = pushToFold a >>= \done -> yld a (go sv done r)
         in foldStreamShared st yieldk single stop m0

    go sv True m0 = m0

-- In case of folds the roles of worker and parent on an SVar are reversed. The
-- parent stream pushes values to an SVar instead of pulling from it and a
-- worker thread running the fold pulls from the SVar and folds the stream. We
-- keep a separate channel for pushing exceptions in the reverse direction i.e.
-- from the fold to the parent stream.
--
-- Note: If we terminate due to an exception, we do not actively terminate the
-- fold. It gets cleaned up by GC.
--
-- NOTE: If we use fromSVar here it will kill the main computation (the parent)
-- when the SVar goes away so we use fromStreamVar instead.

{-# NOINLINE handleChildException #-}
handleChildException :: SVar t m a -> SomeException -> IO ()
handleChildException sv e = do
    tid <- myThreadId
    void $ sendRev sv (ChildStop tid (Just e))

{-# NOINLINE sendStopRev #-}
sendStopRev :: SVar t m a -> IO ()
sendStopRev sv = do
    tid <- myThreadId
    void $ sendRev sv (ChildStop tid Nothing)

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
    -- Buffer size for the SVar is derived from the current state
    sv <- newParallelVar StopNone (adaptState st)
    liftIO myThreadId >>= modifyThread sv

    -- A wrapper to send a Stop event when the fold receives a stop from the
    -- pusher.
    let pullForFold = mkStream $ \s yd sg sp -> do
            let stop = sendStopRev sv >> sp
                single a = yd a (yieldM stop)
            foldStreamShared s yd single stop (fromStreamVar sv)

    void $ doFork (void $ f $ fromStream $ pullForFold)
                  (svarMrun sv)
                  (handleChildException sv)
    foldStreamShared st yld sng stp (teeToSVar sv m)

------------------------------------------------------------------------------
-- Concurrent Application
------------------------------------------------------------------------------

infixr 0 |$
infixr 0 |$.

infixl 1 |&
infixl 1 |&.

-- | Parallel function application operator for streams; just like the regular
-- function application operator '$' except that it is concurrent. The
-- following code prints a value every second even though each stage adds a 1
-- second delay.
--
--
-- @
-- drain $
--    S.mapM (\\x -> threadDelay 1000000 >> print x)
--      |$ S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- @since 0.3.0
{-# INLINE (|$) #-}
(|$) :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> t m a -> t m b
f |$ x = applyParallel f x

-- | Parallel reverse function application operator for streams; just like the
-- regular reverse function application operator '&' except that it is
-- concurrent.
--
-- @
-- drain $
--       S.repeatM (threadDelay 1000000 >> return 1)
--    |& S.mapM (\\x -> threadDelay 1000000 >> print x)
-- @
--
-- /Concurrent/
--
-- @since 0.3.0
{-# INLINE (|&) #-}
(|&) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> t m b) -> t m b
x |& f = f |$ x

-- | Parallel function application operator; applies a @run@ or @fold@ function
-- to a stream such that the fold consumer and the stream producer run in
-- parallel. A @run@ or @fold@ function reduces the stream to a value in the
-- underlying monad. The @.@ at the end of the operator is a mnemonic for
-- termination of the stream.
--
-- @
--    S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
--       |$. S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- @since 0.3.0
{-# INLINE (|$.) #-}
(|$.) :: (IsStream t, MonadAsync m) => (t m a -> m b) -> t m a -> m b
f |$. x = foldParallel f x

-- | Parallel reverse function application operator for applying a run or fold
-- functions to a stream. Just like '|$.' except that the operands are reversed.
--
-- @
--        S.repeatM (threadDelay 1000000 >> return 1)
--    |&. S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- @since 0.3.0
{-# INLINE (|&.) #-}
(|&.) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> m b) -> m b
x |&. f = f |$. x

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

MONAD_APPLICATIVE_INSTANCE(ParallelT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(ParallelT, MONADPARALLEL)
