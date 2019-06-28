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
    , parallelEndByFirst
    , parallelEndByAny

    -- * Function application
    , mkParallel
    , (|$)
    , (|&)
    , (|$.)
    , (|&.)
    )
where

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
import Data.Semigroup (Semigroup(..))
import Prelude hiding (map)

import qualified Data.Set as Set

import Streamly.Streams.SVar (fromSVar)
import Streamly.Streams.Serial (map)
import Streamly.SVar
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
runOne st m0 winfo = go m0

    where

    go m = do
        yieldLimitOk <- liftIO $ decrementYieldLimit sv
        if yieldLimitOk
        then do
            liftIO $ decrementBufferLimit sv
            foldStreamShared st yieldk single stop m
        else liftIO $ cleanupSVarFromWorker sv

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
{-# INLINE parallelEndByFirst #-}
parallelEndByFirst :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelEndByFirst = joinStreamVarPar ParallelVar StopBy

-- This is a race like combinator for streams.
--
-- | Like `parallel` but stops the output as soon as any of the two streams
-- stops.
--
-- @since 0.7.0
{-# INLINE parallelEndByAny #-}
parallelEndByAny :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelEndByAny = joinStreamVarPar ParallelVar StopAny

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

-- | Async composition with simultaneous traversal of all streams.
--
-- The Semigroup instance of 'ParallelT' concurrently /merges/ two streams,
-- running both strictly concurrently and yielding elements from both streams
-- as they arrive. When multiple streams are combined using 'ParallelT' each
-- one is evaluated in its own thread and the results produced are presented in
-- the combined stream on a first come first serve basis.
--
-- 'AsyncT' and 'WAsyncT' are /concurrent lookahead streams/ each with a
-- specific type of consumption pattern (depth first or breadth first). Since
-- they are lookahead, they may introduce certain default latency in starting
-- more concurrent tasks for efficiency reasons or may put a default limitation
-- on the resource consumption (e.g. number of concurrent threads for
-- lookahead).  If we look at the implementation detail, they both can share a
-- pool of worker threads to evaluate the streams in the desired pattern and at
-- the desired rate. However, 'ParallelT' uses a separate runtime thread to
-- evaluate each stream.
--
-- 'WAsyncT' is similar to 'ParallelT', as both of them evaluate the
-- constituent streams fairly in a round robin fashion.
-- However, the key difference is that 'WAsyncT' is lazy or pull driven
-- whereas 'ParallelT' is strict or push driven.  'ParallelT' immediately
-- starts concurrent evaluation of both the streams (in separate threads) and
-- later picks the results whereas 'WAsyncT' may wait for a certain latency
-- threshold before initiating concurrent evaluation of the next stream. The
-- concurrent scheduling of the next stream or the degree of concurrency is
-- driven by the feedback from the consumer. In case of 'ParallelT' each stream
-- is evaluated in a separate thread and results are /pushed/ to a shared
-- output buffer, the evaluation rate is controlled by blocking when the buffer
-- is full.
--
-- Concurrent lookahead streams are generally more efficient than
-- 'ParallelT' and can work pretty efficiently even for smaller tasks because
-- they do not necessarily use a separate thread for each task. So they should
-- be preferred over 'ParallelT' especially when efficiency is a concern and
-- simultaneous strict evaluation is not a requirement.  'ParallelT' is useful
-- for cases when the streams are required to be evaluated simultaneously
-- irrespective of how the consumer consumes them e.g.  when we want to race
-- two tasks and want to start both strictly at the same time or if we have
-- timers in the parallel tasks and our results depend on the timers being
-- started at the same time.  We can say that 'ParallelT' is almost the same
-- (modulo some implementation differences) as 'WAsyncT' when the latter is
-- used with unlimited lookahead and zero latency in initiating lookahead.
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
-- @since 0.1.0
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
