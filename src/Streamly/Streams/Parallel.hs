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
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup(..))
import Prelude hiding (map)

import Streamly.Streams.SVar (fromSVar)
import Streamly.Streams.Serial (map)
import Streamly.SVar
import Streamly.Streams.StreamK (IsStream(..), Stream(..), adapt)
import qualified Streamly.Streams.StreamK as K

#include "Instances.hs"

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

{-# NOINLINE runOne #-}
runOne
    :: MonadIO m
    => State Stream m a -> Stream m a -> Maybe WorkerInfo -> m ()
runOne st m winfo = unStream m st stop single yieldk

    where

    sv = fromJust $ streamVar st
    mrun = runInIO $ svarMrun sv

    withLimitCheck action = do
        yieldLimitOk <- liftIO $ decrementYieldLimitPost sv
        if yieldLimitOk
        then action
        else liftIO $ cleanupSVarFromWorker sv

    stop = liftIO $ sendStop sv winfo
    sendit a = liftIO $ sendYield sv winfo (ChildYield a)
    single a = sendit a >> withLimitCheck stop

    -- XXX there is no flow control in parallel case. We should perhaps use a
    -- queue and queue it back on that and exit the thread when the outputQueue
    -- overflows. Parallel is dangerous because it can accumulate unbounded
    -- output in the buffer.
    yieldk a r = void (sendit a)
        >> withLimitCheck (void $ liftIO $ mrun $ runOne st r winfo)

{-# NOINLINE forkSVarPar #-}
forkSVarPar :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
forkSVarPar m r = Stream $ \st stp sng yld -> do
    sv <- newParallelVar st
    pushWorkerPar sv (runOne st{streamVar = Just sv} m)
    pushWorkerPar sv (runOne st{streamVar = Just sv} r)
    unStream (fromSVar sv) (rstState st) stp sng yld

{-# INLINE joinStreamVarPar #-}
joinStreamVarPar :: MonadAsync m
    => SVarStyle -> Stream m a -> Stream m a -> Stream m a
joinStreamVarPar style m1 m2 = Stream $ \st stp sng yld ->
    case streamVar st of
        Just sv | svarStyle sv == style -> do
            pushWorkerPar sv (runOne st m1)
            unStream m2 st stp sng yld
        _ -> unStream (forkSVarPar m1 m2) st stp sng yld

{-# INLINE parallelStream #-}
parallelStream :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelStream = joinStreamVarPar ParallelVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using parallel.
{-# INLINE consMParallel #-}
consMParallel :: MonadAsync m => m a -> Stream m a -> Stream m a
consMParallel m r = K.yieldM m `parallelStream` r

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'ParallelT'
-- Merges two streams concurrently.
--
-- @since 0.2.0
{-# INLINE parallel #-}
parallel :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallel m1 m2 = fromStream $ Stream $ \st stp sng yld ->
    unStream (parallelStream (toStream m1) (toStream m2))
             st stp sng yld

------------------------------------------------------------------------------
-- Convert a stream to parallel
------------------------------------------------------------------------------

mkParallel :: (IsStream t, MonadAsync m) => t m a -> m (t m a)
mkParallel m = do
    sv <- newParallelVar defState
    pushWorkerPar sv (runOne defState{streamVar = Just sv} $ toStream m)
    return $ fromSVar sv

------------------------------------------------------------------------------
-- Stream to stream concurrent function application
------------------------------------------------------------------------------

{-# INLINE applyWith #-}
applyWith :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> t m a -> t m b
applyWith f m = fromStream $ Stream $ \st stp sng yld -> do
    sv <- newParallelVar (rstState st)
    pushWorkerPar sv (runOne st{streamVar = Just sv} (toStream m))
    unStream (toStream $ f $ fromSVar sv) (rstState st) stp sng yld

------------------------------------------------------------------------------
-- Stream runner concurrent function application
------------------------------------------------------------------------------

{-# INLINE runWith #-}
runWith :: (IsStream t, MonadAsync m) => (t m a -> m b) -> t m a -> m b
runWith f m = do
    sv <- newParallelVar defState
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
-- runStream $
--    S.mapM (\\x -> threadDelay 1000000 >> print x)
--      |$ S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- @since 0.3.0
{-# INLINE (|$) #-}
(|$) :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> t m a -> t m b
f |$ x = applyWith f x

-- | Parallel reverse function application operator for streams; just like the
-- regular reverse function application operator '&' except that it is
-- concurrent.
--
-- @
-- runStream $
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
f |$. x = runWith f x

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
-- main = 'runStream' . 'parallely' $ do
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
    consM m r = fromStream $ consMParallel m (toStream r)

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ParallelT IO a -> ParallelT IO a #-}
    (|:) = consM

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

instance MonadAsync m => Semigroup (ParallelT m a) where
    (<>) = parallel

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (ParallelT m a) where
    mempty = K.nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (ParallelT m) where
    return = pure
    (ParallelT m) >>= f
        = ParallelT $ K.bindWith parallelStream m (getParallelT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(ParallelT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(ParallelT, MONADPARALLEL)
