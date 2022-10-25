-- |
-- Module      : Streamly.Internal.Data.Stream.Concurrent
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Non-parallelizable stream combinators like unfoldrM, iterateM etc. can be
-- evaluated concurrently with the stream consumer by using `eval`.
-- Parallelizable combinators like repeatM, replicateM can generate the stream
-- concurrently using 'concatMap'.

-- Single effects related functionality can be moved to
-- Data.Async/Control.Async.
-- Common Channel functionality to Data.Channel.
-- Stream channel to Data.Stream.Channel.

module Streamly.Internal.Data.Stream.Concurrent
    (
    -- * Imports
    -- $setup

    -- * Types
      MonadAsync

    -- * Configuration
    , Config
    , maxThreads
    , maxBuffer
    , eager
    , StopWhen (..)
    , stopWhen
    , ordered
    , interleaved
    -- maxYields
    , Rate(..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate
    , inspect

    -- * Combinators
    -- | Stream combinators using a concurrent channel

    -- ** Evaluate
    -- | Evaluates a stream concurrently using a channel.
    , eval
    , evalWith
    -- Add unfoldrM/iterateM?

    -- ** Map
    -- | Uses a single channel to evaluate all actions.
    , mapM
    , mapMWith
    , sequence
    , sequenceWith
    -- Add repeatM/replicateM?

    -- ** Combine two
    -- | Use a channel for each pair.
    -- combine/concur/conjoin
    , append2
    , interleave2
    , ahead2
    , parallel2
    , parallelFst2
    , parallelMin2
    , combineWith

    -- ** List of streams
    -- | Shares a single channel across many streams.
    , append
    , interleave
    , ahead
    , parallel
    , parallelFst
    , parallelMin
    , concatListWith
    , zipWithM
    , zipWith

    -- ** Stream of streams
    -- *** Apply
    -- | Uses a separate channel for each application.
    , apply
    , applyWith

    -- *** Concat
    -- | Shares a single channel across many streams.
    , concat
    , concatWith
    , concatMap
    , concatMapInterleave
    , concatMapWith

    -- ** Reactive
    , fromCallback
    , pollCountsD
    , pollCounts
    )
where

#include "inline.hs"

import Control.Concurrent (myThreadId, killThread)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Streamly.Internal.Control.Concurrent (MonadAsync, askRunInIO)
import Streamly.Internal.Control.ForkLifted (forkManaged)
import Streamly.Internal.Data.Stream.Channel.Dispatcher (modifyThread)
import Streamly.Internal.Data.Stream.Channel.Types
    ( ChildEvent(..)
    , concatMapDivK
    )
import Streamly.Internal.Data.Stream.Channel.Worker (sendWithDoorBell)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Stream.StreamD (Step(..))

import qualified Streamly.Internal.Data.IORef.Unboxed as Unboxed
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (mapM, sequence, concat, concatMap, zipWith)
import Streamly.Internal.Data.Stream.Concurrent.Channel

-- $setup
--
-- Imports for example snippets in this module.
--
-- >>> :m
-- >>> {-# LANGUAGE FlexibleContexts #-}
-- >>> import Control.Concurrent (threadDelay)
-- >>> import qualified Streamly.Data.Array.Unboxed as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.Concurrent as Concur
-- >>> import Prelude hiding (concatMap, concat, zipWith)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

-------------------------------------------------------------------------------
-- Evaluating a stream
-------------------------------------------------------------------------------

{-
{-# INLINE_NORMAL evalWithD #-}
evalWithD :: MonadAsync m => (Config -> Config) -> D.Stream m a -> D.Stream m a
evalWithD modifier m = D.Stream step Nothing
    where

    step _ Nothing = do
        chan <- newChannel modifier
        sendFirstWorker chan (D.toStreamK m)
        -- XXX should use an unfold to make this efficient
        return $ D.Skip $ Just $ fromChannelD chan

    step gst (Just (D.UnStream step1 st)) = do
        r <- step1 gst st
        return $ case r of
            D.Yield a s -> D.Yield a (Just $ D.Stream step1 s)
            D.Skip s    -> D.Skip (Just $ D.Stream step1 s)
            D.Stop      -> D.Stop
-}

-- | Like 'eval' but can specify a config modifier to change the concurrent
-- channel parameters.
--
-- > evalWith = withChannel (const id)
--
{-# INLINE evalWith #-}
evalWith :: MonadAsync m => (Config -> Config) -> Stream m a -> Stream m a
evalWith modifier input = withChannel modifier input (const id)
    -- Stream.fromStreamD $ evalWithD cfg $ Stream.toStreamD stream

-- | Evaluate a stream asynchronously using a channel and serve the consumer
-- from the evaluation buffer.
--
-- >>> eval = Concur.evalWith id
--
{-# INLINE eval #-}
eval :: MonadAsync m => Stream m a -> Stream m a
eval = evalWith id

-------------------------------------------------------------------------------
-- combining two streams
-------------------------------------------------------------------------------

{-# INLINE _appendGeneric #-}
_appendGeneric :: MonadAsync m =>
       ((Config -> Config) -> m (Channel m a))
    -> (Config -> Config)
    -> K.Stream m a
    -> K.Stream m a
    -> K.Stream m a
_appendGeneric newChan modifier stream1 stream2 = K.concatEffect action

    where

    action = do
        chan <- newChan modifier
        let cfg = modifier defaultConfig
            done = K.nilM (stopChannel chan)
        case getStopWhen cfg of
            AllStop -> do
                toChannelK chan stream2
                toChannelK chan stream1
            FirstStops -> do
                toChannelK chan stream2
                toChannelK chan (K.serial stream1 done)
            AnyStops -> do
                toChannelK chan (K.serial stream2 done)
                toChannelK chan (K.serial stream1 done)
        return $ Stream.toStreamK $ fromChannel chan

-- | Create a new channel and add both the streams to it for async evaluation.
-- The output stream is the result of the evaluation.
{-# INLINE appendWithK #-}
appendWithK :: MonadAsync m =>
    (Config -> Config) -> K.Stream m a -> K.Stream m a -> K.Stream m a
appendWithK modifier stream1 stream2 =
{-
    if getOrdered (modifier defaultConfig)
    then concatMapWithK modifier id (stream1 `K.cons` K.fromPure stream2)
    else _appendGeneric Append.newChannel modifier stream1 stream2
-}
    concatMapWithK modifier id (stream1 `K.cons` K.fromPure stream2)

-- | Evaluate the first stream in the current thread and add the second stream
-- to the supplied channel. This is to be used by a worker thread.
--
-- This can be used with concatMapWith:
--
-- @
-- concatMap = K.concatMapWith (_appendWithChanK chan) f stream
-- @
--
{-# INLINE _appendWithChanK #-}
_appendWithChanK :: MonadAsync m =>
    Channel m a -> K.Stream m a -> K.Stream m a -> K.Stream m a
_appendWithChanK chan stream1 stream2 =
    K.before (toChannelK chan stream2) stream1


-- | Like 'append' but with a Config modifier.
{-# INLINE combineWith #-}
combineWith :: MonadAsync m =>
    (Config -> Config) -> Stream m a -> Stream m a -> Stream m a
combineWith modifier stream1 stream2 =
    Stream.fromStreamK
        $ appendWithK
            modifier (Stream.toStreamK stream1) (Stream.toStreamK stream2)

-- | Binary operation to evaluate two streams concurrently prioritizing the
-- left stream.
--
-- If you want to combine more than two streams you almost always want the
-- 'concat' operation instead. The performance of this operation degrades
-- rapidly when more streams are combined as each operation adds one more
-- concurrent channel. On the other hand, 'concat' uses a single channel for
-- all streams. However, with this operation you can precisely control the
-- scheduling by creating arbitrary shape expression trees.
--
-- >>> append2 = Concur.combineWith id
--
-- The following code finishes in 4 seconds:
--
-- >>> stream1 = Stream.fromEffect (delay 4)
-- >>> stream2 = Stream.fromEffect (delay 2)
-- >>> Stream.fold Fold.toList $ stream1 `Concur.append2` stream2
-- 2 sec
-- 4 sec
-- [2,4]
--
{-# INLINE append2 #-}
append2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
append2 = combineWith id

-- | Like 'append' but interleaves the streams fairly instead of prioritizing
-- the left stream. This schedules all streams in a round robin fashion over
-- limited number of threads.
--
-- >>> interleave2 = Concur.combineWith (Concur.interleaved True)
--
{-# INLINE interleave2 #-}
interleave2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
interleave2 = combineWith (interleaved True)

-- | Like 'append' but with 'ordered' on.
--
-- >>> ahead2 = Concur.combineWith (Concur.ordered True)
--
{-# INLINE ahead2 #-}
ahead2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
ahead2 = combineWith (ordered True)

-- | Like 'append2' but with 'eager' on.
--
-- >>> parallel2 = Concur.combineWith (Concur.eager True)
--
{-# INLINE parallel2 #-}
parallel2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallel2 = combineWith (eager True)

-- | Like 'parallel2' but stops the output as soon as the first stream stops.
--
-- >>> parallelFst2 = Concur.combineWith (Concur.eager True . Concur.stopWhen Concur.FirstStops)
--
-- /Pre-release/
{-# INLINE parallelFst2 #-}
parallelFst2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelFst2 = combineWith (eager True . stopWhen FirstStops)

-- | Like 'parallel2' but stops the output as soon as any of the two streams
-- stops.
--
-- >>> parallelMin2 = Concur.combineWith (Concur.eager True . Concur.stopWhen Concur.AnyStops)
--
-- /Pre-release/
{-# INLINE parallelMin2 #-}
parallelMin2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelMin2 = combineWith (eager True . stopWhen AnyStops)

-------------------------------------------------------------------------------
-- concat streams
-------------------------------------------------------------------------------

-- | A runner function takes a queuing function @q@ and a stream, it splits the
-- input stream, queuing the tail and using the head to generate a stream.
-- 'mkEnqueue' takes a runner function and generates the queuing function @q@.
-- Note that @q@ and the runner are mutually recursive, mkEnqueue ties the knot
-- between the two.
{-# INLINE mkEnqueue #-}
mkEnqueue :: MonadAsync m =>
    Channel m b
    -> ((K.Stream m a -> m ()) -> K.Stream m a -> K.Stream m b)
    -> m (K.Stream m a -> m ())
mkEnqueue chan runner = do
    runInIO <- askRunInIO
    return
        $ let q stream = do
                -- Enqueue the outer loop
                liftIO $ enqueue chan False (runInIO, runner q stream)
                -- XXX In case of eager dispatch we can just directly dispatch
                -- a worker with the tail stream here rather than first queuing
                -- and then dispatching a worker which dequeues the work. The
                -- older implementation did a direct dispatch here and its perf
                -- characterstics looked much better.
                eagerDispatch chan
           in q

-- | Takes the head element of the input stream and queues the tail of the
-- stream to the channel, then maps the supplied function on the head and
-- evaluates the resulting stream.
--
-- This function is designed to be used by worker threads on a channel to
-- concurrently map and evaluate a stream.
{-# INLINE concatMapWithChanK #-}
concatMapWithChanK :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithChanK chan f stream =
   let run q = concatMapDivK q f
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)
    -- K.concatMapWith (_appendWithChanK chan) f stream

{-# INLINE concatMapWithChanKAny #-}
concatMapWithChanKAny :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithChanKAny chan f stream =
   let done = K.nilM (stopChannel chan)
       run q = concatMapDivK q (\x -> K.serial (f x) done)
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)

{-# INLINE concatMapWithChanKFirst #-}
concatMapWithChanKFirst :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithChanKFirst chan f stream =
   let done = K.nilM (stopChannel chan)
       run q = concatMapDivK q f
    in K.concatEffect $ do
        res <- K.uncons stream
        case res of
            Nothing -> return K.nil
            Just (h, t) -> do
                q <- mkEnqueue chan run
                q t
                return $ K.serial (f h) done

{-# INLINE concatMapWithChanKGeneric #-}
concatMapWithChanKGeneric :: MonadAsync m =>
       (Config -> Config)
    -> Channel m b
    -> (a -> K.Stream m b)
    -> K.Stream m a
    -> K.Stream m b
concatMapWithChanKGeneric modifier chan f stream = do
        let cfg = modifier defaultConfig
        case getStopWhen cfg of
            AllStop -> concatMapWithChanK chan f stream
            FirstStops -> concatMapWithChanKFirst chan f stream
            AnyStops -> concatMapWithChanKAny chan f stream

-- XXX Add a deep evaluation variant that evaluates individual elements in the
-- generated streams in parallel.

-- | Allocate a channel and use it to concurrently evaluate the streams
-- generated by the mapped function.
--
{-# INLINE concatMapWithK #-}
concatMapWithK :: MonadAsync m =>
    (Config -> Config) -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
concatMapWithK modifier f input =
    let g = concatMapWithChanKGeneric modifier
     in withChannelK modifier input (`g` f)

-- concatMapWith modifier f stream = concatWith modifier $ fmap f stream

-- | Like 'concatMap' but we can also specify the concurrent channel's
-- configuration parameters using Config modifiers.
--
{-# INLINE concatMapWith #-}
concatMapWith :: MonadAsync m =>
    (Config -> Config) -> (a -> Stream m b) -> Stream m a -> Stream m b
concatMapWith modifier f stream =
    Stream.fromStreamK
        $ concatMapWithK
            modifier (Stream.toStreamK . f) (Stream.toStreamK stream)

-- | Map each element of the input to a stream and then concurrently evaluate
-- and concatenate the resulting streams. Multiple streams may be evaluated
-- concurrently but earlier streams are perferred. Output from the streams are
-- used as they arrive.
--
-- >>> concatMap = Concur.concatMapWith id
--
-- >>> f cfg xs = Stream.fold Fold.toList $ Concur.concatMapWith cfg id $ Stream.fromList xs
--
-- The following streams finish in 4 seconds:
--
-- >>> stream1 = Stream.fromEffect (delay 4)
-- >>> stream2 = Stream.fromEffect (delay 2)
-- >>> stream3 = Stream.fromEffect (delay 1)
-- >>> f id [stream1, stream2, stream3]
-- 1 sec
-- 2 sec
-- 4 sec
-- [1,2,4]
--
-- Limiting threads to 2 schedules the third stream only after one of the first
-- two has finished, releasing a thread:
--
-- >>> f (Concur.maxThreads 2) [stream1, stream2, stream3]
-- ...
-- [2,1,4]
--
-- When used with a Single thread it behaves like serial concatMap:
--
-- >>> f (Concur.maxThreads 1) [stream1, stream2, stream3]
-- ...
-- [4,2,1]
--
-- >>> stream1 = Stream.fromList [1,2,3]
-- >>> stream2 = Stream.fromList [4,5,6]
-- >>> f (Concur.maxThreads 1) [stream1, stream2]
-- [1,2,3,4,5,6]
--
{-# INLINE concatMap #-}
concatMap :: MonadAsync m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap = concatMapWith id

-- | Map each element of the input to a stream and then concurrently evaluate
-- and interleave the resulting streams. Unlike 'concatMap' which prefers to
-- evaluate the earlier stream first, this schedules all streams in a round
-- robin fashion over the available threads.
--
-- >>> concatMapInterleave = Concur.concatMapWith (Concur.interleaved True)
--
-- When used with a single thread it behaves like serial interleaving:
--
-- >>> f cfg xs = Stream.fold Fold.toList $ Concur.concatMapWith (Concur.interleaved True . cfg) id $ Stream.fromList xs
--
-- >>> stream1 = Stream.fromList [1,2,3]
-- >>> stream2 = Stream.fromList [4,5,6]
-- >>> f (Concur.maxThreads 1) [stream1, stream2]
-- [1,4,2,5,3,6]
--
-- /Works only on finite number of streams/
--
{-# INLINE concatMapInterleave #-}
concatMapInterleave :: MonadAsync m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMapInterleave = concatMapWith (interleaved True)

-- | Like 'concat' but we can also specify the concurrent channel's
-- configuration parameters using Config modifiers.
--
-- >>> concatWith modifier = Concur.concatMapWith modifier id
--
{-# INLINE concatWith #-}
concatWith :: MonadAsync m =>
    (Config -> Config) -> Stream m (Stream m a) -> Stream m a
concatWith modifier = concatMapWith modifier id

-- | Evaluate the streams in the input stream concurrently and combine them.
--
-- >>> concat = Concur.concatWith id
--
{-# INLINE concat #-}
concat :: MonadAsync m => Stream m (Stream m a) -> Stream m a
concat = concatWith id

-------------------------------------------------------------------------------
-- concat Lists
-------------------------------------------------------------------------------

-- | Like 'concatWith' but works on a list of streams.
--
-- >>> concatListWith modifier = Concur.concatWith modifier . Stream.fromList
--
{-# INLINE concatListWith #-}
concatListWith :: MonadAsync m => (Config -> Config) -> [Stream m a] -> Stream m a
concatListWith modifier = concatWith modifier . Stream.fromList

-- | Like 'concat' but works on a list of streams.
--
-- >>> append = Concur.concatListWith id
--
{-# INLINE append #-}
append :: MonadAsync m => [Stream m a] -> Stream m a
append = concatListWith id

-- | Like 'append' but interleaves the streams fairly instead of prioritizing
-- the left stream. This schedules all streams in a round robin fashion over
-- limited number of threads.
--
-- >>> interleave = Concur.concatListWith (Concur.interleaved True)
--
{-# INLINE interleave #-}
interleave :: MonadAsync m => [Stream m a] -> Stream m a
interleave = concatListWith (interleaved True)

-- | Like 'append' but with 'ordered' on.
--
-- >>> ahead = Concur.concatListWith (Concur.ordered True)
--
{-# INLINE ahead #-}
ahead :: MonadAsync m => [Stream m a] -> Stream m a
ahead = concatListWith (ordered True)

-- | Like 'append' but with 'eager' on.
--
-- >>> parallel = Concur.concatListWith (Concur.eager True)
--
{-# INLINE parallel #-}
parallel :: MonadAsync m => [Stream m a] -> Stream m a
parallel = concatListWith (eager True)

-- | Like 'parallel' but stops the output as soon as the first stream stops.
--
-- >>> parallelFst = Concur.concatListWith (Concur.eager True . Concur.stopWhen Concur.FirstStops)
--
{-# INLINE parallelFst #-}
parallelFst :: MonadAsync m => [Stream m a] -> Stream m a
parallelFst = concatListWith (eager True . stopWhen FirstStops)

-- | Like 'parallel' but stops the output as soon as any of the two streams
-- stops.
--
-- >>> parallelMin = Concur.concatListWith (Concur.eager True . Concur.stopWhen Concur.AnyStops)
--
{-# INLINE parallelMin #-}
parallelMin :: MonadAsync m => [Stream m a] -> Stream m a
parallelMin = concatListWith (eager True . stopWhen AnyStops)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE applyWith #-}
{-# SPECIALIZE applyWith ::
   (Config -> Config) -> Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
applyWith :: MonadAsync m =>
    (Config -> Config) -> Stream m (a -> b) -> Stream m a -> Stream m b
applyWith modifier stream1 stream2 =
    concatMapWith
        modifier
        (\g -> concatMapWith modifier (pure . g) stream2)
        stream1

{-# INLINE apply #-}
{-# SPECIALIZE apply :: Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
apply :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
apply = applyWith id

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

-- |
-- >>> mapMWith modifier f = Concur.concatMapWith modifier (Stream.fromEffect . f)
--
{-# INLINE mapMWith #-}
mapMWith :: MonadAsync m =>
    (Config -> Config) -> (a -> m b) -> Stream m a -> Stream m b
mapMWith modifier f = concatMapWith modifier (Stream.fromEffect . f)

-- |
-- >>> mapM = Concur.mapMWith id
{-# INLINE mapM #-}
mapM :: MonadAsync m => (a -> m b) -> Stream m a -> Stream m b
mapM = mapMWith id

-- |
-- >>> sequenceWith modifier = Concur.mapMWith modifier id
--
{-# INLINE sequenceWith #-}
sequenceWith :: MonadAsync m =>
    (Config -> Config) -> Stream m (m a) -> Stream m a
sequenceWith modifier = mapMWith modifier id

-- |
-- >>> sequence = Concur.sequenceWith id
--
{-# INLINE sequence #-}
sequence :: MonadAsync m =>
    Stream m (m a) -> Stream m a
sequence = sequenceWith id

-- |
-- >>> zipWithM f m1 m2 = Stream.zipWithM f (Concur.eval m1) (Concur.eval m2)
--
{-# INLINE zipWithM #-}
zipWithM :: MonadAsync m
    => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM f m1 m2 = Stream.zipWithM f (eval m1) (eval m2)

-- |
-- >>> zipWith f = Stream.zipWithM (\a b -> return $ f a b)
--
-- >>> m1 = Stream.fromList [1,2,3]
-- >>> m2 = Stream.fromList [4,5,6]
-- >>> Stream.fold Fold.toList $ Concur.zipWith (,) m1 m2
-- [(1,4),(2,5),(3,6)]
--
{-# INLINE zipWith #-}
zipWith :: MonadAsync m
    => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f = zipWithM (\a b -> return $ f a b)

-------------------------------------------------------------------------------
-- Reactive
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
newCallbackStream :: MonadAsync m => m (a -> m (), Stream m a)
newCallbackStream = do
    chan <- newChannel (eager True)

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

-- | Takes a callback setter function and provides it with a callback.  The
-- callback when invoked adds a value at the tail of the stream. Returns a
-- stream of values generated by the callback.
--
-- /Pre-release/
--
{-# INLINE fromCallback #-}
fromCallback :: MonadAsync m => ((a -> m ()) -> m ()) -> Stream m a
fromCallback setCallback = Stream.concatM $ do
    (callback, stream) <- newCallbackStream
    setCallback callback
    return stream

{-# INLINE_NORMAL pollCountsD #-}
pollCountsD
    :: MonadAsync m
    => (a -> Bool)
    -> (D.Stream m Int -> m b)
    -> D.Stream m a
    -> D.Stream m a
pollCountsD predicate fld (D.Stream step state) = D.Stream step' Nothing
  where

    {-# INLINE_LATE step' #-}
    step' _ Nothing = do
        -- As long as we are using an "Int" for counts lockfree reads from
        -- Var should work correctly on both 32-bit and 64-bit machines.
        -- However, an Int on a 32-bit machine may overflow quickly.
        countVar <- liftIO $ Unboxed.newIORef (0 :: Int)
        tid <- forkManaged
            $ void $ fld
            $ Unboxed.toStreamD countVar
        return $ Skip (Just (countVar, tid, state))

    step' gst (Just (countVar, tid, st)) = do
        r <- step gst st
        case r of
            Yield x s -> do
                when (predicate x)
                    $ liftIO $ Unboxed.modifyIORef' countVar (+ 1)
                return $ Yield x (Just (countVar, tid, s))
            Skip s -> return $ Skip (Just (countVar, tid, s))
            Stop -> do
                liftIO $ killThread tid
                return Stop

-- | @pollCounts predicate transform fold stream@ counts those elements in the
-- stream that pass the @predicate@. The resulting count stream is sent to
-- another thread which transforms it using @transform@ and then folds it using
-- @fold@.  The thread is automatically cleaned up if the stream stops or
-- aborts due to exception.
--
-- For example, to print the count of elements processed every second:
--
-- @
-- > Stream.drain $ Concur.pollCounts (const True) (Stream.rollingMap (-) . Stream.delayPost 1) (Fold.drainBy print)
--           $ Stream.enumerateFrom 0
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE pollCounts #-}
pollCounts ::
       (MonadAsync m)
    => (a -> Bool)
    -> (Stream m Int -> m b)
    -> Stream m a
    -> Stream m a
pollCounts predicate f xs =
      Stream.fromStreamD
    $ pollCountsD predicate (f . Stream.fromStreamD)
    $ Stream.toStreamD xs
