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
    , parEval
    -- Add unfoldrM/iterateM?

    -- ** Generate
    -- | Uses a single channel to evaluate all actions.
    , repeatM
    , parRepeatM
    , replicateM
    , parReplicateM

    -- ** Map
    -- | Uses a single channel to evaluate all actions.
    , mapM
    , parMapM
    , sequence
    , parSequence

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
    , parLazy
    , parLazyOrdered
    , parLazyInterleaved
    , parEager
    , parEagerFst
    , parEagerMin
    , parConcatList

    , parZipWithM
    , parZipWith

    -- ** Stream of streams
    -- *** Apply
    , apply
    , parApply

    -- *** Concat
    -- | Shares a single channel across many streams.
    , concat
    , parConcat
    , concatMap
    , concatMapInterleave
    , parConcatMap

    -- *** ConcatIterate
    , parConcatIterate

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
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.Concurrent as Stream
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
{-# INLINE_NORMAL parEvalD #-}
parEvalD :: MonadAsync m => (Config -> Config) -> D.Stream m a -> D.Stream m a
parEvalD modifier m = D.Stream step Nothing
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

-- | Evaluate a stream asynchronously using a channel and serve the consumer
-- from the evaluation buffer.
--
-- Note that the evaluation requires a single thread as only one stream needs
-- to be evaluated. Therefore, the concurrency options that are relevant to
-- multiple streams won't apply here e.g. maxThreads, eager, interleaved,
-- ordered, stopWhen options won't have any effect.
--
{-# INLINE parEval #-}
parEval :: MonadAsync m => (Config -> Config) -> Stream m a -> Stream m a
parEval modifier input = withChannel modifier input (const id)
    -- Stream.fromStreamD $ parEvalD cfg $ Stream.toStreamD stream

-- | Definition:
--
-- >>> eval = Stream.parEval id
--
{-# INLINE eval #-}
eval :: MonadAsync m => Stream m a -> Stream m a
eval = parEval id

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
    then parConcatMapK modifier id (stream1 `K.cons` K.fromPure stream2)
    else _appendGeneric Append.newChannel modifier stream1 stream2
-}
    parConcatMapK modifier id (stream1 `K.cons` K.fromPure stream2)

-- | Evaluate the first stream in the current thread and add the second stream
-- to the supplied channel. This is to be used by a worker thread.
--
-- This can be used with parConcatMap:
--
-- @
-- concatMap = K.parConcatMap (_appendWithChanK chan) f stream
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
-- >>> append2 = Stream.combineWith id
--
-- The following code finishes in 4 seconds:
--
-- >>> stream1 = Stream.fromEffect (delay 4)
-- >>> stream2 = Stream.fromEffect (delay 2)
-- >>> Stream.fold Fold.toList $ stream1 `Stream.append2` stream2
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
-- >>> interleave2 = Stream.combineWith (Stream.interleaved True)
--
{-# INLINE interleave2 #-}
interleave2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
interleave2 = combineWith (interleaved True)

-- | Like 'append' but with 'ordered' on.
--
-- >>> ahead2 = Stream.combineWith (Stream.ordered True)
--
{-# INLINE ahead2 #-}
ahead2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
ahead2 = combineWith (ordered True)

-- | Like 'append2' but with 'eager' on.
--
-- >>> parallel2 = Stream.combineWith (Stream.eager True)
--
{-# INLINE parallel2 #-}
parallel2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallel2 = combineWith (eager True)

-- | Like 'parallel2' but stops the output as soon as the first stream stops.
--
-- >>> parallelFst2 = Stream.combineWith (Stream.eager True . Stream.stopWhen Stream.FirstStops)
--
-- /Pre-release/
{-# INLINE parallelFst2 #-}
parallelFst2 :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelFst2 = combineWith (eager True . stopWhen FirstStops)

-- | Like 'parallel2' but stops the output as soon as any of the two streams
-- stops.
--
-- >>> parallelMin2 = Stream.combineWith (Stream.eager True . Stream.stopWhen Stream.AnyStops)
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
{-# INLINE parConcatMapChanK #-}
parConcatMapChanK :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
parConcatMapChanK chan f stream =
   let run q = concatMapDivK q f
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)
    -- K.parConcatMap (_appendWithChanK chan) f stream

{-# INLINE parConcatMapChanKAny #-}
parConcatMapChanKAny :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
parConcatMapChanKAny chan f stream =
   let done = K.nilM (stopChannel chan)
       run q = concatMapDivK q (\x -> K.serial (f x) done)
    in K.concatMapEffect (`run` stream) (mkEnqueue chan run)

{-# INLINE parConcatMapChanKFirst #-}
parConcatMapChanKFirst :: MonadAsync m =>
    Channel m b -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
parConcatMapChanKFirst chan f stream =
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

{-# INLINE parConcatMapChanKGeneric #-}
parConcatMapChanKGeneric :: MonadAsync m =>
       (Config -> Config)
    -> Channel m b
    -> (a -> K.Stream m b)
    -> K.Stream m a
    -> K.Stream m b
parConcatMapChanKGeneric modifier chan f stream = do
        let cfg = modifier defaultConfig
        case getStopWhen cfg of
            AllStop -> parConcatMapChanK chan f stream
            FirstStops -> parConcatMapChanKFirst chan f stream
            AnyStops -> parConcatMapChanKAny chan f stream

-- XXX Add a deep evaluation variant that evaluates individual elements in the
-- generated streams in parallel.

-- | Allocate a channel and use it to concurrently evaluate the streams
-- generated by the mapped function.
--
{-# INLINE parConcatMapK #-}
parConcatMapK :: MonadAsync m =>
    (Config -> Config) -> (a -> K.Stream m b) -> K.Stream m a -> K.Stream m b
parConcatMapK modifier f input =
    let g = parConcatMapChanKGeneric modifier
     in withChannelK modifier input (`g` f)

-- | Map each element of the input to a stream and then concurrently evaluate
-- and concatenate the resulting streams. Multiple streams may be evaluated
-- concurrently but earlier streams are perferred. Output from the streams are
-- used as they arrive.
--
-- Definition:
--
-- >>> parConcatMap modifier f stream = Stream.parConcat modifier $ fmap f stream
--
-- Examples:
--
-- >>> f cfg xs = Stream.fold Fold.toList $ Stream.parConcatMap cfg id $ Stream.fromList xs
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
-- >>> f (Stream.maxThreads 2) [stream1, stream2, stream3]
-- ...
-- [2,1,4]
--
-- When used with a Single thread it behaves like serial concatMap:
--
-- >>> f (Stream.maxThreads 1) [stream1, stream2, stream3]
-- ...
-- [4,2,1]
--
-- >>> stream1 = Stream.fromList [1,2,3]
-- >>> stream2 = Stream.fromList [4,5,6]
-- >>> f (Stream.maxThreads 1) [stream1, stream2]
-- [1,2,3,4,5,6]
--
{-# INLINE parConcatMap #-}
parConcatMap :: MonadAsync m =>
    (Config -> Config) -> (a -> Stream m b) -> Stream m a -> Stream m b
parConcatMap modifier f stream =
    Stream.fromStreamK
        $ parConcatMapK
            modifier (Stream.toStreamK . f) (Stream.toStreamK stream)

-- | Definition:
--
-- >>> concatMap = Stream.parConcatMap id
--
{-# INLINE concatMap #-}
concatMap :: MonadAsync m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap = parConcatMap id

-- | Map each element of the input to a stream and then concurrently evaluate
-- and interleave the resulting streams. Unlike 'concatMap' which prefers to
-- evaluate the earlier stream first, this schedules all streams in a round
-- robin fashion over the available threads.
--
-- >>> concatMapInterleave = Stream.parConcatMap (Stream.interleaved True)
--
-- When used with a single thread it behaves like serial interleaving:
--
-- >>> f cfg xs = Stream.fold Fold.toList $ Stream.parConcatMap (Stream.interleaved True . cfg) id $ Stream.fromList xs
--
-- >>> stream1 = Stream.fromList [1,2,3]
-- >>> stream2 = Stream.fromList [4,5,6]
-- >>> f (Stream.maxThreads 1) [stream1, stream2]
-- [1,4,2,5,3,6]
--
-- /Works only on finite number of streams/
--
{-# INLINE concatMapInterleave #-}
concatMapInterleave :: MonadAsync m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMapInterleave = parConcatMap (interleaved True)

-- | Evaluate the streams in the input stream concurrently and combine them.
--
-- >>> parConcat modifier = Stream.parConcatMap modifier id
--
{-# INLINE parConcat #-}
parConcat :: MonadAsync m =>
    (Config -> Config) -> Stream m (Stream m a) -> Stream m a
parConcat modifier = parConcatMap modifier id

-- | Definition:
--
-- >>> concat = Stream.parConcat id
--
{-# INLINE concat #-}
concat :: MonadAsync m => Stream m (Stream m a) -> Stream m a
concat = parConcat id

-------------------------------------------------------------------------------
-- concat Lists
-------------------------------------------------------------------------------

-- | Like 'parConcat' but works on a list of streams.
--
-- >>> parConcatList modifier = Stream.parConcat modifier . Stream.fromList
--
{-# INLINE parConcatList #-}
parConcatList :: MonadAsync m => (Config -> Config) -> [Stream m a] -> Stream m a
parConcatList modifier = parConcat modifier . Stream.fromList

-- | Like 'concat' but works on a list of streams.
--
-- >>> parLazy = Stream.parConcatList id
--
{-# INLINE parLazy #-}
parLazy :: MonadAsync m => [Stream m a] -> Stream m a
parLazy = parConcatList id

-- | Like 'parLazy' but interleaves the streams fairly instead of prioritizing
-- the left stream. This schedules all streams in a round robin fashion over
-- limited number of threads.
--
-- >>> parLazyInterleaved = Stream.parConcatList (Stream.interleaved True)
--
{-# INLINE parLazyInterleaved #-}
parLazyInterleaved :: MonadAsync m => [Stream m a] -> Stream m a
parLazyInterleaved = parConcatList (interleaved True)

-- | Like 'parLazy' but with 'ordered' on.
--
-- >>> parLazyOrdered = Stream.parConcatList (Stream.ordered True)
--
{-# INLINE parLazyOrdered #-}
parLazyOrdered :: MonadAsync m => [Stream m a] -> Stream m a
parLazyOrdered = parConcatList (ordered True)

-- | Like 'parLazy' but with 'eager' on.
--
-- >>> parEager = Stream.parConcatList (Stream.eager True)
--
{-# INLINE parEager #-}
parEager :: MonadAsync m => [Stream m a] -> Stream m a
parEager = parConcatList (eager True)

-- | Like 'parEager' but stops the output as soon as the first stream stops.
--
-- >>> parEagerFst = Stream.parConcatList (Stream.eager True . Stream.stopWhen Stream.FirstStops)
--
{-# INLINE parEagerFst #-}
parEagerFst :: MonadAsync m => [Stream m a] -> Stream m a
parEagerFst = parConcatList (eager True . stopWhen FirstStops)

-- | Like 'parEager' but stops the output as soon as any of the two streams
-- stops.
--
-- Definition:
--
-- >>> parEagerMin = Stream.parConcatList (Stream.eager True . Stream.stopWhen Stream.AnyStops)
--
{-# INLINE parEagerMin #-}
parEagerMin :: MonadAsync m => [Stream m a] -> Stream m a
parEagerMin = parConcatList (eager True . stopWhen AnyStops)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

-- | Apply an argument stream to a function stream concurrently. Uses a
-- shared channel for all individual applications within a stream application.
{-# INLINE parApply #-}
{-# SPECIALIZE parApply ::
   (Config -> Config) -> Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
parApply :: MonadAsync m =>
    (Config -> Config) -> Stream m (a -> b) -> Stream m a -> Stream m b
parApply modifier stream1 stream2 =
    parConcatMap
        modifier
        (\g -> parConcatMap modifier (Stream.fromPure . g) stream2)
        stream1

{-# INLINE apply #-}
{-# SPECIALIZE apply :: Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
apply :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
apply = parApply id

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

-- |
-- Definition:
--
-- >>> parMapM modifier f = Stream.parConcatMap modifier (Stream.fromEffect . f)
--
-- Example, the following example finishes in 1 second as all actions run in
-- parallel. Even though results are available out of order they are ordered
-- due to the config option::
--
-- >>> f x = delay x >> return x
-- >>> Stream.fold Fold.toList $ Stream.parMapM (Stream.ordered True) f $ Stream.fromList [3,2,1]
-- 1 sec
-- 2 sec
-- 3 sec
-- [3,2,1]
--
{-# INLINE parMapM #-}
parMapM :: MonadAsync m =>
    (Config -> Config) -> (a -> m b) -> Stream m a -> Stream m b
parMapM modifier f = parConcatMap modifier (Stream.fromEffect . f)

-- |
-- Definition:
--
-- >>> mapM = Stream.parMapM id
--
{-# INLINE mapM #-}
mapM :: MonadAsync m => (a -> m b) -> Stream m a -> Stream m b
mapM = parMapM id

-- |
-- >>> parSequence modifier = Stream.parMapM modifier id
--
{-# INLINE parSequence #-}
parSequence :: MonadAsync m =>
    (Config -> Config) -> Stream m (m a) -> Stream m a
parSequence modifier = parMapM modifier id

-- |
-- >>> sequence = Stream.parSequence id
--
{-# INLINE sequence #-}
sequence :: MonadAsync m =>
    Stream m (m a) -> Stream m a
sequence = parSequence id

-- | Evaluates the streams being zipped in separate threads than the consumer.
-- The zip function is evaluated in the consumer thread.
--
-- >>> parZipWithM cfg f m1 m2 = Stream.zipWithM f (Stream.parEval cfg m1) (Stream.parEval cfg m2)
--
-- Multi-stream concurrency options won't apply here, see the notes in
-- 'parEval'.
--
-- If you want to evaluate the zip function as well in a separate thread, you
-- can use a 'parEval' on 'parZipWithM'.
--
{-# INLINE parZipWithM #-}
parZipWithM :: MonadAsync m
    => (Config -> Config)
    -> (a -> b -> m c)
    -> Stream m a
    -> Stream m b
    -> Stream m c
parZipWithM cfg f m1 m2 = Stream.zipWithM f (parEval cfg m1) (parEval cfg m2)

-- |
-- >>> parZipWith cfg f = Stream.parZipWithM cfg (\a b -> return $ f a b)
--
-- >>> m1 = Stream.fromList [1,2,3]
-- >>> m2 = Stream.fromList [4,5,6]
-- >>> Stream.fold Fold.toList $ Stream.parZipWith id (,) m1 m2
-- [(1,4),(2,5),(3,6)]
--
{-# INLINE parZipWith #-}
parZipWith :: MonadAsync m
    => (Config -> Config)
    -> (a -> b -> c)
    -> Stream m a
    -> Stream m b
    -> Stream m c
parZipWith cfg f = parZipWithM cfg (\a b -> return $ f a b)

-------------------------------------------------------------------------------
-- concatIterate
-------------------------------------------------------------------------------

-- | Same as 'concatIterate' but concurrent.
--
-- /Pre-release/
{-# INLINE parConcatIterate #-}
parConcatIterate :: MonadAsync m =>
       (Config -> Config)
    -> (a -> Stream m a)
    -> Stream m a
    -> Stream m a
parConcatIterate modifier f input =
     Stream.fromStreamK
        $ withChannelK modifier (Stream.toStreamK input) iterateStream

    where

    iterateStream channel stream =
        parConcatMapChanKGeneric modifier channel (generate channel) stream

    generate channel x =
        -- XXX The channel q should be FIFO for DFS, otherwise it is BFS
        x `K.cons` iterateStream channel (Stream.toStreamK $ f x)

-------------------------------------------------------------------------------
-- Generate
-------------------------------------------------------------------------------

-- |
-- Definition:
--
-- >>> parRepeatM cfg = Stream.parSequence cfg . Stream.repeat
--
-- Generate a stream by repeatedly executing a monadic action forever.
{-# INLINE parRepeatM #-}
parRepeatM :: MonadAsync m => (Config -> Config) -> m a -> Stream m a
parRepeatM cfg = parSequence cfg . Stream.repeat

-- |
--  Definition:
--
-- >>> repeatM = Stream.parRepeatM id
--
{-# INLINE repeatM #-}
repeatM :: MonadAsync m => m a -> Stream m a
repeatM = parRepeatM id

-- | Generate a stream by concurrently performing a monadic action @n@ times.
--
--  Definition:
--
-- >>> parReplicateM cfg n = Stream.parSequence cfg . Stream.replicate n
--
-- Example, 'parReplicateM' in the following example executes all the
-- replicated actions concurrently, thus taking only 1 second:
--
-- >>> Stream.fold Fold.drain $ Stream.parReplicateM id 10 $ delay 1
-- ...
--
{-# INLINE parReplicateM #-}
parReplicateM :: MonadAsync m => (Config -> Config) -> Int -> m a -> Stream m a
parReplicateM cfg n = parSequence cfg . Stream.replicate n

-- |
--  Definition:
--
-- >>> replicateM n = Stream.parReplicateM id
--
{-# INLINE replicateM #-}
replicateM :: MonadAsync m => Int -> m a -> Stream m a
replicateM = parReplicateM id

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
fromCallback setCallback = Stream.concatEffect $ do
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
-- > Stream.drain $ Stream.pollCounts (const True) (Stream.rollingMap (-) . Stream.delayPost 1) (Fold.drainBy print)
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
