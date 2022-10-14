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
    , eagerEval
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
    , inspectMode

    -- * Combinators
    -- | Stream combinators using a concurrent channel

    -- ** Evaluate
    -- | Evaluate a stream concurrently using a channel.
    , eval
    , evalWith
    -- Add unfoldrM/iterateM?

    -- ** Map
    -- | Use a single channel to evaluate all actions.
    , mapM
    , mapMWith
    , sequence
    , sequenceWith
    -- Add repeatM/replicateM?

    -- ** Combine two
    -- | Use a channel for each pair.
    -- combine/concur/conjoin
    , append
    , interleave
    , ahead
    , parallel
    , parallelFst
    , parallelMin
    , combineWith

    -- ** Apply
    -- | Use a separate channel for each application.
    , apply
    , applyWith

    -- ** Combine many
    -- | Share a single channel across many streams.
    , concatList
    , concat
    , concatWith
    , concatMap
    , concatMapInterleave
    , concatMapWith
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Internal.Data.Stream.Concurrent.Channel
import Prelude hiding (mapM, sequence, concat, concatMap)

-- $setup
--
-- Imports for example snippets in this module.
--
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import qualified Streamly.Data.Array.Unboxed as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.Concurrent as Async
-- >>> import Prelude hiding (concatMap, concat)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

-------------------------------------------------------------------------------
-- Useful operations
-------------------------------------------------------------------------------

-- | Like 'eval' but can specify a config modifier to change the concurrent
-- channel parameters.
{-# INLINE evalWith #-}
evalWith :: MonadAsync m => (Config -> Config) -> Stream m a -> Stream m a
evalWith cfg stream =
    -- Stream.fromStreamD $ evalWithD cfg $ Stream.toStreamD stream
    Stream.fromStreamK $ evalWithK cfg $ Stream.toStreamK stream

-- | Evaluate a stream asynchronously using a channel and serve the consumer
-- from the evaluation buffer.
--
-- >>> eval = Async.evalWith id
--
{-# INLINE eval #-}
eval :: MonadAsync m => Stream m a -> Stream m a
eval = evalWith id

-------------------------------------------------------------------------------
-- Concurrent combinators for "Stream" type
-------------------------------------------------------------------------------

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
-- >>> append = Async.combineWith id
--
-- The following code finishes in 4 seconds:
--
-- >>> stream1 = Stream.fromEffect (delay 4)
-- >>> stream2 = Stream.fromEffect (delay 2)
-- >>> Stream.fold Fold.toList $ stream1 `Async.append` stream2
-- 2 sec
-- 4 sec
-- [2,4]
--
{-# INLINE append #-}
append :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
append = combineWith id

-- | Like 'append' but interleaves the streams fairly instead of prioritizing
-- the left stream. This schedules all streams in a round robin fashion over
-- limited number of threads.
--
-- >>> interleave = Async.combineWith Async.interleaved
--
{-# INLINE interleave #-}
interleave :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
interleave = combineWith interleaved

-- | Like 'append' but with 'ordered' on.
--
-- >>> ahead = Async.combineWith Async.ordered
--
{-# INLINE ahead #-}
ahead :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
ahead = combineWith ordered

-- | Like 'append' but with 'eagerEval' on.
--
-- >>> parallel = Async.combineWith Async.eagerEval
--
{-# INLINE parallel #-}
parallel :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallel = combineWith eagerEval

-- | Like 'parallel' but stops the output as soon as the first stream stops.
--
-- >>> parallelFst = Async.combineWith (Async.eagerEval . Async.stopWhen Async.FirstStops)
--
-- /Pre-release/
{-# INLINE parallelFst #-}
parallelFst :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelFst = combineWith (eagerEval . stopWhen FirstStops)

-- | Like 'parallel' but stops the output as soon as any of the two streams
-- stops.
--
-- >>> parallelMin = Async.combineWith (Async.eagerEval . Async.stopWhen Async.AnyStops)
--
-- /Pre-release/
{-# INLINE parallelMin #-}
parallelMin :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelMin = combineWith (eagerEval . stopWhen AnyStops)

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
-- >>> concatMap = Async.concatMapWith id
--
-- >>> f cfg xs = Stream.fold Fold.toList $ Async.concatMapWith cfg id $ Stream.fromList xs
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
-- >>> f (Async.maxThreads 2) [stream1, stream2, stream3]
-- ...
-- [2,1,4]
--
-- When used with a Single thread it behaves like serial concatMap:
--
-- >>> f (Async.maxThreads 1) [stream1, stream2, stream3]
-- ...
-- [4,2,1]
--
-- >>> stream1 = Stream.fromList [1,2,3]
-- >>> stream2 = Stream.fromList [4,5,6]
-- >>> f (Async.maxThreads 1) [stream1, stream2]
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
-- >>> concatMapInterleave = Async.concatMapWith Async.interleaved
--
-- When used with a single thread it behaves like serial interleaving:
--
-- >>> f cfg xs = Stream.fold Fold.toList $ Async.concatMapWith (Async.interleaved . cfg) id $ Stream.fromList xs
--
-- >>> stream1 = Stream.fromList [1,2,3]
-- >>> stream2 = Stream.fromList [4,5,6]
-- >>> f (Async.maxThreads 1) [stream1, stream2]
-- [1,4,2,5,3,6]
--
-- /Works only on finite number of streams/
--
{-# INLINE concatMapInterleave #-}
concatMapInterleave :: MonadAsync m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMapInterleave = concatMapWith interleaved

-- | Like 'concat' but we can also specify the concurrent channel's
-- configuration parameters using Config modifiers.
--
-- >>> concatWith modifier = Async.concatMapWith modifier id
--
{-# INLINE concatWith #-}
concatWith :: MonadAsync m =>
    (Config -> Config) -> Stream m (Stream m a) -> Stream m a
concatWith modifier = concatMapWith modifier id

-- | Evaluate the streams in the input stream concurrently and combine them.
--
-- >>> concat = Async.concatWith id
--
{-# INLINE concat #-}
concat :: MonadAsync m => Stream m (Stream m a) -> Stream m a
concat = concatWith id

-- | Like 'concat' but works on a list of streams.
--
-- >>> concatList = Async.concat . Stream.fromList
--
{-# INLINE concatList #-}
concatList :: MonadAsync m => [Stream m a] -> Stream m a
concatList = concat . Stream.fromList

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

-- |
-- >>> mapMWith modifier f = Async.concatMapWith modifier (Stream.fromEffect . f)
--
{-# INLINE mapMWith #-}
mapMWith :: MonadAsync m =>
    (Config -> Config) -> (a -> m b) -> Stream m a -> Stream m b
mapMWith modifier f = concatMapWith modifier (Stream.fromEffect . f)

-- |
-- >>> mapM = Async.mapMWith id
{-# INLINE mapM #-}
mapM :: MonadAsync m => (a -> m b) -> Stream m a -> Stream m b
mapM = mapMWith id

-- |
-- >>> sequenceWith modifier = Async.mapMWith modifier id
--
{-# INLINE sequenceWith #-}
sequenceWith :: MonadAsync m =>
    (Config -> Config) -> Stream m (m a) -> Stream m a
sequenceWith modifier = mapMWith modifier id

-- |
-- >>> sequence = Async.sequenceWith id
--
{-# INLINE sequence #-}
sequence :: MonadAsync m =>
    Stream m (m a) -> Stream m a
sequence = sequenceWith id
