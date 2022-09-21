-- |
-- Module      : Streamly.Internal.Data.Stream.Parallel
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Parallel
    (
    -- * Imports
    -- $setup

    -- * Types
      MonadAsync
    , SVarStopStyle (..)

    -- * Configuration
    , Config
    , maxBuffer
    -- maxYields

    -- * Combinators
    -- | Stream combinators.

    , eval
    , evalWith

    , mapM
    , mapMWith
    , sequence
    , sequenceWith

    -- XXX experimental binary ops, can use concat instead
    , parallel -- parallelBoth
    , parallelFst -- parallelLeft
    , parallelMin -- parallelRace
    , parallelWith

    , apply
    , concatList
    , concat
    , concatWith
    , concatMap
    , concatMapWith

    , newCallbackStream
    )
where

#include "inline.hs"

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as Stream
    (toStreamK, fromStreamK, toStreamD)

import Prelude hiding (mapM, sequence, concat, concatMap)
import Streamly.Internal.Data.Stream.Channel.Types
import Streamly.Internal.Data.Stream.Parallel.Channel
import Streamly.Internal.Data.Stream.Parallel.Channel.Type

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
-- >>> import qualified Streamly.Internal.Data.Stream.Parallel as Parallel
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
    Stream.fromStreamK $ evalWithD cfg $ Stream.toStreamD stream
    -- Stream.fromStreamK $ evalWithK cfg $ Stream.toStreamK stream

-- | Evaluate a stream asynchronously using a channel and serve the consumer
-- from the evaluation buffer.
--
-- >>> eval = Parallel.evalWith id
--
{-# INLINE eval #-}
eval :: MonadAsync m => Stream m a -> Stream m a
eval = evalWith id

-------------------------------------------------------------------------------
-- Concurrent combinators for "Stream" type
-------------------------------------------------------------------------------

-- | Like 'parallel' but with a Config modifier.
{-# INLINE parallelWith #-}
parallelWith :: MonadAsync m =>
    SVarStopStyle -> (Config -> Config) -> Stream m a -> Stream m a -> Stream m a
parallelWith stopStyle modifier stream1 stream2 =
    Stream.fromStreamK
        $ appendWithD stopStyle
            modifier (Stream.toStreamD stream1) (Stream.toStreamD stream2)
{-
        $ appendWithK stopStyle
            modifier (Stream.toStreamK stream1) (Stream.toStreamK stream2)
-}

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
-- >>> parallel = Parallel.parallelWith Parallel.StopNone id
--
-- The following code finishes in 4 seconds:
--
-- >>> stream1 = Stream.fromEffect (delay 4)
-- >>> stream2 = Stream.fromEffect (delay 2)
-- >>> Stream.fold Fold.toList $ stream1 `Parallel.parallel` stream2
-- 2 sec
-- 4 sec
-- [2,4]
--
{-# INLINE parallel #-}
parallel :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallel = parallelWith StopNone id

-- | Like `parallel` but stops the output as soon as the first stream stops.
--
-- >>> parallelFst = Parallel.parallelWith Parallel.StopBy id
--
-- /Pre-release/
{-# INLINE parallelFst #-}
parallelFst :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelFst = parallelWith StopBy id

-- | Like `parallel` but stops the output as soon as any of the two streams
-- stops.
--
-- >>> parallelFst = Parallel.parallelWith Parallel.StopAny id
--
-- /Pre-release/
{-# INLINE parallelMin #-}
parallelMin :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallelMin = parallelWith StopAny id

-- XXX We can also have concatMap variants using Fst and Min style finishing.

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
-- and concatenate the resulting streams. Output from the streams are used as
-- they arrive.
--
-- >>> concatMap = Parallel.concatMapWith id
--
-- >>> f cfg xs = Stream.fold Fold.toList $ Parallel.concatMapWith cfg id $ Stream.fromList xs
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
{-# INLINE concatMap #-}
concatMap :: MonadAsync m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap = concatMapWith id

-- | Like 'concat' but we can also specify the concurrent channel's
-- configuration parameters using Config modifiers.
--
-- >>> concatWith modifier = Parallel.concatMapWith modifier id
--
{-# INLINE concatWith #-}
concatWith :: MonadAsync m =>
    (Config -> Config) -> Stream m (Stream m a) -> Stream m a
concatWith modifier = concatMapWith modifier id

-- | Evaluate the streams in the input stream concurrently and combine them.
--
-- >>> concat = Parallel.concatWith id
--
{-# INLINE concat #-}
concat :: MonadAsync m => Stream m (Stream m a) -> Stream m a
concat = concatWith id

-- | Like 'concat' but works on a list of streams.
--
-- >>> concatList = Parallel.concat . Stream.fromList
--
{-# INLINE concatList #-}
concatList :: MonadAsync m => [Stream m a] -> Stream m a
concatList = concat . Stream.fromList

{-# INLINE apply #-}
{-# SPECIALIZE apply :: Stream IO (a -> b) -> Stream IO a -> Stream IO b #-}
apply :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
apply stream1 stream2 = concatMap (\g -> concatMap (pure . g) stream2) stream1

-- |
-- >>> mapMWith modifier f = Parallel.concatMapWith modifier (Stream.fromEffect . f)
--
{-# INLINE mapMWith #-}
mapMWith :: MonadAsync m =>
    (Config -> Config) -> (a -> m b) -> Stream m a -> Stream m b
mapMWith modifier f = concatMapWith modifier (Stream.fromEffect . f)

-- |
-- >>> mapM = Parallel.mapMWith id
{-# INLINE mapM #-}
mapM :: MonadAsync m => (a -> m b) -> Stream m a -> Stream m b
mapM = mapMWith id

-- |
-- >>> sequenceWith modifier = Parallel.mapMWith modifier id
--
{-# INLINE sequenceWith #-}
sequenceWith :: MonadAsync m =>
    (Config -> Config) -> Stream m (m a) -> Stream m a
sequenceWith modifier = mapMWith modifier id

-- |
-- >>> sequence = Parallel.sequenceWith id
--
{-# INLINE sequence #-}
sequence :: MonadAsync m =>
    Stream m (m a) -> Stream m a
sequence = sequenceWith id
