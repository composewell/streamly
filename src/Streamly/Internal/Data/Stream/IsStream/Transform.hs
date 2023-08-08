{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Transform
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.IsStream.Transform {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    (
    -- * Piping
    -- | Pass through a 'Pipe'.
      transform

    -- * Folding
    , foldrS
    , foldrSShared
    , foldrT

    -- * Mapping
    -- | Stateless one-to-one maps.
    , map
    , sequence
    , mapM
    , smapM

    -- * Mapping Side Effects (Observation)
    -- | See also the intersperse*_ combinators.
    , trace
    , trace_
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapAsyncK
    , distributeAsync_
    , pollCounts

    -- * Scanning By 'Fold'
    , scan
    , scanMany
    , postscan

    -- * Scanning
    -- | Left scans. Stateful, mostly one-to-one maps.
    , scanl'
    , scanlM'
    , scanlMAfter'
    , postscanlMAfter'
    , postscanl'
    , postscanlM'
    , prescanl'
    , prescanlM'
    , scanl1'
    , scanl1M'

    -- XXX Once we have pipes the contravariant transformations can be
    -- represented by attaching pipes before a transformation.
    --
    -- , lscanl'
    -- , lscanlM'
    -- , lscanl1'
    -- , lscanl1M'
    --
    -- , lpostscanl'
    -- , lpostscanlM'
    -- , lprescanl'
    -- , lprescanlM'

    -- * Filtering
    -- | Produce a subset of the stream using criteria based on the values of
    -- the elements. We can use a concatMap and scan for filtering but these
    -- combinators are more efficient and convenient.

    , with
    , deleteBy
    , filter
    , filterM
    , uniq
    , uniqBy
    , nubBy
    , prune
    , repeated

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.

    , take
    , takeLast
    , takeLastInterval
    , takeWhile
    , takeWhileM
    , takeWhileLast
    , takeWhileAround
    , drop
    , dropLast
    , dropLastInterval
    , dropWhile
    , dropWhileM
    , dropWhileLast
    , dropWhileAround

    -- * Inserting Elements
    -- | Produce a superset of the stream. This is the opposite of
    -- filtering/sampling.  We can always use concatMap and scan for inserting
    -- but these combinators are more efficient and convenient.

    -- Element agnostic (Opposite of sampling)
    , intersperse
    , intersperseM -- XXX naming
    , intersperseMWith

    , intersperseMSuffix
    , intersperseMSuffixWith
    , interjectSuffix

    -- , interspersePrefix
    -- , interspersePrefixBySpan

    -- * Inserting Side Effects/Time
    , intersperseM_ -- XXX naming
    , delay
    , intersperseMSuffix_
    , delayPost
    , intersperseMPrefix_
    , delayPre

    -- * Element Aware Insertion
    -- | Opposite of filtering
    , insertBy
    -- , intersperseByBefore
    -- , intersperseByAfter

    -- * Reordering
    , reverse
    , reverse'
    , reassembleBy

    -- * Position Indexing
    , indexed
    , indexedR

    -- * Time Indexing
    , timestamped
    , timestampWith
    -- , timestampedR -- timer
    , timeIndexed
    , timeIndexWith

    -- * Searching
    , findIndices -- XXX indicesBy
    , elemIndices -- XXX indicesOf

    -- * Rolling map
    -- | Map using the previous element.
    , rollingMapM
    , rollingMap
    , rollingMap2

    -- * Maybe Streams
    -- Move these to Streamly.Data.Maybe.Stream?
    , catMaybes -- XXX justs (like lefts/rights)
    , mapMaybe
    , mapMaybeM

    -- * Either Streams
    -- Move these to Streamly.Data.Either.Stream?
    , lefts
    , rights
    , both

    -- * Concurrent Evaluation
    -- ** Concurrent Pipelines
    -- | Run streaming stages concurrently.

    , mkAsync
    , mkParallel
    , applyAsync
    , (|$)
    , (|&)

    -- ** Concurrency Control
    , maxThreads

    -- ** Buffering and Sampling
    -- | Evaluate strictly using a buffer of results.  When the buffer becomes
    -- full we can block, drop the new elements, drop the oldest element and
    -- insert the new at the end or keep dropping elements uniformly to match
    -- the rate of the consumer.
    , maxBuffer
    , sampleOld
    , sampleNew
    , sampleRate

    -- ** Rate Limiting
    -- | Evaluate the stream at uniform intervals to maintain a specified
    -- evaluation rate.
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Diagnostics
    , inspectMode

    -- * Deprecated
    , scanx
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Either (isLeft, isRight, fromLeft, fromRight)
import Data.Kind (Type)
import Data.Maybe (isJust, fromJust)
import Streamly.Internal.Data.Fold (Fold (..))
import Streamly.Internal.Data.Pipe (Pipe (..))
import Streamly.Internal.Data.Stream.IsStream.Combinators
      ( inspectMode, maxBuffer, maxThreads, rate, avgRate, minRate
      , maxRate, constRate)
import Streamly.Internal.Data.Stream.IsStream.Common
    ( absTimesWith
    , drop
    , findIndices
    , map
    , postscanlM'
    , relTimesWith
    , reverse
    , reverse'
    , scanlMAfter'
    , postscanlMAfter'
    , smapM
    , take
    , takeWhile
    , interjectSuffix
    , intersperseM
    , mkAsync
    , mkParallel
    , zipWith
    )
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream(..), fromStreamD, toStreamD, toConsK)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.SVar (Rate(..))
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.Concurrent as Concur
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream as D
    (transform, foldrT, tap, tapOffsetEvery, mapM, scan
    , scanMany, postscan, scanlx', scanlM', scanl', postscanl', prescanl'
    , prescanlM', scanl1M', scanl1', filter, filterM, uniq, deleteBy, takeWhileM
    , dropWhile, dropWhileM, insertBy, intersperse
    , intersperseM_, intersperseMSuffix, intersperseMSuffix_
    , intersperseMSuffixWith, indexed, indexedR, rollingMap, rollingMapM
    , rollingMap2, mapMaybe, mapMaybeM)
import qualified Streamly.Internal.Data.StreamK as K
    (foldrS, foldrSShared, mapMWith)
import qualified Prelude

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence
       , reverse, foldr1 , scanl, scanl1, zipWith)

--
-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Data.Function ((&))
-- >>> import Streamly.Prelude ((|$))
-- >>> import Prelude hiding ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence, reverse, foldr1 , scanl, scanl1)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
--
-- >>> hSetBuffering stdout LineBuffering

------------------------------------------------------------------------------
-- Piping
------------------------------------------------------------------------------

-- | Use a 'Pipe' to transform a stream.
--
-- /Pre-release/
--
{-# INLINE transform #-}
transform :: (IsStream t, Monad m) => Pipe m a b -> t m a -> t m b
transform pipe xs = fromStreamD $ D.transform pipe (toStreamD xs)

------------------------------------------------------------------------------
-- Transformation Folds
------------------------------------------------------------------------------

-- | Right fold to a streaming monad.
--
-- > foldrS Stream.cons Stream.nil === id
--
-- 'foldrS' can be used to perform stateless stream to stream transformations
-- like map and filter in general. It can be coupled with a scan to perform
-- stateful transformations. However, note that the custom map and filter
-- routines can be much more efficient than this due to better stream fusion.
--
-- >>> Stream.toList $ Stream.foldrS Stream.cons Stream.nil $ Stream.fromList [1..5]
-- [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> Stream.toList $ Stream.foldrS (\x xs -> if odd x then (Stream.fromPure True) else xs) (Stream.fromPure False) $ (Stream.fromList (2:4:5:undefined) :: Stream.SerialT IO Int)
-- [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> Stream.toList $ Stream.foldrS (\x xs -> if odd x then (x + 2) `Stream.cons` xs else xs) Stream.nil $ (Stream.fromList [1..5] :: Stream.SerialT IO Int)
-- [3,5,7]
--
-- 'foldrM' can also be represented in terms of 'foldrS', however, the former
-- is much more efficient:
--
-- > foldrM f z s = runIdentityT $ foldrS (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- /Pre-release/
{-# INLINE foldrS #-}
foldrS :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrS f z xs =
    fromStream
        $ K.foldrS
            (\y ys -> toStream $ f y (fromStream ys))
            (toStream z)
            (toStream xs)

{-# INLINE foldrSShared #-}
foldrSShared :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrSShared f z xs =
    fromStream
        $ K.foldrSShared
            (\y ys -> toStream $ f y (fromStream ys))
            (toStream z)
            (toStream xs)

-- | Right fold to a transformer monad.  This is the most general right fold
-- function. 'foldrS' is a special case of 'foldrT', however 'foldrS'
-- implementation can be more efficient:
--
-- > foldrS = foldrT
-- > foldrM f z s = runIdentityT $ foldrT (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- 'foldrT' can be used to translate streamly streams to other transformer
-- monads e.g.  to a different streaming type.
--
-- /Pre-release/
{-# INLINE foldrT #-}
foldrT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> t m a -> s m b
foldrT f z s = D.foldrT f z (toStreamD s)

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

-- |
-- @
-- mapM f = sequence . map f
-- @
--
-- Apply a monadic function to each element of the stream and replace it with
-- the output of the resulting action.
--
-- @
-- >>> drain $ Stream.mapM putStr $ Stream.fromList ["a", "b", "c"]
-- abc
--
-- >>> :{
--    drain $ Stream.replicateM 10 (return 1)
--      & (fromSerial . Stream.mapM (\x -> threadDelay 1000000 >> print x))
-- :}
-- 1
-- ...
-- 1
--
-- > drain $ Stream.replicateM 10 (return 1)
--  & (fromAsync . Stream.mapM (\x -> threadDelay 1000000 >> print x))
-- @
--
-- /Concurrent (do not use with 'fromParallel' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE_EARLY mapM #-}
mapM :: forall t m a b. (IsStream t, MonadAsync m) =>
    (a -> m b) -> t m a -> t m b
mapM f = fromStream . K.mapMWith (toConsK (consM @t)) f . toStream

{-# RULES "mapM serial" mapM = mapMSerial #-}
{-# INLINE mapMSerial #-}
mapMSerial :: Monad m => (a -> m b) -> SerialT m a -> SerialT m b
mapMSerial = Serial.mapM

-- |
-- @
-- sequence = mapM id
-- @
--
-- Replace the elements of a stream of monadic actions with the outputs of
-- those actions.
--
-- @
-- >>> drain $ Stream.sequence $ Stream.fromList [putStr "a", putStr "b", putStrLn "c"]
-- abc
--
-- >>> :{
-- drain $ Stream.replicateM 3 (return $ threadDelay 1000000 >> print 1)
--  & (fromSerial . Stream.sequence)
-- :}
-- 1
-- 1
-- 1
--
-- >>> :{
-- drain $ Stream.replicateM 3 (return $ threadDelay 1000000 >> print 1)
--  & (fromAsync . Stream.sequence)
-- :}
-- 1
-- 1
-- 1
--
-- @
--
-- /Concurrent (do not use with 'fromParallel' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence = mapM id

------------------------------------------------------------------------------
-- Mapping side effects
------------------------------------------------------------------------------

-- | Tap the data flowing through a stream into a 'Fold'. For example, you may
-- add a tap to log the contents flowing through the stream. The fold is used
-- only for effects, its result is discarded.
--
-- @
--                   Fold m a b
--                       |
-- -----stream m a ---------------stream m a-----
--
-- @
--
-- >>> Stream.drain $ Stream.tap (Fold.drainBy print) (Stream.enumerateFromTo 1 2)
-- 1
-- 2
--
-- Compare with 'trace'.
--
-- @since 0.7.0
{-# INLINE tap #-}
tap :: (IsStream t, Monad m) => FL.Fold m a b -> t m a -> t m a
tap f xs = fromStreamD $ D.tap f (toStreamD xs)

-- XXX Remove this. It can be expressed in terms of Fold.sampleFromThen.
--
-- | @tapOffsetEvery offset n@ taps every @n@th element in the stream
-- starting at @offset@. @offset@ can be between @0@ and @n - 1@. Offset 0
-- means start at the first element in the stream. If the offset is outside
-- this range then @offset `mod` n@ is used as offset.
--
-- >>> Stream.drain $ Stream.tapOffsetEvery 0 2 (Fold.rmapM print Fold.toList) $ Stream.enumerateFromTo 0 10
-- [0,2,4,6,8,10]

--
-- /Pre-release/
--
{-# INLINE tapOffsetEvery #-}
tapOffsetEvery :: (IsStream t, Monad m)
    => Int -> Int -> FL.Fold m a b -> t m a -> t m a
tapOffsetEvery offset n f xs =
    fromStreamD $ D.tapOffsetEvery offset n f (toStreamD xs)

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
-- >>> Stream.drain $ Stream.tapAsync (Fold.drainBy print) (Stream.enumerateFromTo 1 2)
-- 1
-- 2
--
-- @
--
-- Exceptions from the concurrently running fold are propagated to the current
-- computation.  Note that, because of buffering in the fold, exceptions may be
-- delayed and may not correspond to the current element being processed in the
-- parent stream, but we guarantee that before the parent stream stops the tap
-- finishes and all exceptions from it are drained.
--
-- >>> tapAsync f = Stream.tapAsyncK (Stream.fold f . Stream.adapt)
--
-- Compare with 'tap'.
--
-- /Pre-release/
{-# INLINE tapAsync #-}
tapAsync :: (IsStream t, MonadAsync m) => FL.Fold m a b -> t m a -> t m a
tapAsync f xs = fromStreamD $ Par.tapAsyncF f (toStreamD xs)

-- XXX We should keep only tapAsyncK as tapAsync can be implemented in terms
-- of tapAsyncK and it has better performance. Rename this to tapAsync.

-- | Like 'tapAsyncF' but uses a stream fold function instead of a 'Fold' type.
--
-- /Pre-release/
{-# INLINE tapAsyncK #-}
tapAsyncK :: (IsStream t, MonadAsync m) => (t m a -> m b) -> t m a -> t m a
tapAsyncK f m = fromStream $ Par.tapAsyncK (f . fromStream) (toStream m)

-- XXX rename this to tapManyAsync_?
--
-- | Concurrently distribute a stream to a collection of fold functions,
-- discarding the outputs of the folds.
--
-- @
-- > Stream.drain $ Stream.distributeAsync_ [Stream.mapM_ print, Stream.mapM_ print] (Stream.enumerateFromTo 1 2)
-- 1
-- 2
-- 1
-- 2
--
-- @
--
-- @
-- distributeAsync_ = flip (foldr tapAsync)
-- @
--
-- /Pre-release/
--
{-# INLINE distributeAsync_ #-}
distributeAsync_ :: (Foldable f, IsStream t, MonadAsync m)
    => f (t m a -> m b) -> t m a -> t m a
distributeAsync_ = flip (Prelude.foldr tapAsyncK)

-- | @pollCounts predicate transform fold stream@ counts those elements in the
-- stream that pass the @predicate@. The resulting count stream is sent to
-- another thread which transforms it using @transform@ and then folds it using
-- @fold@.  The thread is automatically cleaned up if the stream stops or
-- aborts due to exception.
--
-- For example, to print the count of elements processed every second:
--
-- @
-- > Stream.drain $ Stream.pollCounts (const True) (Stream.rollingMap (-) . Stream.delayPost 1) (FLold.drainBy print)
--           $ Stream.enumerateFrom 0
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE pollCounts #-}
pollCounts ::
       (IsStream t, MonadAsync m)
    => (a -> Bool)
    -> (t m Int -> m b)
    -> t m a
    -> t m a
pollCounts predicate f xs =
      fromStreamD
    $ Concur.parTapCount predicate (f . fromStreamD)
    $ toStreamD xs

-- | Apply a monadic function to each element flowing through the stream and
-- discard the results.
--
-- >>> Stream.drain $ Stream.trace print (Stream.enumerateFromTo 1 2)
-- 1
-- 2
--
-- Compare with 'tap'.
--
-- @since 0.7.0
{-# INLINE trace #-}
trace :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m a
trace f = mapM (\x -> void (f x) >> return x)

-- | Perform a side effect before yielding each element of the stream and
-- discard the results.
--
-- @
-- >>> Stream.drain $ Stream.trace_ (print "got here") (Stream.enumerateFromTo 1 2)
-- "got here"
-- "got here"
--
-- @
--
-- Same as 'intersperseMPrefix_' but always serial.
--
-- See also: 'trace'
--
-- /Pre-release/
{-# INLINE trace_ #-}
trace_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
trace_ eff = fromStreamD . D.mapM (\x -> eff >> return x) . toStreamD

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

-- | Scan a stream using the given monadic fold.
--
-- >>> Stream.toList $ Stream.takeWhile (< 10) $ Stream.scan Fold.sum (Stream.fromList [1..10])
-- [0,1,3,6]
--
-- @since 0.7.0
{-# INLINE scan #-}
scan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
scan fld m = fromStreamD $ D.scan fld $ toStreamD m

-- | Like 'scan' but restarts scanning afresh when the scanning fold
-- terminates.
--
-- /Pre-release/
{-# INLINE scanMany #-}
scanMany :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
scanMany fld m = fromStreamD $ D.scanMany fld $ toStreamD m

-- | Postscan a stream using the given monadic fold.
--
-- The following example extracts the input stream up to a point where the
-- running average of elements is no more than 10:
--
-- >>> import Data.Maybe (fromJust)
-- >>> let avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> :{
--  Stream.toList
--   $ Stream.map (fromJust . fst)
--   $ Stream.takeWhile (\(_,x) -> x <= 10)
--   $ Stream.postscan (Fold.tee Fold.last avg) (Stream.enumerateFromTo 1.0 100.0)
-- :}
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0]
--
-- @since 0.7.0
{-# INLINE postscan #-}
postscan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
postscan fld = fromStreamD . D.postscan fld . toStreamD

------------------------------------------------------------------------------
-- Scanning - Transformation by Folding
------------------------------------------------------------------------------

-- XXX It may be useful to have a version of scan where we can keep the
-- accumulator independent of the value emitted. So that we do not necessarily
-- have to keep a value in the accumulator which we are not using. We can pass
-- an extraction function that will take the accumulator and the current value
-- of the element and emit the next value in the stream. That will also make it
-- possible to modify the accumulator after using it. In fact, the step function
-- can return new accumulator and the value to be emitted. The signature would
-- be more like mapAccumL. Or we can change the signature of scanx to
-- accommodate this.
--
-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
--
-- /Since 0.2.0/
--
-- /Since: 0.7.0 (Monad m constraint)/
{-# DEPRECATED scanx "Please use scanl followed by map instead." #-}
{-# INLINE scanx #-}
scanx :: (IsStream t, Monad m) => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx step begin done = fromStreamD . D.scanlx' step begin done . toStreamD

-- XXX this needs to be concurrent
-- XXX because of the use of D.cons for appending, scanlM' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.
--
-- | Like 'scanl'' but with a monadic step function and a monadic seed.
--
-- /Since: 0.4.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE scanlM' #-}
scanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
scanlM' step begin m = fromStreamD $ D.scanlM' step begin $ toStreamD m

-- XXX because of the use of D.cons for appending, scanl' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.

-- | Strict left scan. Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- @
-- >>> Stream.toList $ Stream.scanl' (+) 0 $ fromList [1,2,3,4]
-- [0,1,3,6,10]
--
-- @
--
-- @
-- >>> Stream.toList $ Stream.scanl' (flip (:)) [] $ Stream.fromList [1,2,3,4]
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
--
-- @
--
-- The output of 'scanl'' is the initial value of the accumulator followed by
-- all the intermediate steps and the final result of 'foldl''.
--
-- By streaming the accumulated state after each fold step, we can share the
-- state across multiple stages of stream composition. Each stage can modify or
-- extend the state, do some processing with it and emit it for the next stage,
-- thus modularizing the stream processing. This can be useful in
-- stateful or event-driven programming.
--
-- Consider the following monolithic example, computing the sum and the product
-- of the elements in a stream in one go using a @foldl'@:
--
-- @
-- >>> Stream.foldl' (\(s, p) x -> (s + x, p * x)) (0,1) $ Stream.fromList [1,2,3,4]
-- (10,24)
--
-- @
--
-- Using @scanl'@ we can make it modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- @
-- >>> :{
--   Stream.foldl' (\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ Stream.scanl' (\(s, _) x -> (s + x, x)) (0,1)
--   $ Stream.fromList [1,2,3,4]
-- :}
-- (10,24)
--
-- @
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
-- >>> scanl' step z = scan (Fold.foldl' step z)
-- >>> scanl' f z xs = scanlM' (\a b -> return (f a b)) (return z) xs
-- >>> scanl' f z xs = z `Stream.cons` postscanl' f z xs
--
-- See also: 'usingStateT'
--
-- @since 0.2.0
{-# INLINE scanl' #-}
scanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
scanl' step z m = fromStreamD $ D.scanl' step z $ toStreamD m

-- | Like 'scanl'' but does not stream the initial value of the accumulator.
--
-- >>> postscanl' step z = postscan (Fold.foldl' step z)
-- >>> postscanl' f z = postscanlM' (\a b -> return (f a b)) (return z)
-- >>> postscanl' f z xs = Stream.drop 1 $ Stream.scanl' f z xs
--
-- @since 0.7.0
{-# INLINE postscanl' #-}
postscanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
postscanl' step z m = fromStreamD $ D.postscanl' step z $ toStreamD m

-- XXX prescanl does not sound very useful, enable only if there is a
-- compelling use case.
--
-- | Like scanl' but does not stream the final value of the accumulator.
--
-- /Pre-release/
{-# INLINE prescanl' #-}
prescanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
prescanl' step z m = fromStreamD $ D.prescanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like prescanl' but with a monadic step function and a monadic seed.
--
-- /Pre-release/
{-# INLINE prescanlM' #-}
prescanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
prescanlM' step z m = fromStreamD $ D.prescanlM' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like 'scanl1'' but with a monadic step function.
--
-- @since 0.6.0
{-# INLINE scanl1M' #-}
scanl1M' :: (IsStream t, Monad m) => (a -> a -> m a) -> t m a -> t m a
scanl1M' step m = fromStreamD $ D.scanl1M' step $ toStreamD m

-- | Like 'scanl'' but for a non-empty stream. The first element of the stream
-- is used as the initial value of the accumulator. Does nothing if the stream
-- is empty.
--
-- @
-- >>> Stream.toList $ Stream.scanl1' (+) $ fromList [1,2,3,4]
-- [1,3,6,10]
--
-- @
--
-- @since 0.6.0
{-# INLINE scanl1' #-}
scanl1' :: (IsStream t, Monad m) => (a -> a -> a) -> t m a -> t m a
scanl1' step m = fromStreamD $ D.scanl1' step $ toStreamD m

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Modify a @t m a -> t m a@ stream transformation that accepts a predicate
-- @(a -> b)@ to accept @((s, a) -> b)@ instead, provided a transformation @t m
-- a -> t m (s, a)@. Convenient to filter with index or time.
--
-- @
-- filterWithIndex = with indexed filter
-- filterWithAbsTime = with timestamped filter
-- filterWithRelTime = with timeIndexed filter
-- @
--
-- /Pre-release/
{-# INLINE with #-}
with :: forall (t :: (Type -> Type) -> Type -> Type) m a b s. Functor (t m) =>
       (t m a -> t m (s, a))
    -> (((s, a) -> b) -> t m (s, a) -> t m (s, a))
    -> (((s, a) -> b) -> t m a -> t m a)
with f comb g = fmap snd . comb g . f

-- | Include only those elements that pass a predicate.
--
-- @since 0.1.0
{-# INLINE filter #-}
filter :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
filter p m = fromStreamD $ D.filter p $ toStreamD m

-- | Same as 'filter' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE filterM #-}
filterM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
filterM p m = fromStreamD $ D.filterM p $ toStreamD m

-- | Drop repeated elements that are adjacent to each other using the supplied
-- comparison function.
--
-- @uniq = uniqBy (==)
--
-- To strip duplicate path separators:
--
-- @
-- f x y = x == '/' && x == y
-- Stream.toList $ Stream.uniqBy f $ Stream.fromList "//a//b"
-- "/a/b"
-- @
--
-- Space: @O(1)@
--
-- See also: 'nubBy'.
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: (IsStream t, Monad m, Functor (t m)) =>
    (a -> a -> Bool) -> t m a -> t m a
uniqBy eq = catMaybes . rollingMap f

    where

    f pre curr =
        case pre of
            Nothing -> Just curr
            Just x -> if x `eq` curr then Nothing else Just curr

-- | Drop repeated elements that are adjacent to each other.
--
-- @since 0.6.0
{-# INLINE uniq #-}
uniq :: (Eq a, IsStream t, Monad m) => t m a -> t m a
uniq = fromStreamD . D.uniq . toStreamD

-- | Strip all leading and trailing occurrences of an element passing a
-- predicate and make all other consecutive occurrences uniq.
--
-- @
-- prune p = dropWhileAround p $ uniqBy (x y -> p x && p y)
-- @
--
-- @
-- > Stream.prune isSpace (Stream.fromList "  hello      world!   ")
-- "hello world!"
--
-- @
--
-- Space: @O(1)@
--
-- /Unimplemented/
{-# INLINE prune #-}
prune ::
    -- (IsStream t, Monad m, Eq a) =>
    (a -> Bool) -> t m a -> t m a
prune = error "Not implemented yet!"

-- Possible implementation:
-- @repeated =
--      Stream.catMaybes . Stream.parseMany (Parser.groupBy (==) Fold.repeated)@
--
-- 'Fold.repeated' should return 'Just' when repeated, and 'Nothing' for a
-- single element.
--
-- | Emit only repeated elements, once.
--
-- /Unimplemented/
repeated :: -- (IsStream t, Monad m, Eq a) =>
    t m a -> t m a
repeated = undefined

-- We can have more efficient implementations for nubOrd and nubInt by using
-- Set and IntSet to find/remove duplication. For Hashable we can use a
-- hashmap. Use rewrite rules to specialize to more efficient impls.
--
-- | Drop repeated elements anywhere in the stream.
--
-- /Caution: not scalable for infinite streams/
--
-- /See also: nubWindowBy/
--
-- /Unimplemented/
--
{-# INLINE nubBy #-}
nubBy :: -- (IsStream t, Monad m) =>
    (a -> a -> Bool) -> t m a -> t m a
nubBy = undefined -- fromStreamD . D.nubBy . toStreamD

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- @
-- >>> Stream.toList $ Stream.deleteBy (==) 3 $ Stream.fromList [1,3,3,5]
-- [1,3,5]
--
-- @
--
-- @since 0.6.0
{-# INLINE deleteBy #-}
deleteBy :: (IsStream t, Monad m) => (a -> a -> Bool) -> a -> t m a -> t m a
deleteBy cmp x m = fromStreamD $ D.deleteBy cmp x (toStreamD m)

------------------------------------------------------------------------------
-- Lossy Buffering
------------------------------------------------------------------------------

-- XXX We could use 'maxBuffer Block/Drop/Rotate/Sample' instead. However we
-- may want to have the evaluation rate independent of the sampling rate. To
-- support that we can decouple evaluation and sampling in independent stages.
-- The sampling stage would strictly evaluate and sample, the evaluation stage
-- would control the evaluation rate.

-- | Evaluate the input stream continuously and keep only the oldest @n@
-- elements in the buffer, discard the new ones when the buffer is full.  When
-- the output stream is evaluated it consumes the values from the buffer in a
-- FIFO manner.
--
-- /Unimplemented/
--
{-# INLINE sampleOld #-}
sampleOld :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
sampleOld = undefined

-- | Evaluate the input stream continuously and keep only the latest @n@
-- elements in a ring buffer, keep discarding the older ones to make space for
-- the new ones.  When the output stream is evaluated it consumes the values
-- from the buffer in a FIFO manner.
--
-- /Unimplemented/
--
{-# INLINE sampleNew #-}
sampleNew :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
sampleNew = undefined

-- | Like 'sampleNew' but samples at uniform intervals to match the consumer
-- rate. Note that 'sampleNew' leads to non-uniform sampling depending on the
-- consumer pattern.
--
-- /Unimplemented/
--
{-# INLINE sampleRate #-}
sampleRate :: -- (IsStream t, Monad m) =>
    Double -> t m a -> t m a
sampleRate = undefined

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Same as 'takeWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE takeWhileM #-}
takeWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
takeWhileM p m = fromStreamD $ D.takeWhileM p $ toStreamD m

-- See the lastN fold for impl hints. Use a Data.Array based ring buffer.
--
-- takeLast n = Stream.concatM . fmap Array.toStream . fold Fold.lastN
--
-- | Take @n@ elements at the end of the stream.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeLast #-}
takeLast :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
takeLast = undefined -- fromStreamD $ D.takeLast n $ toStreamD m

-- | Take time interval @i@ seconds at the end of the stream.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeLastInterval #-}
takeLastInterval :: -- (IsStream t, Monad m) =>
    Double -> t m a -> t m a
takeLastInterval = undefined -- fromStreamD $ D.takeLast n $ toStreamD m

-- | Take all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeWhileLast #-}
takeWhileLast :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
takeWhileLast = undefined -- fromStreamD $ D.takeWhileLast n $ toStreamD m

-- | Like 'takeWhile' and 'takeWhileLast' combined.
--
-- O(n) space, where n is the number elements taken from the end.
--
-- /Unimplemented/
{-# INLINE takeWhileAround #-}
takeWhileAround :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
takeWhileAround = undefined -- fromStreamD $ D.takeWhileAround n $ toStreamD m

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
-- @since 0.1.0
{-# INLINE dropWhile #-}
dropWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStreamD $ D.dropWhile p $ toStreamD m

-- | Same as 'dropWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE dropWhileM #-}
dropWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
dropWhileM p m = fromStreamD $ D.dropWhileM p $ toStreamD m

-- | Drop @n@ elements at the end of the stream.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropLast #-}
dropLast :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
dropLast = undefined -- fromStreamD $ D.dropLast n $ toStreamD m

-- | Drop time interval @i@ seconds at the end of the stream.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropLastInterval #-}
dropLastInterval :: -- (IsStream t, Monad m) =>
    Int -> t m a -> t m a
dropLastInterval = undefined

-- | Drop all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropWhileLast #-}
dropWhileLast :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
dropWhileLast = undefined -- fromStreamD $ D.dropWhileLast n $ toStreamD m

-- | Like 'dropWhile' and 'dropWhileLast' combined.
--
-- O(n) space, where n is the number elements dropped from the end.
--
-- /Unimplemented/
{-# INLINE dropWhileAround #-}
dropWhileAround :: -- (IsStream t, Monad m) =>
    (a -> Bool) -> t m a -> t m a
dropWhileAround = undefined -- fromStreamD $ D.dropWhileAround n $ toStreamD m

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- @
-- insertBy cmp x = 'mergeBy' cmp ('fromPure' x)
-- @
--
-- @
-- >>> Stream.toList $ Stream.insertBy compare 2 $ Stream.fromList [1,3,5]
-- [1,2,3,5]
--
-- @
--
-- @since 0.6.0
{-# INLINE insertBy #-}
insertBy ::
       (IsStream t, Monad m) => (a -> a -> Ordering) -> a -> t m a -> t m a
insertBy cmp x m = fromStreamD $ D.insertBy cmp x (toStreamD m)

-- | Insert a pure value between successive elements of a stream.
--
-- >>> Stream.toList $ Stream.intersperse ',' $ Stream.fromList "hello"
-- "h,e,l,l,o"
--
-- @since 0.7.0
{-# INLINE intersperse #-}
intersperse :: (IsStream t, MonadAsync m) => a -> t m a -> t m a
intersperse a = fromStreamD . D.intersperse a . toStreamD

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- >>> Stream.drain $ Stream.trace putChar $ Stream.intersperseM_ (putChar '.') $ Stream.fromList "hello"
-- h.e.l.l.o
--
-- /Pre-release/
{-# INLINE intersperseM_ #-}
intersperseM_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseM_ m = fromStreamD . D.intersperseM_ m . toStreamD

-- | Intersperse a monadic action into the input stream after every @n@
-- elements.
--
-- @
-- > Stream.toList $ Stream.intersperseMWith 2 (return ',') $ Stream.fromList "hello"
-- "he,ll,o"
--
-- @
--
-- /Unimplemented/
{-# INLINE intersperseMWith #-}
intersperseMWith :: -- IsStream t =>
    Int -> m a -> t m a -> t m a
intersperseMWith _n _f _xs = undefined

-- | Insert an effect and its output after consuming an element of a stream.
--
-- >>> Stream.toList $ Stream.trace putChar $ intersperseMSuffix (putChar '.' >> return ',') $ Stream.fromList "hello"
-- h.,e.,l.,l.,o.,"h,e,l,l,o,"
--
-- /Pre-release/
{-# INLINE intersperseMSuffix #-}
intersperseMSuffix :: (IsStream t, Monad m) => m a -> t m a -> t m a
intersperseMSuffix m = fromStreamD . D.intersperseMSuffix m . toStreamD

-- | Insert a side effect after consuming an element of a stream.
--
-- @
-- >>> Stream.mapM_ putChar $ Stream.intersperseMSuffix_ (threadDelay 1000000) $ Stream.fromList "hello"
-- hello
--
-- @
--
-- /Pre-release/
--
{-# INLINE intersperseMSuffix_ #-}
intersperseMSuffix_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseMSuffix_ m = fromStreamD . D.intersperseMSuffix_ m . toStreamD

-- XXX Use an offset argument, like tapOffsetEvery
--
-- | Like 'intersperseMSuffix' but intersperses an effectful action into the
-- input stream after every @n@ elements and after the last element.
--
-- >>> Stream.toList $ Stream.intersperseMSuffixWith 2 (return ',') $ Stream.fromList "hello"
-- "he,ll,o,"
--
-- /Pre-release/
--
{-# INLINE intersperseMSuffixWith #-}
intersperseMSuffixWith :: (IsStream t, Monad m)
    => Int -> m a -> t m a -> t m a
intersperseMSuffixWith n eff =
    fromStreamD . D.intersperseMSuffixWith n eff . toStreamD

-- | Insert a side effect before consuming an element of a stream.
--
-- >>> Stream.toList $ Stream.trace putChar $ Stream.intersperseMPrefix_ (putChar '.' >> return ',') $ Stream.fromList "hello"
-- .h.e.l.l.o"hello"
--
-- Same as 'trace_' but may be concurrent.
--
-- /Concurrent/
--
-- /Pre-release/
--
{-# INLINE intersperseMPrefix_ #-}
intersperseMPrefix_ :: (IsStream t, MonadAsync m) => m b -> t m a -> t m a
intersperseMPrefix_ m = mapM (\x -> void m >> return x)

------------------------------------------------------------------------------
-- Inserting Time
------------------------------------------------------------------------------

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds before consuming an element of the
-- stream except the first one.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- @since 0.8.0
--
{-# INLINE delay #-}
delay :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delay n = intersperseM_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delayPost 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE delayPost #-}
delayPost :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPost n = intersperseMSuffix_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial, that's why 'trace_' is used.
--
-- | Introduce a delay of specified seconds before consuming an element of a
-- stream.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delayPre 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE delayPre #-}
delayPre :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPre n = trace_ $ liftIO $ threadDelay $ round $ n * 1000000

------------------------------------------------------------------------------
-- Reorder in sequence
------------------------------------------------------------------------------

-- | Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
--
-- /Unimplemented/
--
{-# INLINE reassembleBy #-}
reassembleBy
    :: -- (IsStream t, Monad m) =>
       Fold m a b
    -> (a -> a -> Int)
    -> t m a
    -> t m b
reassembleBy = undefined

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- |
-- > indexed = Stream.postscanl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- > indexed = Stream.zipWith (,) (Stream.enumerateFrom 0)
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- >>> Stream.toList $ Stream.indexed $ Stream.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
-- @since 0.6.0
{-# INLINE indexed #-}
indexed :: (IsStream t, Monad m) => t m a -> t m (Int, a)
indexed = fromStreamD . D.indexed . toStreamD

-- |
-- > indexedR n = Stream.postscanl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- > indexedR n = Stream.zipWith (,) (Stream.enumerateFromThen n (n - 1))
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- >>> Stream.toList $ Stream.indexedR 10 $ Stream.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
--
-- @since 0.6.0
{-# INLINE indexedR #-}
indexedR :: (IsStream t, Monad m) => Int -> t m a -> t m (Int, a)
indexedR n = fromStreamD . D.indexedR n . toStreamD

-------------------------------------------------------------------------------
-- Time Indexing
-------------------------------------------------------------------------------

-- Note: The timestamp stream must be the second stream in the zip so that the
-- timestamp is generated after generating the stream element and not before.
-- If we do not do that then the following example will generate the same
-- timestamp for first two elements:
--
-- Stream.mapM_ print $ Stream.timestamped $ Stream.delay $ Stream.enumerateFromTo 1 3
--
-- | Pair each element in a stream with an absolute timestamp, using a clock of
-- specified granularity.  The timestamp is generated just before the element
-- is consumed.
--
-- >>> Stream.mapM_ print $ Stream.timestampWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE timestampWith #-}
timestampWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (AbsTime, a)
timestampWith g stream = zipWith (flip (,)) stream (absTimesWith g)

-- TBD: check performance vs a custom implementation without using zipWith.
--
-- /Pre-release/
--
{-# INLINE timestamped #-}
timestamped :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (AbsTime, a)
timestamped = timestampWith 0.01

-- | Pair each element in a stream with relative times starting from 0, using a
-- clock with the specified granularity. The time is measured just before the
-- element is consumed.
--
-- >>> Stream.mapM_ print $ Stream.timeIndexWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (RelTime64 (NanoSecond64 ...),1)
-- (RelTime64 (NanoSecond64 ...),2)
-- (RelTime64 (NanoSecond64 ...),3)
--
-- /Pre-release/
--
{-# INLINE timeIndexWith #-}
timeIndexWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (RelTime64, a)
timeIndexWith g stream = zipWith (flip (,)) stream (relTimesWith g)

-- | Pair each element in a stream with relative times starting from 0, using a
-- 10 ms granularity clock. The time is measured just before the element is
-- consumed.
--
-- >>> Stream.mapM_ print $ Stream.timeIndexed $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- (RelTime64 (NanoSecond64 ...),1)
-- (RelTime64 (NanoSecond64 ...),2)
-- (RelTime64 (NanoSecond64 ...),3)
--
-- /Pre-release/
--
{-# INLINE timeIndexed #-}
timeIndexed :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (RelTime64, a)
timeIndexed = timeIndexWith 0.01

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the value of the element in the stream is equal
-- to the given value.
--
-- > elemIndices a = findIndices (== a)
--
-- @since 0.5.0
{-# INLINE elemIndices #-}
elemIndices :: (IsStream t, Eq a, Monad m) => a -> t m a -> t m Int
elemIndices a = findIndices (== a)

------------------------------------------------------------------------------
-- Rolling map
------------------------------------------------------------------------------

-- XXX this is not a one-to-one map so calling it map may not be right.
-- We can perhaps call it zipWithTail or rollWith.
--
-- | Apply a function on every two successive elements of a stream. The first
-- argument of the map function is the previous element and the second argument
-- is the current element. When the current element is the first element, the
-- previous element is 'Nothing'.
--
-- /Pre-release/
--
{-# INLINE rollingMap #-}
rollingMap :: (IsStream t, Monad m) => (Maybe a -> a -> b) -> t m a -> t m b
rollingMap f m = fromStreamD $ D.rollingMap f $ toStreamD m

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: (IsStream t, Monad m) => (Maybe a -> a -> m b) -> t m a -> t m b
rollingMapM f m = fromStreamD $ D.rollingMapM f $ toStreamD m

-- | Like 'rollingMap' but requires at least two elements in the stream,
-- returns an empty stream otherwise.
--
-- This is the stream equivalent of the list idiom @zipWith f xs (tail xs)@.
--
-- /Pre-release/
--
{-# INLINE rollingMap2 #-}
rollingMap2 :: (IsStream t, Monad m) => (a -> a -> b) -> t m a -> t m b
rollingMap2 f m = fromStreamD $ D.rollingMap2 f $ toStreamD m

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- Equivalent to:
--
-- @
-- mapMaybe f = Stream.map 'fromJust' . Stream.filter 'isJust' . Stream.map f
-- @
--
-- @since 0.3.0
{-# INLINE mapMaybe #-}
mapMaybe :: (IsStream t, Monad m) => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = fromStreamD $ D.mapMaybe f $ toStreamD m

-- | Like 'mapMaybe' but maps a monadic function.
--
-- Equivalent to:
--
-- @
-- mapMaybeM f = Stream.map 'fromJust' . Stream.filter 'isJust' . Stream.mapM f
-- @
--
-- /Concurrent (do not use with 'fromParallel' on infinite streams)/
--
-- @since 0.3.0
{-# INLINE_EARLY mapMaybeM #-}
mapMaybeM :: (IsStream t, MonadAsync m, Functor (t m))
          => (a -> m (Maybe b)) -> t m a -> t m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

{-# RULES "mapMaybeM serial" mapMaybeM = mapMaybeMSerial #-}
{-# INLINE mapMaybeMSerial #-}
mapMaybeMSerial :: Monad m => (a -> m (Maybe b)) -> SerialT m a -> SerialT m b
mapMaybeMSerial f m = fromStreamD $ D.mapMaybeM f $ toStreamD m

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- /Pre-release/
--
{-# INLINE catMaybes #-}
catMaybes :: (IsStream t, Monad m, Functor (t m)) => t m (Maybe a) -> t m a
catMaybes = fmap fromJust . filter isJust

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE lefts #-}
lefts :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m a
lefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE rights #-}
rights :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m b
rights = fmap (fromRight undefined) . filter isRight

-- | Remove the either wrapper and flatten both lefts and as well as rights in
-- the output stream.
--
-- /Pre-release/
--
{-# INLINE both #-}
both :: Functor (t m) => t m (Either a a) -> t m a
both = fmap (either id id)

------------------------------------------------------------------------------
-- Concurrent Application
------------------------------------------------------------------------------

-- | Parallel transform application operator; applies a stream transformation
-- function @t m a -> t m b@ to a stream @t m a@ concurrently; the input stream
-- is evaluated asynchronously in an independent thread yielding elements to a
-- buffer and the transformation function runs in another thread consuming the
-- input from the buffer.  '|$' is just like regular function application
-- operator '$' except that it is concurrent.
--
-- If you read the signature as @(t m a -> t m b) -> (t m a -> t m b)@ you can
-- look at it as a transformation that converts a transform function to a
-- buffered concurrent transform function.
--
-- The following code prints a value every second even though each stage adds a
-- 1 second delay.
--
--
-- >>> :{
-- Stream.drain $
--    Stream.mapM (\x -> threadDelay 1000000 >> print x)
--      |$ Stream.replicateM 3 (threadDelay 1000000 >> return 1)
-- :}
-- 1
-- 1
-- 1
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|$) #-}
(|$) :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> (t m a -> t m b)
-- (|$) f = f . mkAsync
(|$) f = f . mkParallel

infixr 0 |$

-- | Same as '|$'.
--
--  /Internal/
--
{-# INLINE applyAsync #-}
applyAsync :: (IsStream t, MonadAsync m)
    => (t m a -> t m b) -> (t m a -> t m b)
applyAsync = (|$)

-- | Same as '|$' but with arguments reversed.
--
-- (|&) = flip (|$)
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&) #-}
(|&) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> t m b) -> t m b
x |& f = f |$ x

infixl 1 |&
