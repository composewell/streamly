{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Expand
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Expand a stream by combining two or more streams or by combining streams
-- with unfolds.

module Streamly.Internal.Data.Stream.IsStream.Expand {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    (
    -- * Binary Combinators (Linear)
    -- | Functions ending in the shape:
    --
    -- @t m a -> t m a -> t m a@.
    --
    -- The functions in this section have a linear or flat n-ary combining
    -- characterstics. It means that when combined @n@ times (e.g. @a `serial`
    -- b `serial` c ...@) the resulting expression will have an @O(n)@
    -- complexity (instead O(n^2) for pair wise combinators described in the
    -- next section. These functions can be used efficiently with
    -- 'concatMapWith' et. al.  combinators that combine streams in a linear
    -- fashion (contrast with 'concatPairsWith' which combines streams as a
    -- binary tree).

      serial
    , ahead
    , async
    , wAsync
    , parallel
    , parallelFst
    , parallelMin

    -- * Binary Combinators (Pair Wise)
    -- | Like the functions in the section above these functions also combine
    -- two streams into a single stream but when used @n@ times linearly they
    -- exhibit O(n^2) complexity. They are best combined in a binary tree
    -- fashion using 'concatPairsWith' giving a @n * log n@ complexity.  Avoid
    -- using these with 'concatMapWith' when combining a large or infinite
    -- number of streams.

    -- ** Append
    , append

    -- ** wSerial
    -- | 'wSerial' is a CPS based stream interleaving functions. Use
    -- 'concatPairsWith wSerial' to interleave @n@ streams uniformly. It can be
    -- used with 'concatMapWith' as well, however, the interleaving behavior of
    -- @n@ streams would be asymmetric giving exponentially more weightage to
    -- streams that come earlier in the composition.
    --
    , wSerial
    , Serial.wSerialFst
    , Serial.wSerialMin

    -- ** Interleave
    -- | 'interleave' is like 'wSerial'  but using a direct style
    -- implementation instead of CPS. It is faster than 'wSerial' due to stream
    -- fusion but has worse efficiency when used with 'concatMapWith' for large
    -- number of streams.
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix

    -- ** Round Robin
    , roundrobin

    -- ** Zip
    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM

    -- ** Merge
    , merge
    , mergeBy
    , mergeByM
    , mergeByMFused
    , mergeAsyncBy
    , mergeAsyncByM
    , mergeMinBy
    , mergeFstBy

    -- * Combine Streams and Unfolds
    -- |
    -- Expand a stream by repeatedly using an unfold and merging the resulting
    -- streams.  Functions generally ending in the shape:
    --
    -- @Unfold m a b -> t m a -> t m b@

    -- ** Append Many (Unfold)
    -- | Unfold and flatten streams.
    , unfoldMany
    , unfoldManyInterleave
    , unfoldManyRoundRobin

    -- ** Interpose
    -- | Insert effects between streams. Like unfoldMany but intersperses an
    -- effect between the streams. A special case of gintercalate.
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- ** Intercalate
    -- | Insert Streams between Streams.
    -- Like unfoldMany but intersperses streams from another source between
    -- the streams from the first source.
    , intercalate
    , intercalateSuffix
    , gintercalate
    , gintercalateSuffix

    -- * Append Many (concatMap)
    -- | Map and serially append streams. 'concatMapM' is a generalization of
    -- the binary append operation to append many streams.
    , concatMapM
    , concatMap
    , concatM
    , concat

    -- * Flatten Containers
    -- | Flatten 'Foldable' containers using the binary stream merging
    -- operations.
    , IsStream.concatFoldableWith
    , IsStream.concatMapFoldableWith
    , IsStream.concatForFoldableWith

    -- * ConcatMapWith
    -- | Map and flatten a stream like 'concatMap' but using a custom binary
    -- stream merging combinator instead of just appending the streams.  The
    -- merging occurs sequentially, it works efficiently for 'serial', 'async',
    -- 'ahead' like merge operations where we consume one stream before the
    -- next or in case of 'wAsync' or 'parallel' where we consume all streams
    -- simultaneously anyway.
    --
    -- However, in cases where the merging consumes streams in a round robin
    -- fashion, a pair wise merging using 'concatPairsWith' would be more
    -- efficient. These cases include operations like 'mergeBy' or 'zipWith'.

    , IsStream.concatMapWith
    , IsStream.bindWith
    , concatSmapMWith

    -- * ConcatPairsWith
    -- | See the notes about suitable merge functions in the 'concatMapWith'
    -- section.
    , concatPairsWith

    -- * IterateMap
    -- | Map and flatten Trees of Streams
    , iterateMapWith
    , iterateSmapMWith
    , iterateMapLeftsWith
    , iterateUnfold

    -- * Deprecated
    , concatUnfold
    )
where

#include "inline.hs"

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.Ahead (aheadK)
import Streamly.Internal.Data.Stream.Async (asyncK, wAsyncK)
import Streamly.Internal.Data.Stream.IsStream.Common
    ( concatM, concatMapM, concatMap, smapM, fromPure, fromEffect, parallelFst
    , zipWith, zipWithM)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream(..), fromStreamD, toStreamD)
import Streamly.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream as D
    (append, interleave, interleaveFstSuffix, interleaveFst, interleaveMin
    , roundRobin, mergeByM, unfoldMany, unfoldInterleave, intersperse
    , unfoldRoundRobin, interpose, interposeSuffix, gintercalate
    , gintercalateSuffix, intersperseMSuffix)
import qualified Streamly.Internal.Data.StreamK as K
    (interleave, append, mergeMapWith, mergeBy, mergeByM)
import qualified Streamly.Internal.Data.Stream.ZipAsync as ZipAsync

import Prelude hiding (concat, concatMap, zipWith)

-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Data.IORef
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Array as Array
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}
--

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

-- XXX Reconcile the names "serial" and "append".
--
-- | Append the outputs of two streams, yielding all the elements from the
-- first stream and then yielding all the elements from the second stream.
--
-- IMPORTANT NOTE: This could be 100x faster than @serial/<>@ for appending a
-- few (say 100) streams because it can fuse via stream fusion. However, it
-- does not scale for a large number of streams (say 1000s) and becomes
-- qudartically slow. Therefore use this for custom appending of a few streams
-- but use 'concatMap' or 'concatMapWith serial' for appending @n@ streams or
-- infinite containers of streams.
--
-- /Pre-release/
{-# INLINE append #-}
append ::(IsStream t, Monad m) => t m b -> t m b -> t m b
append m1 m2 = fromStreamD $ D.append (toStreamD m1) (toStreamD m2)

infixr 6 `serial`

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> import Streamly.Prelude (serial)
-- >>> stream1 = Stream.fromList [1,2]
-- >>> stream2 = Stream.fromList [3,4]
-- >>> Stream.toList $ stream1 `serial` stream2
-- [1,2,3,4]
--
-- This operation can be used to fold an infinite lazy container of streams.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE serial #-}
serial :: IsStream t => t m a -> t m a -> t m a
serial m1 m2 = fromStream $ K.append (toStream m1) (toStream m2)

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

infixr 6 `wSerial`

-- XXX doc duplicated from Stream.Serial module.
--
-- | Interleaves two streams, yielding one element from each stream
-- alternately.  When one stream stops the rest of the other stream is used in
-- the output stream.
--
-- >>> import Streamly.Prelude (wSerial)
-- >>> stream1 = Stream.fromList [1,2]
-- >>> stream2 = Stream.fromList [3,4]
-- >>> Stream.toList $ Stream.fromWSerial $ stream1 `wSerial` stream2
-- [1,3,2,4]
--
-- Note, for singleton streams 'wSerial' and 'serial' are identical.
--
-- Note that this operation cannot be used to fold a container of infinite
-- streams but it can be used for very large streams as the state that it needs
-- to maintain is proportional to the logarithm of the number of streams.
--
-- @since 0.8.0
--
-- /Since: 0.2.0 ("Streamly")/

-- Scheduling Notes:
--
-- Note that evaluation of @a \`wSerial` b \`wSerial` c@ does not interleave
-- @a@, @b@ and @c@ with equal priority.  This expression is equivalent to @a
-- \`wSerial` (b \`wSerial` c)@, therefore, it fairly interleaves @a@ with the
-- result of @b \`wSerial` c@.  For example, @Stream.fromList [1,2] \`wSerial`
-- Stream.fromList [3,4] \`wSerial` Stream.fromList [5,6]@ would result in
-- [1,3,2,5,4,6].  In other words, the leftmost stream gets the same scheduling
-- priority as the rest of the streams taken together. The same is true for
-- each subexpression on the right.
--
{-# INLINE wSerial #-}
wSerial :: IsStream t => t m a -> t m a -> t m a
wSerial m1 m2 = fromStream $ K.interleave (toStream m1) (toStream m2)

-- XXX Same as 'wSerial'. We should perhaps rename wSerial to interleave.
-- XXX Document the interleaving behavior of side effects in all the
-- interleaving combinators.
-- XXX Write time-domain equivalents of these. In the time domain we can
-- interleave two streams such that the value of second stream is always taken
-- from its last value even if no new value is being yielded, like
-- zipWithLatest. It would be something like interleaveWithLatest.
--
-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. If any of the streams finishes
-- early the other stream continues alone until it too finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleave "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,,,"
--
-- >>> Stream.interleave "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,cd"
--
-- 'interleave' is dual to 'interleaveMin', it can be called @interleaveMax@.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleave #-}
interleave ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleave m1 m2 = fromStreamD $ D.interleave (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. As soon as the first stream
-- finishes, the output stops, discarding the remaining part of the second
-- stream. In this case, the last element in the resulting stream would be from
-- the second stream. If the second stream finishes early then the first stream
-- still continues to yield elements until it finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveSuffix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c,"
-- >>> Stream.interleaveSuffix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveSuffix' is a dual of 'interleaveInfix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveSuffix #-}
interleaveSuffix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveSuffix m1 m2 =
    fromStreamD $ D.interleaveFstSuffix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream and ending at the first stream.
-- If the second stream is longer than the first, elements from the second
-- stream are infixed with elements from the first stream. If the first stream
-- is longer then it continues yielding elements even after the second stream
-- has finished.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveInfix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
-- >>> Stream.interleaveInfix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveInfix' is a dual of 'interleaveSuffix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveInfix #-}
interleaveInfix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveInfix m1 m2 =
    fromStreamD $ D.interleaveFst (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. The output stops as soon as any
-- of the two streams finishes, discarding the remaining part of the other
-- stream. The last element of the resulting stream would be from the longer
-- stream.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveMin "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,"
-- >>> Stream.interleaveMin "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
--
-- 'interleaveMin' is dual to 'interleave'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveMin #-}
interleaveMin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveMin m1 m2 = fromStreamD $ D.interleaveMin (toStreamD m1) (toStreamD m2)

-------------------------------------------------------------------------------
-- Scheduling
-------------------------------------------------------------------------------

-- | Schedule the execution of two streams in a fair round-robin manner,
-- executing each stream once, alternately. Execution of a stream may not
-- necessarily result in an output, a stream may chose to @Skip@ producing an
-- element until later giving the other stream a chance to run. Therefore, this
-- combinator fairly interleaves the execution of two streams rather than
-- fairly interleaving the output of the two streams. This can be useful in
-- co-operative multitasking without using explicit threads. This can be used
-- as an alternative to `async`.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE roundrobin #-}
roundrobin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
roundrobin m1 m2 = fromStreamD $ D.roundRobin (toStreamD m1) (toStreamD m2)

infixr 6 `async`

-- | Merges two streams, both the streams may be evaluated concurrently,
-- outputs from both are used as they arrive:
--
-- >>> import Streamly.Prelude (async)
-- >>> stream1 = Stream.fromEffect (delay 4)
-- >>> stream2 = Stream.fromEffect (delay 2)
-- >>> Stream.toList $ stream1 `async` stream2
-- 2 sec
-- 4 sec
-- [2,4]
--
-- Multiple streams can be combined. With enough threads, all of them can be
-- scheduled simultaneously:
--
-- >>> stream3 = Stream.fromEffect (delay 1)
-- >>> Stream.toList $ stream1 `async` stream2 `async` stream3
-- ...
-- [1,2,4]
--
-- With 2 threads, only two can be scheduled at a time, when one of those
-- finishes, the third one gets scheduled:
--
-- >>> Stream.toList $ Stream.maxThreads 2 $ stream1 `async` stream2 `async` stream3
-- ...
-- [2,1,4]
--
-- With a single thread, it becomes serial:
--
-- >>> Stream.toList $ Stream.maxThreads 1 $ stream1 `async` stream2 `async` stream3
-- ...
-- [4,2,1]
--
-- Only streams are scheduled for async evaluation, how actions within a
-- stream are evaluated depends on the stream type. If it is a concurrent
-- stream they will be evaluated concurrently.
--
-- In the following example, both the streams are scheduled for concurrent
-- evaluation but each individual stream is evaluated serially:
--
-- >>> stream1 = Stream.fromListM $ Prelude.map delay [3,3] -- SerialT IO Int
-- >>> stream2 = Stream.fromListM $ Prelude.map delay [1,1] -- SerialT IO Int
-- >>> Stream.toList $ stream1 `async` stream2 -- IO [Int]
-- ...
-- [1,1,3,3]
--
-- If total threads are 2, the third stream is scheduled only after one of the
-- first two has finished:
--
-- > stream3 = Stream.fromListM $ Prelude.map delay [2,2] -- SerialT IO Int
-- > Stream.toList $ Stream.maxThreads 2 $ stream1 `async` stream2 `async` stream3 -- IO [Int]
-- ...
-- [1,1,3,2,3,2]
--
-- Thus 'async' goes deep in first few streams rather than going wide in all
-- streams. It prefers to evaluate the leftmost streams as much as possible.
-- Because of this behavior, 'async' can be safely used to fold an infinite
-- lazy container of streams.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE async #-}
async :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
async m1 m2 = fromStream $ asyncK (toStream m1) (toStream m2)

infixr 6 `wAsync`

-- | For singleton streams, 'wAsync' is the same as 'async'.  See 'async' for
-- singleton stream behavior. For multi-element streams, while 'async' is left
-- biased i.e. it tries to evaluate the left side stream as much as possible,
-- 'wAsync' tries to schedule them both fairly. In other words, 'async' goes
-- deep while 'wAsync' goes wide. However, outputs are always used as they
-- arrive.
--
-- With a single thread, 'async' starts behaving like 'serial' while 'wAsync'
-- starts behaving like 'wSerial'.
--
-- >>> import Streamly.Prelude (async, wAsync)
-- >>> stream1 = Stream.fromList [1,2,3]
-- >>> stream2 = Stream.fromList [4,5,6]
-- >>> Stream.toList $ Stream.fromAsync $ Stream.maxThreads 1 $ stream1 `async` stream2
-- [1,2,3,4,5,6]
--
-- >>> Stream.toList $ Stream.fromWAsync $ Stream.maxThreads 1 $ stream1 `wAsync` stream2
-- [1,4,2,5,3,6]
--
-- With two threads available, and combining three streams:
--
-- >>> stream3 = Stream.fromList [7,8,9]
-- >>> Stream.toList $ Stream.fromAsync $ Stream.maxThreads 2 $ stream1 `async` stream2 `async` stream3
-- [1,2,3,4,5,6,7,8,9]
--
-- >>> Stream.toList $ Stream.fromWAsync $ Stream.maxThreads 2 $ stream1 `wAsync` stream2 `wAsync` stream3
-- [1,4,2,7,5,3,8,6,9]
--
-- This operation cannot be used to fold an infinite lazy container of streams,
-- because it schedules all the streams in a round robin manner.
--
-- Note that 'WSerialT' and single threaded 'WAsyncT' both interleave streams
-- but the exact scheduling is slightly different in both cases.
--
-- @since 0.8.0
--
-- /Since: 0.2.0 ("Streamly")/

-- Scheduling details:
--
-- This is how the execution of the above example proceeds:
--
-- 1. The scheduler queue is initialized with @[S.fromList [1,2,3],
-- (S.fromList [4,5,6]) \<> (S.fromList [7,8,9])]@ assuming the head of the
-- queue is represented by the  rightmost item.
-- 2. @S.fromList [1,2,3]@ is executed, yielding the element @1@ and putting
-- @[2,3]@ at the back of the scheduler queue. The scheduler queue now looks
-- like @[(S.fromList [4,5,6]) \<> (S.fromList [7,8,9]), S.fromList [2,3]]@.
-- 3. Now @(S.fromList [4,5,6]) \<> (S.fromList [7,8,9])@ is picked up for
-- execution, @S.fromList [7,8,9]@ is added at the back of the queue and
-- @S.fromList [4,5,6]@ is executed, yielding the element @4@ and adding
-- @S.fromList [5,6]@ at the back of the queue. The queue now looks like
-- @[S.fromList [2,3], S.fromList [7,8,9], S.fromList [5,6]]@.
-- 4. Note that the scheduler queue expands by one more stream component in
-- every pass because one more @<>@ is broken down into two components. At this
-- point there are no more @<>@ operations to be broken down further and the
-- queue has reached its maximum size. Now these streams are scheduled in
-- round-robin fashion yielding @[2,7,5,3,8,6,9]@.
--
-- As we see above, in a right associated expression composed with @<>@, only
-- one @<>@ operation is broken down into two components in one execution,
-- therefore, if we have @n@ streams composed using @<>@ it will take @n@
-- scheduler passes to expand the whole expression.  By the time @n-th@
-- component is added to the scheduler queue, the first component would have
-- received @n@ scheduler passes.
--
{-# INLINE wAsync #-}
wAsync :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
wAsync m1 m2 = fromStream $ wAsyncK (toStream m1) (toStream m2)

infixr 6 `ahead`

-- | Appends two streams, both the streams may be evaluated concurrently but
-- the outputs are used in the same order as the corresponding actions in the
-- original streams, side effects will happen in the order in which the streams
-- are evaluated:
--
-- >>> import Streamly.Prelude (ahead, SerialT)
-- >>> stream1 = Stream.fromEffect (delay 4) :: SerialT IO Int
-- >>> stream2 = Stream.fromEffect (delay 2) :: SerialT IO Int
-- >>> Stream.toList $ stream1 `ahead` stream2 :: IO [Int]
-- 2 sec
-- 4 sec
-- [4,2]
--
-- Multiple streams can be combined. With enough threads, all of them can be
-- scheduled simultaneously:
--
-- >>> stream3 = Stream.fromEffect (delay 1)
-- >>> Stream.toList $ stream1 `ahead` stream2 `ahead` stream3
-- 1 sec
-- 2 sec
-- 4 sec
-- [4,2,1]
--
-- With 2 threads, only two can be scheduled at a time, when one of those
-- finishes, the third one gets scheduled:
--
-- >>> Stream.toList $ Stream.maxThreads 2 $ stream1 `ahead` stream2 `ahead` stream3
-- 2 sec
-- 1 sec
-- 4 sec
-- [4,2,1]
--
-- Only streams are scheduled for ahead evaluation, how actions within a stream
-- are evaluated depends on the stream type. If it is a concurrent stream they
-- will be evaluated concurrently. It may not make much sense combining serial
-- streams using 'ahead'.
--
-- 'ahead' can be safely used to fold an infinite lazy container of streams.
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE ahead #-}
ahead :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
ahead m1 m2 = fromStream $ aheadK (toStream m1) (toStream m2)

infixr 6 `parallel`

-- | Like 'Streamly.Prelude.async' except that the execution is much more
-- strict. There is no limit on the number of threads. While
-- 'Streamly.Prelude.async' may not schedule a stream if there is no demand
-- from the consumer, 'parallel' always evaluates both the streams immediately.
-- The only limit that applies to 'parallel' is 'Streamly.Prelude.maxBuffer'.
-- Evaluation may block if the output buffer becomes full.
--
-- >>> import Streamly.Prelude (parallel)
-- >>> stream = Stream.fromEffect (delay 2) `parallel` Stream.fromEffect (delay 1)
-- >>> Stream.toList stream -- IO [Int]
-- 1 sec
-- 2 sec
-- [1,2]
--
-- 'parallel' guarantees that all the streams are scheduled for execution
-- immediately, therefore, we could use things like starting timers inside the
-- streams and relying on the fact that all timers were started at the same
-- time.
--
-- Unlike 'async' this operation cannot be used to fold an infinite lazy
-- container of streams, because it schedules all the streams strictly
-- concurrently.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE parallel #-}
parallel :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallel m1 m2 = fromStream $ Par.parallelK (toStream m1) (toStream m2)

-- This is a race like combinator for streams.
--
-- | Like `parallel` but stops the output as soon as any of the two streams
-- stops.
--
-- /Pre-release/
{-# INLINE parallelMin #-}
parallelMin :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallelMin m1 m2 = fromStream $ Par.parallelMinK (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

-- | Like 'zipAsyncWith' but with a monadic zipping function.
--
-- @since 0.4.0
{-# INLINE zipAsyncWithM #-}
zipAsyncWithM :: (IsStream t, MonadAsync m)
    => (a -> b -> m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 =
    fromStream $ ZipAsync.zipAsyncWithMK f (toStream m1) (toStream m2)

-- XXX Should we rename this to zipParWith or zipParallelWith? This can happen
-- along with the change of behvaior to end the stream concurrently.
--
-- | Like 'zipWith' but zips concurrently i.e. both the streams being zipped
-- are evaluated concurrently using the 'ParallelT' concurrent evaluation
-- style. The maximum number of elements of each stream evaluated in advance
-- can be controlled by 'maxBuffer'.
--
-- The stream ends if stream @a@ or stream @b@ ends. However, if stream @b@
-- ends while we are still evaluating stream @a@ and waiting for a result then
-- stream will not end until after the evaluation of stream @a@ finishes. This
-- behavior can potentially be changed in future to end the stream immediately
-- as soon as any of the stream end is detected.
--
-- @since 0.1.0
{-# INLINE zipAsyncWith #-}
zipAsyncWith :: (IsStream t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f = zipAsyncWithM (\a b -> return (f a b))

------------------------------------------------------------------------------
-- Merging (sorted streams)
------------------------------------------------------------------------------

-- | Merge two streams using a comparison function. The head elements of both
-- the streams are compared and the smaller of the two elements is emitted, if
-- both elements are equal then the element from the first stream is used
-- first.
--
-- If the streams are sorted in ascending order, the resulting stream would
-- also remain sorted in ascending order.
--
-- @
-- >>> Stream.toList $ Stream.mergeBy compare (Stream.fromList [1,3,5]) (Stream.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
--
-- @
--
-- See also: 'mergeByMFused'
--
-- @since 0.6.0
{-# INLINE mergeBy #-}
mergeBy :: IsStream t => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeBy f m1 m2 = fromStream $ K.mergeBy f (toStream m1) (toStream m2)

-- | Like 'mergeBy' but with a monadic comparison function.
--
-- Merge two streams randomly:
--
-- @
-- > randomly _ _ = randomIO >>= \x -> return $ if x then LT else GT
-- > Stream.toList $ Stream.mergeByM randomly (Stream.fromList [1,1,1,1]) (Stream.fromList [2,2,2,2])
-- [2,1,2,2,2,1,1,1]
-- @
--
-- Merge two streams in a proportion of 2:1:
--
-- @
-- >>> :{
-- do
--  let proportionately m n = do
--       ref <- newIORef $ cycle $ Prelude.concat [Prelude.replicate m LT, Prelude.replicate n GT]
--       return $ \_ _ -> do
--          r <- readIORef ref
--          writeIORef ref $ Prelude.tail r
--          return $ Prelude.head r
--  f <- proportionately 2 1
--  xs <- Stream.toList $ Stream.mergeByM f (Stream.fromList [1,1,1,1,1,1]) (Stream.fromList [2,2,2])
--  print xs
-- :}
-- [1,1,2,1,1,2,1,1,2]
--
-- @
--
-- See also: 'mergeByMFused'
--
-- @since 0.6.0
{-# INLINE mergeByM #-}
mergeByM
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByM f m1 m2 = fromStream $ K.mergeByM f (toStream m1) (toStream m2)

-- XXX Fused versions should probably go to a separate module using the same
-- names for the combinators.
--
-- | Like 'mergeByM' but much faster, works best when merging statically known
-- number of streams. When merging more than two streams try to merge pairs and
-- pair pf pairs in a tree like structure.'mergeByM' works better with variable
-- number of streams being merged using 'concatPairsWith'.
--
-- /Internal/
{-# INLINE mergeByMFused #-}
mergeByMFused
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByMFused f m1 m2 =
    fromStreamD $ D.mergeByM f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as any of the two streams stops.
--
-- /Unimplemented/
{-# INLINABLE mergeMinBy #-}
mergeMinBy :: -- (IsStream t, Monad m) =>
    (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeMinBy _f _m1 _m2 = undefined
    -- fromStreamD $ D.mergeMinBy f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as the first stream stops.
--
-- /Unimplemented/
{-# INLINABLE mergeFstBy #-}
mergeFstBy :: -- (IsStream t, Monad m) =>
    (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeFstBy _f _m1 _m2 = undefined
    -- fromStreamD $ D.mergeFstBy f (toStreamD m1) (toStreamD m2)

-- XXX we may want to use the name "merge" differently
-- | Same as @'mergeBy' 'compare'@.
--
-- >>> Stream.toList $ Stream.merge (Stream.fromList [1,3,5]) (Stream.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
--
-- /Internal/
--
{-# INLINABLE merge #-}
merge ::
       (IsStream t, Ord a) => t m a -> t m a -> t m a
merge = mergeBy compare

-- | Like 'mergeBy' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
{-# INLINE mergeAsyncBy #-}
mergeAsyncBy :: (IsStream t, MonadAsync m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeAsyncBy f = mergeAsyncByM (\a b -> return $ f a b)

-- | Like 'mergeByM' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
{-# INLINE mergeAsyncByM #-}
mergeAsyncByM :: (IsStream t, MonadAsync m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeAsyncByM f m1 m2 =
    fromStreamD $
        let par = Par.mkParallelD . toStreamD
        in D.mergeByM f (par m1) (par m2)


-- @since 0.7.0
{-# DEPRECATED concatUnfold "Please use unfoldMany instead." #-}
{-# INLINE concatUnfold #-}
concatUnfold ::(IsStream t, Monad m) => Unfold m a b -> t m a -> t m b
concatUnfold u m = fromStreamD $ D.unfoldMany u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - unfoldMany
------------------------------------------------------------------------------

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
-- @since 0.8.0
{-# INLINE unfoldMany #-}
unfoldMany ::(IsStream t, Monad m) => Unfold m a b -> t m a -> t m b
unfoldMany u m = fromStreamD $ D.unfoldMany u (toStreamD m)

-- | Like 'unfoldMany' but interleaves the streams in the same way as
-- 'interleave' behaves instead of appending them.
--
-- /Pre-release/
{-# INLINE unfoldManyInterleave #-}
unfoldManyInterleave ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
unfoldManyInterleave u m =
    fromStreamD $ D.unfoldInterleave u (toStreamD m)

-- | Like 'unfoldMany' but executes the streams in the same way as
-- 'roundrobin'.
--
-- /Pre-release/
{-# INLINE unfoldManyRoundRobin #-}
unfoldManyRoundRobin ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
unfoldManyRoundRobin u m =
    fromStreamD $ D.unfoldRoundRobin u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - interpose
------------------------------------------------------------------------------

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- > unwords = S.interpose ' '
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interpose x unf str =
    fromStreamD $ D.interpose x unf (toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- > unlines = S.interposeSuffix '\n'
--
-- /Pre-release/
{-# INLINE interposeSuffix #-}
interposeSuffix :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interposeSuffix x unf str =
    fromStreamD $ D.interposeSuffix x unf (toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - intercalate
------------------------------------------------------------------------------

-- XXX we can swap the order of arguments to gintercalate so that the
-- definition of unfoldMany becomes simpler? The first stream should be
-- infixed inside the second one. However, if we change the order in
-- "interleave" as well similarly, then that will make it a bit unintuitive.
--
-- > unfoldMany unf str =
-- >     gintercalate unf str (UF.nilM (\_ -> return ())) (repeat ())
--
-- | 'interleaveInfix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalate #-}
gintercalate
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalate unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalate
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalate unf seed str = gintercalate unf str unf (repeatM seed)
--
-- | 'intersperse' followed by unfold and concat.
--
-- > intercalate unf a str = unfoldMany unf $ intersperse a str
-- > intersperse = intercalate (Unfold.function id)
-- > unwords = intercalate Unfold.fromList " "
--
-- >>> Stream.toList $ Stream.intercalate Unfold.fromList " " $ Stream.fromList ["abc", "def", "ghi"]
-- "abc def ghi"
--
-- @since 0.8.0
{-# INLINE intercalate #-}
intercalate :: (IsStream t, Monad m)
    => Unfold m b c -> b -> t m b -> t m c
intercalate unf seed str = fromStreamD $
    D.unfoldMany unf $ D.intersperse seed (toStreamD str)

-- | 'interleaveSuffix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalateSuffix unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalateSuffix
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalateSuffix unf seed str = gintercalateSuffix unf str unf (repeatM seed)
--
-- | 'intersperseMSuffix' followed by unfold and concat.
--
-- > intercalateSuffix unf a str = unfoldMany unf $ intersperseMSuffix a str
-- > intersperseMSuffix = intercalateSuffix (Unfold.function id)
-- > unlines = intercalateSuffix Unfold.fromList "\n"
--
-- >>> Stream.toList $ Stream.intercalateSuffix Unfold.fromList "\n" $ Stream.fromList ["abc", "def", "ghi"]
-- "abc\ndef\nghi\n"
--
-- @since 0.8.0
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (IsStream t, Monad m)
    => Unfold m b c -> b -> t m b -> t m c
intercalateSuffix unf seed str = fromStreamD $ D.unfoldMany unf
    $ D.intersperseMSuffix (return seed) (toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Flatten a stream of streams to a single stream.
--
-- @
-- concat = concatMap id
-- @
--
-- /Pre-release/
{-# INLINE concat #-}
concat :: (IsStream t, Monad m) => t m (t m a) -> t m a
concat = concatMap id

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- @
-- concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial
-- @
--
-- /Pre-release/
--
{-# INLINE concatSmapMWith #-}
concatSmapMWith
    :: (IsStream t, Monad m)
    => (t m b -> t m b -> t m b)
    -> (s -> a -> m (s, t m b))
    -> m s
    -> t m a
    -> t m b
concatSmapMWith combine f initial =
    IsStream.concatMapWith combine id . smapM f initial

-- XXX Implement a StreamD version for fusion.
--
-- | Combine streams in pairs using a binary stream combinator, then combine
-- the resulting streams in pairs recursively until we get to a single combined
-- stream.
--
-- For example, you can sort a stream using merge sort like this:
--
-- >>> Stream.toList $ Stream.concatPairsWith (Stream.mergeBy compare) Stream.fromPure $ Stream.fromList [5,1,7,9,2]
-- [1,2,5,7,9]
--
-- /Caution: the stream of streams must be finite/
--
-- /Pre-release/
--
{-# INLINE concatPairsWith #-}
concatPairsWith :: IsStream t =>
       (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatPairsWith par f m =
    fromStream
        $ K.mergeMapWith
            (\s1 s2 -> toStream $ fromStream s1 `par` fromStream s2)
            (toStream . f)
            (toStream m)

------------------------------------------------------------------------------
-- IterateMap - Map and flatten Trees of Streams
------------------------------------------------------------------------------

-- | Like 'iterateM' but iterates after mapping a stream generator on the
-- output.
--
-- Yield an input element in the output stream, map a stream generator on it
-- and then do the same on the resulting stream. This can be used for a depth
-- first traversal of a tree like structure.
--
-- Note that 'iterateM' is a special case of 'iterateMapWith':
--
-- @
-- iterateM f = iterateMapWith serial (fromEffect . f) . fromEffect
-- @
--
-- It can be used to traverse a tree structure.  For example, to list a
-- directory tree:
--
-- @
-- Stream.iterateMapWith Stream.serial
--     (either Dir.toEither (const nil))
--     (fromPure (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapWith #-}
iterateMapWith
    :: IsStream t
    => (t m a -> t m a -> t m a)
    -> (a -> t m a)
    -> t m a
    -> t m a
iterateMapWith combine f = IsStream.concatMapWith combine go
    where
    go x = fromPure x `combine` IsStream.concatMapWith combine go (f x)

-- | Same as @iterateMapWith Stream.serial@ but more efficient due to stream
-- fusion.
--
-- /Unimplemented/
{-# INLINE iterateUnfold #-}
iterateUnfold :: -- (IsStream t, MonadAsync m) =>
    Unfold m a a -> t m a -> t m a
iterateUnfold = undefined

------------------------------------------------------------------------------
-- Flattening Graphs
------------------------------------------------------------------------------

-- To traverse graphs we need a state to be carried around in the traversal.
-- For example, we can use a hashmap to store the visited status of nodes.

-- | Like 'iterateMap' but carries a state in the stream generation function.
-- This can be used to traverse graph like structures, we can remember the
-- visited nodes in the state to avoid cycles.
--
-- Note that a combination of 'iterateMap' and 'usingState' can also be used to
-- traverse graphs. However, this function provides a more localized state
-- instead of using a global state.
--
-- See also: 'mfix'
--
-- /Pre-release/
--
{-# INLINE iterateSmapMWith #-}
iterateSmapMWith
    :: (IsStream t, Monad m)
    => (t m a -> t m a -> t m a)
    -> (b -> a -> m (b, t m a))
    -> m b
    -> t m a
    -> t m a
iterateSmapMWith combine f initial stream =
    concatMap
        (\b -> IsStream.concatMapWith combine (go b) stream)
        (fromEffect initial)

    where

    go b a = fromPure a `combine` feedback b a

    feedback b a =
        concatMap
            (\(b1, s) -> IsStream.concatMapWith combine (go b1) s)
            (fromEffect $ f b a)

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- Keep concating either streams as long as rights are generated, stop as soon
-- as a left is generated and concat the left stream.
--
-- See also: 'handle'
--
-- /Unimplemented/
--
{-
concatMapEitherWith
    :: -- (IsStream t, MonadAsync m) =>
       (forall x. t m x -> t m x -> t m x)
    -> (a -> t m (Either (t m b) b))
    -> t m a
    -> t m b
concatMapEitherWith = undefined
-}

-- | In an 'Either' stream iterate on 'Left's.  This is a special case of
-- 'iterateMapWith':
--
-- @
-- iterateMapLeftsWith combine f = iterateMapWith combine (either f (const nil))
-- @
--
-- To traverse a directory tree:
--
-- @
-- iterateMapLeftsWith serial Dir.toEither (fromPure (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapLeftsWith #-}
iterateMapLeftsWith
    :: (IsStream t, b ~ Either a c)
    => (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m b
    -> t m b
iterateMapLeftsWith combine f =
    iterateMapWith combine (either f (const IsStream.nil))
