-- |
-- Module      : Streamly.Internal.Data.Stream.Bottom
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Bottom level Stream module that can be used by all other upper level
-- Stream modules.

module Streamly.Internal.Data.Stream.Bottom
    (
    -- * Generation
      fromPure
    , fromEffect
    , fromList
    , timesWith
    , absTimesWith
    , relTimesWith

    -- * Folds
    , fold
    , foldContinue
    , foldBreak

    -- * Scans
    , smapM
    -- $smapM_Notes
    , postscan
    , take
    , takeWhile
    , takeEndBy
    , drop
    , findIndices

    -- * Merge
    , intersperseM

    -- * Fold and Unfold
    , reverse
    , reverse'

    -- * Expand
    , concatM
    , concatMapM
    , concatMap

    -- * Reduce
    , foldManyPost

    -- * Zipping
    , zipWithM
    , zipWith
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Fold.Type (Fold (..))
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64, addToAbsTime64)
import Streamly.Internal.Data.Unboxed (Unboxed)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Internal.Data.Array.Unboxed.Type as A
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.Common as Common
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (take, takeWhile, drop, reverse, concatMap, map, zipWith)

import Streamly.Internal.Data.Stream.Type

--
-- $setup
-- >>> :m
-- >>> import Control.Monad (join)
-- >>> import Control.Monad.Trans.Class (lift)
-- >>> import Data.Function (fix, (&))
-- >>> import Data.Semigroup (cycle1)
-- >>> import Prelude hiding (take, takeWhile, drop, reverse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Array.Unboxed as Array
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold

------------------------------------------------------------------------------
-- Generation
------------------------------------------------------------------------------

-- |
-- >>> fromList = Prelude.foldr Stream.cons Stream.nil
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'fromFoldable'.
--
{-# INLINE fromList #-}
fromList :: Monad m => [a] -> Stream m a
fromList = fromStreamK . Common.fromList

------------------------------------------------------------------------------
-- Generation - Time related
------------------------------------------------------------------------------

-- | @timesWith g@ returns a stream of time value tuples. The first component
-- of the tuple is an absolute time reference (epoch) denoting the start of the
-- stream and the second component is a time relative to the reference.
--
-- The argument @g@ specifies the granularity of the relative time in seconds.
-- A lower granularity clock gives higher precision but is more expensive in
-- terms of CPU usage. Any granularity lower than 1 ms is treated as 1 ms.
--
-- >>> import Control.Concurrent (threadDelay)
-- >>> f = Fold.drainBy (\x -> print x >> threadDelay 1000000)
-- >>> Stream.fold f $ Stream.take 3 $ Stream.timesWith 0.01
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),RelTime64 (NanoSecond64 ...))
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE timesWith #-}
timesWith :: MonadIO m => Double -> Stream m (AbsTime, RelTime64)
timesWith g = fromStreamD $ D.times g

-- | @absTimesWith g@ returns a stream of absolute timestamps using a clock of
-- granularity @g@ specified in seconds. A low granularity clock is more
-- expensive in terms of CPU usage.  Any granularity lower than 1 ms is treated
-- as 1 ms.
--
-- >>> f = Fold.drainBy print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.absTimesWith 0.01
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
-- AbsTime (TimeSpec {sec = ..., nsec = ...})
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE absTimesWith #-}
absTimesWith :: MonadIO m => Double -> Stream m AbsTime
absTimesWith = fmap (uncurry addToAbsTime64) . timesWith

-- | @relTimesWith g@ returns a stream of relative time values starting from 0,
-- using a clock of granularity @g@ specified in seconds. A low granularity
-- clock is more expensive in terms of CPU usage.  Any granularity lower than 1
-- ms is treated as 1 ms.
--
-- >>> f = Fold.drainBy print
-- >>> Stream.fold f $ Stream.delayPre 1 $ Stream.take 3 $ Stream.relTimesWith 0.01
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
-- RelTime64 (NanoSecond64 ...)
--
-- Note: This API is not safe on 32-bit machines.
--
-- /Pre-release/
--
{-# INLINE relTimesWith #-}
relTimesWith :: MonadIO m => Double -> Stream m RelTime64
relTimesWith = fmap snd . timesWith

------------------------------------------------------------------------------
-- Elimination - Running a Fold
------------------------------------------------------------------------------

-- | We can create higher order folds using 'foldContinue'. We can fold a
-- number of streams to a given fold efficiently with full stream fusion. For
-- example, to fold a list of streams on the same sum fold:
--
-- >>> concatFold = Prelude.foldl Stream.foldContinue Fold.sum
--
-- >>> fold f = Fold.finish . Stream.foldContinue f
--
-- /Internal/
{-# INLINE foldContinue #-}
foldContinue :: Monad m => Fold m a b -> Stream m a -> Fold m a b
foldContinue f s = D.foldContinue f $ toStreamD s

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> Stream.fold Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
-- Folds never fail, therefore, they produce a default value even when no input
-- is provided. It means we can always fold an empty stream and get a valid
-- result.  For example:
--
-- >>> Stream.fold Fold.sum Stream.nil
-- 0
--
-- >>> fold f = Stream.parse (Parser.fromFold f)
--
-- @since 0.9.0
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> Stream m a -> m b
fold fl strm = D.fold fl $ D.fromStreamK $ toStreamK strm

-- | Like 'fold' but also returns the remaining stream.
--
-- /Inhibits stream fusion/
--
{-# INLINE foldBreak #-}
foldBreak :: Monad m => Fold m a b -> Stream m a -> m (b, Stream m a)
{-
-- XXX This shows quadratic performance when used recursively perhaps because
-- of StreamK to StreamD conversions not getting eliminated sue to recursion.
foldBreak fl (Stream strm) = fmap f $ D.foldBreak fl $ D.fromStreamK strm

    where

    f (b, str) = (b, Stream (D.toStreamK str))
-}
foldBreak fl strm = fmap f $ K.foldBreak fl (toStreamK strm)

    where

    f (b, str) = (b, fromStreamK str)

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

{-
-- |
-- >>> map = fmap
--
-- Same as 'fmap'.
--
-- >>> Stream.fold Fold.toList $ fmap (+1) $ Stream.fromList [1,2,3]
-- [2,3,4]
--
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f = fromStreamD . D.map f . toStreamD
-}

-- | Postscan a stream using the given monadic fold.
--
-- The following example extracts the input stream up to a point where the
-- running average of elements is no more than 10:
--
-- >>> import Data.Maybe (fromJust)
-- >>> let avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> s = Stream.enumerateFromTo 1.0 100.0
-- >>> :{
--  Stream.fold Fold.toList
--   $ fmap (fromJust . fst)
--   $ Stream.takeWhile (\(_,x) -> x <= 10)
--   $ Stream.postscan (Fold.tee Fold.last avg) s
-- :}
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0]
--
{-# INLINE postscan #-}
postscan :: Monad m => Fold m a b -> Stream m a -> Stream m b
postscan fld = fromStreamD . D.postscanOnce fld . toStreamD

-- $smapM_Notes
--
-- The stateful step function can be simplified to @(s -> a -> m b)@ to provide
-- a read-only environment. However, that would just be 'mapM'.
--
-- The initial action could be @m (s, Maybe b)@, and we can also add a final
-- action @s -> m (Maybe b)@. This can be used to get pre/post scan like
-- functionality and also to flush the state in the end like scanlMAfter'.
-- We can also use it along with a fusible version of bracket to get
-- scanlMAfter' like functionality. See issue #677.
--
-- This can be further generalized to a type similar to Fold/Parser, giving it
-- filtering and parsing capability as well (this is in fact equivalent to
-- parseMany):
--
-- smapM :: (s -> a -> m (Step s b)) -> m s -> Stream m a -> Stream m b
--

-- | A stateful 'mapM', equivalent to a left scan, more like mapAccumL.
-- Hopefully, this is a better alternative to @scan@. Separation of state from
-- the output makes it easier to think in terms of a shared state, and also
-- makes it easier to keep the state fully strict and the output lazy.
--
-- See also: 'postscan'
--
-- /Pre-release/
--
{-# INLINE smapM #-}
smapM :: Monad m =>
       (s -> a -> m (s, b))
    -> m s
    -> Stream m a
    -> Stream m b
smapM step initial stream =
    -- XXX implement this directly instead of using postscan
    let f = Fold.foldlM'
                (\(s, _) a -> step s a)
                (fmap (,undefined) initial)
     in fmap snd $ postscan f stream

------------------------------------------------------------------------------
-- Transformation - Trimming
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
--
{-# INLINE take #-}
take :: Monad m => Int -> Stream m a -> Stream m a
take n m = fromStreamD $ D.take n $ toStreamD m

-- | End the stream as soon as the predicate fails on an element.
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile p m = fromStreamD $ D.takeWhile p $ toStreamD m

{-# INLINE takeEndBy #-}
takeEndBy :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeEndBy p m = fromStreamD $ D.takeEndBy p $ toStreamD m

-- | Discard first 'n' elements from the stream and take the rest.
--
{-# INLINE drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
drop n m = fromStreamD $ D.drop n $ toStreamD m

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- >>> findIndices p = Stream.fold (Fold.findIndices p)
--
{-# INLINE findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Stream m a -> Stream m Int
findIndices p m = fromStreamD $ D.findIndices p (toStreamD m)

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- intersperseM = intersperseBySpan 1

-- | Insert an effect and its output before consuming an element of a stream
-- except the first one.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ Stream.intersperseM (putChar '.' >> return ',') input
-- h.,e.,l.,l.,o"h,e,l,l,o"
--
-- Be careful about the order of effects. In the above example we used trace
-- after the intersperse, if we use it before the intersperse the output would
-- be he.l.l.o."h,e,l,l,o".
--
-- >>> Stream.fold Fold.toList $ Stream.intersperseM (putChar '.' >> return ',') $ Stream.trace putChar input
-- he.l.l.o."h,e,l,l,o"
--
{-# INLINE intersperseM #-}
intersperseM :: Monad m => m a -> Stream m a -> Stream m a
intersperseM m = fromStreamD . D.intersperseM m . toStreamD

------------------------------------------------------------------------------
-- Transformation by Reordering
------------------------------------------------------------------------------

-- XXX Use a compact region list to temporarily store the list, in both reverse
-- as well as in reverse'.
--
-- /Note:/ 'reverse'' is much faster than this, use that when performance
-- matters.
--
-- | Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- >>> reverse = Stream.foldlT (flip Stream.cons) Stream.nil
--
{-# INLINE reverse #-}
reverse :: Monad m => Stream m a -> Stream m a
reverse s = fromStreamD $ D.reverse $ toStreamD s

-- | Like 'reverse' but several times faster, requires a 'Storable' instance.
--
-- /Pre-release/
{-# INLINE reverse' #-}
reverse' :: (MonadIO m, Unboxed a) => Stream m a -> Stream m a
-- reverse' s = fromStreamD $ D.reverse' $ toStreamD s
reverse' =
        fromStreamD
        . A.flattenArraysRev -- unfoldMany A.readRev
        . D.fromStreamK
        . K.reverse
        . D.toStreamK
        . A.arraysOf defaultChunkSize
        . toStreamD

------------------------------------------------------------------------------
-- Combine streams and flatten
------------------------------------------------------------------------------

-- | Map a stream producing monadic function on each element of the stream
-- and then flatten the results into a single stream. Since the stream
-- generation function is monadic, unlike 'concatMap', it can produce an
-- effect at the beginning of each iteration of the inner loop.
--
{-# INLINE concatMapM #-}
concatMapM :: Monad m => (a -> m (Stream m b)) -> Stream m a -> Stream m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

-- | Map a stream producing function on each element of the stream and then
-- flatten the results into a single stream.
--
-- >>> concatMap f = Stream.concatMapM (return . f)
-- >>> concatMap f = Stream.concatMapWith Stream.append f
-- >>> concatMap f = Stream.concat . fmap f
-- >>> concatMap f = Stream.unfoldMany (Unfold.lmap f Unfold.fromStream)
--
{-# INLINE concatMap #-}
concatMap ::Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- >>> concatM = Stream.concat . Stream.fromEffect
-- >>> concatM = Stream.concat . lift    -- requires (MonadTrans t)
-- >>> concatM = join . lift             -- requires (MonadTrans t, Monad (Stream m))
--
-- See also: 'concat', 'sequence'
--
--  /Internal/
--
{-# INLINE concatM #-}
concatM :: Monad m => m (Stream m a) -> Stream m a
concatM generator = concatMapM (\() -> generator) (fromPure ())

-- | Like 'foldMany' but appends empty fold output if the fold and stream
-- termination aligns:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> Stream.fold Fold.toList $ Stream.foldManyPost f $ Stream.fromList []
-- [0]
-- >>> Stream.fold Fold.toList $ Stream.foldManyPost f $ Stream.fromList [1..9]
-- [3,7,11,15,9]
-- >>> Stream.fold Fold.toList $ Stream.foldManyPost f $ Stream.fromList [1..10]
-- [3,7,11,15,19,0]
--
-- /Pre-release/
--
{-# INLINE foldManyPost #-}
foldManyPost
    :: Monad m
    => Fold m a b
    -> Stream m a
    -> Stream m b
foldManyPost f m = fromStreamD $ D.foldManyPost f (toStreamD m)

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

-- | Like 'zipWith' but using a monadic zipping function.
--
{-# INLINE zipWithM #-}
zipWithM :: Monad m =>
    (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM f m1 m2 = fromStreamD $ D.zipWithM f (toStreamD m1) (toStreamD m2)

-- | Stream @a@ is evaluated first, followed by stream @b@, the resulting
-- elements @a@ and @b@ are then zipped using the supplied zip function and the
-- result @c@ is yielded to the consumer.
--
-- If stream @a@ or stream @b@ ends, the zipped stream ends. If stream @b@ ends
-- first, the element @a@ from previous evaluation of stream @a@ is discarded.
--
-- >>> s1 = Stream.fromList [1,2,3]
-- >>> s2 = Stream.fromList [4,5,6]
-- >>> Stream.fold Fold.toList $ Stream.zipWith (+) s1 s2
-- [5,7,9]
--
{-# INLINE zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f m1 m2 = fromStreamD $ D.zipWith f (toStreamD m1) (toStreamD m2)
