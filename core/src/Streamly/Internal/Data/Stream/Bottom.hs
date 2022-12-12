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
    , foldBreak
    , foldBreak2
    , foldEither
    , foldEither2
    , foldConcat

    -- * Builders
    , build
    , buildl

    -- * Scans
    , smapM
    -- $smapM_Notes
    , postscan
    , catMaybes
    , scanMaybe

    , take
    , takeWhile
    , takeEndBy
    , drop
    , findIndices

    -- * Merge
    , intersperseM

    -- * Fold and Unfold
    , reverse
    , reverseGeneric

    -- * Expand
    , concatEffect
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
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Fold.Type (Fold (..))
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64, addToAbsTime64)
import Streamly.Internal.Data.Unboxed (Unbox)
import Streamly.Internal.Data.Producer.Type (Producer(..))
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Internal.Data.SVar.Type (defState)

import qualified Streamly.Internal.Data.Array.Generic as GA
import qualified Streamly.Internal.Data.Array.Type as A
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (take, takeWhile, drop, reverse, concatMap, map, zipWith)

import Streamly.Internal.Data.Stream.Type

--
-- $setup
-- >>> :m
-- >>> import Control.Monad (join, (>=>), (<=<))
-- >>> import Data.Function (fix, (&))
-- >>> import Data.Maybe (fromJust, isJust)
-- >>> import Prelude hiding (take, takeWhile, drop, reverse)
-- >>> import System.IO.Unsafe (unsafePerformIO)
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import Streamly.Internal.Data.Stream as Stream

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
-- >>> f = Fold.drainMapM (\x -> print x >> threadDelay 1000000)
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
-- >>> f = Fold.drainMapM print
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
-- >>> f = Fold.drainMapM print
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

-- | Append a stream to a fold lazily to build an accumulator incrementally.
--
-- Example, to continue folding a list of streams on the same sum fold:
--
-- >>> streams = [Stream.fromList [1..5], Stream.fromList [6..10]]
-- >>> f = Prelude.foldl Stream.buildl Fold.sum streams
-- >>> Fold.extractM f
-- 55
--
{-# INLINE buildl #-}
buildl :: Monad m => Fold m a b -> Stream m a -> Fold m a b
buildl f s = D.foldContinue f $ toStreamD s

-- | Append a stream to a fold strictly to build an accumulator incrementally.
--
-- Definitions:
--
-- >>> build f = Stream.fold (Fold.duplicate f)
-- >>> build f = Stream.buildl f >=> Fold.reduce
--
-- Example:
--
-- >>> :{
-- do
--  sum1 <- Stream.build Fold.sum (Stream.enumerateFromTo 1 10)
--  sum2 <- Stream.build sum1 (Stream.enumerateFromTo 11 20)
--  Stream.fold sum2 (Stream.enumerateFromTo 21 30)
-- :}
-- 465
--
build :: Monad m => Fold m a b -> Stream m a -> m (Fold m a b)
build f = fold (Fold.duplicate f)

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- Definitions:
--
-- >>> fold f = fmap fst . Stream.foldBreak f
-- >>> fold f = Stream.parse (Parser.fromFold f)
-- >>> fold f = Stream.fold Fold.one . Stream.foldManyPost f
-- >>> fold f = Fold.extractM . Stream.buildl f
-- >>> fold f = Fold.extractM <=< Stream.build f
--
-- Example:
--
-- >>> Stream.fold Fold.sum (Stream.enumerateFromTo 1 100)
-- 5050
--
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> Stream m a -> m b
fold fl strm = D.fold fl $ D.fromStreamK $ toStreamK strm

-- Alternative name foldSome, but may be confused vs foldMany.

-- | Like 'fold' but also returns the remaining stream. The resulting stream
-- would be 'Stream.nil' if the stream finished before the fold.
--
-- /Not fused/
--
{-# INLINE foldBreak #-}
foldBreak :: Monad m => Fold m a b -> Stream m a -> m (b, Stream m a)
foldBreak fl strm = fmap f $ K.foldBreak fl (toStreamK strm)

    where

    f (b, str) = (b, fromStreamK str)

-- XXX The quadratic slowdown in recursive use is because recursive function
-- cannot be inlined and StreamD/StreamK conversions pile up and cannot be
-- eliminated by rewrite rules.

-- | Like 'foldBreak' but fuses.
--
-- /Note:/ Unlike 'foldBreak', recursive application on the resulting stream
-- would lead to quadratic slowdown. If you need recursion with fusion (within
-- one iteration of recursion) use StreamD.foldBreak directly.
--
-- /Internal/
{-# INLINE foldBreak2 #-}
foldBreak2 :: Monad m => Fold m a b -> Stream m a -> m (b, Stream m a)
foldBreak2 fl strm = fmap f $ D.foldBreak fl $ toStreamD strm

    where

    f (b, str) = (b, fromStreamD str)

-- | Fold resulting in either breaking the stream or continuation of the fold.
-- Instead of supplying the input stream in one go we can run the fold multiple
-- times, each time supplying the next segment of the input stream. If the fold
-- has not yet finished it returns a fold that can be run again otherwise it
-- returns the fold result and the residual stream.
--
-- /Internal/
{-# INLINE foldEither #-}
foldEither :: Monad m =>
    Fold m a b -> Stream m a -> m (Either (Fold m a b) (b, Stream m a))
foldEither fl strm = fmap (fmap f) $ K.foldEither fl $ toStreamK strm

    where

    f (b, str) = (b, fromStreamK str)

-- | Like 'foldEither' but fuses. However, recursive application on resulting
-- stream would lead to quadratic slowdown.
--
-- /Internal/
{-# INLINE foldEither2 #-}
foldEither2 :: Monad m =>
    Fold m a b -> Stream m a -> m (Either (Fold m a b) (b, Stream m a))
foldEither2 fl strm = fmap (fmap f) $ D.foldEither fl $ toStreamD strm

    where

    f (b, str) = (b, fromStreamD str)

-- XXX Array folds can be implemented using this.
-- foldContainers? Specialized to foldArrays.

-- | Generate streams from individual elements of a stream and fold the
-- concatenation of those streams using the supplied fold. Return the result of
-- the fold and residual stream.
--
-- For example, this can be used to efficiently fold an Array Word8 stream
-- using Word8 folds.
--
-- The outer stream forces CPS to allow scalable appends and the inner stream
-- forces direct style for stream fusion.
--
-- /Internal/
{-# INLINE foldConcat #-}
foldConcat :: Monad m =>
    Producer m a b -> Fold m b c -> Stream m a -> m (c, Stream m a)
foldConcat
    (Producer pstep pinject pextract)
    (Fold fstep begin done)
    stream = do

    res <- begin
    case res of
        Fold.Partial fs -> go fs streamK
        Fold.Done fb -> return (fb, fromStreamK streamK)

    where

    streamK = toStreamK stream

    go !acc m1 = do
        let stop = do
                r <- done acc
                return (r, fromStreamK K.nil)
            single a = do
                st <- pinject a
                res <- go1 SPEC acc st
                case res of
                    Left fs -> do
                        r <- done fs
                        return (r, fromStreamK K.nil)
                    Right (b, s) -> do
                        x <- pextract s
                        return (b, fromStreamK (K.fromPure x))
            yieldk a r = do
                st <- pinject a
                res <- go1 SPEC acc st
                case res of
                    Left fs -> go fs r
                    Right (b, s) -> do
                        x <- pextract s
                        return (b, fromStreamK (x `K.cons` r))
         in K.foldStream defState yieldk single stop m1

    {-# INLINE go1 #-}
    go1 !_ !fs st = do
        r <- pstep st
        case r of
            D.Yield x s -> do
                res <- fstep fs x
                case res of
                    Fold.Done b -> return $ Right (b, s)
                    Fold.Partial fs1 -> go1 SPEC fs1 s
            D.Skip s -> go1 SPEC fs s
            D.Stop -> return $ Left fs

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
--   $ Stream.postscan (Fold.tee Fold.latest avg) s
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

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- >>> catMaybes = Stream.mapMaybe id
-- >>> catMaybes = fmap fromJust . Stream.filter isJust
--
-- /Pre-release/
--
{-# INLINE catMaybes #-}
catMaybes :: Monad m => Stream m (Maybe a) -> Stream m a
-- catMaybes = fmap fromJust . filter isJust
catMaybes = fromStreamD . D.catMaybes . toStreamD

-- | Use a filtering fold on a stream.
--
-- >>> scanMaybe f = Stream.catMaybes . Stream.postscan f
--
{-# INLINE scanMaybe #-}
scanMaybe :: Monad m => Fold m a (Maybe b) -> Stream m a -> Stream m b
scanMaybe p = catMaybes . postscan p

------------------------------------------------------------------------------
-- Transformation - Trimming
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
--
{-# INLINE take #-}
take :: Monad m => Int -> Stream m a -> Stream m a
-- take n = scanMaybe (Fold.taking n)
take n m = fromStreamD $ D.take n $ toStreamD m

-- | End the stream as soon as the predicate fails on an element.
--
{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
-- takeWhile p = scanMaybe (Fold.takingEndBy_ (not . p))
takeWhile p m = fromStreamD $ D.takeWhile p $ toStreamD m

{-# INLINE takeEndBy #-}
takeEndBy :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
-- takeEndBy p = scanMaybe (Fold.takingEndBy p)
takeEndBy p m = fromStreamD $ D.takeEndBy p $ toStreamD m

-- | Discard first 'n' elements from the stream and take the rest.
--
{-# INLINE drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
-- drop n = scanMaybe (Fold.dropping n)
drop n m = fromStreamD $ D.drop n $ toStreamD m

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- >>> findIndices p = Stream.scanMaybe (Fold.findIndices p)
--
{-# INLINE findIndices #-}
findIndices :: Monad m => (a -> Bool) -> Stream m a -> Stream m Int
-- findIndices p = scanMaybe (Fold.findIndices p)
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
-- as well as in reverseGeneric to avoid GC overhead.

-- | Like 'reverse' but does not require an 'Unbox' instance. However, in most
-- cases this could be many times slower than 'reverse'. This may work better
-- for a stream of few large objects though. Also, if the stream being reversed
-- is already buffered in memory, this will perform lesser allocations that
-- 'reverse'.
--
{-# INLINE reverseGeneric #-}
reverseGeneric :: Monad m => Stream m a -> Stream m a
-- reverse m = fromStreamD $ D.concatEffect (fmap D.fromList $ fold Fold.toListRev m)
-- reverse s = fromStreamD $ D.reverse $ toStreamD s
-- reverse s = fromStreamK $ K.reverse $ toStreamK s -- very bad
-- reverse = Stream.foldlT (flip Stream.cons) Stream.nil
reverseGeneric =
    -- XXX Use K.reverse and arraysOf like in reverseUnbox
    fromStreamD
    . D.unfoldMany GA.readerRev
    -- . D.reverse
    . D.fromStreamK
    . K.reverse -- D.reverse is slightly worse in CPU cost
    . D.toStreamK
    . GA.arraysOf defaultChunkSize
    -- . D.foldMany (GA.writeNPure defaultChunkSize)
    . toStreamD

-- | Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- See 'reverseGeneric' if you do not want to write an 'Unbox' instance for
-- your data. 'reverseGeneric' may perform better if your stream consists of a
-- small number of large objects or if the stream is already buffered in
-- memory.
--
-- /Pre-release/
{-# INLINE reverse #-}
reverse :: (Monad m, Unbox a) => Stream m a -> Stream m a
-- reverseUnbox s = fromStreamD $ D.reverse' $ toStreamD s
reverse =
        fromStreamD
        -- . A.flattenArraysRev -- unfoldMany A.readRev
        . D.unfoldMany A.readerRev
        -- . D.reverse
        . D.fromStreamK
        . K.reverse -- D.reverse is slightly worse in CPU cost
        . D.toStreamK
        . A.arraysOf defaultChunkSize
        -- . D.foldMany (A.writeNPure defaultChunkSize)
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

-- >>> concatEffect = Stream.concat . lift    -- requires (MonadTrans t)
-- >>> concatEffect = join . lift             -- requires (MonadTrans t, Monad (Stream m))

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- >>> concatEffect = Stream.concat . Stream.fromEffect
--
-- See also: 'concat', 'sequence'
--
--  /Inhibits stream fusion/
--
{-# INLINE concatEffect #-}
concatEffect :: Monad m => m (Stream m a) -> Stream m a
-- concatEffect generator = concatMapM (\() -> generator) (fromPure ())
concatEffect generator =
    fromStreamK $ K.concatEffect $ fmap toStreamK generator

-- XXX Need a more intuitive name, and need to reconcile the names
-- foldMany/fold/parse/parseMany/parseManyPost etc.

-- | Like 'foldMany' but evaluates the fold before the stream, and yields its
-- output even if the stream is empty, therefore, always results in a non-empty
-- output even on an empty stream (default result of the fold).
--
-- Example, empty stream:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> fmany = Stream.fold Fold.toList . Stream.foldManyPost f
-- >>> fmany $ Stream.fromList []
-- [0]
--
-- Example, last fold empty:
--
-- >>> fmany $ Stream.fromList [1..4]
-- [3,7,0]
--
-- Example, last fold non-empty:
--
-- >>> fmany $ Stream.fromList [1..5]
-- [3,7,5]
--
-- Note that using a closed fold e.g. @Fold.take 0@, would result in an
-- infinite stream without consuming the input.
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
