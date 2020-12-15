-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Transform
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.IsStream.Transform
    (
    -- * Transformation
      transform

    -- ** Mapping
    , Serial.map
    , sequence
    , mapM
    , smapM
    -- $smapM_Notes

    -- ** Special Maps
    , mapM_
    , trace
    , trace_
    , tap
    , tapOffsetEvery
    , tapAsync
    , tapRate
    , pollCounts

    -- ** Scanning
    -- ** Left scans
    , scanl'
    , scanlM'
    , scanlMAfter'
    , postscanl'
    , postscanlM'
    , prescanl'
    , prescanlM'
    , scanl1'
    , scanl1M'

    -- ** Scan Using Fold
    , scan
    , postscan

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

    -- ** Concurrent Transformation
    , D.mkParallel
    -- Par.mkParallel
    , applyAsync
    , (|$)
    , (|&)

    -- ** Filtering

    , filter
    , filterM

    -- ** Deleting Elements
    , deleteBy
    , uniq
    -- , uniqBy -- by predicate e.g. to remove duplicate "/" in a path
    -- , uniqOn -- to remove duplicate sequences
    -- , pruneBy -- dropAround + uniqBy - like words

    -- ** Inserting Elements

    , insertBy
    , intersperseM
    , intersperseM_
    , intersperse
    , intersperseSuffix
    , intersperseSuffix_
    , interspersePrefix_
    , intersperseSuffixBySpan
    -- , intersperseBySpan
    -- , intersperseByIndices -- using an index function/stream

    -- time domain intersperse
    -- , intersperseByTime
    -- , intersperseByEvent
    , delay
    , delayPost
    , delayPre

    -- ** Indexing
    , indexed
    , indexedR
    -- , timestampedR -- timer

    -- ** Reordering
    , reverse
    , reverse'

    -- ** Composable Left Folds
    -- | See "Streamly.Internal.Data.Fold"
    , foldMany
    , foldSequence
    , foldIterate

    -- ** Parsers
    -- | See "Streamly.Internal.Data.Parser"
    , parseMany
    , parseManyD
    , parseManyTill
    , parseSequence
    , parseIterate

    -- ** Trimming
    , take
    -- , takeGE
    -- , takeBetween
    , takeByTime
    -- , takeEnd
    , takeWhile
    , takeWhileM
    -- , takeWhileEnd
    , drop
    , dropByTime
    -- , dropEnd
    , dropWhile
    , dropWhileM
    -- , dropWhileEnd
    -- , dropAround

    -- ** Breaking

    -- Nary
    , chunksOf
    , chunksOf2
    , arraysOf

    -- ** Searching
    -- -- *** Searching Elements
    , findIndices
    , elemIndices

    -- -- *** Searching Sequences
    -- , seqIndices -- search a sequence in the stream

    -- -- *** Searching Multiple Sequences
    -- , seqIndicesAny -- search any of the given sequence in the stream

    -- -- -- ** Searching Streams
    -- -- | Finding a stream within another stream.

    -- ** Splitting
    -- | Streams can be sliced into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- -- *** Using Element Separators
    , splitOn
    , splitOnSuffix
    -- , splitOnPrefix

    -- , splitBy
    , splitWithSuffix
    -- , splitByPrefix
    , wordsBy -- stripAndCompactBy

    -- -- *** Splitting By Sequences
    , splitOnSeq
    , splitOnSuffixSeq
    -- , splitOnPrefixSeq

    -- Keeping the delimiters
    , splitWithSuffixSeq
    -- , splitByPrefixSeq
    -- , wordsBySeq

    -- Splitting using multiple sequence separators
    -- , splitOnAnySeq
    -- , splitOnAnySuffixSeq
    -- , splitOnAnyPrefixSeq

    -- -- *** Splitting By Streams
    -- -- | Splitting a stream using another stream as separator.

    -- Nested splitting
    , splitInnerBy
    , splitInnerBySuffix

    -- ** Grouping
    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    -- , groupScan

    , groups
    , groupsBy
    , groupsByRolling

    -- ** Group map
    , rollingMapM
    , rollingMap

    -- ** Appending
    , serial
    , append

    -- ** Interleaving
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix

    , wSerial
    , Serial.wSerialFst
    , Serial.wSerialMin

    -- ** Scheduling
    , ahead
    , async
    , wAsync
    , roundrobin

    -- ** Parallel
    , parallel
    , Par.parallelFst
    , Par.parallelMin

    -- ** Merging
    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- ** Zipping
    , Z.zipWith
    , Z.zipWithM
    , Z.zipAsyncWith
    , Z.zipAsyncWithM

    -- ** Flattening a Container of Streams
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- ** Flattening a Stream of Streams
    , concat
    , concatMap
    , concatMapM
    , concatMapWith
    , concatSmapMWith
    -- , bindWith

    -- ** Flattening Using Unfolds
    , concatUnfold
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin

    -- ** Inserting Streams in Streams
    , gintercalate
    , gintercalateSuffix
    , intercalate
    , intercalateSuffix
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- * Generalize Inner Monad
    , hoist
    , generally

    -- * Transform Inner Monad
    , liftInner
    , usingReaderT
    , runReaderT
    , evalStateT
    , usingStateT
    , runStateT

    -- ** Maybe Streams
    , catMaybes
    , mapMaybe
    , mapMaybeM

    -- ** Either Streams
    , lefts
    , rights

    -- * Deprecated
    , scanx
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (isJust, fromJust)
import Foreign.Storable (Storable)
import Streamly.Internal.BaseCompat (fromLeft, fromRight)
import Streamly.Internal.Data.Fold.Types (Fold (..), Fold2 (..))
import Streamly.Internal.Data.Parser (Parser (..))
import Streamly.Internal.Data.Unfold.Types (Unfold)
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array, writeNUnsafe)
import Streamly.Internal.Data.SVar (MonadAsync)
import Streamly.Internal.Data.Stream.Ahead (ahead)
import Streamly.Internal.Data.Stream.Async (async, wAsync)
import Streamly.Internal.Data.Stream.Combinators (maxYields)
import Streamly.Internal.Data.Stream.Parallel (parallel)
import Streamly.Internal.Data.Stream.Prelude
       (fromStreamS, toStreamS, concatFoldableWith, concatMapFoldableWith
       , concatForFoldableWith)
import Streamly.Internal.Data.Stream.StreamD (fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.StreamK (IsStream)
import Streamly.Internal.Data.Stream.Serial (SerialT, serial, wSerial)
import Streamly.Internal.Data.Pipe.Types (Pipe (..))
import Streamly.Internal.Data.Time.Units (TimeUnit64)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Zip as Z
import qualified Streamly.Internal.Data.Parser.ParserK.Types as PRK
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD as S
#endif

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, zipWith, foldr
       , foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, foldr1, (!!)
       , scanl, scanl1, replicate, concatMap, span, splitAt, break
       , repeat, concat, mconcat)

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- > findIndices = fold Fold.findIndices
--
-- @since 0.5.0
{-# INLINE findIndices #-}
findIndices :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m Int
findIndices p m = fromStreamS $ S.findIndices p (toStreamS m)

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
-- Map and Fold
------------------------------------------------------------------------------

-- XXX this can utilize parallel mapping if we implement it as drain . mapM
-- |
-- > mapM_ = drain . mapM
--
-- Apply a monadic action to each element of the stream and discard the output
-- of the action. This is not really a pure transformation operation but a
-- transformation followed by fold.
--
-- @since 0.1.0
{-# INLINE mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> SerialT m a -> m ()
mapM_ f m = S.mapM_ f $ toStreamS m

------------------------------------------------------------------------------
-- Concurrent Application
------------------------------------------------------------------------------

infixr 0 |$
infixl 1 |&

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
-- @
-- drain $
--    S.mapM (\\x -> threadDelay 1000000 >> print x)
--      |$ S.repeatM (threadDelay 1000000 >> return 1)
-- @
--
-- /Concurrent/
--
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|$) #-}
(|$) :: (IsStream t, MonadAsync m) => (t m a -> t m b) -> (t m a -> t m b)
-- (|$) f = f . Async.mkAsync
(|$) f = f . D.mkParallel

-- | Same as '|$'.
--
--  /Internal/
--
{-# INLINE applyAsync #-}
applyAsync :: (IsStream t, MonadAsync m)
    => (t m a -> t m b) -> (t m a -> t m b)
applyAsync = (|$)

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
-- /Since: 0.3.0 ("Streamly")/
--
-- @since 0.8.0
{-# INLINE (|&) #-}
(|&) :: (IsStream t, MonadAsync m) => t m a -> (t m a -> t m b) -> t m b
x |& f = f |$ x

------------------------------------------------------------------------------
-- General Transformation
------------------------------------------------------------------------------

-- | Use a 'Pipe' to transform a stream.
--
-- /Internal/
--
{-# INLINE transform #-}
transform :: (IsStream t, Monad m) => Pipe m a b -> t m a -> t m b
transform pipe xs = fromStreamD $ D.transform pipe (toStreamD xs)

------------------------------------------------------------------------------
-- Transformation by Folding (Scans)
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
scanx = P.scanlx'

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

-- | @scanlMAfter' accumulate initial done stream@ is like 'scanlM'' except
-- that it provides an additional @done@ function to be applied on the
-- accumulator when the stream stops. The result of @done@ is also emitted in
-- the stream.
--
-- This function can be used to allocate a resource in the beginning of the
-- scan and release it when the stream ends or to flush the internal state of
-- the scan at the end.
--
-- /Internal/
--
{-# INLINE scanlMAfter' #-}
scanlMAfter' :: (IsStream t, Monad m)
    => (b -> a -> m b) -> m b -> (b -> m b) -> t m a -> t m b
scanlMAfter' step initial done stream =
    fromStreamD $ D.scanlMAfter' step initial done $ toStreamD stream

-- XXX because of the use of D.cons for appending, scanl' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.

-- | Strict left scan. Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- @
-- > S.toList $ S.scanl' (+) 0 $ fromList [1,2,3,4]
-- [0,1,3,6,10]
-- @
--
-- @
-- > S.toList $ S.scanl' (flip (:)) [] $ S.fromList [1,2,3,4]
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
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
-- > S.foldl' (\\(s, p) x -> (s + x, p * x)) (0,1) $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- Using @scanl'@ we can make it modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- @
-- >   S.foldl' (\\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ S.scanl' (\\(s, _) x -> (s + x, x)) (0,1)
--   $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
-- See also: 'usingStateT'
--
-- @since 0.2.0
{-# INLINE scanl' #-}
scanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
scanl' step z m = fromStreamS $ S.scanl' step z $ toStreamS m

-- | Like 'scanl'' but does not stream the initial value of the accumulator.
--
-- > postscanl' f z xs = S.drop 1 $ S.scanl' f z xs
--
-- @since 0.7.0
{-# INLINE postscanl' #-}
postscanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
postscanl' step z m = fromStreamD $ D.postscanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like 'postscanl'' but with a monadic step function and a monadic seed.
--
-- /Since: 0.7.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE postscanlM' #-}
postscanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
postscanlM' step z m = fromStreamD $ D.postscanlM' step z $ toStreamD m

-- XXX prescanl does not sound very useful, enable only if there is a
-- compelling use case.
--
-- | Like scanl' but does not stream the final value of the accumulator.
--
-- /Internal/
{-# INLINE prescanl' #-}
prescanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
prescanl' step z m = fromStreamD $ D.prescanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like prescanl' but with a monadic step function and a monadic seed.
--
-- /Internal/
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
-- > S.toList $ S.scanl1 (+) $ fromList [1,2,3,4]
-- [1,3,6,10]
-- @
--
-- @since 0.6.0
{-# INLINE scanl1' #-}
scanl1' :: (IsStream t, Monad m) => (a -> a -> a) -> t m a -> t m a
scanl1' step m = fromStreamD $ D.scanl1' step $ toStreamD m

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

-- | Scan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE scan #-}
scan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
scan = P.scanOnce

-- | Postscan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE postscan #-}
postscan :: (IsStream t, Monad m) => Fold m a b -> t m a -> t m b
postscan = P.postscanOnce

------------------------------------------------------------------------------
-- Stateful Transformations
------------------------------------------------------------------------------

-- XXX this is not a one-to-one map so calling it map may not be right.
-- We can perhaps call it zipWithTail or rollWith.
--
-- | Apply a function on every two successive elements of a stream. If the
-- stream consists of a single element the output is an empty stream.
--
-- This is the stream equivalent of the list idiom @zipWith f xs (tail xs)@.
--
-- /Internal/
--
{-# INLINE rollingMap #-}
rollingMap :: (IsStream t, Monad m) => (a -> a -> b) -> t m a -> t m b
rollingMap f m = fromStreamD $ D.rollingMap f $ toStreamD m

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Internal/
--
{-# INLINE rollingMapM #-}
rollingMapM :: (IsStream t, Monad m) => (a -> a -> m b) -> t m a -> t m b
rollingMapM f m = fromStreamD $ D.rollingMapM f $ toStreamD m

------------------------------------------------------------------------------
-- Transformation by Filtering
------------------------------------------------------------------------------

-- | Include only those elements that pass a predicate.
--
-- @since 0.1.0
{-# INLINE filter #-}
#if __GLASGOW_HASKELL__ != 802
-- GHC 8.2.2 crashes with this code, when used with "stack"
filter :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
filter p m = fromStreamS $ S.filter p $ toStreamS m
#else
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter = K.filter
#endif

-- | Same as 'filter' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE filterM #-}
filterM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
filterM p m = fromStreamD $ D.filterM p $ toStreamD m

-- | Drop repeated elements that are adjacent to each other.
--
-- @since 0.6.0
{-# INLINE uniq #-}
uniq :: (Eq a, IsStream t, Monad m) => t m a -> t m a
uniq = fromStreamD . D.uniq . toStreamD

-- | Take first 'n' elements from the stream and discard the rest.
--
-- @since 0.1.0
{-# INLINE take #-}
take :: (IsStream t, Monad m) => Int -> t m a -> t m a
take n m = fromStreamS $ S.take n $ toStreamS
    (maxYields (Just (fromIntegral n)) m)

-- | End the stream as soon as the predicate fails on an element.
--
-- @since 0.1.0
{-# INLINE takeWhile #-}
takeWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStreamS $ S.takeWhile p $ toStreamS m

-- | Same as 'takeWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE takeWhileM #-}
takeWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
takeWhileM p m = fromStreamD $ D.takeWhileM p $ toStreamD m

-- | @takeByTime duration@ yields stream elements upto specified time
-- @duration@. The duration starts when the stream is evaluated for the first
-- time, before the first element is yielded. The time duration is checked
-- before generating each element, if the duration has expired the stream
-- stops.
--
-- The total time taken in executing the stream is guaranteed to be /at least/
-- @duration@, however, because the duration is checked before generating an
-- element, the upper bound is indeterminate and depends on the time taken in
-- generating and processing the last element.
--
-- No element is yielded if the duration is zero. At least one element is
-- yielded if the duration is non-zero.
--
-- /Internal/
--
{-# INLINE takeByTime #-}
takeByTime ::(MonadIO m, IsStream t, TimeUnit64 d) => d -> t m a -> t m a
takeByTime d = fromStreamD . D.takeByTime d . toStreamD

-- | Discard first 'n' elements from the stream and take the rest.
--
-- @since 0.1.0
{-# INLINE drop #-}
drop :: (IsStream t, Monad m) => Int -> t m a -> t m a
drop n m = fromStreamS $ S.drop n $ toStreamS m

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
-- @since 0.1.0
{-# INLINE dropWhile #-}
dropWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStreamS $ S.dropWhile p $ toStreamS m

-- | Same as 'dropWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE dropWhileM #-}
dropWhileM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> t m a
dropWhileM p m = fromStreamD $ D.dropWhileM p $ toStreamD m

-- | @dropByTime duration@ drops stream elements until specified @duration@ has
-- passed.  The duration begins when the stream is evaluated for the first
-- time. The time duration is checked /after/ generating a stream element, the
-- element is yielded if the duration has expired otherwise it is dropped.
--
-- The time elapsed before starting to generate the first element is /at most/
-- @duration@, however, because the duration expiry is checked after the
-- element is generated, the lower bound is indeterminate and depends on the
-- time taken in generating an element.
--
-- All elements are yielded if the duration is zero.
--
-- /Internal/
--
{-# INLINE dropByTime #-}
dropByTime ::(MonadIO m, IsStream t, TimeUnit64 d) => d -> t m a -> t m a
dropByTime d = fromStreamD . D.dropByTime d . toStreamD

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
-- > drain $ S.mapM putStr $ S.fromList ["a", "b", "c"]
-- abc
--
-- drain $ S.replicateM 10 (return 1)
--           & (serially . S.mapM (\\x -> threadDelay 1000000 >> print x))
--
-- drain $ S.replicateM 10 (return 1)
--           & (asyncly . S.mapM (\\x -> threadDelay 1000000 >> print x))
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE_EARLY mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM = K.mapM

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
-- > drain $ S.sequence $ S.fromList [putStr "a", putStr "b", putStrLn "c"]
-- abc
--
-- drain $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (serially . S.sequence)
--
-- drain $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (asyncly . S.sequence)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.1.0
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = fromStreamS $ S.sequence (toStreamS m)

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
-- smapM :: (s -> a -> m (Step s b)) -> m s -> t m a -> t m b
--

-- | A stateful 'mapM', equivalent to a left scan, more like mapAccumL.
-- Hopefully, this is a better alternative to @scan@. Separation of state from
-- the output makes it easier to think in terms of a shared state, and also
-- makes it easier to keep the state fully strict and the output lazy.
--
-- See also: 'scanlM''
--
-- /Internal/
--
{-# INLINE smapM #-}
smapM :: (IsStream t, Monad m) =>
       (s -> a -> m (s, b))
    -> m s
    -> t m a
    -> t m b
smapM step initial stream =
    -- XXX implement this directly instead of using scanlM'
    -- Once we have postscanlM' with monadic initial we can use this code
    -- let r = postscanlM'
    --              (\(s, _) a -> step s a)
    --              (fmap (,undefined) initial)
    --              stream
    let r = postscanlM'
                (\(s, _) a -> step s a)
                (fmap (,undefined) initial)
                stream
     in Serial.map snd r

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- Equivalent to:
--
-- @
-- mapMaybe f = S.map 'fromJust' . S.filter 'isJust' . S.map f
-- @
--
-- @since 0.3.0
{-# INLINE mapMaybe #-}
mapMaybe :: (IsStream t, Monad m) => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = fromStreamS $ S.mapMaybe f $ toStreamS m

-- | Like 'mapMaybe' but maps a monadic function.
--
-- Equivalent to:
--
-- @
-- mapMaybeM f = S.map 'fromJust' . S.filter 'isJust' . S.mapM f
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @since 0.3.0
{-# INLINE_EARLY mapMaybeM #-}
mapMaybeM :: (IsStream t, MonadAsync m, Functor (t m))
          => (a -> m (Maybe b)) -> t m a -> t m b
mapMaybeM f = fmap fromJust . filter isJust . K.mapM f

{-# RULES "mapMaybeM serial" mapMaybeM = mapMaybeMSerial #-}
{-# INLINE mapMaybeMSerial #-}
mapMaybeMSerial :: Monad m => (a -> m (Maybe b)) -> SerialT m a -> SerialT m b
mapMaybeMSerial f m = fromStreamD $ D.mapMaybeM f $ toStreamD m

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- /Internal/
--
catMaybes :: (IsStream t, Monad m, Functor (t m)) => t m (Maybe a) -> t m a
catMaybes = fmap fromJust . filter isJust

------------------------------------------------------------------------------
-- Transformation by Reordering
------------------------------------------------------------------------------

-- XXX Use a compact region list to temporarily store the list, in both reverse
-- as well as in reverse'.
--
-- /Note:/ 'reverse'' is much faster than this, use that when performance
-- matters.
--
-- > reverse = S.foldlT (flip S.cons) S.nil
--
-- | Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- /Since 0.7.0 (Monad m constraint)/
--
-- /Since: 0.1.1/
{-# INLINE reverse #-}
reverse :: (IsStream t, Monad m) => t m a -> t m a
reverse s = fromStreamS $ S.reverse $ toStreamS s

-- | Like 'reverse' but several times faster, requires a 'Storable' instance.
--
-- /Internal/
{-# INLINE reverse' #-}
reverse' :: (IsStream t, MonadIO m, Storable a) => t m a -> t m a
reverse' s = fromStreamD $ D.reverse' $ toStreamD s

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- intersperseM = intersperseBySpan 1

-- | Insert an effect and its output before consuming an element of a stream
-- except the first one.
--
-- @
-- >>> S.toList $ S.trace putChar $ S.intersperseM (putChar '.' >> return ',') $ S.fromList "hello"
-- > h.,e.,l.,l.,o"h,e,l,l,o"
-- @
--
-- @since 0.5.0
{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM m = fromStreamS . S.intersperseM m . toStreamS

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- @
-- >>> S.drain $ S.trace putChar $ S.intersperseM_ (putChar '.') $ S.fromList "hello"
-- > h.e.l.l.o
-- @
--
-- /Internal/
{-# INLINE intersperseM_ #-}
intersperseM_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseM_ m = fromStreamD . D.intersperseM_ m . toStreamD

-- | Insert a pure value between successive elements of a stream.
--
-- @
-- > S.toList $ S.intersperse ',' $ S.fromList "hello"
-- "h,e,l,l,o"
-- @
--
-- @since 0.7.0
{-# INLINE intersperse #-}
intersperse :: (IsStream t, MonadAsync m) => a -> t m a -> t m a
intersperse a = fromStreamS . S.intersperse a . toStreamS

-- | Insert an effect and its output after consuming an element of a stream.
--
-- @
-- >>> S.toList $ S.trace putChar $ S.intersperseSuffix (putChar '.' >> return ',') $ S.fromList "hello"
-- > h.,e.,l.,l.,o.,"h,e,l,l,o,"
-- @
--
-- /Internal/
{-# INLINE intersperseSuffix #-}
intersperseSuffix :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseSuffix m = fromStreamD . D.intersperseSuffix m . toStreamD

-- | Insert a side effect after consuming an element of a stream.
--
-- @
-- > S.mapM_ putChar $ S.intersperseSuffix_ (threadDelay 1000000) $ S.fromList "hello"
-- @
--
-- /Internal/
--
{-# INLINE intersperseSuffix_ #-}
intersperseSuffix_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
intersperseSuffix_ m = fromStreamD . D.intersperseSuffix_ m . toStreamD

-- | Insert a side effect before consuming an element of a stream.
--
-- @
-- >>> S.toList $ S.trace putChar $ S.interspersePrefix_ (putChar '.' >> return ',') $ S.fromList "hello"
-- > .h.e.l.l.o"hello"
-- @
--
-- See also: 'trace_'
--
-- /Concurrent/
--
-- /Internal/
--
{-# INLINE interspersePrefix_ #-}
interspersePrefix_ :: (IsStream t, MonadAsync m) => m b -> t m a -> t m a
interspersePrefix_ m = mapM (\x -> void m >> return x)

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds before consuming an element of the
-- stream except the first one.
--
-- @
-- >>> S.mapM_ print $ S.timestamped $ S.delay 1 $ S.enumerateFromTo 1 3
-- > (AbsTime (TimeSpec {sec = 2502706, nsec = 751137000}),1)
-- > (AbsTime (TimeSpec {sec = 2502707, nsec = 743535000}),2)
-- > (AbsTime (TimeSpec {sec = 2502708, nsec = 749758000}),3)
-- @
--
-- /Internal/
--
{-# INLINE delay #-}
delay :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delay n = intersperseM_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- @
-- >>> S.mapM_ print $ S.timestamped $ S.delayPost 1 $ S.enumerateFromTo 1 3
-- > (AbsTime (TimeSpec {sec = 2502826, nsec = 119030000}),1)
-- > (AbsTime (TimeSpec {sec = 2502827, nsec = 111393000}),2)
-- > (AbsTime (TimeSpec {sec = 2502828, nsec = 112221000}),3)
-- @
--
-- /Internal/
--
{-# INLINE delayPost #-}
delayPost :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPost n = intersperseSuffix_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial, that's why 'trace_' is used.
--
-- | Introduce a delay of specified seconds before consuming an element of a
-- stream.
--
-- @
-- >>> S.mapM_ print $ S.timestamped $ S.delayPre 1 $ S.enumerateFromTo 1 3
-- > (AbsTime (TimeSpec {sec = 2502207, nsec = 533177000}),1)
-- > (AbsTime (TimeSpec {sec = 2502208, nsec = 530859000}),2)
-- > (AbsTime (TimeSpec {sec = 2502209, nsec = 531619000}),3)
-- @
--
-- /Internal/
--
{-# INLINE delayPre #-}
delayPre :: (IsStream t, MonadIO m) => Double -> t m a -> t m a
delayPre n = trace_ $ liftIO $ threadDelay $ round $ n * 1000000

-- | Like 'intersperseSuffix' but intersperses an effectful action into the
-- input stream after every @n@ elements and after the last element.
--
-- @
-- > S.toList $ S.intersperseSuffixBySpan 2 (return ',') $ S.fromList "hello"
-- "he,ll,o,"
-- @
--
-- /Internal/
--
{-# INLINE intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: (IsStream t, MonadAsync m)
    => Int -> m a -> t m a -> t m a
intersperseSuffixBySpan n eff =
    fromStreamD . D.intersperseSuffixBySpan n eff . toStreamD

{-
-- | Intersperse a monadic action into the input stream after every @n@
-- elements.
--
-- @
-- > S.toList $ S.intersperseBySpan 2 (return ',') $ S.fromList "hello"
-- "he,ll,o"
-- @
--
-- @since 0.7.0
{-# INLINE intersperseBySpan #-}
intersperseBySpan :: IsStream t => Int -> m a -> t m a -> t m a
intersperseBySpan _n _f _xs = undefined
-}

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- @
-- insertBy cmp x = 'mergeBy' cmp ('yield' x)
-- @
--
-- @
-- > S.toList $ S.insertBy compare 2 $ S.fromList [1,3,5]
-- [1,2,3,5]
-- @
--
-- @since 0.6.0
{-# INLINE insertBy #-}
insertBy ::
       (IsStream t, Monad m) => (a -> a -> Ordering) -> a -> t m a -> t m a
insertBy cmp x m = fromStreamS $ S.insertBy cmp x (toStreamS m)

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- @
-- > S.toList $ S.deleteBy (==) 3 $ S.fromList [1,3,3,5]
-- [1,3,5]
-- @
--
-- @since 0.6.0
{-# INLINE deleteBy #-}
deleteBy :: (IsStream t, Monad m) => (a -> a -> Bool) -> a -> t m a -> t m a
deleteBy cmp x m = fromStreamS $ S.deleteBy cmp x (toStreamS m)

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

-- |
-- > indexed = S.postscanl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- > indexed = S.zipWith (,) (S.enumerateFrom 0)
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- @
-- > S.toList $ S.indexed $ S.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
-- @
--
-- @since 0.6.0
{-# INLINE indexed #-}
indexed :: (IsStream t, Monad m) => t m a -> t m (Int, a)
indexed = fromStreamD . D.indexed . toStreamD

-- |
-- > indexedR n = S.postscanl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- > indexedR n = S.zipWith (,) (S.enumerateFromThen n (n - 1))
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- @
-- > S.toList $ S.indexedR 10 $ S.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
-- @
--
-- @since 0.6.0
{-# INLINE indexedR #-}
indexedR :: (IsStream t, Monad m) => Int -> t m a -> t m (Int, a)
indexedR n = fromStreamD . D.indexedR n . toStreamD

------------------------------------------------------------------------------
-- Merge
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
-- > S.toList $ S.mergeBy compare (S.fromList [1,3,5]) (S.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
-- @
--
-- @since 0.6.0
{-# INLINABLE mergeBy #-}
mergeBy ::
       (IsStream t, Monad m) => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeBy f m1 m2 = fromStreamS $ S.mergeBy f (toStreamS m1) (toStreamS m2)

-- | Like 'mergeBy' but with a monadic comparison function.
--
-- Merge two streams randomly:
--
-- @
-- > randomly _ _ = randomIO >>= \x -> return $ if x then LT else GT
-- > S.toList $ S.mergeByM randomly (S.fromList [1,1,1,1]) (S.fromList [2,2,2,2])
-- [2,1,2,2,2,1,1,1]
-- @
--
-- Merge two streams in a proportion of 2:1:
--
-- @
-- proportionately m n = do
--  ref <- newIORef $ cycle $ concat [replicate m LT, replicate n GT]
--  return $ \\_ _ -> do
--      r <- readIORef ref
--      writeIORef ref $ tail r
--      return $ head r
--
-- main = do
--  f <- proportionately 2 1
--  xs <- S.toList $ S.mergeByM f (S.fromList [1,1,1,1,1,1]) (S.fromList [2,2,2])
--  print xs
-- @
-- @
-- [1,1,2,1,1,2,1,1,2]
-- @
--
-- @since 0.6.0
{-# INLINABLE mergeByM #-}
mergeByM
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByM f m1 m2 = fromStreamS $ S.mergeByM f (toStreamS m1) (toStreamS m2)

{-
-- | Like 'mergeByM' but stops merging as soon as any of the two streams stops.
{-# INLINABLE mergeEndByAny #-}
mergeEndByAny
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeEndByAny f m1 m2 = fromStreamD $
    D.mergeEndByAny f (toStreamD m1) (toStreamD m2)

-- Like 'mergeByM' but stops merging as soon as the first stream stops.
{-# INLINABLE mergeEndByFirst #-}
mergeEndByFirst
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeEndByFirst f m1 m2 = fromStreamS $
    D.mergeEndByFirst f (toStreamD m1) (toStreamD m2)
-}

-- Holding this back for now, we may want to use the name "merge" differently
{-
-- | Same as @'mergeBy' 'compare'@.
--
-- @
-- > S.toList $ S.merge (S.fromList [1,3,5]) (S.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
-- @
--
-- @since 0.6.0
{-# INLINABLE merge #-}
merge ::
       (IsStream t, Monad m, Ord a) => t m a -> t m a -> t m a
merge = mergeBy compare
-}

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
mergeAsyncByM f m1 m2 = fromStreamD $
    D.mergeByM f (D.mkParallelD $ toStreamD m1) (D.mkParallelD $ toStreamD m2)

------------------------------------------------------------------------------
-- Flattening Lists
------------------------------------------------------------------------------

-- | @concatMapWith merge map stream@ is a two dimensional looping combinator.
-- The first argument specifies a merge or concat function that is used to
-- merge the streams generated by applying the second argument i.e. the @map@
-- function to each element of the input stream. The concat function could be
-- 'serial', 'parallel', 'async', 'ahead' or any other zip or merge function
-- and the second argument could be any stream generation function using a
-- seed.
--
-- /Compare 'foldMapWith'/
--
-- /Since: 0.7.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE concatMapWith #-}
concatMapWith
    :: IsStream t
    => (t m b -> t m b -> t m b)
    -> (a -> t m b)
    -> t m a
    -> t m b
concatMapWith = K.concatMapBy

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- @
-- concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial
-- @
--
-- /Internal/
--
{-# INLINE concatSmapMWith #-}
concatSmapMWith
    :: (IsStream t, Monad m)
    => (t m b -> t m b -> t m b)
    -> (s -> a -> m (s, t m b))
    -> m s
    -> t m a
    -> t m b
concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial

-- | Map a stream producing function on each element of the stream and then
-- flatten the results into a single stream.
--
-- @
-- concatMap f = 'concat . map f'
-- concatMap = 'concatMapWith' 'Serial.serial'
-- concatMap f = 'concatMapM' (return . f)
-- concatMap f = 'concatUnfold' (UF.lmap f UF.fromStream)
-- @
--
-- @since 0.6.0
{-# INLINE concatMap #-}
concatMap ::(IsStream t, Monad m) => (a -> t m b) -> t m a -> t m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Flatten a stream of streams to a single stream.
--
-- @
-- concat = concatMap id
-- @
--
-- /Internal/
{-# INLINE concat #-}
concat :: (IsStream t, Monad m) => t m (t m a) -> t m a
concat = concatMap id

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
-- /Internal/
{-# INLINE append #-}
append ::(IsStream t, Monad m) => t m b -> t m b -> t m b
append m1 m2 = fromStreamD $ D.append (toStreamD m1) (toStreamD m2)

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
-- >>> interleave "ab" ",,,," :: SerialT Identity Char
-- fromList "a,b,,,"
-- >>> interleave "abcd" ",," :: SerialT Identity Char
-- fromList "a,b,cd"
--
-- 'interleave' is dual to 'interleaveMin', it can be called @interleaveMax@.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
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
-- >>> interleaveSuffix "abc" ",,,," :: SerialT Identity Char
-- fromList "a,b,c,"
-- >>> interleaveSuffix "abc" "," :: SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveSuffix' is a dual of 'interleaveInfix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
{-# INLINE interleaveSuffix #-}
interleaveSuffix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveSuffix m1 m2 =
    fromStreamD $ D.interleaveSuffix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream and ending at the first stream.
-- If the second stream is longer than the first, elements from the second
-- stream are infixed with elements from the first stream. If the first stream
-- is longer then it continues yielding elements even after the second stream
-- has finished.
--
-- >>> :set -XOverloadedStrings
-- >>> interleaveInfix "abc" ",,,," :: SerialT Identity Char
-- fromList "a,b,c"
-- >>> interleaveInfix "abc" "," :: SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveInfix' is a dual of 'interleaveSuffix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
{-# INLINE interleaveInfix #-}
interleaveInfix ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveInfix m1 m2 =
    fromStreamD $ D.interleaveInfix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. The output stops as soon as any
-- of the two streams finishes, discarding the remaining part of the other
-- stream. The last element of the resulting stream would be from the longer
-- stream.
--
-- >>> :set -XOverloadedStrings
-- >>> interleaveMin "ab" ",,,," :: SerialT Identity Char
-- fromList "a,b,"
-- >>> interleaveMin "abcd" ",," :: SerialT Identity Char
-- fromList "a,b,c"
--
-- 'interleaveMin' is dual to 'interleave'.
--
-- Do not use at scale in concatMapWith.
--
-- /Internal/
{-# INLINE interleaveMin #-}
interleaveMin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
interleaveMin m1 m2 = fromStreamD $ D.interleaveMin (toStreamD m1) (toStreamD m2)

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
-- /Internal/
{-# INLINE roundrobin #-}
roundrobin ::(IsStream t, Monad m) => t m b -> t m b -> t m b
roundrobin m1 m2 = fromStreamD $ D.roundRobin (toStreamD m1) (toStreamD m2)

-- | Map a stream producing monadic function on each element of the stream
-- and then flatten the results into a single stream. Since the stream
-- generation function is monadic, unlike 'concatMap', it can produce an
-- effect at the beginning of each iteration of the inner loop.
--
-- @since 0.6.0
{-# INLINE concatMapM #-}
concatMapM :: (IsStream t, Monad m) => (a -> m (t m b)) -> t m a -> t m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
-- @since 0.7.0
{-# INLINE concatUnfold #-}
concatUnfold ::(IsStream t, Monad m) => Unfold m a b -> t m a -> t m b
concatUnfold u m = fromStreamD $ D.concatMapU u (toStreamD m)

-- | Like 'concatUnfold' but interleaves the streams in the same way as
-- 'interleave' behaves instead of appending them.
--
-- /Internal/
{-# INLINE concatUnfoldInterleave #-}
concatUnfoldInterleave ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
concatUnfoldInterleave u m =
    fromStreamD $ D.concatUnfoldInterleave u (toStreamD m)

-- | Like 'concatUnfold' but executes the streams in the same way as
-- 'roundrobin'.
--
-- /Internal/
{-# INLINE concatUnfoldRoundrobin #-}
concatUnfoldRoundrobin ::(IsStream t, Monad m)
    => Unfold m a b -> t m a -> t m b
concatUnfoldRoundrobin u m =
    fromStreamD $ D.concatUnfoldRoundrobin u (toStreamD m)

-- XXX we can swap the order of arguments to gintercalate so that the
-- definition of concatUnfold becomes simpler? The first stream should be
-- infixed inside the second one. However, if we change the order in
-- "interleave" as well similarly, then that will make it a bit unintuitive.
--
-- > concatUnfold unf str =
-- >     gintercalate unf str (UF.nilM (\_ -> return ())) (repeat ())
--
-- | 'interleaveInfix' followed by unfold and concat.
--
-- /Internal/
{-# INLINE gintercalate #-}
gintercalate
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalate unf1 str1 unf2 str2 =
    D.fromStreamD $ D.gintercalate
        unf1 (D.toStreamD str1)
        unf2 (D.toStreamD str2)

-- XXX The order of arguments in "intercalate" is consistent with the list
-- intercalate but inconsistent with gintercalate and other stream interleaving
-- combinators. We can change the order of the arguments in other combinators
-- but then 'interleave' combinator may become a bit unintuitive because we
-- will be starting with the second stream.

-- > intercalate seed unf str = gintercalate unf str unf (repeatM seed)
-- > intercalate a unf str = concatUnfold unf $ intersperse a str
--
-- | 'intersperse' followed by unfold and concat.
--
-- > unwords = intercalate " " UF.fromList
--
-- >>> intercalate " " UF.fromList ["abc", "def", "ghi"]
-- > "abc def ghi"
--
-- /Internal/
{-# INLINE intercalate #-}
intercalate :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalate seed unf str = D.fromStreamD $
    D.concatMapU unf $ D.intersperse seed (toStreamD str)

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- > unwords = S.interpose ' '
--
-- /Internal/
{-# INLINE interpose #-}
interpose :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interpose x unf str =
    D.fromStreamD $ D.interpose (return x) unf (D.toStreamD str)

-- | 'interleaveSuffix' followed by unfold and concat.
--
-- /Internal/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: (IsStream t, Monad m)
    => Unfold m a c -> t m a -> Unfold m b c -> t m b -> t m c
gintercalateSuffix unf1 str1 unf2 str2 =
    D.fromStreamD $ D.gintercalateSuffix
        unf1 (D.toStreamD str1)
        unf2 (D.toStreamD str2)

-- > intercalateSuffix seed unf str = gintercalateSuffix unf str unf (repeatM seed)
-- > intercalateSuffix a unf str = concatUnfold unf $ intersperseSuffix a str
--
-- | 'intersperseSuffix' followed by unfold and concat.
--
-- > unlines = intercalateSuffix "\n" UF.fromList
--
-- >>> intercalate "\n" UF.fromList ["abc", "def", "ghi"]
-- > "abc\ndef\nghi\n"
--
-- /Internal/
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (IsStream t, Monad m)
    => b -> Unfold m b c -> t m b -> t m c
intercalateSuffix seed unf str = fromStreamD $ D.concatMapU unf
    $ D.intersperseSuffix (return seed) (D.toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- > unlines = S.interposeSuffix '\n'
--
-- /Internal/
{-# INLINE interposeSuffix #-}
interposeSuffix :: (IsStream t, Monad m)
    => c -> Unfold m b c -> t m b -> t m c
interposeSuffix x unf str =
    D.fromStreamD $ D.interposeSuffix (return x) unf (D.toStreamD str)

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Internal/
--
lefts :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m a
lefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Internal/
--
rights :: (IsStream t, Monad m, Functor (t m)) => t m (Either a b) -> t m b
rights = fmap (fromRight undefined) . filter isRight

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- Splitting operations that take a predicate and a Fold can be
-- expressed using parseMany. Operations like chunksOf, intervalsOf, split*,
-- can be expressed using parseMany when used with an appropriate Parser.
--
-- XXX We need takeGE/takeBetween to implement "some" using "many".

-- | Apply a 'Fold' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- This is the streaming dual of the 'Streamly.Internal.Data.Fold.many'
-- parse combinator.
--
-- >>> f = Fold.ltake 2 Fold.sum
-- >>> Stream.toList $ Stream.foldMany f $ Stream.fromList [1..10]
-- > [3,7,11,15,19]
--
-- >>> f = Fold.sliceEndWith Fold.toList
-- >>> Stream.toList $ Stream.foldMany f $ Stream.fromList "hello\nworld"
-- > ["hello\n","world"]
--
-- /Internal/
--
{-# INLINE foldMany #-}
foldMany
    :: (IsStream t, Monad m)
    => Fold m a b
    -> t m a
    -> t m b
foldMany f m = D.fromStreamD $ D.foldMany f (D.toStreamD m)

-- | Apply a stream of folds to an input stream and emit the results in the
-- output stream.
--
-- /Internal/
--
{-# INLINE foldSequence #-}
foldSequence
       :: -- (IsStream t, Monad m) =>
       t m (Fold m a b)
    -> t m a
    -> t m b
foldSequence _f _m = undefined

-- | Iterate a fold generator on a stream. The initial value @b@ is used to
-- generate the first fold, the fold is applied on the stream and the result of
-- the fold is used to generate the next fold and so on.
--
-- >>> f x = Fold.ltake 2 (Fold.mconcatTo x)
-- >>> s = Stream.map Sum $ Stream.fromList [1..10]
-- >>> Stream.toList $ Stream.map getSum $ Stream.foldIterate f 0 s
-- > [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- folds where next fold is dependent on the previous fold.
--
-- /Internal/
--
{-# INLINE foldIterate #-}
foldIterate
    :: -- (IsStream t, Monad m) =>
       (b -> Fold m a b)
    -> b
    -> t m a
    -> t m b
foldIterate _f _i _m = undefined
-- D.fromStreamD $ D.foldIterate f i (D.toStreamD m)

-- | Apply a 'Parser' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- This is the streaming equivalent of the 'Streamly.Internal.Data.Parser.many'
-- parse combinator.
--
-- >>> S.toList $ S.parseMany (PR.take 2 $ PR.fromFold FL.sum) $ S.fromList [1..10]
-- > [3,7,11,15,19]
--
-- >>> S.toList $ S.parseMany (PR.line FL.toList) $ S.fromList "hello\nworld"
-- > ["hello\n","world"]
--
-- /Internal/
--
{-# INLINE parseMany #-}
parseMany
    :: (IsStream t, MonadThrow m)
    => Parser m a b
    -> t m a
    -> t m b
parseMany p m =
    D.fromStreamD $ D.parseMany (PRK.fromParserK p) (D.toStreamD m)

{-# INLINE parseManyD #-}
parseManyD
    :: (IsStream t, MonadThrow m)
    => PRD.Parser m a b
    -> t m a
    -> t m b
parseManyD p m =
    D.fromStreamD $ D.parseMany p (D.toStreamD m)

-- | Apply a stream of parsers to an input stream and emit the results in the
-- output stream.
--
-- /Internal/
--
{-# INLINE parseSequence #-}
parseSequence
       :: -- (IsStream t, Monad m) =>
       t m (Parser m a b)
    -> t m a
    -> t m b
parseSequence _f _m = undefined

-- | @parseManyTill collect test stream@ tries the parser @test@ on the input,
-- if @test@ fails it backtracks and tries @collect@, after @collect@ succeeds
-- @test@ is tried again and so on. The parser stops when @test@ succeeds.  The
-- output of @test@ is discarded and the output of @collect@ is emitted in the
-- output stream. The parser fails if @collect@ fails.
--
-- /Unimplemented/
--
{-# INLINE parseManyTill #-}
parseManyTill ::
    -- (IsStream t, MonadThrow m) =>
       Parser m a b
    -> Parser m a x
    -> t m a
    -> t m b
parseManyTill = undefined

-- | Iterate a parser generating function on a stream. The initial value @b@ is
-- used to generate the first parser, the parser is applied on the stream and
-- the result is used to generate the next parser and so on.
--
-- >>> S.toList $ S.map getSum $ S.parseIterate (\b -> PR.take 2 (FL.mconcatTo b)) 0 $ S.map Sum $ S.fromList [1..10]
-- > [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- parsers where next parser is dependent on the previous parser.
--
-- /Internal/
--
{-# INLINE parseIterate #-}
parseIterate
    :: (IsStream t, MonadThrow m)
    => (b -> Parser m a b)
    -> b
    -> t m a
    -> t m b
parseIterate f i m = D.fromStreamD $
    D.parseIterate (PRK.fromParserK . f) i (D.toStreamD m)

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Grouping without looking at elements
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------
--

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Generalized grouping
------------------------------------------------------------------------------

-- This combinator is the most general grouping combinator and can be used to
-- implement all other grouping combinators.
--
-- XXX check if this can implement the splitOn combinator i.e. we can slide in
-- new elements, slide out old elements and incrementally compute the hash.
-- Also, can we implement the windowed classification combinators using this?
--
-- In fact this is a parse. Instead of using a special return value in the fold
-- we are using a mapping function.
--
-- Note that 'scanl'' (usually followed by a map to extract the desired value
-- from the accumulator) can be used to realize many implementations e.g. a
-- sliding window implementation. A scan followed by a mapMaybe is also a good
-- pattern to express many problems where we want to emit a filtered output and
-- not emit an output on every input.
--
-- Passing on of the initial accumulator value to the next fold is equivalent
-- to returning the leftover concept.

{-
-- | @groupScan splitter fold stream@ folds the input stream using @fold@.
-- @splitter@ is applied on the accumulator of the fold every time an item is
-- consumed by the fold. The fold continues until @splitter@ returns a 'Just'
-- value.  A 'Just' result from the @splitter@ specifies a result to be emitted
-- in the output stream and the initial value of the accumulator for the next
-- group's fold. This allows us to control whether to start fresh for the next
-- fold or to continue from the previous fold's output.
--
{-# INLINE groupScan #-}
groupScan
    :: (IsStream t, Monad m)
    => (x -> m (Maybe (b, x))) -> Fold m a x -> t m a -> t m b
groupScan split fold m = undefined
-}

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- >> S.toList $ S.chunksOf 2 FL.sum (S.enumerateFromTo 1 10)
-- > [3,7,11,15,19]
--
-- This can be considered as an n-fold version of 'ltake' where we apply
-- 'ltake' repeatedly on the leftover stream until the stream exhausts.
--
-- @since 0.7.0
{-# INLINE chunksOf #-}
chunksOf
    :: (IsStream t, Monad m)
    => Int -> Fold m a b -> t m a -> t m b
chunksOf n f = foldMany (FL.ltake n f)

-- |
--
-- /Internal/
{-# INLINE chunksOf2 #-}
chunksOf2
    :: (IsStream t, Monad m)
    => Int -> m c -> Fold2 m c a b -> t m a -> t m b
chunksOf2 n action f m = D.fromStreamD $ D.groupsOf2 n action f (D.toStreamD m)

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- > arraysOf n = S.chunksOf n (A.writeN n)
--
-- /Internal/
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Storable a)
    => Int -> t m a -> t m (Array a)
arraysOf n = chunksOf n (writeNUnsafe n)

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------

-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @b \`cmp` a@ is 'True' then @b@ is also assigned to the same
-- group.  If @c \`cmp` a@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@ and the result of the fold is emitted in
-- the output stream.
--
-- >>> S.toList $ S.groupsBy (>) FL.toList $ S.fromList [1,3,7,0,2,5]
-- > [[1,3,7],[0,2,5]]
--
-- @since 0.7.0
{-# INLINE groupsBy #-}
groupsBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsBy cmp f m = D.fromStreamD $ D.groupsBy cmp f (D.toStreamD m)

-- | Unlike @groupsBy@ this function performs a rolling comparison of two
-- successive elements in the input stream. @groupsByRolling cmp f $ S.fromList
-- [a,b,c,...]@ assigns the element @a@ to the first group, if @a \`cmp` b@ is
-- 'True' then @b@ is also assigned to the same group.  If @b \`cmp` c@ is
-- 'True' then @c@ is also assigned to the same group and so on. When the
-- comparison fails a new group is started. Each group is folded using the fold
-- @f@.
--
-- >>> S.toList $ S.groupsByRolling (\a b -> a + 1 == b) FL.toList $ S.fromList [1,2,3,7,8,9]
-- > [[1,2,3],[7,8,9]]
--
-- @since 0.7.0
{-# INLINE groupsByRolling #-}
groupsByRolling
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsByRolling cmp f m =  D.fromStreamD $ D.groupsRollingBy cmp f (D.toStreamD m)

-- |
-- > groups = groupsBy (==)
-- > groups = groupsByRolling (==)
--
-- Groups contiguous spans of equal elements together in individual groups.
--
-- >>> S.toList $ S.groups FL.toList $ S.fromList [1,1,2,2]
-- > [[1,1],[2,2]]
--
-- @since 0.7.0
{-# INLINE groups #-}
groups :: (IsStream t, Monad m, Eq a) => Fold m a b -> t m a -> t m b
groups = groupsBy (==)

------------------------------------------------------------------------------
-- N-ary split on a predicate
------------------------------------------------------------------------------

-- TODO: Use a Splitter configuration similar to the "split" package to make it
-- possible to express all splitting combinations. In general, we can have
-- infix/suffix/prefix/condensing of separators, dropping both leading/trailing
-- separators. We can have a single split operation taking the splitter config
-- as argument.

-- | Split on an infixed separator element, dropping the separator. Splits the
-- stream on separator elements determined by the supplied predicate, separator
-- is considered as infixed between two segments, if one side of the separator
-- is missing then it is parsed as an empty stream.  The supplied 'Fold' is
-- applied on the split segments. With '-' representing non-separator elements
-- and '.' as separator, 'splitOn' splits as follows:
--
-- @
-- "--.--" => "--" "--"
-- "--."   => "--" ""
-- ".--"   => ""   "--"
-- @
--
-- @splitOn (== x)@ is an inverse of @intercalate (S.yield x)@
--
-- Let's use the following definition for illustration:
--
-- > splitOn' p xs = S.toList $ S.splitOn p (FL.toList) (S.fromList xs)
--
-- >>> splitOn' (== '.') ""
-- [""]
--
-- >>> splitOn' (== '.') "."
-- ["",""]
--
-- >>> splitOn' (== '.') ".a"
-- > ["","a"]
--
-- >>> splitOn' (== '.') "a."
-- > ["a",""]
--
-- >>> splitOn' (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitOn' (== '.') "a..b"
-- > ["a","","b"]
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakOn' where we apply
-- 'breakOn' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOn predicate f = foldMany (FL.sliceSepBy predicate f)

-- | Like 'splitOn' but the separator is considered as suffixed to the segments
-- in the stream. A missing suffix at the end is allowed. A separator at the
-- beginning is parsed as empty segment.  With '-' representing elements and
-- '.' as separator, 'splitOnSuffix' splits as follows:
--
-- @
--  "--.--." => "--" "--"
--  "--.--"  => "--" "--"
--  ".--."   => "" "--"
-- @
--
-- > splitOnSuffix' p xs = S.toList $ S.splitSuffixBy p (FL.toList) (S.fromList xs)
--
-- >>> splitOnSuffix' (== '.') ""
-- []
--
-- >>> splitOnSuffix' (== '.') "."
-- [""]
--
-- >>> splitOnSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitOnSuffix' (== '.') ".a"
-- > ["","a"]
--
-- >>> splitOnSuffix' (== '.') "a."
-- > ["a"]
--
-- >>> splitOnSuffix' (== '.') "a.b"
-- > ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a.b."
-- > ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitOnSuffix (== '\n')
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream, dropping the first element
-- of the second segment after each break.
--
{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOnSuffix predicate f m =
    D.fromStreamD $ D.foldMany1 (FL.sliceSepBy predicate f) (D.toStreamD m)

-- | Like 'splitOn' after stripping leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ with '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- > wordsBy' p xs = S.toList $ S.wordsBy p (FL.toList) (S.fromList xs)
--
-- >>> wordsBy' (== ',') ""
-- > []
--
-- >>> wordsBy' (== ',') ","
-- > []
--
-- >>> wordsBy' (== ',') ",a,,b,"
-- > ["a","b"]
--
-- > words = wordsBy isSpace
--
-- @since 0.7.0

-- It is equivalent to splitting in any of the infix/prefix/suffix styles
-- followed by removal of empty segments.
{-# INLINE wordsBy #-}
wordsBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
wordsBy predicate f m =
    D.fromStreamD $ D.wordsBy predicate f (D.toStreamD m)

-- | Like 'splitOnSuffix' but keeps the suffix attached to the resulting
-- splits.
--
-- > splitWithSuffix' p xs = S.toList $ S.splitWithSuffix p (FL.toList) (S.fromList xs)
--
-- >>> splitWithSuffix' (== '.') ""
-- []
--
-- >>> splitWithSuffix' (== '.') "."
-- ["."]
--
-- >>> splitWithSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitWithSuffix' (== '.') ".a"
-- > [".","a"]
--
-- >>> splitWithSuffix' (== '.') "a."
-- > ["a."]
--
-- >>> splitWithSuffix' (== '.') "a.b"
-- > ["a.","b"]
--
-- >>> splitWithSuffix' (== '.') "a.b."
-- > ["a.","b."]
--
-- >>> splitWithSuffix' (== '.') "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream.
--
{-# INLINE splitWithSuffix #-}
splitWithSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitWithSuffix predicate f m =
    D.fromStreamD $ D.foldMany1 (FL.sliceEndWith predicate f) (D.toStreamD m)

------------------------------------------------------------------------------
-- Split on a delimiter sequence
------------------------------------------------------------------------------

-- Int list examples for splitOn:
--
-- >>> splitList [] [1,2,3,3,4]
-- > [[1],[2],[3],[3],[4]]
--
-- >>> splitList [5] [1,2,3,3,4]
-- > [[1,2,3,3,4]]
--
-- >>> splitList [1] [1,2,3,3,4]
-- > [[],[2,3,3,4]]
--
-- >>> splitList [4] [1,2,3,3,4]
-- > [[1,2,3,3],[]]
--
-- >>> splitList [2] [1,2,3,3,4]
-- > [[1],[3,3,4]]
--
-- >>> splitList [3] [1,2,3,3,4]
-- > [[1,2],[],[4]]
--
-- >>> splitList [3,3] [1,2,3,3,4]
-- > [[1,2],[4]]
--
-- >>> splitList [1,2,3,3,4] [1,2,3,3,4]
-- > [[],[]]

-- | Like 'splitOn' but the separator is a sequence of elements instead of a
-- single element.
--
-- For illustration, let's define a function that operates on pure lists:
--
-- @
-- splitOnSeq' pat xs = S.toList $ S.splitOnSeq (A.fromList pat) (FL.toList) (S.fromList xs)
-- @
--
-- >>> splitOnSeq' "" "hello"
-- > ["h","e","l","l","o"]
--
-- >>> splitOnSeq' "hello" ""
-- > [""]
--
-- >>> splitOnSeq' "hello" "hello"
-- > ["",""]
--
-- >>> splitOnSeq' "x" "hello"
-- > ["hello"]
--
-- >>> splitOnSeq' "h" "hello"
-- > ["","ello"]
--
-- >>> splitOnSeq' "o" "hello"
-- > ["hell",""]
--
-- >>> splitOnSeq' "e" "hello"
-- > ["h","llo"]
--
-- >>> splitOnSeq' "l" "hello"
-- > ["he","","o"]
--
-- >>> splitOnSeq' "ll" "hello"
-- > ["he","o"]
--
-- 'splitOnSeq' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOn == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOn . intercalate == id
--
-- /Internal/

-- XXX We can use a polymorphic vector implemented by Array# to represent the
-- sequence, that way we can avoid the Storable constraint. If we still need
-- Storable Array for performance, we can use a separate splitOnArray API for
-- that. We can also have an API where the sequence itself is a lazy stream, so
-- that we can search files in files for example.
{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m = D.fromStreamD $ D.splitOnSeq patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split on any one of the given patterns.
{-# INLINE splitOnAny #-}
splitOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitOnAny subseq f m = undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)
-}

-- | Like 'splitSuffixBy' but the separator is a sequence of elements, instead
-- of a predicate for a single element.
--
-- > splitOnSuffixSeq_ pat xs = S.toList $ S.splitOnSuffixSeq (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitOnSuffixSeq_ "." ""
-- []
--
-- >>> splitOnSuffixSeq_ "." "."
-- [""]
--
-- >>> splitOnSuffixSeq_ "." "a"
-- ["a"]
--
-- >>> splitOnSuffixSeq_ "." ".a"
-- > ["","a"]
--
-- >>> splitOnSuffixSeq_ "." "a."
-- > ["a"]
--
-- >>> splitOnSuffixSeq_ "." "a.b"
-- > ["a","b"]
--
-- >>> splitOnSuffixSeq_ "." "a.b."
-- > ["a","b"]
--
-- >>> splitOnSuffixSeq_ "." "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitOnSuffixSeq "\n"
--
-- 'splitOnSuffixSeq' is an inverse of 'intercalateSuffix'. The following law
-- always holds:
--
-- > intercalateSuffix . splitOnSuffixSeq == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitSuffixOn . intercalateSuffix == id
--
-- /Internal/
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSuffixSeq patt f m =
    D.fromStreamD $ D.splitOnSuffixSeq False patt f (D.toStreamD m)

{-
-- | Like 'splitOn' but drops any empty splits.
--
{-# INLINE wordsOn #-}
wordsOn
    :: (IsStream t, Monad m, Storable a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
wordsOn subseq f m = undefined -- D.fromStreamD $ D.wordsOn f subseq (D.toStreamD m)
-}

-- | Like 'splitOnSuffixSeq' but keeps the suffix intact in the splits.
--
-- > splitWithSuffixSeq'_ pat xs = S.toList $ S.splitWithSuffixSeq (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitWithSuffixSeq' "." ""
-- [""]
--
-- >>> splitWithSuffixSeq' "." "."
-- ["."]
--
-- >>> splitWithSuffixSeq' "." "a"
-- ["a"]
--
-- >>> splitWithSuffixSeq' "." ".a"
-- > [".","a"]
--
-- >>> splitWithSuffixSeq' "." "a."
-- > ["a."]
--
-- >>> splitWithSuffixSeq' "." "a.b"
-- > ["a.","b"]
--
-- >>> splitWithSuffixSeq' "." "a.b."
-- > ["a.","b."]
--
-- >>> splitWithSuffixSeq' "." "a..b.."
-- > ["a.",".","b.","."]
--
-- /Internal/
{-# INLINE splitWithSuffixSeq #-}
splitWithSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitWithSuffixSeq patt f m =
    D.fromStreamD $ D.splitOnSuffixSeq True patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
{-# INLINE splitOnSuffixSeqAny #-}
splitOnSuffixSeqAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitOnSuffixSeqAny subseq f m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)
-}

------------------------------------------------------------------------------
-- Nested Split
------------------------------------------------------------------------------

-- | @splitInnerBy splitter joiner stream@ splits the inner containers @f a@ of
-- an input stream @t m (f a)@ using the @splitter@ function. Container
-- elements @f a@ are collected until a split occurs, then all the elements
-- before the split are joined using the @joiner@ function.
--
-- For example, if we have a stream of @Array Word8@, we may want to split the
-- stream into arrays representing lines separated by '\n' byte such that the
-- resulting stream after a split would be one array for each line.
--
-- CAUTION! This is not a true streaming function as the container size after
-- the split and merge may not be bounded.
--
-- /Internal/
{-# INLINE splitInnerBy #-}
splitInnerBy
    :: (IsStream t, Monad m)
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBy splitter joiner xs =
    D.fromStreamD $ D.splitInnerBy splitter joiner $ D.toStreamD xs

-- | Like 'splitInnerBy' but splits assuming the separator joins the segment in
-- a suffix style.
--
-- /Internal/
{-# INLINE splitInnerBySuffix #-}
splitInnerBySuffix
    :: (IsStream t, Monad m, Eq (f a), Monoid (f a))
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBySuffix splitter joiner xs =
    D.fromStreamD $ D.splitInnerBySuffix splitter joiner $ D.toStreamD xs

------------------------------------------------------------------------------
-- Reorder in sequence
------------------------------------------------------------------------------

{-
-- Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
{-# INLINE reassembleBy #-}
reassembleBy
    :: (IsStream t, Monad m)
    => Fold m a b
    -> (a -> a -> Int)
    -> t m a
    -> t m b
reassembleBy = undefined
-}

------------------------------------------------------------------------------
-- Distributing
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
-- @
-- > S.drain $ S.tap (FL.drainBy print) (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Compare with 'trace'.
--
-- @since 0.7.0
{-# INLINE tap #-}
tap :: (IsStream t, Monad m) => FL.Fold m a b -> t m a -> t m a
tap f xs = D.fromStreamD $ D.tap f (D.toStreamD xs)

-- | @tapOffsetEvery offset n@ taps every @n@th element in the stream
-- starting at @offset@. @offset@ can be between @0@ and @n - 1@. Offset 0
-- means start at the first element in the stream. If the offset is outside
-- this range then @offset `mod` n@ is used as offset.
--
-- @
-- >>> S.drain $ S.tapOffsetEvery 0 2 (FL.mapM print FL.toList) $ S.enumerateFromTo 0 10
-- > [0,2,4,6,8,10]
-- @
--
-- /Internal/
--
{-# INLINE tapOffsetEvery #-}
tapOffsetEvery :: (IsStream t, Monad m)
    => Int -> Int -> FL.Fold m a b -> t m a -> t m a
tapOffsetEvery offset n f xs =
    D.fromStreamD $ D.tapOffsetEvery offset n f (D.toStreamD xs)

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
-- > S.drain $ S.tapAsync (S.mapM_ print) (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
--
-- Exceptions from the concurrently running fold are propagated to the current
-- computation.  Note that, because of buffering in the fold, exceptions may be
-- delayed and may not correspond to the current element being processed in the
-- parent stream, but we guarantee that before the parent stream stops the tap
-- finishes and all exceptions from it are drained.
--
--
-- Compare with 'tap'.
--
-- /Internal/
{-# INLINE tapAsync #-}
tapAsync :: (IsStream t, MonadAsync m) => FL.Fold m a b -> t m a -> t m a
tapAsync f xs = D.fromStreamD $ D.tapAsync f (D.toStreamD xs)

-- | @pollCounts predicate transform fold stream@ counts those elements in the
-- stream that pass the @predicate@. The resulting count stream is sent to
-- another thread which transforms it using @transform@ and then folds it using
-- @fold@.  The thread is automatically cleaned up if the stream stops or
-- aborts due to exception.
--
-- For example, to print the count of elements processed every second:
--
-- @
-- > S.drain $ S.pollCounts (const True) (S.rollingMap (-) . S.delayPost 1) (FL.drainBy print)
--           $ S.enumerateFrom 0
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Internal/
--
{-# INLINE pollCounts #-}
pollCounts ::
       (IsStream t, MonadAsync m)
    => (a -> Bool)
    -> (t m Int -> t m Int)
    -> Fold m Int b
    -> t m a
    -> t m a
pollCounts predicate transf f xs =
      D.fromStreamD
    $ D.pollCounts predicate (D.toStreamD . transf . D.fromStreamD) f
    $ D.toStreamD xs

-- | Calls the supplied function with the number of elements consumed
-- every @n@ seconds. The given function is run in a separate thread
-- until the end of the stream. In case there is an exception in the
-- stream the thread is killed during the next major GC.
--
-- Note: The action is not guaranteed to run if the main thread exits.
--
-- @
-- > delay n = threadDelay (round $ n * 1000000) >> return n
-- > S.drain $ S.tapRate 2 (\\n -> print $ show n ++ " elements processed") (delay 1 S.|: delay 0.5 S.|: delay 0.5 S.|: S.nil)
-- 2 elements processed
-- 1 elements processed
-- @
--
-- Note: This may not work correctly on 32-bit machines.
--
-- /Internal/
{-# INLINE tapRate #-}
tapRate ::
       (IsStream t, MonadAsync m, MonadCatch m)
    => Double
    -> (Int -> m b)
    -> t m a
    -> t m a
tapRate n f xs = D.fromStreamD $ D.tapRate n f $ D.toStreamD xs

-- | Apply a monadic function to each element flowing through the stream and
-- discard the results.
--
-- @
-- > S.drain $ S.trace print (S.enumerateFromTo 1 2)
-- 1
-- 2
-- @
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
-- > S.drain $ S.trace_ (print "got here") (S.enumerateFromTo 1 2)
-- "got here"
-- "got here"
-- @
--
-- See also: 'trace', 'interspersePrefix_'
--
-- /Internal/
{-# INLINE trace_ #-}
trace_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
trace_ eff = Serial.mapM (\x -> eff >> return x)


------------------------------------------------------------------------------
-- Generalize the underlying monad
------------------------------------------------------------------------------

-- | Transform the inner monad of a stream using a natural transformation.
--
-- / Internal/
--
{-# INLINE hoist #-}
hoist :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> SerialT m a -> SerialT n a
hoist f xs = fromStreamS $ S.hoist f (toStreamS xs)

-- | Generalize the inner monad of the stream from 'Identity' to any monad.
--
-- / Internal/
--
{-# INLINE generally #-}
generally :: (IsStream t, Monad m) => t Identity a -> t m a
generally xs = fromStreamS $ S.hoist (return . runIdentity) (toStreamS xs)

------------------------------------------------------------------------------
-- Add and remove a monad transformer
------------------------------------------------------------------------------

-- | Lift the inner monad @m@ of a stream @t m a@ to @tr m@ using the monad
-- transformer @tr@.
--
-- / Internal/
--
{-# INLINE liftInner #-}
liftInner :: (Monad m, IsStream t, MonadTrans tr, Monad (tr m))
    => t m a -> t (tr m) a
liftInner xs = fromStreamD $ D.liftInner (toStreamD xs)

------------------------------------------------------------------------------
-- Sharing read only state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'ReaderT'.
--
-- / Internal/
--
{-# INLINE runReaderT #-}
runReaderT :: (IsStream t, Monad m) => m s -> t (ReaderT s m) a -> t m a
runReaderT s xs = fromStreamD $ D.runReaderT s (toStreamD xs)

-- | Run a stream transformation using a given environment.
--
-- See also: 'Serial.map'
--
-- / Internal/
--
{-# INLINE usingReaderT #-}
usingReaderT
    :: (Monad m, IsStream t)
    => m r
    -> (t (ReaderT r m) a -> t (ReaderT r m) a)
    -> t m a
    -> t m a
usingReaderT r f xs = runReaderT r $ f $ liftInner xs

------------------------------------------------------------------------------
-- Sharing read write state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'StateT'.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- / Internal/
--
{-# INLINE evalStateT #-}
evalStateT ::  Monad m => m s -> SerialT (StateT s m) a -> SerialT m a
evalStateT s xs = fromStreamD $ D.evalStateT s (toStreamD xs)

-- | Run a stateful (StateT) stream transformation using a given state.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- See also: 'scanl''
--
-- / Internal/
--
{-# INLINE usingStateT #-}
usingStateT
    :: Monad m
    => m s
    -> (SerialT (StateT s m) a -> SerialT (StateT s m) a)
    -> SerialT m a
    -> SerialT m a
usingStateT s f xs = evalStateT s $ f $ liftInner xs

-- | Evaluate the inner monad of a stream as 'StateT' and emit the resulting
-- state and value pair after each step.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- / Internal/
--
{-# INLINE runStateT #-}
runStateT :: Monad m => m s -> SerialT (StateT s m) a -> SerialT m (s, a)
runStateT s xs = fromStreamD $ D.runStateT s (toStreamD xs)
