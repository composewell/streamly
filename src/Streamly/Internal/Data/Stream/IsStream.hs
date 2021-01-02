{-# OPTIONS_GHC -Wno-orphans  #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This is an Internal module consisting of released, unreleased and
-- unimplemented APIs. For stable and released APIs please see
-- "Streamly.Prelude" module. This module provides documentation only for the
-- unreleased and unimplemented APIs. For documentation on released APIs please
-- see "Streamly.Prelude" module.

module Streamly.Internal.Data.Stream.IsStream
    (
    -- * Stream Types
    -- ** Serial Streams
      SerialT
    , Serial
    , WSerialT
    , WSerial

    -- ** Speculative Streams
    , AheadT
    , Ahead

    -- ** Asynchronous Streams
    , AsyncT
    , Async
    , WAsyncT
    , WAsync
    , ParallelT
    , Parallel
    , mkAsync

    -- ** Zipping Streams
    , ZipSerialM
    , ZipSerial
    , ZipAsyncM
    , ZipAsync

    -- * Stream Type Adapters
    , IsStream ()

    , serially
    , wSerially
    , asyncly
    , aheadly
    , wAsyncly
    , parallely
    , zipSerially
    , zipAsyncly
    , adapt

    -- * Type Synonyms
    , MonadAsync

    -- * Construction
    -- ** Primitives
    , K.nil
    , K.nilM
    , K.cons
    , (K..:)

    , consM
    , (|:)

    -- ** From Values
    , yield
    , yieldM
    , repeat
    , repeatM
    , replicate
    , replicateM

    -- ** Enumeration
    , Enumerable (..)
    , enumerate
    , enumerateTo

    -- ** Time Enumeration
    , times
    , absTimes
    , relTimes
    , durations
    , ticks
    , timeout
    , currentTime

    -- ** From Generators
    , unfoldr
    , unfoldrM
    , unfold
    , unfold0
    , fromIndices
    , fromIndicesM

    -- ** Iteration
    , iterate
    , iterateM

    -- ** Cyclic Elements
    , K.mfix

    -- ** From Containers
    , P.fromList
    , fromListM
    , K.fromFoldable
    , fromFoldableM
    , fromPrimIORef
    , fromCallback

    -- * Elimination

    -- ** Deconstruction
    , uncons
    , tail
    , init

    -- ** Folding
    -- ** Right Folds
    , foldrM
    , foldrS
    , foldrT
    , foldr

    -- ** Left Folds
    , foldl'
    , foldl1'
    , foldlM'

    -- ** Composable Left Folds
    -- | See "Streamly.Internal.Data.Fold"
    , fold
    , foldMany
    , foldSequence
    , foldIterate

    -- ** Parsers
    -- | See "Streamly.Internal.Data.Parser"
    , parse
    , parseK
    , parseD
    , parseMany
    , parseManyD
    , parseManyTill
    , parseSequence
    , parseIterate

    -- ** Concurrent Folds
    , foldAsync
    , (|$.)
    , (|&.)

    -- ** Full Folds

    -- -- ** To Summary (Full Folds)
    , drain
    , last
    , length
    , sum
    , product
    , mconcat

    -- -- ** To Summary (Maybe) (Full Folds)
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the

    -- ** Lazy Folds
    -- -- ** To Containers (Full Folds)
    , toList
    , toListRev
    , toPure
    , toPureRev

    -- ** Composable Left Folds

    , toStream    -- XXX rename to write?
    , toStreamRev -- XXX rename to writeRev?

    -- ** Partial Folds

    -- -- ** To Elements (Partial Folds)
    , drainN
    , drainWhile

    -- -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , headElse
    , findM
    , find
    , lookup
    , findIndex
    , elemIndex

    -- -- ** To Boolean (Partial Folds)
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- ** Multi-Stream folds
    -- Full equivalence
    , eqBy
    , cmpBy

    -- finding subsequences
    , isPrefixOf
    , isSuffixOf
    , isInfixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix
    , stripSuffix
    -- , stripInfix
    , dropPrefix
    , dropInfix
    , dropSuffix

    -- * Transformation
    , transform

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
    , interjectSuffix
    , delay
    , delayPost
    , delayPre

    -- ** Indexing
    , indexed
    , indexedR
    , timestamped
    , timeIndexed
    -- , timestampedR -- timer

    -- ** Reordering
    , reverse
    , reverse'

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
    , intervalsOf

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
    , splitBySeq
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

    -- * Windowed Classification

    -- | Split the stream into windows or chunks in space or time. Each window
    -- can be associated with a key, all events associated with a particular
    -- key in the window can be folded to a single result. The stream is split
    -- into windows of specified size, the window can be terminated early if
    -- the closing flag is specified in the input stream.
    --
    -- The term "chunk" is used for a space window and the term "session" is
    -- used for a time window.

    -- ** Tumbling Windows
    -- | A new window starts after the previous window is finished.

    -- , classifyChunksOf
    , classifySessionsBy
    , classifySessionsOf

    -- ** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.

    -- , classifyKeepAliveChunks
    , classifyKeepAliveSessions

    {-
    -- ** Sliding Windows
    -- | A new window starts after the specified slide from the previous
    -- window. Therefore windows can overlap.
    , classifySlidingChunks
    , classifySlidingSessions
    -}
    -- ** Sliding Window Buffers
    -- , slidingChunkBuffer
    -- , slidingSessionBuffer

    -- * Combining Streams

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
    , concatM
    , concatMap
    , concatMapM
    , concatMapWith
    , concatSmapMWith
    -- , bindWith

    -- ** Flattening Using Unfolds
    , concatUnfold
    , concatUnfoldInterleave
    , concatUnfoldRoundrobin

    -- ** Flattening a Tree of Streams
    , iterateMapWith

    -- ** Flattening a Graph of Streams
    , iterateSmapMWith

    -- ** Inserting Streams in Streams
    , gintercalate
    , gintercalateSuffix
    , intercalate
    , intercalateSuffix
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- * Exceptions
    , before
    , after_
    , after
    , bracket_
    , bracket
    , onException
    , finally_
    , finally
    , ghandle
    , handle

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

    -- * Maybe Streams
    , catMaybes
    , mapMaybe
    , mapMaybeM

    -- * Either Streams
    , lefts
    , rights
    , iterateMapLeftsWith

    -- * Concurrency Control
    , maxThreads
    , maxBuffer

    -- * Rate Limiting
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Diagnostics
    , inspectMode

    -- * Deprecated
    , K.once
    , each
    , scanx
    , foldx
    , foldxM
    , foldr1
    , runStream
    , runN
    , runWhile
    , fromHandle
    , toHandle
    )
where

import Control.Concurrent (threadDelay)
import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind (Type)
import Data.Heap (Entry(..))
import Data.Maybe (isNothing)
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Stream.Enumeration
       (Enumerable(..), enumerate, enumerateTo)
import Streamly.Internal.Data.Fold.Types (Fold (..))
import Streamly.Internal.Data.Array.Storable.Foreign.Types (Array)
import Streamly.Internal.Data.SVar (MonadAsync, Rate (..))
import Streamly.Internal.Data.Stream.Ahead (AheadT, Ahead, aheadly)
import Streamly.Internal.Data.Stream.Async
       ( AsyncT, Async, WAsyncT, WAsync, mkAsync, asyncly
       , wAsyncly)
import Streamly.Internal.Data.Stream.Combinators
      ( inspectMode, maxBuffer, maxThreads, rate, avgRate, minRate
      , maxRate, constRate)
import Streamly.Internal.Data.Stream.Parallel
       ( ParallelT, Parallel, parallely)
import Streamly.Internal.Data.Stream.Prelude (toStreamS)
import Streamly.Internal.Data.Stream.StreamK (IsStream((|:), consM), adapt)
import Streamly.Internal.Data.Stream.Serial
       ( SerialT, WSerialT, Serial, WSerial, serially
       , wSerially)
import Streamly.Internal.Data.Stream.Zip
       ( ZipSerialM, ZipSerial, ZipAsyncM, ZipAsync, zipSerially, zipAsyncly)
import Streamly.Internal.Data.Time.Units
       ( AbsTime, MilliSecond64(..), addToAbsTime, toRelTime
       , toAbsTime, RelTime64)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import Streamly.Internal.Data.Stream.IsStream.Generate
import Streamly.Internal.Data.Stream.IsStream.Eliminate
import Streamly.Internal.Data.Stream.IsStream.Transform
import Streamly.Internal.Data.Stream.IsStream.Exception

import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.Prelude as P
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Zip as Z
import qualified Data.Heap as H
import qualified Data.Map.Strict as Map
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
-- Conversions
------------------------------------------------------------------------------

-- | Takes a callback setter function and provides it with a callback.  The
-- callback when invoked adds a value at the tail of the stream. Returns a
-- stream of values generated by the callback.
--
-- /Internal/
--
{-# INLINE fromCallback #-}
fromCallback :: MonadAsync m => ((a -> m ()) -> m ()) -> SerialT m a
fromCallback setCallback = concatM $ do
    (callback, stream) <- D.newCallbackStream
    setCallback callback
    return stream

------------------------------------------------------------------------------
-- Specialized folds
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

-- |
-- > drainN n = drain . take n
-- > drainN n = fold (Fold.ltake n Fold.drain)
--
-- Run maximum up to @n@ iterations of a stream.
--
-- @since 0.7.0
{-# INLINE drainN #-}
drainN :: Monad m => Int -> SerialT m a -> m ()
drainN n = drain . take n

-- |
-- > runN n = runStream . take n
--
-- Run maximum up to @n@ iterations of a stream.
--
-- @since 0.6.0
{-# DEPRECATED runN "Please use \"drainN\" instead" #-}
{-# INLINE runN #-}
runN :: Monad m => Int -> SerialT m a -> m ()
runN = drainN

-- |
-- > drainWhile p = drain . takeWhile p
-- > drainWhile p = fold (Fold.sliceSepBy (not . p) Fold.drain)
--
-- Run a stream as long as the predicate holds true.
--
-- @since 0.7.0
{-# INLINE drainWhile #-}
drainWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
drainWhile p = drain . takeWhile p

-- |
-- > runWhile p = runStream . takeWhile p
--
-- Run a stream as long as the predicate holds true.
--
-- @since 0.6.0
{-# DEPRECATED runWhile "Please use \"drainWhile\" instead" #-}
{-# INLINE runWhile #-}
runWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
runWhile = drainWhile

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- | Intersperse a monadic action into the input stream after every @n@
-- seconds.
--
-- @
-- > S.drain $ S.interjectSuffix 1 (putChar ',') $ S.mapM (\\x -> threadDelay 1000000 >> putChar x) $ S.fromList "hello"
-- "h,e,l,l,o"
-- @
--
-- /Internal/
{-# INLINE interjectSuffix #-}
interjectSuffix
    :: (IsStream t, MonadAsync m)
    => Double -> m a -> t m a -> t m a
interjectSuffix n f xs = xs `Par.parallelFst` repeatM timed
    where timed = liftIO (threadDelay (round $ n * 1000000)) >> f

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

-- Note: The timestamp stream must be the second stream in the zip so that the
-- timestamp is generated after generating the stream element and not before.
-- If we do not do that then the following example will generate the same
-- timestamp for first two elements:
--
-- S.mapM_ print $ S.timestamped $ S.delay $ S.enumerateFromTo 1 3
--
-- | Pair each element in a stream with an absolute timestamp, using a clock of
-- specified granularity.  The timestamp is generated just before the element
-- is consumed.
--
-- @
-- >>> S.mapM_ print $ S.timestampWith 0.01 $ S.delay 1 $ S.enumerateFromTo 1 3
-- @
--
-- /Internal/
--
{-# INLINE timestampWith #-}
timestampWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (AbsTime, a)
timestampWith g stream = Z.zipWith (flip (,)) stream (absTimesWith g)

-- TBD: check performance vs a custom implementation without using zipWith.
--
-- /Internal/
--
{-# INLINE timestamped #-}
timestamped :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (AbsTime, a)
timestamped = timestampWith 0.01

-- | Pair each element in a stream with relative times starting from 0, using a
-- clock with the specified granularity. The time is measured just before the
-- element is consumed.
--
-- @
-- >>> S.mapM_ print $ S.timeIndexWith 0.01 $ S.delay 1 $ S.enumerateFromTo 1 3
-- @
--
-- /Internal/
--
{-# INLINE timeIndexWith #-}
timeIndexWith :: (IsStream t, MonadAsync m, Functor (t m))
    => Double -> t m a -> t m (RelTime64, a)
timeIndexWith g stream = Z.zipWith (flip (,)) stream (relTimesWith g)

-- | Pair each element in a stream with relative times starting from 0, using a
-- 10 ms granularity clock. The time is measured just before the element is
-- consumed.
--
-- @
-- >>> S.mapM_ print $ S.timeIndexed $ S.delay 1 $ S.enumerateFromTo 1 3
-- (RelTime64 (NanoSecond64 0),1)
-- (RelTime64 (NanoSecond64 996239000),2)
-- (RelTime64 (NanoSecond64 1996712000),3)
-- @
--
-- /Internal/
--
{-# INLINE timeIndexed #-}
timeIndexed :: (IsStream t, MonadAsync m, Functor (t m))
    => t m a -> t m (RelTime64, a)
timeIndexed = timeIndexWith 0.01

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Returns the first index that satisfies the given predicate.
--
-- > findIndex = fold Fold.findIndex
--
-- @since 0.5.0
{-# INLINE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe Int)
findIndex p = head . findIndices p

-- | Returns the first index where a given value is found in the stream.
--
-- > elemIndex a = findIndex (== a)
--
-- @since 0.5.0
{-# INLINE elemIndex #-}
elemIndex :: (Monad m, Eq a) => a -> SerialT m a -> m (Maybe Int)
elemIndex a = findIndex (== a)

------------------------------------------------------------------------------
-- Substreams
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is a suffix of the second. A stream is
-- considered a suffix of itself.
--
-- @
-- > S.isSuffixOf (S.fromList "hello") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- Space: @O(n)@, buffers entire input stream and the suffix.
--
-- /Internal/
--
-- /Suboptimal/ - Help wanted.
--
{-# INLINE isSuffixOf #-}
isSuffixOf :: (Monad m, Eq a) => SerialT m a -> SerialT m a -> m Bool
isSuffixOf suffix stream = reverse suffix `isPrefixOf` reverse stream

-- | Returns 'True' if the first stream is an infix of the second. A stream is
-- considered an infix of itself.
--
-- @
-- > S.isInfixOf (S.fromList "hello") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- Space: @O(n)@ worst case where @n@ is the length of the infix.
--
-- /Internal/
--
-- /Requires 'Storable' constraint/ - Help wanted.
--
{-# INLINE isInfixOf #-}
isInfixOf :: (MonadIO m, Eq a, Enum a, Storable a)
    => SerialT m a -> SerialT m a -> m Bool
isInfixOf infx stream = do
    arr <- fold A.write infx
    -- XXX can use breakOnSeq instead (when available)
    r <- null $ drop 1 $ splitOnSeq arr FL.drain stream
    return (not r)

-- Note: If we want to return a Maybe value to know whether the
-- suffix/infix was present or not along with the stripped stream then
-- we need to buffer the whole input stream.
--
-- | Drops the given suffix from a stream. Returns 'Nothing' if the stream does
-- not end with the given suffix. Returns @Just nil@ when the suffix is the
-- same as the stream.
--
-- It may be more efficient to convert the stream to an Array and use
-- stripSuffix on that especially if the elements have a Storable or Prim
-- instance.
--
-- Space: @O(n)@, buffers the entire input stream as well as the suffix
--
-- /Internal/
{-# INLINE stripSuffix #-}
stripSuffix
    :: (Monad m, Eq a)
    => SerialT m a -> SerialT m a -> m (Maybe (SerialT m a))
stripSuffix m1 m2 = fmap reverse <$> stripPrefix (reverse m1) (reverse m2)

------------------------------------------------------------------------------
-- Split on a delimiter sequence
------------------------------------------------------------------------------

-- XXX use a non-monadic intersperse to remove the MonadAsync constraint.
--
-- | Like 'splitOnSeq' but splits the separator as well, as an infix token.
--
-- > splitOn'_ pat xs = S.toList $ S.splitOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitOn'_ "" "hello"
-- > ["h","","e","","l","","l","","o"]
--
-- >>> splitOn'_ "hello" ""
-- > [""]
--
-- >>> splitOn'_ "hello" "hello"
-- > ["","hello",""]
--
-- >>> splitOn'_ "x" "hello"
-- > ["hello"]
--
-- >>> splitOn'_ "h" "hello"
-- > ["","h","ello"]
--
-- >>> splitOn'_ "o" "hello"
-- > ["hell","o",""]
--
-- >>> splitOn'_ "e" "hello"
-- > ["h","e","llo"]
--
-- >>> splitOn'_ "l" "hello"
-- > ["he","l","","l","o"]
--
-- >>> splitOn'_ "ll" "hello"
-- > ["he","ll","o"]
--
-- /Internal/
{-# INLINE splitBySeq #-}
splitBySeq
    :: (IsStream t, MonadAsync m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitBySeq patt f m =
    intersperseM (fold f (A.toStream patt)) $ splitOnSeq patt f m

------------------------------------------------------------------------------
-- Flattening Trees
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
-- iterateM f = iterateMapWith serial (yieldM . f) . yieldM
-- @
--
-- It can be used to traverse a tree structure.  For example, to list a
-- directory tree:
--
-- @
-- Stream.iterateMapWith Stream.serial
--     (either Dir.toEither (const nil))
--     (yield (Left "tmp"))
-- @
--
-- /Internal/
--
{-# INLINE iterateMapWith #-}
iterateMapWith
    :: IsStream t
    => (t m a -> t m a -> t m a)
    -> (a -> t m a)
    -> t m a
    -> t m a
iterateMapWith combine f = concatMapWith combine go
    where
    go x = yield x `combine` concatMapWith combine go (f x)

{-
{-# INLINE iterateUnfold #-}
iterateUnfold :: (IsStream t, MonadAsync m)
    => Unfold m a a -> t m a -> t m a
iterateUnfold unf xs = undefined
-}

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
-- /Internal/
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
    concatMap (\b -> concatMapWith combine (go b) stream) (yieldM initial)

    where

    go b a = yield a `combine` feedback b a

    feedback b a =
        concatMap
            (\(b1, s) -> concatMapWith combine (go b1) s)
            (yieldM $ f b a)

------------------------------------------------------------------------------
-- Flattening Lists
------------------------------------------------------------------------------

-- | Given a stream value in the underlying monad, lift and join the underlying
-- monad with the stream monad.
--
-- @
-- concatM = concat . yieldM
-- concatM = concat . lift    -- requires @(MonadTrans t)@
-- concatM = join . lift      -- requires @(MonadTrans t@, @Monad (t m))@
-- @
--
-- See also: 'concat', 'sequence'
--
--  /Internal/
--
{-# INLINE concatM #-}
concatM :: (IsStream t, Monad m) => m (t m a) -> t m a
concatM generator = concatMapM (\() -> generator) (yield ())

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

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
-- iterateMapLeftsWith serial Dir.toEither (yield (Left "tmp"))
-- @
--
-- /Internal/
--
{-# INLINE iterateMapLeftsWith #-}
iterateMapLeftsWith
    :: IsStream t
    => (t m (Either a b) -> t m (Either a b) -> t m (Either a b))
    -> (a -> t m (Either a b))
    -> t m (Either a b)
    -> t m (Either a b)
iterateMapLeftsWith combine f = iterateMapWith combine (either f (const K.nil))

-------------------------------------------------------------------------------
-- Breaking
-------------------------------------------------------------------------------

-- XXX we can implement this by repeatedly applying the 'lrunFor' fold.
-- XXX add this example after fixing the serial stream rate control
-- >>> S.toList $ S.take 5 $ intervalsOf 1 FL.sum $ constRate 2 $ S.enumerateFrom 1
-- > [3,7,11,15,19]
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- @since 0.7.0
{-# INLINE intervalsOf #-}
intervalsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
intervalsOf n f xs =
    splitWithSuffix isNothing (FL.lcatMaybes f)
        (interjectSuffix n (return Nothing) (Serial.map Just xs))

------------------------------------------------------------------------------
-- Windowed classification
------------------------------------------------------------------------------

-- We divide the stream into windows or chunks in space or time and each window
-- can be associated with a key, all events associated with a particular key in
-- the window can be folded to a single result. The stream can be split into
-- windows by size or by using a split predicate on the elements in the stream.
-- For example, when we receive a closing flag, we can close the window.
--
-- A "chunk" is a space window and a "session" is a time window. Are there any
-- other better short words to describe them. An alternative is to use
-- "swindow" and "twindow". Another word for "session" could be "spell".
--
-- TODO: To mark the position in space or time we can have Indexed or
-- TimeStamped types. That can make it easy to deal with the position indices
-- or timestamps.

------------------------------------------------------------------------------
-- Keyed Sliding Windows
------------------------------------------------------------------------------

{-
{-# INLINABLE classifySlidingChunks #-}
classifySlidingChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Int              -- ^ window slide
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifySlidingChunks wsize wslide (Fold step initial extract) str
    = undefined

-- XXX Another variant could be to slide the window on an event, e.g. in TCP we
-- slide the send window when an ack is received and we slide the receive
-- window when a sequence is complete. Sliding is stateful in case of TCP,
-- sliding releases the send buffer or makes data available to the user from
-- the receive buffer.
{-# INLINABLE classifySlidingSessions #-}
classifySlidingSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ time window size
    -> Double         -- ^ window slide
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
classifySlidingSessions tick interval slide (Fold step initial extract) str
    = undefined
-}

------------------------------------------------------------------------------
-- Sliding Window Buffers
------------------------------------------------------------------------------

-- These buffered versions could be faster than concurrent incremental folds of
-- all overlapping windows as in many cases we may not need all the values to
-- compute the fold, we can just compute the result using the old value and new
-- value.  However, we may need the buffer once in a while, for example for
-- string search we usually compute the hash incrementally but when the hash
-- matches the hash of the pattern we need to compare the whole string.
--
-- XXX we should be able to implement sequence based splitting combinators
-- using this combinator.

{-
-- | Buffer n elements of the input in a ring buffer. When t new elements are
-- collected, slide the window to remove the same number of oldest elements,
-- insert the new elements, and apply an incremental fold on the sliding
-- window, supplying the outgoing elements, the new ring buffer as arguments.
slidingChunkBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int -- window size
    -> Int -- window slide
    -> Fold m (Ring a, Array a) b
    -> t m a
    -> t m b
slidingChunkBuffer = undefined

-- Buffer n seconds worth of stream elements of the input in a radix tree.
-- Every t seconds, remove the items that are older than n seconds, and apply
-- an incremental fold on the sliding window, supplying the outgoing elements,
-- and the new radix tree buffer as arguments.
slidingSessionBuffer
    :: (IsStream t, Monad m, Ord a, Storable a)
    => Int    -- window size
    -> Int    -- tick size
    -> Fold m (RTree a, Array a) b
    -> t m a
    -> t m b
slidingSessionBuffer = undefined
-}

------------------------------------------------------------------------------
-- Keyed Session Windows
------------------------------------------------------------------------------

{-
-- | Keyed variable size space windows. Close the window if we do not receive a
-- window event in the next "spaceout" elements.
{-# INLINABLE classifyChunksBy #-}
classifyChunksBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Bool  -- ^ reset the spaceout when a chunk window element is received
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyChunksBy spanout reset (Fold step initial extract) str = undefined

-- | Like 'classifyChunksOf' but the chunk size is reset if an element is
-- received within the chunk size window. The chunk gets closed only if no
-- element is received within the chunk window.
--
{-# INLINABLE classifyKeepAliveChunks #-}
classifyKeepAliveChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyKeepAliveChunks spanout = classifyChunksBy spanout True
-}

data SessionState t m k a b = SessionState
    { sessionCurTime :: !AbsTime  -- ^ time since last event
    , sessionEventTime :: !AbsTime -- ^ time as per last event
    , sessionCount :: !Int -- ^ total number sessions in progress
    , sessionTimerHeap :: H.Heap (H.Entry AbsTime k) -- ^ heap for timeouts
    , sessionKeyValueMap :: Map.Map k a -- ^ Stored sessions for keys
    , sessionOutputStream :: t (m :: Type -> Type) (k, b) -- ^ Completed sessions
    }

#undef Type

-- XXX Perhaps we should use an "Event a" type to represent timestamped data.
-- | @classifySessionsBy tick timeout idle pred f stream@ groups timestamped
-- events in an input event stream into sessions based on a session key. Each
-- element in the input stream is an event consisting of a triple @(session key,
-- sesssion data, timestamp)@.  @session key@ is a key that uniquely identifies
-- the session.  All the events belonging to a session are folded using the fold
-- @f@ until the fold terminates or a timeout has occurred.  The session key and
-- the result of the fold are emitted in the output stream when the session is
-- purged.
--
-- When @idle@ is 'False', @timeout@ is the maximum lifetime of a session in
-- seconds, measured from the @timestamp@ of the first event in that session.
-- When @idle@ is 'True' then the timeout is an idle timeout, it is reset after
-- every event received in the session.
--
-- @timestamp@ in an event characterizes the time when the input event was
-- generated, this is an absolute time measured from some @Epoch@.  The notion
-- of current time is maintained by a monotonic event time clock using the
-- timestamps seen in the input stream. The latest timestamp seen till now is
-- used as the base for the current time.  When no new events are seen, a timer
-- is started with a tick duration specified by @tick@. This timer is used to
-- detect session timeouts in the absence of new events.
--
-- The predicate @pred@ is invoked with the current session count, if it
-- returns 'True' a session is ejected from the session cache before inserting
-- a new session. This could be useful to alert or eject sessions when the
-- number of sessions becomes too high.
--
-- /Internal/
--
{-# INLINABLE classifySessionsBy #-}
classifySessionsBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ session timeout in seconds
    -> Bool           -- ^ reset the timeout when an event is received
    -> (Int -> m Bool) -- ^ predicate to eject sessions based on session count
    -> Fold m a b  -- ^ Fold to be applied to session events
    -> t m (k, a, AbsTime) -- ^ session key, data, timestamp
    -> t m (k, b) -- ^ session key, fold result
classifySessionsBy tick tmout reset ejectPred
    (Fold step initial extract) str =
    concatMap sessionOutputStream $
        scanlMAfter' sstep (return szero) flush stream

    where

    timeoutMs = toRelTime (round (tmout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = SessionState
        { sessionCurTime = toAbsTime (0 :: MilliSecond64)
        , sessionEventTime = toAbsTime (0 :: MilliSecond64)
        , sessionCount = 0
        , sessionTimerHeap = H.empty
        , sessionKeyValueMap = Map.empty
        , sessionOutputStream = K.nil
        }

    -- We can eject sessions based on the current session count to limit
    -- memory consumption. There are two possible strategies:
    --
    -- 1) Eject old sessions or sessions beyond a certain/lower timeout
    -- threshold even before timeout, effectively reduce the timeout.
    -- 2) Drop creation of new sessions but keep accepting new events for the
    -- old ones.
    --
    -- We use the first strategy as of now.

    -- Got a new stream input element
    sstep session@SessionState{..} (Just (key, value, timestamp)) = do
        -- XXX we should use a heap in pinned memory to scale it to a large
        -- size
        --
        -- To detect session inactivity we keep a timestamp of the latest event
        -- in the Map along with the fold result.  When we purge the session
        -- from the heap we match the timestamp in the heap with the timestamp
        -- in the Map, if the latest timestamp is newer and has not expired we
        -- reinsert the key in the heap.
        --
        -- XXX if the key is an Int, we can also use an IntMap for slightly
        -- better performance.
        --
        let curTime = max sessionEventTime timestamp
            mOld = Map.lookup key sessionKeyValueMap

        fs <-
            case mOld of
                Nothing -> initial
                Just (Tuple' _ acc) -> return acc
        res <- step fs value
        case res of
            FL.Done fb -> do
                -- deleting a key from the heap is expensive, so we never
                -- delete a key from heap, we just purge it from the Map and it
                -- gets purged from the heap on timeout. We just need an extra
                -- lookup in the Map when the key is purged from the heap, that
                -- should not be expensive.
                --
                let (mp, cnt) = case mOld of
                        Nothing -> (sessionKeyValueMap, sessionCount)
                        Just _ -> (Map.delete key sessionKeyValueMap
                                  , sessionCount - 1)
                return $ session
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt
                    , sessionKeyValueMap = mp
                    , sessionOutputStream = yield (key, fb)
                    }
            FL.Partial fs1 -> do
                let acc = Tuple' timestamp fs1
                (hp1, mp1, out1, cnt1) <- do
                        let vars = (sessionTimerHeap, sessionKeyValueMap,
                                           K.nil, sessionCount)
                        case mOld of
                            -- inserting new entry
                            Nothing -> do
                                -- Eject a session from heap and map is needed
                                eject <- ejectPred sessionCount
                                (hp, mp, out, cnt) <-
                                    if eject
                                    then ejectOne vars
                                    else return vars

                                -- Insert the new session in heap
                                let expiry = addToAbsTime timestamp timeoutMs
                                    hp' = H.insert (Entry expiry key) hp
                                 in return (hp', mp, out, cnt + 1)
                            -- updating old entry
                            Just _ -> return vars

                let mp2 = Map.insert key acc mp1
                return $ SessionState
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt1
                    , sessionTimerHeap = hp1
                    , sessionKeyValueMap = mp2
                    , sessionOutputStream = out1
                    }

    -- Got a timer tick event
    sstep sessionState@SessionState{..} Nothing =
        let curTime = addToAbsTime sessionCurTime tickMs
        in ejectExpired sessionState curTime

    flush session@SessionState{..} = do
        (hp', mp', out, count) <-
            ejectAll
                ( sessionTimerHeap
                , sessionKeyValueMap
                , K.nil
                , sessionCount
                )
        return $ session
            { sessionCount = count
            , sessionTimerHeap = hp'
            , sessionKeyValueMap = mp'
            , sessionOutputStream = out
            }

    -- delete from map and output the fold accumulator
    ejectEntry hp mp out cnt acc key = do
        sess <- extract acc
        let out1 = (key, sess) `K.cons` out
        let mp1 = Map.delete key mp
        return (hp, mp1, out1, cnt - 1)

    ejectAll (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry _ key, hp1) -> do
                r <- case Map.lookup key mp of
                    Nothing -> return (hp1, mp, out, cnt)
                    Just (Tuple' _ acc) -> ejectEntry hp1 mp out cnt acc key
                ejectAll r
            Nothing -> do
                assert (Map.null mp) (return ())
                return (hp, mp, out, cnt)

    ejectOne (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry expiry key, hp1) ->
                case Map.lookup key mp of
                    Nothing -> ejectOne (hp1, mp, out, cnt)
                    Just (Tuple' latestTS acc) -> do
                        let expiry1 = addToAbsTime latestTS timeoutMs
                        if not reset || expiry1 <= expiry
                        then ejectEntry hp1 mp out cnt acc key
                        else
                            -- reset the session timeout and continue
                            let hp2 = H.insert (Entry expiry1 key) hp1
                            in ejectOne (hp2, mp, out, cnt)
            Nothing -> do
                assert (Map.null mp) (return ())
                return (hp, mp, out, cnt)

    ejectExpired session@SessionState{..} curTime = do
        (hp', mp', out, count) <-
            ejectLoop sessionTimerHeap sessionKeyValueMap K.nil sessionCount
        return $ session
            { sessionCurTime = curTime
            , sessionCount = count
            , sessionTimerHeap = hp'
            , sessionKeyValueMap = mp'
            , sessionOutputStream = out
            }

        where

        ejectLoop hp mp out !cnt = do
            let hres = H.uncons hp
            case hres of
                Just (Entry expiry key, hp1) -> do
                    (eject, force) <-
                        if curTime >= expiry
                        then return (True, False)
                        else do
                            r <- ejectPred cnt
                            return (r, r)
                    if eject
                    then
                        case Map.lookup key mp of
                            Nothing -> ejectLoop hp1 mp out cnt
                            Just (Tuple' latestTS acc) -> do
                                let expiry1 = addToAbsTime latestTS timeoutMs
                                if expiry1 <= curTime || not reset || force
                                then do
                                    (hp2,mp1,out1,cnt1) <-
                                        ejectEntry hp1 mp out cnt acc key
                                    ejectLoop hp2 mp1 out1 cnt1
                                else
                                    -- reset the session timeout and continue
                                    let hp2 = H.insert (Entry expiry1 key) hp1
                                    in ejectLoop hp2 mp out cnt
                    else return (hp, mp, out, cnt)
                Nothing -> do
                    assert (Map.null mp) (return ())
                    return (hp, mp, out, cnt)

    -- merge timer events in the stream
    stream = Serial.map Just str `Par.parallelFst` repeatM timer
    timer = do
        liftIO $ threadDelay (round $ tick * 1000000)
        return Nothing

-- | Like 'classifySessionsOf' but the session is kept alive if an event is
-- received within the session window. The session times out and gets closed
-- only if no event is received within the specified session window size.
--
-- If the ejection predicate returns 'True', the session that was idle for
-- the longest time is ejected before inserting a new session.
--
-- @
-- classifyKeepAliveSessions timeout pred = classifySessionsBy 1 timeout True pred
-- @
--
-- /Internal/
--
{-# INLINABLE classifyKeepAliveSessions #-}
classifyKeepAliveSessions ::
       (IsStream t, MonadAsync m, Ord k)
    => Double -- ^ session inactive timeout
    -> (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Fold m a b -- ^ Fold to be applied to session payload data
    -> t m (k, a, AbsTime) -- ^ session key, data, timestamp
    -> t m (k, b)
classifyKeepAliveSessions tmout =
    classifySessionsBy 1 tmout True

------------------------------------------------------------------------------
-- Keyed tumbling windows
------------------------------------------------------------------------------

-- Tumbling windows is a special case of sliding windows where the window slide
-- is the same as the window size. Or it can be a special case of session
-- windows where the reset flag is set to False.

-- XXX instead of using the early termination flag in the stream, we can use an
-- early terminating fold instead.

{-
-- | Split the stream into fixed size chunks of specified size. Within each
-- such chunk fold the elements in buckets identified by the keys. A particular
-- bucket fold can be terminated early if a closing flag is encountered in an
-- element for that key.
--
-- @since 0.7.0
{-# INLINABLE classifyChunksOf #-}
classifyChunksOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifyChunksOf wsize = classifyChunksBy wsize False
-}

-- | Split the stream into fixed size time windows of specified interval in
-- seconds. Within each such window, fold the elements in sessions identified
-- by the session keys. The fold result is emitted in the output stream if the
-- fold returns a 'Left' result or if the time window ends.
--
-- Session @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input element was generated.  To detect
-- session window end, a monotonic event time clock is maintained synced with
-- the timestamps with a clock resolution of 1 second.
--
-- If the ejection predicate returns 'True', the session with the longest
-- lifetime is ejected before inserting a new session.
--
-- @
-- classifySessionsOf interval pred = classifySessionsBy 1 interval False pred
-- @
--
-- @
-- >>> S.mapM_ print
--   $ S.classifySessionsOf 3 (const (return False)) (fmap Right FL.toList)
--   $ S.map (\(ts,(k,a)) -> (k, a, ts))
--   $ S.timestamped
--   $ S.delay 1
--   $ (,) <$> S.fromList [1,2,3] <*> S.fromList [1,2,3]
-- @
--
-- /Internal/
--
{-# INLINABLE classifySessionsOf #-}
classifySessionsOf ::
       (IsStream t, MonadAsync m, Ord k)
    => Double -- ^ time window size
    -> (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Fold m a b -- ^ Fold to be applied to session events
    -> t m (k, a, AbsTime) -- ^ session key, data, timestamp
    -> t m (k, b)
classifySessionsOf interval =
    classifySessionsBy 1 interval False
