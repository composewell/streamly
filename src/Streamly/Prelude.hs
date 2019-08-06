{-# LANGUAGE CPP                       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE FlexibleContexts          #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

#include "Streams/inline.hs"

-- |
-- Module      : Streamly.Prelude
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module is designed to be imported qualified:
--
-- @
-- import qualified Streamly.Prelude as S
-- @
--
-- Functions with the suffix @M@ are general functions that work on monadic
-- arguments. The corresponding functions without the suffix @M@ work on pure
-- arguments and can in general be derived from their monadic versions but are
-- provided for convenience and for consistency with other pure APIs in the
-- @base@ package.
--
-- In many cases, short definitions of the combinators are provided in the
-- documentation for illustration. The actual implementation may differ for
-- performance reasons.
--
-- Functions having a 'MonadAsync' constraint work concurrently when used with
-- appropriate stream type combinator. Please be careful to not use 'parallely'
-- with infinite streams.
--
-- Deconstruction and folds accept a 'SerialT' type instead of a polymorphic
-- type to ensure that streams always have a concrete monomorphic type by
-- default, reducing type errors. In case you want to use any other type of
-- stream you can use one of the type combinators provided in the "Streamly"
-- module to convert the stream type.

module Streamly.Prelude
    (
    -- * Construction
    -- ** Primitives
    -- | Primitives to construct a stream from pure values or monadic actions.
    -- All other stream construction and generation combinators described later
    -- can be expressed in terms of these primitives. However, the special
    -- versions provided in this module can be much more efficient in most
    -- cases. Users can create custom combinators using these primitives.

      K.nil
    , K.cons
    , (K..:)

    , consM
    , (|:)

    -- ** From Values
    -- | Generate a monadic stream from a seed value or values.
    , yield
    , yieldM
    , K.repeat
    , repeatM
    , replicate
    , replicateM

    -- Note: Using enumeration functions e.g. 'Prelude.enumFromThen' turns out
    -- to be slightly faster than the idioms like @[from, then..]@.
    --
    -- ** Enumeration
    -- | We can use the 'Enum' type class to enumerate a type producing a list
    -- and then convert it to a stream:
    --
    -- @
    -- 'fromList' $ 'Prelude.enumFromThen' from then
    -- @
    --
    -- However, this is not particularly efficient.
    -- The 'Enumerable' type class provides corresponding functions that
    -- generate a stream instead of a list, efficiently.

    , Enumerable (..)
    , enumerate
    , enumerateTo

    -- ** From Generators
    -- | Generate a monadic stream from a seed value and a generator function.
    , unfoldr
    , unfoldrM
    , iterate
    , iterateM
    , fromIndices
    , fromIndicesM

    -- ** From Containers
    -- | Convert an input structure, container or source into a stream. All of
    -- these can be expressed in terms of primitives.
    , P.fromList
    , fromListM
    , K.fromFoldable
    , fromFoldableM

    -- * Elimination

    -- ** Deconstruction
    -- | It is easy to express all the folds in terms of the 'uncons' primitive,
    -- however the specific implementations provided later are generally more
    -- efficient.
    --
    , uncons
    , tail
    , init

    -- ** Folding
-- | In imperative terms a fold can be considered as a loop over the stream
-- that reduces the stream to a single value.
-- Left and right folds use a fold function @f@ and an identity element @z@
-- (@zero@) to recursively deconstruct a structure and then combine and reduce
-- the values or transform and reconstruct a new container.
--
-- In general, a right fold is suitable for transforming and reconstructing a
-- right associated structure (e.g. cons lists and streamly streams) and a left
-- fold is suitable for reducing a right associated structure.  The behavior of
-- right and left folds are described in detail in the individual fold's
-- documentation.  To illustrate the two folds for cons lists:
--
-- > foldr :: (a -> b -> b) -> b -> [a] -> b
-- > foldr f z [] = z
-- > foldr f z (x:xs) = x `f` foldr f z xs
-- >
-- > foldl :: (b -> a -> b) -> b -> [a] -> b
-- > foldl f z [] = z
-- > foldl f z (x:xs) = foldl f (z `f` x) xs
--
-- @foldr@ is conceptually equivalent to:
--
-- > foldr f z [] = z
-- > foldr f z [x] = f x z
-- > foldr f z xs = foldr f (foldr f z (tail xs)) [head xs]
--
-- @foldl@ is conceptually equivalent to:
--
-- > foldl f z [] = z
-- > foldl f z [x] = f z x
-- > foldl f z xs = foldl f (foldl f z (init xs)) [last xs]
--
-- Left and right folds are duals of each other.
--
-- @
-- foldr f z xs = foldl (flip f) z (reverse xs)
-- foldl f z xs = foldr (flip f) z (reverse xs)
-- @
--
-- More generally:
--
-- @
-- foldr f z xs = foldl g id xs z where g k x = k . f x
-- foldl f z xs = foldr g id xs z where g x k = k . flip f x
-- @
--

-- As a general rule, foldr cannot have state and foldl cannot have control.

-- NOTE: Folds are inherently serial as each step needs to use the result of
-- the previous step. However, it is possible to fold parts of the stream in
-- parallel and then combine the results using a monoid.

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
    -- $runningfolds

    , runFold

    -- ** Full Folds
    -- | Folds that are guaranteed to evaluate the whole stream.

    -- -- ** To Summary (Full Folds)
    -- -- | Folds that summarize the stream to a single value.
    , drain
    , last
    , length
    , sum
    , product
    --, mconcat

    -- -- ** To Summary (Maybe) (Full Folds)
    -- -- | Folds that summarize a non-empty stream to a 'Just' value and return
    -- 'Nothing' for an empty stream.
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the
    -- , toListRev -- experimental

    -- ** Lazy Folds
    --
    -- | Folds that generate a lazy structure. Note that the generated
    -- structure may not be lazy if the underlying monad is strict.

    -- -- ** To Containers (Full Folds)
    -- -- | Convert or divert a stream into an output structure, container or
    -- sink.
    , toList

    -- ** Partial Folds
    -- | Folds that may terminate before evaluating the whole stream. These
    -- folds strictly evaluate the stream until the result is determined.

    -- -- ** To Elements (Partial Folds)
    , drainN
    , drainWhile

    -- -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , findM
    , find
    , lookup
    , findIndex
    , elemIndex

    -- -- ** To Boolean (Partial Folds)
    -- -- | Folds that summarize the stream to a boolean value.
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- * Transformation

    --, transform

    -- ** Mapping
    -- | In imperative terms a map operation can be considered as a loop over
    -- the stream that transforms the stream into another stream by performing
    -- an operation on each element of the stream.
    --
    -- 'map' is the least powerful transformation operation with strictest
    -- guarantees.  A map, (1) is a stateless loop which means that no state is
    -- allowed to be carried from one iteration to another, therefore,
    -- operations on different elements are guaranteed to not affect each
    -- other, (2) is a strictly one-to-one transformation of stream elements
    -- which means it guarantees that no elements can be added or removed from
    -- the stream, it can merely transform them.
    , Serial.map
    , sequence
    , mapM
    , mapM_

    -- ** Scanning
    --
    -- | A scan is more powerful than map. While a 'map' is a stateless loop, a
    -- @scan@ is a stateful loop which means that a state can be shared across
    -- all the loop iterations, therefore, future iterations can be impacted by
    -- the state changes made by the past iterations. A scan yields the state
    -- of the loop after each iteration. Like a map, a @postscan@ or @prescan@
    -- does not add or remove elements in the stream, it just transforms them.
    -- However, a @scan@ adds one extra element to the stream.
    --
    -- A left associative scan, also known as a prefix sum, can be thought of
    -- as a stream transformation consisting of left folds of all prefixes of a
    -- stream.  Another way of thinking about it is that it streams all the
    -- intermediate values of the accumulator while applying a left fold on the
    -- input stream.  A right associative scan, on the other hand, can be
    -- thought of as a stream consisting of right folds of all the suffixes of
    -- a stream.
    --
    -- The following equations hold for lists:
    --
    -- > scanl f z xs == map (foldl f z) $ inits xs
    -- > scanr f z xs == map (foldr f z) $ tails xs
    --
    -- @
    -- > scanl (+) 0 [1,2,3,4]
    -- 0                 = 0
    -- 0 + 1             = 1
    -- 0 + 1 + 2         = 3
    -- 0 + 1 + 2 + 3     = 6
    -- 0 + 1 + 2 + 3 + 4 = 10
    --
    -- > scanr (+) 0 [1,2,3,4]
    -- 1 + 2 + 3 + 4 + 0 = 10
    --     2 + 3 + 4 + 0 = 9
    --         3 + 4 + 0 = 7
    --             4 + 0 = 4
    --                 0 = 0
    -- @
    --
    -- Left and right scans are duals:
    --
    -- > scanr f z xs ==  reverse $ scanl (flip f) z (reverse xs)
    -- > scanl f z xs ==  reverse $ scanr (flip f) z (reverse xs)
    --
    -- A scan is a stateful map i.e. a combination of map and fold:
    --
    -- > map f xs =           tail $ scanl (\_ x -> f x) z xs
    -- > map f xs = reverse $ head $ scanr (\_ x -> f x) z xs
    --
    -- > foldl f z xs = last $ scanl f z xs
    -- > foldr f z xs = head $ scanr f z xs

    -- ** Left scans
    , scanl'
    , scanlM'
    , postscanl'
    , postscanlM'
    -- , prescanl'
    -- , prescanlM'
    , scanl1'
    , scanl1M'

    -- ** Scan Using Fold
    , runScan
    , runPostscan

    -- , lscanl'
    -- , lscanlM'
    -- , lscanl1'
    -- , lscanl1M'
    --
    -- , lpostscanl'
    -- , lpostscanlM'
    -- , lprescanl'
    -- , lprescanlM'

    -- ** Indexing
    , indexed
    , indexedR
    -- , timestamped
    -- , timestampedR -- timer

    -- ** Filtering
    -- | Remove some elements from the stream based on a predicate. In
    -- imperative terms a filter over a stream corresponds to a loop with a
    -- @continue@ clause for the cases when the predicate fails.

    , filter
    , filterM

    -- ** Stateful Filters
    , take
    -- , takeEnd
    , takeWhile
    , takeWhileM
    -- , takeWhileEnd
    , drop
    -- , dropEnd
    , dropWhile
    , dropWhileM
    -- , dropWhileEnd
    -- , dropAround
    , deleteBy
    , uniq
    -- , uniqBy -- by predicate e.g. to remove duplicate "/" in a path
    -- , uniqOn -- to remove duplicate sequences
    -- , pruneBy -- dropAround + uniqBy - like words

    -- ** Mapping Filters
    -- | Mapping along with filtering

    , mapMaybe
    , mapMaybeM

    -- ** Scanning Filters
    -- | Stateful transformation along with filtering

    , findIndices
    , elemIndices
    -- , seqIndices -- search a sequence in the stream

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , insertBy
    , intersperseM
    , intersperse
    -- , insertAfterEach
    -- , intersperseBySpan
    -- , intersperseByTime

    -- ** Reordering
    , reverse
    -- , reverse'

    -- * Multi-Stream Operations
    -- | New streams can be constructed by appending, merging or zipping
    -- existing streams.

    -- ** Appending
    -- | Streams form a 'Semigroup' and a 'Monoid' under the append
    -- operation.
    --
    -- @
    --
    -- -------Stream m a------|-------Stream m a------|=>----Stream m a---
    --
    -- @
    --
    -- @
    -- >> S.toList $ S.fromList [1,2] \<> S.fromList [3,4]
    -- [1,2,3,4]
    -- >> S.toList $ fold $ [S.fromList [1,2], S.fromList [3,4]]
    -- [1,2,3,4]
    -- @

    -- ** Merging
    -- | Streams form a commutative semigroup under the merge
    -- operation.
    --
    -- @
    --
    -- -------Stream m a------|
    --                        |=>----Stream m a---
    -- -------Stream m a------|
    -- @
    --

    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- ** Zipping
    -- |
    -- @
    --
    -- -------Stream m a------|
    --                        |=>----Stream m c---
    -- -------Stream m b------|
    -- @
    --
    , zipWith
    , zipWithM
    , Z.zipAsyncWith
    , Z.zipAsyncWithM

    -- ** Nested Streams
    -- | Stream operations represent loops in imperative programming, streams
    -- of streams represent nested loops.

    , concatMapM
    , concatMap
    , K.concatMapBy
    -- , interposeBy
    -- , intercalate

    -- -- ** Breaking

    -- By chunks
    -- , splitAt -- spanN
    -- , splitIn -- sessionN

    -- By elements
    -- , span  -- spanWhile
    -- , break -- breakBefore
    -- , breakAfter
    -- , breakOn
    -- , breakAround
    -- , spanBy
    -- , spanByRolling

    -- By sequences
    -- breakOnSeq

    -- ** Splitting
    -- | Streams can be split into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    -- , groupScan

    -- -- *** Chunks
    , chunksOf
    , sessionsOf

    -- , lchunksOf
    -- , lsessionsOf

    -- -- *** Using Element Separators
    -- On == Dropping the separator
    , splitOn
    , splitOnSuffix
    -- , splitOnPrefix

    -- By == Keeping the separator
    -- , splitBy
    , splitBySuffix
    -- , splitByPrefix
    , wordsBy -- stripAndCompactBy

    -- -- *** Using Sequence Separators
    -- , splitOnSeq
    -- , splitOnSuffixSeq
    -- , splitOnPrefixSeq

    -- Keeping the delimiters
    -- , splitBySeq
    -- , splitBySeqSuffix
    -- , splitBySeqPrefix
    -- , wordsBySeq

    -- Splitting using multiple sequence separators
    -- , splitOnAnySeq
    -- , splitOnAnySuffixSeq
    -- , splitOnAnyPrefixSeq

    -- ** Grouping
    , groups
    , groupsBy
    , groupsByRolling

    -- ** Distributing
    , trace
    , tap
    , Par.tee

    {-
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
    -- , classifySessionsOf

    -- ** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.
    -- , classifyKeepAliveChunks
    -- , classifyKeepAliveSessions

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
    -}

    -- ** Containers of Streams
    -- | These are variants of standard 'Foldable' fold functions that use a
    -- polymorphic stream sum operation (e.g. 'async' or 'wSerial') to fold a
    -- finite container of streams.
    --
    , foldWith
    , foldMapWith
    , forEachWith

    -- ** Folding
    , eqBy
    , cmpBy
    , isPrefixOf
    -- , isSuffixOf
    -- , isInfixOf
    , isSubsequenceOf
    , stripPrefix
    -- , stripSuffix
    -- , stripInfix

    -- * Exceptions
    , before
    , after
    , bracket
    , onException
    , finally
    , handle

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
import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Functor.Identity (Identity)
import Data.Heap (Entry(..))
import Data.Maybe (isJust, fromJust, isNothing)
import Foreign.Storable (Storable)
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap, span)

import qualified Data.Heap as H
import qualified Data.Map.Strict as Map
import qualified Prelude
import qualified System.IO as IO

import Streamly.Enumeration (Enumerable(..), enumerate, enumerateTo)
import Streamly.Fold.Types (Fold (..))
import Streamly.Memory.Array (Array)
import Streamly.SVar (MonadAsync, defState)
import Streamly.Streams.Async (mkAsync')
import Streamly.Streams.Combinators (maxYields)
import Streamly.Streams.Prelude
       (fromStreamS, toStreamS, foldWith, foldMapWith, forEachWith)
import Streamly.Streams.StreamD (fromStreamD, toStreamD)
import Streamly.Streams.StreamK (IsStream(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Pipe.Types (Pipe (..))
import Streamly.Time.Units
       (AbsTime, MilliSecond64(..), addToAbsTime, diffAbsTime, toRelTime,
       toAbsTime)

import Streamly.Strict

import qualified Streamly.Memory.Array as A
import qualified Streamly.Fold as FL
import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Streams.Zip as Z

#ifdef USE_STREAMK_ONLY
import qualified Streamly.Streams.StreamK as S
import qualified Streamly.Streams.Zip as S
#else
import qualified Streamly.Streams.StreamD as S
#endif

import qualified Streamly.Streams.Serial as Serial
import qualified Streamly.Streams.Parallel as Par

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- This is a brute force primitive. Avoid using it as long as possible, use it
-- when no other combinator can do the job. This can be used to do pretty much
-- anything in an imperative manner, as it just breaks down the stream into
-- individual elements and we can loop over them as we deem fit. For example,
-- this can be used to convert a streamly stream into other stream types.
--
-- @since 0.1.0
{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (a, t m a))
uncons m = K.uncons (K.adapt m)

------------------------------------------------------------------------------
-- Generation by Unfolding
------------------------------------------------------------------------------

-- |
-- @
-- unfoldr step s =
--     case step s of
--         Nothing -> 'K.nil'
--         Just (a, b) -> a \`cons` unfoldr step b
-- @
--
-- Build a stream by unfolding a /pure/ step function @step@ starting from a
-- seed @s@.  The step function returns the next element in the stream and the
-- next seed value. When it is done it returns 'Nothing' and the stream ends.
-- For example,
--
-- @
-- let f b =
--         if b > 3
--         then Nothing
--         else Just (b, b + 1)
-- in toList $ unfoldr f 0
-- @
-- @
-- [0,1,2,3]
-- @
--
-- @since 0.1.0
{-# INLINE_EARLY unfoldr #-}
unfoldr :: (Monad m, IsStream t) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step seed = fromStreamS (S.unfoldr step seed)
{-# RULES "unfoldr fallback to StreamK" [1]
    forall a b. S.toStreamK (S.unfoldr a b) = K.unfoldr a b #-}

-- | Build a stream by unfolding a /monadic/ step function starting from a
-- seed.  The step function returns the next element in the stream and the next
-- seed value. When it is done it returns 'Nothing' and the stream ends. For
-- example,
--
-- @
-- let f b =
--         if b > 3
--         then return Nothing
--         else print b >> return (Just (b, b + 1))
-- in drain $ unfoldrM f 0
-- @
-- @
--  0
--  1
--  2
--  3
-- @
-- When run concurrently, the next unfold step can run concurrently with the
-- processing of the output of the previous step.  Note that more than one step
-- cannot run concurrently as the next step depends on the output of the
-- previous step.
--
-- @
-- (asyncly $ S.unfoldrM (\\n -> liftIO (threadDelay 1000000) >> return (Just (n, n + 1))) 0)
--     & S.foldlM' (\\_ a -> threadDelay 1000000 >> print a) ()
-- @
--
-- /Concurrent/
--
-- /Since: 0.1.0/
{-# INLINE_EARLY unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE_EARLY unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> SerialT m a
unfoldrMSerial step seed = fromStreamS (S.unfoldrM step seed)

------------------------------------------------------------------------------
-- Specialized Generation
------------------------------------------------------------------------------

-- Faster than yieldM because there is no bind.
--
-- |
-- @
-- yield a = a \`cons` nil
-- @
--
-- Create a singleton stream from a pure value.
--
-- The following holds in monadic streams, but not in Zip streams:
--
-- @
-- yield = pure
-- yield = yieldM . pure
-- @
--
-- In Zip applicative streams 'yield' is not the same as 'pure' because in that
-- case 'pure' is equivalent to 'repeat' instead. 'yield' and 'pure' are
-- equally efficient, in other cases 'yield' may be slightly more efficient
-- than the other equivalent definitions.
--
-- @since 0.4.0
{-# INLINE yield #-}
yield :: IsStream t => a -> t m a
yield = K.yield

-- |
-- @
-- yieldM m = m \`consM` nil
-- @
--
-- Create a singleton stream from a monadic action.
--
-- @
-- > toList $ yieldM getLine
-- hello
-- ["hello"]
-- @
--
-- @since 0.4.0
{-# INLINE yieldM #-}
yieldM :: (Monad m, IsStream t) => m a -> t m a
yieldM = K.yieldM

-- |
-- @
-- fromIndices f = let g i = f i \`cons` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a function @f@
-- applied on the corresponding index.  Index starts at 0.
--
-- @
-- > S.toList $ S.take 5 $ S.fromIndices id
-- [0,1,2,3,4]
-- @
--
-- @since 0.6.0
{-# INLINE fromIndices #-}
fromIndices :: (IsStream t, Monad m) => (Int -> a) -> t m a
fromIndices = fromStreamS . S.fromIndices

--
-- |
-- @
-- fromIndicesM f = let g i = f i \`consM` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a monadic
-- function @f@ applied on the corresponding index. Index starts at 0.
--
-- /Concurrent/
--
-- @since 0.6.0
{-# INLINE_EARLY fromIndicesM #-}
fromIndicesM :: (IsStream t, MonadAsync m) => (Int -> m a) -> t m a
fromIndicesM = K.fromIndicesM

{-# RULES "fromIndicesM serial" fromIndicesM = fromIndicesMSerial #-}
{-# INLINE fromIndicesMSerial #-}
fromIndicesMSerial :: MonadAsync m => (Int -> m a) -> SerialT m a
fromIndicesMSerial = fromStreamS . S.fromIndicesM

-- |
-- @
-- replicateM = take n . repeatM
-- @
--
-- Generate a stream by performing a monadic action @n@ times. Same as:
--
-- @
-- drain $ serially $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent/
--
-- @since 0.1.1
{-# INLINE_EARLY replicateM #-}
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM = K.replicateM

{-# RULES "replicateM serial" replicateM = replicateMSerial #-}
{-# INLINE replicateMSerial #-}
replicateMSerial :: MonadAsync m => Int -> m a -> SerialT m a
replicateMSerial n = fromStreamS . S.replicateM n

-- |
-- @
-- replicate = take n . repeat
-- @
--
-- Generate a stream of length @n@ by repeating a value @n@ times.
--
-- @since 0.6.0
{-# INLINE_NORMAL replicate #-}
replicate :: (IsStream t, Monad m) => Int -> a -> t m a
replicate n = fromStreamS . S.replicate n

-- |
-- @
-- repeatM = fix . consM
-- repeatM = cycle1 . yieldM
-- @
--
-- Generate a stream by repeatedly executing a monadic action forever.
--
-- @
-- drain $ serially $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent, infinite (do not use with 'parallely')/
--
-- @since 0.2.0
repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
repeatM = go
    where go m = m |: go m

-- |
-- @
-- iterate f x = x \`cons` iterate f x
-- @
--
-- Generate an infinite stream with @x@ as the first element and each
-- successive element derived by applying the function @f@ on the previous
-- element.
--
-- @
-- > S.toList $ S.take 5 $ S.iterate (+1) 1
-- [1,2,3,4,5]
-- @
--
-- @since 0.1.2
iterate :: IsStream t => (a -> a) -> a -> t m a
iterate step = fromStream . go
    where
    go s = K.cons s (go (step s))

-- |
-- @
-- iterateM f m = m >>= \a -> return a \`consM` iterateM f (f a)
-- @
--
-- Generate an infinite stream with the first element generated by the action
-- @m@ and each successive element derived by applying the monadic function
-- @f@ on the previous element.
--
-- When run concurrently, the next iteration can run concurrently with the
-- processing of the previous iteration. Note that more than one iteration
-- cannot run concurrently as the next iteration depends on the output of the
-- previous iteration.
--
-- @
-- drain $ serially $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
--
-- drain $ asyncly  $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) (return 0)
-- @
--
-- /Concurrent/
--
-- /Since: 0.7.0 (signature change)/
--
-- /Since: 0.1.2/
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> m a -> t m a
iterateM step = go
    where
    go s = K.mkStream $ \st stp sng yld -> do
        next <- s
        K.foldStreamShared st stp sng yld (return next |: go (step next))

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromListM = 'Prelude.foldr' 'K.consM' 'K.nil'
-- @
--
-- Construct a stream from a list of monadic actions. This is more efficient
-- than 'fromFoldableM' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY fromListM #-}
fromListM :: (MonadAsync m, IsStream t) => [m a] -> t m a
fromListM = fromStreamD . D.fromListM
{-# RULES "fromListM fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromListM a) = fromFoldableM a #-}

-- |
-- @
-- fromFoldableM = 'Prelude.foldr' 'consM' 'K.nil'
-- @
--
-- Construct a stream from a 'Foldable' containing monadic actions.
--
-- @
-- drain $ serially $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- drain $ asyncly  $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- @
--
-- /Concurrent (do not use with 'parallely' on infinite containers)/
--
-- @since 0.3.0
{-# INLINE fromFoldableM #-}
fromFoldableM :: (IsStream t, MonadAsync m, Foldable f) => f (m a) -> t m a
fromFoldableM = Prelude.foldr consM K.nil

-- | Same as 'fromFoldable'.
--
-- @since 0.1.0
{-# DEPRECATED each "Please use fromFoldable instead." #-}
{-# INLINE each #-}
each :: (IsStream t, Foldable f) => f a -> t m a
each = K.fromFoldable

-- | Read lines from an IO Handle into a stream of Strings.
--
-- @since 0.1.0
{-# DEPRECATED fromHandle
   "Please use Streamly.FileSystem.Handle module (see the changelog)" #-}
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
fromHandle h = go
  where
  go = K.mkStream $ \_ yld _ stp -> do
        eof <- liftIO $ IO.hIsEOF h
        if eof
        then stp
        else do
            str <- liftIO $ IO.hGetLine h
            yld str go

------------------------------------------------------------------------------
-- Elimination by Folding
------------------------------------------------------------------------------

-- | Right associative/pull fold.
--
-- Example, determine if any element is 'odd' in a stream:
--
-- >>> S.foldrM (\x xs -> if odd x then return True else xs) (return False) $ S.fromList (2:4:5:undefined)
-- > True
--
-- Let's take a closer look at the @foldr@ definition for lists, as given
-- earlier:
--
-- @
-- foldr f z (x:xs) = x \`f` foldr f z xs
-- @
--
-- @foldr@ invokes the fold step function @f@ as @f x (foldr f z xs)@. At each
-- invocation of @f@ @foldr@ gives us the next element in the input container
-- @x@ and a recursive expression @foldr f z xs@ representing the yet unbuilt
-- (lazy thunk) part of the output. Therefore, when @f x xs@ is lazy in @xs@
-- it can consume the input one element at a time in FIFO order to build a lazy
-- output expression. For example,
--
-- > f x remaining = show x : remaining
--
-- @take 2 $ foldr f [] (1:2:undefined)@ would consume the input lazily on
-- demand, consuming only first two elements and resulting in ["1", "2"]. @f@
-- can terminate recursion by not evaluating the @remaining@ part:
--
-- > f 2 remaining = show 2 : []
-- > f x remaining = show x : remaining
--
-- @f@ would terminate recursion whenever it sees element @2@ in the input.
-- Therefore, @foldr f [] (1:2:undefined)@ would work as before.  If @f a b@ is
-- strict in @b@ it would end up consuming the whole input right away and
-- expanding the recursive expression @b@ (i.e.  @foldr f z xs@) fully before
-- it yields an output expression, resulting in the following /right
-- associated expression/:
--
-- @
-- foldr f z xs == x1 \`f` (x2 \`f` ...(xn \`f` z))
-- @
--
-- For example,
--
-- > f x remaining = x + remaining
--
-- With this definition, @foldr f 0 [1..1000]@, would recurse completely until
-- it reaches the recursion terminating case @... `f` (1000 `f` 0)@, and then
-- start reducing the whole expression from right to left, therefore, consuming
-- the input elements in LIFO order. Thus, such an evaluation would require
-- memory proportional to the size of input. Try out @foldr (+) 0 (map (\\x ->
-- trace (show x) x) [1..10])@.
--
-- Notice, the order of the arguments to the step function @f a b@. It follows
-- the order of @a@ and @b@ in the right associative recursive expression
-- generated by expanding @a \`f` b@.
--
-- A right fold is a pull fold, the step function is the puller, it can pull
-- more data from the input container by using its second argument in the
-- output expression or terminate pulling by not using it. As a corollary:
--
-- 1. a step function which is lazy in its second argument (usually functions
-- or constructors that build a lazy structure e.g. @(:)@) can pull lazily on
-- demand.
-- 2. a step function strict in its second argument (usually reducers e.g.
-- (+)) would end up pulling all of its input and buffer it in memory before
-- potentially reducing it.
--
-- A right fold is suitable for lazy reconstructions e.g.  transformation,
-- mapping, filtering of /right associated input structures/ (e.g. cons lists).
-- Whereas a left fold is suitable for reductions (e.g. summing a stream of
-- numbers) of right associated structures. Note that these roles will reverse
-- for left associated structures (e.g. snoc lists). Most of our observations
-- here assume right associated structures, lists being the canonical example.
--
-- 1. A lazy FIFO style pull using a right fold allows pulling a potentially
-- /infinite/ input stream lazily, perform transformations on it, and
-- reconstruct a new structure without having to buffer the whole structure. In
-- contrast, a left fold would buffer the entire structure before the
-- reconstructed structure can be consumed.
-- 2. Even if buffering the entire input structure is ok, we need to keep in
-- mind that a right fold reconstructs structures in a FIFO style, whereas a
-- left fold reconstructs in a LIFO style, thereby reversing the order of
-- elements..
-- 3. A right fold has termination control and therefore can terminate early
-- without going throught the entire input, a left fold cannot terminate
-- without consuming all of its input.  For example, a right fold
-- implementation of 'or' can terminate as soon as it finds the first 'True'
-- element, whereas a left fold would necessarily go through the entire input
-- irrespective of that.
-- 4. Reduction (e.g. using (+) on a stream of numbers) using a right fold
-- occurs in a LIFO style, which means that the entire input gets buffered
-- before reduction starts. Whereas with a strict left fold reductions occur
-- incrementally in FIFO style. Therefore, a strict left fold is more suitable
-- for reductions.
--
-- /Since: 0.7.0 (signature changed)/
--
-- /Since: 0.2.0 (signature changed)/
--
-- /Since: 0.1.0/
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> SerialT m a -> m b
foldrM = P.foldrM

-- | Right fold to a streaming monad.
--
-- > foldrS S.cons S.nil === id
--
-- 'foldrS' can be used to perform stateless stream to stream transformations
-- like map and filter in general. It can be coupled with a scan to perform
-- stateful transformations. However, note that the custom map and filter
-- routines can be much more efficient than this due to better stream fusion.
--
-- >>> S.toList $ S.foldrS S.cons S.nil $ S.fromList [1..5]
-- > [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> S.toList $ S.foldrS (\x xs -> if odd x then return True else xs) (return False) $ (S.fromList (2:4:5:undefined) :: SerialT IO Int)
-- > [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> S.toList $ S.foldrS (\x xs -> if odd x then (x + 2) `S.cons` xs else xs) S.nil $ (S.fromList [1..5] :: SerialT IO Int)
-- > [3,5,7]
--
-- 'foldrM' can also be represented in terms of 'foldrS', however, the former
-- is much more efficient:
--
-- > foldrM f z s = runIdentityT $ foldrS (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
--
-- @since 0.7.0
{-# INLINE foldrS #-}
foldrS :: IsStream t => (a -> t m b -> t m b) -> t m b -> t m a -> t m b
foldrS = K.foldrS

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
-- @since 0.7.0
{-# INLINE foldrT #-}
foldrT :: (IsStream t, Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> t m a -> s m b
foldrT f z s = S.foldrT f z (toStreamS s)

-- | Right fold for lazy monads or pure streams.
--
-- Please avoid using this routine in strict monads like IO unless you need a
-- strict right fold. This is provided only for use in lazy monads (e.g.
-- Identity) or pure streams. Note that with this signature it is not possible
-- to implement a lazy foldr when the monad @m@ is strict. In that case it
-- would be strict in its accumulator and therefore would necessarily consume
-- all its input.
--
-- @since 0.1.0
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> SerialT m a -> m b
foldr = P.foldr

-- XXX This seems to be of limited use as it cannot be used to construct
-- recursive structures and for reduction foldl1' is better.
--
-- | Lazy right fold for non-empty streams, using first element as the starting
-- value. Returns 'Nothing' if the stream is empty.
--
-- @since 0.5.0
{-# INLINE foldr1 #-}
{-# DEPRECATED foldr1 "Use foldrM instead." #-}
foldr1 :: Monad m => (a -> a -> a) -> SerialT m a -> m (Maybe a)
foldr1 f m = S.foldr1 f (toStreamS m)

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.2.0
{-# DEPRECATED foldx "Please use foldl' followed by fmap instead." #-}
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldx = P.foldlx'

-- | Strict left associative/push fold.  Note that the observations below about
-- the behavior of a left fold assume that we are working on a right associated
-- structure like cons lists and streamly streams. If we are working on a left
-- associated structure (e.g. snoc lists) the roles of right and left folds
-- would reverse.
--
-- Let's take a closer look at the @foldl@ definition for lists given above:
--
-- @
-- foldl f z (x:xs) = foldl f (z \`f` x) xs
-- @
--
-- @foldl@ calls itself recursively, in each call it invokes @f@ as @f z x@
-- providing it with the result accumulated till now @z@ (the state) and the
-- next element from the input container. First call to @f@ is supplied with
-- the initial value of the accumulator @z@ and each subsequent call uses the
-- output of the previous call to @f z x@.
--
-- >> foldl' (+) 0 [1,2,3]
-- > 6
--
-- The recursive call at the head of the output expression is bound to be
-- evaluated until recursion terminates, therefore, a left fold always
-- /consumes the whole input container/. The following would result in an
-- error, even though the fold is not using the values at all:
--
-- >> foldl' (\_ _ -> 0) 0 (1:undefined)
-- > *** Exception: Prelude.undefined
--
-- As @foldl@ recurses, it builds the left associated expression shown below.
-- Notice, the order of the arguments to the step function @f b a@. It follows
-- the left associative recursive expression generated by expanding @b \`f` a@.
--
-- @
-- foldl f z xs == (((z \`f` x1) \`f` x2) ...) \`f` xn
-- @
--
--
-- The strict left fold @foldl'@ forces the reduction of its argument @z \`f`
-- x@ before using it, therefore it never builds the whole expression in
-- memory.  Thus, @z \`f` x1@ would get reduced to @z1@ and then @z1 \`f` x2@
-- would get reduced to @z2@ and so on, incrementally reducing the expression
-- from left to right as it recurses, consuming the input in FIFO order.  Try
-- out @foldl' (+) 0 (map (\\x -> trace (show x) x) [1..10])@ to see how it
-- works. For example:
--
-- @
-- > S.foldl' (+) 0 $ S.fromList [1,2,3,4]
-- 10
-- @
--
-- @
-- 0 + 1 = 1
-- 1 + 2 = 3
-- 3 + 3 = 6
-- 6 + 4 = 10
-- @
--
-- However, @foldl'@ evaluates the accumulator only to WHNF. It may further
-- help if the step function uses a strict data structure as accumulator to
-- improve performance and to keep the expression fully reduced at all times
-- during the fold.
--
-- A left fold can also build a new structure instead of reducing one if a
-- constructor is used as a fold step. However, it may not be very useful
-- because it will consume the whole input and construct the new structure in
-- memory before we can consume it. Thus the whole structure gets buffered in
-- memory. When the list constructor is used it would build a new list in
-- reverse (LIFO) order:
--
-- @
-- > S.foldl' (flip (:)) [] $ S.fromList [1,2,3,4]
-- [4,3,2,1]
-- @
--
-- A left fold is a push fold. The producer pushes its contents to the step
-- function of the fold. The step function therefore has no control to stop the
-- input, it can only discard it if it does not need it. We can also consider a
-- left fold as a state machine where the state is store in the accumulator,
-- the state can be modified based on new inputs that are pushed to the fold.
--
-- In general, a strict left fold is a reducing fold, whereas a right fold is a
-- constructing fold. A strict left fold reduces in a FIFO order whereas it
-- constructs in a LIFO order, and vice-versa for the right fold. See the
-- documentation of 'foldrM' for a discussion on where a left or right fold is
-- suitable.
--
-- To perform a left fold lazily without having to consume all the input one
-- can use @scanl@ to stream the intermediate results of the fold and consume
-- the resulting stream lazily.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> SerialT m a -> m b
foldl' = P.foldl'

-- | Strict left fold, for non-empty streams, using first element as the
-- starting value. Returns 'Nothing' if the stream is empty.
--
-- @since 0.5.0
{-# INLINE foldl1' #-}
foldl1' :: Monad m => (a -> a -> a) -> SerialT m a -> m (Maybe a)
foldl1' step m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> do
            res <- foldl' step h t
            return $ Just res

-- | Like 'foldx', but with a monadic step function.
--
-- @since 0.2.0
{-# DEPRECATED foldxM "Please use foldlM' followed by fmap instead." #-}
{-# INLINE foldxM #-}
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldxM = P.foldlMx'

-- | Like 'foldl'' but with a monadic step function.
--
-- @since 0.2.0
{-# INLINE foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> b -> SerialT m a -> m b
foldlM' step begin m = S.foldlM' step begin $ toStreamS m

------------------------------------------------------------------------------
-- Running a Fold
------------------------------------------------------------------------------

-- $runningfolds
--
-- "Streamly.Fold" module defines composable left folds which can be combined
-- together in many interesting ways. Those folds can be run using 'runFold'.
-- The following two ways of folding are equivalent in functionality and
-- performance,
--
-- >>> S.runFold FL.sum (S.enumerateFromTo 1 100)
-- 5050
-- >>> S.sum (S.enumerateFromTo 1 100)
-- 5050
--
-- However, left folds cannot terminate early even if it does not need to
-- consume more input to determine the result.  Therefore, the performance is
-- equivalent only for full folds like 'sum' and 'length'. For partial folds
-- like 'head' or 'any' the the folds defined in this module may be much more
-- efficient because they are implemented as right folds that terminate as soon
-- as we get the result. Note that when a full fold is composed with a partial
-- fold in parallel the performance is not impacted as we anyway have to
-- consume the whole stream due to the full fold.
--
-- >>> S.head (1 `S.cons` undefined)
-- Just 1
-- >>> S.runFold FL.head (1 `S.cons` undefined)
-- *** Exception: Prelude.undefined
--
-- However, we can wrap the fold in a scan to convert it into a lazy stream of
-- fold steps. We can then terminate the stream whenever we want.  For example,
--
-- >>> S.toList $ S.take 1 $ S.runScan FL.head (1 `S.cons` undefined)
-- [Nothing]
--
-- The following example extracts the input stream up to a point where the
-- running average of elements is no more than 10:
--
-- @
-- > S.toList
--   $ S.map (fromJust . fst)
--   $ S.takeWhile (\\(_,x) -> x <= 10)
--   $ S.runPostscan ((,) \<$> FL.last \<*> avg) (S.enumerateFromTo 1.0 100.0)
-- @
-- @
--  [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0]
-- @

-- | Fold a stream using the supplied left fold.
--
-- >>> S.runFold FL.sum (S.enumerateFromTo 1 100)
-- 5050
--
-- @since 0.7.0
{-# INLINE runFold #-}
runFold :: Monad m => Fold m a b -> SerialT m a -> m b
runFold = P.runFold

------------------------------------------------------------------------------
-- Running a sink
------------------------------------------------------------------------------

{-
-- | Drain a stream to a 'Sink'.
{-# INLINE runSink #-}
runSink :: Monad m => Sink m a -> SerialT m a -> m ()
runSink = runFold . toFold
-}

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

-- |
-- > drain = mapM_ (\_ -> return ())
--
-- Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @drain . 'asyncly'@.
--
-- @since 0.7.0
{-# INLINE drain #-}
drain :: Monad m => SerialT m a -> m ()
drain = P.drain

-- | Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @runStream . 'asyncly'@.
--
-- @since 0.2.0
{-# DEPRECATED runStream "Please use \"drain\" instead" #-}
{-# INLINE runStream #-}
runStream :: Monad m => SerialT m a -> m ()
runStream = drain

-- |
-- > drainN n = drain . take n
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

-- | Determine whether the stream is empty.
--
-- @since 0.1.1
{-# INLINE null #-}
null :: Monad m => SerialT m a -> m Bool
null = K.null

-- | Extract the first element of the stream, if any.
--
-- > head = (!! 0)
--
-- @since 0.1.0
{-# INLINE head #-}
head :: Monad m => SerialT m a -> m (Maybe a)
head = K.head

-- |
-- > tail = fmap (fmap snd) . uncons
--
-- Extract all but the first element of the stream, if any.
--
-- @since 0.1.1
{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
tail m = K.tail (K.adapt m)

-- | Extract all but the last element of the stream, if any.
--
-- @since 0.5.0
{-# INLINE init #-}
init :: (IsStream t, Monad m) => SerialT m a -> m (Maybe (t m a))
init m = K.init (K.adapt m)

-- | Extract the last element of the stream, if any.
--
-- > last xs = xs !! (length xs - 1)
--
-- @since 0.1.1
{-# INLINE last #-}
last :: Monad m => SerialT m a -> m (Maybe a)
last m = S.last $ toStreamS m

-- | Determine whether an element is present in the stream.
--
-- @since 0.1.0
{-# INLINE elem #-}
elem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
elem e m = S.elem e (toStreamS m)

-- | Determine whether an element is not present in the stream.
--
-- @since 0.1.0
{-# INLINE notElem #-}
notElem :: (Monad m, Eq a) => a -> SerialT m a -> m Bool
notElem e m = S.notElem e (toStreamS m)

-- | Determine the length of the stream.
--
-- @since 0.1.0
{-# INLINE length #-}
length :: Monad m => SerialT m a -> m Int
length = foldl' (\n _ -> n + 1) 0

-- | Determine whether all elements of a stream satisfy a predicate.
--
-- @since 0.1.0
{-# INLINE all #-}
all :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
all p m = S.all p (toStreamS m)

-- | Determine whether any of the elements of a stream satisfy a predicate.
--
-- @since 0.1.0
{-# INLINE any #-}
any :: Monad m => (a -> Bool) -> SerialT m a -> m Bool
any p m = S.any p (toStreamS m)

-- | Determines if all elements of a boolean stream are True.
--
-- @since 0.5.0
{-# INLINE and #-}
and :: Monad m => SerialT m Bool -> m Bool
and = all (==True)

-- | Determines whether at least one element of a boolean stream is True.
--
-- @since 0.5.0
{-# INLINE or #-}
or :: Monad m => SerialT m Bool -> m Bool
or = any (==True)

-- | Determine the sum of all elements of a stream of numbers. Returns @0@ when
-- the stream is empty. Note that this is not numerically stable for floating
-- point numbers.
--
-- @since 0.1.0
{-# INLINE sum #-}
sum :: (Monad m, Num a) => SerialT m a -> m a
sum = foldl' (+) 0

-- | Determine the product of all elements of a stream of numbers. Returns @1@
-- when the stream is empty.
--
-- @since 0.1.1
{-# INLINE product #-}
product :: (Monad m, Num a) => SerialT m a -> m a
product = foldl' (*) 1

-- |
-- @
-- minimum = 'minimumBy' compare
-- @
--
-- Determine the minimum element in a stream.
--
-- @since 0.1.0
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
minimum m = S.minimum (toStreamS m)

-- | Determine the minimum element in a stream using the supplied comparison
-- function.
--
-- @since 0.6.0
{-# INLINE minimumBy #-}
minimumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
minimumBy cmp m = S.minimumBy cmp (toStreamS m)

-- |
-- @
-- maximum = 'maximumBy' compare
-- @
--
-- Determine the maximum element in a stream.
--
-- @since 0.1.0
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => SerialT m a -> m (Maybe a)
maximum m = S.maximum (toStreamS m)

-- | Determine the maximum element in a stream using the supplied comparison
-- function.
--
-- @since 0.6.0
{-# INLINE maximumBy #-}
maximumBy :: Monad m => (a -> a -> Ordering) -> SerialT m a -> m (Maybe a)
maximumBy cmp m = S.maximumBy cmp (toStreamS m)

-- | Lookup the element at the given index.
--
-- @since 0.6.0
{-# INLINE (!!) #-}
(!!) :: Monad m => SerialT m a -> Int -> m (Maybe a)
m !! i = toStreamS m S.!! i

-- | In a stream of (key-value) pairs @(a, b)@, return the value @b@ of the
-- first pair where the key equals the given value @a@.
--
-- > lookup = snd <$> find ((==) . fst)
--
-- @since 0.5.0
{-# INLINE lookup #-}
lookup :: (Monad m, Eq a) => a -> SerialT m (a, b) -> m (Maybe b)
lookup a m = S.lookup a (toStreamS m)

-- | Like 'findM' but with a non-monadic predicate.
--
-- > find p = findM (return . p)
--
-- @since 0.5.0
{-# INLINE find #-}
find :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe a)
find p m = S.find p (toStreamS m)

-- | Returns the first element that satisfies the given predicate.
--
-- @since 0.6.0
{-# INLINE findM #-}
findM :: Monad m => (a -> m Bool) -> SerialT m a -> m (Maybe a)
findM p m = S.findM p (toStreamS m)

-- | Find all the indices where the element in the stream satisfies the given
-- predicate.
--
-- @since 0.5.0
{-# INLINE findIndices #-}
findIndices :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m Int
findIndices p m = fromStreamS $ S.findIndices p (toStreamS m)

-- | Returns the first index that satisfies the given predicate.
--
-- @since 0.5.0
{-# INLINE findIndex #-}
findIndex :: Monad m => (a -> Bool) -> SerialT m a -> m (Maybe Int)
findIndex p = head . findIndices p

-- | Find all the indices where the value of the element in the stream is equal
-- to the given value.
--
-- @since 0.5.0
{-# INLINE elemIndices #-}
elemIndices :: (IsStream t, Eq a, Monad m) => a -> t m a -> t m Int
elemIndices a = findIndices (==a)

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

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second.
--
-- @
-- > S.isPrefixOf (S.fromList "hello") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- @since 0.6.0
{-# INLINE isPrefixOf #-}
isPrefixOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isPrefixOf m1 m2 = D.isPrefixOf (toStreamD m1) (toStreamD m2)

-- | Returns 'True' if all the elements of the first stream occur, in order, in
-- the second stream. The elements do not have to occur consecutively. A stream
-- is treated as a subsequence of itself.
--
-- @
-- > S.isSubsequenceOf (S.fromList "hlo") (S.fromList "hello" :: SerialT IO Char)
-- True
-- @
--
-- @since 0.6.0
{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, IsStream t, Monad m) => t m a -> t m a -> m Bool
isSubsequenceOf m1 m2 = D.isSubsequenceOf (toStreamD m1) (toStreamD m2)

-- | Drops the given prefix from a stream. Returns 'Nothing' if the stream does
-- not start with the given prefix. Returns @Just nil@ when the prefix is the
-- same as the stream.
--
-- @since 0.6.0
{-# INLINE stripPrefix #-}
stripPrefix
    :: (Eq a, IsStream t, Monad m)
    => t m a -> t m a -> m (Maybe (t m a))
stripPrefix m1 m2 = fmap fromStreamD <$>
    D.stripPrefix (toStreamD m1) (toStreamD m2)

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
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- toList = S.foldr (:) []
-- @
--
-- Convert a stream into a list in the underlying monad. The list can be
-- consumed lazily in a lazy monad (e.g. 'Identity'). In a strict monad (e.g.
-- IO) the whole list is generated and buffered before it can be consumed.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: Monad m => SerialT m a -> m [a]
toList = P.toList

-- |
-- @
-- toListRev = S.foldl' (flip (:)) []
-- @
--
-- Convert a stream into a list in reverse order in the underlying monad.
--
-- /Warning!/ working on large lists accumulated as buffers in memory could be
-- very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0
{-# INLINE _toListRev #-}
_toListRev :: Monad m => SerialT m a -> m [a]
_toListRev = D.toListRev . toStreamD

-- |
-- @
-- toHandle h = S.mapM_ $ hPutStrLn h
-- @
--
-- Write a stream of Strings to an IO Handle.
--
-- @since 0.1.0
{-# DEPRECATED toHandle
   "Please use Streamly.FileSystem.Handle module (see the changelog)" #-}
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
toHandle h m = go m
    where
    go m1 =
        let stop = return ()
            single a = liftIO (IO.hPutStrLn h a)
            yieldk a r = liftIO (IO.hPutStrLn h a) >> go r
        in K.foldStream defState yieldk single stop m1

-- | A fold that buffers its input to a pure stream.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0
{-# INLINE _toStream #-}
_toStream :: Monad m => Fold m a (SerialT Identity a)
_toStream = Fold (\f x -> return $ f . (x `K.cons`))
                (return id)
                (return . ($ K.nil))

-- This is more efficient than 'toStream'. toStream is exactly the same as
-- reversing the stream after toStreamRev.
--
-- | Buffers the input stream to a pure stream in the reverse order of the
-- input.
--
-- /Warning!/ working on large streams accumulated as buffers in memory could
-- be very inefficient, consider using "Streamly.Array" instead.
--
-- @since 0.7.0

--  xn : ... : x2 : x1 : []
{-# INLINABLE _toStreamRev #-}
_toStreamRev :: Monad m => Fold m a (SerialT Identity a)
_toStreamRev = Fold (\xs x -> return $ x `K.cons` xs) (return K.nil) return

------------------------------------------------------------------------------
-- General Transformation
------------------------------------------------------------------------------

-- | Use a 'Pipe' to transform a stream.
{-# INLINE _transform #-}
_transform :: (IsStream t, Monad m) => Pipe m a b -> t m a -> t m b
_transform pipe xs = fromStreamD $ D.transform pipe (toStreamD xs)

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
-- /Since: 0.7.0 (Monad m constraint)/
--
-- /Since 0.2.0/
{-# DEPRECATED scanx "Please use scanl followed by map instead." #-}
{-# INLINE scanx #-}
scanx :: (IsStream t, Monad m) => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx = P.scanlx'

-- XXX this needs to be concurrent
-- | Like 'scanl'' but with a monadic fold function.
--
-- @since 0.4.0
{-# INLINE scanlM' #-}
scanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> t m b
scanlM' step begin m = fromStreamD $ D.scanlM' step begin $ toStreamD m

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
-- Consider the following monolothic example, computing the sum and the product
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
-- | Like 'postscanl'' but with a monadic step function.
--
-- @since 0.7.0
{-# INLINE postscanlM' #-}
postscanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> t m b
postscanlM' step z m = fromStreamD $ D.postscanlM' step z $ toStreamD m

-- XXX prescanl does not sound very useful, enable only if there is a
-- compelling use case.
--
-- | Like scanl' but does not stream the final value of the accumulator.
--
-- @since 0.6.0
{-# INLINE _prescanl' #-}
_prescanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
_prescanl' step z m = fromStreamD $ D.prescanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like postscanl' but with a monadic step function.
--
-- @since 0.6.0
{-# INLINE _prescanlM' #-}
_prescanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> m b -> t m a -> t m b
_prescanlM' step z m = fromStreamD $ D.prescanlM' step z $ toStreamD m

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
{-# INLINE runScan #-}
runScan :: Monad m => Fold m a b -> SerialT m a -> SerialT m b
runScan (Fold step begin done) = P.scanlMx' step begin done

-- | Postscan a stream using the given monadic fold.
--
-- @since 0.7.0
{-# INLINE runPostscan #-}
runPostscan :: Monad m => Fold m a b -> SerialT m a -> SerialT m b
runPostscan (Fold step begin done) = P.postscanlMx' step begin done

-- XXX runPrescan

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

-- | Ensures that all the elements of the stream are identical and then returns
-- that unique element.
--
-- @since 0.6.0
{-# INLINE the #-}
the :: (Eq a, Monad m) => SerialT m a -> m (Maybe a)
the m = S.the (toStreamS m)

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

------------------------------------------------------------------------------
-- Transformation by Map and Filter
------------------------------------------------------------------------------

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- @since 0.3.0
{-# INLINE mapMaybe #-}
mapMaybe :: (IsStream t, Monad m) => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = fromStreamS $ S.mapMaybe f $ toStreamS m

-- | Like 'mapMaybe' but maps a monadic function.
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

------------------------------------------------------------------------------
-- Transformation by Reordering
------------------------------------------------------------------------------

-- XXX Use a compact region list to temporarily store the list, in both reverse
-- as well as in reverse'.
--
-- /Note:/ 'reverse'' is much faster than this, use that when performance
-- matters.
--
-- |
-- > reverse = S.foldlT (flip S.cons) S.nil
--
-- Returns the elements of the stream in reverse order.  The stream must be
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
-- @since 0.7.0
{-# INLINE _reverse' #-}
_reverse' :: (IsStream t, MonadIO m, Storable a) => t m a -> t m a
_reverse' s = fromStreamD $ D.reverse' $ toStreamD s

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- intersperseM = intersperseBySpan 1

-- | Generate a stream by performing a monadic action between consecutive
-- elements of the given stream.
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @
-- > S.toList $ S.intersperseM (return ',') $ S.fromList "hello"
-- "h,e,l,l,o"
-- @
--
-- @since 0.5.0
{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM m = fromStreamS . S.intersperseM m . toStreamS

-- | Generate a stream by inserting a given element between consecutive
-- elements of the given stream.
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

-- | Insert a monadic action after each element in the stream.
--
-- @since 0.7.0
{-# INLINE _insertAfterEach #-}
_insertAfterEach :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
_insertAfterEach m = fromStreamD . D.insertAfterEach m . toStreamD

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

-- | Intersperse a monadic action into the input stream after every @n@
-- seconds.
--
-- @
-- > S.drain $ S.intersperseByTime 1 (putChar ',') $ S.mapM (\\x -> threadDelay 1000000 >> putChar x) $ S.fromList "hello"
-- "h,e,l,l,o"
-- @
--
-- @since 0.7.0
{-# INLINE intersperseByTime #-}
intersperseByTime
    :: (IsStream t, MonadAsync m)
    => Double -> m a -> t m a -> t m a
intersperseByTime n f xs = xs `Par.parallelEndByFirst` repeatM timed
    where timed = liftIO (threadDelay (round $ n * 1000000)) >> f

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

-- | Deletes the first occurence of the element in the stream that satisfies
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

-- | Like 'zipWith' but using a monadic zipping function.
--
-- @since 0.4.0
{-# INLINABLE zipWithM #-}
zipWithM :: (IsStream t, Monad m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStreamS $ S.zipWithM f (toStreamS m1) (toStreamS m2)

-- | Zip two streams serially using a pure zipping function.
--
-- @
-- > S.toList $ S.zipWith (+) (S.fromList [1,2,3]) (S.fromList [4,5,6])
-- [5,7,9]
-- @
--
-- @since 0.1.0
{-# INLINABLE zipWith #-}
zipWith :: (IsStream t, Monad m) => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStreamS $ S.zipWith f (toStreamS m1) (toStreamS m2)

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality using an equality function.
--
-- @since 0.6.0
{-# INLINABLE eqBy #-}
eqBy :: (IsStream t, Monad m) => (a -> b -> Bool) -> t m a -> t m b -> m Bool
eqBy = P.eqBy

-- | Compare two streams lexicographically using a comparison function.
--
-- @since 0.6.0
{-# INLINABLE cmpBy #-}
cmpBy
    :: (IsStream t, Monad m)
    => (a -> b -> Ordering) -> t m a -> t m b -> m Ordering
cmpBy = P.cmpBy

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
{-# INLINABLE mergeUptoShortest #-}
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
mergeAsyncBy :: (IsStream t, MonadAsync m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeAsyncBy f m1 m2 = K.mkStream $ \st stp sng yld -> do
    ma <- mkAsync' st m1
    mb <- mkAsync' st m2
    K.foldStream st stp sng yld (K.mergeBy f ma mb)

-- | Like 'mergeByM' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
mergeAsyncByM :: (IsStream t, MonadAsync m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeAsyncByM f m1 m2 = K.mkStream $ \st stp sng yld -> do
    ma <- mkAsync' st m1
    mb <- mkAsync' st m2
    K.foldStream st stp sng yld (K.mergeByM f ma mb)

------------------------------------------------------------------------------
-- Nesting
------------------------------------------------------------------------------

-- | Map each element of the stream to produce a stream and then flatten the
-- results into a single stream.
--
-- > concatMap f = concatMapM (return . f)
--
-- In imperative terms, a 'concatMap' corresponds to nested loops. We loop over
-- the input stream and then for each element of the input stream another
-- stream is generated and then we loop over that inner stream as well to yield
-- each element of that stream to generate a single output stream.
--
-- 'concatMap' can perform filtering by mapping an element to a 'nil'
-- stream:
--
-- > filter p m = S.concatMap (\x -> if p x then S.yield x else S.nil) m
--
-- Its a map, therefore it can degenerate to a simple map operation as well:
--
-- > map f m = S.concatMap (\x -> S.yield (f x)) m
--
-- 'concatMap' is not inherently stateful, however, it can be combined with a
-- scan to perform stateful operations.
--
-- @since 0.6.0
{-# INLINE concatMap #-}
concatMap ::(IsStream t, Monad m) => (a -> t m b) -> t m a -> t m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Map each element to a stream to produce a stream using a monadic function
-- and then flatten the results into a single stream.
--
-- @since 0.6.0
{-# INLINE concatMapM #-}
concatMapM :: (IsStream t, Monad m) => (a -> m (t m b)) -> t m a -> t m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

{-
-- | A generalization of insertBy to inserting sequences.
interposeBy :: (IsStream t, Monad m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a

-- | A generalization of intersperseM to intersperse sequences.
intercalate :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
-}

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

-- | @splitAt n f1 f2@ composes folds @f1@ and @f2@ such that first @n@
-- elements of its input are consumed by fold @f1@ and the rest of the stream
-- is consumed by fold @f2@.
--
-- > let splitAt_ n xs = S.runFold (FL.splitAt n FL.toList FL.toList) $ S.fromList xs
--
-- >>> splitAt_ 6 "Hello World!"
-- > ("Hello ","World!")
--
-- >>> splitAt_ (-1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 0 [1,2,3]
-- > ([],[1,2,3])
--
-- >>> splitAt_ 1 [1,2,3]
-- > ([1],[2,3])
--
-- >>> splitAt_ 3 [1,2,3]
-- > ([1,2,3],[])
--
-- >>> splitAt_ 4 [1,2,3]
-- > ([1,2,3],[])
--
-- This can be considered as a two-fold version of 'ltake' where we take both
-- the segments instead of discarding the leftover.
--
-- @since 0.7.0
{-# INLINE _splitAt #-}
_splitAt
    :: Monad m
    => Int
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_splitAt n (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract
    where
      initial  = Tuple3' <$> return n <*> initialL <*> initialR

      step (Tuple3' i xL xR) input =
        if i > 0
        then stepL xL input >>= (\a -> return (Tuple3' (i - 1) a xR))
        else stepR xR input >>= (\b -> return (Tuple3' i xL b))

      extract (Tuple3' _ a b) = (,) <$> extractL a <*> extractR b

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
chunksOf n f m = D.fromStreamD $ D.groupsOf n f (D.toStreamD m)

-- | Transform a fold from a pure input to a 'Maybe' input, consuming only
-- 'Just' values.
{-# INLINE lcatMaybes #-}
lcatMaybes :: Monad m => Fold m a b -> Fold m (Maybe a) b
lcatMaybes = FL.lfilter isJust . FL.lmap fromJust

-- XXX we can implement this by repeatedly applying the 'lrunFor' fold.
-- XXX add this example after fixing the serial stream rate control
-- >>> S.toList $ S.take 5 $ sessionsOf 1 FL.sum $ constRate 2 $ S.enumerateFrom 1
-- > [3,7,11,15,19]
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- @since 0.7.0
{-# INLINE sessionsOf #-}
sessionsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
sessionsOf n f xs =
    splitBySuffix isNothing (lcatMaybes f)
        (intersperseByTime n (return Nothing) (Serial.map Just xs))

------------------------------------------------------------------------------
-- Element Aware APIs
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Binary APIs
------------------------------------------------------------------------------

-- | Break the input stream into two groups, the first group takes the input as
-- long as the predicate applied to the first element of the stream and next
-- input element holds 'True', the second group takes the rest of the input.
--
_spanBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_spanBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where
      initial = Tuple3' <$> initialL <*> initialR <*> return (Tuple' Nothing True)

      step (Tuple3' a b (Tuple' (Just frst) isFirstG)) input =
        if cmp frst input && isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just frst) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      step (Tuple3' a b (Tuple' Nothing isFirstG)) input =
        if isFirstG
        then stepL a input
              >>= (\a' -> return (Tuple3' a' b (Tuple' (Just input) isFirstG)))
        else stepR b input
              >>= (\a' -> return (Tuple3' a a' (Tuple' Nothing False)))

      extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- | @span p f1 f2@ composes folds @f1@ and @f2@ such that @f1@ consumes the
-- input as long as the predicate @p@ is 'True'.  @f2@ consumes the rest of the
-- input.
--
-- > let span_ p xs = S.runFold (S.span p FL.toList FL.toList) $ S.fromList xs
--
-- >>> span_ (< 1) [1,2,3]
-- > ([],[1,2,3])
--
-- >>> span_ (< 2) [1,2,3]
-- > ([1],[2,3])
--
-- >>> span_ (< 4) [1,2,3]
-- > ([1,2,3],[])
--
-- @since 0.7.0

-- This can be considered as a two-fold version of 'ltakeWhile' where we take
-- both the segments instead of discarding the leftover.
{-# INLINE span #-}
span
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
span p (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

    where

    initial = Tuple3' <$> initialL <*> initialR <*> return True

    step (Tuple3' a b isFirstG) input =
        if isFirstG && p input
        then stepL a input >>= (\a' -> return (Tuple3' a' b True))
        else stepR b input >>= (\a' -> return (Tuple3' a a' False))

    extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

-- |
-- > break p = span (not . p)
--
-- Break as soon as the predicate becomes 'True'. @break p f1 f2@ composes
-- folds @f1@ and @f2@ such that @f1@ stops consuming input as soon as the
-- predicate @p@ becomes 'True'. The rest of the input is consumed @f2@.
--
-- This is the binary version of 'splitBy'.
--
-- > let break_ p xs = S.runFold (S.break p FL.toList FL.toList) $ S.fromList xs
--
-- >>> break_ (< 1) [3,2,1]
-- > ([3,2,1],[])
--
-- >>> break_ (< 2) [3,2,1]
-- > ([3,2],[1])
--
-- >>> break_ (< 4) [3,2,1]
-- > ([],[3,2,1])
--
-- @since 0.7.0
{-# INLINE _break #-}
_break
    :: Monad m
    => (a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_break p = span (not . p)

-- | Like 'spanBy' but applies the predicate in a rolling fashion i.e.
-- predicate is applied to the previous and the next input elements.
{-# INLINE _spanRollingBy #-}
_spanRollingBy
    :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Fold m a c
    -> Fold m a (b, c)
_spanRollingBy cmp (Fold stepL initialL extractL) (Fold stepR initialR extractR) =
    Fold step initial extract

  where
    initial = Tuple3' <$> initialL <*> initialR <*> return Nothing

    step (Tuple3' a b (Just frst)) input =
      if cmp input frst
      then stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))
      else stepR b input >>= (\b' -> return (Tuple3' a b' (Just input)))

    step (Tuple3' a b Nothing) input =
      stepL a input >>= (\a' -> return (Tuple3' a' b (Just input)))

    extract (Tuple3' a b _) = (,) <$> extractL a <*> extractR b

------------------------------------------------------------------------------
-- N-ary APIs
------------------------------------------------------------------------------
--
-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @a \`cmp` b@ is 'True' then @b@ is also assigned to the same
-- group.  If @a \`cmp` c@ is 'True' then @c@ is also assigned to the same
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
groups :: (IsStream t, Monad m, Eq a) => Fold m a b -> t m a -> t m b
groups = groupsBy (==)

------------------------------------------------------------------------------
-- Binary splitting on a separator
------------------------------------------------------------------------------

{-
-- | Find the first occurrence of the specified sequence in the input stream
-- and break the input stream into two parts, the first part consisting of the
-- stream before the sequence and the second part consisting of the sequence
-- and the rest of the stream.
--
-- > let breakOn_ pat xs = S.runFold (S.breakOn pat FL.toList FL.toList) $ S.fromList xs
--
-- >>> breakOn_ "dear" "Hello dear world!"
-- > ("Hello ","dear world!")
--
{-# INLINE breakOn #-}
breakOn :: Monad m => Array a -> Fold m a b -> Fold m a c -> Fold m a (b,c)
breakOn pat f m = undefined
-}

------------------------------------------------------------------------------
-- N-ary split on a predicate
------------------------------------------------------------------------------

-- TODO: Use a Splitter configuration similar to the "split" package to make it
-- possible to express all splitting combinations. In general, we can have
-- infix/suffix/prefix/condensing of separators, dropping both leading/trailing
-- separators. We can have a single split operation taking the splitter config
-- as argument.

-- | Split a stream on separator elements determined by a predicate, dropping
-- the separator. Separator is considered as infixed between two segments, if
-- one side of the separator is missing then it is parsed as an empty stream.
-- With '-' representing elements and '.' as separator, 'splitOn' splits as
-- follows:
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
splitOn predicate f m =
    D.fromStreamD $ D.splitBy predicate f (D.toStreamD m)

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
    D.fromStreamD $ D.splitSuffixBy predicate f (D.toStreamD m)

-- | Like 'splitBy' but ignores repeated separators or separators in leading
-- or trailing position. Therefore, @"..a..b.."@ would be parsed as
-- @["a","b"]@.  In other words, it treats the input like words separated by
-- whitespace elements determined by the predicate.
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
{-# INLINE wordsBy #-}
wordsBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
wordsBy predicate f m =
    D.fromStreamD $ D.wordsBy predicate f (D.toStreamD m)

-- | Like 'splitOnSuffix' but keeps the suffix attached to the resulting
-- splits.
--
-- > splitBySuffix' p xs = S.toList $ S.splitBySuffix p (FL.toList) (S.fromList xs)
--
-- >>> splitBySuffix' (== '.') ""
-- []
--
-- >>> splitBySuffix' (== '.') "."
-- ["."]
--
-- >>> splitBySuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitBySuffix' (== '.') ".a"
-- > [".","a"]
--
-- >>> splitBySuffix' (== '.') "a."
-- > ["a."]
--
-- >>> splitBySuffix' (== '.') "a.b"
-- > ["a.","b"]
--
-- >>> splitBySuffix' (== '.') "a.b."
-- > ["a.","b."]
--
-- >>> splitBySuffix' (== '.') "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0

-- This can be considered as an n-fold version of 'breakPost' where we apply
-- 'breakPost' successively on the input stream.
--
{-# INLINE splitBySuffix #-}
splitBySuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitBySuffix predicate f m =
    D.fromStreamD $ D.splitSuffixBy' predicate f (D.toStreamD m)

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
-- @since 0.7.0

-- XXX We can use a polymorphic vector implemented by Array# to represent the
-- sequence, that way we can avoid the Storable constraint. If we still need
-- Storable Array for performance, we can use a separate splitOnArray API for
-- that. We can also have an API where the sequence itself is a lazy stream, so
-- that we can search files in files for example.
{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m = D.fromStreamD $ D.splitOn patt f (D.toStreamD m)

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
-- > splitSuffixOn_ pat xs = S.toList $ S.splitSuffixOn (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn_ "." ""
-- [""]
--
-- >>> splitSuffixOn_ "." "."
-- [""]
--
-- >>> splitSuffixOn_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn_ "." ".a"
-- > ["","a"]
--
-- >>> splitSuffixOn_ "." "a."
-- > ["a"]
--
-- >>> splitSuffixOn_ "." "a.b"
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a.b."
-- > ["a","b"]
--
-- >>> splitSuffixOn_ "." "a..b.."
-- > ["a","","b",""]
--
-- > lines = splitSuffixOn "\n"
--
-- @since 0.7.0
{-# INLINE _splitOnSuffixSeq #-}
_splitOnSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
_splitOnSuffixSeq patt f m =
    D.fromStreamD $ D.splitSuffixOn False patt f (D.toStreamD m)

{-
-- | Like 'splitOn' but drops any empty splits.
--
{-# INLINE wordsOn #-}
wordsOn
    :: (IsStream t, Monad m, Storable a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
wordsOn subseq f m = undefined -- D.fromStreamD $ D.wordsOn f subseq (D.toStreamD m)
-}

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
-- @since 0.7.0
{-# INLINE _splitBySeq #-}
_splitBySeq
    :: (IsStream t, MonadAsync m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
_splitBySeq patt f m = intersperseM (runFold f (A.read patt)) $ splitOnSeq patt f m

-- | Like 'splitSuffixOn' but keeps the suffix intact in the splits.
--
-- > splitSuffixOn'_ pat xs = S.toList $ FL.splitSuffixOn' (A.fromList pat) (FL.toList) (S.fromList xs)
--
-- >>> splitSuffixOn'_ "." ""
-- [""]
--
-- >>> splitSuffixOn'_ "." "."
-- ["."]
--
-- >>> splitSuffixOn'_ "." "a"
-- ["a"]
--
-- >>> splitSuffixOn'_ "." ".a"
-- > [".","a"]
--
-- >>> splitSuffixOn'_ "." "a."
-- > ["a."]
--
-- >>> splitSuffixOn'_ "." "a.b"
-- > ["a.","b"]
--
-- >>> splitSuffixOn'_ "." "a.b."
-- > ["a.","b."]
--
-- >>> splitSuffixOn'_ "." "a..b.."
-- > ["a.",".","b.","."]
--
-- @since 0.7.0
{-# INLINE _splitBySuffixSeq #-}
_splitBySuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
_splitBySuffixSeq patt f m =
    D.fromStreamD $ D.splitSuffixOn True patt f (D.toStreamD m)

{-
-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
{-# INLINE splitSuffixOnAny #-}
splitSuffixOnAny
    :: (IsStream t, Monad m, Storable a, Integral a)
    => [Array a] -> Fold m a b -> t m a -> t m b
splitSuffixOnAny subseq f m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)
-}

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
tap :: (IsStream t, Monad m) => FL.Fold m a b -> t m a -> t m a
tap f xs = D.fromStreamD $ D.tap f (D.toStreamD xs)

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
trace :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m a
trace f = mapM (\x -> void (f x) >> return x)

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

-- | @classifySessionsBy tick timeout reset f stream@ groups together all input
-- stream elements that belong to the same session. @timeout@ is the maximum
-- lifetime of a session in seconds. All elements belonging to a session are
-- purged after this duration.  If "reset" is 'Ture' then the timeout is reset
-- after every event received in the session. Session duration is measured
-- using the timestamp of the first element seen for that session.  To detect
-- session timeouts, a monotonic event time clock is maintained using the
-- timestamps seen in the inputs and a timer with a tick duration specified by
-- @tick@.
--
-- @session key@ is a key that uniquely identifies the session for the given
-- element, @timestamp@ characterizes the time when the input element was
-- generated, this is an absolute time measured from some @Epoch@. @session
-- close@ is a boolean indicating whether this element marks the closing of the
-- session. When an input element with @session close@ set to @True@ is seen
-- the session is purged immediately.
--
-- All the input elements belonging to a session are collected using the fold
-- @f@.  The session key and the fold result are emitted in the output stream
-- when the session is purged either via the session close event or via the
-- session liftime timeout.
--
-- @since 0.7.0
{-# INLINABLE classifySessionsBy #-}
classifySessionsBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ session timeout
    -> Bool           -- ^ reset the timeout when an event is received
    -> Fold m a b     -- ^ Fold to be applied to session events
    -> t m (k, a, Bool, AbsTime) -- ^ session key, timestamp, close event, data
    -> t m (k, b)
classifySessionsBy tick timeout reset (Fold step initial extract) str =
    concatMap (\(Tuple4' _ _ _ s) -> s) $ scanlM' sstep szero stream

    where

    timeoutMs = toRelTime (round (timeout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = Tuple4' (toAbsTime (0 :: MilliSecond64)) H.empty Map.empty K.nil

    -- Got a new stream input element
    sstep (Tuple4' evTime hp mp _) (Just (key, a, closing, ts)) =
        -- XXX we should use a heap in pinned memory to scale it to a large
        -- size
        --
        -- deleting a key from the heap is expensive, so we never delete a
        -- key, we just purge it from the Map and it gets purged from the
        -- heap on timeout. We just need an extra lookup in the Map when
        -- the key is purged from the heap, that should not be expensive.
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
        let accumulate v = do
                Tuple' _ old <- maybe (initial >>= return . Tuple' ts) return v
                new <- step old a
                return $ Tuple' ts new
        in if closing
           then do
                let (r, mp') = Map.updateLookupWithKey (\_ _ -> Nothing) key mp
                Tuple' _ acc <- accumulate r
                res <- extract acc
                return $ Tuple4' evTime hp mp' (yield (key, res))
           else do
                    let r = Map.lookup key mp
                    acc <- accumulate r
                    let mp' = Map.insert key acc mp
                    let hp' =
                            case r of
                                Nothing ->
                                    let expiry = addToAbsTime ts timeoutMs
                                    in H.insert (Entry expiry key) hp
                                Just _ -> hp
                    -- Event time is maintained as monotonically increasing
                    -- time. If we have lagged behind any of the timestamps
                    -- seen then we increase it to match the latest time seen
                    -- in the timestamps. We also increase it on timer ticks.
                    return $ Tuple4' (max evTime ts) hp' mp' K.nil

    -- Got a timer tick event
    -- XXX can we yield the entries without accumulating them?
    sstep (Tuple4' evTime heap sessions _) Nothing = do
        (hp', mp', out) <- go heap sessions K.nil
        return $ Tuple4' curTime hp' mp' out

        where

        curTime = addToAbsTime evTime tickMs
        go hp mp out = do
            let hres = H.uncons hp
            case hres of
                Just (Entry ts key, hp') -> do
                    let duration = diffAbsTime curTime ts
                    if duration >= timeoutMs
                    then do
                        let (r, mp') = Map.updateLookupWithKey
                                            (\_ _ -> Nothing) key mp
                        case r of
                            Nothing -> go hp' mp' out
                            Just (Tuple' latestTS acc) -> do
                                let dur = diffAbsTime curTime latestTS
                                if dur >= timeoutMs || not reset
                                then do
                                    sess <- extract acc
                                    go hp' mp' ((key, sess) `K.cons` out)
                                else
                                    -- reset the session timeout
                                    let expiry = addToAbsTime latestTS timeoutMs
                                        hp'' = H.insert (Entry expiry key) hp'
                                        mp'' = Map.insert key (Tuple' latestTS acc) mp'
                                    in go hp'' mp'' out
                    else return (hp, mp, out)
                Nothing -> return (hp, mp, out)

    -- merge timer events in the stream
    stream = Serial.map Just str `Par.parallel` repeatM timer
    timer = do
        liftIO $ threadDelay (round $ tick * 1000000)
        return Nothing

-- | Like 'classifySessionsOf' but the session is kept alive if an event is
-- received within the session window. The session times out and gets closed
-- only if no event is received within the specified session window size.
--
-- @since 0.7.0
{-# INLINABLE _classifyKeepAliveSessions #-}
_classifyKeepAliveSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ session inactive timeout
    -> Fold m a b     -- ^ Fold to be applied to session payload data
    -> t m (k, a, Bool, AbsTime) -- ^ session key, data, close flag, timestamp
    -> t m (k, b)
_classifyKeepAliveSessions timeout = classifySessionsBy 1 timeout True

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
-- seconds. Within each such window, fold the elements in buckets identified by
-- the keys. A particular bucket fold can be terminated early if a closing flag
-- is encountered in an element for that key. Once a fold is terminated the key
-- and value for that bucket are emitted in the output stream.
--
-- Session @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input element was generated.  To detect
-- session window end, a monotonic event time clock is maintained synced with
-- the timestamps with a clock resolution of 1 second.
--
-- @since 0.7.0
{-# INLINABLE _classifySessionsOf #-}
_classifySessionsOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ time window size
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
_classifySessionsOf interval = classifySessionsBy 1 interval False
------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Run a side effect before the stream yields its first element.
--
-- @since 0.7.0
{-# INLINE before #-}
before :: (IsStream t, Monad m) => m b -> t m a -> t m a
before action xs = D.fromStreamD $ D.before action $ D.toStreamD xs

-- | Run a side effect whenever the stream stops normally.
--
-- @since 0.7.0
{-# INLINE after #-}
after :: (IsStream t, Monad m) => m b -> t m a -> t m a
after action xs = D.fromStreamD $ D.after action $ D.toStreamD xs

-- | Run a side effect whenever the stream aborts due to an exception.
--
-- @since 0.7.0
{-# INLINE onException #-}
onException :: (IsStream t, MonadCatch m) => m b -> t m a -> t m a
onException action xs = D.fromStreamD $ D.onException action $ D.toStreamD xs

-- | Run a side effect whenever the stream stops normally or aborts due to an
-- exception.
--
-- @since 0.7.0
{-# INLINE finally #-}
finally :: (IsStream t, MonadCatch m) => m b -> t m a -> t m a
finally action xs = D.fromStreamD $ D.finally action $ D.toStreamD xs

-- | Run the first action before the stream starts and remember its output,
-- generate a stream using the output, run the second action using the
-- remembered value as an argument whenever the stream ends normally or due to
-- an exception.
--
-- @since 0.7.0
{-# INLINE bracket #-}
bracket :: (IsStream t, MonadCatch m)
    => m b -> (b -> m c) -> (b -> t m a) -> t m a
bracket bef aft bet = D.fromStreamD $
    D.bracket bef aft (\x -> toStreamD $ bet x)

-- | When evaluating a stream if an exception occurs, stream evaluation aborts
-- and the specified exception handler is run with the exception as argument.
--
-- @since 0.7.0
{-# INLINE handle #-}
handle :: (IsStream t, MonadCatch m, Exception e)
    => (e -> m a) -> t m a -> t m a
handle handler xs = D.fromStreamD $ D.handle handler $ D.toStreamD xs
