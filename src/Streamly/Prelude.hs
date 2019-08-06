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

      nil
    , cons
    , (.:)

    , consM
    , (|:)

    -- ** From Values
    -- | Generate a monadic stream from a seed value or values.
    , yield
    , yieldM
    , repeat
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
    , fromList
    , fromListM
    , fromFoldable
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
    , map
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
    , zipAsyncWith
    , zipAsyncWithM

    -- ** Nested Streams
    -- | Stream operations represent loops in imperative programming, streams
    -- of streams represent nested loops.

    , concatMapM
    , concatMap
    , concatMapBy
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
    , once
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

import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, repeat, replicate, concatMap, span)

import Streamly.Prelude.Internal
