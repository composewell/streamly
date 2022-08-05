{-# OPTIONS_GHC -Wno-orphans #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Data.Stream
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
--
-- We will add some more imports in the examples as needed.
--
-- For effectful streams we will use the following IO action:
--
-- >>> effect n = print n >> return n
--
-- = Overview
--
-- Streamly is a framework for modular data flow based programming and
-- declarative concurrency.  Powerful stream fusion framework in streamly
-- allows high performance combinatorial programming even when using byte level
-- streams.  Streamly API is similar to Haskell lists.
--
-- Streams can be constructed
-- like lists, except that they use 'nil' instead of '[]' and 'cons' instead of
-- ':'.
--
-- `cons` constructs a pure stream which is more or less the same as a list:
--
-- >>> import Streamly.Data.Stream (Stream, cons, consM, nil)
-- >>> stream = 1 `cons` 2 `cons` nil :: Stream IO Int
-- >>> Stream.fold Fold.toList stream -- IO [Int]
-- [1,2]
--
-- 'consM' constructs a stream from effectful actions:
--
-- >>> stream = effect 1 `consM` effect 2 `consM` nil
-- >>> Stream.fold Fold.toList stream
-- 1
-- 2
-- [1,2]
--
-- == Console Echo Example
--
-- In the following example, 'repeatM' generates an infinite stream of 'String'
-- by repeatedly performing the 'getLine' IO action. 'mapM' then applies
-- 'putStrLn' on each element in the stream converting it to stream of '()'.
-- Finally, 'drain' folds the stream to IO discarding the () values, thus
-- producing only effects.
--
-- >>> import Data.Function ((&))
--
-- >>> :{
-- echo =
--  Stream.repeat getLine        -- Stream IO (IO String)
--      & Stream.sequence        -- Stream IO String
--      & Stream.mapM putStrLn   -- Stream IO ()
--      & Stream.fold Fold.drain -- IO ()
-- :}
--
-- This is a console echo program. It is an example of a declarative loop
-- written using streaming combinators.  Compare it with an imperative @while@
-- loop.
--
-- Hopefully, this gives you an idea how we can program declaratively by
-- representing loops using streams. In this module, you can find all
-- "Data.List" like functions and many more powerful combinators to perform
-- common programming tasks.
--
-- Also see "Streamly.Internal.Data.Stream" module for many more @Pre-release@
-- combinators. See the <https://github.com/composewell/streamly-examples>
-- repository for many more real world examples of stream programming.
--
module Streamly.Data.Stream
    (
      Stream
    -- * Construction
    -- | Functions ending in the general shape @b -> Stream m a@.
    --
    -- See also: "Streamly.Internal.Data.Stream.Generate" for
    -- @Pre-release@ functions.

    -- ** Primitives
    -- | Primitives to construct a stream from pure values or monadic actions.
    -- All other stream construction and generation combinators described later
    -- can be expressed in terms of these primitives. However, the special
    -- versions provided in this module can be much more efficient in most
    -- cases. Users can create custom combinators using these primitives.
    , nil
    , cons
    , consM
    -- , cons2 -- fused version
    -- , consM2 -- fused version

    -- ** Unfolding
    -- | 'unfoldrM' is the most general way of generating a stream efficiently.
    -- All other generation operations can be expressed using it.
    , unfoldr
    , unfoldrM

    -- ** From Values
    -- | Generate a monadic stream from a seed value or values.
    , fromPure
    , fromEffect
    , repeat
    , replicate

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

    -- ** Iteration
    , iterate
    , iterateM

    -- ** From Containers
    -- | Convert an input structure, container or source into a stream. All of
    -- these can be expressed in terms of primitives.
    , fromList
    , fromFoldable

    -- ** From Unfolds
    -- | Most of the above stream generation operations can also be expressed
    -- using the corresponding unfolds in the "Streamly.Data.Unfold" module.
    , unfold -- XXX rename to fromUnfold?

    -- * Elimination
    -- | Functions ending in the general shape @Stream m a -> m b@ or @Stream m
    -- a -> m (b, Stream m a)@
    --
    -- See also: "Streamly.Internal.Data.Stream.Eliminate" for @Pre-release@
    -- functions.

-- EXPLANATION: In imperative terms a fold can be considered as a loop over the stream
-- that reduces the stream to a single value.
-- Left and right folds both use a fold function @f@ and an identity element
-- @z@ (@zero@) to deconstruct a recursive data structure and reconstruct a
-- new data structure. The new structure may be a recursive construction (a
-- container) or a non-recursive single value reduction of the original
-- structure.
--
-- Both right and left folds are mathematical duals of each other, they are
-- functionally equivalent.  Operationally, a left fold on a left associated
-- structure behaves exactly in the same way as a right fold on a right
-- associated structure. Similarly, a left fold on a right associated structure
-- behaves in the same way as a right fold on a left associated structure.
-- However, the behavior of a right fold on a right associated structure is
-- operationally different (even though functionally equivalent) than a left
-- fold on the same structure.
--
-- On right associated structures like Haskell @cons@ lists or Streamly
-- streams, a lazy right fold is naturally suitable for lazy recursive
-- reconstruction of a new structure, while a strict left fold is naturally
-- suitable for efficient reduction. In right folds control is in the hand of
-- the @puller@ whereas in left folds the control is in the hand of the
-- @pusher@.
--
-- The behavior of right and left folds are described in detail in the
-- individual fold's documentation.  To illustrate the two folds for right
-- associated @cons@ lists:
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

-- NOTE: Folds are inherently serial as each step needs to use the result of
-- the previous step. However, it is possible to fold parts of the stream in
-- parallel and then combine the results using a monoid.

    -- ** Primitives
    -- Consuming a part of the stream and returning the rest. Functions
    -- ending in the general shape @Stream m a -> m (b, Stream m a)@
    , uncons

    -- ** Folding
    -- XXX Need to have a general parse operations here which can be used to
    -- express all others.
    , fold -- XXX rename to run? We can have a Stream.run and Fold.run.
    -- XXX fold1 can be achieved using Monoids or Refolds.
    , foldBreak
    , foldContinue

    -- ** Parsing
    , parse
    , parseBreak

    -- -- ** Lazy Right Folds
    -- Consuming a stream to build a right associated expression, suitable
    -- for lazy evaluation. Evaluation of the input happens when the output of
    -- the fold is evaluated, the fold output is a lazy thunk.
    --
    -- This is suitable for stream transformation operations, for example,
    -- operations like mapping a function over the stream.
    -- , foldrM
    -- , foldr

    -- * Mapping
    -- | Stateless one-to-one transformations. Use 'fmap' for mapping a pure
    -- function on a stream.

    -- EXPLANATION:
    -- In imperative terms a map operation can be considered as a loop over
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
    , sequence
    , mapM
    -- , trace -- XXX Use "tracing" map instead?
    , tap
    , delay

    -- * Scanning
    -- | Stateful one-to-one transformations.
    --
    -- See also: "Streamly.Internal.Data.Stream.Transform" for
    -- @Pre-release@ functions.
    , scan
    , postscan
    -- XXX postscan1 can be implemented using Monoids or Refolds.
    -- Indexing can be considered as a special type of zipping where we zip a
    -- stream with an index stream.
    , indexed
    , indexedR

    -- * Insertion
    -- | Add elements to the stream.

    -- Inserting elements is a special case of interleaving/merging streams.
    , insertBy
    , intersperseM
    , intersperse

    -- * Filtering
    -- | Remove elements from the stream.

    -- ** Stateless Filters
    -- | 'mapMaybeM' is the most general stateless filtering operation. All
    -- other filtering operations can be expressed using it.
    -- EXPLANATION:
    -- In imperative terms a filter over a stream corresponds to a loop with a
    -- @continue@ clause for the cases when the predicate fails.

    , mapMaybe
    , mapMaybeM
    , filter
    , filterM

    -- ** Stateful Filters
    -- | 'scanMaybe' is the most general stateful filtering operation. The
    -- filtering folds (folds returning a 'Maybe' type) in
    -- "Streamly.Internal.Data.Fold" can be used along with 'scanMaybe' to
    -- perform stateful filtering operations in general.
    , scanMaybe
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM
    , deleteBy
    , uniq

    -- ** Searching
    -- Finding the presence or location of an element, a sequence of elements
    -- or another stream within a stream.

    -- -- ** Searching Elements
    , findIndices
    , elemIndices

    -- * Combining Two Streams
    -- ** Appending
    , append
    , append2

    -- ** Interleaving
    , interleave
    -- , interleave2

    -- ** Merging
    -- | Merging of @n@ streams can be performed by combining the streams pair
    -- wise using
    -- 'Streamly.Internal.Data.Stream.IsStream.Expand.concatPairsWith' to give
    -- O(n * log n) time complexity.
    -- If used with 'concatMapWith' it will have O(n^2) performance.

    , mergeBy
    , mergeByM
    -- , mergeBy2
    -- , mergeByM2

    -- ** Zipping
    -- | Zipping of @n@ streams can be performed by combining the streams pair
    -- wise using
    -- 'Streamly.Internal.Data.Stream.IsStream.Expand.concatPairsWith' with
    -- O(n * log n) time complexity.
    -- If used with 'concatMapWith' it will have O(n^2) performance.
    , zipWith
    , zipWithM
    -- , zipWith2
    -- , zipWithM2

    -- * Unfold Each
    , unfoldMany
    , intercalate
    , intercalateSuffix

    -- * Stream of streams
    -- | Stream operations like map and filter represent loop processing in
    -- imperative programming terms. Similarly, the imperative concept of
    -- nested loops are represented by streams of streams. The 'concatMap'
    -- operation represents nested looping.
    -- A 'concatMap' operation loops over the input stream and then for each
    -- element of the input stream generates another stream and then loops over
    -- that inner stream as well producing effects and generating a single
    -- output stream.
    -- The 'Monad' instances of different stream types provide a more
    -- convenient way of writing nested loops. Note that the monad bind
    -- operation is just @flip concatMap@.
    --
    -- One dimension loops are just a special case of nested loops.  For
    -- example, 'concatMap' can degenerate to a simple map operation:
    --
    -- > map f m = S.concatMap (\x -> S.fromPure (f x)) m
    --
    -- Similarly, 'concatMap' can perform filtering by mapping an element to a
    -- 'nil' stream:
    --
    -- > filter p m = S.concatMap (\x -> if p x then S.fromPure x else S.nil) m
    --

    , concatMapWith
    , concatMap
    , concatMapM

    -- * Repeated Fold
    , foldMany -- XXX Rename to foldRepeat

    -- * Buffered Operations
    -- | Operations that require buffering of the stream.
    -- Reverse is essentially a left fold followed by unfold.
    , reverse

    -- * Multi-Stream folds
    -- | Operations that consume multiple streams at the same time.
    , eqBy
    , cmpBy
    , isPrefixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix

    -- * Exceptions
    -- | Most of these combinators inhibit stream fusion, therefore, when
    -- possible, they should be called in an outer loop to mitigate the cost.
    -- For example, instead of calling them on a stream of chars call them on a
    -- stream of arrays before flattening it to a stream of chars.
    --
    -- See also: "Streamly.Internal.Data.Stream.Exception" for
    -- @Pre-release@ functions.

    , onException
    , handle

    -- * Resource Management
    -- | 'bracket' is the most general operation, all other operations can be
    -- expressed using it.
    , before
    , after
    , finally
    , bracket

    -- * Lifting Inner Monad
    -- | See also: "Streamly.Internal.Data.Stream.Lift" for
    -- @Pre-release@ functions.

    , liftInner
    , runReaderT
    , runStateT

    -- -- * Stream Types
    -- $serial
    -- , Interleave
    -- , Zip
    )
where

import Streamly.Internal.Data.Stream
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, repeat, replicate, concatMap, span)
