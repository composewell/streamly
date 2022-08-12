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
-- For effectful streams we will use the following IO action that blocks for
-- @n@ seconds:
--
-- >>> import Control.Concurrent (threadDelay)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}
--
-- >>> delay 1
-- 1 sec
-- 1
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
-- >>> stream = delay 1 `consM` delay 2 `consM` nil
-- >>> Stream.fold Fold.toList stream
-- 1 sec
-- 2 sec
-- [1,2]
--
-- == Console Echo Program
--
-- In the following example, 'repeatM' generates an infinite stream of 'String'
-- by repeatedly performing the 'getLine' IO action. 'mapM' then applies
-- 'putStrLn' on each element in the stream converting it to stream of '()'.
-- Finally, 'drain' folds the stream to IO discarding the () values, thus
-- producing only effects.
--
-- >>> import Data.Function ((&))
--
-- @
-- > :{
--  Stream.repeatM getLine      -- Stream IO String
--      & Stream.mapM putStrLn  -- Stream IO ()
--      & Stream.fold Fold.drain          -- IO ()
-- :}
-- @
--
-- This is a console echo program. It is an example of a declarative loop
-- written using streaming combinators.  Compare it with an imperative @while@
-- loop.
--
-- Hopefully, this gives you an idea how we can program declaratively by
-- representing loops using streams. In this module, you can find all
-- "Data.List" like functions and many more powerful combinators to perform
-- common programming tasks. Also see "Streamly.Internal.Data.Stream.IsStream"
-- module for many more @Pre-release@ combinators. See the
-- <https://github.com/composewell/streamly-examples> repository for many more
-- real world examples of stream programming.
-- == Combining two streams
--
-- Two streams can be combined to form a single stream in various interesting
-- ways. 'serial' (append), 'wSerial' (interleave), 'ahead' (concurrent,
-- ordered append), 'async' (lazy concurrent, unordered append) , 'wAsync'
-- (lazy concurrent, unordered interleave), 'parallel' (strict concurrent
-- merge), 'zipWith', 'zipAsyncWith' (concurrent zip), 'mergeBy',
-- 'mergeAsyncBy' (concurrent merge) are some ways of combining two streams.
--
-- For example, the `parallel` combinator schedules both the streams
-- concurrently.
--
-- >>> stream1 = Stream.mapM id $ Stream.fromList [delay 3, delay 4]
-- >>> stream2 = Stream.mapM id $ Stream.fromList [delay 1, delay 2]
-- >>> Stream.fold Fold.toList $ stream1 `parallel` stream2
-- ...
--
-- We can chain the operations to combine more than two streams:
--
-- >>> stream3 = Stream.mapM id $ Stream.fromList [delay 1, delay 2]
-- >>> Stream.fold Fold.toList $ stream1 `parallel` stream2 `parallel` stream3
-- ...
--
-- Concurrent generation ('consM') and concurrent merging of streams is the
-- fundamental basis of all concurrency in streamly.
--
-- == Combining many streams
--
-- The 'concatMapWith' combinator can be used to generalize the two stream
-- combining combinators to @n@ streams.  For example, we can use
-- @concatMapWith parallel@ to read concurrently from all incoming network
-- connections and combine the input streams into a single output stream:
--
-- @
-- import qualified Streamly.Network.Inet.TCP as TCP
-- import qualified Streamly.Network.Socket as Socket
--
-- Stream.unfold TCP.acceptOnPort 8090
--  & Stream.concatMapWith Stream.parallel (Stream.unfold Socket.read)
-- @
--
-- See the @streamly-examples@ repository for a full working example.
--
-- == Semigroup Instance
--
-- Earlier we distinguished stream types based on the execution behavior of
-- actions within a stream. Stream types are also distinguished based on how
-- actions from different streams are scheduled for execution when two streams
-- are combined together.
--

-- For 'Stream', '<>' has an appending behavior i.e. it executes the actions
-- from the second stream after executing actions from the first stream:
--
-- >>> stream1 = Stream.mapM id $ Stream.fromList [delay 1, delay 2]
-- >>> stream2 = Stream.mapM id $ Stream.fromList [delay 3, delay 4]
-- >>> Stream.fold Fold.toList $ stream1 <> stream2
-- 1 sec
-- 2 sec
-- 3 sec
-- 4 sec
-- [1,2,3,4]
--
-- By default, folds like 'drain' force the stream type to be 'Stream', so
-- 'replicateM' in the following code runs serially, and takes 10 seconds:
--
-- >>> Stream.fold Fold.drain $ Stream.replicateM 10 $ delay 1
-- ...
--
module Streamly.Data.Stream
    (
      Stream
    -- * Construction
    -- | Functions ending in the general shape @b -> Stream m a@.
    --
    -- ** Primitives
    -- | Primitives to construct a stream from pure values or monadic actions.
    -- All other stream construction and generation combinators described later
    -- can be expressed in terms of these primitives. However, the special
    -- versions provided in this module can be much more efficient in most
    -- cases. Users can create custom combinators using these primitives.
    , nil
    , cons
    , consM

    -- ** Unfolding
    -- | Generalized way of generating a stream efficiently.
    , unfold
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

    -- * Elimination
    -- | Functions ending in the general shape @Stream m a -> m b@
    --
    -- ** Running a 'Fold'
    -- $runningfolds
    , fold

    -- ** Deconstruction
    -- | Functions ending in the general shape @Stream m a -> m (b, Stream m a)@
    , uncons

    -- ** General Folds
-- | In imperative terms a fold can be considered as a loop over the stream
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

-- As a general rule, foldr cannot have state and foldl cannot have control.

-- NOTE: Folds are inherently serial as each step needs to use the result of
-- the previous step. However, it is possible to fold parts of the stream in
-- parallel and then combine the results using a monoid.

    -- *** Right Folds
    -- $rightfolds
    , foldrM
    , foldr

    -- ** Multi-Stream folds
    , eqBy
    , cmpBy
    , isPrefixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix

    -- * Transformation
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
    , sequence
    , mapM

    -- ** Mapping Side Effects
    , trace
    , tap
    , delay

    -- ** Scanning By 'Fold'
    , scan
    , postscan

    -- ** Filtering
    -- | Remove some elements from the stream based on a predicate. In
    -- imperative terms a filter over a stream corresponds to a loop with a
    -- @continue@ clause for the cases when the predicate fails.
    , deleteBy
    , filter
    , filterM
    , uniq

    -- ** Trimming
    -- | Take or remove elements from one or both ends of a stream.
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM

    -- ** Inserting Elements
    -- | Inserting elements is a special case of interleaving/merging streams.
    , insertBy
    , intersperseM
    , intersperse

    -- ** Reordering Elements
    , reverse

    -- ** Indexing
    -- | Indexing can be considered as a special type of zipping where we zip a
    -- stream with an index stream.
    , indexed
    , indexedR

    -- ** Searching
    -- | Finding the presence or location of an element, a sequence of elements
    -- or another stream within a stream.

    -- ** Searching Elements
    , findIndices
    , elemIndices

    -- ** Maybe Streams
    , mapMaybe
    , mapMaybeM

    -- ** PairWise combinators
    -- | These functions have O(n^2) append performance when used linearly e.g.
    -- using 'concatMapWith'. However, they can be combined pair wise using
    -- 'Streamly.Internal.Data.Stream.IsStream.Expand.concatPairsWith' to give
    -- O(n * log n) complexity.
    , mergeBy
    , mergeByM
    , zipWith
    , zipWithM

    -- ** Nested Unfolds
    , unfoldMany
    , intercalate
    , intercalateSuffix

    -- ** Nested Streams
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

    -- * Nested Folds
    , foldMany

    -- * Exceptions
    -- | Most of these combinators inhibit stream fusion, therefore, when
    -- possible, they should be called in an outer loop to mitigate the cost.
    -- For example, instead of calling them on a stream of chars call them on a
    -- stream of arrays before flattening it to a stream of chars.
    , before
    , after
    , bracket
    , onException
    , finally
    , handle

    -- * Lifting Inner Monad
    , liftInner
    , runReaderT
    , runStateT
    )
where

import Streamly.Internal.Data.Stream
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, repeat, replicate, concatMap, span)
