{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Stream
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Fast, composable stream producers with ability to terminate, supporting
-- stream fusion.
--
-- Please refer to "Streamly.Internal.Data.Stream" for more functions that have
-- not yet been released.
--
-- For continuation passing style (CPS) stream type, please refer to
-- the "Streamly.Data.StreamK" module.
--
-- Checkout the <https://github.com/composewell/streamly-examples>
-- repository for many more real world examples of stream programming.

module Streamly.Data.Stream
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Overview
    -- $overview

    -- * The Stream Type
      Stream

    -- * Construction
    -- | Functions ending in the general shape @b -> Stream m a@.
    --
    -- See also: "Streamly.Internal.Data.Stream.Generate" for
    -- @Pre-release@ functions.
    --
    -- Useful Idioms:
    --
    -- >>> fromIndices f = fmap f $ Stream.enumerateFrom 0
    -- >>> fromIndicesM f = Stream.mapM f $ Stream.enumerateFrom 0
    -- >>> fromListM = Stream.sequence . Stream.fromList
    -- >>> fromFoldable = StreamK.toStream . StreamK.fromFoldable
    -- >>> fromFoldableM = Stream.sequence . fromFoldable

    -- ** Primitives
    -- | A fused 'Stream' is never constructed using 'cons', we always convert
    -- other type of containers like list into streams, or generate it using
    -- custom functions provided in this module. The 'cons' primitive has a
    -- rare use in fusing a small number of elements. On the other hand, it is
    -- common to construct 'StreamK' stream using the 'StreamK.cons' primitive.
    , nil
    , nilM
    , cons
    , consM

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

    -- ** Iteration
    , iterate
    , iterateM

    -- ** From Containers
    -- | Convert an input structure, container or source into a stream. All of
    -- these can be expressed in terms of primitives.
    , fromList

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

    -- ** Strict Left Folds
    -- XXX Need to have a general parse operation here which can be used to
    -- express all others.
    , fold -- XXX rename to run? We can have a Stream.run and Fold.run.
    -- XXX fold1 can be achieved using Monoids or Refolds.
    -- XXX We can call this just "break" and parseBreak as "munch"
    , foldBreak

    -- XXX should we have a Fold returning function in stream module?
    -- , foldAdd
    -- , buildl

    -- ** Parsing
    , parse
    -- , parseBreak

    -- ** Lazy Right Folds
    -- | Consuming a stream to build a right associated expression, suitable
    -- for lazy evaluation. Evaluation of the input happens when the output of
    -- the fold is evaluated, the fold output is a lazy thunk.
    --
    -- This is suitable for stream transformation operations, for example,
    -- operations like mapping a function over the stream.
    , foldrM
    , foldr

    -- ** Specific Folds
    -- | Usually you can use the folds in "Streamly.Data.Fold". However, some
    -- folds that may be commonly used or may have an edge in performance in
    -- some cases are provided here.
    -- , drain
    , toList

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
    , trace
    , tap
    , delay

    -- * Scanning
    -- | Stateful one-to-one transformations.
    --
    -- See also: "Streamly.Internal.Data.Stream.Transform" for
    -- @Pre-release@ functions.

    {-
    -- ** Left scans
    -- | We can perform scans using folds with the 'scan' combinator in the
    -- next section. However, the combinators supplied in this section are
    -- better amenable to stream fusion when combined with other operations.
    -- Note that 'postscan' using folds fuses well and does not require custom
    -- combinators like these.
    , scanl'
    , scanlM'
    , scanl1'
    , scanl1M'
    -}

    -- ** Scanning By 'Fold'
    -- | Useful idioms:
    --
    -- >>> scanl' f z = Stream.scan (Fold.foldl' f z)
    -- >>> scanlM' f z = Stream.scan (Fold.foldlM' f z)
    -- >>> postscanl' f z = Stream.postscan (Fold.foldl' f z)
    -- >>> postscanlM' f z = Stream.postscan (Fold.foldlM' f z)
    -- >>> scanl1' f = Stream.catMaybes . Stream.scan (Fold.foldl1' f)
    -- >>> scanl1M' f = Stream.catMaybes . Stream.scan (Fold.foldlM1' f)
    , scan
    , postscan
    -- XXX postscan1 can be implemented using Monoids or Refolds.

    -- ** Specific scans
    -- Indexing can be considered as a special type of zipping where we zip a
    -- stream with an index stream.
    , indexed

    -- * Insertion
    -- | Add elements to the stream.

    -- Inserting elements is a special case of interleaving/merging streams.
    , insertBy
    , intersperseM
    , intersperseM_
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

    -- Filter and concat
    , catMaybes
    , catLefts
    , catRights
    , catEithers

    -- ** Stateful Filters
    -- | 'scanMaybe' is the most general stateful filtering operation. The
    -- filtering folds (folds returning a 'Maybe' type) in
    -- "Streamly.Internal.Data.Fold" can be used along with 'scanMaybe' to
    -- perform stateful filtering operations in general.
    --
    -- Useful idioms:
    --
    -- >>> deleteBy cmp x = Stream.scanMaybe (Fold.deleteBy cmp x)
    -- >>> findIndices p = Stream.scanMaybe (Fold.findIndices p)
    -- >>> elemIndices a = findIndices (== a)
    -- >>> uniq = Stream.scanMaybe (Fold.uniqBy (==))
    , scanMaybe
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM

    -- XXX These are available as scans in folds. We need to check the
    -- performance though. If these are common and we need convenient stream
    -- ops then we can expose these.

    -- , deleteBy
    -- , uniq
    -- , uniqBy

    -- -- ** Sampling
    -- , strideFromThen

    -- -- ** Searching
    -- Finding the presence or location of an element, a sequence of elements
    -- or another stream within a stream.

    -- -- ** Searching Elements
    -- , findIndices
    -- , elemIndices

    -- * Combining Two Streams
    -- ** Appending
    , append

    -- ** Interleaving
    -- | When interleaving more than two streams you may want to interleave
    -- them pairwise creating a balanced binary merge tree.
    , interleave

    -- ** Merging
    -- | When merging more than two streams you may want to merging them
    -- pairwise creating a balanced binary merge tree.
    --
    -- Merging of @n@ streams can be performed by combining the streams pair
    -- wise using 'mergeMapWith' to give O(n * log n) time complexity. If used
    -- with 'concatMapWith' it will have O(n^2) performance.

    , mergeBy
    , mergeByM

    -- ** Zipping
    -- | When zipping more than two streams you may want to zip them
    -- pairwise creating a balanced binary tree.
    --
    -- Zipping of @n@ streams can be performed by combining the streams pair
    -- wise using 'mergeMapWith' with O(n * log n) time complexity. If used
    -- with 'concatMapWith' it will have O(n^2) performance.
    , zipWith
    , zipWithM
    -- , ZipStream (..)

    -- ** Cross Product
    -- XXX The argument order in this operation is such that it seems we are
    -- transforming the first stream using the second stream because the second
    -- stream is evaluated many times or buffered and better be finite, first
    -- stream could potentially be infinite. In the tradition of using the
    -- transformed stream at the end we can have a flipped version called
    -- "crossMap" or "nestWith".
    , crossWith
    -- , cross
    -- , joinInner
    -- , CrossStream (..)

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

    , concatEffect
    , concatMap
    , concatMapM

    -- * Repeated Fold
    -- | Useful idioms:
    --
    -- >>> splitWithSuffix p f = Stream.foldMany (Fold.takeEndBy p f)
    -- >>> splitOnSuffix p f = Stream.foldMany (Fold.takeEndBy_ p f)
    -- >>> groupsBy eq f = Stream.parseMany (Parser.groupBy eq f)
    -- >>> groupsByRolling eq f = Stream.parseMany (Parser.groupByRolling eq f)
    -- >>> wordsBy p f = Stream.parseMany (Parser.wordBy p f)
    -- >>> groupsOf n f = Stream.foldMany (Fold.take n f)
    , foldMany -- XXX Rename to foldRepeat
    , parseMany
    , Array.chunksOf

    -- * Buffered Operations
    -- | Operations that require buffering of the stream.
    -- Reverse is essentially a left fold followed by an unfold.
    , reverse

    -- * Multi-Stream folds
    -- | Operations that consume multiple streams at the same time.
    , eqBy
    , cmpBy
    , isPrefixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix

    -- Exceptions and resource management depend on the "exceptions" package
    -- XXX We can have IO Stream operations not depending on "exceptions"
    -- in Exception.Base

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
    -- | 'bracket' is the most general resource management operation, all other
    -- operations can be expressed using it. These functions have IO suffix
    -- because the allocation and cleanup functions are IO actions. For
    -- generalized allocation and cleanup functions see the functions without
    -- the IO suffix in the "streamly" package.
    , before
    , afterIO
    , finallyIO
    , bracketIO
    , bracketIO3

    -- * Transforming Inner Monad

    , morphInner
    , liftInner
    , runReaderT
    , runStateT

    -- -- * Stream Types
    -- $serial
    -- , Interleave
    -- , Zip
    )
where

import qualified Streamly.Internal.Data.Array.Type as Array
import Streamly.Internal.Data.Stream.StreamD
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, map, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, repeat, replicate, concatMap, span)

#include "DocTestDataStream.hs"

-- $overview
--
-- Streamly is a framework for modular data flow based programming and
-- declarative concurrency.  Powerful stream fusion framework in streamly
-- allows high performance combinatorial programming even when using byte level
-- streams.  Streamly API is similar to Haskell lists.
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
--  Stream.repeatM getLine       -- Stream IO String
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
-- == Stream Fusion
--
-- The fused 'Stream' type employs stream fusion for C-like performance when
-- looping over data. It represents a stream source or transformation by
-- defining a state machine with explicit state, and a step function working on
-- the state. A typical stream operation consumes elements from the previous
-- state machine in the pipeline, transforms them and yields new values for the
-- next stage to consume. The stream operations are modular and represent a
-- single task, they have no knowledge of previous or next operation on the
-- elements.
--
-- A typical stream pipeline consists of a stream producer, several stream
-- transformation operations and a stream consumer. All these operations taken
-- together form a closed loop processing the stream elements. Elements are
-- transferred between stages using a boxed data constructor. However, all the
-- stages of the pipeline are fused together by GHC, eliminating the
-- intermediate constructors, and thus forming a tight C like loop without any
-- boxed data being used in the loop.
--
-- Stream fusion works effectively when:
--
-- * the stream pipeline is composed statically (known at compile time)
-- * all the operations forming the loop are inlined
-- * the loop is not recursively defined, recursion breaks inlining
--
-- If these conditions cannot be met, the CPS style stream type 'StreamK' may
-- turn out to be a better choice than the fused stream type 'Stream'.
--
-- == Stream vs StreamK
--
-- The fused stream model avoids constructor allocations or function call
-- overheads. However, the stream is represented as a state machine and to
-- generate elements it has to navigate the decision tree of the state machine.
-- Moreover, the state machine is cranked for each element in the stream. This
-- performs extremely well when the number of states are limited. The state
-- machine starts getting expensive as the number of states increase. For
-- example, generating a million element stream from a list requires a single
-- state and is very efficient. However, using fused 'cons' to generate a
-- million element stream would be a disaster.
--
-- A typical worst case scenario for fused stream model is a large number of
-- `cons` or `append` operations. A few static `cons` or `append` operations
-- are very fast and much faster than a CPS style stream. However, if we
-- construct a large stream using `cons` it introduces as many states in the
-- state machine as the number of elements. If we compose the `cons` as a
-- binary tree it will take @n * log n@ time to navigate the tree, and @n * n@
-- if it is a right associative composition.
--
-- For quadratic cases of fused stream, after a certain threshold the CPS
-- stream would perform much better and exhibit linear performance behavior.
-- Operations like 'cons' or 'append'; are typically recursively called to
-- construct a lazy infinite stream. For such use cases the CPS style 'StreamK'
-- type is provided. CPS streams do not have a state machine that needs to be
-- cranked for each element, past state has no effect on the future element
-- processing. However, it incurs a function call overhead for each operation
-- for each element, which could be very large overhead compared to fused state
-- machines even if it has many states and cranks it for each element. But in
-- some cases scales tip in favor of the CPS stream. In those cases even though
-- CPS has a large constant overhead, it has a linear performance rather than
-- quadratic.
--
-- As a general guideline, if you have to use 'cons' or 'append' or operations
-- of similar nature, at a large scale, then 'StreamK' should be used. When you
-- need to compose the stream dynamically or recursively, then 'StreamK' should
-- be used. Typically you would use a dynamically generated 'StreamK' with
-- chunks of data which can then be processed by statically fused stream
-- pipeline operations.
--
-- 'Stream' and 'StreamK' types can be interconverted. See
-- "Streamly.Data.StreamK" module for conversion operations.
