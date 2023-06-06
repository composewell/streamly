{-# OPTIONS_GHC -Wno-deprecations #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Prelude
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
-- >>> import qualified Streamly.Prelude as Stream
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
-- The basic stream type is 'SerialT'. The type @SerialT IO a@ is an effectful
-- equivalent of a list @[a]@ using the IO monad.  Streams can be constructed
-- like lists, except that they use 'nil' instead of '[]' and 'cons' instead of
-- ':'.
--
-- `cons` constructs a pure stream which is more or less the same as a list:
--
-- >>> import Streamly.Prelude (SerialT, cons, consM, nil)
-- >>> stream = 1 `cons` 2 `cons` nil :: SerialT IO Int
-- >>> Stream.toList stream -- IO [Int]
-- [1,2]
--
-- 'consM' constructs a stream from effectful actions:
--
-- >>> stream = delay 1 `consM` delay 2 `consM` nil
-- >>> Stream.toList stream
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
--  Stream.repeatM getLine      -- SerialT IO String
--      & Stream.mapM putStrLn  -- SerialT IO ()
--      & Stream.drain          -- IO ()
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
--
-- == Polymorphic Combinators
--
-- Streamly has several stream types, 'SerialT' is one type of stream with
-- serial execution of actions, 'AsyncT' is another with concurrent execution.
-- The combinators in this module are polymorphic in stream type. For example,
--
-- @
-- repeatM :: (IsStream t, MonadAsync m) => m a -> t m a
-- @
--
-- @t@ is the stream type, @m@ is the underlying 'Monad' of the stream (e.g.
-- IO) and @a@ is the type of elements in the stream (e.g. Int).
--
-- Stream elimination combinators accept a 'SerialT' type instead of a
-- polymorphic type to force a concrete monomorphic type by default, reducing
-- type errors. That's why in the console echo example above the stream type is
-- 'SerialT'.
--
-- @
-- drain :: Monad m => SerialT m a -> m ()
-- @
--
-- We can force a certain stream type in polymorphic code by using "Stream Type
-- Adaptors". For example, to force 'AsyncT':
--
-- >>> Stream.drain $ Stream.fromAsync $ Stream.replicateM 10 $ delay 1
-- ...
--
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
-- >>> stream1 = Stream.fromListM [delay 3, delay 4]
-- >>> stream2 = Stream.fromListM [delay 1, delay 2]
-- >>> Stream.toList $ stream1 `parallel` stream2
-- ...
--
-- We can chain the operations to combine more than two streams:
--
-- >>> stream3 = Stream.fromListM [delay 1, delay 2]
-- >>> Stream.toList $ stream1 `parallel` stream2 `parallel` stream3
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
-- == Concurrent Nested Loops
--
-- The Monad instance of 'SerialT' is an example of nested looping. It is in
-- fact a list transformer. Different stream types provide different variants
-- of nested looping.  For example, the 'Monad' instance of 'ParallelT' uses
-- @concatMapWith parallel@ as its bind operation. Therefore, each iteration of
-- the loop for 'ParallelT' stream can run concurrently.  See the documentation
-- for individual stream types for the specific execution behavior of the
-- stream as well as the behavior of 'Semigroup' and 'Monad' instances.
--
-- == Stream Types
--
-- Streamly has several stream types. These types differ in three fundamental
-- operations, 'consM' ('IsStream' instance), '<>' ('Semigroup' instance) and
-- '>>=' ('Monad' instance).  Below we will see how 'consM' behaves for
-- 'SerialT', 'AsyncT' and 'AheadT' stream types.
--
-- 'SerialT' executes actions serially, so the total delay in the following
-- example is @2 + 1 = 3@ seconds:
--
-- >>> stream = delay 2 `consM` delay 1 `consM` nil
-- >>> Stream.toList stream -- IO [Int]
-- 2 sec
-- 1 sec
-- [2,1]
--
-- 'AsyncT' executes the actions concurrently, so the total delay is @max 2 1 =
-- 2@ seconds:
--
-- >>> Stream.toList $ Stream.fromAsync stream -- IO [Int]
-- 1 sec
-- 2 sec
-- [1,2]
--
-- 'AsyncT' produces the results in the order in which execution finishes.
-- Notice the order of elements in the list above, it is not the same as the
-- order of actions in the stream.
--
-- 'AheadT' is similar to 'AsyncT' but the order of results is the same as the
-- order of actions, even though they execute concurrently:
--
-- >>> Stream.toList $ Stream.fromAhead stream -- IO [Int]
-- 1 sec
-- 2 sec
-- [2,1]
--
-- == Semigroup Instance
--
-- Earlier we distinguished stream types based on the execution behavior of
-- actions within a stream. Stream types are also distinguished based on how
-- actions from different streams are scheduled for execution when two streams
-- are combined together.
--
-- For example, both 'SerialT' and 'WSerialT' execute actions within the stream
-- serially, however, they differ in how actions from individual streams are
-- executed when two streams are combined with '<>' (the 'Semigroup' instance).
--
-- For 'SerialT', '<>' has an appending behavior i.e. it executes the actions
-- from the second stream after executing actions from the first stream:
--
-- >>> stream1 = Stream.fromListM [delay 1, delay 2]
-- >>> stream2 = Stream.fromListM [delay 3, delay 4]
-- >>> Stream.toList $ stream1 <> stream2
-- 1 sec
-- 2 sec
-- 3 sec
-- 4 sec
-- [1,2,3,4]
--
-- For 'WSerialT', '<>' has an interleaving behavior i.e. it executes one
-- action from the first stream and then one action from the second stream and
-- so on:
--
-- >>> Stream.toList $ Stream.fromWSerial $ stream1 <> stream2
-- 1 sec
-- 3 sec
-- 2 sec
-- 4 sec
-- [1,3,2,4]
--
-- The '<>' operation of 'SerialT' and 'WSerialT' is the same as 'serial' and
-- 'wSerial' respectively. The 'serial' combinator combines two streams of any
-- type in the same way as a serial stream combines.
--
-- == Concurrent Combinators
--
-- Like 'consM', there are several other stream generation operations whose
-- execution behavior depends on the stream type, they all follow behavior
-- similar to 'consM'.
--
-- By default, folds like 'drain' force the stream type to be 'SerialT', so
-- 'replicateM' in the following code runs serially, and takes 10 seconds:
--
-- >>> Stream.drain $ Stream.replicateM 10 $ delay 1
-- ...
--
-- We can use the 'fromAsync' combinator to force the argument stream to be of
-- 'AsyncT' type, 'replicateM' in the following example executes the replicated
-- actions concurrently, thus taking only 1 second:
--
-- >>> Stream.drain $ Stream.fromAsync $ Stream.replicateM 10 $ delay 1
-- ...
--
-- We can use 'mapM' to map an action concurrently:
--
-- >>> f x = delay 1 >> return (x + 1)
-- >>> Stream.toList $ Stream.fromAhead $ Stream.mapM f $ Stream.fromList [1..3]
-- ...
-- [2,3,4]
--
-- 'fromAhead' forces mapM to happen in 'AheadT' style, thus all three actions
-- take only one second even though each individual action blocks for a second.
--
-- See the documentation of individual combinators to check if it is concurrent
-- or not. The concurrent combinators necessarily have a @MonadAsync m@
-- constraint. However, a @MonadAsync m@ constraint does not necessarily mean
-- that the combinator is concurrent.
--
-- == Automatic Concurrency Control
--
-- 'SerialT' (and 'WSerialT') runs all tasks serially whereas 'ParallelT' runs
-- all tasks concurrently i.e. one thread per task. The stream types 'AsyncT',
-- 'WAsyncT', and 'AheadT' provide demand driven concurrency. It means that
-- based on the rate at which the consumer is consuming the stream, it
-- maintains the optimal number of threads to increase or decrease parallelism.
--
-- However, the programmer can control the maximum number of threads using
-- 'maxThreads'. It provides an upper bound on the concurrent IO requests or
-- CPU cores that can be used. 'maxBuffer' limits the number of evaluated
-- stream elements that we can buffer.  See the "Concurrency Control" section
-- for details.
--
-- == Caveats
--
-- When we use combinators like 'fromAsync' on a piece of code, all combinators
-- inside the argument of fromAsync become concurrent which is often counter
-- productive.  Therefore, we recommend that in a pipeline, you identify the
-- combinators that you really want to be concurrent and add a 'fromSerial' after
-- those combinators so that the code following the combinator remains serial:
--
-- @
-- Stream.fromAsync $ ... concurrent combinator here ... $ Stream.fromSerial $ ...
-- @
--
-- == Conventions
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

module Streamly.Prelude
{-# DEPRECATED "Please use \"Streamly.Data.Stream.Prelude\" from streamly package and \"Streamly.Data.Fold\" from streamly-core package instead." #-}
    (
    -- * Construction
    -- | Functions ending in the general shape @b -> t m a@.
    --
    -- See also: "Streamly.Internal.Data.Stream.IsStream.Generate" for
    -- @Pre-release@ functions.

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

    -- ** From Generators
    -- | Generate a monadic stream from a seed value and a generator function.
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
    -- | Functions ending in the general shape @t m a -> m b@
    --
    -- See also: "Streamly.Internal.Data.Stream.IsStream.Eliminate" for
    -- @Pre-release@ functions.

    -- ** Running a 'Fold'
    -- $runningfolds

    , fold

    -- ** Deconstruction
    -- | Functions ending in the general shape @t m a -> m (b, t m a)@

    , uncons
    , tail
    , init

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

    -- *** Left Folds
    -- $leftfolds
    , foldl'
    , foldl1'
    , foldlM'

    -- ** Specific Folds
    -- *** Full Folds
    -- | Folds that are guaranteed to evaluate the whole stream.

    -- -- ** To Summary (Full Folds)
    -- -- | Folds that summarize the stream to a single value.
    , drain
    , last
    , length
    , sum
    , product

    -- -- ** To Summary (Maybe) (Full Folds)
    -- -- | Folds that summarize a non-empty stream to a 'Just' value and return
    -- 'Nothing' for an empty stream.
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the

    -- *** Partial Folds
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

    -- *** To Containers
    -- | Convert a stream into a container holding all the values.
    , toList

    -- ** Folding Concurrently
    , (|$.)
    , (|&.)

    -- ** Multi-Stream folds
    , eqBy
    , cmpBy
    , isPrefixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix

    -- * Transformation
    -- | See also: "Streamly.Internal.Data.Stream.IsStream.Transform" for
    -- @Pre-release@ functions.

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

    -- ** Mapping Side Effects
    , mapM_
    , trace
    , tap
    , delay

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
    , scanl1'
    , scanl1M'

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

    -- -- ** Searching Elements
    , findIndices
    , elemIndices

    -- ** Maybe Streams
    , mapMaybe
    , mapMaybeM

    -- * Concurrent Transformation
    -- ** Concurrent Pipelines
    -- $application
    , (|$)
    , (|&)
    , mkAsync

    -- ** Concurrency Control
    -- $concurrency
    , maxThreads
    , maxBuffer

    -- ** Rate Limiting
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- * Combining Streams
    -- | New streams can be constructed by appending, merging or zipping
    -- existing streams.
    --
    -- Any exceptions generated by concurrent evaluation are propagated to
    -- the consumer of the stream as soon as they occur.  Exceptions from a
    -- particular stream are guaranteed to arrive in the same order in the
    -- output stream as they were generated in the input stream.
    --
    -- See 'maxThreads' and 'maxBuffer' to control the concurrency of the
    -- concurrent combinators.
    --
    -- See also: "Streamly.Internal.Data.Stream.IsStream.Expand" for
    -- @Pre-release@ functions.

    -- ** Linear combinators
    -- | These functions have O(n) append performance.  They can be used
    -- efficiently with 'concatMapWith' et. al.

    , serial
    , wSerial
    , ahead
    , async
    , wAsync
    , parallel

    -- ** PairWise combinators
    -- | These functions have O(n^2) append performance when used linearly e.g.
    -- using 'concatMapWith'. However, they can be combined pair wise using
    -- 'Streamly.Internal.Data.Stream.IsStream.Expand.concatPairsWith' to give
    -- O(n * log n) complexity.

    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM

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

    -- ** Containers of Streams
    -- | These are variants of standard 'Foldable' fold functions that use a
    -- polymorphic stream sum operation (e.g. 'async' or 'wSerial') to fold a
    -- finite container of streams. Note that these are just special cases of
    -- the more general 'concatMapWith' operation.
    --
    , concatFoldableWith
    , concatMapFoldableWith
    , concatForFoldableWith

    -- * Reducing
    -- |
    -- See also: "Streamly.Internal.Data.Stream.IsStream.Reduce" for
    -- @Pre-release@ functions.

    -- ** Nested Folds
    , foldMany
    , chunksOf
    , intervalsOf

    -- ** Splitting
    -- | In general we can express splitting in terms of parser combinators.
    -- These are some common use functions for convenience and efficiency.
    -- While parsers can fail these functions are designed to transform a
    -- stream without failure with a predefined behavior for all cases.
    --
    -- In general, there are three possible ways of combining stream segments
    -- with a separator. The separator could be prefixed to each segment,
    -- suffixed to each segment, or it could be infixed between segments.
    -- 'intersperse' and 'intercalate' operations are examples of infixed
    -- combining whereas 'unlines' is an example of suffixed combining. When we
    -- split a stream with separators we can split in three different ways,
    -- each being an opposite of the three ways of combining.
    --
    -- Splitting may keep the separator or drop it. Depending on how we split,
    -- the separator may be kept attached to the stream segments in prefix or
    -- suffix position or as a separate element in infix position. Combinators
    -- like 'splitOn' that use @On@ in their names drop the separator and
    -- combinators that use 'With' in their names keep the separator. When a
    -- segment is missing it is considered as empty, therefore, we never
    -- encounter an error in parsing.

    -- -- ** Splitting By Elements
    , splitOn
    , splitOnSuffix

    , splitWithSuffix
    , wordsBy -- strip, compact and split

    -- ** Grouping
    -- | Splitting a stream by combining multiple contiguous elements into
    -- groups using some criterion.
    , groups
    , groupsBy
    , groupsByRolling

    -- * Exceptions
    -- | Most of these combinators inhibit stream fusion, therefore, when
    -- possible, they should be called in an outer loop to mitigate the cost.
    -- For example, instead of calling them on a stream of chars call them on a
    -- stream of arrays before flattening it to a stream of chars.
    --
    -- See also: "Streamly.Internal.Data.Stream.IsStream.Exception" for
    -- @Pre-release@ functions.

    , before
    , after
    , bracket
    , onException
    , finally
    , handle

    -- * Lifting Inner Monad
    -- | See also: "Streamly.Internal.Data.Stream.IsStream.Lift" for
    -- @Pre-release@ functions.

    , liftInner
    , runReaderT
    , runStateT

    -- * Stream Types
    -- | Stream types that end with a @T@ (e.g. 'SerialT') are monad
    -- transformers.

    -- ** Serial Streams
    -- $serial
    , SerialT
    , WSerialT

    -- ** Speculative Streams
    -- $ahead
    , AheadT

    -- ** Asynchronous Streams
    -- $async
    , AsyncT
    , WAsyncT
    , ParallelT

    -- ** Zipping Streams
    -- $zipping
    , ZipSerialM
    , ZipAsyncM

    -- * IO Streams
    , Serial
    , WSerial
    , Ahead
    , Async
    , WAsync
    , Parallel
    , ZipSerial
    , ZipAsync

    -- * Type Synonyms
    , MonadAsync

    -- * Converting from/to Stream/StreamK types
    , fromStream
    , toStream
    , fromStreamK
    , toStreamK

    -- * Stream Type Adapters
    -- $adapters
    , IsStream ()

    , fromSerial
    , fromWSerial
    , fromAsync
    , fromAhead
    , fromWAsync
    , fromParallel
    , fromZipSerial
    , fromZipAsync
    , adapt

    -- * Deprecated
    , yield
    , yieldM
    , scanx
    , foldx
    , foldxM
    , foldr1
    , runStream
    , runN
    , runWhile
    , fromHandle
    , toHandle
    , concatUnfold
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Prelude
       hiding (Foldable(..), filter, drop, dropWhile, take, takeWhile, zipWith,
               map, mapM, mapM_, sequence, all, any,
               notElem, head, last, tail,
               reverse, iterate, init, and, or, lookup, (!!),
               scanl, scanl1, repeat, replicate, concatMap, span)

import Streamly.Internal.Data.Stream.IsStream

-- $setup
-- >>> :set -fno-warn-deprecations
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Prelude as Stream

-- $serial
--
-- Serial streams are /spatially ordered/, they execute the actions in the
-- stream strictly one after the other.
--
-- There are two serial stream types 'SerialT' and 'WSerialT'.  They differ
-- only in the 'Semigroup' and 'Monad' instance behavior.

-- $ahead
--
-- Speculative streams evaluate some future actions speculatively and
-- concurrently, and keep the results ready for consumption. As in serial
-- streams, results are consumed in the same order as the actions in the
-- stream.
--
-- Even though the results of actions are ordered, the side effects are only
-- partially ordered.  Therefore, the semigroup operation is not commutative
-- from the pure outputs perspective but commutative from side effects
-- perspective.

-- $async
--
-- Asynchronous streams evaluate some future actions concurrently, the results
-- are given to the consumer as soon as they become available, they may not be
-- in the same order as the actions in the stream.
--
-- The results of the actions as well as their side effects are partially
-- ordered. Since the order of elements does not matter the 'Semigroup'
-- operation is effectively commutative.
--
-- There are two asynchronous stream types 'AsyncT' and 'WAsyncT'.  They differ
-- only in the 'Semigroup' and 'Monad' instance behavior.

-- $zipping
--
-- 'ZipSerialM' and 'ZipAsyncM', provide 'Applicative' instances for zipping the
-- corresponding elements of two streams together. Note that these types are
-- not monads.

-- $rightfolds
--
-- Let's take a closer look at the @foldr@ definition for lists, as given
-- earlier:
--
-- @
-- foldr f z (x:xs) = x \`f` foldr f z xs
-- @
--
-- @foldr@ invokes the fold step function @f@ as @f x (foldr f z xs)@. At each
-- invocation of @f@, @foldr@ gives us the next element in the input container
-- @x@ and a recursive expression @foldr f z xs@ representing the yet unbuilt
-- (lazy thunk) part of the output.
--
-- When @f x xs@ is lazy in @xs@ it can consume the input one element at a time
-- in FIFO order to build a lazy output expression. For example,
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
-- Therefore, @take 2 $ foldr f [] (1:2:undefined)@ would work without any
-- problem.
--
-- On the other hand, if @f a b@ is strict in @b@ it would end up consuming the
-- whole input right away and expanding the recursive expression @b@ (i.e.
-- @foldr f z xs@) fully before it yields an output expression, resulting in
-- the following /right associated expression/:
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
-- it reaches the terminating case @... `f` (1000 `f` 0)@, and then
-- start reducing the whole expression from right to left, therefore, consuming
-- the input elements in LIFO order. Thus, such an evaluation would require
-- memory proportional to the size of input. Try out @foldr (+) 0 (map (\\x ->
-- trace (show x) x) [1..10])@.
--
-- Notice the order of the arguments to the step function @f a b@. It follows
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
-- without going through the entire input, a left fold cannot terminate
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

-- $leftfolds
--
-- Note that the observations below about the behavior of a left fold assume
-- that we are working on a right associated structure like cons lists and
-- streamly streams. If we are working on a left associated structure (e.g.
-- snoc lists) the roles of right and left folds would reverse.
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
-- >>> Stream.foldl' (+) 0 $ Stream.fromList [1,2,3,4]
-- 10
--
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
-- >>> Stream.foldl' (flip (:)) [] $ Stream.fromList [1,2,3,4]
-- [4,3,2,1]
--
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

-- $runningfolds
--
-- See "Streamly.Data.Fold" for an overview of composable folds. All folds in
-- this module can be expressed in terms of composable folds using 'fold'.
--
-- $application
--
-- Stream processing functions can be composed in a chain using function
-- application with or without the '$' operator, or with reverse function
-- application operator '&'. Streamly provides concurrent versions of these
-- operators applying stream processing functions such that each stage of the
-- stream can run in parallel. The operators start with a @|@; we can read '|$'
-- as "@parallel dollar@" to remember that @|@ comes before '$'.

-- $concurrency
--
-- These combinators can be used at any point in a stream composition to set
-- parameters to control the concurrency of the /argument stream/.  A control
-- parameter set at any point remains effective for any concurrent combinators
-- used in the argument stream until it is reset by using the combinator again.
-- These control parameters have no effect on non-concurrent combinators in the
-- stream, or on non-concurrent streams.
--
-- /Pitfall:/ Remember that 'maxBuffer' in the following example applies to
-- 'mapM' and any other combinators that may follow it, and it does not apply
-- to the combinators before it:
--
-- @
--  ...
--  $ Stream.maxBuffer 10
--  $ Stream.mapM ...
--  ...
-- @
--
-- If we use '&' instead of '$' the situation will reverse, in the following
-- example, 'maxBuffer' does not apply to 'mapM', it applies to combinators
-- that come before it, because those are the arguments to 'maxBuffer':
--
-- @
--  ...
--  & Stream.maxBuffer 10
--  & Stream.mapM ...
--  ...
-- @

-- $adapters
--
-- You may want to use different stream composition styles at different points
-- in your program. Stream types can be freely converted or adapted from one
-- type to another.  The 'IsStream' type class facilitates type conversion of
-- one stream type to another. It is not used directly, instead the type
-- combinators provided below are used for conversions.
--
-- To adapt from one monomorphic type (e.g. 'AsyncT') to another monomorphic
-- type (e.g. 'SerialT') use the 'adapt' combinator. To give a polymorphic code
-- a specific interpretation or to adapt a specific type to a polymorphic type
-- use the type specific combinators e.g. 'fromAsync' or 'fromWSerial'. You
-- cannot adapt polymorphic code to polymorphic code, as the compiler would not know
-- which specific type you are converting from or to. If you see a an
-- @ambiguous type variable@ error then most likely you are using 'adapt'
-- unnecessarily on polymorphic code.
