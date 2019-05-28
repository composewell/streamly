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

    , foldrM
    , foldrS
    , foldrT
    , foldr

    , foldl'
    , foldl1'
    , foldlM'

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

    , scanl'
    , scanlM'
    -- , postscanl'
    -- , postscanlM'
    -- , prescanl'
    -- , prescanlM'
    , scanl1'
    , scanl1M'

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
    -- , intersperseBySpan
    , intersperseByTime

    -- ** Reordering
    , reverse
    , reverse'

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
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Maybe (isJust, fromJust)
import Foreign.Storable (Storable)
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap)

import qualified Prelude
import qualified System.IO as IO

import Streamly.Enumeration (Enumerable(..), enumerate, enumerateTo)
import Streamly.SVar (MonadAsync, defState)
import Streamly.Streams.Async (mkAsync')
import Streamly.Streams.Combinators (maxYields)
import Streamly.Streams.Prelude
       (fromStreamS, toStreamS, foldWith, foldMapWith, forEachWith)
import Streamly.Streams.StreamD (fromStreamD, toStreamD)
import Streamly.Streams.StreamK (IsStream(..))
import Streamly.Streams.Serial (SerialT)

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
fromIndices = fromStreamD . D.fromIndices

-- XXX this needs to be concurrent
--
-- |
-- @
-- fromIndicesM f = let g i = f i \`consM` g (i + 1) in g 0
-- @
--
-- Generate an infinite stream, whose values are the output of a monadic
-- function @f@ applied on the corresponding index. Index starts at 0.
--
-- @since 0.6.0
{-# INLINE fromIndicesM #-}
fromIndicesM :: (IsStream t, Monad m) => (Int -> m a) -> t m a
fromIndicesM = fromStreamD . D.fromIndicesM

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
-- iterateM f m = m \`consM` iterateM f m
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
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) 0
--
-- drain $ asyncly  $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) 0
-- @
--
-- /Concurrent/
--
-- @since 0.1.2
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> a -> t m a
iterateM step = go
    where
    go s = K.mkStream $ \st stp sng yld -> do
       next <- step s
       K.foldStreamShared st stp sng yld (return s |: go next)

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
{-# DEPRECATED fromHandle "Please use Streamly.FileIO instead." #-}
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
-- @since 0.7.0
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
-- Specialized folds
------------------------------------------------------------------------------

-- |
-- > drain = mapM_ (\_ -> return ())
--
-- Run a stream, discarding the results. By default it interprets the stream
-- as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @drain . 'asyncly'@.
--
-- @since 0.2.0
{-# INLINE drain #-}
drain :: Monad m => SerialT m a -> m ()
drain = P.drain

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

{-# DEPRECATED runN "Please use \"drainN\" instead" #-}
{-# INLINE runN #-}
runN :: Monad m => Int -> SerialT m a -> m ()
runN = drainN

-- |
-- > drainWhile p = drain . takeWhile p
--
-- Run a stream as long as the predicate holds true.
--
-- @since 0.6.0
{-# INLINE drainWhile #-}
drainWhile :: Monad m => (a -> Bool) -> SerialT m a -> m ()
drainWhile p = drain . takeWhile p

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
{-# DEPRECATED toHandle "Please use Streamly.FileIO instead." #-}
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
toHandle h m = go m
    where
    go m1 =
        let stop = return ()
            single a = liftIO (IO.hPutStrLn h a)
            yieldk a r = liftIO (IO.hPutStrLn h a) >> go r
        in K.foldStream defState yieldk single stop m1

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
-- @since 0.2.0
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

-- XXX enable once the signature (monadic zero) change is settled
-- | Like scanl' but does not stream the initial value of the accumulator.
--
-- > postscanl' f z xs = S.drop 1 $ scanl' f z xs
--
-- @since 0.6.0
{-# INLINE _postscanl' #-}
_postscanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
_postscanl' step z m = fromStreamD $ D.postscanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like postscanl' but with a monadic step function.
--
-- @since 0.6.0
{-# INLINE _postscanlM' #-}
_postscanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> t m b
_postscanlM' step z m = fromStreamD $ D.postscanlM' step z $ toStreamD m

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
-- |
-- > reverse = S.foldlT (flip S.cons) S.nil
--
-- Returns the elements of the stream in reverse order.  The stream must be
-- finite. Note that this necessarily buffers the entire stream in memory.
--
-- /Note:/ 'reverse'' is much faster than this, use that when performance
-- matters.
--
-- @since 0.1.1
{-# INLINE reverse #-}
reverse :: (IsStream t, Monad m) => t m a -> t m a
reverse s = fromStreamS $ S.reverse $ toStreamS s

-- | Like 'reverse' but several times faster, requires a 'Storable' instance.
--
-- @since 0.7.0
{-# INLINE reverse' #-}
reverse' :: (IsStream t, MonadIO m, Storable a) => t m a -> t m a
reverse' s = fromStreamD $ D.reverse' $ toStreamD s

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
intersperseM = K.intersperseM

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
