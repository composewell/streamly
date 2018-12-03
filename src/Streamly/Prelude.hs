{-# LANGUAGE CPP                       #-}
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

    -- ** Enumeration
    -- | The haskell 'Enum' typeclass provides enumerations for member types.
    -- However, the typeclass is tied to lists, it is impossible to use it to
    -- write enumeration functions producing something other than a list in a
    -- generic way.
    -- We can use the 'Enum' facilities to enumerate the type producing a list
    -- and then convert it to a stream:
    --
    -- @
    -- 'fromList' $ 'Prelude.enumFromThen' from then
    -- @
    --
    -- However, this is not particularly efficient.  Note that using
    -- enumeration functions e.g. 'Prelude.enumFromThen' is slightly faster
    -- than the idioms like @[from, then..]@.
    --
    -- For the cases where speed matters, we have provided specific 'Enum' like
    -- implementations for enumeration.
    --
    , intFrom
    , intFromTo
    , intFromThen
    , intFromThenTo
    , intFromStep

    , fracFrom
    , fracFromTo
    , fracFromThen
    , fracFromThenTo
    , numFromStep

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

    -- ** From External Containers
    , fromHandle

    -- * Elimination

    -- ** Primitives
    -- | It is easy to express all the folds in terms of the 'uncons' primitive,
    -- however the specific implementations provided later are generally more
    -- efficient.  Folds are inherently serial as each step needs to use the
    -- result of the previous step.
    , uncons

    -- ** General Folds
-- | Right and left folds.
-- As a simple rule, always use lazy right fold for construction and strict
-- left fold for reduction. By construction we mean using a constructor as the
-- outermost operation in the fold function, by reduction we mean using a
-- function as the outermost operation in the fold function.
--
-- +-----------------------------------+--------------------------------------+
-- | Right Fold                        | Left Fold                            |
-- +===================================+======================================+
-- | Construction consumes input       | Construction consumes all input,     |
-- | lazily and streams it in FIFO     | and constructs in reverse (LIFO)     |
-- | order                             | order                                |
-- +-----------------------------------+--------------------------------------+
-- | Reduction ends up buffering all   | Strict reduction works               |
-- | input before it can be reduced    | incrementally, without buffering.    |
-- +-----------------------------------+--------------------------------------+
--
-- Almost always, we need lazy construction and strict reduction, therefore,
-- strict @foldr@ and lazy @foldl@ are rarely useful. If needed, strict @foldr@
-- and lazy @foldl@ can be expressed in terms of the available versions.  For
-- example, a lazy @foldl@ can be replaced by a strict @foldl@ to reverse the
-- structure followed by a @foldr@.
--
-- The following equations may help understand the relation between the two
-- folds for lists:
--
-- @
-- foldr f z xs = foldl (flip f) z (reverse xs)
-- foldl f z xs = foldr (flip f) z (reverse xs)
-- @
--
-- More generally:
--
-- @
-- foldl f z xs = foldr g id xs z where g x k = k . flip f x
-- foldr f z xs = foldl g id xs z where g k x = k . f x
-- @

    , foldr
    , foldr1
    , foldrM
    , foldl'
    , foldl1'
    , foldlM'
    , foldx
    , foldxM

    -- ** To Elements
    -- | Folds that extract selected elements of a stream or their properties.
    , (!!)
    , head
    , last
    , findM
    , find
    , lookup
    , findIndex
    , elemIndex

    -- ** To Parts
    -- | Folds that extract selected parts of a stream.
    , tail
    , init

    -- ** To Boolean
    -- | Folds that summarize the stream to a boolean value.
    , null
    , elem
    , notElem
    , all
    , any
    , and
    , or

    -- ** To Summary
    -- | Folds that summarize the stream to a single value.
    , length
    , sum
    , product

    -- ** To Summary (Maybe)
    -- | Folds that summarize a non-empty stream to a 'Just' value and return
    -- 'Nothing' for an empty stream.
    , maximumBy
    , maximum
    , minimumBy
    , minimum
    , the

    -- ** To Containers
    -- | Convert or divert a stream into an output structure, container or
    -- sink.
    , toList
    , toHandle

    -- * Transformation

    -- ** Scanning
    -- | Scans stream all the intermediate reduction steps of the corresponding
    -- folds. The following equations hold for lists:
    --
    -- > scanl f z xs == map (foldl f z) $ inits xs
    -- > scanr f z xs == map (foldr f z) $ tails
    --
    -- We do not provide a right associative scan, it can be recovered from a
    -- 'scanl'' as follows:
    --
    -- > scanr f z xs ==  reverse $ scanl' (flip f) z (reverse xs)
    --
    -- Scan is like a stateful map. If we discard the state, we get the map:
    --
    -- > S.drop 1 $ S.scanl' (\_ x -> f x) z xs == map f xs

    -- > S.postscanl' (\_ x -> f x) z xs == map f xs

    , scanl'
    , scanlM'
    -- , postscanl'
    -- , postscanlM'
    -- , prescanl'
    -- , prescanlM'
    , scanl1'
    , scanl1M'
    , scanx

    -- ** Mapping
    -- | Map is a strictly one-to-one transformation of stream elements. It
    -- cannot add or remove elements from the stream, just transforms them.
    , Serial.map

    -- ** Flattening
    , sequence
    , mapM

    -- ** Filtering
    -- | Filtering may remove some elements from the stream.

    , filter
    , filterM
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM
    , deleteBy
    , uniq

    -- ** Insertion
    -- | Insertion adds more elements to the stream.

    , insertBy
    , intersperseM

    -- ** Reordering
    , reverse

    -- * Hybrid Operations
    -- ** Map and Fold
    , mapM_

    -- ** Map and Filter
    , mapMaybe
    , mapMaybeM

    -- ** Scan and filter
    , findIndices
    , elemIndices

    -- * Multi-Stream Operations
    -- | New streams can be constructed by appending, merging or zipping
    -- existing streams.

    -- ** Appending
    -- | Streams form a 'Semigroup' and a 'Monoid' under the append
    -- operation.
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

    -- , merge
    , mergeBy
    , mergeByM
    , mergeAsyncBy
    , mergeAsyncByM

    -- ** Zipping
    , zipWith
    , zipWithM
    , Z.zipAsyncWith
    , Z.zipAsyncWithM

    -- Special zips
    , indexed
    , indexedR

    -- ** Flattening
    , concatMapM
    , concatMap

    -- ** Folds
    , eqBy
    , cmpBy
    , isPrefixOf
    , isSubsequenceOf
    , stripPrefix

    -- * Deprecated
    , K.once
    , each
    , scan
    , foldl
    , foldlM
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (isJust, fromJust)
import Prelude
       hiding (filter, drop, dropWhile, take, takeWhile, zipWith, foldr,
               foldl, mapM, mapM_, sequence, all, any, sum, product, elem,
               notElem, maximum, minimum, head, last, tail, length, null,
               reverse, iterate, init, and, or, lookup, foldr1, (!!),
               scanl, scanl1, replicate, concatMap)
import qualified Prelude
import qualified System.IO as IO

import Streamly.SVar (MonadAsync, defState, rstState)
import Streamly.Streams.Async (mkAsync')
import Streamly.Streams.Combinators (maxYields)
import Streamly.Streams.Prelude (fromStreamS, toStreamS)
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

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- @since 0.1.0
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
-- in runStream $ unfoldrM f 0
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

-- | @intFromStep from step@ generates an infinite stream whose first element
-- is @from@ and the successive elements are in increments of @step@. This does
-- not check for overflow or underflow if the 'Integral' type is bounded.
--
-- @
-- > S.toList $ S.take 4 $ S.intFromStep 0 2
-- [0,2,4,6]
-- > S.toList $ S.take 3 $ S.intFromStep 0 (-2)
-- [0,-2,-4]
-- @
--
-- @since 0.6.0
{-# INLINE intFromStep #-}
intFromStep :: (IsStream t, Monad m, Integral a) => a -> a -> t m a
intFromStep from stride = fromStreamD $ D.intFromStep from stride

-- | Enumerate an 'Integral' type. @intFrom from@ generates a stream whose
-- first element is @from@ and the successive elements are in increments of
-- @1@. The stream is bounded by the size of the 'Integral' type.
--
-- @
-- > S.toList $ S.take 4 $ S.intFrom (0 :: Int)
-- [0,1,2,3]
-- @
--
-- @since 0.6.0
{-# INLINE intFrom #-}
intFrom :: (IsStream t, Monad m, Integral a, Bounded a) => a -> t m a
intFrom from = fromStreamD $ D.intFrom from

-- | Enumerate an 'Integral' type in steps. @intFromThen from then@ generates a
-- stream whose first element is @from@, the second element is @then@ and the
-- successive elements are in increments of @then - from@.  The stream is
-- bounded by the size of the 'Integral' type.
--
-- @
-- > S.toList $ S.take 4 $ S.intFromThen (0 :: Int) 2
-- [0,2,4,6]
-- > S.toList $ S.take 4 $ S.intFromThen (0 :: Int) (-2)
-- [0,-2,-4,-6]
-- @
--
-- @since 0.6.0
{-# INLINE intFromThen #-}
intFromThen :: (IsStream t, Monad m, Integral a, Bounded a) => a -> a -> t m a
intFromThen from next = fromStreamD $ D.intFromThen from next

-- | Enumerate an 'Integral' type up to a given limit.  @intFromTo from to@
-- generates a finite stream whose first element is @from@ and successive
-- elements are in increments of @1@ up to @to@.
--
-- @
-- > S.toList $ S.intFromTo 0 4
-- [0,1,2,3,4]
-- @
--
-- @since 0.6.0
{-# INLINE intFromTo #-}
intFromTo :: (IsStream t, Monad m, Integral a) => a -> a -> t m a
intFromTo from to = fromStreamD $ D.intFromTo from to

-- | Enumerate an 'Integral' type in steps up to a given limit.  @intFromThenTo
-- from then to@ generates a finite stream whose first element is @from@, the
-- second element is @then@ and the successive elements are in increments of
-- @then - from@ up to @to@.
--
-- @
-- > S.toList $ S.intFromThenTo 0 2 6
-- [0,2,4,6]
-- > S.toList $ S.intFromThenTo 0 (-2) (-6)
-- [0,-2,-4,-6]
-- @
--
-- @since 0.6.0
{-# INLINE intFromThenTo #-}
intFromThenTo :: (IsStream t, Monad m, Integral a) => a -> a -> a -> t m a
intFromThenTo from next to = fromStreamD $ D.intFromThenTo from next to

-- | @numFromStep from step@ generates an infinite stream whose first element
-- is @from@ and the successive elements are in increments of @step@. This is
-- numerically stable but does not check for overflow or underflow for bounded
-- types. Note, for 'Integral' types 'intFromStep' is faster.
--
--
-- @
-- > S.toList $ S.take 4 $ S.numFromStep 0.1 2
-- [0.1,2.1,4.1,6.1]
-- > S.toList $ S.take 3 $ S.numFromStep 0.1 (-2)
-- [0.1,-1.9,-3.9,-5.9]
-- @
--
-- @since 0.6.0
{-# INLINE numFromStep #-}
numFromStep :: (IsStream t, Monad m, Num a) => a -> a -> t m a
numFromStep from stride = fromStreamD $ D.numFromStep from stride

-- Even though the underlying implementation of fracFrom and fracFromThen works
-- for any 'Num' we have restricted these to 'Fractional' because these do not
-- perform any bounds check, in contrast to int versions and are therefore not
-- equivalent substitutes for those.
--
-- | Numerically stable enumeration from a 'Fractional' number in steps of size
-- @1@. @fracFrom from@ generates a stream whose first element is @from@ and
-- the successive elements are in increments of @1@.  No overflow or underflow
-- checks are performed.
--
-- This is the equivalent to 'enumFrom' for 'Fractional' types. For example:
--
-- @
-- > S.toList $ S.take 4 $ S.fracFrom 1.1
-- [1.1,2.1,3.1,4.1]
-- @
--
--
-- @since 0.6.0
{-# INLINE fracFrom #-}
fracFrom :: (IsStream t, Monad m, Fractional a) => a -> t m a
fracFrom from = fromStreamD $ D.numFrom from

-- | Numerically stable enumeration from a 'Fractional' number in steps.
-- @fracFromThen from then@ generates a stream whose first element is @from@,
-- the second element is @then@ and the successive elements are in increments
-- of @then - from@.  No overflow or underflow checks are performed.
--
-- This is the equivalent of 'enumFromThen' for 'Fractional' types. For
-- example:
--
-- @
-- > S.toList $ S.take 4 $ S.fracFromThen 1.1 2.1
-- [1.1,2.1,3.1,4.1]
-- > S.toList $ S.take 4 $ S.fracFromThen 1.1 (-2.1)
-- [1.1,-2.1,-5.300000000000001,-8.500000000000002]
-- @
--
-- @since 0.6.0
{-# INLINE fracFromThen #-}
fracFromThen :: (IsStream t, Monad m, Fractional a) => a -> a -> t m a
fracFromThen from next = fromStreamD $ D.numFromThen from next

-- | Numerically stable enumeration from a 'Fractional' number to a given
-- limit.  @fracFromTo from to@ generates a finite stream whose first element
-- is @from@ and successive elements are in increments of @1@ up to @to@.
--
-- This is the equivalent of 'enumFromTo' for 'Fractional' types. For
-- example:
--
-- @
-- > S.toList $ S.fracFromTo 1.1 4
-- [1.1,2.1,3.1,4.1]
-- > S.toList $ S.fracFromTo 1.1 4.6
-- [1.1,2.1,3.1,4.1,5.1]
-- @
--
-- Notice that the last element is equal to the specified @to@ value after
-- rounding to the nearest integer.
--
-- @since 0.6.0
{-# INLINE fracFromTo #-}
fracFromTo :: (IsStream t, Monad m, Fractional a, Ord a) => a -> a -> t m a
fracFromTo from to = fromStreamD $ D.fracFromTo from to

-- | Numerically stable enumeration from a 'Fractional' number in steps up to a
-- given limit.  @fracFromThenTo from then to@ generates a finite stream whose
-- first element is @from@, the second element is @then@ and the successive
-- elements are in increments of @then - from@ up to @to@.
--
-- This is the equivalent of 'enumFromThenTo' for 'Fractional' types. For
-- example:
--
-- @
-- > S.toList $ S.fracFromThenTo 0.1 2 6
-- [0.1,2.0,3.9,5.799999999999999]
-- > S.toList $ S.fracFromThenTo 0.1 (-2) (-6)
-- [0.1,-2.0,-4.1000000000000005,-6.200000000000001]
-- @
--
--
-- @since 0.6.0
{-# INLINE fracFromThenTo #-}
fracFromThenTo
    :: (IsStream t, Monad m, Fractional a, Ord a)
    => a -> a -> a -> t m a
fracFromThenTo from next to = fromStreamD $ D.fracFromThenTo from next to

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
-- runStream $ serially $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
-- runStream $ asyncly  $ S.replicateM 10 $ (threadDelay 1000000 >> print 1)
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
-- runStream $ serially $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
-- runStream $ asyncly  $ S.take 10 $ S.repeatM $ (threadDelay 1000000 >> print 1)
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
-- runStream $ serially $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) 0
--
-- runStream $ asyncly  $ S.take 10 $ S.iterateM
--      (\\x -> threadDelay 1000000 >> print x >> return (x + 1)) 0
-- @
--
-- /Concurrent/
--
-- @since 0.1.2
iterateM :: (IsStream t, MonadAsync m) => (a -> m a) -> a -> t m a
iterateM step = go
    where
    go s = fromStream $ K.Stream $ \svr stp sng yld -> do
       next <- step s
       K.unStream (toStream (return s |: go next)) svr stp sng yld

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
-- runStream $ serially $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
-- runStream $ asyncly  $ S.fromFoldableM $ replicateM 10 (threadDelay 1000000 >> print 1)
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
fromHandle :: (IsStream t, MonadIO m) => IO.Handle -> t m String
fromHandle h = fromStream go
  where
  go = K.Stream $ \_ stp _ yld -> do
        eof <- liftIO $ IO.hIsEOF h
        if eof
        then stp
        else do
            str <- liftIO $ IO.hGetLine h
            yld str go

------------------------------------------------------------------------------
-- Elimination by Folding
------------------------------------------------------------------------------

-- | Lazy right fold with a monadic step function. For example, to fold a
-- stream into a list:
--
-- @
-- >> S.foldrM (\\x xs -> return (x : xs)) [] $ fromList [1,2,3]
-- [1,2,3]
-- @
--
-- @since 0.2.0
{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> SerialT m a -> m b
foldrM = P.foldrM

-- | Lazy right associative fold.
--
-- @foldr f z xs@ deconstructs @xs@ one element @x@ at a time, applying the
-- function @f@ to @x@ and the tail of the output. The tail recurses until the
-- input finishes and @z@ is used as the tail end.
--
-- @
-- foldr f z xs == x1 \`f` tail1
-- tail1        == x2 \`f` tail2
-- tail2        == x3 \`f` tail3
-- ...
-- tailn        == xn \`f` z
-- @
--
-- Resulting in a right associated expression:
--
-- @
-- foldr f z xs == x1 \`f` (x2 \`f` ...(xn \`f` z))
-- @
--
-- When the outermost operation in the fold function is a constructor, foldr
-- generates a structure that can be consumed lazily.  For example:
--
-- @
-- > S.foldr (:) [] $ S.fromList [1,2,3,4]
-- [1,2,3,4]
-- @
--
-- @
-- 1 : tail
-- 1 : (2 : tail)
-- 1 : (2 : (3 : tail))
-- 1 : (2 : (3 : (4 : [])))
-- @
--
-- When the outermost operation is a function @foldr@ results in a right
-- associated expression which cannot be reduced until the whole expression has
-- been built. Therefore, it ends up consuming the whole input, buffering the
-- whole expression in memory before reduction can start. For example:
--
-- @
-- > S.foldr (+) 0 $ S.fromList [1,2,3,4]
-- 10
-- @
--
-- @
-- 1 + tail
-- 1 + (2 + tail)
-- 1 + (2 + (3 + tail))
-- 1 + (2 + (3 + (4 + 0)))
-- @
--
-- In @foldr@, the output tail is the source of recursion, we can stop
-- recursion and terminate the fold by yielding a terminal value as tail:
--
-- >> S.foldr (\x rest -> if x == 3 then [] else x : rest) [] $ S.fromList [4,1,3,undefined]
-- >[4,1]
--
-- The arguments to the folding function (@a -> b -> b@) are in the head and
-- tail order of the output, @a@ is the head and @b@ is the tail. Remember, in
-- a right fold the zero is on the right, it is the tail end.
--
-- @since 0.1.0
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> SerialT m a -> m b
foldr = P.foldr

-- | Lazy right fold for non-empty streams, using first element as the starting
-- value. Returns 'Nothing' if the stream is empty.
--
-- @since 0.5.0
{-# INLINE foldr1 #-}
foldr1 :: Monad m => (a -> a -> a) -> SerialT m a -> m (Maybe a)
foldr1 f m = S.foldr1 f (toStreamS m)

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.2.0
{-# INLINE foldx #-}
foldx :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldx = K.foldx

-- |
-- @since 0.1.0
{-# DEPRECATED foldl "Please use foldx instead." #-}
foldl :: Monad m => (x -> a -> x) -> x -> (x -> b) -> SerialT m a -> m b
foldl = foldx

-- | Strict left associative fold.
--
-- @foldl' f z xs@ deconstructs @xs@ one element @x@ at a time, applying the
-- function @f@ to the output accumulated till now (the head of the expression)
-- and @x@. The accumulator starts with the initial value @z@ and keeps
-- accumulating elements until the input finishes.
--
-- @
-- head1        == z         \`f` x1
-- head2        == head1     \`f` x2
-- head3        == head2     \`f` x3
-- ...
-- foldl' f z xs == head(n-1) \`f` xn
-- @
--
-- Recursively building a left associated expression:
--
-- @
-- foldl' f z xs == (((z \`f` x1) \`f` x2) ...) \`f` xn
-- @
--
-- When the outermost operation in the fold function is a (left associated)
-- constructor, foldl' consumes the whole input to construct the new structure
-- buffered in memory in the reverse order of the input. For example:
--
-- @
-- > S.foldl' (flip (:)) [] $ S.fromList [1,2,3,4]
-- [4,3,2,1]
-- @
--
-- When the outermost operation is a function, @foldl'@ results in a left
-- associated expression which is incrementally reduced at each step of the
-- fold, thus never building the whole expression in memory. For example:
--
-- @
-- > S.foldl' (+) 0 $ S.fromList [1,2,3,4]
-- 10
-- @
--
-- @
-- 0 + 1
-- (0 + 1) + 2
-- ((0 + 1) + 2) + 3
-- (((0 + 1) + 2) + 3) + 4
-- @
-- @
-- 0 + 1 => reduce to 1
-- 1 + 2 => reduce to 3
-- 3 + 3 => reduce to 6
-- 6 + 4 => reduce to 10
-- @
--
-- We can stop recursion and terminate the fold by yielding an expression that
-- is independent of the input element:
--
-- >> S.foldl' (\acc x -> if acc >= 8 then acc else x + acc) 0 $ S.fromList [4,1,3,undefined]
-- >8
--
-- To stop on encountering the number 3 we will have to store the previous
-- number in the accumulator so that we do not depend on the current input:
--
-- @
-- > S.foldl' (\(acc, prev) x -> if prev == 3 then (acc,prev) else (x + acc,x))
--            (0,0) $ S.fromList \[4,1,3,undefined]
-- (8,3)
-- @
--
-- To map the fold operation to stateful or event-driven programming, we can
-- consider @z@ as the initial state and the stream being folded as a stream of
-- events, thus @foldl'@ processes all the events in the stream updating the
-- state on each event and then ultimately returning the final state.
--
-- The arguments to the folding function (@b -> a -> b@) are in the head and
-- tail order of the output expression, @b@ is the head and @a@ is the tail.
-- Remember, in a left fold the zero is on the left, at the head of the
-- expression.
--
-- IMPORTANT: 'foldl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, and to help GHC optimize better, it
-- is recommended that a strict data structure is used for accumulator.
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
foldxM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldxM = K.foldxM

-- |
-- @since 0.1.0
{-# DEPRECATED foldlM "Please use foldxM instead." #-}
foldlM :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> SerialT m a -> m b
foldlM = foldxM

-- | Like 'foldl'' but with a monadic step function.
--
-- @since 0.2.0
foldlM' :: Monad m => (b -> a -> m b) -> b -> SerialT m a -> m b
foldlM' step begin m = S.foldlM' step begin $ toStreamS m

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

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

-- | Extract all but the first element of the stream, if any.
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

-- | Map each element to a stream and then flatten the results into a single
-- stream.
--
-- > concatMap f = concatMapM (return . f)
--
-- @since 0.6.0
{-# INLINE concatMap #-}
concatMap ::(IsStream t, Monad m) => (a -> t m b) -> t m a -> t m b
concatMap f m = fromStreamD $ D.concatMap (toStreamD . f) (toStreamD m)

-- | Map each element to a stream using a monadic function and then flatten the
-- results into a single stream.
--
-- @since 0.6.0
{-# INLINE concatMapM #-}
concatMapM :: (IsStream t, Monad m) => (a -> m (t m b)) -> t m a -> t m b
concatMapM f m = fromStreamD $ D.concatMapM (fmap toStreamD . f) (toStreamD m)

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

-- XXX this can utilize parallel mapping if we implement it as runStream . mapM
-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
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
-- Convert a stream into a list in the underlying monad. Same as:
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: Monad m => SerialT m a -> m [a]
toList = P.toList

-- |
-- @
-- toHandle h = S.mapM_ $ hPutStrLn h
-- @
--
-- Write a stream of Strings to an IO Handle.
--
-- @since 0.1.0
toHandle :: MonadIO m => IO.Handle -> SerialT m String -> m ()
toHandle h m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single a = liftIO (IO.hPutStrLn h a)
            yieldk a r = liftIO (IO.hPutStrLn h a) >> go r
        in K.unStream m1 defState stop single yieldk

------------------------------------------------------------------------------
-- Transformation by Folding (Scans)
------------------------------------------------------------------------------

-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
--
-- @since 0.2.0
{-# INLINE scanx #-}
scanx :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx = K.scanx

-- |
-- @since 0.1.1
{-# DEPRECATED scan "Please use scanx instead." #-}
scan :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scan = scanx

-- XXX this needs to be concurrent
-- | Like 'scanl'' but with a monadic fold function.
--
-- @since 0.4.0
{-# INLINE scanlM' #-}
scanlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> t m b
scanlM' step begin m = fromStreamD $ D.scanlM' step begin $ toStreamD m

-- | Strict left scan.
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
-- Consider the following example, computing the sum and the product of the
-- elements in a stream in one go using a @foldl'@:
--
-- @
-- > S.foldl' (\\(s, p) x -> (s + x, p * x)) (0,1) $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- Using @scanl'@ we can compute the sum in the first stage and pass it down to
-- the next stage for computing the product:
--
-- @
-- >   S.foldl' (\\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ S.scanl' (\\(s, _) x -> (s + x, x)) (0,1)
--   $ S.fromList \[1,2,3,4]
-- (10,24)
-- @
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, and to help GHC optimize better, it
-- is recommended that a strict data structure is used for accumulator.
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
-- > runStream $ S.mapM putStr $ S.fromList ["a", "b", "c"]
-- abc
--
-- runStream $ S.replicateM 10 (return 1)
--           & (serially . S.mapM (\\x -> threadDelay 1000000 >> print x))
--
-- runStream $ S.replicateM 10 (return 1)
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
-- > runStream $ S.sequence $ S.fromList [putStr "a", putStr "b", putStrLn "c"]
-- abc
--
-- runStream $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
--           & (serially . S.sequence)
--
-- runStream $ S.replicateM 10 (return $ threadDelay 1000000 >> print 1)
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

-- XXX to scale this we need to use a slab allocated array backed
-- representation for temporary storage.
--
-- | Returns the elements of the stream in reverse order.
-- The stream must be finite.
--
-- @since 0.1.1
reverse :: (IsStream t) => t m a -> t m a
reverse m = fromStream $ go K.nil (toStream m)
    where
    go rev rest = K.Stream $ \st stp sng yld ->
        let runIt x = K.unStream x (rstState st) stp sng yld
            stop = runIt rev
            single a = runIt $ a `K.cons` rev
            yieldk a r = runIt $ go (a `K.cons` rev) r
         in K.unStream rest (rstState st) stop single yieldk

------------------------------------------------------------------------------
-- Transformation by Inserting
------------------------------------------------------------------------------

-- | Generate a stream by performing a monadic action between consecutive
-- elements of the given stream.
--
-- /Concurrent (do not use with 'parallely' on infinite streams)/
--
-- @
-- > S.toList $ S.intersperseM (putChar \'a' >> return ',') $ S.fromList "hello"
-- aaaa"h,e,l,l,o"
-- @
--
-- @since 0.5.0
{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM = K.intersperseM

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
-- > indexed = S.zipWith (,) (S.intFrom 0)
--
-- Pair each element in a stream with its index.
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
-- > indexedR n = S.zipWith (,) (S.intFromThen n (n - 1))
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- @
-- > S.toList $ S.indexedR 10 $ S.fromList "hello"
-- [(9,'h'),(8,'e'),(7,'l'),(6,'l'),(5,'o')]
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
mergeAsyncBy f m1 m2 = K.fromStream $ K.Stream $ \st stp sng yld -> do
    ma <- mkAsync' (rstState st) m1
    mb <- mkAsync' (rstState st) m2
    K.unStream (K.toStream (K.mergeBy f ma mb)) (rstState st) stp sng yld

-- | Like 'mergeByM' but merges concurrently (i.e. both the elements being
-- merged are generated concurrently).
--
-- @since 0.6.0
mergeAsyncByM :: (IsStream t, MonadAsync m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeAsyncByM f m1 m2 = K.fromStream $ K.Stream $ \st stp sng yld -> do
    ma <- mkAsync' (rstState st) m1
    mb <- mkAsync' (rstState st) m2
    K.unStream (K.toStream (K.mergeByM f ma mb)) (rstState st) stp sng yld
