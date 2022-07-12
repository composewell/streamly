-- |
-- Module      : Streamly.Internal.Data.Stream.Transform
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Transform
    (
    -- * Piping
    -- | Pass through a 'Pipe'.
      transform

    -- * Folding
    , foldrS
    , foldrT

    -- * Mapping
    -- | Stateless one-to-one maps.
    , sequence
    , mapM

    -- * Mapping Side Effects (Observation)
    -- | See also the intersperse*_ combinators.
    , trace
    , trace_
    , tap

    -- * Scanning
    , scan
    , scanMany
    , postscan
    , smapM

    -- * Filtering
    -- | Produce a subset of the stream using criteria based on the values of
    -- the elements. We can use a concatMap and scan for filtering but these
    -- combinators are more efficient and convenient.

    , with
    , deleteBy
    , filter
    , filterM
    , foldFilter
    , uniq
    , uniqBy
    , nubBy
    , prune
    , repeated

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.

    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM
    , init
    , tail

    -- * Inserting Elements
    -- | Produce a superset of the stream. This is the opposite of
    -- filtering/sampling.  We can always use concatMap and scan for inserting
    -- but these combinators are more efficient and convenient.

    -- Element agnostic (Opposite of sampling)
    , intersperse
    , intersperseM -- XXX naming
    , intersperseBySpan

    , intersperseSuffix
    , intersperseSuffixBySpan

    -- , interspersePrefix
    -- , interspersePrefixBySpan

    -- * Inserting Side Effects/Time
    , intersperseM_ -- XXX naming
    , delay
    , intersperseSuffix_
    , delayPost
    , interspersePrefix_
    , delayPre

    -- * Element Aware Insertion
    -- | Opposite of filtering
    , insertBy
    -- , intersperseByBefore
    -- , intersperseByAfter

    -- * Reordering
    , reverse
    , reverse'
    , reassembleBy

    -- * Position Indexing
    , indexed
    , indexedR

    -- * Searching
    , findIndices -- XXX indicesBy
    , elemIndices -- XXX indicesOf

    -- * Rolling map
    -- | Map using the previous element.
    , rollingMapM
    , rollingMap
    , rollingMap2

    -- * Maybe Streams
    -- Move these to Streamly.Data.Maybe.Stream?
    , catMaybes -- XXX justs (like lefts/rights)
    , mapMaybe
    , mapMaybeM

    -- * Either Streams
    -- Move these to Streamly.Data.Either.Stream?
    , lefts
    , rights
    , both
    )
where

#include "inline.hs"

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Either (fromLeft, isLeft, isRight, fromRight)
import Data.Maybe (isJust, fromJust)

import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Pipe (Pipe)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.StreamD.Transform as D
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Bottom
import Streamly.Internal.Data.Stream.Type

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence
       , reverse, foldr1 , repeat, scanl, scanl1, zipWith, init, tail)

--
-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Control.Monad.Trans (lift)
-- >>> import Control.Monad.Trans.Identity (runIdentityT)
-- >>> import Data.Function ((&))
-- >>> import Data.Maybe (fromJust, isJust)
-- >>> import Prelude hiding (filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence, reverse, foldr1 , scanl, scanl1)
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
--
-- >>> hSetBuffering stdout LineBuffering

-- XXX because of the use of D.cons for appending, folds and scans have
-- quadratic complexity when iterated over a stream. We should use StreamK for
-- linear performance on iteration.

------------------------------------------------------------------------------
-- Piping
------------------------------------------------------------------------------

-- | Use a 'Pipe' to transform a stream.
--
-- /Pre-release/
--
{-# INLINE transform #-}
transform :: Monad m => Pipe m a b -> Stream m a -> Stream m b
transform pipe xs = fromStreamD $ D.transform pipe (toStreamD xs)

------------------------------------------------------------------------------
-- Transformation Folds
------------------------------------------------------------------------------

-- | Right fold to a streaming monad.
--
-- > foldrS Stream.cons Stream.nil === id
--
-- 'foldrS' can be used to perform stateless stream to stream transformations
-- like map and filter in general. It can be coupled with a scan to perform
-- stateful transformations. However, note that the custom map and filter
-- routines can be much more efficient than this due to better stream fusion.
--
-- >>> input = Stream.unfold Unfold.fromList [1..5]
-- >>> Stream.fold Fold.toList $ Stream.foldrS Stream.cons Stream.nil input
-- [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> step x xs = if odd x then return True else xs
-- >>> input = Stream.unfold Unfold.fromList (2:4:5:undefined) :: Stream IO Int
-- >>> Stream.fold Fold.toList $ Stream.foldrS step (return False) input
-- [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> step x xs = if odd x then (x + 2) `Stream.cons` xs else xs
-- >>> input = Stream.unfold Unfold.fromList [1..5] :: Stream IO Int
-- >>> Stream.fold Fold.toList $ Stream.foldrS step Stream.nil input
-- [3,5,7]
--
-- /Pre-release/
{-# INLINE foldrS #-}
foldrS ::
     (a -> Stream m b -> Stream m b)
  -> Stream m b
  -> Stream m a
  -> Stream m b
foldrS f z xs =
    fromStreamK
        $ K.foldrS
            (\y ys -> toStreamK $ f y (fromStreamK ys))
            (toStreamK z)
            (toStreamK xs)

-- | Right fold to a transformer monad.  This is the most general right fold
-- function. 'foldrS' is a special case of 'foldrT', however 'foldrS'
-- implementation can be more efficient:
--
-- >>> foldrS = foldrT
--
-- >>> step f x xs = lift $ f x (runIdentityT xs)
-- >>> foldrM f z s = runIdentityT $ foldrT (step f) (lift z) s
--
-- 'foldrT' can be used to translate streamly streams to other transformer
-- monads e.g.  to a different streaming type.
--
-- /Pre-release/
{-# INLINE foldrT #-}
foldrT :: (Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> Stream m a -> s m b
foldrT f z s = D.foldrT f z (toStreamD s)

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

-- |
-- >>> mapM f = Stream.sequence . fmap f
--
-- Apply a monadic function to each element of the stream and replace it with
-- the output of the resulting action.
--
-- >>> s = Stream.unfold Unfold.fromList ["a", "b", "c"]
-- >>> Stream.fold Fold.drain $ Stream.mapM putStr s
-- abc
--
-- /Pre-release/
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f m = fromStreamK $ D.toStreamK $ D.mapM f $ toStreamD m

-- |
-- sequence = Stream.mapM id
--
-- Replace the elements of a stream of monadic actions with the outputs of
-- those actions.
--
-- >>> s = Stream.unfold Unfold.fromList [putStr "a", putStr "b", putStrLn "c"]
-- >>> Stream.fold Fold.drain $ Stream.sequence s
-- abc
--
{-# INLINE sequence #-}
sequence :: Monad m => Stream m (m a) -> Stream m a
sequence = mapM id

------------------------------------------------------------------------------
-- Mapping side effects
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
-- >>> s = Stream.unfold Unfold.enumerateFromTo (1, 2)
-- >>> Stream.fold Fold.drain $ Stream.tap (Fold.drainBy print) s
-- 1
-- 2
--
-- Compare with 'trace'.
--
{-# INLINE tap #-}
tap :: Monad m => FL.Fold m a b -> Stream m a -> Stream m a
tap f xs = fromStreamD $ D.tap f (toStreamD xs)

-- | Apply a monadic function to each element flowing through the stream and
-- discard the results.
--
-- >>> s = Stream.unfold Unfold.enumerateFromTo (1, 2)
-- >>> Stream.fold Fold.drain $ Stream.trace print s
-- 1
-- 2
--
-- Compare with 'tap'.
--
{-# INLINE trace #-}
trace :: Monad m => (a -> m b) -> Stream m a -> Stream m a
trace f = mapM (\x -> void (f x) >> return x)

-- | Perform a side effect before yielding each element of the stream and
-- discard the results.
--
-- >>> s = Stream.unfold Unfold.enumerateFromTo (1, 2)
-- >>> Stream.fold Fold.drain $ Stream.trace_ (print "got here") s
-- "got here"
-- "got here"
--
-- Same as 'interspersePrefix_' but always serial.
--
-- See also: 'trace'
--
-- /Pre-release/
{-# INLINE trace_ #-}
trace_ :: Monad m => m b -> Stream m a -> Stream m a
trace_ eff = fromStreamD . D.mapM (\x -> eff >> return x) . toStreamD

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

-- XXX It may be useful to have a version of scan where we can keep the
-- accumulator independent of the value emitted. So that we do not necessarily
-- have to keep a value in the accumulator which we are not using. We can pass
-- an extraction function that will take the accumulator and the current value
-- of the element and emit the next value in the stream. That will also make it
-- possible to modify the accumulator after using it. In fact, the step function
-- can return new accumulator and the value to be emitted. The signature would
-- be more like mapAccumL.

-- | Strict left scan. Scan a stream using the given monadic fold.
--
-- >>> s = Stream.unfold Unfold.fromList [1..10]
-- >>> Stream.fold Fold.toList $ Stream.takeWhile (< 10) $ Stream.scan Fold.sum s
-- [0,1,3,6]
--
-- >>> scanl' step z = Stream.scan (Fold.foldl' step z)
--
-- Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- >>> s = Stream.unfold Unfold.fromList [1,2,3,4]
-- >>> Stream.fold Fold.toList $ scanl' (+) 0 s
-- [0,1,3,6,10]
--
-- >>> Stream.fold Fold.toList $ scanl' (flip (:)) [] s
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
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
-- >>> foldl' step z = Stream.fold (Fold.foldl' step z)
-- >>> foldl' (\(s, p) x -> (s + x, p * x)) (0,1) s
-- (10,24)
--
-- Using @scanl'@ we can make it modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- >>> :{
--   foldl' (\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ scanl' (\(s, _) x -> (s + x, x)) (0,1)
--   $ Stream.unfold Unfold.fromList [1,2,3,4]
-- :}
-- (10,24)
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
-- See also: 'usingStateT'
--
{-# INLINE scan #-}
scan :: Monad m => Fold m a b -> Stream m a -> Stream m b
scan fld m = fromStreamD $ D.scanOnce fld $ toStreamD m

-- | Like 'scan' but restarts scanning afresh when the scanning fold
-- terminates.
--
-- /Pre-release/
{-# INLINE scanMany #-}
scanMany :: Monad m => Fold m a b -> Stream m a -> Stream m b
scanMany fld m = fromStreamD $ D.scanMany fld $ toStreamD m

------------------------------------------------------------------------------
-- Filtering
------------------------------------------------------------------------------

-- | Modify a @Stream m a -> Stream m a@ stream transformation that accepts a
-- predicate @(a -> b)@ to accept @((s, a) -> b)@ instead, provided a
-- transformation @Stream m a -> Stream m (s, a)@. Convenient to filter with
-- index or time.
--
-- >>> filterWithIndex = Stream.with Stream.indexed Stream.filter
--
-- /Pre-release/
{-# INLINE with #-}
with :: Monad m =>
       (Stream m a -> Stream m (s, a))
    -> (((s, a) -> b) -> Stream m (s, a) -> Stream m (s, a))
    -> (((s, a) -> b) -> Stream m a -> Stream m a)
with f comb g = fmap snd . comb g . f

-- | Include only those elements that pass a predicate.
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter p m = fromStreamD $ D.filter p $ toStreamD m

-- | Same as 'filter' but with a monadic predicate.
--
{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filterM p m = fromStreamD $ D.filterM p $ toStreamD m

-- | Use a filtering fold on a stream.
--
-- >>> input = Stream.unfold Unfold.fromList [1..10]
-- >>> Stream.fold Fold.sum $ Stream.foldFilter (Fold.satisfy (> 5)) input
-- 40
--
-- /Pre-release/
--
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> Stream m a -> Stream m b
foldFilter p = fromStreamD . D.foldFilter p . toStreamD

-- | Drop repeated elements that are adjacent to each other using the supplied
-- comparison function.
--
-- >>> uniq = Stream.uniqBy (==)
--
-- To strip duplicate path separators:
--
-- >>> input = Stream.unfold Unfold.fromList "//a//b"
-- >>> f x y = x == '/' && y == '/'
-- >>> Stream.fold Fold.toList $ Stream.uniqBy f input
-- "/a/b"
--
-- Space: @O(1)@
--
-- See also: 'nubBy'.
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a
uniqBy eq = catMaybes . rollingMap f

    where

    f pre curr =
        case pre of
            Nothing -> Just curr
            Just x -> if x `eq` curr then Nothing else Just curr

-- | Drop repeated elements that are adjacent to each other.
--
{-# INLINE uniq #-}
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
uniq = fromStreamD . D.uniq . toStreamD

-- | Strip all leading and trailing occurrences of an element passing a
-- predicate and make all other consecutive occurrences uniq.
--
-- >> prune p = Stream.dropWhileAround p $ Stream.uniqBy (x y -> p x && p y)
--
-- @
-- > Stream.prune isSpace (Stream.fromList "  hello      world!   ")
-- "hello world!"
--
-- @
--
-- Space: @O(1)@
--
-- /Unimplemented/
{-# INLINE prune #-}
prune ::
    -- (Monad m, Eq a) =>
    (a -> Bool) -> Stream m a -> Stream m a
prune = error "Not implemented yet!"

-- Possible implementation:
-- @repeated =
--      Stream.catMaybes . Stream.parseMany (Parser.groupBy (==) Fold.repeated)@
--
-- 'Fold.repeated' should return 'Just' when repeated, and 'Nothing' for a
-- single element.

-- | Emit only repeated elements, once.
--
-- /Unimplemented/
repeated :: -- (Monad m, Eq a) =>
    Stream m a -> Stream m a
repeated = undefined

-- We can have more efficient implementations for nubOrd and nubInt by using
-- Set and IntSet to find/remove duplication. For Hashable we can use a
-- hashmap. Use rewrite rules to specialize to more efficient impls.

-- | Drop repeated elements anywhere in the stream.
--
-- /Caution: not scalable for infinite streams/
--
-- /Unimplemented/
--
{-# INLINE nubBy #-}
nubBy :: -- Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a
nubBy = undefined -- fromStreamD . D.nubBy . toStreamD

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- >>> input = Stream.unfold Unfold.fromList [1,3,3,5]
-- >>> Stream.fold Fold.toList $ Stream.deleteBy (==) 3 input
-- [1,3,5]
--
{-# INLINE deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Stream m a -> Stream m a
deleteBy cmp x m = fromStreamD $ D.deleteBy cmp x (toStreamD m)

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Same as 'takeWhile' but with a monadic predicate.
--
{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
takeWhileM p m = fromStreamD $ D.takeWhileM p $ toStreamD m

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
dropWhile p m = fromStreamD $ D.dropWhile p $ toStreamD m

-- | Same as 'dropWhile' but with a monadic predicate.
--
{-# INLINE dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
dropWhileM p m = fromStreamD $ D.dropWhileM p $ toStreamD m

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- >>> insertBy cmp x = Stream.mergeBy cmp (Stream.fromPure x)
--
-- >>> input = Stream.unfold Unfold.fromList [1,3,5]
-- >>> Stream.fold Fold.toList $ Stream.insertBy compare 2 input
-- [1,2,3,5]
--
{-# INLINE insertBy #-}
insertBy ::Monad m => (a -> a -> Ordering) -> a -> Stream m a -> Stream m a
insertBy cmp x m = fromStreamD $ D.insertBy cmp x (toStreamD m)

-- | Insert a pure value between successive elements of a stream.
--
-- >>> input = Stream.unfold Unfold.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.intersperse ',' input
-- "h,e,l,l,o"
--
{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = fromStreamD . D.intersperse a . toStreamD

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- >>> input = Stream.unfold Unfold.fromList "hello"
-- >>> Stream.fold Fold.drain $ Stream.trace putChar $ Stream.intersperseM_ (putChar '.') input
-- h.e.l.l.o
--
-- /Pre-release/
{-# INLINE intersperseM_ #-}
intersperseM_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseM_ m = fromStreamD . D.intersperseM_ m . toStreamD

-- | Intersperse a monadic action into the input stream after every @n@
-- elements.
--
-- >> input = Stream.unfold Unfold.fromList "hello"
-- >> Stream.fold Fold.toList $ Stream.intersperseBySpan 2 (return ',') input
-- "he,ll,o"
--
-- /Unimplemented/
{-# INLINE intersperseBySpan #-}
intersperseBySpan :: -- Monad m =>
    Int -> m a -> t m a -> t m a
intersperseBySpan _n _f _xs = undefined

-- | Insert an effect and its output after consuming an element of a stream.
--
-- >>> input = Stream.unfold Unfold.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ intersperseSuffix (putChar '.' >> return ',') input
-- h.,e.,l.,l.,o.,"h,e,l,l,o,"
--
-- /Pre-release/
{-# INLINE intersperseSuffix #-}
intersperseSuffix :: Monad m => m a -> Stream m a -> Stream m a
intersperseSuffix m = fromStreamD . D.intersperseSuffix m . toStreamD

-- | Insert a side effect after consuming an element of a stream.
--
-- >>> input = Stream.unfold Unfold.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.intersperseSuffix_ (threadDelay 1000000) input
-- "hello"
--
-- /Pre-release/
--
{-# INLINE intersperseSuffix_ #-}
intersperseSuffix_ :: Monad m => m b -> Stream m a -> Stream m a
intersperseSuffix_ m = fromStreamD . D.intersperseSuffix_ m . toStreamD

-- XXX Use an offset argument, like tapOffsetEvery

-- | Like 'intersperseSuffix' but intersperses an effectful action into the
-- input stream after every @n@ elements and after the last element.
--
-- >>> input = Stream.unfold Unfold.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.intersperseSuffixBySpan 2 (return ',') input
-- "he,ll,o,"
--
-- /Pre-release/
--
{-# INLINE intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: Monad m
    => Int -> m a -> Stream m a -> Stream m a
intersperseSuffixBySpan n eff =
    fromStreamD . D.intersperseSuffixBySpan n eff . toStreamD

-- | Insert a side effect before consuming an element of a stream.
--
-- >>> input = Stream.unfold Unfold.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ Stream.interspersePrefix_ (putChar '.' >> return ',') input
-- .h.e.l.l.o"hello"
--
-- Same as 'trace_'.
--
-- /Pre-release/
--
{-# INLINE interspersePrefix_ #-}
interspersePrefix_ :: Monad m => m b -> Stream m a -> Stream m a
interspersePrefix_ m = mapM (\x -> void m >> return x)

------------------------------------------------------------------------------
-- Inserting Time
------------------------------------------------------------------------------

-- | Introduce a delay of specified seconds between elements of the stream.
--
-- >>> input = Stream.unfold Unfold.enumerateFromTo (1, 3)
-- >>> Stream.fold (Fold.drainBy print) $ Stream.delay 1 input
-- 1
-- 2
-- 3
--
{-# INLINE delay #-}
delay :: MonadIO m => Double -> Stream m a -> Stream m a
delay n = intersperseM_ $ liftIO $ threadDelay $ round $ n * 1000000

-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- >>> input = Stream.unfold Unfold.enumerateFromTo (1, 3)
-- >>> Stream.fold (Fold.drainBy print) $ Stream.delayPost 1 input
-- 1
-- 2
-- 3
--
-- /Pre-release/
--
{-# INLINE delayPost #-}
delayPost :: MonadIO m => Double -> Stream m a -> Stream m a
delayPost n = intersperseSuffix_ $ liftIO $ threadDelay $ round $ n * 1000000

-- | Introduce a delay of specified seconds before consuming an element of a
-- stream.
--
-- >>> input = Stream.unfold Unfold.enumerateFromTo (1, 3)
-- >>> Stream.fold (Fold.drainBy print) $ Stream.delayPre 1 input
-- 1
-- 2
-- 3
--
-- /Pre-release/
--
{-# INLINE delayPre #-}
delayPre :: MonadIO m => Double -> Stream m a -> Stream m a
delayPre n = trace_ $ liftIO $ threadDelay $ round $ n * 1000000

------------------------------------------------------------------------------
-- Reorder in sequence
------------------------------------------------------------------------------

-- | Buffer until the next element in sequence arrives. The function argument
-- determines the difference in sequence numbers. This could be useful in
-- implementing sequenced streams, for example, TCP reassembly.
--
-- /Unimplemented/
--
{-# INLINE reassembleBy #-}
reassembleBy
    :: -- Monad m =>
       Fold m a b
    -> (a -> a -> Int)
    -> t m a
    -> t m b
reassembleBy = undefined

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- |
-- >>> f = Fold.foldl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- >>> indexed = Stream.postscan f
-- >>> indexed = Stream.zipWith (,) (Stream.unfold Unfold.enumerateFrom 0)
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- >>> Stream.fold Fold.toList $ Stream.indexed $ Stream.unfold Unfold.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
{-# INLINE indexed #-}
indexed :: Monad m => Stream m a -> Stream m (Int, a)
indexed = fromStreamD . D.indexed . toStreamD

-- |
-- >>> f n = Fold.foldl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- >>> indexedR n = Stream.postscan (f n)
--
-- >>> s n = Stream.unfold Unfold.enumerateFromThen (n, (n - 1))
-- >>> indexedR n = Stream.zipWith (,) (s n)
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- >>> Stream.fold Fold.toList $ Stream.indexedR 10 $ Stream.unfold Unfold.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
--
{-# INLINE indexedR #-}
indexedR :: Monad m => Int -> Stream m a -> Stream m (Int, a)
indexedR n = fromStreamD . D.indexedR n . toStreamD

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the value of the element in the stream is equal
-- to the given value.
--
-- >>> elemIndices a = Stream.findIndices (== a)
--
{-# INLINE elemIndices #-}
elemIndices :: (Monad m, Eq a) => a -> Stream m a -> Stream m Int
elemIndices a = findIndices (== a)

------------------------------------------------------------------------------
-- Rolling map
------------------------------------------------------------------------------

-- XXX this is not a one-to-one map so calling it map may not be right.
-- We can perhaps call it zipWithTail or rollWith.

-- | Apply a function on every two successive elements of a stream. The first
-- argument of the map function is the previous element and the second argument
-- is the current element. When the current element is the first element, the
-- previous element is 'Nothing'.
--
-- /Pre-release/
--
{-# INLINE rollingMap #-}
rollingMap :: Monad m => (Maybe a -> a -> b) -> Stream m a -> Stream m b
rollingMap f m = fromStreamD $ D.rollingMap f $ toStreamD m

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (Maybe a -> a -> m b) -> Stream m a -> Stream m b
rollingMapM f m = fromStreamD $ D.rollingMapM f $ toStreamD m

-- | Like 'rollingMap' but requires at least two elements in the stream,
-- returns an empty stream otherwise.
--
-- This is the stream equivalent of the list idiom @zipWith f xs (tail xs)@.
--
-- /Pre-release/
--
{-# INLINE rollingMap2 #-}
rollingMap2 :: Monad m => (a -> a -> b) -> Stream m a -> Stream m b
rollingMap2 f m = fromStreamD $ D.rollingMap2 f $ toStreamD m

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- Equivalent to:
--
-- >>> mapMaybe f = fmap fromJust . Stream.filter isJust . fmap f
--
{-# INLINE mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f m = fromStreamD $ D.mapMaybe f $ toStreamD m

-- | Like 'mapMaybe' but maps a monadic function.
--
-- Equivalent to:
--
-- >>> mapMaybeM f = fmap fromJust . Stream.filter isJust . Stream.mapM f
--
{-# INLINE_EARLY mapMaybeM #-}
mapMaybeM :: Monad m
          => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- /Pre-release/
--
{-# INLINE catMaybes #-}
catMaybes :: Monad m => Stream m (Maybe a) -> Stream m a
catMaybes = fmap fromJust . filter isJust

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE lefts #-}
lefts :: Monad m => Stream m (Either a b) -> Stream m a
lefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE rights #-}
rights :: Monad m => Stream m (Either a b) -> Stream m b
rights = fmap (fromRight undefined) . filter isRight

-- | Remove the either wrapper and flatten both lefts and as well as rights in
-- the output stream.
--
-- /Pre-release/
--
{-# INLINE both #-}
both :: Monad m => Stream m (Either a a) -> Stream m a
both = fmap (either id id)

#ifdef USE_STREAMK_ONLY
{-# INLINE init #-}
init :: Monad m => Stream m a -> Stream m a
init = fromStreamK . K.init . toStreamK

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> Stream m a
tail = fromStreamK . K.tail . toStreamK
#else
{-# INLINE init #-}
init :: Monad m => Stream m a -> Stream m a
init = fromStreamD . D.init . toStreamD

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> Stream m a
tail = fromStreamD . D.tail . toStreamD
#endif
