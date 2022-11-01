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
    , scanlMAfter'

    -- * Filtering
    -- | Produce a subset of the stream using criteria based on the values of
    -- the elements. We can use a concatMap and scan for filtering but these
    -- combinators are more efficient and convenient.

    -- mapMaybeM is a general filtering combinator as we can map the stream to
    -- Just/Nothing using any stateful fold and then use this to filter out.
    , mapMaybeM
    , mapMaybe
    , catMaybes
    , scanMaybe

    , with
    , deleteBy
    , filter
    , filterM

    -- Stateful/scanning filters
    , uniq
    , uniqBy
    , prune
    , repeated

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.

    , take
    , takeWhile
    , takeWhileM
    , takeWhileLast
    , takeWhileAround
    , drop
    , dropLast
    , dropWhile
    , dropWhileM
    , dropWhileLast
    , dropWhileAround

    -- * Position Indexing
    , indexed
    , indexedR

      -- * Time Indexing
    , timestamped
    , timestampWith
    , timeIndexed
    , timeIndexWith

    -- * Searching
    , findIndices -- XXX indicesBy
    , elemIndices -- XXX indicesOf

    -- * Rolling map
    -- | Map using the previous element.
    , rollingMapM
    , rollingMap
    , rollingMap2

    -- Merge

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

    -- Fold and Unfold, Buffering

    -- * Reordering
    , reverse
    , reverse'
    , reassembleBy

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
import Streamly.Internal.Data.Time.Units (AbsTime, RelTime64)

import qualified Streamly.Internal.Data.Fold as FL
-- import qualified Streamly.Internal.Data.Fold.Window as Window
import qualified Streamly.Internal.Data.Stream.StreamD.Transform as D
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K


import Streamly.Internal.Data.Stream.Bottom
import Streamly.Internal.Data.Stream.Type

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence
       , reverse, foldr1 , repeat, scanl, scanl1, zipWith)

--
-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Control.Monad.IO.Class (MonadIO (liftIO))
-- >>> import Control.Monad.Trans (lift)
-- >>> import Control.Monad.Trans.Identity (runIdentityT)
-- >>> import Data.Either (fromLeft, fromRight, isLeft, isRight, either)
-- >>> import Data.Function ((&))
-- >>> import Data.Maybe (fromJust, isJust)
-- >>> import Prelude hiding (filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence, reverse, foldr1 , scanl, scanl1)
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold.Window as Window
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
-- >>> input = Stream.fromList [1..5]
-- >>> Stream.fold Fold.toList $ Stream.foldrS Stream.cons Stream.nil input
-- [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> step x xs = if odd x then return True else xs
-- >>> input = Stream.fromList (2:4:5:undefined) :: Stream IO Int
-- >>> Stream.fold Fold.toList $ Stream.foldrS step (return False) input
-- [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> step x xs = if odd x then (x + 2) `Stream.cons` xs else xs
-- >>> input = Stream.fromList [1..5] :: Stream IO Int
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
-- >>> s = Stream.fromList ["a", "b", "c"]
-- >>> Stream.fold Fold.drain $ Stream.mapM putStr s
-- abc
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f m = fromStreamK $ D.toStreamK $ D.mapM f $ toStreamD m

-- |
-- >>> sequence = Stream.mapM id
--
-- Replace the elements of a stream of monadic actions with the outputs of
-- those actions.
--
-- >>> s = Stream.fromList [putStr "a", putStr "b", putStrLn "c"]
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
-- >>> s = Stream.enumerateFromTo 1 2
-- >>> Stream.fold Fold.drain $ Stream.tap (Fold.drainMapM print) s
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
-- >>> s = Stream.enumerateFromTo 1 2
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
-- >>> s = Stream.enumerateFromTo 1 2
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

-------------------------------------------------------------------------------
-- Scanning
-------------------------------------------------------------------------------

-- | @scanlMAfter' accumulate initial done stream@ is like 'scanlM'' except
-- that it provides an additional @done@ function to be applied on the
-- accumulator when the stream stops. The result of @done@ is also emitted in
-- the stream.
--
-- This function can be used to allocate a resource in the beginning of the
-- scan and release it when the stream ends or to flush the internal state of
-- the scan at the end.
--
-- /Pre-release/
--
{-# INLINE scanlMAfter' #-}
scanlMAfter' ::
       Monad m
    => (b -> a -> m b)
    -> m b
    -> (b -> m b)
    -> Stream m a
    -> Stream m b
scanlMAfter' step initial done stream =
    fromStreamD $ D.scanlMAfter' step initial done $ toStreamD stream

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
-- >>> s = Stream.fromList [1..10]
-- >>> Stream.fold Fold.toList $ Stream.takeWhile (< 10) $ Stream.scan Fold.sum s
-- [0,1,3,6]
--
-- See also: 'usingStateT'
--

-- EXPLANATION:
-- >>> scanl' step z = Stream.scan (Fold.foldl' step z)
--
-- Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- >>> s = Stream.fromList [1,2,3,4]
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
--   $ Stream.fromList [1,2,3,4]
-- :}
-- (10,24)
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
{-# INLINE scan #-}
scan :: Monad m => Fold m a b -> Stream m a -> Stream m b
scan fld m = fromStreamD $ D.scanOnce fld $ toStreamD m

-- | Like 'scan' but restarts scanning afresh when the scanning fold
-- terminates.
--
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
-- >>> filter p = Stream.filterM (return . p)
-- >>> filter p = Stream.mapMaybe (\x -> if p x then Just x else Nothing)
-- >>> filter p = Stream.scanMaybe (Fold.filtering p)
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
-- filter p = scanMaybe (FL.filtering p)
filter p m = fromStreamD $ D.filter p $ toStreamD m

-- | Same as 'filter' but with a monadic predicate.
--
-- >>> f p x = p x >>= \r -> return $ if r then Just x else Nothing
-- >>> filterM p = Stream.mapMaybeM (f p)
--
{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filterM p m = fromStreamD $ D.filterM p $ toStreamD m

-- | Drop repeated elements that are adjacent to each other using the supplied
-- comparison function.
--
-- >>> uniq = Stream.uniqBy (==)
--
-- To strip duplicate path separators:
--
-- >>> input = Stream.fromList "//a//b"
-- >>> f x y = x == '/' && y == '/'
-- >>> Stream.fold Fold.toList $ Stream.uniqBy f input
-- "/a/b"
--
-- Space: @O(1)@
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a
-- uniqBy eq = scanMaybe (FL.uniqBy eq)
uniqBy eq = catMaybes . rollingMap f

    where

    f pre curr =
        case pre of
            Nothing -> Just curr
            Just x -> if x `eq` curr then Nothing else Just curr

-- | Drop repeated elements that are adjacent to each other.
--
-- >>> uniq = Stream.uniqBy (==)
--
{-# INLINE uniq #-}
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
-- uniq = scanMaybe FL.uniq
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

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- >>> input = Stream.fromList [1,3,3,5]
-- >>> Stream.fold Fold.toList $ Stream.deleteBy (==) 3 input
-- [1,3,5]
--
{-# INLINE deleteBy #-}
deleteBy :: Monad m => (a -> a -> Bool) -> a -> Stream m a -> Stream m a
-- deleteBy cmp x = scanMaybe (FL.deleteBy cmp x)
deleteBy cmp x m = fromStreamD $ D.deleteBy cmp x (toStreamD m)

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Same as 'takeWhile' but with a monadic predicate.
--
{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
-- takeWhileM p = scanMaybe (FL.takingEndByM_ (\x -> not <$> p x))
takeWhileM p m = fromStreamD $ D.takeWhileM p $ toStreamD m

-- | Take all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeWhileLast #-}
takeWhileLast :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
takeWhileLast = undefined -- fromStreamD $ D.takeWhileLast n $ toStreamD m

-- | Like 'takeWhile' and 'takeWhileLast' combined.
--
-- O(n) space, where n is the number elements taken from the end.
--
-- /Unimplemented/
{-# INLINE takeWhileAround #-}
takeWhileAround :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
takeWhileAround = undefined -- fromStreamD $ D.takeWhileAround n $ toStreamD m

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
-- dropWhile p = scanMaybe (FL.droppingWhile p)
dropWhile p m = fromStreamD $ D.dropWhile p $ toStreamD m

-- | Same as 'dropWhile' but with a monadic predicate.
--
{-# INLINE dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
-- dropWhileM p = scanMaybe (FL.droppingWhileM p)
dropWhileM p m = fromStreamD $ D.dropWhileM p $ toStreamD m

-- | Drop @n@ elements at the end of the stream.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropLast #-}
dropLast :: -- Monad m =>
    Int -> Stream m a -> Stream m a
dropLast = undefined -- fromStreamD $ D.dropLast n $ toStreamD m

-- | Drop all consecutive elements at the end of the stream for which the
-- predicate is true.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropWhileLast #-}
dropWhileLast :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
dropWhileLast = undefined -- fromStreamD $ D.dropWhileLast n $ toStreamD m

-- | Like 'dropWhile' and 'dropWhileLast' combined.
--
-- O(n) space, where n is the number elements dropped from the end.
--
-- /Unimplemented/
{-# INLINE dropWhileAround #-}
dropWhileAround :: -- Monad m =>
    (a -> Bool) -> Stream m a -> Stream m a
dropWhileAround = undefined -- fromStreamD $ D.dropWhileAround n $ toStreamD m

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- >>> insertBy cmp x = Stream.mergeBy cmp (Stream.fromPure x)
--
-- >>> input = Stream.fromList [1,3,5]
-- >>> Stream.fold Fold.toList $ Stream.insertBy compare 2 input
-- [1,2,3,5]
--
{-# INLINE insertBy #-}
insertBy ::Monad m => (a -> a -> Ordering) -> a -> Stream m a -> Stream m a
insertBy cmp x m = fromStreamD $ D.insertBy cmp x (toStreamD m)

-- | Insert a pure value between successive elements of a stream.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.intersperse ',' input
-- "h,e,l,l,o"
--
{-# INLINE intersperse #-}
intersperse :: Monad m => a -> Stream m a -> Stream m a
intersperse a = fromStreamD . D.intersperse a . toStreamD

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- >>> input = Stream.fromList "hello"
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
-- >> input = Stream.fromList "hello"
-- >> Stream.fold Fold.toList $ Stream.intersperseBySpan 2 (return ',') input
-- "he,ll,o"
--
-- /Unimplemented/
{-# INLINE intersperseBySpan #-}
intersperseBySpan :: -- Monad m =>
    Int -> m a -> Stream m a -> Stream m a
intersperseBySpan _n _f _xs = undefined

-- | Insert an effect and its output after consuming an element of a stream.
--
-- >>> input = Stream.fromList "hello"
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ intersperseSuffix (putChar '.' >> return ',') input
-- h.,e.,l.,l.,o.,"h,e,l,l,o,"
--
-- /Pre-release/
{-# INLINE intersperseSuffix #-}
intersperseSuffix :: Monad m => m a -> Stream m a -> Stream m a
intersperseSuffix m = fromStreamD . D.intersperseSuffix m . toStreamD

-- | Insert a side effect after consuming an element of a stream.
--
-- >>> input = Stream.fromList "hello"
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
-- >>> input = Stream.fromList "hello"
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
-- >>> input = Stream.fromList "hello"
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
-- >>> input = Stream.enumerateFromTo 1 3
-- >>> Stream.fold (Fold.drainMapM print) $ Stream.delay 1 input
-- 1
-- 2
-- 3
--
-- >>> sleep n = liftIO $ threadDelay $ round $ n * 1000000
-- >>> delay n = Stream.intersperseM_ $ sleep n
--
{-# INLINE delay #-}
delay :: MonadIO m => Double -> Stream m a -> Stream m a
delay n = intersperseM_ $ liftIO $ threadDelay $ round $ n * 1000000

-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- >>> input = Stream.enumerateFromTo 1 3
-- >>> Stream.fold (Fold.drainMapM print) $ Stream.delayPost 1 input
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
-- >>> input = Stream.enumerateFromTo 1 3
-- >>> Stream.fold (Fold.drainMapM print) $ Stream.delayPre 1 input
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
    -> Stream m a
    -> Stream m b
reassembleBy = undefined

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- |
-- >>> f = Fold.foldl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- >>> indexed = Stream.postscan f
-- >>> indexed = Stream.zipWith (,) (Stream.enumerateFrom 0)
-- >>> indexedR n = fmap (\(i, a) -> (n - i, a)) . indexed
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- >>> Stream.fold Fold.toList $ Stream.indexed $ Stream.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
{-# INLINE indexed #-}
indexed :: Monad m => Stream m a -> Stream m (Int, a)
-- indexed = scanMaybe FL.indexing
indexed = fromStreamD . D.indexed . toStreamD

-- |
-- >>> f n = Fold.foldl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- >>> indexedR n = Stream.postscan (f n)
--
-- >>> s n = Stream.enumerateFromThen n (n - 1)
-- >>> indexedR n = Stream.zipWith (,) (s n)
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- >>> Stream.fold Fold.toList $ Stream.indexedR 10 $ Stream.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
--
{-# INLINE indexedR #-}
indexedR :: Monad m => Int -> Stream m a -> Stream m (Int, a)
-- indexedR n = scanMaybe (FL.indexingRev n)
indexedR n = fromStreamD . D.indexedR n . toStreamD

-------------------------------------------------------------------------------
-- Time Indexing
-------------------------------------------------------------------------------

-- Note: The timestamp stream must be the second stream in the zip so that the
-- timestamp is generated after generating the stream element and not before.
-- If we do not do that then the following example will generate the same
-- timestamp for first two elements:
--
-- Stream.fold Fold.toList $ Stream.timestamped $ Stream.delay $ Stream.enumerateFromTo 1 3
--
-- | Pair each element in a stream with an absolute timestamp, using a clock of
-- specified granularity.  The timestamp is generated just before the element
-- is consumed.
--
-- >>> Stream.fold Fold.toList $ Stream.timestampWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- [(AbsTime (TimeSpec {sec = ..., nsec = ...}),1),(AbsTime (TimeSpec {sec = ..., nsec = ...}),2),(AbsTime (TimeSpec {sec = ..., nsec = ...}),3)]
--
-- /Pre-release/
--
{-# INLINE timestampWith #-}
timestampWith :: (MonadIO m)
    => Double -> Stream m a -> Stream m(AbsTime, a)
timestampWith g stream = zipWith (flip (,)) stream (absTimesWith g)

-- TBD: check performance vs a custom implementation without using zipWith.
--
-- /Pre-release/
--
{-# INLINE timestamped #-}
timestamped :: (MonadIO m)
    => Stream m a -> Stream m(AbsTime, a)
timestamped = timestampWith 0.01

-- | Pair each element in a stream with relative times starting from 0, using a
-- clock with the specified granularity. The time is measured just before the
-- element is consumed.
--
-- >>> Stream.fold Fold.toList $ Stream.timeIndexWith 0.01 $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- [(RelTime64 (NanoSecond64 ...),1),(RelTime64 (NanoSecond64 ...),2),(RelTime64 (NanoSecond64 ...),3)]
--
-- /Pre-release/Monad
--
{-# INLINE timeIndexWith #-}
timeIndexWith :: (MonadIO m)
    => Double -> Stream m a -> Stream m(RelTime64, a)
timeIndexWith g stream = zipWith (flip (,)) stream (relTimesWith g)

-- | Pair each element in a stream with relative times starting from 0, using a
-- 10 ms granularity clock. The time is measured just before the element is
-- consumed.
--
-- >>> Stream.fold Fold.toList $ Stream.timeIndexed $ Stream.delay 1 $ Stream.enumerateFromTo 1 3
-- [(RelTime64 (NanoSecond64 ...),1),(RelTime64 (NanoSecond64 ...),2),(RelTime64 (NanoSecond64 ...),3)]
--
-- /Pre-release/
--
{-# INLINE timeIndexed #-}
timeIndexed :: (MonadIO m)
    => Stream m a -> Stream m(RelTime64, a)
timeIndexed = timeIndexWith 0.01

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
-- rollingMap f = scanMaybe (FL.slide2 $ Window.rollingMap f)
rollingMap f m = fromStreamD $ D.rollingMap f $ toStreamD m

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: Monad m => (Maybe a -> a -> m b) -> Stream m a -> Stream m b
-- rollingMapM f = scanMaybe (FL.slide2 $ Window.rollingMapM f)
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
-- >>> mapMaybe f = Stream.catMaybes . fmap f
--
{-# INLINE mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f m = fromStreamD $ D.mapMaybe f $ toStreamD m

-- | Like 'mapMaybe' but maps a monadic function.
--
-- Equivalent to:
--
-- >>> mapMaybeM f = Stream.catMaybes . Stream.mapM f
--
-- >>> mapM f = Stream.mapMaybeM (\x -> Just <$> f x)
--
{-# INLINE_EARLY mapMaybeM #-}
mapMaybeM :: Monad m
          => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- >>> lefts = fmap (fromLeft undefined) . Stream.filter isLeft
--
-- /Pre-release/
--
{-# INLINE lefts #-}
lefts :: Monad m => Stream m (Either a b) -> Stream m a
lefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- >>> rights = fmap (fromRight undefined) . Stream.filter isRight
--
-- /Pre-release/
--
{-# INLINE rights #-}
rights :: Monad m => Stream m (Either a b) -> Stream m b
rights = fmap (fromRight undefined) . filter isRight

-- | Remove the either wrapper and flatten both lefts and as well as rights in
-- the output stream.
--
-- >>> both = fmap (either id id)
--
-- /Pre-release/
--
{-# INLINE both #-}
both :: Monad m => Stream m (Either a a) -> Stream m a
both = fmap (either id id)
