-- |
-- Module      : Streamly.Internal.Data.Stream.Transform
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Transform
    (
     -- * Piping
    -- | Pass through a 'Pipe'.
      transform

    -- * Folding
    , foldrS
    , foldrSShared
   -- , foldrT

    -- * Mapping
    -- | Stateless one-to-one maps.
    , map
    , sequence
    , mapM
    , smapM

    -- * Mapping Side Effects (Observation)
    -- | See also the intersperse*_ combinators.
    , trace
    , trace_
    , tap
    --, tapOffsetEvery
    --, tapAsync
    --, tapAsyncK
    --, distributeAsync_
    --, pollCounts

    -- * Scanning By 'Fold'
    , scan
    , scanMany
    , postscan

    -- * Scanning
    -- | Left scans. Stateful, mostly one-to-one maps.
    , scanl'
    , scanlM'
    , scanlMAfter'
    , postscanlMAfter'
    , postscanl'
    , postscanlM'
    , prescanl'
    , prescanlM'
    , scanl1'
    , scanl1M'

    -- XXX Once we have pipes the contravariant transformations can be
    -- represented by attaching pipes before a transformation.
    --
    -- , lscanl'
    -- , lscanlM'
    -- , lscanl1'
    -- , lscanl1M'
    --
    -- , lpostscanl'
    -- , lpostscanlM'
    -- , lprescanl'
    -- , lprescanlM'

    -- * Filtering
    -- | Produce a subset of the stream using criteria based on the values of
    -- the elements. We can use a concatMap and scan for filtering but these
    -- combinators are more efficient and convenient.

    --, with
    , deleteBy
    , filter
    , filterM
    , foldFilter
    , uniq
    , uniqBy
    --, nubBy
    --, nubWindowBy
    --, prune
    --, repeated

    -- * Trimming
    -- | Produce a subset of the stream trimmed at ends.

    , take
    , takeInterval
    --, takeLast
    --, takeLastInterval
    , takeWhile
    , takeWhileM
    --, takeWhileLast
    --, takeWhileAround
    , drop
    , dropInterval
    --, dropLast
    --, dropLastInterval
    , dropWhile
    , dropWhileM
    --, dropWhileLast
    --, dropWhileAround

    -- * Inserting Elements
    -- | Produce a superset of the stream. This is the opposite of
    -- filtering/sampling.  We can always use concatMap and scan for inserting
    -- but these combinators are more efficient and convenient.

    -- Element agnostic (Opposite of sampling)
    , intersperse
    , intersperseM -- XXX naming
    --, intersperseBySpan

    , intersperseSuffix
    , intersperseSuffixBySpan
    --, interjectSuffix

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
   -- , reassembleBy

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

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (fromLeft, isLeft, isRight, fromRight)
import Data.Maybe (isJust, fromJust)

import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Pipe (Pipe)
import Streamly.Internal.Data.Time.Units (TimeUnit64)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.StreamD.Transform as D
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Streamly.Internal.Data.Stream.Bottom
import Streamly.Internal.Data.Stream.Type

import Prelude hiding
       ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence
       , reverse, foldr1 , repeat, scanl, scanl1, zipWith)

#include "Instances.hs"
#include "inline.hs"

--
-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Data.Function ((&))
-- >>> import Prelude hiding ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence, reverse, foldr1 , scanl, scanl1)
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
--
-- >>> hSetBuffering stdout LineBuffering

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
-- > drain $ Stream.mapM putStr $ Stream.unfold Unfold.fromList ["a", "b", "c"]
-- abc
--
-- > :{
--    drain $ Stream.replicateM 10 (return 1)
--      & (fromSerial . Stream.mapM (\x -> threadDelay 1000000 >> print x))
-- :}
-- 1
-- ...
-- 1
--
-- > drain $ Stream.replicateM 10 (return 1)
--  & (fromAsync . Stream.mapM (\x -> threadDelay 1000000 >> print x))
-- @
--
-- /Concurrent (do not use with 'fromParallel' on infinite streams)/
--
-- /Pre-release/
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f m = fromStreamK $ D.toStreamK $ D.mapM f $ toStreamD m

-- | Include only those elements that pass a predicate.
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter p m = fromStreamD $ D.filter p $ toStreamD m

-- | Same as 'filter' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE filterM #-}
filterM :: (Monad m) => (a -> m Bool) -> Stream m a -> Stream m a
filterM p m = fromStreamD $ D.filterM p $ toStreamD m

-- | Use a filtering fold on a stream.
--
-- > Stream.sum $ Stream.foldFilter (Fold.satisfy (> 5)) $ Stream.unfold Unfold.fromList [1..10]
-- 40
--
-- /Pre-release/
--
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> Stream m a -> Stream m b
foldFilter p = fromStreamD . D.foldFilter p . toStreamD

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
-- >>> Stream.fold Fold.toList $ Stream.foldrS Stream.cons Stream.nil $ Stream.unfold Unfold.fromList [1..5]
-- [1,2,3,4,5]
--
-- Find if any element in the stream is 'True':
--
-- >>> Stream.fold Fold.toList $ Stream.foldrS (\x xs -> if odd x then return True else xs) (return False) $ (Stream.unfold Unfold.fromList (2:4:5:undefined) :: Stream IO Int)
-- [True]
--
-- Map (+2) on odd elements and filter out the even elements:
--
-- >>> Stream.fold Fold.toList $ Stream.foldrS (\x xs -> if odd x then (x + 2) `Stream.cons` xs else xs) Stream.nil $ (Stream.unfold Unfold.fromList [1..5] :: Stream IO Int)
-- [3,5,7]
--
-- 'foldrM' can also be represented in terms of 'foldrS', however, the former
-- iStream much more efficient:
--
-- > foldrM f z s = runIdentityT $ foldrS (\x xs -> lift $ f x (runIdentityT xs)) (lift z) s
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

{-# INLINE foldrSShared #-}
foldrSShared ::
     (a -> Stream m b -> Stream m b)
  -> Stream m b
  -> Stream m a
  -> Stream m b
foldrSShared f z xs =
    fromStreamK
        $ K.foldrSShared
            (\y ys -> toStreamK $ f y (fromStreamK ys))
            (toStreamK z)
            (toStreamK xs)

{-# INLINE sequence #-}
sequence :: (Monad m) => Stream m (m a) -> Stream m a
sequence = mapM id

{-# INLINE tap #-}
tap :: Monad m => FL.Fold m a b -> Stream m a -> Stream m a
tap f xs = fromStreamD $ D.tap f (toStreamD xs)

-- | Apply a monadic function to each element flowing through the stream and
-- discard the results.
--
-- @
-- >>> Stream.fold Fold.drain $ Stream.trace print (Stream.unfold Unfold.fromList[1, 2])
-- 1
-- 2
--
-- @
--
-- Compare with 'tap'.
--
-- @since 0.7.0
{-# INLINE trace #-}
trace :: (Monad m) => (a -> m b) -> Stream m a -> Stream m a
trace f = mapM (\x -> void (f x) >> return x)

-- | Perform a side effect before yielding each element of the stream and
-- discard the results.
--
-- @
-- >>> Stream.fold Fold.drain $ Stream.trace_ (print "got here") (Stream.unfold Unfold.fromList[1, 2])
-- "got here"
-- "got here"
--
-- @
--
-- Same as 'interspersePrefix_' but always serial.
--
-- See also: 'trace'
--
-- /Pre-release/
{-# INLINE trace_ #-}
trace_ :: (Monad m) => m b -> Stream m a -> Stream m a
trace_ eff = fromStreamD . D.mapM (\x -> eff >> return x) . toStreamD

------------------------------------------------------------------------------
-- Scanning with a Fold
------------------------------------------------------------------------------

-- | Scan a stream using the given monadic fold.
--
-- >>> Stream.fold Fold.toList $ Stream.takeWhile (< 10) $ Stream.scan Fold.sum (Stream.unfold Unfold.fromList [1..10])
-- [0,1,3,6]
--
-- @since 0.7.0
{-# INLINE scan #-}
scan :: (Monad m) => Fold m a b -> Stream m a -> Stream m b
scan fld m = fromStreamD $ D.scanOnce fld $ toStreamD m

-- | Like 'scan' but restarts scanning afresh when the scanning fold
-- terminates.
--
-- /Pre-release/
{-# INLINE scanMany #-}
scanMany :: (Monad m) => Fold m a b -> Stream m a -> Stream m b
scanMany fld m = fromStreamD $ D.scanMany fld $ toStreamD m

-- | Postscan a stream using the given monadic fold.
--
-- The following example extracts the input stream up to a point where the
-- running average of elements is no more than 10:
--
-- >>> import Data.Maybe (fromJust)
-- >>> let avg = Fold.teeWith (/) Fold.sum (fmap fromIntegral Fold.length)
-- >>> :{
--  Stream.fold Fold.toList
--   $ Stream.map (fromJust . fst)
--   $ Stream.takeWhile (\(_,x) -> x <= 10)
--   $ Stream.postscan (Fold.tee Fold.last avg) (Stream.unfold Unfold.fromList $ enumFromTo 1.0 100.0)
-- :}
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0]
--
-- @since 0.7.0
{-# INLINE postscan #-}
postscan :: (Monad m) => Fold m a b -> Stream m a -> Stream m b
postscan fld = fromStreamD . D.postscanOnce fld . toStreamD

-- XXX this needs to be concurrent
-- XXX because of the use of D.cons for appending, scanlM' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.
--
-- | Like 'scanl'' but with a monadic step function and a monadic seed.
--
-- /Since: 0.4.0/
--
-- /Since: 0.8.0 (signature change)/
{-# INLINE scanlM' #-}
scanlM' :: (Monad m) => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
scanlM' step begin m = fromStreamD $ D.scanlM' step begin $ toStreamD m

-- XXX because of the use of D.cons for appending, scanl' has quadratic
-- complexity when iterated over a stream. We should use StreamK style scanlM'
-- for linear performance on iteration.

-- | Strict left scan. Like 'map', 'scanl'' too is a one to one transformation,
-- however it adds an extra element.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.scanl' (+) 0 $ Stream.unfold Unfold.fromList [1,2,3,4]
-- [0,1,3,6,10]
--
-- @
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.scanl' (flip (:)) [] $ Stream.unfold Unfold.fromList [1,2,3,4]
-- [[],[1],[2,1],[3,2,1],[4,3,2,1]]
--
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
-- Consider the following monolithic example, computing the sum and the product
-- of the elements in a stream in one go using a @foldl'@:
--
-- @
-- >>> Stream.foldl' (\(s, p) x -> (s + x, p * x)) (0,1) $ Stream.unfold Unfold.fromList [1,2,3,4]
-- (10,24)
--
-- @
--
-- Using @scanl'@ we can make iStream modular by computing the sum in the first
-- stage and passing it down to the next stage for computing the product:
--
-- @
-- >>> :{
--   Stream.foldl' (\(_, p) (s, x) -> (s, p * x)) (0,1)
--   $ Stream.scanl' (\(s, _) x -> (s + x, x)) (0,1)
--   $ Stream.unfold Unfold.fromList [1,2,3,4]
-- :}
-- (10,24)
--
-- @
--
-- IMPORTANT: 'scanl'' evaluates the accumulator to WHNF.  To avoid building
-- lazy expressions inside the accumulator, it is recommended that a strict
-- data structure is used for accumulator.
--
-- >>> scanl' step z = scan (Fold.foldl' step z)
-- >>> scanl' f z xs = scanlM' (\a b -> return (f a b)) (return z) xs
-- >>> scanl' f z xs = z `Stream.cons` postscanl' f z xs
--
-- See also: 'usingStateT'
--
-- @since 0.2.0
{-# INLINE scanl' #-}
scanl' :: (Monad m) => (b -> a -> b) -> b -> Stream m a -> Stream m b
scanl' step z m = fromStreamD $ D.scanl' step z $ toStreamD m

-- | Like 'scanl'' but does not stream the initial value of the accumulator.
--
-- >>> postscanl' step z = postscan (Fold.foldl' step z)
-- >>> postscanl' f z = postscanlM' (\a b -> return (f a b)) (return z)
-- >>> postscanl' f z xs = Stream.drop 1 $ Stream.scanl' f z xs
--
-- @since 0.7.0
{-# INLINE postscanl' #-}
postscanl' :: (Monad m) => (b -> a -> b) -> b -> Stream m a -> Stream m b
postscanl' step z m = fromStreamD $ D.postscanl' step z $ toStreamD m

-- XXX prescanl does not sound very useful, enable only if there is a
-- compelling use case.
--
-- | Like scanl' but does not stream the final value of the accumulator.
--
-- /Pre-release/
{-# INLINE prescanl' #-}
prescanl' :: (Monad m) => (b -> a -> b) -> b -> Stream m a -> Stream m b
prescanl' step z m = fromStreamD $ D.prescanl' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like prescanl' but with a monadic step function and a monadic seed.
--
-- /Pre-release/
{-# INLINE prescanlM' #-}
prescanlM' :: (Monad m) => (b -> a -> m b) -> m b -> Stream m a -> Stream m b
prescanlM' step z m = fromStreamD $ D.prescanlM' step z $ toStreamD m

-- XXX this needs to be concurrent
-- | Like 'scanl1'' but with a monadic step function.
--
-- @since 0.6.0
{-# INLINE scanl1M' #-}
scanl1M' :: (Monad m) => (a -> a -> m a) -> Stream m a -> Stream m a
scanl1M' step m = fromStreamD $ D.scanl1M' step $ toStreamD m

-- | Like 'scanl'' but for a non-empty stream. The first element of the stream
-- is used as the initial value of the accumulator. Does nothing if the stream
-- is empty.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.scanl1' (+) $ Stream.unfold Unfold.fromList [1,2,3,4]
-- [1,3,6,10]
--
-- @
--
-- @since 0.6.0
{-# INLINE scanl1' #-}
scanl1' :: (Monad m) => (a -> a -> a) -> Stream m a -> Stream m a
scanl1' step m = fromStreamD $ D.scanl1' step $ toStreamD m

-- | Drop repeated elements that are adjacent to each other using the supplied
-- comparison function.
--
-- @uniq = uniqBy (==)
--
-- To strip duplicate path separators:
--
-- @
-- f x y = x == '/' && x == y
-- Stream.fold Fold.toList $ Stream.uniqBy f $ Stream.unfold Unfold.fromList "//a//b"
-- "/a/b"
-- @
--
-- Space: @O(1)@
--
-- See also: 'nubBy'.
--
-- /Pre-release/
--
{-# INLINE uniqBy #-}
uniqBy :: (Monad m) =>
    (a -> a -> Bool) -> Stream m a -> Stream m a
uniqBy eq = catMaybes . rollingMap f

    where

    f pre curr =
        case pre of
            Nothing -> Just curr
            Just x -> if x `eq` curr then Nothing else Just curr

-- | Drop repeated elements that are adjacent to each other.
--
-- @since 0.6.0
{-# INLINE uniq #-}
uniq :: (Eq a, Monad m) => Stream m a -> Stream m a
uniq = fromStreamD . D.uniq . toStreamD

-- | Deletes the first occurrence of the element in the stream that satisfies
-- the given equality predicate.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.deleteBy (==) 3 $ Stream.unfold Unfold.fromList [1,3,3,5]
-- [1,3,5]
--
-- @
--
-- @since 0.6.0
{-# INLINE deleteBy #-}
deleteBy :: (Monad m) => (a -> a -> Bool) -> a -> Stream m a -> Stream m a
deleteBy cmp x m = fromStreamD $ D.deleteBy cmp x (toStreamD m)

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Same as 'takeWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE takeWhileM #-}
takeWhileM :: (Monad m) => (a -> m Bool) -> Stream m a -> Stream m a
takeWhileM p m = fromStreamD $ D.takeWhileM p $ toStreamD m

-- | @takeInterval duration@ yields stream elements upto specified time
-- @duration@. The duration starts when the stream is evaluated for the first
-- time, before the first element is yielded. The time duration is checked
-- before generating each element, if the duration has expired the stream
-- stops.
--
-- The total time taken in executing the stream is guaranteed to be /at least/
-- @duration@, however, because the duration is checked before generating an
-- element, the upper bound is indeterminate and depends on the time taken in
-- generating and processing the last element.
--
-- No element is yielded if the duration is zero. At least one element is
-- yielded if the duration is non-zero.
--
-- /Pre-release/
--
{-# INLINE takeInterval #-}
takeInterval ::(MonadIO m, TimeUnit64 d) => d -> Stream m a -> Stream m a
takeInterval d = fromStreamD . D.takeByTime d . toStreamD

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
--
-- @since 0.1.0
{-# INLINE dropWhile #-}
dropWhile :: (Monad m) => (a -> Bool) -> Stream m a -> Stream m a
dropWhile p m = fromStreamD $ D.dropWhile p $ toStreamD m

-- | Same as 'dropWhile' but with a monadic predicate.
--
-- @since 0.4.0
{-# INLINE dropWhileM #-}
dropWhileM :: (Monad m) => (a -> m Bool) -> Stream m a -> Stream m a
dropWhileM p m = fromStreamD $ D.dropWhileM p $ toStreamD m

-- | @dropInterval duration@ drops stream elements until specified @duration@ has
-- passed.  The duration begins when the stream is evaluated for the first
-- time. The time duration is checked /after/ generating a stream element, the
-- element is yielded if the duration has expired otherwise it is dropped.
--
-- The time elapsed before starting to generate the first element is /aStream most/
-- @duration@, however, because the duration expiry is checked after the
-- element is generated, the lower bound is indeterminate and depends on the
-- time taken in generating an element.
--
-- All elements are yielded if the duration is zero.
--
-- /Pre-release/
--
{-# INLINE dropInterval #-}
dropInterval ::(MonadIO m, TimeUnit64 d) => d -> Stream m a -> Stream m a
dropInterval d = fromStreamD . D.dropByTime d . toStreamD

------------------------------------------------------------------------------
-- Inserting Elements
------------------------------------------------------------------------------

-- | @insertBy cmp elem stream@ inserts @elem@ before the first element in
-- @stream@ that is less than @elem@ when compared using @cmp@.
--
-- @
-- insertBy cmp x = 'mergeBy' cmp ('fromPure' x)
-- @
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.insertBy compare 2 $ Stream.unfold Unfold.fromList [1,3,5]
-- [1,2,3,5]
--
-- @
--
-- @since 0.6.0
{-# INLINE insertBy #-}
insertBy ::(Monad m) => (a -> a -> Ordering) -> a -> Stream m a -> Stream m a
insertBy cmp x m = fromStreamD $ D.insertBy cmp x (toStreamD m)

-- | Insert a pure value between successive elements of a stream.
--
-- >>> Stream.fold Fold.toList $ Stream.intersperse ',' $ Stream.unfold Unfold.fromList "hello"
-- "h,e,l,l,o"
--
-- @since 0.7.0
{-# INLINE intersperse #-}
intersperse :: (Monad m) => a -> Stream m a -> Stream m a
intersperse a = fromStreamD . D.intersperse a . toStreamD

-- | Insert a side effect before consuming an element of a stream except the
-- first one.
--
-- >>> Stream.fold Fold.drain $ Stream.trace putChar $ Stream.intersperseM_ (putChar '.') $ Stream.unfold Unfold.fromList "hello"
-- h.e.l.l.o
--
-- /Pre-release/
{-# INLINE intersperseM_ #-}
intersperseM_ :: (Monad m) => m b -> Stream m a -> Stream m a
intersperseM_ m = fromStreamD . D.intersperseM_ m . toStreamD

-- | Insert an effect and its output after consuming an element of a stream.
--
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ intersperseSuffix (putChar '.' >> return ',') $ Stream.unfold Unfold.fromList "hello"
-- h.,e.,l.,l.,o.,"h,e,l,l,o,"
--
-- /Pre-release/
{-# INLINE intersperseSuffix #-}
intersperseSuffix :: (Monad m) => m a -> Stream m a -> Stream m a
intersperseSuffix m = fromStreamD . D.intersperseSuffix m . toStreamD

-- | Insert a side effect after consuming an element of a stream.
--
-- @
-- >>> Stream.fold Fold.toList $ Stream.intersperseSuffix_ (threadDelay 1000000) $ Stream.unfold Unfold.fromList "hello"
-- "hello"
--
-- @
--
-- /Pre-release/
--
{-# INLINE intersperseSuffix_ #-}
intersperseSuffix_ :: (Monad m) => m b -> Stream m a -> Stream m a
intersperseSuffix_ m = fromStreamD . D.intersperseSuffix_ m . toStreamD

-- XXX Use an offset argument, like tapOffsetEvery
--
-- | Like 'intersperseSuffix' but intersperses an effectful action into the
-- input stream after every @n@ elements and after the last element.
--
-- >>> Stream.fold Fold.toList $ Stream.intersperseSuffixBySpan 2 (return ',') $ Stream.unfold Unfold.fromList "hello"
-- "he,ll,o,"
--
-- /Pre-release/
--
{-# INLINE intersperseSuffixBySpan #-}
intersperseSuffixBySpan :: (Monad m)
    => Int -> m a -> Stream m a -> Stream m a
intersperseSuffixBySpan n eff =
    fromStreamD . D.intersperseSuffixBySpan n eff . toStreamD

-- | Insert a side effect before consuming an element of a stream.
--
-- >>> Stream.fold Fold.toList $ Stream.trace putChar $ Stream.interspersePrefix_ (putChar '.' >> return ',') $ Stream.unfold Unfold.fromList "hello"
-- .h.e.l.l.o"hello"
--
-- Same as 'trace_' buStream may be concurrent.
--
-- /Concurrent/
--
-- /Pre-release/
--
{-# INLINE interspersePrefix_ #-}
interspersePrefix_ :: (Monad m) => m b -> Stream m a -> Stream m a
interspersePrefix_ m = mapM (\x -> void m >> return x)

------------------------------------------------------------------------------
-- Inserting Time
------------------------------------------------------------------------------

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds before consuming an element of the
-- stream except the first one.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delay 1 $ Stream.unfold Unfold.fromList $ enumFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- @since 0.8.0
--
{-# INLINE delay #-}
delay :: (MonadIO m) => Double -> Stream m a -> Stream m a
delay n = intersperseM_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial.
--
-- | Introduce a delay of specified seconds after consuming an element of a
-- stream.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delayPost 1 $ Stream.unfold Unfold.fromList $ enumFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE delayPost #-}
delayPost :: (MonadIO m) => Double -> Stream m a -> Stream m a
delayPost n = intersperseSuffix_ $ liftIO $ threadDelay $ round $ n * 1000000

-- Note: delay must be serial, that's why 'trace_' is used.
--
-- | Introduce a delay of specified seconds before consuming an element of a
-- stream.
--
-- >>> Stream.mapM_ print $ Stream.timestamped $ Stream.delayPre 1 $ Stream.unfold Unfold.fromList $ enumFromTo 1 3
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),1)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),2)
-- (AbsTime (TimeSpec {sec = ..., nsec = ...}),3)
--
-- /Pre-release/
--
{-# INLINE delayPre #-}
delayPre :: (MonadIO m) => Double -> Stream m a -> Stream m a
delayPre n = trace_ $ liftIO $ threadDelay $ round $ n * 1000000

------------------------------------------------------------------------------
-- Position Indexing
------------------------------------------------------------------------------

-- |
-- > indexed = Stream.postscanl' (\(i, _) x -> (i + 1, x)) (-1,undefined)
-- > indexed = Stream.zipWith (,) (Stream.enumerateFrom 0)
--
-- Pair each element in a stream with its index, starting from index 0.
--
-- >>> Stream.fold Fold.toList $ Stream.indexed $ Stream.unfold Unfold.fromList "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
-- @since 0.6.0
{-# INLINE indexed #-}
indexed :: (Monad m) => Stream m a -> Stream m (Int, a)
indexed = fromStreamD . D.indexed . toStreamD

-- |
-- > indexedR n = Stream.postscanl' (\(i, _) x -> (i - 1, x)) (n + 1,undefined)
-- > indexedR n = Stream.zipWith (,) (Stream.unfold Unfold.fromList $ enumFromThen n (n - 1))
--
-- Pair each element in a stream with its index, starting from the
-- given index @n@ and counting down.
--
-- >>> Stream.fold Fold.toList $ Stream.indexedR 10 $ Stream.unfold Unfold.fromList "hello"
-- [(10,'h'),(9,'e'),(8,'l'),(7,'l'),(6,'o')]
--
-- @since 0.6.0
{-# INLINE indexedR #-}
indexedR :: (Monad m) => Int -> Stream m a -> Stream m (Int, a)
indexedR n = fromStreamD . D.indexedR n . toStreamD

------------------------------------------------------------------------------
-- Searching
------------------------------------------------------------------------------

-- | Find all the indices where the value of the element in the stream is equal
-- to the given value.
--
-- > elemIndices a = findIndices (== a)
--
-- @since 0.5.0
{-# INLINE elemIndices #-}
elemIndices :: (Eq a, Monad m) => a -> Stream m a -> Stream m Int
elemIndices a = findIndices (== a)

------------------------------------------------------------------------------
-- Rolling map
------------------------------------------------------------------------------

-- XXX this is not a one-to-one map so calling iStream map may not be right.
-- We can perhaps call it zipWithTail or rollWith.
--
-- | Apply a function on every two successive elements of a stream. The first
-- argument of the map function is the previous element and the second argument
-- is the current element. When the current element is the first element, the
-- previous element is 'Nothing'.
--
-- /Pre-release/
--
{-# INLINE rollingMap #-}
rollingMap :: (Monad m) => (Maybe a -> a -> b) -> Stream m a -> Stream m b
rollingMap f m = fromStreamD $ D.rollingMap f $ toStreamD m

-- | Like 'rollingMap' but with an effectful map function.
--
-- /Pre-release/
--
{-# INLINE rollingMapM #-}
rollingMapM :: (Monad m) => (Maybe a -> a -> m b) -> Stream m a -> Stream m b
rollingMapM f m = fromStreamD $ D.rollingMapM f $ toStreamD m

-- | Like 'rollingMap' but requires at least two elements in the stream,
-- returns an empty stream otherwise.
--
-- This is the stream equivalent of the list idiom @zipWith f xs (tail xs)@.
--
-- /Pre-release/
--
{-# INLINE rollingMap2 #-}
rollingMap2 :: (Monad m) => (a -> a -> b) -> Stream m a -> Stream m b
rollingMap2 f m = fromStreamD $ D.rollingMap2 f $ toStreamD m

------------------------------------------------------------------------------
-- Maybe Streams
------------------------------------------------------------------------------

-- | Map a 'Maybe' returning function to a stream, filter out the 'Nothing'
-- elements, and return a stream of values extracted from 'Just'.
--
-- Equivalent to:
--
-- @
-- mapMaybe f = Stream.map 'fromJust' . Stream.filter 'isJust' . Stream.map f
-- @
--
-- @since 0.3.0
{-# INLINE mapMaybe #-}
mapMaybe :: (Monad m) => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f m = fromStreamD $ D.mapMaybe f $ toStreamD m

-- | Like 'mapMaybe' buStream maps a monadic function.
--
-- Equivalent to:
--
-- @
-- mapMaybeM f = Stream.map 'fromJust' . Stream.filter 'isJust' . Stream.mapM f
-- @
--
-- /Concurrent (do not use with 'fromParallel' on infinite streams)/
--
-- @since 0.3.0
{-# INLINE_EARLY mapMaybeM #-}
mapMaybeM :: (Monad m)
          => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

-- | In a stream of 'Maybe's, discard 'Nothing's and unwrap 'Just's.
--
-- /Pre-release/
--
{-# INLINE catMaybes #-}
catMaybes :: (Monad m) => Stream m (Maybe a) -> Stream m a
catMaybes = fmap fromJust . filter isJust

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- | Discard 'Right's and unwrap 'Left's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE lefts #-}
lefts :: (Monad m) => Stream m (Either a b) -> Stream m a
lefts = fmap (fromLeft undefined) . filter isLeft

-- | Discard 'Left's and unwrap 'Right's in an 'Either' stream.
--
-- /Pre-release/
--
{-# INLINE rights #-}
rights :: (Monad m) => Stream m (Either a b) -> Stream m b
rights = fmap (fromRight undefined) . filter isRight

-- | Remove the either wrapper and flatten both lefts and as well as rights in
-- the output stream.
--
-- /Pre-release/
--
{-# INLINE both #-}
both :: Monad m => Stream m (Either a a) -> Stream m a
both = fmap (either id id)
