{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Top
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Top level module that can depend on all other lower level Stream modules.
--
-- Design notes:
--
-- The order of arguments in the join operations should ideally be opposite. It
-- should be such that the infinite stream is the last one. The transformation
-- should be on the last argument, so if you curry the functions with all other
-- arguments we get a @Stream -> Stream@ function. The first stream argument
-- may be considered as a config or modifier for the operation.
--
-- Benefit of changing the order is that we get a more intuitive Stream ->
-- Stream transformation after currying all other arguments. The inner loop
-- streams become arguments for the transformation, more like local modifiers
-- for the global outer stream as the last argument. Thus we can continue using
-- transformations on the outer stream in a composed pipeline. Otherwise we can
-- use flip to flip the order.
--
-- The fact that the inner stream can be used in the loop multiple times also
-- tells that this is not the real effectful stream, it is more like a pure
-- stream or an array. In fact we may consider using an Identity streams as
-- inner streams in which case these functions will not look nice.
--
-- Downsides:
--
-- * Maybe less intuitive to think about, because we usually think the first
--   stream as the outer loop and second as the inner.
-- * Zip and merge operations will continue using the opposite order.
-- * Need to change the order of cross, crossWith operations as well
-- * It will be inconsistent with Data.List. The functions cannot be used as
-- intuitive operators.
--
-- The choice is similar to concatMap vs bind. concatMap is pipeline
-- composition friendly but bind is user intuition friendly. Another option is
-- to have other functions with a different argument order e.g. flippedCross
-- instead of cross.
--
-- If we change the order we have to make sure that we have a consistent
-- convention for set-like and the cross join operations.

module Streamly.Internal.Data.Stream.Top
    (
    -- * Sampling
    -- | Value agnostic filtering.
      strideFromThen

    -- * Straight Joins
    -- | These are set-like operations but not exactly set operations because
    -- streams are not necessarily sets, they may have duplicated elements.
    -- These operations are generic i.e. they work on streams of unconstrained
    -- types, therefore, they have quadratic performance characterstics. For
    -- better performance using Set or Map structures see the
    -- Streamly.Internal.Data.Stream.Container module.
    , intersectBy
    , deleteFirstsBy
    , unionBy

    -- Set like operations on sorted streams
    , sortedIntersectBy
    , sortedDeleteFirstsBy
    , sortedUnionBy

    -- * Cross Joins
    , innerJoin

    -- Joins on sorted stream
    , innerSortedJoin
    , leftSortedJoin
    , outerSortedJoin
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Stream.Type (Stream(..), Step(..), cross)

import qualified Data.List as List
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.Nesting as Stream
import qualified Streamly.Internal.Data.Stream.Transform as Stream

import Prelude hiding (filter, zipWith, concatMap, concat)

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Sampling
------------------------------------------------------------------------------

-- XXX We can implement this using addition instead of "mod" to make it more
-- efficient.

-- | @strideFromthen offset stride@ takes the element at @offset@ index and
-- then every element at strides of @stride@.
--
-- >>> Stream.fold Fold.toList $ Stream.strideFromThen 2 3 $ Stream.enumerateFromTo 0 10
-- [2,5,8]
--
{-# INLINE strideFromThen #-}
strideFromThen :: Monad m => Int -> Int -> Stream m a -> Stream m a
strideFromThen offset stride =
    Stream.with Stream.indexed Stream.filter
        (\(i, _) -> i >= offset && (i - offset) `mod` stride == 0)

------------------------------------------------------------------------------
-- SQL Joins
------------------------------------------------------------------------------
--
-- Some references:
-- * https://en.wikipedia.org/wiki/Relational_algebra
-- * https://en.wikipedia.org/wiki/Join_(SQL)

-- TODO: OrdSet/IntSet/hashmap based versions of these. With Eq only
-- constraint, the best would be to use an Array with linear search. If the
-- second stream is sorted we can also use a binary search, using Ord
-- constraint or an ordering function.
--
-- For Storables we can cache the second stream into an unboxed array for
-- possibly faster access/compact representation?
--
-- If we do not want to keep the stream in memory but always read it from the
-- source (disk/network) every time we iterate through it then we can do that
-- too by reading the stream every time, the stream must have immutable state
-- in that case and the user is responsible for the behavior if the stream
-- source changes during iterations. We can also use an Unfold instead of
-- stream. We probably need a way to distinguish streams that can be read
-- mutliple times without any interference (e.g. unfolding a stream using an
-- immutable handle would work i.e. using pread/pwrite instead of maintaining
-- an offset in the handle).

-- XXX We can do this concurrently.
-- XXX If the second stream is sorted and passed as an Array we could use
-- binary search if we have an Ord instance or Ordering returning function. The
-- time complexity would then become (m x log n).

-- | Like 'cross' but emits only those tuples where @a == b@ using the supplied
-- equality predicate. This is essentially a @cross intersection@ of two
-- streams.
--
-- Definition:
--
-- >>> innerJoin eq s1 s2 = Stream.filter (\(a, b) -> a `eq` b) $ Stream.cross s1 s2
--
-- The second (inner) stream must be finite. Moreover, it must be either pure
-- or capable of multiple evaluations. If not then the caller should cache it
-- in an 'Data.Array.Array', if the type does not have an 'Unbox' instance then
-- use the Generic 'Data.Array.Generic.Array'. Convert the array to stream
-- before calling this function. Caching may also improve performance if the
-- stream is expensive to evaluate.
--
-- If you care about performance this function should be your last choice among
-- all inner joins. 'Streamly.Internal.Data.Unfold.innerJoin' is a much faster
-- fused alternative. 'innerSortedJoin' is a faster alternative when streams
-- are sorted. 'innerOrdJoin' is an order of magnitude faster alternative when
-- the type has an 'Ord' instance.
--
-- Note: Conceptually, this is a commutative operation. Result includes all the
-- elements from the left and the right stream. The order of streams can be
-- changed without affecting results, except for the ordering within the tuple.
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE innerJoin #-}
innerJoin :: Monad m =>
    (a -> b -> Bool) -> Stream m a -> Stream m b -> Stream m (a, b)
innerJoin eq s1 s2 = Stream.filter (\(a, b) -> a `eq` b) $ cross s1 s2
{-
innerJoin eq s1 s2 = do
    -- ConcatMap works faster than bind
    Stream.concatMap (\a ->
        Stream.concatMap (\b ->
            if a `eq` b
            then Stream.fromPure (a, b)
            else Stream.nil
            ) s2
        ) s1
-}

-- | A more efficient 'innerJoin' for sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE innerSortedJoin #-}
innerSortedJoin ::
    (a -> b -> Ordering) -> Stream m a -> Stream m b -> Stream m (a, b)
innerSortedJoin = undefined

-- | A more efficient 'leftJoin' for sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE leftSortedJoin #-}
leftSortedJoin :: -- Monad m =>
    (a -> b -> Ordering) -> Stream m a -> Stream m b -> Stream m (a, Maybe b)
leftSortedJoin _eq _s1 _s2 = undefined

-- | A more efficient 'outerJoin' for sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE outerSortedJoin #-}
outerSortedJoin :: -- Monad m =>
       (a -> b -> Ordering)
    -> Stream m a
    -> Stream m b
    -> Stream m (Maybe a, Maybe b)
outerSortedJoin _eq _s1 _s2 = undefined

------------------------------------------------------------------------------
-- Set operations (special joins)
------------------------------------------------------------------------------
--
-- TODO: OrdSet/IntSet/hashmap based versions of these. With Eq only constraint
-- the best would be to use an Array with linear search. If the second stream
-- is sorted we can also use a binary search, using Ord constraint.

-- | Keep only those elements in the first stream that are present in the
-- second stream too. The second stream is folded to a container using the
-- supplied fold and then the elements in the container are looked up using the
-- supplied lookup function.
--
-- The first stream must be finite and must not block.
{-# INLINE filterStreamWith #-}
filterStreamWith :: Monad m =>
       Fold m a (f a)
    -> (a -> f a -> Bool)
    -> Stream m a
    -> Stream m a
    -> Stream m a
filterStreamWith fld member s1 s2 =
    Stream.concatEffect
        $ do
            xs <- Stream.fold fld s2
            return $ Stream.filter (`member` xs) s1

-- XXX instead of folding the second stream to a list we could use it directly.
-- If the user wants they can generate the stream from an array and also call
-- uniq or nub on it. We can provide a convenience Stream -> Stream to cache
-- a finite stream in an array and serve it from the cache. The user can decide
-- what is best based on the context. They can also choose to use a boxed or
-- unboxed array for caching. To force caching we can make the second stream
-- monad type Identity. But that may be less flexible. One option is to use
-- cachedIntersectBy etc for automatic caching.

-- | 'intersectBy' returns a subsequence of the first stream which intersects
-- with the second stream. Note that this is not a commutative operation unlike
-- a set intersection, because of duplicate elements in the stream the order of
-- the streams matters. This is similar to 'Data.List.intersectBy'. Note that
-- intersectBy is a special case of 'innerJoin'.
--
-- >>> f s1 s2 = Stream.fold Fold.toList $ Stream.intersectBy (==) (Stream.fromList s1) (Stream.fromList s2)
-- >>> f [1,3,4,4,5]) [2,3,4,5,5]
-- [3,4,4,5]
--
-- First stream can be infinite, the second stream must be finite and must be
-- capable of multiple evaluations.
--
-- Space: O(n) where @n@ is the number of elements in the second stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE intersectBy #-}
intersectBy :: Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
intersectBy eq =
    -- XXX Use an (unboxed) array instead.
    filterStreamWith
        (Fold.scanMaybe (Fold.uniqBy eq) Fold.toListRev)
        (List.any . eq)

-------------------------------------------------------------------------------
-- Intersection of sorted streams
-------------------------------------------------------------------------------

-- XXX The sort order is not important as long both the streams have the same
-- sort order. We need to move only in one direction in each stream.
-- XXX Fix the argument order to use the same behavior as intersectBy.

-- | Like 'intersectBy' but assumes that the input streams are sorted in
-- ascending order. To use it on streams sorted in descending order pass an
-- inverted comparison function returning GT for less than and LT for greater
-- than.
--
-- Both streams can be infinite.
--
-- Space: O(1)
--
-- Time: O(m+n)
--
-- /Pre-release/
{-# INLINE_NORMAL sortedIntersectBy #-}
sortedIntersectBy :: Monad m =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
sortedIntersectBy cmp (Stream stepa ta) (Stream stepb tb) =
    Stream step
        ( ta -- left stream state
        , tb -- right stream state
        , Nothing -- left value
        , Nothing -- right value
        )

    where

    {-# INLINE_LATE step #-}
    -- step 1, fetch the first value
    step gst (sa, sb, Nothing, b) = do
        r <- stepa gst sa
        return $ case r of
            Yield a sa' -> Skip (sa', sb, Just a, b) -- step 2/3
            Skip sa'    -> Skip (sa', sb, Nothing, b)
            Stop        -> Stop

    -- step 2, fetch the second value
    step gst (sa, sb, a@(Just _), Nothing) = do
        r <- stepb gst sb
        return $ case r of
            Yield b sb' -> Skip (sa, sb', a, Just b) -- step 3
            Skip sb'    -> Skip (sa, sb', a, Nothing)
            Stop        -> Stop

    -- step 3, compare the two values
    step _ (sa, sb, Just a, Just b) = do
        let res = cmp a b
        return $ case res of
            GT -> Skip (sa, sb, Just a, Nothing) -- step 2
            LT -> Skip (sa, sb, Nothing, Just b) -- step 1
            EQ -> Yield a (sa, sb, Nothing, Just b) -- step 1

-- | Returns a subsequence of the first stream, deleting first occurrences of
-- those elements that are present in the second stream. Note that this is not
-- a commutative operation. This is similar to the 'Data.List.deleteFirstsBy'.
--
-- >>> f xs ys = Stream.fold Fold.toList $ Stream.deleteFirstsBy (==) (Stream.fromList xs) (Stream.fromList ys)
-- >>> f [1,2,2,3,3,5] [1,2,2,3,4]
-- [2,3,5]
--
-- The following holds:
--
-- > deleteFirstsBy (==) (Stream.nub s2 `append` s1) s2 === s1
-- > deleteFirstsBy (==) (Stream.nub s2 `interleave` s1) s2 === s1
--
-- First stream can be infinite, second stream must be finite.
--
-- Space: O(m) where @m@ is the number of elements in the first stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE deleteFirstsBy #-}
deleteFirstsBy :: Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
deleteFirstsBy eq s2 s1 =
    -- XXX s2 can be a sorted mutable array and we can use binary
    -- search to find. Mark the element deleted, count the deletions
    -- and reconsolidate the array when a min number of elements is
    -- deleted.

    -- XXX Use StreamK or list as second argument instead of Stream to avoid
    -- concatEffect?
    Stream.concatEffect $ do
        xs <- Stream.toList s1
        -- It reverses the list but that is fine.
        let del x =
                List.foldl' (\(ys,res) y ->
                    if x `eq` y
                    then (ys, True)
                    else (x:ys, res)) ([], False)
            g (ys,_) x =
                let (ys1, deleted) = del x ys
                 in if deleted
                    then (ys1, Nothing)
                    else (ys1, Just x)
         in return
                $ Stream.catMaybes
                $ fmap snd
                $ Stream.postscanl' g (xs, Nothing) s2

-- | A more efficient 'deleteFirstsBy' for streams sorted in ascending order.
--
-- Both streams can be infinite.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE sortedDeleteFirstsBy #-}
sortedDeleteFirstsBy :: -- (Monad m) =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
sortedDeleteFirstsBy _eq _s1 _s2 = undefined

-- XXX Remove the MonadIO constraint. We can just cache one stream and then
-- implement using differenceEqBy.

-- | Returns the first stream appended with those unique elements from the
-- second stream that are not already present in the first stream. Note that
-- this is not a commutative operation unlike a set union, argument order
-- matters. The behavior is similar to the 'Data.List.unionBy'.
--
-- Equivalent to the following except that @s2@ is evaluated only once:
--
-- >>> unionBy eq s1 s2 = s1 `Stream.append` Stream.deleteFirstsBy eq s1 (Stream.nub s2)
--
-- Example:
--
-- >>> f s1 s2 = Stream.fold Fold.toList $ Stream.unionBy (==) (Stream.fromList s1) (Stream.fromList s2)
-- >>> f [1,2,2,4] [1,1,2,3,3]
-- [1,2,2,4,3]
--
-- First stream can be infinite, but second stream must be finite. Note that if
-- the first stream is infinite the union means just the first stream. Thus
-- union is useful only when both streams are finite. See 'sortedUnionBy' where
-- union can work on infinite streams if they are sorted.
--
-- Space: O(n)
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE unionBy #-}
unionBy :: MonadIO m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
unionBy eq s2 s1 =
    Stream.concatEffect
        $ do
            -- XXX use a rewrite rule such that if a list converted to stream
            -- is passed to unionBy then this becomes an identity operation.
            xs <- Stream.fold Fold.toList  s1
            -- XXX we can use postscanlMAfter' instead of IORef
            ref <- liftIO $ newIORef $! List.nubBy eq xs
            let f x = do
                    liftIO $ modifyIORef' ref (List.deleteBy eq x)
                    return x
                s3 = Stream.concatEffect
                        $ do
                            xs1 <- liftIO $ readIORef ref
                            return $ Stream.fromList xs1
            return $ Stream.mapM f s2 `Stream.append` s3

-- | A more efficient 'unionBy' for sorted streams.
--
-- Note that the behavior is different from 'unionBy'. In 'unionBy' we append
-- the unique elements from second stream only after exhausting the first one
-- whereas in sorted streams we can determine unique elements early even when
-- we are going through the first stream. Thus the result is an interleaving of
-- the two streams, merging those elements from the second stream that are not
-- present in the first.
--
-- Space: O(1)
--
-- Both streams can be infinite.
--
-- /Unimplemented/
{-# INLINE sortedUnionBy #-}
sortedUnionBy :: -- (Monad m) =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
sortedUnionBy _eq _s1 _s2 = undefined
