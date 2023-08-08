{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Top
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Top level module that can depend on all other lower level Stream modules.

module Streamly.Internal.Data.Stream.StreamD.Top
    (
    -- * Transformation
    -- ** Sampling
    -- | Value agnostic filtering.
      strideFromThen

    -- * Nesting
    -- ** Set like operations
    -- | These are not exactly set operations because streams are not
    -- necessarily sets, they may have duplicated elements. These operations
    -- are generic i.e. they work on streams of unconstrained types, therefore,
    -- they have quadratic performance characterstics. For better performance
    -- using Set structures see the Streamly.Internal.Data.Stream.Container
    -- module.
    , filterInStreamGenericBy
    , deleteInStreamGenericBy
    , unionWithStreamGenericBy

    -- ** Set like operations on sorted streams
    , filterInStreamAscBy
    , deleteInStreamAscBy
    , unionWithStreamAscBy

    -- ** Join operations
    , joinInnerGeneric

    -- * Joins on sorted stream
    , joinInnerAscBy
    , joinLeftAscBy
    , joinOuterAscBy
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Stream.Common ()
import Streamly.Internal.Data.Stream.StreamD.Type (Stream, cross)

import qualified Data.List as List
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.StreamD.Type as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Nesting as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Transform as Stream

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

-- | Like 'cross' but emits only those tuples where @a == b@ using the
-- supplied equality predicate.
--
-- Definition:
--
-- >>> joinInnerGeneric eq s1 s2 = Stream.filter (\(a, b) -> a `eq` b) $ Stream.cross s1 s2
--
-- You should almost always prefer @joinInnerOrd@ over 'joinInnerGeneric' if
-- possible. @joinInnerOrd@ is an order of magnitude faster but may take more
-- space for caching the second stream.
--
-- See 'Streamly.Internal.Data.Unfold.joinInnerGeneric' for a much faster fused
-- alternative.
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE joinInnerGeneric #-}
joinInnerGeneric :: Monad m =>
    (a -> b -> Bool) -> Stream m a -> Stream m b -> Stream m (a, b)
joinInnerGeneric eq s1 s2 = Stream.filter (\(a, b) -> a `eq` b) $ cross s1 s2
{-
joinInnerGeneric eq s1 s2 = do
    -- ConcatMap works faster than bind
    Stream.concatMap (\a ->
        Stream.concatMap (\b ->
            if a `eq` b
            then Stream.fromPure (a, b)
            else Stream.nil
            ) s2
        ) s1
-}

-- | A more efficient 'joinInner' for sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE joinInnerAscBy #-}
joinInnerAscBy ::
    (a -> b -> Ordering) -> Stream m a -> Stream m b -> Stream m (a, b)
joinInnerAscBy = undefined

-- | A more efficient 'joinLeft' for sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE joinLeftAscBy #-}
joinLeftAscBy :: -- Monad m =>
    (a -> b -> Ordering) -> Stream m a -> Stream m b -> Stream m (a, Maybe b)
joinLeftAscBy _eq _s1 _s2 = undefined

-- | A more efficient 'joinOuter' for sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE joinOuterAscBy #-}
joinOuterAscBy :: -- Monad m =>
       (a -> b -> Ordering)
    -> Stream m a
    -> Stream m b
    -> Stream m (Maybe a, Maybe b)
joinOuterAscBy _eq _s1 _s2 = undefined

------------------------------------------------------------------------------
-- Set operations (special joins)
------------------------------------------------------------------------------
--
-- TODO: OrdSet/IntSet/hashmap based versions of these. With Eq only constraint
-- the best would be to use an Array with linear search. If the second stream
-- is sorted we can also use a binary search, using Ord constraint.

-- | Keep only those elements in the second stream that are present in the
-- first stream too. The first stream is folded to a container using the
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
            xs <- Stream.fold fld s1
            return $ Stream.filter (`member` xs) s2

-- | 'filterInStreamGenericBy' retains only those elements in the second stream that
-- are present in the first stream.
--
-- >>> Stream.fold Fold.toList $ Stream.filterInStreamGenericBy (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [2,1,1,3])
-- [2,1,1]
--
-- >>> Stream.fold Fold.toList $ Stream.filterInStreamGenericBy (==) (Stream.fromList [2,1,1,3]) (Stream.fromList [1,2,2,4])
-- [1,2,2]
--
-- Similar to the list intersectBy operation but with the stream argument order
-- flipped.
--
-- The first stream must be finite and must not block. Second stream is
-- processed only after the first stream is fully realized.
--
-- Space: O(n) where @n@ is the number of elements in the second stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE filterInStreamGenericBy #-}
filterInStreamGenericBy :: Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
filterInStreamGenericBy eq =
    -- XXX Use an (unboxed) array instead.
    filterStreamWith
        (Fold.scanMaybe (Fold.uniqBy eq) Fold.toListRev)
        (List.any . eq)

-- | Like 'filterInStreamGenericBy' but assumes that the input streams are sorted in
-- ascending order. To use it on streams sorted in descending order pass an
-- inverted comparison function returning GT for less than and LT for greater
-- than.
--
-- Space: O(1)
--
-- Time: O(m+n)
--
-- /Pre-release/
{-# INLINE filterInStreamAscBy #-}
filterInStreamAscBy :: Monad m =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
filterInStreamAscBy eq s1 s2 = Stream.intersectBySorted eq s2 s1

-- | Delete all elements of the first stream from the seconds stream. If an
-- element occurs multiple times in the first stream as many occurrences of it
-- are deleted from the second stream.
--
-- >>> Stream.fold Fold.toList $ Stream.deleteInStreamGenericBy (==) (Stream.fromList [1,2,3]) (Stream.fromList [1,2,2])
-- [2]
--
-- The following laws hold:
--
-- > deleteInStreamGenericBy (==) s1 (s1 `append` s2) === s2
-- > deleteInStreamGenericBy (==) s1 (s1 `interleave` s2) === s2
--
-- Same as the list 'Data.List.//' operation but with argument order flipped.
--
-- The first stream must be finite and must not block. Second stream is
-- processed only after the first stream is fully realized.
--
-- Space: O(m) where @m@ is the number of elements in the first stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE deleteInStreamGenericBy #-}
deleteInStreamGenericBy :: Monad m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
deleteInStreamGenericBy eq s1 s2 =
    Stream.concatEffect
        $ do
            -- This may work well if s1 is small
            -- If s1 is big we can go through s1, deleting elements from s2 and
            -- not emitting an element if it was successfully deleted from s2.
            -- we will need a deleteBy that can return whether the element was
            -- deleted or not.
            xs <- Stream.fold Fold.toList s2
            let f = Fold.foldl' (flip (List.deleteBy eq)) xs
            fmap Stream.fromList $ Stream.fold f s1

-- | A more efficient 'deleteInStreamGenericBy' for streams sorted in ascending order.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE deleteInStreamAscBy #-}
deleteInStreamAscBy :: -- (Monad m) =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
deleteInStreamAscBy _eq _s1 _s2 = undefined

-- XXX Remove the MonadIO constraint. We can just cache one stream and then
-- implement using differenceEqBy.

-- | This essentially appends to the second stream all the occurrences of
-- elements in the first stream that are not already present in the second
-- stream.
--
-- Equivalent to the following except that @s2@ is evaluated only once:
--
-- >>> unionWithStreamGenericBy eq s1 s2 = s2 `Stream.append` (Stream.deleteInStreamGenericBy eq s2 s1)
--
-- Example:
--
-- >>> Stream.fold Fold.toList $ Stream.unionWithStreamGenericBy (==) (Stream.fromList [1,1,2,3]) (Stream.fromList [1,2,2,4])
-- [1,2,2,4,3]
--
-- Space: O(n)
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE unionWithStreamGenericBy #-}
unionWithStreamGenericBy :: MonadIO m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
unionWithStreamGenericBy eq s1 s2 =
    Stream.concatEffect
        $ do
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

-- | A more efficient 'unionWithStreamGenericBy' for sorted streams.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE unionWithStreamAscBy #-}
unionWithStreamAscBy :: -- (Monad m) =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
unionWithStreamAscBy _eq _s1 _s2 = undefined
