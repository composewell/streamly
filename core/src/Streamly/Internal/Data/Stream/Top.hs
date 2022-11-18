-- |
-- Module      : Streamly.Internal.Data.Stream.Top
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Top level module that can depend on all other lower level Stream modules.

module Streamly.Internal.Data.Stream.Top
    (
    -- * Transformation
    -- ** Sampling
    -- | Value agnostic filtering.
      sampleFromThen

    -- ** Reordering
    , sortBy

    -- * Nesting
    -- ** Set like operations
    -- | These are not exactly set operations because streams are not
    -- necessarily sets, they may have duplicated elements.
    , intersectBy
    , intersectBySorted
    , differenceBy
    , mergeDifferenceBy
    , unionBy
    , mergeUnionBy

    -- ** Join operations
    , crossJoin
    , joinInner
    , joinInnerMerge
    , joinLeft
    , mergeLeftJoin
    , joinOuter
    , mergeOuterJoin
    )
where

#include "inline.hs"

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State.Strict (get, put)
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Maybe (isJust)
import Streamly.Internal.Data.Stream.Common ()
import Streamly.Internal.Data.Stream.Cross (CrossStream (..))
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)

import qualified Data.List as List
import qualified Streamly.Internal.Data.Array.Generic as Array
import qualified Streamly.Internal.Data.Array.Mut.Type as MA
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream.Transformer as Stream
import qualified Streamly.Internal.Data.Stream.Eliminate as Stream
import qualified Streamly.Internal.Data.Stream.Generate as Stream
import qualified Streamly.Internal.Data.Stream.Expand as Stream
import qualified Streamly.Internal.Data.Stream.Reduce as Stream
import qualified Streamly.Internal.Data.Stream.Transform as Stream
import qualified Streamly.Internal.Data.Stream.StreamD as StreamD

import Prelude hiding (filter, zipWith, concatMap, concat)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (filter, zipWith, concatMap, concat)
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Sampling
------------------------------------------------------------------------------

-- XXX We can implement this using addition instead of "mod" to make it more
-- efficient.
--
-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
-- >>> Stream.fold Fold.toList $ Stream.sampleFromThen 2 3 $ Stream.enumerateFromTo 0 10
-- [2,5,8]
--
-- /Pre-release/
--
{-# INLINE sampleFromThen #-}
sampleFromThen :: Monad m => Int -> Int -> Stream m a -> Stream m a
sampleFromThen offset stride =
    Stream.with Stream.indexed Stream.filter
        (\(i, _) -> i >= offset && (i - offset) `mod` stride == 0)

------------------------------------------------------------------------------
-- Reordering
------------------------------------------------------------------------------
--
-- We could possibly choose different algorithms depending on whether the
-- input stream is almost sorted (ascending/descending) or random. We could
-- serialize the stream to an array and use quicksort.
--
-- | Sort the input stream using a supplied comparison function.
--
-- /O(n) space/
--
-- Note: this is not the fastest possible implementation as of now.
--
-- /Pre-release/
--
{-# INLINE sortBy #-}
sortBy :: Monad m => (a -> a -> Ordering) -> Stream m a -> Stream m a
-- sortBy f = Stream.concatPairsWith (Stream.mergeBy f) Stream.fromPure
sortBy cmp =
    let p =
            Parser.groupByRollingEither
                (\x -> (< GT) . cmp x)
                Fold.toStreamRev
                Fold.toStream
     in   Stream.concatPairsWith (Stream.mergeBy cmp) id
        . Stream.rights . Stream.parseMany (fmap (either id id) p)

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
-- immutable handle would work i.e. using pread/pwrite instead of maintianing
-- an offset in the handle).

-- XXX We can do this concurrently.
--
-- | This is the same as 'Streamly.Internal.Data.Unfold.outerProduct' but less
-- efficient.
--
-- The second stream is evaluated multiple times. If the second stream is
-- consume-once stream then it can be cached in an 'Data.Array.Array' before
-- calling this function. Caching may also improve performance if the stream is
-- expensive to evaluate.
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE crossJoin #-}
crossJoin :: Monad m => Stream m a -> Stream m b -> Stream m (a, b)
crossJoin s1 s2 = getCrossStream $ do
    -- XXX use concatMap instead?
    a <- CrossStream s1
    b <- CrossStream s2
    return (a, b)

-- XXX We can do this concurrently.
-- XXX If the second stream is sorted and passed as an Array we could use
-- binary search if we have an Ord instance or Ordering returning function. The
-- time complexity would then become (m x log n).
--
-- | For all elements in @Stream m a@, for all elements in @Stream m b@ if @a@ and @b@
-- are equal by the given equality pedicate then return the tuple (a, b).
--
-- The second stream is evaluated multiple times. If the stream is a
-- consume-once stream then the caller should cache it (e.g. in a
-- 'Data.Array.Array') before calling this function. Caching may also improve
-- performance if the stream is expensive to evaluate.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- You should almost always use joinInnerMap instead of joinInner. joinInnerMap
-- is an order of magnitude faster. joinInner may be used when the second
-- stream is generated from a seed, therefore, need not be stored in memory and
-- the amount of memory it takes is a concern.
--
-- Space: O(n) assuming the second stream is cached in memory.
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE joinInner #-}
joinInner :: Monad m =>
    (a -> b -> Bool) -> Stream m a -> Stream m b -> Stream m (a, b)
joinInner eq s1 s2 = do
    -- ConcatMap works faster than bind
    Stream.concatMap (\a ->
        Stream.concatMap (\b ->
            if a `eq` b
            then Stream.fromPure (a, b)
            else Stream.nil
            ) s2
        ) s1

-- | Like 'joinInner' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE joinInnerMerge #-}
joinInnerMerge ::
    (a -> b -> Ordering) -> Stream m a -> Stream m b -> Stream m (a, b)
joinInnerMerge = undefined

-- XXX We can do this concurrently.
-- XXX If the second stream is sorted and passed as an Array or a seek capable
-- stream then we could use binary search if we have an Ord instance or
-- Ordering returning function. The time complexity would then become (m x log
-- n).
--
-- | For all elements in @Stream m a@, for all elements in @Stream m b@ if @a@ and @b@
-- are equal then return the tuple @(a, Just b)@.  If @a@ is not present in @t
-- m b@ then return @(a, Nothing)@.
--
-- The second stream is evaluated multiple times. If the stream is a
-- consume-once stream then the caller should cache it in an 'Data.Array.Array'
-- before calling this function. Caching may also improve performance if the
-- stream is expensive to evaluate.
--
-- @
-- rightJoin = flip joinLeft
-- @
--
-- Space: O(n) assuming the second stream is cached in memory.
--
-- Time: O(m x n)
--
-- /Unimplemented/
{-# INLINE joinLeft #-}
joinLeft :: Monad m =>
    (a -> b -> Bool) -> Stream m a -> Stream m b -> Stream m (a, Maybe b)
joinLeft eq s1 s2 = Stream.evalStateT (return False) $ getCrossStream $ do
    a <- CrossStream (Stream.liftInner s1)
    -- XXX should we use StreamD monad here?
    -- XXX Is there a better way to perform some action at the end of a loop
    -- iteration?
    CrossStream (Stream.fromEffect $ put False)
    let final = Stream.concatEffect $ do
            r <- get
            if r
            then pure Stream.nil
            else pure (Stream.fromPure Nothing)
    b <- CrossStream (fmap Just (Stream.liftInner s2) <> final)
    case b of
        Just b1 ->
            if a `eq` b1
            then do
                CrossStream (Stream.fromEffect $ put True)
                return (a, Just b1)
            else CrossStream Stream.nil
        Nothing -> return (a, Nothing)

-- | Like 'joinLeft' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE mergeLeftJoin #-}
mergeLeftJoin :: -- Monad m =>
    (a -> b -> Ordering) -> Stream m a -> Stream m b -> Stream m (a, Maybe b)
mergeLeftJoin _eq _s1 _s2 = undefined

-- XXX We can do this concurrently.
--
-- | For all elements in @t m a@, for all elements in @t m b@ if @a@ and @b@
-- are equal by the given equality pedicate then return the tuple (Just a, Just
-- b).  If @a@ is not found in @t m b@ then return (a, Nothing), return
-- (Nothing, b) for vice-versa.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- Space: O(n)
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE joinOuter #-}
joinOuter :: MonadIO m =>
       (a -> b -> Bool)
    -> Stream m a
    -> Stream m b
    -> Stream m (Maybe a, Maybe b)
joinOuter eq s1 s =
    Stream.concatEffect $ do
        inputArr <- Array.fromStream s
        let len = Array.length inputArr
        foundArr <-
            Stream.fold
            (MA.writeN len)
            (Stream.fromList (Prelude.replicate len False))
        return $ go inputArr foundArr <> leftOver inputArr foundArr

    where

    leftOver inputArr foundArr =
            let stream1 = Array.read inputArr
                stream2 = Stream.unfold MA.reader foundArr
            in Stream.filter
                    isJust
                    ( Stream.zipWith (\x y ->
                        if y
                        then Nothing
                        else Just (Nothing, Just x)
                        ) stream1 stream2
                    ) & Stream.catMaybes

    evalState = Stream.evalStateT (return False) . getCrossStream

    go inputArr foundArr = evalState $ do
        a <- CrossStream (Stream.liftInner s1)
        -- XXX should we use StreamD monad here?
        -- XXX Is there a better way to perform some action at the end of a loop
        -- iteration?
        CrossStream (Stream.fromEffect $ put False)
        let final = Stream.concatEffect $ do
                r <- get
                if r
                then pure Stream.nil
                else pure (Stream.fromPure Nothing)
        (i, b) <-
            let stream = Array.read inputArr
             in CrossStream
                (Stream.indexed $ fmap Just (Stream.liftInner stream) <> final)

        case b of
            Just b1 ->
                if a `eq` b1
                then do
                    CrossStream (Stream.fromEffect $ put True)
                    MA.putIndex i True foundArr
                    return (Just a, Just b1)
                else CrossStream Stream.nil
            Nothing -> return (Just a, Nothing)

-- | Like 'joinOuter' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE mergeOuterJoin #-}
mergeOuterJoin :: -- Monad m =>
       (a -> b -> Ordering)
    -> Stream m a
    -> Stream m b
    -> Stream m (Maybe a, Maybe b)
mergeOuterJoin _eq _s1 _s2 = undefined

------------------------------------------------------------------------------
-- Set operations (special joins)
------------------------------------------------------------------------------
--
-- TODO: OrdSet/IntSet/hashmap based versions of these. With Eq only constraint
-- the best would be to use an Array with linear search. If the second stream
-- is sorted we can also use a binary search, using Ord constraint.

-- | 'intersectBy' is essentially a filtering operation that retains only those
-- elements in the first stream that are present in the second stream.
--
-- >>> Stream.fold Fold.toList $ Stream.intersectBy (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [2,1,1,3])
-- [1,2,2]
--
-- >>> Stream.fold Fold.toList $ Stream.intersectBy (==) (Stream.fromList [2,1,1,3]) (Stream.fromList [1,2,2,4])
-- [2,1,1]
--
-- 'intersectBy' is similar to but not the same as 'joinInner':
--
-- >>> Stream.fold Fold.toList $ fmap fst $ Stream.joinInner (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [2,1,1,3])
-- [1,1,2,2]
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
intersectBy eq s1 s2 =
    Stream.concatEffect
        $ do
            -- This may work well when s2 is small
            xs <- Stream.fold Fold.toListRev $ Stream.uniqBy eq  s2
            return $ Stream.filter (\x -> List.any (eq x) xs) s1

-- | Like 'intersectBy' but works only on streams sorted in ascending order.
--
-- Space: O(1)
--
-- Time: O(m+n)
--
-- /Pre-release/
{-# INLINE intersectBySorted #-}
intersectBySorted :: (Monad m) =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
intersectBySorted eq s1 =
      fromStreamD
    . StreamD.intersectBySorted eq (toStreamD s1)
    . toStreamD

-- Roughly joinLeft s1 s2 = s1 `difference` s2 + s1 `intersection` s2

-- | Delete first occurrences of those elements from the first stream that are
-- present in the second stream. If an element occurs multiple times in the
-- second stream as many occurrences of it are deleted from the first stream.
--
-- >>> Stream.fold Fold.toList $ Stream.differenceBy (==) (Stream.fromList [1,2,2]) (Stream.fromList [1,2,3])
-- [2]
--
-- The following laws hold:
--
-- @
-- (s1 `serial` s2) `differenceBy eq` s1 === s2
-- (s1 `wSerial` s2) `differenceBy eq` s1 === s2
-- @
--
-- Same as the list 'Data.List.//' operation.
--
-- Space: O(m) where @m@ is the number of elements in the first stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE differenceBy #-}
differenceBy :: (Monad m) =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
differenceBy eq s1 s2 =
    Stream.concatEffect
        $ do
            -- This may work well if s1 is small
            -- If s1 is big we can go through s1, deleting elements from s2 and
            -- not emitting an element if it was successfully deleted from s2.
            -- we will need a deleteBy that can return whether the element was
            -- deleted or not.
            xs <- Stream.fold Fold.toList s1
            let f = Fold.foldl' (flip (List.deleteBy eq)) xs
            fmap Stream.fromList $ Stream.fold f s2

-- | Like 'differenceBy' but works only on sorted streams.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE mergeDifferenceBy #-}
mergeDifferenceBy :: -- (Monad m) =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeDifferenceBy _eq _s1 _s2 = undefined

-- | This is essentially an append operation that appends all the extra
-- occurrences of elements from the second stream that are not already present
-- in the first stream.
--
-- >>> Stream.fold Fold.toList $ Stream.unionBy (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [1,1,2,3])
-- [1,2,2,4,3]
--
-- Equivalent to the following except that @s1@ is evaluated only once:
--
-- @
-- unionBy eq s1 s2 = s1 \`serial` (s2 `differenceBy eq` s1)
-- @
--
-- Similar to 'joinOuter' but not the same.
--
-- Space: O(n)
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE unionBy #-}
unionBy :: MonadIO m =>
    (a -> a -> Bool) -> Stream m a -> Stream m a -> Stream m a
unionBy eq s1 s2 =
    Stream.concatEffect
        $ do
            xs <- Stream.fold Fold.toList  s2
            -- XXX we can use postscanlMAfter' instead of IORef
            ref <- liftIO $ newIORef $! List.nubBy eq xs
            let f x = do
                    liftIO $ modifyIORef' ref (List.deleteBy eq x)
                    return x
                s3 = Stream.concatEffect
                        $ do
                            xs1 <- liftIO $ readIORef ref
                            return $ Stream.fromList xs1
            return $ Stream.mapM f s1 <> s3

-- | Like 'unionBy' but works only on sorted streams.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE mergeUnionBy #-}
mergeUnionBy :: -- (Monad m) =>
    (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeUnionBy _eq _s1 _s2 = undefined
