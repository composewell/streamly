{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Top
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Top level IsStream module that can use all other lower level IsStream
-- modules.

module Streamly.Internal.Data.Stream.IsStream.Top {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    (
    -- * Transformation
    -- ** Sampling
    -- | Value agnostic filtering.
      sampleFromThen
    , sampleIntervalStart
    , sampleIntervalEnd
    , sampleBurstStart
    , sampleBurstEnd

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
    , joinInnerMap
    , joinInnerMerge
    -- , joinLeft
    , mergeLeftJoin
    , joinLeftMap
    -- , joinOuter
    , mergeOuterJoin
    , joinOuterMap
    )
where

#include "inline.hs"

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.State.Strict (get, put)
-- import Data.Function ((&))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Kind (Type)
-- import Data.Maybe (isJust)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.IsStream.Common (concatM)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream(..), adapt, foldl', fromList)
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Time.Units (NanoSecond64(..), toRelTime64)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
-- import qualified Streamly.Internal.Data.Array.Generic as Array
--     (fromStream, length, read)
-- import qualified Streamly.Data.Array.Mut as MA
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
    (groupByRollingEither)
-- import qualified Streamly.Internal.Data.Stream.IsStream.Lift as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Eliminate as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Generate as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Reduce as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Transform as Stream
import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Stream as StreamD

import Prelude hiding (Foldable(..), filter, zipWith, concatMap, concat)

-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> import Prelude hiding (filter, zipWith, concatMap, concat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream

------------------------------------------------------------------------------
-- Sampling
------------------------------------------------------------------------------

-- XXX We can implement this using addition instead of "mod" to make it more
-- efficient.
--
-- | @sampleFromthen offset stride@ samples the element at @offset@ index and
-- then every element at strides of @stride@.
--
-- >>> Stream.toList $ Stream.sampleFromThen 2 3 $ Stream.enumerateFromTo 0 10
-- [2,5,8]
--
-- /Pre-release/
--
{-# INLINE sampleFromThen #-}
sampleFromThen :: (IsStream t, Monad m, Functor (t m)) =>
    Int -> Int -> t m a -> t m a
sampleFromThen offset stride =
    Stream.with Stream.indexed Stream.filter
        (\(i, _) -> i >= offset && (i - offset) `mod` stride == 0)

-- | Continuously evaluate the input stream and sample the last event in time
-- window of @n@ seconds.
--
-- This is also known as @throttle@ in some libraries.
--
-- @
-- sampleIntervalEnd n = Stream.catMaybes . Stream.intervalsOf n Fold.last
-- @
--
-- /Pre-release/
--
{-# INLINE sampleIntervalEnd #-}
sampleIntervalEnd :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleIntervalEnd n = Stream.catMaybes . Stream.intervalsOf n Fold.last

-- | Like 'sampleInterval' but samples at the beginning of the time window.
--
-- @
-- sampleIntervalStart n = Stream.catMaybes . Stream.intervalsOf n Fold.one
-- @
--
-- /Pre-release/
--
{-# INLINE sampleIntervalStart #-}
sampleIntervalStart :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleIntervalStart n = Stream.catMaybes . Stream.intervalsOf n Fold.one

data BurstState t x =
      BurstNone
    | BurstWait !t !x
    | BurstDone !x
    | BurstDoneNext !x !t !x

{-# INLINE sampleBurst #-}
sampleBurst :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Bool -> Double -> t m a -> t m a
sampleBurst sampleAtEnd gap xs =
    -- XXX Ideally we should schedule a timer event exactly after gap time,
    -- but the tick stream should work well as long as the timer
    -- granularity is small enough compared to the gap.
    Stream.mapMaybe extract
        $ Stream.scanl' step BurstNone
        $ Stream.timeIndexed
        $ Stream.interjectSuffix 0.01 (return Nothing) (Stream.map Just xs)

    where

    gap1 = toRelTime64 (NanoSecond64 (round (gap * 10^(9::Int))))

    {-# INLINE step #-}
    step BurstNone (t1, Just x1) = BurstWait t1 x1
    step BurstNone _ = BurstNone

    step (BurstDone _) (t1, Just x1) = BurstWait t1 x1
    step (BurstDone _) _ = BurstNone

    step old@(BurstWait t0 x0) (t1, Nothing)
        | t1 - t0 >= gap1 = BurstDone x0
        | otherwise = old
    -- This can happen due to scheduling delays, if we received back to
    -- back events spaced by more than the timeout without an
    -- intervening timeout event then we emit the old event instead of
    -- replacing it by the new.
    step (BurstWait t0 x0) (t1, Just x1)
        | t1 - t0 >= gap1 = BurstDoneNext x0 t1 x1
        | sampleAtEnd = BurstWait t1 x1
        | otherwise = BurstWait t1 x0

    step (BurstDoneNext _ t0 x0) (t1, Nothing)
        | t1 - t0 >= gap1 = BurstDone x0
        | otherwise =  BurstWait t0 x0
    step (BurstDoneNext _ t0 x0) (t1, Just x1)
        | t1 - t0 >= gap1 = BurstDoneNext x0 t1 x1
        | sampleAtEnd = BurstWait t1 x1
        | otherwise = BurstWait t1 x0

    {-# INLINE extract #-}
    extract (BurstDoneNext x _ _) = Just x
    extract (BurstDone x) = Just x
    extract _ = Nothing

-- | Sample one event at the end of each burst of events.  A burst is a group
-- of events close together in time, it ends when an event is spaced by more
-- than the specified time interval (in seconds) from the previous event.
--
-- This is known as @debounce@ in some libraries.
--
-- The clock granularity is 10 ms.
--
-- /Pre-release/
--
{-# INLINE sampleBurstEnd #-}
sampleBurstEnd :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleBurstEnd = sampleBurst True

-- | Like 'sampleBurstEnd' but samples the event at the beginning of the burst
-- instead of at the end of it.
--
-- /Pre-release/
--
{-# INLINE sampleBurstStart #-}
sampleBurstStart :: (IsStream t, MonadAsync m, Functor (t m)) =>
    Double -> t m a -> t m a
sampleBurstStart = sampleBurst False

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
sortBy :: MonadCatch m => (a -> a -> Ordering) -> SerialT m a -> SerialT m a
-- sortBy f = Stream.concatPairsWith (Stream.mergeBy f) Stream.fromPure
sortBy cmp =
    let p =
            Parser.groupByRollingEither
                (\x -> (< GT) . cmp x)
                (fmap fromStream Fold.toStreamKRev)
                (fmap fromStream Fold.toStreamK)
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
crossJoin :: Monad (t m) => t m a -> t m b -> t m (a, b)
crossJoin s1 s2 = do
    -- XXX use concatMap instead?
    a <- s1
    b <- s2
    return (a, b)

-- XXX We can do this concurrently.
-- XXX If the second stream is sorted and passed as an Array we could use
-- binary search if we have an Ord instance or Ordering returning function. The
-- time complexity would then become (m x log n).
--
-- | For all elements in @t m a@, for all elements in @t m b@ if @a@ and @b@
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
joinInner ::
    forall (t :: (Type -> Type) -> Type -> Type) m a b.
    (IsStream t, Monad m) =>
        (a -> b -> Bool) -> t m a -> t m b -> t m (a, b)
joinInner eq s1 s2 = do
    -- ConcatMap works faster than bind
    Stream.concatMap (\a ->
        Stream.concatMap (\b ->
            if a `eq` b
            then Stream.fromPure (a, b)
            else Stream.nil
            ) s2
        ) s1

-- XXX Generate error if a duplicate insertion is attempted?
toMap ::  (Monad m, Ord k) => IsStream.SerialT m (k, v) -> m (Map.Map k v)
toMap = Stream.foldl' (\kv (k, b) -> Map.insert k b kv) Map.empty

-- If the second stream is too big it can be partitioned based on hashes and
-- then we can process one parition at a time.
--
-- XXX An IntMap may be faster when the keys are Int.
-- XXX Use hashmap instead of map?
--
-- | Like 'joinInner' but uses a 'Map' for efficiency.
--
-- If the input streams have duplicate keys, the behavior is undefined.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- Space: O(n)
--
-- Time: O(m + n)
--
-- /Pre-release/
{-# INLINE joinInnerMap #-}
joinInnerMap :: (IsStream t, Monad m, Ord k) =>
    t m (k, a) -> t m (k, b) -> t m (k, a, b)
joinInnerMap s1 s2 =
    Stream.concatM $ do
        km <- toMap $ IsStream.adapt s2
        pure $ Stream.mapMaybe (joinAB km) s1

    where

    joinAB kvm (k, a) =
        case k `Map.lookup` kvm of
            Just b -> Just (k, a, b)
            Nothing -> Nothing

-- | Like 'joinInner' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE joinInnerMerge #-}
joinInnerMerge :: (a -> b -> Ordering) -> t m a -> t m b -> t m (a, b)
joinInnerMerge = undefined

{-
-- XXX We can do this concurrently.
-- XXX If the second stream is sorted and passed as an Array or a seek capable
-- stream then we could use binary search if we have an Ord instance or
-- Ordering returning function. The time complexity would then become (m x log
-- n).
--
-- | For all elements in @t m a@, for all elements in @t m b@ if @a@ and @b@
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
    (a -> b -> Bool) -> SerialT m a -> SerialT m b -> SerialT m (a, Maybe b)
joinLeft eq s1 s2 = Stream.evalStateT (return False) $ do
    a <- Stream.liftInner s1
    -- XXX should we use StreamD monad here?
    -- XXX Is there a better way to perform some action at the end of a loop
    -- iteration?
    lift $ put False
    let final = do
            r <- lift get
            if r
            then Stream.nil
            else Stream.fromPure Nothing
    b <- fmap Just (Stream.liftInner s2) <> final
    case b of
        Just b1 ->
            if a `eq` b1
            then do
                lift $ put True
                return (a, Just b1)
            else Stream.nil
        Nothing -> return (a, Nothing)
-}

-- | Like 'joinLeft' but uses a hashmap for efficiency.
--
-- Space: O(n)
--
-- Time: O(m + n)
--
-- /Pre-release/
{-# INLINE joinLeftMap #-}
joinLeftMap :: (IsStream t, Ord k, Monad m) =>
    t m (k, a) -> t m (k, b) -> t m (k, a, Maybe b)
joinLeftMap s1 s2 =
    Stream.concatM $ do
        km <- toMap $ IsStream.adapt s2
        return $ Stream.map (joinAB km) s1

            where

            joinAB km (k, a) =
                case k `Map.lookup` km of
                    Just b -> (k, a, Just b)
                    Nothing -> (k, a, Nothing)

-- | Like 'joinLeft' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE mergeLeftJoin #-}
mergeLeftJoin :: -- Monad m =>
    (a -> b -> Ordering) -> t m a -> t m b -> t m (a, Maybe b)
mergeLeftJoin _eq _s1 _s2 = undefined

{-
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
-- /Unimplemented/
{-# INLINE joinOuter #-}
joinOuter :: MonadIO m =>
       (a -> b -> Bool)
    -> SerialT m a
    -> SerialT m b
    -> SerialT m (Maybe a, Maybe b)
joinOuter eq s1 s =
    Stream.concatM $ do
        inputArr <- Array.fromStream s
        let len = Array.length inputArr
        foundArr <-
            Stream.fold
            (MA.writeN len)
            (Stream.fromList (Prelude.replicate len False))
        return $ go inputArr foundArr <> leftOver inputArr foundArr

    where

    leftOver inputArr foundArr =
            let stream1 = IsStream.fromSerial $ Array.read inputArr
                stream2 = Stream.unfold MA.reader foundArr
            in Stream.filter
                    isJust
                    ( Stream.zipWith (\x y ->
                        if y
                        then Nothing
                        else Just (Nothing, Just x)
                        ) stream1 stream2
                    ) & Stream.catMaybes

    go inputArr foundArr = Stream.evalStateT (return False) $ do
        a <- Stream.liftInner s1
        -- XXX should we use StreamD monad here?
        -- XXX Is there a better way to perform some action at the end of a loop
        -- iteration?
        lift $ put False
        let final = do
                r <- lift get
                if r
                then Stream.nil
                else Stream.fromPure Nothing
        (i, b) <-
            let stream = IsStream.fromSerial $ Array.read inputArr
             in Stream.indexed $ fmap Just (Stream.liftInner stream) <> final

        case b of
            Just b1 ->
                if a `eq` b1
                then do
                    lift $ put True
                    MA.putIndex i True foundArr
                    return (Just a, Just b1)
                else Stream.nil
            Nothing -> return (Just a, Nothing)
-}

-- Put the b's that have been paired, in another hash or mutate the hash to set
-- a flag. At the end go through @t m b@ and find those that are not in that
-- hash to return (Nothing, b).
--
-- | Like 'joinOuter' but uses a 'Map' for efficiency.
--
-- Space: O(m + n)
--
-- Time: O(m + n)
--
-- /Pre-release/
{-# INLINE joinOuterMap #-}
joinOuterMap ::
    (IsStream t, Ord k, MonadIO m) =>
    t m (k, a) -> t m (k, b) -> t m (k, Maybe a, Maybe b)
joinOuterMap s1 s2 =
    Stream.concatM $ do
        km1 <- kvFold $ IsStream.adapt s1
        km2 <- kvFold $ IsStream.adapt s2

        -- XXX Not sure if toList/fromList would fuse optimally. We may have to
        -- create a fused Map.toStream function.
        let res1 = Stream.map (joinAB km2) $ Stream.fromList $ Map.toList km1
                    where
                    joinAB km (k, a) =
                        case k `Map.lookup` km of
                            Just b -> (k, Just a, Just b)
                            Nothing -> (k, Just a, Nothing)

        -- XXX We can take advantage of the lookups in the first pass above to
        -- reduce the number of lookups in this pass. If we keep mutable cells
        -- in the second Map, we can flag it in the first pass and not do any
        -- lookup in the second pass if it is flagged.
        let res2 = Stream.mapMaybe (joinAB km1) $ Stream.fromList $ Map.toList km2
                    where
                    joinAB km (k, b) =
                        case k `Map.lookup` km of
                            Just _ -> Nothing
                            Nothing -> Just (k, Nothing, Just b)

        return $ Stream.serial res1 res2

        where

        -- XXX Generate error if a duplicate insertion is attempted?
        kvFold = Stream.foldl' (\kv (k, b) -> Map.insert k b kv) Map.empty

-- | Like 'joinOuter' but works only on sorted streams.
--
-- Space: O(1)
--
-- Time: O(m + n)
--
-- /Unimplemented/
{-# INLINE mergeOuterJoin #-}
mergeOuterJoin :: -- Monad m =>
    (a -> b -> Ordering) -> t m a -> t m b -> t m (Maybe a, Maybe b)
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
-- >>> Stream.toList $ Stream.intersectBy (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [2,1,1,3])
-- [1,2,2]
--
-- >>> Stream.toList $ Stream.intersectBy (==) (Stream.fromList [2,1,1,3]) (Stream.fromList [1,2,2,4])
-- [2,1,1]
--
-- 'intersectBy' is similar to but not the same as 'joinInner':
--
-- >>> Stream.toList $ fmap fst $ Stream.joinInner (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [2,1,1,3])
-- [1,1,2,2]
--
-- Space: O(n) where @n@ is the number of elements in the second stream.
--
-- Time: O(m x n) where @m@ is the number of elements in the first stream and
-- @n@ is the number of elements in the second stream.
--
-- /Pre-release/
{-# INLINE intersectBy #-}
intersectBy :: (IsStream t, Monad m) =>
    (a -> a -> Bool) -> t m a -> t m a -> t m a
intersectBy eq s1 s2 =
    concatM
        $ do
            -- This may work well when s2 is small
            xs <- Stream.toListRev $ Stream.uniqBy eq $ adapt s2
            return $ Stream.filter (\x -> List.any (eq x) xs) s1

-- | Like 'intersectBy' but works only on streams sorted in ascending order.
--
-- Space: O(1)
--
-- Time: O(m+n)
--
-- /Pre-release/
{-# INLINE intersectBySorted #-}
intersectBySorted :: (IsStream t, Monad m) =>
    (a -> a -> Ordering) -> t m a -> t m a -> t m a
intersectBySorted eq s1 =
      IsStream.fromStreamD
    . StreamD.intersectBySorted eq (IsStream.toStreamD s1)
    . IsStream.toStreamD

-- Roughly joinLeft s1 s2 = s1 `difference` s2 + s1 `intersection` s2

-- | Delete first occurrences of those elements from the first stream that are
-- present in the second stream. If an element occurs multiple times in the
-- second stream as many occurrences of it are deleted from the first stream.
--
-- >>> Stream.toList $ Stream.differenceBy (==) (Stream.fromList [1,2,2]) (Stream.fromList [1,2,3])
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
differenceBy :: (IsStream t, Monad m) =>
    (a -> a -> Bool) -> t m a -> t m a -> t m a
differenceBy eq s1 s2 =
    concatM
        $ do
            -- This may work well if s1 is small
            -- If s1 is big we can go through s1, deleting elements from s2 and
            -- not emitting an element if it was successfully deleted from s2.
            -- we will need a deleteBy that can return whether the element was
            -- deleted or not.
            xs <- Stream.toList $ adapt s1
            fmap fromList $ foldl' (flip (List.deleteBy eq)) xs s2

-- | Like 'differenceBy' but works only on sorted streams.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE mergeDifferenceBy #-}
mergeDifferenceBy :: -- (IsStream t, Monad m) =>
    (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeDifferenceBy _eq _s1 _s2 = undefined

-- | This is essentially an append operation that appends all the extra
-- occurrences of elements from the second stream that are not already present
-- in the first stream.
--
-- >>> Stream.toList $ Stream.unionBy (==) (Stream.fromList [1,2,2,4]) (Stream.fromList [1,1,2,3])
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
unionBy :: (IsStream t, MonadAsync m, Semigroup (t m a)) =>
    (a -> a -> Bool) -> t m a -> t m a -> t m a
unionBy eq s1 s2 =
    concatM
        $ do
            xs <- Stream.toList $ adapt s2
            -- XXX we can use postscanlMAfter' instead of IORef
            ref <- liftIO $ newIORef $! List.nubBy eq xs
            let f x = do
                    liftIO $ modifyIORef' ref (List.deleteBy eq x)
                    return x
                s3 = concatM
                        $ do
                            xs1 <- liftIO $ readIORef ref
                            return $ fromList xs1
            return $ Stream.mapM f s1 <> s3

-- | Like 'unionBy' but works only on sorted streams.
--
-- Space: O(1)
--
-- /Unimplemented/
{-# INLINE mergeUnionBy #-}
mergeUnionBy :: -- (IsStream t, Monad m) =>
    (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeUnionBy _eq _s1 _s2 = undefined
