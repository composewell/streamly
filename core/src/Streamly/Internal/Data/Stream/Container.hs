{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Container
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Stream operations that require transformers or containers like Set or Map.

module Streamly.Internal.Data.Stream.Container
    (
    -- * Deduplication
      ordNub

    -- * Joins
    , leftJoin
    , outerJoin

    -- * Ord Joins
    , innerOrdJoin
    , leftOrdJoin
    , outerOrdJoin
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State.Strict (get, put)
import Data.Function ((&))
import Data.Maybe (isJust)
import Streamly.Internal.Data.Stream.Step (Step(..))
import Streamly.Internal.Data.Stream.Type
    (Stream(..), mkCross, unCross)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array.Generic as Array
import qualified Streamly.Internal.Data.MutArray.Type as MA
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.Generate as Stream
import qualified Streamly.Internal.Data.Stream.Transform as Stream
import qualified Streamly.Internal.Data.Stream.Transformer as Stream

#include "DocTestDataStream.hs"

-- | @nub@ specialized to 'Ord' types for better performance. Returns a
-- subsequence of the stream removing any duplicate elements.
--
-- The memory used is proportional to the number of unique elements in the
-- stream. One way to limit the memory is to  use @take@ on the resulting
-- stream to limit the unique elements in the stream.
{-# INLINE_NORMAL ordNub #-}
ordNub :: (Monad m, Ord a) => Stream m a -> Stream m a
ordNub (Stream step1 state1) = Stream step (Set.empty, state1)

    where

    step gst (set, st) = do
        r <- step1 gst st
        return
            $ case r of
                Yield x s ->
                    if Set.member x set
                    then Skip (set, s)
                    else Yield x (Set.insert x set, s)
                Skip s -> Skip (set, s)
                Stop -> Stop

-- XXX Generate error if a duplicate insertion is attempted?
toMap ::  (Monad m, Ord k) => Stream m (k, v) -> m (Map.Map k v)
toMap =
    let f = Fold.foldl' (\kv (k, b) -> Map.insert k b kv) Map.empty
     in Stream.fold f

-- If the second stream is too big it can be partitioned based on hashes and
-- then we can process one parition at a time.
--
-- XXX An IntMap may be faster when the keys are Int.
-- XXX Use hashmap instead of map?

-- | 'innerJoin' specialized to 'Ord' types for better performance.
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
{-# INLINE innerOrdJoin #-}
innerOrdJoin :: (Monad m, Ord k) =>
    Stream m (k, a) -> Stream m (k, b) -> Stream m (k, a, b)
innerOrdJoin s1 s2 =
    Stream.concatEffect $ do
        km <- toMap s2
        pure $ Stream.mapMaybe (joinAB km) s1

    where

    joinAB kvm (k, a) =
        case k `Map.lookup` kvm of
            Just b -> Just (k, a, b)
            Nothing -> Nothing

-- XXX We can do this concurrently.
-- XXX Check performance of StreamD vs StreamK
-- XXX If the second stream is sorted and passed as an Array or a seek capable
-- stream then we could use binary search if we have an Ord instance or
-- Ordering returning function. The time complexity would then become (m x log
-- n).

-- | Like 'innerJoin' but emits @(a, Just b)@ whenever a and b are equal, for
-- those @a@'s that are not equal to any @b@ emits @(a, Nothing)@.
--
-- This is a generalization of 'innerJoin' to include all elements from the
-- left stream and not just those which have an equal in the right stream. This
-- is not a commutative operation, the order of the stream arguments matters.
--
-- All the caveats mentioned in 'innerJoin' apply here as well. Right join is
-- not provided because it is just a flipped left join:
--
-- >>> rightJoin eq = flip (Stream.leftJoin eq)
--
-- Space: O(n) assuming the second stream is cached in memory.
--
-- Time: O(m x n)
--
-- /Unimplemented/
{-# INLINE leftJoin #-}
leftJoin :: Monad m =>
    (a -> b -> Bool) -> Stream m a -> Stream m b -> Stream m (a, Maybe b)
leftJoin eq s1 s2 = Stream.evalStateT (return False) $ unCross $ do
    a <- mkCross (Stream.liftInner s1)
    -- XXX should we use StreamD monad here?
    -- XXX Is there a better way to perform some action at the end of a loop
    -- iteration?
    mkCross (Stream.fromEffect $ put False)
    let final = Stream.concatEffect $ do
            r <- get
            if r
            then pure Stream.nil
            else pure (Stream.fromPure Nothing)
    b <- mkCross (fmap Just (Stream.liftInner s2) `Stream.append` final)
    case b of
        Just b1 ->
            if a `eq` b1
            then do
                mkCross (Stream.fromEffect $ put True)
                return (a, Just b1)
            else mkCross Stream.nil
        Nothing -> return (a, Nothing)

-- | 'leftJoin' specialized to 'Ord' types for better performance.
--
-- Space: O(n)
--
-- Time: O(m + n)
--
-- /Pre-release/
{-# INLINE leftOrdJoin #-}
leftOrdJoin :: (Ord k, Monad m) =>
    Stream m (k, a) -> Stream m (k, b) -> Stream m (k, a, Maybe b)
leftOrdJoin s1 s2 =
    Stream.concatEffect $ do
        km <- toMap s2
        return $ fmap (joinAB km) s1

            where

            joinAB km (k, a) =
                case k `Map.lookup` km of
                    Just b -> (k, a, Just b)
                    Nothing -> (k, a, Nothing)

-- XXX We can do this concurrently.
-- XXX Check performance of StreamD vs StreamK cross operation.

-- | Like 'leftJoin' but emits a @(Just a, Just b)@. Like 'leftJoin', for those
-- @a@'s that are not equal to any @b@ emit @(Just a, Nothing)@, but
-- additionally, for those @b@'s that are not equal to any @a@ emit @(Nothing,
-- Just b)@.
--
-- This is a generalization of left join to include all the elements from the
-- right stream as well, in other words it is a combination of left and right
-- joins. This is a commutative operation. The order of stream arguments can be
-- changed without affecting results, except for the ordering of elements in
-- the resulting tuple.
--
-- For space efficiency use the smaller stream as the second stream.
--
-- Space: O(n)
--
-- Time: O(m x n)
--
-- /Pre-release/
{-# INLINE outerJoin #-}
outerJoin :: MonadIO m =>
       (a -> b -> Bool)
    -> Stream m a
    -> Stream m b
    -> Stream m (Maybe a, Maybe b)
outerJoin eq s1 s2 =
    Stream.concatEffect $ do
        inputArr <- Array.fromStream s2
        let len = Array.length inputArr
        foundArr <-
            Stream.fold
            (MA.createOf len)
            (Stream.fromList (Prelude.replicate len False))
        return $ go inputArr foundArr `Stream.append` leftOver inputArr foundArr

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

    evalState = Stream.evalStateT (return False) . unCross

    go inputArr foundArr = evalState $ do
        a <- mkCross (Stream.liftInner s1)
        -- XXX should we use StreamD monad here?
        -- XXX Is there a better way to perform some action at the end of a loop
        -- iteration?
        mkCross (Stream.fromEffect $ put False)
        let final = Stream.concatEffect $ do
                r <- get
                if r
                then pure Stream.nil
                else pure (Stream.fromPure Nothing)
        (i, b) <-
            let stream = Array.read inputArr
             in mkCross
                (Stream.indexed $ fmap Just (Stream.liftInner stream) `Stream.append` final)

        case b of
            Just b1 ->
                if a `eq` b1
                then do
                    mkCross (Stream.fromEffect $ put True)
                    MA.putIndex i foundArr True
                    return (Just a, Just b1)
                else mkCross Stream.nil
            Nothing -> return (Just a, Nothing)

-- Put the b's that have been paired, in another hash or mutate the hash to set
-- a flag. At the end go through @Stream m b@ and find those that are not in that
-- hash to return (Nothing, b).

-- | 'outerJoin' specialized to 'Ord' types for better performance.
--
-- Space: O(m + n)
--
-- Time: O(m + n)
--
-- /Pre-release/
{-# INLINE outerOrdJoin #-}
outerOrdJoin ::
    (Ord k, MonadIO m) =>
    Stream m (k, a) -> Stream m (k, b) -> Stream m (k, Maybe a, Maybe b)
outerOrdJoin s1 s2 =
    Stream.concatEffect $ do
        km1 <- kvFold s1
        km2 <- kvFold s2

        -- XXX Not sure if toList/fromList would fuse optimally. We may have to
        -- create a fused Map.toStream function.
        let res1 = fmap (joinAB km2)
                        $ Stream.fromList $ Map.toList km1
                    where
                    joinAB km (k, a) =
                        case k `Map.lookup` km of
                            Just b -> (k, Just a, Just b)
                            Nothing -> (k, Just a, Nothing)

        -- XXX We can take advantage of the lookups in the first pass above to
        -- reduce the number of lookups in this pass. If we keep mutable cells
        -- in the second Map, we can flag it in the first pass and not do any
        -- lookup in the second pass if it is flagged.
        let res2 = Stream.mapMaybe (joinAB km1)
                        $ Stream.fromList $ Map.toList km2
                    where
                    joinAB km (k, b) =
                        case k `Map.lookup` km of
                            Just _ -> Nothing
                            Nothing -> Just (k, Nothing, Just b)

        return $ Stream.append res1 res2

        where

        -- XXX Generate error if a duplicate insertion is attempted?
        kvFold =
            let f = Fold.foldl' (\kv (k, b) -> Map.insert k b kv) Map.empty
             in Stream.fold f
