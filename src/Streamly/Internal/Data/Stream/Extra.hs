-- |
-- Module      : Streamly.Internal.Data.Stream.Extra
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Extra
    (
      nub
    , joinInnerMap
    , joinLeftMap
    , joinOuterMap
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as Stream (concatM)
import qualified Streamly.Internal.Data.Stream.StreamD as D

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

-- | The memory used is proportional to the number of unique elements in the
-- stream. If we want to limit the memory we can just use "take" to limit the
-- uniq elements in the stream.
{-# INLINE_NORMAL nub #-}
nub :: (Monad m, Ord a) => D.Stream m a -> D.Stream m a
nub (D.Stream step1 state1) = D.Stream step (Set.empty, state1)

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
joinInnerMap :: (Monad m, Ord k) =>
    Stream m (k, a) -> Stream m (k, b) -> Stream m (k, a, b)
joinInnerMap s1 s2 =
    Stream.concatM $ do
        km <- toMap s2
        pure $ Stream.mapMaybe (joinAB km) s1

    where

    joinAB kvm (k, a) =
        case k `Map.lookup` kvm of
            Just b -> Just (k, a, b)
            Nothing -> Nothing

-- | Like 'joinLeft' but uses a hashmap for efficiency.
--
-- Space: O(n)
--
-- Time: O(m + n)
--
-- /Pre-release/
{-# INLINE joinLeftMap #-}
joinLeftMap :: (Ord k, Monad m) =>
    Stream m (k, a) -> Stream m (k, b) -> Stream m (k, a, Maybe b)
joinLeftMap s1 s2 =
    Stream.concatM $ do
        km <- toMap s2
        return $ fmap (joinAB km) s1

            where

            joinAB km (k, a) =
                case k `Map.lookup` km of
                    Just b -> (k, a, Just b)
                    Nothing -> (k, a, Nothing)

-- Put the b's that have been paired, in another hash or mutate the hash to set
-- a flag. At the end go through @Stream m b@ and find those that are not in that
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
    (Ord k, MonadIO m) =>
    Stream m (k, a) -> Stream m (k, b) -> Stream m (k, Maybe a, Maybe b)
joinOuterMap s1 s2 =
    Stream.concatM $ do
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
