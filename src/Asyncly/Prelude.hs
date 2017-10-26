{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Asyncly.Prelude
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Asyncly.Prelude
    (
    -- * Construction
      unfoldr
    , fromCallback

    -- * Elimination
    , runStreaming
    , toList
    , uncons

    -- * Special folds
    , take
    , drop

    -- * Zipping
    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM
    )
where

import           Control.Monad               (liftM)
import           Data.Semigroup              (Semigroup(..))
import           Prelude hiding              (drop, take, zipWith)
import           Asyncly.Core

------------------------------------------------------------------------------
-- Elimination
------------------------------------------------------------------------------

-- | Collect the results of an 'AsyncT' stream into a list.
{-# INLINABLE toList #-}
toList :: (Monad m, Streaming t) => t m a -> m [a]
toList m = go (toStream m)

    where

    go m1 = (runStream m1) Nothing stop yield

    stop = return []

    {-# INLINE yield #-}
    yield a Nothing  = return [a]
    yield a (Just x) = liftM (a :) (go x)

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: (Streaming t, Monad m, Monoid (t m a))
    => t m a -> m (Maybe (a, t m a))
uncons m = (runStream (toStream m)) Nothing stop yield
    where
    stop = return Nothing

    {-# INLINE yield #-}
    yield a Nothing  = return (Just (a, mempty))
    yield a (Just x) = return (Just (a, (fromStream x)))

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | Build a Stream by unfolding steps starting from a seed.
unfoldr :: (Streaming t, Monad m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldr step = fromStream . go
    where
    go s = Stream $ \_ stp yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

-- | Convert a callback into a stream.
fromCallback :: (forall r. (a -> m r) -> m r) -> Stream m a
fromCallback k = Stream $ \_ _ yld -> k (\a -> yld a Nothing)

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
take :: Streaming t => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld -> do
        let yield a Nothing  = yld a Nothing
            yield a (Just x) = yld a (Just (go (n1 - 1) x))
        if (n1 <= 0)
        then stp
        else (runStream m1) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: Streaming t => Int -> t m a -> t m a
drop n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld -> do
        let yield _ Nothing  = stp
            yield _ (Just x) = (runStream $ go (n1 - 1) x) ctx stp yld
        if (n1 <= 0)
        then (runStream m1) ctx stp yld
        else (runStream m1) ctx stp yield

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams serially using a monadic zipping function.
zipWithM :: Streaming t => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp yld -> do
        let merge a ra =
                let yield2 b Nothing   = (runStream (g a b)) Nothing stp yld
                    yield2 b (Just rb) =
                        (runStream ((g a b) <> (go ra rb))) Nothing stp yld
                 in (runStream my) Nothing stp yield2
        let yield1 a Nothing   = merge a mempty
            yield1 a (Just ra) = merge a ra
        (runStream mx) Nothing stp yield1
    g a b = toStream $ f a b

-- | Zip two AsyncT streams serially using a pure zipping function.
zipWith :: Streaming t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp yld -> do
        let merge a ra =
                let yield2 b Nothing   = yld (f a b) Nothing
                    yield2 b (Just rb) = yld (f a b) (Just (go ra rb))
                 in (runStream my) Nothing stp yield2
        let yield1 a Nothing   = merge a mempty
            yield1 a (Just ra) = merge a ra
        (runStream mx) Nothing stp yield1

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a monadic zipping function.
zipAsyncWithM :: (Streaming t, MonadAsync m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (toStream (zipWithM f ma mb))) Nothing stp yld

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a pure zipping function.
zipAsyncWith :: (Streaming t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (toStream (zipWith f ma mb))) Nothing stp yld
