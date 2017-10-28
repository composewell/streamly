{-# LANGUAGE BangPatterns              #-}
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

    -- * Elimination
    , foldr
    , foldrM
    , foldl
    , foldlM
    , uncons

    -- * Special folds
    , toList
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile

    -- * Zipping
    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM
    )
where

import           Control.Monad               (liftM)
import           Data.Semigroup              (Semigroup(..))
import           Prelude hiding              (filter, drop, dropWhile, take,
                                              takeWhile, zipWith, foldr, foldl)
import           Asyncly.Core
import           Asyncly.Streams

------------------------------------------------------------------------------
-- Elimination
------------------------------------------------------------------------------

-- | Right fold.
foldr :: (Monad m, Streaming t) => (a -> b -> b) -> b -> t m a -> m b
foldr step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            yield a Nothing  = return (step a acc)
            yield a (Just x) = go x >>= \b -> return (step a b)
        in (runStream m1) Nothing stop yield

-- | Right fold with a monadic step function.  See 'toList' for an example use.
foldrM :: Streaming t => (a -> m b -> m b) -> m b -> t m a -> m b
foldrM step acc m = go (toStream m)
    where
    go m1 =
        let stop = acc
            yield a Nothing  = step a acc
            yield a (Just x) = step a (go x)
        in (runStream m1) Nothing stop yield

-- | Strict left fold. This is typed to work with the foldl package. To use
-- directly pass 'id' as the third argument.
foldl :: (Monad m, Streaming t)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> m b
foldl step begin done m = go begin (toStream m)
    where
    go !acc m1 =
        let stop = return (done acc)
            yield a Nothing  = return (done (step acc a))
            yield a (Just x) = go (step acc a) x
         in (runStream m1) Nothing stop yield

-- | Strict left fold, with monadic step function. This is typed to work
-- with the foldl package. To use directly pass 'id' as the third argument.
foldlM :: (Monad m, Streaming t)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> m b
foldlM step begin done m = go begin (toStream m)
    where
    go !acc m1 =
        let stop = acc >>= done
            yield a Nothing  = acc >>= \b -> step b a >>= done
            yield a (Just x) = acc >>= \b -> go (step b a) x
         in (runStream m1) Nothing stop yield

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: (Streaming t, Monad m, Monoid (t m a))
    => t m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        yield a Nothing  = return (Just (a, mempty))
        yield a (Just x) = return (Just (a, (fromStream x)))
    in (runStream (toStream m)) Nothing stop yield

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

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Convert a stream into a list in the underlying monad.
{-# INLINABLE toList #-}
toList :: (Monad m, Streaming t) => t m a -> m [a]
toList = foldrM (\a xs -> liftM (a :) xs) (return [])

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

-- | Include only those elements that pass a predicate.
filter :: Streaming t => (a -> Bool) -> t m a -> t m a
filter p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld -> do
        let yield a Nothing  | p a       = yld a Nothing
                             | otherwise = stp
            yield a (Just x) | p a       = yld a (Just (go x))
                             | otherwise = (runStream (go x)) ctx stp yield
         in (runStream m1) ctx stp yield

-- | End the stream as soon as the predicate fails on an element.
takeWhile :: Streaming t => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld -> do
        let yield a Nothing  | p a       = yld a Nothing
                             | otherwise = stp
            yield a (Just x) | p a       = yld a (Just (go x))
                             | otherwise = stp
         in (runStream m1) ctx stp yield

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

-- | Drop elements in the stream as long as the predicate succeeds and then
-- take the rest of the stream.
dropWhile :: Streaming t => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \ctx stp yld -> do
        let yield a Nothing  | p a       = stp
                             | otherwise = yld a Nothing
            yield a (Just x) | p a       = (runStream (go x)) ctx stp yield
                             | otherwise = yld a (Just x)
         in (runStream m1) ctx stp yield

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams serially using a monadic zipping function.
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

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------

-- | Zip two streams asyncly (i.e. both the elements being zipped are generated
-- concurrently) using a monadic zipping function.
zipAsyncWithM :: (Streaming t, MonadAsync m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (toStream (zipWithM f ma mb))) Nothing stp yld
