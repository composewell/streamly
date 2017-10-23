{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
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
    , uncons

    -- * Special folds
    , take
    , drop

    -- * Zipping
    , ZipSerial (..)
    , zipWith
    , zipWithM

    , ZipAsync (..)
    , zipAsyncWith
    , zipAsyncWithM
    )
where

import Control.Applicative (Alternative (..), liftA2)
import Data.Semigroup (Semigroup(..), cycle1)
import Prelude hiding (drop, take, zipWith)
import Asyncly.AsyncT

------------------------------------------------------------------------------
-- Construct and deconstruct
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: MonadAsync m => AsyncT m a -> m (Maybe (a, AsyncT m a))
uncons m = (runAsyncT m) Nothing stop yield

    where

    stop = return Nothing

    {-# INLINE yield #-}
    yield a Nothing  = return (Just (a, empty))
    yield a (Just x) = return (Just (a, x))

-- | Build a Stream by unfolding steps starting from a seed.
unfoldr :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> AsyncT m a
unfoldr step = go
    where
    go s = AsyncT $ \_ stp yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
take :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
take n m = AsyncT $ \ctx stp yld -> do
    let yield a Nothing  = yld a Nothing
        yield a (Just x) = yld a (Just (take (n - 1) x))
    if (n <= 0)
    then stp
    else (runAsyncT m) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: MonadAsync m => Int -> AsyncT m a -> AsyncT m a
drop n m = AsyncT $ \ctx stp yld -> do
    let yield _ Nothing  = stp
        yield _ (Just x) = (runAsyncT $ drop (n - 1) x) ctx stp yld
    if (n <= 0)
    then (runAsyncT m) ctx stp yld
    else (runAsyncT m) ctx stp yield

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams serially using a monadic zipping function.
zipWithM :: Monad m =>
    (a -> b -> AsyncT m c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipWithM f m1 m2 = AsyncT $ \_ stp yld -> do
    let merge a ra =
            let yield2 b Nothing   = (runAsyncT (f a b)) Nothing stp yld
                yield2 b (Just rb) =
                    (runAsyncT ((f a b) <> (zipWithM f ra rb))) Nothing stp yld
             in (runAsyncT m2) Nothing stp yield2
    let yield1 a Nothing   = merge a mempty
        yield1 a (Just ra) = merge a ra
    (runAsyncT m1) Nothing stp yield1

-- | Zip two AsyncT streams serially using a pure zipping function.
zipWith :: Monad m => (a -> b -> c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipWith f m1 m2 = AsyncT $ \_ stp yld -> do
    let merge a ra =
            let yield2 b Nothing   = yld (f a b) Nothing
                yield2 b (Just rb) = yld (f a b) (Just (zipWith f ra rb))
             in (runAsyncT m2) Nothing stp yield2
    let yield1 a Nothing   = merge a mempty
        yield1 a (Just ra) = merge a ra
    (runAsyncT m1) Nothing stp yield1

-- | Wrapper around AsyncT type with a serial zipping Applicative instance.
-- Note that the binary function interleave (\<=>) is a special case of
-- ZipSerial Applicative.
--
-- > f <$> ZipSerial xs1 <*> ... <*> ZipSerial xsN
newtype ZipSerial m a = ZipSerial {getZipSerial :: AsyncT m a}
        deriving (Functor)

instance Monad m => Applicative (ZipSerial m) where
    pure a = ZipSerial $ cycle1 (pure a)
    (ZipSerial xs) <*> (ZipSerial ys) = ZipSerial (zipWith id xs ys)

instance (Monad m, Num a) => Num (ZipSerial m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (ZipSerial m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (ZipSerial m a) where
    pi = pure pi

    exp  = fmap exp
    sqrt = fmap sqrt
    log  = fmap log
    sin  = fmap sin
    tan  = fmap tan
    cos  = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    tanh = fmap tanh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

------------------------------------------------------------------------------
-- Asyncly Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a monadic zipping function.
zipAsyncWithM :: MonadAsync m
    => (a -> b -> AsyncT m c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipAsyncWithM f m1 m2 = AsyncT $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runAsyncT (zipWithM f ma mb)) Nothing stp yld

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a pure zipping function.
zipAsyncWith :: MonadAsync m
    => (a -> b -> c) -> AsyncT m a -> AsyncT m b -> AsyncT m c
zipAsyncWith f m1 m2 = AsyncT $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runAsyncT (zipWith f ma mb)) Nothing stp yld

-- | Wrapper around AsyncT type with a parallel zipping Applicative instance.
-- Note that the binary operator (\<|>) from the Alternative instance of AsyncT
-- is a special case of ZipAsync Applicative.
--
-- > f <$> ZipAsync xs1 <*> ... <*> ZipAsync xsN
newtype ZipAsync m a = ZipAsync {getZipAsync :: AsyncT m a}
        deriving (Functor)

instance MonadAsync m => Applicative (ZipAsync m) where
    pure a = ZipAsync $ cycle1 (pure a)
    (ZipAsync xs) <*> (ZipAsync ys) = ZipAsync (zipAsyncWith id xs ys)

instance (MonadAsync m, Num a) => Num (ZipAsync m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (MonadAsync m, Fractional a) => Fractional (ZipAsync m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (MonadAsync m, Floating a) => Floating (ZipAsync m a) where
    pi = pure pi

    exp  = fmap exp
    sqrt = fmap sqrt
    log  = fmap log
    sin  = fmap sin
    tan  = fmap tan
    cos  = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    tanh = fmap tanh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase
