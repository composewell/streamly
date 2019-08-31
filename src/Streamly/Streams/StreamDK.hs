{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Streams.StreamDK
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Streams.StreamDK
    (
    -- * Stream Type

      Stream
    , Step (..)

    -- * Construction
    , nil
    , cons
    , consM
    , unfoldr
    , unfoldrM
    , replicateM

    -- * Folding
    , uncons
    , foldrS

    -- * Specific Folds
    , drain
    )
where

import Streamly.Streams.StreamDK.Type (Stream(..), Step(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Identity (IdentityT(..))
-- import Data.Functor.Identity

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

nil :: Monad m => Stream m a
nil = Stream $ return Stop

{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> Stream m a -> Stream m a
cons x xs = Stream $ return $ Yield x xs

consM :: Monad m => m a -> Stream m a -> Stream m a
consM eff xs = Stream $ eff >>= \x -> return $ Yield x xs

unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream (step' state)
  where
    step' st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x (Stream (step' s))
            Nothing     -> Stop
{-
unfoldrM next s0 = buildM $ \yld stp ->
    let go s = do
            r <- next s
            case r of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0
-}

{-# INLINE unfoldr #-}
unfoldr :: Monad m => (b -> Maybe (a, b)) -> b -> Stream m a
unfoldr next s0 = build $ \yld stp ->
    let go s =
            case next s of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0

replicateM :: Monad m => Int -> a -> Stream m a
replicateM n x = Stream (step n)
    where
    step i = return $
        if i <= 0
        then Stop
        else Yield x (Stream (step (i - 1)))

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons (Stream step) = do
    r <- step
    return $ case r of
        Yield x xs -> Just (x, xs)
        Stop -> Nothing

-- | Lazy right associative fold to a stream.
{-# INLINE_LATE foldrS #-}
foldrS :: Monad m
       => (a -> Stream m b -> Stream m b)
       -> Stream m b
       -> Stream m a
       -> Stream m b
foldrS f streamb = go
    where
    go (Stream stepa) = Stream $ do
        r <- stepa
        case r of
            Yield x xs -> let Stream step = f x (go xs) in step
            Stop -> let Stream step = streamb in step

{-# INLINE_LATE foldrT #-}
foldrT :: (MonadTrans t, Monad m, Monad (t m))
       => (a -> t m b -> t m b)
       -> t m b
       -> Stream m a
       -> t m b
foldrT f acc = go
  where
    go (Stream stepa) = do
      r <- lift stepa
      case r of
          Yield x xs -> f x (go xs)
          Stop -> acc

{-# INLINE_LATE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM fstep acc ys = go ys
    where
    go (Stream step) = do
        r <- step
        case r of
            Yield x xs -> fstep x (go xs)
            Stop -> acc

{-# INLINE_NORMAL build #-}
build :: Monad m
    => forall a. (forall b. (a -> b -> b) -> b -> b) -> Stream m a
build g = g cons nil

{-# RULES
"foldrM/build"  forall k z (g :: forall b. (a -> b -> b) -> b -> b).
                foldrM k z (build g) = g k z #-}

{-# INLINE_NORMAL buildT #-}
buildT ::
       Monad m
    => (forall r t. (MonadTrans t, Monad (t m)) =>
                      (a -> t m r -> t m r) -> t m r -> t m r)
    -> Stream m a
buildT g = g cons nil

{-# INLINE_LATE buildS #-}
buildS ::
       Monad m
    => (forall r t. (MonadTrans t, Monad (t m)) =>
                      (a -> t m r -> t m r) -> t m r -> t m r)
    -> Stream m a
buildS g = g cons nil

{-# RULES
"foldrS/buildS"  forall (k :: a -> Stream m b -> Stream m b) (z :: Stream m b) (g :: forall r t. (MonadTrans t, Monad (t m)) =>
                      (a -> t m r -> t m r) -> t m r -> t m r).
                foldrS k z (buildS g) = g k z #-}

{-# RULES
"foldrT/buildT"  forall k z (g :: forall r t. (MonadTrans t, Monad (t m)) =>
                      (a -> t m r -> t m r) -> t m r -> t m r).
                foldrT k z (buildT g) = g k z #-}

{-
-- To fuse foldrM with unfoldrM we need the type m1 to be polymorphic such that
-- it is either Monad m or Stream m.  So that we can use cons/nil as well as
-- monadic construction function as its arguments.
--
{-# INLINE_NORMAL buildM #-}
buildM :: Monad m
    => forall a. (forall b. (a -> m1 b -> m1 b) -> m1 b -> m1 b) -> Stream m a
buildM g = g cons nil

{-# INLINE unfoldrM #-}
unfoldrM ::
       (Monad m)
    => (s -> m (Maybe (a, s)))
    -> s
    -> Stream m a
unfoldrM next s0 = buildS $ \yld stp ->
    let go s = do
            r <- lift $ next s
            case r of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0
-}

-------------------------------------------------------------------------------
-- Specific folds
-------------------------------------------------------------------------------

{-# INLINE drain #-}
drain :: Monad m => Stream m a -> m ()
-- drain = drainX . foldrS cons nil
-- drain = foldrM (\_ xs -> xs) (return ()) -- . foldrS cons nil
-- drain = runIdentityT . foldrT (\_ xs -> xs) (return ())
drain = foldrM (\_ xs -> xs) (return ())

{-
{-# INLINE drainX #-}
drainX :: Monad m => Stream m a -> m ()
drainX (Stream step) = do
    r <- step
    case r of
        Yield _ next -> drain next
        Stop      -> return ()
        -}
