-- |
-- Module      : Streamly.Internal.Data.Producer
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators on stream transition functions of type
-- @s -> m (Step s a)@. These are shared by the @Stream@ and @Unfold@
-- step functions.

module Streamly.Internal.Data.Producer
    (
      CrossApplyState(..)
    , TupleState(..)
    , crossApply
    , fromEffect
    , fromList
    , fromTuple
    , mapM
    , mapMaybeM
    , takeWhileM
    , unfoldrM
    )
where

#include "inline.hs"

import Data.Functor ((<&>))
import Streamly.Internal.Data.Stream.Step (Step(..))

import Prelude hiding (mapM)

-- | A stream transition: given the current state, produce the next 'Step'.
-- The state type @a@ is also the type carried inside 'Step', so a 'Yield'
-- delivers a new value alongside the updated state.
type Producer m a b = a -> m (Step a b)

data CrossApplyState m s1 s2 a b =
      CrossApplyOuter (m s2) s1
    | CrossApplyInner (m s2) (a -> b) s1 s2

data TupleState a = TupleBoth a a | TupleOne a | TupleNone

-- | Build a single element 'Producer' from an effect. The 'Bool' state is
-- 'True' before the effect is run and 'False' after, when the producer stops.
{-# INLINE_LATE fromEffect #-}
fromEffect :: Applicative m => m b -> Producer m Bool b
fromEffect m True  = (`Yield` False) <$> m
fromEffect _ False = pure Stop

{-# INLINE_LATE fromTuple #-}
fromTuple :: Applicative m => Producer m (TupleState a) a
fromTuple (TupleBoth x y) = pure $ Yield x (TupleOne y)
fromTuple (TupleOne y) = pure $ Yield y TupleNone
fromTuple TupleNone = pure Stop

{-# INLINE_LATE fromList #-}
fromList :: Applicative m => Producer m [a] a
fromList (x:xs) = pure $ Yield x xs
fromList [] = pure Stop

{-# INLINE_LATE crossApply #-}
crossApply
    :: Monad m
    => Producer m s1 (a -> b)
    -> Producer m s2 a
    -> Producer m (CrossApplyState m s1 s2 a b) b
crossApply step1 _ (CrossApplyOuter inject2 st) = do
    r <- step1 st
    case r of
        Yield f s -> do
            s2 <- inject2
            return $ Skip (CrossApplyInner inject2 f s s2)
        Skip s -> return $ Skip (CrossApplyOuter inject2 s)
        Stop -> return Stop
crossApply _ step2 (CrossApplyInner inject2 f os st) = do
    r <- step2 st
    return $ case r of
        Yield a s -> Yield (f a) (CrossApplyInner inject2 f os s)
        Skip s -> Skip (CrossApplyInner inject2 f os s)
        Stop -> Skip (CrossApplyOuter inject2 os)

{-# INLINE_LATE mapM #-}
mapM :: Monad m => (b -> m c) -> Producer m s b -> Producer m s c
mapM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ Yield b s
        Skip s -> return (Skip s)
        Stop   -> return Stop

{-# INLINE_LATE mapMaybeM #-}
mapMaybeM :: Monad m
    => (b -> m (Maybe c)) -> Producer m s b -> Producer m s c
mapMaybeM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ case b of
                Just c  -> Yield c s
                Nothing -> Skip s
        Skip s -> return (Skip s)
        Stop   -> return Stop

{-# INLINE_LATE takeWhileM #-}
takeWhileM :: Monad m
    => (b -> m Bool) -> Producer m s b -> Producer m s b
takeWhileM f step1 st = do
    r <- step1 st
    case r of
        Yield x s -> do
            b <- f x
            return $ if b then Yield x s else Stop
        Skip s -> return (Skip s)
        Stop   -> return Stop

-- | Build a 'Producer' from a /monadic/ step function that generates the next
-- element and the next seed value from the current seed value. It is invoked
-- until it returns 'Nothing'.
{-# INLINE_LATE unfoldrM #-}
unfoldrM :: Applicative m => (a -> m (Maybe (b, a))) -> Producer m a b
unfoldrM next a =
    next a <&> \case
        Just (b, a1) -> Yield b a1
        Nothing -> Stop
