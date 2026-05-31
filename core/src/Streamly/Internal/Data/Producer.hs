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
    , CrossApplyFstState(..)
    , TupleState(..)
    , crossApply
    , crossApplyFst
    , crossApplySnd
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

-- | State of a cross-apply style producer. @x@ is the seed from which the
-- inner producer's state is (re)injected for every element of the outer
-- producer. We store the first-order @x@ seed rather than the injection
-- action @m s2@ so that the inner injection stays a known, statically inlined
-- call and the loop state remains unboxable (storing @m s2@ here defeats
-- fusion and forces per-element allocation).
data CrossApplyState x s1 s2 a b =
      CrossApplyOuter x s1
    | CrossApplyInner x (a -> b) s1 s2

-- | State for 'crossApplyFst'. The inner constructor stores the outer
-- producer's value @b@ /directly/ so that it can be re-yielded for each element
-- of the inner producer as a loop-invariant value. Storing a function
-- (@const b@) here instead would force a per-element PAP application in the hot
-- inner loop and defeat the hoisting the original yielded value gets.
data CrossApplyFstState x s1 s2 b =
      CrossApplyFstOuter x s1
    | CrossApplyFstInner x b s1 s2

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
    => (x -> m s2)
    -> Producer m s1 (a -> b)
    -> Producer m s2 a
    -> Producer m (CrossApplyState x s1 s2 a b) b
crossApply inject2 step1 _ (CrossApplyOuter seed st) = do
    r <- step1 st
    case r of
        Yield f s -> do
            s2 <- inject2 seed
            return $ Skip (CrossApplyInner seed f s s2)
        Skip s -> return $ Skip (CrossApplyOuter seed s)
        Stop -> return Stop
crossApply _ _ step2 (CrossApplyInner seed f os st) = do
    r <- step2 st
    return $ case r of
        Yield a s -> Yield (f a) (CrossApplyInner seed f os s)
        Skip s -> Skip (CrossApplyInner seed f os s)
        Stop -> Skip (CrossApplyOuter seed os)

-- | Outer product discarding the second (inner) element. For each element of
-- the first producer the entire second producer is run, yielding the first
-- producer's element each time.
{-# INLINE_LATE crossApplyFst #-}
crossApplyFst
    :: Monad m
    => (x -> m s2)
    -> Producer m s1 b
    -> Producer m s2 a
    -> Producer m (CrossApplyFstState x s1 s2 b) b
crossApplyFst inject2 step1 _ (CrossApplyFstOuter seed st) = do
    r <- step1 st
    case r of
        Yield b s -> do
            s2 <- inject2 seed
            return $ Skip (CrossApplyFstInner seed b s s2)
        Skip s -> return $ Skip (CrossApplyFstOuter seed s)
        Stop -> return Stop
crossApplyFst _ _ step2 (CrossApplyFstInner seed b os st) = do
    r <- step2 st
    return $ case r of
        Yield _ s -> Yield b (CrossApplyFstInner seed b os s)
        Skip s -> Skip (CrossApplyFstInner seed b os s)
        Stop -> Skip (CrossApplyFstOuter seed os)

-- | Outer product discarding the first (outer) element. For each element of
-- the first producer the entire second producer is run, yielding the second
-- producer's elements.
{-# INLINE_LATE crossApplySnd #-}
crossApplySnd
    :: Monad m
    => (x -> m s2)
    -> Producer m s1 a
    -> Producer m s2 b
    -> Producer m (CrossApplyState x s1 s2 b b) b
crossApplySnd inject2 step1 _ (CrossApplyOuter seed st) = do
    r <- step1 st
    case r of
        Yield _ s -> do
            s2 <- inject2 seed
            return $ Skip (CrossApplyInner seed id s s2)
        Skip s -> return $ Skip (CrossApplyOuter seed s)
        Stop -> return Stop
crossApplySnd _ _ step2 (CrossApplyInner seed f os st) = do
    r <- step2 st
    return $ case r of
        Yield a s -> Yield (f a) (CrossApplyInner seed f os s)
        Skip s -> Skip (CrossApplyInner seed f os s)
        Stop -> Skip (CrossApplyOuter seed os)

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
