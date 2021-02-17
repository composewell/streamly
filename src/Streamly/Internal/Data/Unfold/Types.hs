-- |
-- Module      : Streamly.Internal.Data.Unfold.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Unfold.Types
    ( Unfold (..)
    , lmap
    , map
    , const
    , apSequence
    , apDiscardSnd
    , cross
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))

import Prelude hiding (const, map)

------------------------------------------------------------------------------
-- Monadic Unfolds
------------------------------------------------------------------------------

-- | An @Unfold m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- @since 0.7.0

data Unfold m a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> m (Step s b)) (a -> m s)

-- | Map a function on the input argument of the 'Unfold'.
--
-- @
-- lmap f = concat (singleton f)
-- @
--
-- /Internal/
{-# INLINE_NORMAL lmap #-}
lmap :: (a -> c) -> Unfold m c b -> Unfold m a b
lmap f (Unfold ustep uinject) = Unfold ustep (uinject . f)

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

-- | Map a function on the output of the unfold (the type @b@).
--
-- /Internal/
{-# INLINE_NORMAL map #-}
map :: Functor m => (b -> c) -> Unfold m a b -> Unfold m a c
map f (Unfold ustep uinject) = Unfold step uinject

    where

    {-# INLINE_LATE step #-}
    step st = fmap (fmap f) (ustep st)

-- | Maps a function on the output of the unfold (the type @b@).
instance Functor m => Functor (Unfold m a) where
    {-# INLINE fmap #-}
    fmap = map

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

{-# INLINE const #-}
const :: Applicative m => m b -> Unfold m a b
const m = Unfold step inject

    where

    inject _ = pure False

    step False = (`Yield` True) <$> m
    step True = pure Stop

-- | Outer product discarding the first element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL apSequence #-}
apSequence :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a c
apSequence (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

-- | Outer product discarding the second element.
--
-- /Unimplemented/
--
{-# INLINE_NORMAL apDiscardSnd #-}
apDiscardSnd :: -- Monad m =>
    Unfold m a b -> Unfold m a c -> Unfold m a b
apDiscardSnd (Unfold _step1 _inject1) (Unfold _step2 _inject2) = undefined

data Cross s1 s2 sy x y = CrossOuter s1 y | CrossInner s1 sy s2 x

-- | Create a cross product (vector product or cartesian product) of the
-- output streams of two unfolds.
--
{-# INLINE_NORMAL cross #-}
cross :: Monad m => Unfold m a b -> Unfold m a c -> Unfold m a (b, c)
cross (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject x = do
        s1 <- inject1 x
        return $ CrossOuter s1 x

    {-# INLINE_LATE step #-}
    step (CrossOuter st1 sy) = do
        r <- step1 st1
        case r of
            Yield x s -> do
                s2 <- inject2 sy
                return $ Skip (CrossInner s sy s2 x)
            Skip s    -> return $ Skip (CrossOuter s sy)
            Stop      -> return Stop

    step (CrossInner ost sy ist x) = do
        r <- step2 ist
        return $ case r of
            Yield y s -> Yield (x, y) (CrossInner ost sy s x)
            Skip s    -> Skip (CrossInner ost sy s x)
            Stop      -> Skip (CrossOuter ost sy)

-- | Example:
--
-- >>> Stream.toList $ Stream.unfold ((,) <$> Unfold.lmap fst Unfold.fromList <*> Unfold.lmap snd Unfold.fromList) ([1,2],[3,4])
-- [(1,3),(1,4),(2,3),(2,4)]
--
instance Monad m => Applicative (Unfold m a) where
    {-# INLINE pure #-}
    pure = const . return

    {-# INLINE (<*>) #-}
    u1 <*> u2 = fmap (\(a, b) -> a b) (cross u1 u2)

    -- {-# INLINE (*>) #-}
    -- (*>) = apSequence

    -- {-# INLINE (<*) #-}
    -- (<*) = apDiscardSnd
