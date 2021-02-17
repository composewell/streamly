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

data Cross a s1 b s2 = CrossOuter a s1 | CrossInner a s1 b s2

-- | Create a cross product (vector product or cartesian product) of the
-- output streams of two unfolds.
--
{-# INLINE_NORMAL cross #-}
cross :: Monad m => Unfold m a b -> Unfold m a c -> Unfold m a (b, c)
cross (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject

    where

    inject a = do
        s1 <- inject1 a
        return $ CrossOuter a s1

    {-# INLINE_LATE step #-}
    step (CrossOuter a s1) = do
        r <- step1 s1
        case r of
            Yield b s -> do
                s2 <- inject2 a
                return $ Skip (CrossInner a s b s2)
            Skip s    -> return $ Skip (CrossOuter a s)
            Stop      -> return Stop

    step (CrossInner a s1 b s2) = do
        r <- step2 s2
        return $ case r of
            Yield c s -> Yield (b, c) (CrossInner a s1 b s)
            Skip s    -> Skip (CrossInner a s1 b s)
            Stop      -> Skip (CrossOuter a s1)

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
