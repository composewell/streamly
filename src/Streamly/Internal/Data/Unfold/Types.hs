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
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))

import Prelude hiding (map)

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
