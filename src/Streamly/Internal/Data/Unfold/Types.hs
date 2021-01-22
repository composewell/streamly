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
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))

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
