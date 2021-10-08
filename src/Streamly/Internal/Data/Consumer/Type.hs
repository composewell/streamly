-- |
-- Module      : Streamly.Internal.Data.Consumer.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Consumer is to Fold as Unfold is to Stream.
--
module Streamly.Internal.Data.Consumer.Type
    (
    -- * Types
      Consumer (..)

    -- Consumers
    , drainBy
    )
where

import Control.Monad (void)

-- All folds in the Fold module should be implemented as Refolds.
--
-- | Experimental type to provide a side input to the fold for generating the
-- initial state. For example, if we have to fold chunks of a stream and write
-- each chunk to a different file, then we can generate the file name using a
-- monadic action. This is a generalized version of 'Fold'.
--
-- /Internal/
data Consumer m c a b =
  -- | @Fold @ @ step @ @ inject @ @ extract@
  forall s. Consumer (s -> a -> m s) (c -> m s) (s -> m b)

-- |
--
-- /Internal/
{-# INLINABLE drainBy #-}
drainBy ::  Monad m => (a -> m b) -> Consumer m c a ()
drainBy f = Consumer (const (void . f)) (\_ -> return ()) return
