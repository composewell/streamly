-- |
-- Module      : Streamly.Internal.Control.Monad
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Additional "Control.Monad" utilities.

{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Internal.Control.Monad
    ( discard
    )
where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, catch, SomeException)

-- | Discard any exceptions or value returned by an effectful action.
--
-- /Internal/
--
discard :: MonadCatch m => m b -> m ()
discard action = (void $ action) `catch` (\(_ :: SomeException) -> return ())
