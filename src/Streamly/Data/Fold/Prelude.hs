-- |
-- Module      : Streamly.Data.Fold.Prelude
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
module Streamly.Data.Fold.Prelude
    (
        toHashMapIO
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Streamly.Internal.Data.Fold
import Streamly.Internal.Data.Fold.Container (toContainerIO)

{-# INLINE toHashMapIO #-}
toHashMapIO :: (MonadIO m, Hashable k, Ord k) =>
    (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
toHashMapIO = toContainerIO
