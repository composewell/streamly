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

import Streamly.Internal.Data.Fold
import Streamly.Internal.Data.Fold.Container (toContainerIO)
import Streamly.Internal.Data.IsMap (IsMap(..))

{-# INLINE toHashMapIO #-}
toHashMapIO :: (MonadIO m, Ord (Key (HashMap k)), IsMap (HashMap k)) =>
    (a -> Key (HashMap k)) -> Fold m a b -> Fold m a (HashMap k b)
toHashMapIO = toContainerIO
