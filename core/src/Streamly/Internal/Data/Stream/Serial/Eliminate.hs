-- |
-- Module      : Streamly.Internal.Data.Stream.Serial.Eliminate
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Serial.Eliminate
    (
      drain
    )
where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

import qualified Streamly.Internal.Data.Stream.Common as P

import Streamly.Internal.Data.Stream.Serial.Type

import Prelude hiding (map, mapM, repeat, filter)

#include "inline.hs"

-- |
-- > drain = mapM_ (\_ -> return ())
-- > drain = Stream.fold Fold.drain
--
-- Run a stream serially, discarding the results.
--
-- /Pre-release/
--
{-# INLINE drain #-}
drain :: Monad m => SerialT m a -> m ()
drain (SerialT m) = P.drain m
