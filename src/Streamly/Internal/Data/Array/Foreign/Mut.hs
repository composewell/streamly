{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.Data.Array.Foreign.Mut
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Mutable arrays.

module Streamly.Internal.Data.Array.Foreign.Mut
    (
      Array

    -- Stream Folds
    , fromStreamN
    )
where

#include "inline.hs"

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Foreign.Storable (Storable(..))
import Prelude hiding (length, null, last, map, (!!), read, concat)

import Streamly.Internal.Data.Array.Foreign.Mut.Types (Array(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)

import qualified Streamly.Internal.Data.Array.Foreign.Mut.Types as MA
import qualified Streamly.Internal.Data.Stream.StreamD as D

-- XXX Deduplicate code between immutable and mutable arrays. Use freeze/thaw
-- where possible.

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Create an 'Array' from the first N elements of a stream. The array is
-- allocated to size N, if the stream terminates before N elements then the
-- array may hold less than N elements.
--
-- /Internal/
{-# INLINE fromStreamN #-}
fromStreamN :: (MonadIO m, Storable a) => Int -> SerialT m a -> m (Array a)
fromStreamN n m = do
    when (n < 0) $ error "writeN: negative write count specified"
    MA.fromStreamDN n $ D.toStreamD m
