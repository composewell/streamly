-- |
-- Module      : Streamly.Data.Unbox
-- Copyright   : (c) 2022 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module provides the 'Unbox' type class for unboxing values in
-- 'MutableByteArray'.
--

module Streamly.Data.Unbox
    (
      Unbox(..)
    , MutableByteArray(..)
    )
where

import Streamly.Internal.Data.Unboxed (Unbox(..), MutableByteArray(..))

-- $setup
-- >>> :m
-- >>> :set -XFlexibleContexts
-- >>> :set -package streamly
-- >>> import Streamly.Internal.Data.Unboxed (Unbox(..), MutableByteArray(..))
