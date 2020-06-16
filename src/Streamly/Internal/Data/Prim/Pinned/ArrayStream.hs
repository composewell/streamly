{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Prim.Pinned.ArrayStream
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of arrays.
--
module Streamly.Internal.Data.Prim.Pinned.ArrayStream
    (
    -- * Creation
      arraysOf

    -- * Flattening to elements
    , concat
    , concatRev
    , interpose
    , interposeSuffix
    , intercalateSuffix

    -- * Transformation
    , splitOn
    , splitOnSuffix
    , compact -- compact

    -- * Elimination
    , toArray
    )
where

import Streamly.Internal.Data.Prim.Pinned.Array.Types (Array(..), length)
import qualified Streamly.Internal.Data.Prim.Pinned.Array as A
import qualified Streamly.Internal.Data.Prim.Pinned.Array.Types as A
import qualified Streamly.Internal.Data.Prim.Pinned.Mutable.Array.Types as MA

#include "prim-array-stream.hs"
