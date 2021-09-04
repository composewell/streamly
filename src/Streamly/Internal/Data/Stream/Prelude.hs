{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Prelude
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low level functions using StreamK as the intermediate stream type. These
-- functions are used in SerialT/AsyncT/AheadT/ParallelT stream modules to
-- implement their instances..
--
module Streamly.Internal.Data.Stream.Prelude
    (
    -- * Running Effects
      drain

    -- * Conversion operations
    , fromList
    , toList

    -- * Fold operations
    , foldrM
    , foldrMx
    , foldr

    , foldlx'
    , foldlMx'
    , foldl'
    , fold

    -- * Zip style operations
    , eqBy
    , cmpBy
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Fold.Type (Fold (..))

#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
import qualified Streamly.Internal.Data.Stream.StreamK.Type as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD.Type as S
#endif

import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

import Prelude hiding (foldr, repeat)

#include "PreludeCommon.hs"
