{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.IsMap.HashMap
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Adds an orphan HashMap instance for the IsMap type class from streamly-core
-- package. This is useful for various combinators that use a map type. We
-- cannot define this in streamly-core as it adds several non-boot library
-- dependencies on streamly-core.

module Streamly.Internal.Data.IsMap.HashMap () where

import Data.Hashable (Hashable)
import Streamly.Internal.Data.IsMap (IsMap(..))

import qualified Data.HashMap.Strict as HashMap

#if MIN_VERSION_hashable(1,4,0)
instance (Hashable k) => IsMap (HashMap.HashMap k) where
#else
instance (Hashable k, Eq k) => IsMap (HashMap.HashMap k) where
#endif
    type Key (HashMap.HashMap k) = k

    mapEmpty = HashMap.empty
    mapAlterF = HashMap.alterF
    mapLookup = HashMap.lookup
    mapInsert = HashMap.insert
    mapDelete = HashMap.delete
    mapUnion = HashMap.union
    mapNull = HashMap.null
    mapTraverseWithKey = HashMap.traverseWithKey
