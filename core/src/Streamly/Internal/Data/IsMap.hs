-- |
-- Module      : Streamly.Internal.Data.IsMap
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.IsMap (IsMap(..)) where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

-- XXX Try unpacked-containers

class IsMap f where
    type Key f :: Type

    mapEmpty :: f a
    mapAlterF :: Functor g =>
        (Maybe a -> g (Maybe a)) -> Key f -> f a -> g (f a)
    -- These can be implemented in terms of alterF itself
    mapLookup :: Key f -> f a -> Maybe a
    mapInsert :: Key f -> a -> f a -> f a
    mapDelete :: Key f -> f a -> f a
    mapUnion :: f a -> f a -> f a
    mapNull :: f a -> Bool

instance Ord k => IsMap (Map k) where
    type Key (Map k) = k

    mapEmpty = Map.empty
    mapAlterF = Map.alterF
    mapLookup = Map.lookup
    mapInsert = Map.insert
    mapDelete = Map.delete
    mapUnion = Map.union
    mapNull = Map.null

instance (Hashable k) => IsMap (HashMap k) where
    type Key (HashMap k) = k

    mapEmpty = HashMap.empty
    mapAlterF = HashMap.alterF
    mapLookup = HashMap.lookup
    mapInsert = HashMap.insert
    mapDelete = HashMap.delete
    mapUnion = HashMap.union
    mapNull = HashMap.null

instance IsMap IntMap.IntMap where
    type Key IntMap.IntMap = Int

    mapEmpty = IntMap.empty
    mapAlterF = IntMap.alterF
    mapLookup = IntMap.lookup
    mapInsert = IntMap.insert
    mapDelete = IntMap.delete
    mapUnion = IntMap.union
    mapNull = IntMap.null
