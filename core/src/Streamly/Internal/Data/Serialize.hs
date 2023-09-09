{-# LANGUAGE TemplateHaskell #-}

-- This is required as all the instances in this module are orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.Internal.Data.Serialize
    ( module Streamly.Internal.Data.Serialize.Type
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Proxy (Proxy)
import Streamly.Internal.Data.Serialize.Type
import qualified Streamly.Internal.Data.Serialize.TH as Serialize

--------------------------------------------------------------------------------
-- Common instances
--------------------------------------------------------------------------------

-- Note
-- ====
--
-- Even a non-functional change such as changing the order of constructors will
-- change the instance derivation.
--
-- This will not pose a problem if both, encode, and decode are done by the same
-- version of the application. There *might* be a problem if version that
-- encodes differs from the version that decodes.
--
-- We need to add some compatibility tests using different versions of
-- dependencies.
--
-- Although such chages for the most basic types won't happen we need to detect
-- if it ever happens.
--
-- Should we worry about these kind of changes and this kind of compatibility?
-- This is a problem for all types of derivations that depend on the order of
-- constructors, for example, Enum.

$(Serialize.deriveSerialize ''Maybe)
$(Serialize.deriveSerialize ''Either)
$(Serialize.deriveSerializeWith
      (Serialize.defaultConfig {Serialize.unconstrained = ["t"]})
      ''Proxy)
