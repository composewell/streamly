{-# LANGUAGE TemplateHaskell #-}

module Streamly.Benchmark.Data.Serialize.RecCompatible
    ( RecCompatible(..)
    , valRecCompatible
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Serialize (Serialize)
import Streamly.Benchmark.Data.Serialize.TH (genLargeRecord)

import qualified Streamly.Internal.Data.Serialize as Serialize

--------------------------------------------------------------------------------
-- Code
--------------------------------------------------------------------------------

$(genLargeRecord "RecCompatible" 50)
$(Serialize.deriveSerializeWith
      (Serialize.encodeRecordFields True Serialize.defaultConfig)
      [d|instance Serialize RecCompatible|])
