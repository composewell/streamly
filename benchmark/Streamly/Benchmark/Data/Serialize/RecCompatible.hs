{-# LANGUAGE TemplateHaskell #-}

module Streamly.Benchmark.Data.Serialize.RecCompatible
    ( RecCompatible(..)
    , valRecCompatible
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.MutByteArray (Serialize)
import Streamly.Benchmark.Data.Serialize.TH (genLargeRecord)

import qualified Streamly.Internal.Data.MutByteArray as Serialize

--------------------------------------------------------------------------------
-- Code
--------------------------------------------------------------------------------

$(genLargeRecord "RecCompatible" 50)
$(Serialize.deriveSerializeWith
      (Serialize.encodeRecordFields True)
      [d|instance Serialize RecCompatible|])
