{-# LANGUAGE TemplateHaskell #-}

module Streamly.Benchmark.Data.Serialize.RecNonCompatible
    ( RecNonCompatible(..)
    , valRecNonCompatible
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Benchmark.Data.Serialize.TH (genLargeRecord)

import qualified Streamly.Internal.Data.Serialize.TH as Serialize

--------------------------------------------------------------------------------
-- Code
--------------------------------------------------------------------------------

$(genLargeRecord "RecNonCompatible" 50)
$(Serialize.deriveSerialize ''RecNonCompatible)
