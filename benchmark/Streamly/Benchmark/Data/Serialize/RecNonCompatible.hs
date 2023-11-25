{-# LANGUAGE TemplateHaskell #-}

module Streamly.Benchmark.Data.Serialize.RecNonCompatible
    ( RecNonCompatible(..)
    , valRecNonCompatible
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Benchmark.Data.Serialize.TH (genLargeRecord)

import qualified Streamly.Internal.Data.MutByteArray as Serialize

--------------------------------------------------------------------------------
-- Code
--------------------------------------------------------------------------------

$(genLargeRecord "RecNonCompatible" 50)
$(Serialize.deriveSerialize [d|instance Serialize.Serialize RecNonCompatible|])
