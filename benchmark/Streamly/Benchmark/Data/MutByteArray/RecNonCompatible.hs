{-# LANGUAGE TemplateHaskell #-}

module Streamly.Benchmark.Data.MutByteArray.RecNonCompatible
    ( RecNonCompatible(..)
    , valRecNonCompatible
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Benchmark.Data.MutByteArray.TH (genLargeRecord)

import qualified Streamly.Internal.Data.MutByteArray as Serialize

--------------------------------------------------------------------------------
-- Code
--------------------------------------------------------------------------------

$(genLargeRecord "RecNonCompatible" 50)
$(Serialize.deriveSerialize [d|instance Serialize.Serialize RecNonCompatible|])
