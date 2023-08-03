-- |
-- Module      : Streamly.Data.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Data.Serialize
    ( Unbox(..)
    , MutableByteArray
    , deriveUnbox
    , deriveUnboxWith
    ) where

import Streamly.Internal.Data.Unbox
import Streamly.Internal.Data.Unbox.TH
