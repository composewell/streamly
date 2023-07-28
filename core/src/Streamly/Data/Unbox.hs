-- |
-- Module      : Streamly.Data.Unbox
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Data.Unbox
    ( Unbox(..)
    , MutableByteArray
    , deriveUnbox
    , deriveUnboxWith
    ) where

import Streamly.Internal.Data.Unbox
import Streamly.Internal.Data.Unbox.TH
