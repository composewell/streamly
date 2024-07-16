-- |
-- Module      : Streamly.Data.Scan
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Data.Scan
    (
    -- * Type
      Scan

    -- * Primitive Scans
    , identity
    , function
    , functionM
    , filter
    , filterM

    -- * Combinators
    , compose
    , teeWithMay
    , teeWith
    )
where

import Streamly.Internal.Data.Scan
import Prelude hiding (filter)
