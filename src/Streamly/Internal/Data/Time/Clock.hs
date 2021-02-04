-- |
-- Module      : Streamly.Internal.Data.Time.Clock
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Time.Clock
    (
    -- * System clock
      Clock(..)
    , getTime
    )
where

import Streamly.Internal.Data.Time.System
