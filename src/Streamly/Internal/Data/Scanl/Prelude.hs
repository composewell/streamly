-- |
-- Module      : Streamly.Internal.Data.Scanl.Prelude
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Scanl.Prelude
    (
    -- * Channel
      module Streamly.Internal.Data.Fold.Channel
    -- * Concurrency
    , module Streamly.Internal.Data.Scanl.Concurrent
    )
where

import Streamly.Internal.Data.Fold.Channel
import Streamly.Internal.Data.Scanl.Concurrent
