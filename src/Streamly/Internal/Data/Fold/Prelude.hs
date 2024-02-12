-- |
-- Module      : Streamly.Internal.Data.Fold.Prelude
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Fold.Prelude
    (
    -- * Channel
      module Streamly.Internal.Data.Fold.Channel
    -- * Concurrency
    , module Streamly.Internal.Data.Fold.Concurrent
    -- * Time
    , module Streamly.Internal.Data.Fold.Time
    -- * Deprecated
    , module Streamly.Internal.Data.Fold.SVar
    )
where

import Streamly.Internal.Data.Fold.Channel
import Streamly.Internal.Data.Fold.Concurrent
import Streamly.Internal.Data.Fold.SVar
import Streamly.Internal.Data.Fold.Time
