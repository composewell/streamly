-- |
-- Module      : Streamly.Data.Stream.Time
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module provides time related streaming abstractions.
--
-- The names in this module do not conflict with other stream modules,
-- therefore, you can import it in the same namespace:
--
-- >>> import qualified Streamly.Data.Stream.Time as Stream
--
module Streamly.Data.Stream.Time
    (
    -- ** Timers
      interject

    -- ** Trimming
    , takeInterval
    , dropInterval

    -- ** Chunking
    , intervalsOf

    -- ** Sampling
    , sampleIntervalEnd
    , sampleIntervalStart
    , sampleBurstEnd
    , sampleBurstStart
    )
where

import Streamly.Internal.Data.Stream.Time
