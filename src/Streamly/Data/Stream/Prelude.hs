-- |
-- Module      : Streamly.Data.Stream.Prelude
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module re-exports the "Streamly.Data.Stream" module from the
-- "streamly-core" package and additionally provides concurrency, time and
-- lifted exception operations as well in a single module.
--
module Streamly.Data.Stream.Prelude
    ( module Streamly.Data.Stream
    , module Streamly.Data.Stream.Concurrent
    , module Streamly.Data.Stream.Time
    , module Streamly.Data.Stream.Exception
    )
where

import Streamly.Data.Stream
import Streamly.Data.Stream.Concurrent
import Streamly.Data.Stream.Exception
import Streamly.Data.Stream.Time
