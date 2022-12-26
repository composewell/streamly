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
-- Also see the following modules for more pre-release operations:
--
-- * "Streamly.Internal.Data.Stream.Concurrent"
-- * "Streamly.Internal.Data.Stream.Time"
-- * "Streamly.Internal.Data.Stream.Exception.Lifted"
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
