{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Scanl.Prelude
-- Copyright   : (c) 2025 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- All Scan related combinators including the streamly-core
-- "Streamly.Data.Scanl" module, concurrency, unordered container operations.
--
module Streamly.Data.Scanl.Prelude
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * "Streamly.Data.Scanl"
    -- | All "Streamly.Data.Scanl" combinators are re-exported via this
    -- module. For more pre-release combinators also see
    -- "Streamly.Internal.Data.Scanl" module.
      module Streamly.Data.Scanl
    -- * Concurrent Operations
    -- ** Configuration
    , Config
    , maxBuffer
    , boundThreads
    , inspect

    -- ** Combinators
    , parTeeWith
    , parDistributeScanM
    , parDistributeScan
    , parDemuxScanM
    , parDemuxScan
    )
where

import Streamly.Data.Scanl
import Streamly.Internal.Data.Scanl.Prelude

#include "DocTestDataScanl.hs"
