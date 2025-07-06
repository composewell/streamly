{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Control.Exception
-- Copyright   : (c) 2025 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Exception handling and resource managment operations complementing
-- the "Control.Exception" module in base package.

module Streamly.Control.Exception
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup
    --
    -- * Resource Management
    -- | Exception safe, thread safe resource managment operations, similar to
    -- but more powerful than the @bracket@ and @finally@ operations available
    -- in the base package.
    --
    -- These operations support allocation and free only in the IO monad,
    -- hence the IO suffix.
    --
      AcquireIO
    , withAcquireIO
    , acquire
    , register
    , hook
    )
where

import Streamly.Internal.Control.Exception

#include "DocTestControlException.hs"
