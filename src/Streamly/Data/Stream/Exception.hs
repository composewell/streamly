-- |
-- Module      : Streamly.Data.Stream.Exception
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- This module is designed such that it does not conflict with
-- "Streamly.Data.Stream" module.
--
-- >>> import qualified Streamly.Data.Stream.Exception as Stream
--
module Streamly.Data.Stream.Exception
    (
    -- * Lifted Exceptions
      after
    , bracket
    -- , bracket3
    , finally
    )
where

import Streamly.Internal.Data.Stream.Exception.Lifted
    ( bracket, finally, after )
