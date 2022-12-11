-- |
-- Module      : Streamly.FileSystem.Dir
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC

module Streamly.FileSystem.Dir
    (
    -- * Streams
      read
    , readEither
    )
where

import Streamly.Internal.FileSystem.Dir
import Prelude hiding (read)
