-- |
-- Module      : Streamly.FileSystem.DirIO
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- High performance and streaming APIs for reading directories.
--
-- >>> import qualified Streamly.FileSystem.DirIO as Dir
--

module Streamly.FileSystem.DirIO
    (
    -- * Streams
      read
    , readEither
    )
where

import Streamly.Internal.FileSystem.DirIO
import Prelude hiding (read)
