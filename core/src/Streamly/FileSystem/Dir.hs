{-# OPTIONS_GHC -Wno-deprecations #-}
-- |
-- Module      : Streamly.FileSystem.Dir
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Warning\: The API of this module is subject to change in future releases.
-- Especially the type for representing paths may change from 'FilePath' to
-- something else.

module Streamly.FileSystem.Dir
{-# DEPRECATED "Please use \"Streamly.FileSystem.DirIO\" instead." #-}
    (
    -- * Streams
      read
    , readEither
    )
where

import Streamly.Internal.FileSystem.Dir
import Prelude hiding (read)
