-- |
-- Module      : Streamly.FileSystem.Path.SegNode
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.FileSystem.Path.SegNode
    (
    -- * Statically Verified Path Literals
    -- | Quasiquoters.
      rtdir
    , brdir
    , rtfile
    , brfile

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , rtdirE
    , brdirE
    , rtfileE
    , brfileE

    -- * Operations
    , append
    )
where

import Streamly.Internal.FileSystem.Path.SegNode
