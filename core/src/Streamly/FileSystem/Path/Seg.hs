-- |
-- Module      : Streamly.FileSystem.Path.Seg
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.FileSystem.Path.Seg
    (
    -- * Types
      Rooted
    , Branch
    , IsSeg

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , rt
    , br

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , rtE
    , brE

    -- * Operations
    , append
    )
where

import Streamly.Internal.FileSystem.Path.Seg
