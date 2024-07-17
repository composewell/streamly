-- |
-- Module      : Streamly.FileSystem.Path.LocSeg
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.FileSystem.Path.LocSeg
    (
    -- * Types
      Loc
    , Seg
    , IsLocSeg

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , loc
    , seg

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , locE
    , segE

    -- * Operations
    , append
    )
where

import Streamly.Internal.FileSystem.Path.LocSeg
