-- |
-- Module      : Streamly.FileSystem.Path
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.FileSystem.Path
    (
    -- * Type
      Path

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Construction
    , fromString

    -- * Statically Verified String Literals
    -- | Quasiquoters.
    , path

    -- * Statically Verified Strings
    -- | Template Haskell expression splices.
    , pathE

    -- * Elimination
    , toString

    -- * Operations
    -- , dropTrailingSeparators
    , isLocation
    , isSegment

    -- * Combinators
    , append
    )
where

import Streamly.Internal.FileSystem.Path
