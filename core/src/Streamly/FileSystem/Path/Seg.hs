-- |
-- Module      : Streamly.FileSystem.Path.Seg
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Represent 'Rooted' or 'Unrooted' type path segments explicitly as separate
-- types for the safety of path append operation. A Rooted path is an absolute
-- path or a path that is relative to the current directory with a leading dot.
-- Rooted paths cannot be appended to other paths.
--
-- See the overview in the "Streamly.FileSystem.Path" module for more details.
--
module Streamly.FileSystem.Path.Seg
    (
    -- * Types
      Rooted
    , Unrooted
    , IsSeg

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , rt
    , ur

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , rtE
    , urE

    -- * Operations
    , extend
    )
where

import Streamly.Internal.FileSystem.Path.Seg
