-- |
-- Module      : Streamly.FileSystem.Path.Node
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.FileSystem.Path.Node
    (
    -- * Types
      File
    , Dir
    , IsFileDir

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , dir
    , file

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , dirE
    , fileE

    -- * Operations
    , append
    )
where

import Streamly.Internal.FileSystem.Path.Node
