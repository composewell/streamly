-- |
-- Module      : Streamly.FileSystem.Path.Node
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Represent 'File' or 'Dir' type path nodes explicitly as separate types for
-- the safety of path append operation. A 'Dir' path is a branching or
-- intermediate node whereas a 'File' type is a terminal or leaf node. We
-- cannot append a path to a 'File' type path.
--
-- See the overview in the "Streamly.FileSystem.Path" module for more details.
--
module Streamly.FileSystem.Path.Node
    (
    -- * Types
      File
    , Dir
    , IsNode

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , dir
    , file

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , dirE
    , fileE

    -- * Operations
    , join
    )
where

import Streamly.Internal.FileSystem.Path.Node
