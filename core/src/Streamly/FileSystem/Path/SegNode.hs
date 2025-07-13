-- |
-- Module      : Streamly.FileSystem.Path.SegNode
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Use 'Rooted' or 'Unrooted' path segment type annotations as well as 'File' and
-- 'Dir' node type annotations on the same path for the safety of path append
-- operation. A Rooted path cannot be appended to other paths, and you canno
-- append a path to a 'File' type path.
--
-- See the overview in the "Streamly.FileSystem.Path" module for more details.
--

module Streamly.FileSystem.Path.SegNode
    (
    -- * Statically Verified Path Literals
    -- | Quasiquoters.
      rtdir
    , urdir
    , rtfile
    , urfile

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , rtdirE
    , urdirE
    , rtfileE
    , urfileE

    -- * Operations
    , extend
    )
where

import Streamly.Internal.FileSystem.Path.SegNode
