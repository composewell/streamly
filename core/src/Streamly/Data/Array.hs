-- |
-- Module      : Streamly.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Data.Array
    (
      A.Array

    -- * Construction
    , A.fromListN
    , A.fromList

    -- MonadicAPIs
    , A.writeN
    , A.write

    -- * Streams
    , A.read
    , A.readRev

    -- * Unfolds
    , A.reader

    -- * Random Access
    , A.length
     -- , (!!)
    , A.getIndex
    )
where

import qualified Streamly.Data.Array.Unboxed as A
