-- |
-- Module      : Streamly.Data.Array.Generic
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Data.Array.Generic
    ( Array

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

    -- * Folding Arrays
    -- , A.streamFold
    -- , A.fold
    )
where

import Streamly.Internal.Data.Array.Generic (Array)

import qualified Streamly.Internal.Data.Array.Generic as A
