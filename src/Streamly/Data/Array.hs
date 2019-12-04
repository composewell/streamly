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
    ( Array

    -- * Construction
    , A.fromListN
    , A.fromList

    -- Stream Folds
    , A.fromStreamN
    , A.fromStream

    -- MonadicAPIs
    , A.writeN
    , A.write

    -- * Elimination

    , A.toStream
    , A.toStreamRev
    , A.read

    -- * Random Access

    -- * Folding Arrays
    , A.streamFold
    , A.fold

    , A.length
    )
where

import Streamly.Internal.Data.Array (Array)

import qualified Streamly.Internal.Data.Array as A
