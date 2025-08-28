-- |
-- Module      : Streamly.Data.Array.Generic
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Unconstrained version of "Streamly.Data.Array" module.
--
-- See the "Streamly.Data.Array" module for documentation.
--
module Streamly.Data.Array.Generic
    ( Array

    -- * Construction
    , fromListN
    , fromList

    -- MonadicAPIs
    , createOf
    , create

    -- * Conversion
    , toList

    -- * Streams
    , read
    , readRev

    -- * Unfolds
    , reader
    -- , A.readerRev

    -- * Stream of Arrays
    , chunksOf
    , toParserK
    , parse
    , parseBreak
    , parsePos
    , parseBreakPos

    -- * Random Access
    , length
    , getIndex

    -- -- * Folding Arrays
    -- , A.streamFold
    -- , A.fold

    -- * Deprecated
    , writeN
    , write
    )
where

import Streamly.Internal.Data.Array.Generic
import Prelude hiding (length, read)
