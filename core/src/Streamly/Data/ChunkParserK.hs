-- |
-- Module      : Streamly.Data.ChunkParserK
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- 'ChunkParserK' represents a Continuation Passing Style (CPS) parser that
-- parses a stream of chunks. See notes in "Streamly.Data.Parser" module to know
-- when to use CPS parsers.
--
-- 'ChunkParserK' is more efficient compared to 'ParserK' as the chunks can be
-- parsed more efficiently due to stream fusion. Use 'ChunkParserK' over
-- 'ParserK' whenever possible.
--
-- To run a 'ChunkParserK' use 'Streamly.Data.StreamK.parseChunks'.
--
module Streamly.Data.ChunkParserK
    (
    -- * Parser Type
      ChunkParserK

    -- * Parsers
    -- ** Conversions
    , fromFold
    , fromParser
    , toK
    -- , toParser

    -- ** Without Input
    , fromPure
    , fromEffect
    , die
    )

where

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Unbox (Unbox)
import qualified Streamly.Internal.Data.Parser as ParserD

import Streamly.Internal.Data.ChunkParserK

-- | Convert a 'Fold' to a 'ChunkParserK'.
--
{-# INLINE fromFold #-}
fromFold :: (MonadIO m, Unbox a) => Fold m a b -> ChunkParserK (Array a) m b
fromFold = fromParser . ParserD.fromFold
