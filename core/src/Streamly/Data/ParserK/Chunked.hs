-- |
-- Module      : Streamly.Data.ParserK.Chunked
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Parsers using Continuation Passing Style (CPS). See notes in
-- "Streamly.Data.Parser" module to know when to use this module.
--
-- To run a 'ChunkParserK' use 'Streamly.Data.StreamK.parseChunks'.
--
module Streamly.Data.ParserK.Chunked
    (
    -- * Parser Type
      ChunkParserK

    -- * Parsers
    -- ** Conversions
    , fromFold
    , fromParser
    -- , toParser

    -- ** Without Input
    , fromPure
    , fromEffect
    , die
    )

where

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Unboxed (Unbox)
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD

import Streamly.Internal.Data.Parser.ParserK.Chunked

-- | Convert a 'Fold' to a 'ChunkParserK'.
--
{-# INLINE fromFold #-}
fromFold :: (MonadIO m, Unbox a) => Fold m a b -> ChunkParserK a m b
fromFold = fromParser . ParserD.fromFold
