-- |
-- Module      : Streamly.Data.ParserK
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Parsers using Continuation Passing Style (CPS). See notes in
-- "Streamly.Data.Parser" module to know when to use this module.
--
-- To run a 'ParserK' use 'Streamly.Data.StreamK.parseChunks'.
--
module Streamly.Data.ParserK {-# DEPRECATED "Please use \"Streamly.Data.ChunkParserK\" instead." #-}
    (
    -- * Parser Type
      ParserK

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

import Streamly.Data.ChunkParserK

type ParserK a m b = ChunkParserK a m b
