-- |
-- Module      : Streamly.Data.Parser.ParserK
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Parsers using Continuation Passing Style (CPS). See notes in
-- "Streamly.Data.Parser" module to know when to use this module.
--
-- To run a 'ParserK' convert it to a 'Parser' and then run it.
--
module Streamly.Data.Parser.ParserK
    (
    -- * Parser Type
      ParserK
    , Parser

    -- * Parsers
    -- ** Conversions
    , fromFold
    , fromParser
    , toParser

    -- ** Without Input
    , fromPure
    , fromEffect
    , die
    )

where

import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Parser.ParserK.Type
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD

-- | Convert a 'Fold' to a 'ParserK'.
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> ParserK a m b
fromFold = ParserD.toParserK . ParserD.fromFold

-- | Convert a 'Parser' to a 'ParserK'.
--
{-# INLINE fromParser #-}
fromParser :: Monad m => ParserD.Parser a m b -> ParserK a m b
fromParser = ParserD.toParserK

-- | Convert a 'ParserK' to a 'Parser'.
--
{-# INLINE toParser #-}
toParser :: Monad m => ParserK a m b -> ParserD.Parser a m b
toParser = ParserD.fromParserK
