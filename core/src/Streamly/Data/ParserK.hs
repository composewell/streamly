-- |
-- Module      : Streamly.Data.ParserK
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Parsers using Continuation Passing Style (CPS). See notes in
-- "Streamly.Data.Parser" module to know when to use this module.
--
-- To run a 'ParserK' use 'Streamly.Data.StreamK.parseChunks'.
--
module Streamly.Data.ParserK
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

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.Array (Array)
import qualified Streamly.Internal.Data.Parser as ParserD

import Streamly.Internal.Data.Parser.ParserK.Type

-- | Convert a 'Fold' to a 'ParserK'.
--
{-# INLINE fromFold #-}
fromFold :: (MonadIO m, Unbox a) => Fold m a b -> ParserK (Array a) m b
fromFold = fromParser . ParserD.fromFold
