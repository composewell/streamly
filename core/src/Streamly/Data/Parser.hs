-- |
-- Module      : Streamly.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Fast backtracking parsers with stream fusion and native streaming
-- capability. Parsers are just more powerful folds, folds and parsers can be
-- interconverted.
--
-- Note that these parsers do not report the error context (e.g. line number or
-- column). Though this can be supported in future.
--
module Streamly.Data.Parser
    (
    -- * Parser Type
      Parser

    -- XXX Should we use Fold.fromParser instead?
    -- -- * Downgrade to Fold
    -- , toFold

    -- * Parsers
    -- ** From Folds
    , fromFold

    -- ** Without Input
    -- , fromFoldMaybe
    , fromPure
    , fromEffect
    , die
    -- , dieM

    -- ** Element parsers
    , peek

    -- All of these can be expressed in terms of either
    , one
    , element
    , except
    , oneOf
    , noneOf
    , satisfy
    -- , maybe
    -- , either
    , eof

    -- ** Sequences
    , eqBy
    , list

    -- * Combinators
    -- Mapping on output
    -- , rmapM

    -- ** Mapping on input
    , lmap
    , lmapM

     -- * Map on output
    , rmapM

    -- ** Filtering
    , filter

    -- ** Look Ahead
    , lookAhead

    -- ** Tokenize by length
    -- , takeBetween
    , takeEQ
    -- , takeGE
    -- , takeP

    -- ** Tokenize by predicate
    -- , takeWhileP
    , takeWhile
    , takeWhile1
    , dropWhile
    -- , takeEndBy
    -- , takeEndByEsc
    -- , takeStartBy
    , wordBy

    -- ** Grouping
    , groupBy
    -- , groupByRolling
    -- , groupByRollingEither

    -- ** Framing
    , wordFramedBy
    , wordQuotedBy

    -- ** Splitting
    , many
    , some
    , manyTill

    -- ** De-interleaving
    , deintercalate
    )

where

import Streamly.Internal.Data.Parser
import Prelude hiding (dropWhile, takeWhile, filter)
