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
-- column). This may be supported in future.
--
-- mtl instances are not provided. If the 'Parser' type is the top most layer
-- (which should be the case almost always) you can just use 'fromEffect' to
-- execute the lower layer monad effects.
--
module Streamly.Data.Parser
    (
    -- * Parser Type
      Parser

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
    , peek
    , eof

    -- ** Element parsers

    -- All of these can be expressed in terms of either
    , one
    -- , oneEq
    -- , oneNotEq
    , oneOf
    , noneOf
    , satisfy
    -- , maybe
    -- , either

    -- ** Sequences
    , eqBy
    , listEqBy
    , listEq

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
