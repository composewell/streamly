-- |
-- Module      : Streamly.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Fast backtracking parsers with stream fusion and native streaming
-- capability.
--
module Streamly.Data.Parser
    (
      Parser
    , ParseError

    -- * Downgrade to Fold
    , toFold

    -- First order parsers
    -- * Accumulators
    , fromFold
    , fromFoldMaybe
    , fromPure
    , fromEffect
    , die
    , dieM

    -- * Map on input
    , lmap
    , lmapM
    , filter

    -- * Element parsers
    , peek

    -- All of these can be expressed in terms of either
    , one
    , element
    , except
    , oneOf
    , noneOf
    , eof
    , satisfy
    , maybe
    , either

    -- * Sequence parsers (tokenizers)
    --
    -- | Parsers chained in series, if one parser terminates the composition
    -- terminates.

    , lookAhead

    -- ** By length
    -- | Grab a sequence of input elements without inspecting them
    , takeBetween
    , takeEQ -- takeBetween n n
    , takeGE -- takeBetween n maxBound
    , takeP

    -- Grab a sequence of input elements by inspecting them
    -- ** Exact match
    , eqBy
    , list

    -- ** By predicate
    , takeWhileP
    , takeWhile
    -- $takeWhile
    , takeWhile1
    , dropWhile

    -- ** Separators
    , takeEndBy
    , takeEndByEsc
    , takeStartBy
    , wordBy

    -- ** By comparing
    , groupBy
    , groupByRolling
    , groupByRollingEither

    -- ** Framing
    , wordFramedBy
    , wordQuotedBy

    -- Second order parsers (parsers using parsers)
    -- * Binary Combinators

    -- ** Sequential Applicative
    , serialWith

    -- ** Sequential Interleaving
    -- Use two folds, run a primary parser, its rejected values go to the
    -- secondary parser.
    , deintercalate

    -- ** Sequential Alternative
    , alt

    -- * N-ary Combinators
    -- ** Sequential Collection
    , concatSequence
    , concatMap

    -- ** Sequential Repetition
    , many
    , some
    , manyTill
    )
where
import Streamly.Internal.Data.Parser
import Prelude hiding
    ( any, all, dropWhile, take, takeWhile, sequence, concatMap, maybe, either
    , filter )
