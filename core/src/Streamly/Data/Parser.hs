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
-- == Performance Notes
--
-- This module is designed for fusion, inline the operations in this module for
-- fusion to occur, avoid using these operations in recursive calls, avoid
-- operations like 'sequence', 'asum' on these parsers. If you need these then
-- use the 'ParserK' module instead.
--
-- The 'Parser' type represents a stream consumer by composing state as data
-- which enables stream fusion. Stream fusion generates a tight loop without
-- any constructor allocations between the stages, providing C like performance
-- for the loop. Stream fusion works when multiple functions are combined in a
-- pipeline statically. Therefore, the operations in this module must be
-- inlined and must not be used recursively to allow for stream fusion.
--
-- Using the 'Parser' type parsing operations like 'one', 'splitWith' etc.
-- degrade quadratically (O(n^2)) when combined many times. If you need to
-- combine these operations, say more than 50 times in a single loop, then you
-- should use the continuation style parser type 'ParserK' instead. Also, if
-- you need to use these operations in a recursive loop you should use
-- 'ParserK' instead.
--
-- The 'ParserK' type represents a stream consumer by composing function calls,
-- therefore, a function call overhead is incurred at each composition. It is
-- quite fast in general but may be a few times slower than a fused parser.
-- However, it allows for scalable dynamic composition especially parsers can
-- be used in recursive calls. Using the 'ParserK' type operations like
-- 'splitWith' provide linear (O(n)) performance with respect to the number of
-- compositions..
--
-- 'Parser' and 'ParserK' types can be interconverted.
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
    , streamEqBy
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
    -- , wordFramedBy
    , wordWithQuotes

    -- * Alternative
    , alt

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
