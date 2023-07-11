{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Fast, composable stream consumers with ability to terminate, backtrack and
-- fail, supporting stream fusion. Parsers are a natural extension of
-- "Streamly.Data.Fold". Parsers and folds can be interconverted.
--
-- Please refer to "Streamly.Internal.Data.Parser" for functions that have
-- not yet been released.
--
module Streamly.Data.Parser
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Overview
    -- $overview

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
    , groupByRolling
    , groupByRollingEither

    -- ** Framing
    -- , wordFramedBy
    , wordWithQuotes
    -- , wordProcessQuotes
    -- , wordKeepQuotes

    -- -- * Alternative
    -- , alt

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

#include "DocTestDataParser.hs"

-- $overview
--
-- Several combinators in this module can be many times faster than CPS based
-- parsers because of stream fusion. For example,
-- 'Streamly.Internal.Data.Parser.many' combinator in this module is much
-- faster than the 'Control.Applicative.many' combinator of
-- 'Control.Applicative.Alternative' type class used by CPS based parsers.
--
-- The use of 'Alternative' type class, in parsers has another drawback.
-- Alternative based parsers use plain Haskell lists to collect the results. In
-- a strict Monad like IO, the results are necessarily buffered before they can
-- be consumed.  This may not perform optimally in streaming applications
-- processing large amounts of data.  Equivalent combinators in this module can
-- consume the results of parsing using a 'Fold' or another parser, thus
-- providing a scalable and composable consumer.
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
-- The 'Parser' type represents a stream consumer by composing state as data
-- which enables stream fusion. Stream fusion generates a tight loop without
-- any constructor allocations between the stages, providing C like performance
-- for the loop. Stream fusion works when multiple functions are combined in a
-- pipeline statically. Therefore, the operations in this module must be
-- inlined and must not be used recursively to allow for stream fusion. Note
-- that operations like 'sequence', and 'asum' that compose pasrers using
-- recursion should be avoided with these parsers. You can use these with the
-- 'ParserK' module instead.
--
-- Using the 'Parser' type, parsing operations like 'one', 'splitWith' etc.
-- degrade quadratically (O(n^2)) when combined many times. If you need to
-- combine these operations, say more than 8 times in a single loop, then you
-- should consider using the continuation style parser type 'ParserK' instead.
-- Also, if you need to use these operations in a recursive loop you should use
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
-- 'Parser' can be converted to 'ParserK' but not the other way around.
