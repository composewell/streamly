{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Parsers are more powerful 'Streamly.Data.Fold.Fold's:
--
-- * folds cannot fail but parsers can fail and backtrack.
-- * folds can be composed as a Tee but parsers cannot.
-- * folds can be used for scanning but parsers cannot.
-- * folds can be converted to parsers.
--
-- Streamly parsers support all operations offered by popular Haskell parser
-- libraries. They operate on a generic input type, support streaming, and are
-- faster.
--
-- Like folds, parsers use stream fusion, compiling to efficient low-level code
-- comparable to the speed of C. Parsers are suitable for high-performance
-- parsing of streams.
--
-- Operations in this module are designed to be composed statically rather than
-- dynamically. They are inlined to enable static fusion. More importantly,
-- they are not designed to be used recursively. Recursive use will break
-- fusion and will lead to quadratic performance slowdown. For dynamic or
-- recursive composition use the continuation passing style (CPS) operations
-- from the "Streamly.Data.ParserK" module. 'Parser' and
-- 'Streamly.Data.ParserK.ParserK' types are interconvertible.
--
-- == Using Parsers
--
-- This module provides elementary parsers and parser combinators that can be
-- used to parse a stream of data. Additionally, all the folds from the
-- "Streamly.Data.Fold" module can be converted to parsers using 'fromFold'.
-- All the parsing functionality provided by popular parsing libraries, and
-- more is available. Also see "Streamly.Unicode.Parser" module for Char stream
-- parsers.
--
-- A data stream can be transformed to a stream of parsed data elements. Parser
-- combinators can be used to create a pipeline of folds or parsers such that
-- the next fold or parser consumes the result of the previous parser. See
-- 'Streamly.Data.Stream.parse' and 'Streamly.Data.Stream.parseMany' to run
-- these parsers on a stream.
--
-- == Parser vs ParserK
--
-- There are two functionally equivalent parsing modules,
-- "Streamly.Data.Parser" (this module) and "Streamly.Data.ParserK". The latter
-- is a CPS based wrapper over the former, and can be used for parsing in
-- general. "Streamly.Data.Parser" enables stream fusion and should be
-- preferred over "Streamly.Data.ParserK" for high performance stream parsing
-- use cases. However, there are a few cases where this module is not
-- suitable and ParserK should be used instead.
--
-- For static fusion, parser combinators have to use strict pattern matching on
-- arguments of type Parser. This leads to infinte loop when a parser is
-- defined recursively, due to strict evaluation of the recursive call. For
-- example, the following implementation loops infinitely because of the
-- recursive use of parser @p@ in the @*>@ combinator:
--
-- >>> import Streamly.Data.Parser (Parser)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import Control.Applicative ((<|>))
--
-- >>> :{
-- >>> p :: Monad m => Parser Char m String
-- >>> p = Parser.satisfy (== '(') *> p <|> Parser.fromFold Fold.toList
-- >>> :}
--
-- Use ParserK when recursive use is required:
--
-- >>> import Streamly.Data.ParserK (ParserK)
-- >>> import qualified Streamly.Data.StreamK as StreamK
-- >>> import qualified Streamly.Internal.Data.StreamK as StreamK (parse)
-- >>> import qualified Streamly.Internal.Data.ParserK as ParserK (adapt)
--
-- >>> :{
-- >>> p :: Monad m => ParserK Char m String
-- >>> p = ParserK.adapt (Parser.satisfy (== '(')) *> p <|> ParserK.adapt (Parser.fromFold Fold.toList)
-- >>> :}
--
-- >>> StreamK.parse p $ StreamK.fromStream $ Stream.fromList "hello"
-- Right "hello"
--
-- For this reason Applicative, Alternative or Monad compositions with
-- recursion cannot be used with the 'Parser' type. Alternative type class based
-- operations like 'asum' and Alternative based generic parser combinators use
-- recursion. Similarly, Applicative type class based operations like
-- 'Prelude.sequence' use recursion. Custom implementations of many such
-- operations are provided in this module (e.g. 'some', 'many'), and those
-- should be used instead.
--
-- Another limitation of Parser type is due to the quadratic complexity causing
-- slowdown when too many nested compositions are used. Especially Applicative,
-- Monad, Alternative instances, and sequenced parsing operations (e.g. nested
-- 'one', and 'splitWith') degrade the performance quadratically (O(n^2)) when
-- combined @n@ times, roughly 8 or less sequenced parsers are fine. READ THE
-- DOCS OF APPLICATIVE, MONAD AND ALTERNATIVE INSTANCES.
--
-- == Streaming Parsers
--
-- With 'Streamly.Data.ParserK.ParserK' you can use the generic Alternative
-- type class based parsers from the
-- <https://hackage.haskell.org/package/parser-combinators parser-combinators>
-- library or similar. However, we recommend that you use the equivalent
-- functionality from this module for better performance and for streaming
-- behavior.
--
-- Firstly, the combinators in this module are faster due to stream fusion.
-- Secondly, these are streaming in nature as the results can be passed
-- directly to other stream consumers (folds or parsers). The Alternative type
-- class based parsers would end up buffering all the results in lists before
-- they can be consumed.
--
-- When recursion or heavy nesting is needed use ParserK.
--
-- == Error Reporting
--
-- These parsers do not report the error context (e.g. line number or column).
-- This may be supported in future.
--
-- == Monad Transformer Stack
--
-- 'MonadTrans' instance is not provided. If the 'Parser' type is the top most
-- layer (which should be the case almost always) you can just use 'fromEffect'
-- to execute the lower layer monad effects.
--
-- == Experimental APIs
--
-- Please refer to "Streamly.Internal.Data.Parser" for functions that have not
-- yet been released.
--
module Streamly.Data.Parser
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Parser Type
      Parser
    , ParseError

    -- -- * Downgrade to Fold
    -- , toFold

    -- * Elementary Parsers
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

    -- ** Single Elements

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

    -- * Transformations
    -- Mapping on output
    -- , rmapM

    -- ** Map on input
    , lmap
    , lmapM

     -- ** Map on output
    , rmapM

    -- ** Filtering
    , filter

    -- ** Look Ahead
    , lookAhead

    -- * Tokenizing Combinators
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

    -- * Splitting
    , many
    , some
    , manyTill

    -- * De-interleaving
    , deintercalate
    )

where

import Streamly.Internal.Data.Parser
import Prelude hiding (dropWhile, takeWhile, filter)

#include "DocTestDataParser.hs"
