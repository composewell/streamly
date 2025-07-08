{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- Parsers are more powerful but less general than 'Streamly.Data.Fold.Fold's:
--
-- * folds cannot fail but parsers can fail and backtrack.
-- * folds can be composed as a Tee but parsers cannot.
-- * folds can be converted to parsers.
--
-- Streamly parsers support all operations offered by popular Haskell parser
-- libraries. Unlike other parser libraries, (1) streamly parsers can operate
-- on any Haskell type as input - not just bytes, (2) natively support
-- streaming, (3) and are faster.
--
-- == High Performance by Static Parser Fusion
--
-- Like folds, parsers are designed to utilize stream fusion, compiling to
-- efficient low-level code comparable to the speed of C. Parsers are suitable
-- for high-performance parsing of streams.
--
-- Operations in this module are designed to be composed statically rather than
-- dynamically. They are inlined to enable static fusion. More importantly,
-- they are not designed to be used recursively. Recursive use will break
-- fusion and lead to quadratic performance slowdown. For dynamic and
-- recursive compositions use the continuation passing style (CPS) operations
-- from the "Streamly.Data.ParserK" module. 'Parser' and
-- 'Streamly.Data.ParserK.ParserK' types are interconvertible.
--
-- == How to parse a stream?
--
-- Parser combinators can be used to create a pipeline of parsers such
-- that the next parser consumes the result of the previous parser.
-- Such a composed pipeline of parsers can then be driven by one of many parser
-- drivers available in the Stream and Array modules.
--
-- Use Streamly.Data.Stream.'Streamly.Data.Stream.parse' or
-- Streamly.Data.Stream.'Streamly.Data.Stream.parseBreak' to run a parser on an
-- input stream and return the parsed result.
--
-- Use Streamly.Data.Stream.'Streamly.Data.Stream.parseMany' or
-- Streamly.Data.Stream.'Streamly.Data.Stream.parseIterate' to transform an
-- input data stream to an output stream of parsed data elements using a
-- parser.
--
-- == Parser vs ParserK
--
-- There are two functionally equivalent parsing modules,
-- "Streamly.Data.Parser" (this module) and "Streamly.Data.ParserK". The latter
-- is a CPS based wrapper over the former, and can be used for parsing in
-- general. "Streamly.Data.Parser" enables stream fusion and where possible it should be
-- preferred over "Streamly.Data.ParserK" for high performance stream parsing
-- use cases. However, there are a few cases where this module is not
-- suitable and ParserK should be used instead. As a thumb rule, when recursion
-- or heavy nesting is needed use ParserK.
--
-- === Parser: suitable for non-recursive static fusion
--
-- The 'Parser' type is suitable only for non-recursive static fusion. It could
-- be problematic for recursive definitions. To enable static fusion, parser
-- combinators use strict pattern matching on arguments of type Parser. This
-- leads to infinte loop when a parser is defined recursively, due to strict
-- evaluation of the recursive call. For example, the following implementation
-- loops infinitely because of the recursive use of parser @p@ in the @*>@
-- combinator:
--
-- >>> import Streamly.Data.Parser (Parser)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import Control.Applicative ((<|>))
--
-- >>> :{
-- >>> p, p1, p2 :: Monad m => Parser Char m String
-- >>> p1 = Parser.satisfy (== '(') *> p
-- >>> p2 = Parser.fromFold Fold.toList
-- >>> p = p1 <|> p2
-- >>> :}
--
-- Another limitation of Parser type quadratic performance slowdown when too
-- many nested compositions are used. Especially Applicative, Monad,
-- Alternative instances, and sequenced parsing operations (e.g. nested 'one',
-- and 'splitWith') exhibit quadratic slowdown (O(n^2) complexity) when
-- combined @n@ times, roughly 8 or less sequenced parsers usually work fine.
-- READ THE DOCS OF APPLICATIVE, MONAD AND ALTERNATIVE INSTANCES.
--
-- === ParserK: suitable for recursive definitions
--
-- ParserK is suitable for recursive definitions:
--
-- >>> import Streamly.Data.ParserK (ParserK)
-- >>> import Streamly.Data.StreamK (parserK)
-- >>> import qualified Streamly.Data.StreamK as StreamK
--
-- >>> :{
-- >>> p, p1, p2 :: Monad m => ParserK Char m String
-- >>> p1 = parserK (Parser.satisfy (== '(')) *> p
-- >>> p2 = parserK (Parser.fromFold Fold.toList)
-- >>> p = p1 <|> p2
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
-- == Parsers Galore!
--
-- Streamly provides all the parsing functionality provided by popular parsing
-- libraries, and much more with higher performance.
-- This module provides most of the elementary parsers and parser combinators.
-- Additionally,
--
-- * all the folds from the "Streamly.Data.Fold" module can be converted to
-- parsers using 'fromFold'.
-- * "Streamly.Unicode.Parser" module provides Char stream parsers.
-- * all the combinators from the
-- <https://hackage.haskell.org/package/parser-combinators parser-combinators>
-- package can be used with streamly ParserK.
-- * See "Streamly.Internal.Data.Parser" for many more unreleased but useful APIs.
--
-- == Generic Parser Combinators
--
-- With 'Streamly.Data.ParserK.ParserK' you can use the 'Applicative' and
-- 'Control.Applicative.Alternative' type class based generic parser
-- combinators from the
-- <https://hackage.haskell.org/package/parser-combinators parser-combinators>
-- library or similar. However, if available, we recommend that you use the
-- equivalent functionality from this module where performance and streaming
-- behavior matters.
-- Firstly, the combinators in this module are faster due to stream fusion.
-- Secondly, these are streaming in nature as the results can be passed
-- directly to other stream consumers (folds or parsers). The Alternative type
-- class based parsers would end up buffering all the results in lists before
-- they can be consumed.
--
-- == Error Reporting
--
-- There are two types of parser drivers available, @parse@ and @parseBreak@
-- drivers do not track stream position, whereas @parsePos@ and @parseBreakPos@
-- drivers track and report stream position information with slightly more
-- performance overhead.
--
-- When an error occurs the stream position is reported, in case of byte streams
-- or unboxed array streams this is the byte position, in case of generic
-- element parsers or generic array parsers this is the element position in the
-- stream.
--
-- These parsers do not report a case specific error context (e.g. line number
-- or column). If you need line number or column information you can read the
-- stream again (if it is immutable) and this count the lines to translate the
-- reported byte position to line number and column. More elaborate support for
-- building arbitrary and custom error context information is planned to be
-- added in future.
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
    , ParseError(..)
    , ParseErrorPos(..)

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
