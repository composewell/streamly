{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.ParserK
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- See the general notes about parsing in the "Streamly.Data.Parser" module.
-- This module implements a using Continuation Passing Style (CPS) wrapper over
-- the "Streamly.Data.Parser" module. It is as fast or faster than attoparsec.
--
-- Streamly parsers support all operations offered by popular Haskell parser
-- libraries. They operate on a generic input type, support streaming, and are
-- faster.
--
-- The 'ParserK' type represents a stream-consumer as a composition of function
-- calls, therefore, a function call overhead is incurred at each composition.
-- It is reasonably fast in general but may be a few times slower than a fused
-- parser represented by the 'Streamly.Data.Parser.Parser' type. However, it
-- allows for scalable dynamic composition, especially, 'ParserK' can be used
-- in recursive calls. Operations like 'splitWith' on 'ParserK' type have
-- linear (O(n)) performance with respect to the number of compositions.
--
-- 'ParserK' is preferred over 'Streamly.Data.Parser.Parser' when extensive
-- applicative, alternative and monadic composition is required, or when
-- recursive or dynamic composition of parsers is required. 'ParserK' also
-- allows efficient parsing of a stream of arrays, it can also break the input
-- stream into a parse result and remaining stream so that the stream can be
-- parsed independently in segments.
--
-- == Using ParserK
--
-- All the parsers from the "Streamly.Data.Parser" module can be converted to
-- ParserK using the 'Streamly.Data.Array.parserK',
-- 'Streamly.Internal.Data.ParserK.parserK', and
-- 'Streamly.Internal.Data.Array.Generic.parserK' combinators.
--
-- 'Streamly.Data.Array.parse' runs a parser on a stream of unboxed
-- arrays, this is the preferred and most efficient way to parse chunked input.
-- The more general 'Streamly.Data.Array.parseBreak' function returns
-- the remaining stream as well along with the parse result. There are
-- 'Streamly.Internal.Data.Array.Generic.parse',
-- 'Streamly.Internal.Data.Array.Generic.parseBreak' as well to run
-- parsers on boxed arrays. 'Streamly.Internal.Data.StreamK.parse',
-- 'Streamly.Internal.Data.StreamK.parseBreak' run parsers on a stream of
-- individual elements instead of stream of arrays.
--
-- == Monadic Composition
--
-- Monad composition can be used for lookbehind parsers, we can dynamically
-- compose new parsers based on the results of the previously parsed values.
--
-- If we have to parse "a9" or "9a" but not "99" or "aa" we can use the
-- following non-monadic, backtracking parser:
--
-- >>> digits p1 p2 = ((:) <$> p1 <*> ((:) <$> p2 <*> pure []))
-- >>> :{
-- backtracking :: Monad m => ParserK Char m String
-- backtracking = ParserK.parserK $
--     digits (Parser.satisfy isDigit) (Parser.satisfy isAlpha)
--     <|>
--     digits (Parser.satisfy isAlpha) (Parser.satisfy isDigit)
-- :}
--
-- We know that if the first parse resulted in a digit at the first place then
-- the second parse is going to fail.  However, we waste that information and
-- parse the first character again in the second parse only to know that it is
-- not an alphabetic char.  By using lookbehind in a 'Monad' composition we can
-- avoid redundant work:
--
-- >>> data DigitOrAlpha = Digit Char | Alpha Char
--
-- >>> :{
-- lookbehind :: Monad m => ParserK Char m String
-- lookbehind = do
--     x1 <- ParserK.parserK $
--              Digit <$> Parser.satisfy isDigit
--          <|> Alpha <$> Parser.satisfy isAlpha
--     -- Note: the parse depends on what we parsed already
--     x2 <- ParserK.parserK $
--           case x1 of
--              Digit _ -> Parser.satisfy isAlpha
--              Alpha _ -> Parser.satisfy isDigit
--     return $ case x1 of
--         Digit x -> [x,x2]
--         Alpha x -> [x,x2]
-- :}
--
-- == Experimental APIs
--
-- Please refer to "Streamly.Internal.Data.ParserK" for functions that have
-- not yet been released.
--
module Streamly.Data.ParserK
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Parser Type
      ParserK

    -- * Parsers
    -- ** Conversions
    , parserK
    -- , toParser

    -- ** Without Input
    , fromPure
    , fromEffect
    , die

    -- * Deprecated
    , fromFold
    , fromParser
    , adapt
    , adaptC
    , adaptCG
    )

where

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.Array (Array)
import qualified Streamly.Internal.Data.Parser as ParserD
import qualified Streamly.Internal.Data.Array as Array

import Streamly.Internal.Data.ParserK

#include "DocTestDataParserK.hs"

{-# DEPRECATED fromFold "Please use \"Array.parserK . Parser.fromFold\" instead." #-}
{-# INLINE fromFold #-}
fromFold :: (MonadIO m, Unbox a) => Fold m a b -> ParserK (Array a) m b
fromFold = Array.parserK . ParserD.fromFold

{-# DEPRECATED fromParser "Please use \"Array.parserK\" instead." #-}
{-# INLINE fromParser #-}
fromParser ::
       (MonadIO m, Unbox a) => ParserD.Parser a m b -> ParserK (Array a) m b
fromParser = Array.parserK
