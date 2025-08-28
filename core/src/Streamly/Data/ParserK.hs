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
-- This (ParserK) module implements a Continuation Passing Style (CPS) wrapper
-- over the fused "Streamly.Data.Parser" module. It is a faster CPS parser than
-- attoparsec.
--
-- The 'ParserK' type represents a stream-consumer as a composition of function
-- calls, therefore, a function call overhead is incurred at each composition.
-- It is reasonably fast in general but may be a few times slower than the
-- fused 'Streamly.Data.Parser.Parser' type. However, unlike fused parsers, it
-- allows for scalable dynamic composition, especially, 'ParserK' can be used
-- in recursive calls. Operations like 'splitWith' on 'ParserK' type have
-- linear (O(n)) performance with respect to the number of compositions.
--
-- 'ParserK' is preferred over the fused 'Streamly.Data.Parser.Parser' when
-- extensive applicative, alternative and monadic composition is required, or
-- when recursive or dynamic composition of parsers is required. 'ParserK' also
-- allows efficient parsing of a stream of byte arrays, it can also break the
-- input stream into a parse result and the remaining stream so that the stream
-- can be parsed independently in segments.
--
-- == How to parse a stream?
--
-- All the fused parsers from the "Streamly.Data.Parser" module can be
-- converted to the CPS ParserK, for use with different types of parser
-- drivers, using
-- the @toParserK@ combinators -
-- Streamly.Data.Array.'Streamly.Data.Array.toParserK',
-- Streamly.Data.StreamK.'Streamly.Data.StreamK.toParserK', and
-- Streamly.Data.Array.Generic.'Streamly.Data.Array.Generic.toParserK'
--
-- To parse a stream of unboxed arrays, use
-- Streamly.Data.Array.'Streamly.Data.Array.parse' for running the parser, this
-- is the preferred and most efficient way to parse chunked input. The
-- Streamly.Data.Array.'Streamly.Data.Array.parseBreak' function returns the
-- remaining stream as well along with the parse result.
--
-- To parse a stream of boxed arrays, use
-- Streamly.Data.Array.Generic.'Streamly.Data.Array.Generic.parse' or
-- Streamly.Data.Array.Generic.'Streamly.Data.Array.Generic.parseBreak' to run
-- the parser.
--
-- To parse a stream of individual elements, use
-- Streamly.Data.StreamK.'Streamly.Data.StreamK.parse' and
-- Streamly.Data.StreamK.'Streamly.Data.StreamK.parseBreak' to run the parser.
--
-- == Applicative  Composition
--
-- Applicative parsers are simpler but we cannot use lookbehind as we can in
-- the monadic parsers.
--
-- If we have to parse "9a" or "a9" but not "99" or "aa" we can use the
-- following Applicative, backtracking parser:
--
-- >>> -- parse p1 : p2 : []
-- >>> token p1 p2 = ((:) <$> p1 <*> ((:) <$> p2 <*> pure []))
-- >>> :{
-- backtracking :: Monad m => ParserK Char m String
-- backtracking = StreamK.toParserK $
--     token (Parser.satisfy isDigit) (Parser.satisfy isAlpha) -- e.g. "9a"
--     <|>
--     token (Parser.satisfy isAlpha) (Parser.satisfy isDigit) -- e.g. "a9"
-- :}
--
-- == Monadic Composition
--
-- Monad composition can be used to implement lookbehind parsers, we can dynamically
-- compose new parsers based on the results of the previously parsed values.
--
-- In the previous example, we know that if the first parse resulted in a digit
-- at the first place then the second parse is going to fail.  However, we
-- waste that information and parse the first character again in the second
-- parse only to know that it is not an alphabetic char.  By using lookbehind
-- in a 'Monad' composition we can make dynamic decisions based on previously
-- parsed information and avoid redundant work:
--
-- >>> data DigitOrAlpha = Digit Char | Alpha Char
--
-- >>> :{
-- lookbehind :: Monad m => ParserK Char m String
-- lookbehind = do
--     x1 <- StreamK.toParserK $
--              Digit <$> Parser.satisfy isDigit
--          <|> Alpha <$> Parser.satisfy isAlpha
--     -- Note: the parse depends on what we parsed already
--     x2 <- StreamK.toParserK $
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

    -- -- ** Without Input
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

{-# DEPRECATED fromFold "Please use \"Array.toParserK . Parser.fromFold\" instead." #-}
{-# INLINE fromFold #-}
fromFold :: (MonadIO m, Unbox a) => Fold m a b -> ParserK (Array a) m b
fromFold = Array.toParserK . ParserD.fromFold

{-# DEPRECATED fromParser "Please use \"Array.toParserK\" instead." #-}
{-# INLINE fromParser #-}
fromParser ::
       (MonadIO m, Unbox a) => ParserD.Parser a m b -> ParserK (Array a) m b
fromParser = Array.toParserK
