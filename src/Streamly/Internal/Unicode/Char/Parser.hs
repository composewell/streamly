{-# LANGUAGE CPP #-}

-- |
-- Module      : Streamly.Internal.Unicode.Char.Parser
-- Copyright   : (c) 2021 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Char.Parser where

import Control.Monad.Catch (MonadCatch)
import Streamly.Internal.Data.Parser (Parser)

import qualified Data.Char as Char
import qualified Streamly.Internal.Data.Parser as Parser

--------------------------------------------------------------------------------
-- Character classification
--------------------------------------------------------------------------------

#define CHAR_PARSER_SIG(NAME)         NAME :: MonadCatch m => Parser m Char Char
#define CHAR_PARSER(NAME, PREDICATE)  NAME = Parser.satisfy Char.PREDICATE

CHAR_PARSER_SIG(space)
CHAR_PARSER(space,isSpace)

CHAR_PARSER_SIG(lower)
CHAR_PARSER(lower,isLower)

CHAR_PARSER_SIG(upper)
CHAR_PARSER(upper,isUpper)

CHAR_PARSER_SIG(alpha)
CHAR_PARSER(alpha,isAlpha)

CHAR_PARSER_SIG(alphaNum)
CHAR_PARSER(alphaNum,isAlphaNum)

CHAR_PARSER_SIG(print)
CHAR_PARSER(print,isPrint)

CHAR_PARSER_SIG(digit)
CHAR_PARSER(digit,isDigit)

CHAR_PARSER_SIG(octDigit)
CHAR_PARSER(octDigit,isOctDigit)

CHAR_PARSER_SIG(hexDigit)
CHAR_PARSER(hexDigit,isHexDigit)

CHAR_PARSER_SIG(letter)
CHAR_PARSER(letter,isLetter)

CHAR_PARSER_SIG(mark)
CHAR_PARSER(mark,isMark)

CHAR_PARSER_SIG(number)
CHAR_PARSER(number,isNumber)

CHAR_PARSER_SIG(punctuation)
CHAR_PARSER(punctuation,isPunctuation)

CHAR_PARSER_SIG(symbol)
CHAR_PARSER(symbol,isSymbol)

CHAR_PARSER_SIG(separator)
CHAR_PARSER(separator,isSeparator)

CHAR_PARSER_SIG(ascii)
CHAR_PARSER(ascii,isAscii)

CHAR_PARSER_SIG(latin1)
CHAR_PARSER(latin1,isLatin1)

CHAR_PARSER_SIG(asciiUpper)
CHAR_PARSER(asciiUpper,isAsciiUpper)

CHAR_PARSER_SIG(asciiLower)
CHAR_PARSER(asciiLower,isAsciiLower)
