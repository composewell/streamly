-- |
-- Module      : Streamly.Internal.Unicode.Char.Parser
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To parse a text input, use the decode routines from
-- "Streamly.Unicode.Stream" module to convert an input byte stream to a
-- Unicode Char stream and then use these parsers on the Char stream.

module Streamly.Internal.Unicode.Char.Parser
    (
    -- * Generic
      char
    , charAnyCase

    -- * Sequences
    , string
    , stringAnyCase
    , dropSpace
    , dropSpace1

    -- * Classes
    , alpha
    , alphaNum
    , letter
    , ascii
    , asciiLower
    , asciiUpper
    , latin1
    , lower
    , upper
    , mark
    , print
    , punctuation
    , separator
    , space
    , symbol

    -- digits
    , digit
    , octDigit
    , hexDigit
    , number

    -- * Numeric
    , signed
    , double
    , decimal
    , hexadecimal
    )
where

import Control.Applicative (Alternative(..))
import Control.Monad.Catch (MonadCatch)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Char (ord)
import Prelude hiding (print)
import Streamly.Internal.Data.Parser (Parser)

import qualified Data.Char as Char
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Fold as Fold

--------------------------------------------------------------------------------
-- Character classification
--------------------------------------------------------------------------------

-- XXX It may be possible to implement faster predicates for ASCII byte stream.
-- We can measure if there is a signficant difference and if so we can add such
-- predicates to Streamly.Unicode.Char.Parser.Latin1.
--
#define CHAR_PARSER_SIG(NAME)         NAME :: MonadCatch m => Parser m Char Char
-- XXX Need to use the predicates from Unicode.Char module/unicode-data package
#define CHAR_PARSER(NAME, PREDICATE)  NAME = Parser.satisfy Char.PREDICATE

-- XXX Add haddock documentation
-- XXX Add INLINE pragmas

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

--------------------------------------------------------------------------------
-- Character parsers
--------------------------------------------------------------------------------

-- | Match a specific character.
{-# INLINE char #-}
char :: MonadCatch m => Char -> Parser m Char Char
char c = Parser.satisfy (== c)

-- XXX Case conversion may lead to change in number of chars
-- | Match a specific character ignoring case.
{-# INLINE charAnyCase #-}
charAnyCase :: MonadCatch m => Char -> Parser m Char Char
charAnyCase c = Parser.lmap Char.toLower (Parser.satisfy (== Char.toLower c))

--------------------------------------------------------------------------------
-- Character sequences
--------------------------------------------------------------------------------

-- | Match the input with the supplied string and return it if successful.
string :: MonadCatch m => String -> Parser m Char String
string = Parser.list

-- XXX Not accurate unicode case conversion
-- | Match the input with the supplied string and return it if successful.
stringAnyCase :: MonadCatch m => String -> Parser m Char String
stringAnyCase s = Parser.lmap Char.toLower (Parser.list (map Char.toLower s))

-- | Drop /zero/ or more white space characters.
dropSpace :: MonadCatch m => Parser m Char ()
dropSpace = Parser.dropWhile Char.isSpace

-- | Drop /one/ or more white space characters.
dropSpace1 :: MonadCatch m => Parser m Char ()
dropSpace1 = Parser.takeWhile1 Char.isSpace Fold.drain

--------------------------------------------------------------------------------
-- Numeric parsers
--------------------------------------------------------------------------------

-- XXX It should fail if the number is larger than the size of the type.
--
-- | Parse and decode an unsigned integral decimal number.
{-# INLINE decimal #-}
decimal :: (MonadCatch m, Integral a) => Parser m Char a
decimal = Parser.takeWhile1 Char.isDigit (Fold.foldl' step 0)

    where

    step a c = a * 10 + fromIntegral (ord c - 48)

-- | Parse and decode an unsigned integral hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- Note: This parser does not accept a leading @\"0x\"@ string.
{-# INLINE hexadecimal #-}
hexadecimal :: (MonadCatch m, Integral a, Bits a) => Parser m Char a
hexadecimal = Parser.takeWhile1 isHexDigit (Fold.foldl' step 0)

    where

    isHexDigit c =
           (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F')

    step a c
        | w >= 48 && w <= 57 =
            (a `shiftL` 4) .|. fromIntegral (w - 48)
        | w >= 97 =
            (a `shiftL` 4) .|. fromIntegral (w - 87)
        | otherwise =
            (a `shiftL` 4) .|. fromIntegral (w - 55)

        where

        w = ord c

-- | Allow an optional leading @\'+\'@ or @\'-\'@ sign character before any
-- parser.
{-# INLINE signed #-}
signed :: (Num a, MonadCatch m) => Parser m Char a -> Parser m Char a
signed p = (negate <$> (char '-' *> p)) <|> (char '+' *> p) <|> p

-- | Parse a 'Double'.
--
-- This parser accepts an optional leading sign character, followed by
-- at most one decimal digit.  The syntax is similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ is
-- consumed.
--
-- === Examples
--
-- Examples with behaviour identical to 'read', if you feed an empty
-- continuation to the first result:
--
-- > IS.parse double (IS.fromList "3")     == 3.0
-- > IS.parse double (IS.fromList "3.1")   == 3.1
-- > IS.parse double (IS.fromList "3e4")   == 30000.0
-- > IS.parse double (IS.fromList "3.1e4") == 31000.0
-- > IS.parse double (IS.fromList "3e")    == 30
--
-- Examples with behaviour identical to 'read':
--
-- > IS.parse (IS.fromList ".3")    == error "Parse failed"
-- > IS.parse (IS.fromList "e3")    == error "Parse failed"
--
-- Example of difference from 'read':
--
-- > IS.parse double (IS.fromList "3.foo") == 3.0
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
--
-- /Unimplemented/
double :: Parser m Char Double
double = undefined
