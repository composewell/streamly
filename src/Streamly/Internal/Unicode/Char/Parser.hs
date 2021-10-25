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

import Control.Applicative (Alternative(..))
import Control.Monad.Catch (MonadCatch)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Char (ord)
import Data.Scientific (Scientific)
import Streamly.Internal.Data.Parser (Parser)

import qualified Data.Char as Char
import qualified Data.Scientific as Scientific
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Fold as Fold

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

--------------------------------------------------------------------------------
-- Character parsers
--------------------------------------------------------------------------------

-- | Match a specific character.
char :: MonadCatch m => Char -> Parser m Char Char
char c = Parser.satisfy (== c)

--------------------------------------------------------------------------------
-- Numeric parsers
--------------------------------------------------------------------------------

-- | Parse and decode an unsigned decimal number.
{-# INLINE decimal #-}
decimal :: (MonadCatch m, Integral a) => Parser m Char a
decimal = Parser.takeWhile1 Char.isDigit (Fold.foldl' step 0)

    where

    step a c = a * 10 + fromIntegral (ord c - 48)


-- | Parse and decode an unsigned hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string.
{-# INLINE hexadecimal #-}
hexadecimal :: (MonadCatch m, Integral a, Bits a) => Parser m Char a
hexadecimal = Parser.takeWhile1 isHexDigit (Fold.foldl' step 0)

    where

    isHexDigit c =
        (c >= '0' && c <= '9')
            || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    step a c
        | w >= 48 && w <= 57 = (a `shiftL` 4) .|. fromIntegral (w - 48)
        | w >= 97 = (a `shiftL` 4) .|. fromIntegral (w - 87)
        | otherwise = (a `shiftL` 4) .|. fromIntegral (w - 55)

        where

        w = ord c

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
{-# INLINE signed #-}
signed :: (Num a, MonadCatch m) => Parser m Char a -> Parser m Char a
signed p = (negate <$> (char '-' *> p)) <|> (char '+' *> p) <|> p

-- XXX Use Tuple'?
-- A strict pair
data SP =
    SP !Integer {-# UNPACK #-}!Int

{-# INLINE scientifically #-}
scientifically :: MonadCatch m => (Scientific -> a) -> Parser m Char a
scientifically h = do
    !positive <-
        ((== '+') <$> Parser.satisfy (\c -> c == '-' || c == '+')) <|> pure True
    n <- decimal
    let fracFold =
            Fold.teeWith
                (\a b -> SP a (negate b))
                (Fold.foldl' step n)
                (Fold.length)
        step a c = a * 10 + fromIntegral (ord c - 48)
    SP c e <-
        (Parser.satisfy (== '.') *> (Parser.takeWhile Char.isDigit fracFold))
            <|> pure (SP n 0)
    let !signedCoeff
            | positive = c
            | otherwise = -c
    (Parser.satisfy (\w -> w == 'e' || w == 'E')
         *> fmap
               (h . Scientific.scientific signedCoeff . (e +))
               (signed decimal))
        <|> return (h $ Scientific.scientific signedCoeff e)


-- | Parse a 'Double'.
--
-- This parser accepts an optional leading sign character, followed by
-- at most one decimal digit.  The syntax is similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ is
-- consumed.
--
-- === Examples
--
-- These examples use this helper:
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
double :: MonadCatch m => Parser m Char Double
double = scientifically Scientific.toRealFloat

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double' or 'scientific'
-- instead.
rational :: (MonadCatch m, Fractional f) => Parser m Char f
rational = scientifically realToFrac

-- | Parse a scientific number.
--
-- The syntax accepted by this parser is the same as for 'double'.
scientific :: MonadCatch m => Parser m Char Scientific
scientific = scientifically id
