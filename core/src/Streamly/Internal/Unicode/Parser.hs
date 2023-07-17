-- |
-- Module      : Streamly.Internal.Unicode.Parser
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To parse a text input, use the decode routines from
-- "Streamly.Unicode.Stream" module to convert an input byte stream to a
-- Unicode Char stream and then use these parsers on the Char stream.

module Streamly.Internal.Unicode.Parser
    (
    -- * Generic
      char
    , charIgnoreCase

    -- * Sequences
    , string
    , stringIgnoreCase
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
    , printable
    , punctuation
    , separator
    , space
    , symbol

    -- digits
    , digit
    , octDigit
    , hexDigit
    , numeric

    -- * Numeric
    , signed
    , double
    , decimal
    , hexadecimal
    )
where

import Control.Applicative (Alternative(..))
import Data.Bits (Bits, (.|.), shiftL)
import Data.Char (ord)
import Streamly.Internal.Data.Parser (Parser, Initial(..),  Step(..))

import qualified Data.Char as Char
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
    (
      lmap
    , satisfy
    , listEq
    , takeWhile1
    , dropWhile
    )
import Data.Ratio ((%))
import Streamly.Internal.Data.Parser.ParserD (Parser(..))

--------------------------------------------------------------------------------
-- Character classification
--------------------------------------------------------------------------------

-- XXX It may be possible to implement faster predicates for ASCII byte stream.
-- We can measure if there is a signficant difference and if so we can add such
-- predicates to Streamly.Unicode.Parser.Latin1.
--
#define CHAR_PARSER_SIG(NAME)         NAME :: Monad m => Parser Char m Char
-- XXX Need to use the predicates from Unicode.Char module/unicode-data package
#define CHAR_PARSER(NAME, PREDICATE)  NAME = Parser.satisfy Char.PREDICATE
#define CHAR_PARSER_DOC(PREDICATE) -- | Match any character that satisfies 'Char.PREDICATE'
#define CHAR_PARSER_INLINE(NAME)      {-# INLINE NAME #-}

CHAR_PARSER_DOC(isSpace)
CHAR_PARSER_INLINE(space)
CHAR_PARSER_SIG(space)
CHAR_PARSER(space,isSpace)

CHAR_PARSER_DOC(isLower)
CHAR_PARSER_INLINE(lower)
CHAR_PARSER_SIG(lower)
CHAR_PARSER(lower,isLower)

CHAR_PARSER_DOC(isUpper)
CHAR_PARSER_INLINE(upper)
CHAR_PARSER_SIG(upper)
CHAR_PARSER(upper,isUpper)

CHAR_PARSER_DOC(isAlpha)
CHAR_PARSER_INLINE(alpha)
CHAR_PARSER_SIG(alpha)
CHAR_PARSER(alpha,isAlpha)

CHAR_PARSER_DOC(isAlphaNum)
CHAR_PARSER_INLINE(alphaNum)
CHAR_PARSER_SIG(alphaNum)
CHAR_PARSER(alphaNum,isAlphaNum)

CHAR_PARSER_DOC(isPrint)
CHAR_PARSER_INLINE(printable)
CHAR_PARSER_SIG(printable)
CHAR_PARSER(printable,isPrint)

CHAR_PARSER_DOC(isDigit)
CHAR_PARSER_INLINE(digit)
CHAR_PARSER_SIG(digit)
CHAR_PARSER(digit,isDigit)

CHAR_PARSER_DOC(isOctDigit)
CHAR_PARSER_INLINE(octDigit)
CHAR_PARSER_SIG(octDigit)
CHAR_PARSER(octDigit,isOctDigit)

CHAR_PARSER_DOC(isHexDigit)
CHAR_PARSER_INLINE(hexDigit)
CHAR_PARSER_SIG(hexDigit)
CHAR_PARSER(hexDigit,isHexDigit)

CHAR_PARSER_DOC(isLetter)
CHAR_PARSER_INLINE(letter)
CHAR_PARSER_SIG(letter)
CHAR_PARSER(letter,isLetter)

CHAR_PARSER_DOC(isMark)
CHAR_PARSER_INLINE(mark)
CHAR_PARSER_SIG(mark)
CHAR_PARSER(mark,isMark)

CHAR_PARSER_DOC(isNumber)
CHAR_PARSER_INLINE(numeric)
CHAR_PARSER_SIG(numeric)
CHAR_PARSER(numeric,isNumber)

CHAR_PARSER_DOC(isPunctuation)
CHAR_PARSER_INLINE(punctuation)
CHAR_PARSER_SIG(punctuation)
CHAR_PARSER(punctuation,isPunctuation)

CHAR_PARSER_DOC(isSymbol)
CHAR_PARSER_INLINE(symbol)
CHAR_PARSER_SIG(symbol)
CHAR_PARSER(symbol,isSymbol)

CHAR_PARSER_DOC(isSeparator)
CHAR_PARSER_INLINE(separator)
CHAR_PARSER_SIG(separator)
CHAR_PARSER(separator,isSeparator)

CHAR_PARSER_DOC(isAscii)
CHAR_PARSER_INLINE(ascii)
CHAR_PARSER_SIG(ascii)
CHAR_PARSER(ascii,isAscii)

CHAR_PARSER_DOC(isLatin1)
CHAR_PARSER_INLINE(latin1)
CHAR_PARSER_SIG(latin1)
CHAR_PARSER(latin1,isLatin1)

CHAR_PARSER_DOC(isAsciiUpper)
CHAR_PARSER_INLINE(asciiUpper)
CHAR_PARSER_SIG(asciiUpper)
CHAR_PARSER(asciiUpper,isAsciiUpper)

CHAR_PARSER_DOC(isAsciiLower)
CHAR_PARSER_INLINE(asciiLower)
CHAR_PARSER_SIG(asciiLower)
CHAR_PARSER(asciiLower,isAsciiLower)

--------------------------------------------------------------------------------
-- Character parsers
--------------------------------------------------------------------------------

-- | Match a specific character.
{-# INLINE char #-}
char :: Monad m => Char -> Parser Char m Char
char c = Parser.satisfy (== c)

-- XXX Case conversion may lead to change in number of chars
-- | Match a specific character ignoring case.
{-# INLINE charIgnoreCase #-}
charIgnoreCase :: Monad m => Char -> Parser Char m Char
charIgnoreCase c = Parser.lmap Char.toLower (Parser.satisfy (== Char.toLower c))

--------------------------------------------------------------------------------
-- Character sequences
--------------------------------------------------------------------------------

-- | Match the input with the supplied string and return it if successful.
string :: Monad m => String -> Parser Char m String
string = Parser.listEq

-- XXX Not accurate unicode case conversion
-- | Match the input with the supplied string and return it if successful.
stringIgnoreCase :: Monad m => String -> Parser Char m String
stringIgnoreCase s =
    Parser.lmap Char.toLower (Parser.listEq (map Char.toLower s))

-- | Drop /zero/ or more white space characters.
dropSpace :: Monad m => Parser Char m ()
dropSpace = Parser.dropWhile Char.isSpace

-- | Drop /one/ or more white space characters.
dropSpace1 :: Monad m => Parser Char m ()
dropSpace1 = Parser.takeWhile1 Char.isSpace Fold.drain

--------------------------------------------------------------------------------
-- Numeric parsers
--------------------------------------------------------------------------------

-- XXX It should fail if the number is larger than the size of the type.
--
-- | Parse and decode an unsigned integral decimal number.
{-# INLINE decimal #-}
decimal :: (Monad m, Integral a) => Parser Char m a
decimal = Parser.takeWhile1 Char.isDigit (Fold.foldl' step 0)

    where

    step a c = a * 10 + fromIntegral (ord c - 48)

-- | Parse and decode an unsigned integral hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- Note: This parser does not accept a leading @\"0x\"@ string.
{-# INLINE hexadecimal #-}
hexadecimal :: (Monad m, Integral a, Bits a) => Parser Char m a
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
signed :: (Num a, Monad m) => Parser Char m a -> Parser Char m a
signed p = (negate <$> (char '-' *> p)) <|> (char '+' *> p) <|> p

data TE =
    TE  {-# UNPACK #-}!Int      -- ^ Total number of digits
        {-# UNPACK #-}!Int      -- ^ Decimal point exsistence
        {-# UNPACK #-}!Int      -- ^ The sign positive or negative
        !Integer                -- ^ The coefficient of a scientific number.
        {-# UNPACK #-}!Int      -- ^ The base-10 exponent of a scientific number.

-- | Parse a 'Double'.
-- This parser accepts an optional sign (+ or -) followed by
-- at least one decimal digit.  The syntax is similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ is
-- consumed.
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".

-- >>> import qualified Streamly.Internal.Data.Stream as S
--
-- >>> doubleParser = S.parse double . S.fromList
--
-- >>> doubleParser "3"
-- Right 3.0
--
-- >>> doubleParser ".3"
-- Right 0.3
--
-- >>> doubleParser "00.3"
-- Right 0.3
--
-- >>> doubleParser "0.003"
-- Right 3.0e-3
--
-- >>> doubleParser "1000.003"
-- Right 1000.003
--
-- >>> doubleParser "+123.345"
-- Right 123.345
--
-- >>> doubleParser "-123.345"
-- Right (-123.345)
--
-- >>> doubleParser "0.0.00"
-- Right 0.0
--
-- >>> doubleParser "44.56.67"
-- Right 44.56
--
-- >>> doubleParser "44.56.67-9"
-- Right 44.56
--
-- >>> doubleParser "44-6008"
-- Right 44.0
--
-- >>> doubleParser "44+36008"
-- Right 44.0
--
-- >>> doubleParser "447u"
-- Right 447.0
--
-- >>> doubleParser "+"
-- Right 0.0
--
-- >>> doubleParser "-"
-- Right 0.0
--
-- >>> doubleParser ""
-- Right 0.0
--
-- >>> doubleParser "+."
-- Right 0.0
--
-- >>> doubleParser "-."
-- Right 0.0
--
-- >>> doubleParser "4."
-- Right 4.0
--
-- >>> doubleParser "a."
-- Left (ParseError "Invalid double fromat")
--
{-# INLINE double #-}
double :: (Monad m) => Parser Char m Double
double =  Parser step initial extract

    where

    initial = return $ IPartial (TE 0 0 0 0 0)

    calc m x = m * 10 + fromIntegral (ord x - 48)

    step (TE c p s m e) a = do
        let stp = c + 1
        return $
            case a of
                '-' | c == 0 -> Partial 0 (TE stp 0 (-1) m e)
                '+' | c == 0 -> Partial 0 (TE stp 0 0 m e)
                '.' | e == 0 -> Partial 0 (TE stp 1 s m e)
                x | Char.isDigit x ->
                        if (p > 0)      -- parsing after decimal point?
                        then Partial 0 (TE stp (p+1) s (calc m x) (e + 1))
                        else Partial 0 (TE stp p s (calc m x) e)
                _ ->
                    if (c == 0) -- No digit found
                    then Error "Invalid double fromat"
                    else do
                        case s < 0 of
                            True ->
                                if e >= 0
                                then Done 0 (fromRational (-m % 10 ^ e))
                                else Done 0 (fromRational (-m % 10 ^ (-e)))
                            False ->
                                if e >= 0
                                then Done 0 (fromRational (m % 10 ^ e))
                                else Done 0 (fromRational (m % 10 ^ (-e)))

    extract (TE _stp _p s m ex) = return $ do
            case s < 0 of
                True ->
                    if ex >= 0
                    then Done 0 (fromRational (-m % 10 ^ ex))
                    else Done 0 (fromRational (-m % 10 ^ (-ex)))
                False ->
                    if ex >= 0
                    then Done 0 (fromRational (m % 10 ^ ex))
                    else Done 0 (fromRational (m % 10 ^ (-ex)))
