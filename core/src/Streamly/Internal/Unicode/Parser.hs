{-# LANGUAGE CPP #-}
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
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

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
    , number
    , doubleParser
    , double
    , decimal
    , hexadecimal

    -- * Utilities
    , mkDouble
    )
where

import Control.Applicative (Alternative(..))
import Data.Bits (Bits, (.|.), shiftL, (.&.))
import Data.Char (ord)
import Data.Ratio ((%))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Parser (Parser(..), Initial(..), Step(..), Final(..))

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

#include "DocTestUnicodeParser.hs"

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

-- XXX Change Multiplier to Sign
type Multiplier = Int

-- XXX We can use Int instead of Integer to make it twice as fast. But then we
-- will have to truncate the significant digits before overflow occurs.
type Number = Integer
type DecimalPlaces = Int
type PowerMultiplier = Int
type Power = Int

{-# ANN type ScientificParseState Fuse #-}
data ScientificParseState
  = SPInitial
  | SPSign !Multiplier
  | SPAfterSign !Multiplier !Number
  | SPDot !Multiplier !Number
  | SPAfterDot !Multiplier !Number !DecimalPlaces
  | SPExponent !Multiplier !Number !DecimalPlaces
  | SPExponentWithSign !Multiplier !Number !DecimalPlaces !PowerMultiplier
  | SPAfterExponent !Multiplier !Number !DecimalPlaces !PowerMultiplier !Power

-- XXX See https://hackage.haskell.org/package/integer-conversion for large
-- integers.

-- | A generic parser for scientific notation of numbers. Returns (mantissa,
-- exponent) tuple. The result can be mapped to 'Double' or any other number
-- representation e.g. @Scientific@.
--
-- For example, using the @scientific@ package:
-- >> parserScientific = uncurry Data.Scientific.scientific <$> 'number'
{-# INLINE number #-}
number :: Monad m => Parser Char m (Integer, Int)
number =  Parser (\s a -> return $ step s a) initial (return . extract)

    where

    intToInteger :: Int -> Integer
    intToInteger = fromIntegral

    combineNum buf num = buf * 10 + num

    {-# INLINE initial #-}
    initial = pure $ IPartial SPInitial

    exitSPInitial msg =
        "number: expecting sign or decimal digit, got " ++ msg
    exitSPSign msg =
        "number: expecting decimal digit, got " ++ msg
    exitSPAfterSign multiplier num = (intToInteger multiplier * num, 0)
    exitSPAfterDot multiplier num decimalPlaces =
        ( intToInteger multiplier * num
        , -decimalPlaces
        )
    exitSPAfterExponent mult num decimalPlaces powerMult powerNum =
        let e = powerMult * powerNum - decimalPlaces
         in (intToInteger mult * num, e)

    {-# INLINE step #-}
    step SPInitial val =
        case val of
          '+' -> SContinue 1 (SPSign 1)
          '-' -> SContinue 1 (SPSign (-1))
          _ -> do
              let num = ord val - 48
              if num >= 0 && num <= 9
              then SPartial 1 $ SPAfterSign 1 (intToInteger num)
              else SError $ exitSPInitial $ show val
    step (SPSign multiplier) val =
        let num = ord val - 48
         in if num >= 0 && num <= 9
            then SPartial 1 $ SPAfterSign multiplier (intToInteger num)
            else SError $ exitSPSign $ show val
    step (SPAfterSign multiplier buf) val =
        case val of
            '.' -> SContinue 1 $ SPDot multiplier buf
            'e' -> SContinue 1 $ SPExponent multiplier buf 0
            'E' -> SContinue 1 $ SPExponent multiplier buf 0
            _ ->
                let num = ord val - 48
                 in if num >= 0 && num <= 9
                    then
                        SPartial 1
                            $ SPAfterSign multiplier (combineNum buf (intToInteger num))
                    else SDone 0 $ exitSPAfterSign multiplier buf
    step (SPDot multiplier buf) val =
        let num = ord val - 48
         in if num >= 0 && num <= 9
            then SPartial 1 $ SPAfterDot multiplier (combineNum buf (intToInteger num)) 1
            else SDone (-1) $ exitSPAfterSign multiplier buf
    step (SPAfterDot multiplier buf decimalPlaces) val =
        case val of
            'e' -> SContinue 1 $ SPExponent multiplier buf decimalPlaces
            'E' -> SContinue 1 $ SPExponent multiplier buf decimalPlaces
            _ ->
                let num = ord val - 48
                 in if num >= 0 && num <= 9
                    then
                        SPartial 1
                            $ SPAfterDot
                                  multiplier
                                  (combineNum buf (intToInteger num))
                                  (decimalPlaces + 1)
                    else SDone 0 $ exitSPAfterDot multiplier buf decimalPlaces
    step (SPExponent multiplier buf decimalPlaces) val =
        case val of
          '+' -> SContinue 1 (SPExponentWithSign multiplier buf decimalPlaces 1)
          '-' -> SContinue 1 (SPExponentWithSign multiplier buf decimalPlaces (-1))
          _ -> do
              let num = ord val - 48
              if num >= 0 && num <= 9
              then SPartial 1 $ SPAfterExponent multiplier buf decimalPlaces 1 num
              else SDone (-1) $ exitSPAfterDot multiplier buf decimalPlaces
    step (SPExponentWithSign mult buf decimalPlaces powerMult) val =
        let num = ord val - 48
         in if num >= 0 && num <= 9
            then SPartial 1 $ SPAfterExponent mult buf decimalPlaces powerMult num
            else SDone (-2) $ exitSPAfterDot mult buf decimalPlaces
    step (SPAfterExponent mult num decimalPlaces powerMult buf) val =
        let n = ord val - 48
         in if n >= 0 && n <= 9
            then
                SPartial 1
                    $ SPAfterExponent
                          mult num decimalPlaces powerMult (combineNum buf n)
            else
                SDone 0
                    $ exitSPAfterExponent mult num decimalPlaces powerMult buf

    {-# INLINE extract #-}
    extract SPInitial = FError $ exitSPInitial "end of input"
    extract (SPSign _) = FError $ exitSPSign "end of input"
    extract (SPAfterSign mult num) = FDone 0 $ exitSPAfterSign mult num
    extract (SPDot mult num) = FDone (-1) $ exitSPAfterSign mult num
    extract (SPAfterDot mult num decimalPlaces) =
        FDone 0 $ exitSPAfterDot mult num decimalPlaces
    extract (SPExponent mult num decimalPlaces) =
        FDone (-1) $ exitSPAfterDot mult num decimalPlaces
    extract (SPExponentWithSign mult num decimalPlaces _) =
        FDone (-2) $ exitSPAfterDot mult num decimalPlaces
    extract (SPAfterExponent mult num decimalPlaces powerMult powerNum) =
        FDone 0 $ exitSPAfterExponent mult num decimalPlaces powerMult powerNum

type MantissaInt = Int
type OverflowPower = Int

{-# ANN type DoubleParseState Fuse #-}
data DoubleParseState
  = DPInitial
  | DPSign !Multiplier
  | DPAfterSign !Multiplier !MantissaInt !OverflowPower
  | DPDot !Multiplier !MantissaInt !OverflowPower
  | DPAfterDot !Multiplier !MantissaInt !OverflowPower
  | DPExponent !Multiplier !MantissaInt !OverflowPower
  | DPExponentWithSign !Multiplier !MantissaInt !OverflowPower !PowerMultiplier
  | DPAfterExponent !Multiplier !MantissaInt !OverflowPower !PowerMultiplier !Power

-- | A fast, custom parser for double precision flaoting point numbers. Returns
-- (mantissa, exponent) tuple. This is much faster than 'number' because it
-- assumes the number will fit in a 'Double' type and uses 'Int' representation
-- to store mantissa.
--
-- Number larger than 'Double' may overflow. Int overflow is not checked in the
-- exponent.
--
{-# INLINE doubleParser #-}
doubleParser :: Monad m => Parser Char m (Int, Int)
doubleParser =  Parser (\s a -> return $ step s a) initial (return . extract)

    where

    -- XXX Assuming Int = Int64

    -- Up to 58 bits Int won't overflow
    -- ghci> (2^59-1)*10+9 :: Int
    -- 5764607523034234879
    mask :: Word
    mask = 0x7c00000000000000 -- 58 bits, ignore the sign bit

    {-# INLINE combineNum #-}
    combineNum :: Int -> Int -> Int -> (Int, Int)
    combineNum mantissa power num =
         if fromIntegral mantissa .&. mask == 0
         then (mantissa * 10 + num, power)
         else (mantissa, power + 1)

    {-# INLINE initial #-}
    initial = pure $ IPartial DPInitial

    exitDPInitial msg =
        "number: expecting sign or decimal digit, got " ++ msg
    exitDPSign msg =
        "number: expecting decimal digit, got " ++ msg
    exitDPAfterSign multiplier num opower = (fromIntegral multiplier * num, opower)
    exitDPAfterDot multiplier num opow =
        (fromIntegral multiplier * num , opow)
    exitDPAfterExponent mult num opow powerMult powerNum =
        (fromIntegral mult * num, opow + powerMult * powerNum)

    {-# INLINE step #-}
    step DPInitial val =
        case val of
          '+' -> SContinue 1 (DPSign 1)
          '-' -> SContinue 1 (DPSign (-1))
          _ -> do
              let num = ord val - 48
              if num >= 0 && num <= 9
              then SPartial 1 $ DPAfterSign 1 num 0
              else SError $ exitDPInitial $ show val
    step (DPSign multiplier) val =
        let num = ord val - 48
         in if num >= 0 && num <= 9
            then SPartial 1 $ DPAfterSign multiplier num 0
            else SError $ exitDPSign $ show val
    step (DPAfterSign multiplier buf opower) val =
        case val of
            '.' -> SContinue 1 $ DPDot multiplier buf opower
            'e' -> SContinue 1 $ DPExponent multiplier buf opower
            'E' -> SContinue 1 $ DPExponent multiplier buf opower
            _ ->
                let num = ord val - 48
                 in if num >= 0 && num <= 9
                    then
                        let (buf1, power1) = combineNum buf opower num
                         in SPartial 1
                            $ DPAfterSign multiplier buf1 power1
                    else SDone 0 $ exitDPAfterSign multiplier buf opower
    step (DPDot multiplier buf opower) val =
        let num = ord val - 48
         in if num >= 0 && num <= 9
            then
                let (buf1, power1) = combineNum buf opower num
                 in SPartial 1 $ DPAfterDot multiplier buf1 (power1 - 1)
            else SDone (-1) $ exitDPAfterSign multiplier buf opower
    step (DPAfterDot multiplier buf opower) val =
        case val of
            'e' -> SContinue 1 $ DPExponent multiplier buf opower
            'E' -> SContinue 1 $ DPExponent multiplier buf opower
            _ ->
                let num = ord val - 48
                 in if num >= 0 && num <= 9
                    then
                        let (buf1, power1) = combineNum buf opower num
                         in SPartial 1 $ DPAfterDot multiplier buf1 (power1 - 1)
                    else SDone 0 $ exitDPAfterDot multiplier buf opower
    step (DPExponent multiplier buf opower) val =
        case val of
          '+' -> SContinue 1 (DPExponentWithSign multiplier buf opower 1)
          '-' -> SContinue 1 (DPExponentWithSign multiplier buf opower (-1))
          _ -> do
              let num = ord val - 48
              if num >= 0 && num <= 9
              then SPartial 1 $ DPAfterExponent multiplier buf opower 1 num
              else SDone (-1) $ exitDPAfterDot multiplier buf opower
    step (DPExponentWithSign mult buf opower powerMult) val =
        let num = ord val - 48
         in if num >= 0 && num <= 9
            then SPartial 1 $ DPAfterExponent mult buf opower powerMult num
            else SDone (-2) $ exitDPAfterDot mult buf opower
    step (DPAfterExponent mult num opower powerMult buf) val =
        let n = ord val - 48
         in if n >= 0 && n <= 9
            then
                SPartial 1
                    $ DPAfterExponent mult num opower powerMult (buf * 10 + n)
            else SDone 0 $ exitDPAfterExponent mult num opower powerMult buf

    {-# INLINE extract #-}
    extract DPInitial = FError $ exitDPInitial "end of input"
    extract (DPSign _) = FError $ exitDPSign "end of input"
    extract (DPAfterSign mult num opow) = FDone 0 $ exitDPAfterSign mult num opow
    extract (DPDot mult num opow) = FDone (-1) $ exitDPAfterSign mult num opow
    extract (DPAfterDot mult num opow) =
        FDone 0 $ exitDPAfterDot mult num opow
    extract (DPExponent mult num opow) =
        FDone (-1) $ exitDPAfterDot mult num opow
    extract (DPExponentWithSign mult num opow _) =
        FDone (-2) $ exitDPAfterDot mult num opow
    extract (DPAfterExponent mult num opow powerMult powerNum) =
        FDone 0 $ exitDPAfterExponent mult num opow powerMult powerNum

-- XXX We can have a `realFloat` parser instead to parse any RealFloat value.
-- And a integral parser to read any integral value.

-- XXX This is very expensive, takes much more time than the rest of the
-- parsing. Need to look into fromRational.

-- | @mkDouble mantissa exponent@ converts a mantissa and exponent to a
-- 'Double' value equivalent to @mantissa * 10^exponent@. It does not check for
-- overflow, powers more than 308 will overflow.
{-# INLINE mkDouble #-}
mkDouble :: Integer -> Int -> Double
mkDouble mantissa power =
    if power > 0
    then fromRational ((mantissa * 10 ^ power) % 1)
    else fromRational (mantissa % 10 ^ (-power))

-- | Parse a decimal 'Double' value. This parser accepts an optional sign (+ or
-- -) followed by at least one decimal digit. Decimal digits are optionally
-- followed by a decimal point and at least one decimal digit after the point.
-- This parser accepts the maximal valid input as long as it gives a valid
-- number. Specifcally a trailing decimal point is allowed but not consumed.
-- This function does not accept \"NaN\" or \"Infinity\" string representations
-- of double values.
--
-- Definition:
--
-- >>> double = uncurry Unicode.mkDouble <$> Unicode.number
--
-- Examples:
--
-- >>> p = Stream.parsePos Unicode.double . Stream.fromList
--
-- >>> p "-1.23e-123"
-- Right (-1.23e-123)
--
-- Trailing input examples:
--
-- >>> p "1."
-- Right 1.0
--
-- >>> p "1.2.3"
-- Right 1.2
--
-- >>> p "1e"
-- Right 1.0
--
-- >>> p "1e2.3"
-- Right 100.0
--
-- >>> p "1+2"
-- Right 1.0
--
-- Error cases:
--
-- >>> p ""
-- Left (ParseError 0 "number: expecting sign or decimal digit, got end of input")
--
-- >>> p ".1"
-- Left (ParseError 1 "number: expecting sign or decimal digit, got '.'")
--
-- >>> p "+"
-- Left (ParseError 1 "number: expecting decimal digit, got end of input")
--
{-# INLINE double #-}
double :: Monad m => Parser Char m Double
double = fmap (\(m,e) -> mkDouble (fromIntegral m) e) doubleParser
