-- |
-- Module      : Streamly.Unicode.Parser
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- To parse a text input, use the decode routines from
-- "Streamly.Unicode.Stream" module to convert an input byte stream to a
-- Unicode Char stream and then use these parsers on the Char stream.

module Streamly.Unicode.Parser
    (
     -- * Single Chars

     -- Any char
      char
    , charIgnoreCase

    -- Char classes
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

    -- Digits
    , digit
    , octDigit
    , hexDigit
    , numeric

    -- * Char Sequences
    , string
    , stringIgnoreCase
    , dropSpace
    , dropSpace1

    -- * Digit Sequences (Numbers)
    , decimal
    , hexadecimal
    , double

    -- * Modifiers
    , signed
    )
where

import Streamly.Internal.Unicode.Parser
