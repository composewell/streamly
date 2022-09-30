-- |
-- Module      : Streamly.Unicode.Char.Parser
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To parse a text input, use the decode routines from
-- "Streamly.Unicode.Stream" module to convert an input byte stream to a
-- Unicode Char stream and then use these parsers on the Char stream.

module Streamly.Unicode.Char.Parser
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
    , decimal
    , hexadecimal
    )
where

import Streamly.Internal.Unicode.Char.Parser
