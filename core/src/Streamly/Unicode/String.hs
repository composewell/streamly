{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Unicode.String
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Convenient template Haskell quasiquoters to format strings.
--
-- The 'str' quasiquoter retains newlines in the string when the line is split
-- across multiple lines. The @unwords . lines@ idiom can be used on the
-- resulting string to collapse it into a single line.

module Streamly.Unicode.String
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    str
    )
where

import Streamly.Internal.Unicode.String

#include "DocTestUnicodeString.hs"
