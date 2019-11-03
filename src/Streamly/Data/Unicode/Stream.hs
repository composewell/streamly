{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Streamly.Data.Unicode.Stream
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A byte stream of Unicode text can be decoded into a stream of 'Char' and
-- processed efficiently using regular stream processing operations. A stream
-- of 'Char' can be encoded into a byte stream using UTF formats and written to
-- IO devices.
--
-- If you have to store a unicode string in memory you can either write it as a
-- pure stream "SerialT Identity Char" or write the 'Char' stream as "Array
-- Char". If space efficiency is a concern you can UTF8 encode the 'Char'
-- stream and then write it as "Array Word8".  "SerialT Identity Char" and
-- "Array Char" are instances of 'IsString' and 'IsList', therefore,
-- 'OverloadedStrings' and 'OverloadedLists' extensions can be used for
-- convenience.
--
-- Some experimental APIs to conveniently process text using "Array Char"
-- represenation can be found in "Streamly.Internal.Unicode.Array".
--
-- Please note the following:
--
-- * Case conversion: Some unicode characters translate to more than one code
-- point on case conversion. The 'toUpper' and 'toLower' functions in @base@
-- package do not handle such characters. Therefore, operations like @map
-- toUpper@ on a character stream or character array may not always perform
-- correct conversion.
-- * String comparison: In some cases, visually identical strings may have
-- different unicode representations, therefore, a character stream or
-- character array cannot be directly compared. A normalized comparison may be
-- needed to check string equivalence correctly.

module Streamly.Data.Unicode.Stream
    (
    -- * Construction (Decoding)
      decodeLatin1
    , decodeUtf8
    , decodeUtf8Lax

    -- * Elimination (Encoding)
    , encodeLatin1
    , encodeLatin1Lax
    , encodeUtf8
    {-
    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd
    , stripStart
    -}
    -- Not exposing these yet as we have consider these with respect to Unicode
    -- segmentation routines which are yet to be implemented.
    -- -- * Transformation
    -- , lines
    -- , words
    -- , unlines
    -- , unwords
    )
where

import Streamly.Internal.Data.Unicode.Stream
import Prelude hiding (lines, words, unlines, unwords)
