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
-- A unicode string can be represented either as a stream of 'Char' e.g.
-- 'SerialT' 'Identity' 'Char' or as 'Array' 'Char'.  Unicode text processing
-- can be done efficiently by applying stream operations and folds on the
-- stream of 'Char'. When using 'Array' 'Char' direct array operations can be
-- applied where available or the array can be read into a stream and then
-- processed using stream operations. Use 'Array' 'Char' when you need to store
-- or buffer strings temporarily in memory.  Streams in 'Identity' monad and
-- 'Array' 'Char' are instances of 'IsString' and 'IsList', therefore,
-- 'OverloadedStrings' and 'OverloadedLists' extensions can be used for
-- convenience.
--
-- 'Array' 'Char' is usually perfectly fine to buffer short to medium or even
-- large amounts of text in memory. Also, it is computationally efficient as
-- there is no encoding/decoding involved.  We recommend using true streaming
-- operations to avoid buffering large amounts of data as long as possible e.g.
-- use `foldLines` instead of `lines`. However, if for some reason you are
-- buffering very large amounts of text in memory and are worried about space
-- efficiency you can use 'encodeUtf8' on the stream to convert it to a utf8
-- encoded 'Array' 'Word8'.
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
      decodeChar8
    , decodeUtf8
    , decodeUtf8Lenient

    -- * Elimination (Encoding)
    , encodeChar8
    , encodeChar8Unchecked
    , encodeUtf8
    {-
    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd
    , stripStart
    -}
    -- * Transformation
    , foldLines
    , foldWords
    , unfoldLines
    , unfoldWords
    )
where

import Streamly.Internal.Data.Unicode.Stream
import Prelude hiding (lines, words, unlines, unwords)
import qualified Streamly.Streams.StreamD as D
