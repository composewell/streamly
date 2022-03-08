-- |
-- Module      : Streamly.Data.Unicode.Stream
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- = Processing Unicode Strings
--
-- A 'Char' stream is the canonical representation to process Unicode strings.
-- It can be processed efficiently using regular stream processing operations.
-- A byte stream of Unicode text read from an IO device or from an
-- 'Streamly.Data.Array.Foreign.Array' in memory can be decoded into a 'Char' stream
-- using the decoding routines in this module.  A 'String' (@[Char]@) can be
-- converted into a 'Char' stream using 'Streamly.Prelude.fromList'.  An @Array
-- Char@ can be 'Streamly.Prelude.unfold'ed into a stream using the array
-- 'Streamly.Data.Array.Foreign.read' unfold.
--
-- = Storing Unicode Strings
--
-- A stream of 'Char' can be encoded into a byte stream using the encoding
-- routines in this module and then written to IO devices or to arrays in
-- memory.
--
-- If you have to store a 'Char' stream in memory you can convert it into a
-- 'String' using 'Streamly.Prelude.toList' or using the
-- 'Streamly.Data.Fold.toList' fold. The 'String' type can be more efficient
-- than pinned arrays for short and short lived strings.
--
-- For longer or long lived streams you can 'Streamly.Prelude.fold' the 'Char'
-- stream as @Array Char@ using the array 'Streamly.Data.Array.Foreign.write' fold.
-- The 'Array' type provides a more compact representation and pinned memory
-- reducing GC overhead. If space efficiency is a concern you can use
-- 'encodeUtf8'' on the 'Char' stream before writing it to an 'Array' providing
-- an even more compact representation.
--
-- = String Literals
--
-- @SerialT Identity Char@ and @Array Char@ are instances of 'IsString' and
-- 'IsList', therefore, 'OverloadedStrings' and 'OverloadedLists' extensions
-- can be used for convenience when specifying unicode strings literals using
-- these types.
--
-- = Pitfalls
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
--
-- = Experimental APIs
--
-- Some experimental APIs to conveniently process text using the
-- @Array Char@ represenation directly can be found in
-- "Streamly.Internal.Memory.Unicode.Array".

-- XXX an unpinned array representation can be useful to store short and short
-- lived strings in memory.
--
module Streamly.Data.Unicode.Stream
    {-# DEPRECATED "Use \"Streamly.Unicode.Stream\" instead" #-}
    (
    -- * Construction (Decoding)
      decodeLatin1
    , decodeUtf8

    -- * Elimination (Encoding)
    , encodeLatin1
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

    -- * Deprecations
    , decodeUtf8Lax
    , encodeLatin1Lax
    , encodeUtf8Lax
    )
where

import Streamly.Internal.Unicode.Stream
import Prelude hiding (lines, words, unlines, unwords)