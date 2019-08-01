{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Streamly.String
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Stream of 'Char' are the equivalent of Haskell Strings but without any
-- performance issues that are usually associated with the 'String' type.
-- Haskell lists are not suitable for storage in GC memory. We use streams for
-- processing and arrays for storing. We can use the usual stream processing
-- routines on these 'Char' streams. Stream processing is very efficient due to
-- stream fusion. If strings are to be stored or buffered in memory, they can
-- be encoded to 'Word8' arrays using the encoding routines provided in the
-- module. Therefore, a separate type for text representation is not required.
-- A @Stream Identity Char@ can be used almost as a drop-in replacement for the
-- standard Haskell @String@, especially when used with @OverloadedStrings@
-- extension, with little differences.

-- The 'String' type in this module is just a synonym for the type @List Char@.
-- It provides better performance compared to the standard Haskell @String@
-- type and can be used almost as a drop-in replacement, especially when used
-- with @OverloadedStrings@ extension, with little differences.
--
-- See "Streamly.List", <src/docs/streamly-vs-lists.md> for more details and
-- <src/test/PureStreams.hs> for comprehensive usage examples.

-- TBD write a note on performance comparison with text, bytestrings, lists and
-- vector.
--
module Streamly.String
    (
    -- String

    -- * Encoding and Decoding
      encodeChar8
    , encodeChar8Unchecked
    , decodeChar8

    , encodeUtf8
    , decodeUtf8
{-
    -- * Unicode aware operations
    , toCaseFold
    , toLower
    , toUpper
    , toTitle

    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd-}
    -- * Substrings
    , stripStart
    , foldLines
    , foldWords
    , lines
    , words
    , unlines
    , unwords
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Char (ord)
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import Streamly (IsStream, MonadAsync)
import Prelude hiding (String, lines, words, unlines, unwords)
import Streamly.Fold (Fold)
import Streamly.Mem.Array (Array, toArray)

import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array.Types as A (unlines)
import qualified Streamly.Mem.Array as A
import qualified Streamly.Streams.StreamD as D

-- type String = List Char

-------------------------------------------------------------------------------
-- Encoding/Decoding Characters
-------------------------------------------------------------------------------

-- decodeWith :: TextEncoding -> t m Word8 -> t m Char
-- decodeWith = undefined

-------------------------------------------------------------------------------
-- Encoding/Decoding Unicode Characters
-------------------------------------------------------------------------------

-- | Decode a stream of bytes to Unicode characters by mapping each byte to a
-- corresponding Unicode 'Char' in 0-255 range.
{-# INLINE decodeChar8 #-}
decodeChar8 :: (IsStream t, Monad m) => t m Word8 -> t m Char
decodeChar8 = S.map (unsafeChr . fromIntegral)

-- | Encode a stream of Unicode characters to bytes by mapping each character
-- to a byte in 0-255 range. Throws an error if the input stream contains
-- characters beyond 255.
{-# INLINE encodeChar8 #-}
encodeChar8 :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeChar8 = S.map convert
    where
    convert c =
        let codepoint = ord c
        in if codepoint > 255
           then error $ "Streamly.String.encodeChar8 invalid \
                    \input char codepoint " ++ show codepoint
           else fromIntegral codepoint

-- | Like 'encodeChar8' but silently truncates and maps input characters beyond
-- 255 to (incorrect) chars in 0-255 range. No error or exception is thrown
-- when such truncation occurs.
{-# INLINE encodeChar8Unchecked #-}
encodeChar8Unchecked :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeChar8Unchecked = S.map (fromIntegral . ord)

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- The incoming stream is truncated if an invalid codepoint is encountered.
{-# INLINE decodeUtf8 #-}
decodeUtf8 :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8 = D.fromStreamD . D.decodeUtf8 . D.toStreamD

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream.
{-# INLINE encodeUtf8 #-}
encodeUtf8 :: (Monad m, IsStream t) => t m Char -> t m Word8
encodeUtf8 = D.fromStreamD . D.encodeUtf8 . D.toStreamD

{-
-------------------------------------------------------------------------------
-- Unicode aware operations on strings
-------------------------------------------------------------------------------

toCaseFold :: IsStream t => t m Char -> t m Char
toCaseFold = undefined

toLower :: IsStream t => t m Char -> t m Char
toLower = undefined

toUpper :: IsStream t => t m Char -> t m Char
toUpper = undefined

toTitle :: IsStream t => t m Char -> t m Char
toTitle = undefined

-------------------------------------------------------------------------------
-- Utility operations on strings
-------------------------------------------------------------------------------

strip :: IsStream t => t m Char -> t m Char
strip = undefined

stripEnd :: IsStream t => t m Char -> t m Char
stripEnd = undefined
-}

-- | Remove leading whitespace from a String.
--
-- > stripStart = S.dropWhile isSpace
{-# INLINE stripStart #-}
stripStart :: (Monad m, IsStream t) => t m Char -> t m Char
stripStart = S.dropWhile isSpace

-- | Fold each line of the stream using the supplied Fold
-- and stream the result.
--
-- >>> S.toList $ foldLines (S.fromList "lines\nthis\nstring\n\n\n") FL.toList
-- ["lines", "this", "string", "", ""]
{-# INLINE foldLines #-}
foldLines :: (Monad m, IsStream t) => t m Char -> Fold m Char b -> t m b
foldLines = flip (FL.splitOnSuffix (== '\n'))

-- | Fold each word of the stream using the supplied Fold
-- and stream the result.
--
-- >>>  S.toList $ foldWords (S.fromList "fold these     words") FL.toList
-- ["fold", "these", "words"]
{-# INLINE foldWords #-}
foldWords :: (Monad m, IsStream t) => t m Char -> Fold m Char b -> t m b
foldWords = flip (FL.wordsBy isSpace)

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

-- | Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

-- | Break a string up into a list of strings at newline characters.
-- The resulting strings do not contain newlines.
--
-- >>> S.toList $ lines $ S.fromList "lines\nthis\nstring\n\n\n"
-- ["lines","this","string","",""]
--
-- If you're dealing with lines of massive length, consider using
-- 'foldLines' instead.
{-# INLINE lines #-}
lines :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
lines = FL.splitOnSuffix (== '\n') toArray

-- | Break a string up into a list of strings, which were delimited
-- by characters representing white space.
--
-- >>> S.toList $ words $ S.fromList "A  newline\nis considered white space?"
-- ["A", "newline", "is", "considered", "white", "space?"]
--
-- If you're dealing with words of massive length, consider using
-- 'foldWords' instead.
{-# INLINE words #-}
words :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
words = FL.wordsBy isSpace toArray

-- | Flattens the stream of @Array Char@, after appending a terminating
-- newline to each string.
--
-- 'unlines' is an inverse operation to 'lines'.
--
-- >>> S.toList $ unlines $ S.fromList ["lines", "this", "string"]
-- "lines\nthis\nstring\n"
--
-- Note that, in general
--
-- > unlines . lines /= id
{-# INLINE unlines #-}
unlines :: (MonadIO m, IsStream t) => t m (Array Char) -> t m Char
unlines = D.fromStreamD . A.unlines '\n' . D.toStreamD

-- | Flattens the stream of @Array Char@, after appending a separating
-- space to each string.
--
-- 'unwords' is an inverse operation to 'words'.
--
-- >>> S.toList $ unwords $ S.fromList ["unwords", "this", "string"]
-- "unwords this string"
--
-- Note that, in general
--
-- > unwords . words /= id
{-# INLINE unwords #-}
unwords :: (MonadAsync m, IsStream t) => t m (Array Char) -> t m Char
unwords = A.flattenArrays . (S.intersperse (A.fromList " "))
