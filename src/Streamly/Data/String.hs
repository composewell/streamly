{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Streamly.Data.String
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
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

-- See "Streamly.Internal.Data.List", <src/docs/streamly-vs-lists.md> for more details and
-- <src/test/PureStreams.hs> for comprehensive usage examples.

-- TBD write a note on performance comparison with text, bytestrings, lists and
-- vector.
--
module Streamly.Data.String
    (
    -- * Construction (Decoding)
      decodeChar8
    , decodeUtf8
    , decodeUtf8Lenient
    , decodeUtf8Arrays
    , decodeUtf8ArraysLenient

    -- * Elimination (Encoding)
    , encodeChar8
    , encodeChar8Unchecked
    , encodeUtf8
{-
    -- * Unicode aware operations
    , toCaseFold
    , toLower
    , toUpper
    , toTitle

    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd-}
    -- * Transformation
    , stripStart
    , foldLines
    , foldWords
    , unfoldLines

    -- * Streams of Strings
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
import Streamly.Data.Fold (Fold)
import Streamly.Memory.Array (Array)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Internal.Data.Unfold as UF

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

{-# INLINE decodeUtf8Arrays #-}
decodeUtf8Arrays :: (MonadIO m, IsStream t) => t m (Array Word8) -> t m Char
decodeUtf8Arrays = D.fromStreamD . D.decodeUtf8Arrays . D.toStreamD

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- Any invalid codepoint encountered is replaced with the unicode replacement
-- character.
{-# INLINE decodeUtf8Lenient #-}
decodeUtf8Lenient :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8Lenient = D.fromStreamD . D.decodeUtf8Lenient . D.toStreamD

{-# INLINE decodeUtf8ArraysLenient #-}
decodeUtf8ArraysLenient ::
       (MonadIO m, IsStream t) => t m (Array Word8) -> t m Char
decodeUtf8ArraysLenient =
    D.fromStreamD . D.decodeUtf8ArraysLenient . D.toStreamD

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

-- | Remove leading whitespace from a string.
--
-- > stripStart = S.dropWhile isSpace
{-# INLINE stripStart #-}
stripStart :: (Monad m, IsStream t) => t m Char -> t m Char
stripStart = S.dropWhile isSpace

-- | Fold each line of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>> S.toList $ foldLines FL.toList (S.fromList "lines\nthis\nstring\n\n\n")
-- ["lines", "this", "string", "", ""]
--
-- > foldLines = S.splitOnSuffix (== '\n')
--
{-# INLINE foldLines #-}
foldLines :: (Monad m, IsStream t) => Fold m Char b -> t m Char -> t m b
foldLines = S.splitOnSuffix (== '\n')

-- | Fold each word of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>>  S.toList $ foldWords FL.toList (S.fromList "fold these     words")
-- ["fold", "these", "words"]
--
-- > foldWords = S.wordsBy isSpace
--
{-# INLINE foldWords #-}
foldWords :: (Monad m, IsStream t) => Fold m Char b -> t m Char -> t m b
foldWords = S.wordsBy isSpace

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

-- | Break a string up into a stream of strings at newline characters.
-- The resulting strings do not contain newlines.
--
-- > lines = foldLines A.write
--
-- >>> S.toList $ lines $ S.fromList "lines\nthis\nstring\n\n\n"
-- ["lines","this","string","",""]
--
-- If you're dealing with lines of massive length, consider using
-- 'foldLines' instead to avoid buffering the data in 'Array'.
{-# INLINE lines #-}
lines :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
lines = foldLines A.write

-- | Break a string up into a stream of strings, which were delimited
-- by characters representing white space.
--
-- > words = foldWords A.write
--
-- >>> S.toList $ words $ S.fromList "A  newline\nis considered white space?"
-- ["A", "newline", "is", "considered", "white", "space?"]
--
-- If you're dealing with words of massive length, consider using
-- 'foldWords' instead to avoid buffering the data in 'Array'.
{-# INLINE words #-}
words :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
words = foldWords A.write

-- | Unfold a stream to character streams using the supplied 'Unfold'
-- and concat the results suffixing a newline character @\\n@ to each stream.
--
-- > unfoldLines = S.intercalateSuffix UF.singleton '\n'
--
{-# INLINE unfoldLines #-}
unfoldLines :: (MonadIO m, IsStream t) => Unfold m a Char -> t m a -> t m Char
unfoldLines unf = S.intercalateSuffix UF.singleton '\n' unf

-- | Flattens the stream of @Array Char@, after appending a terminating
-- newline to each string.
--
-- 'unlines' is an inverse operation to 'lines'.
--
-- >>> S.toList $ unlines $ S.fromList ["lines", "this", "string"]
-- "lines\nthis\nstring\n"
--
-- > unlines = unfoldLines A.read
--
-- Note that, in general
--
-- > unlines . lines /= id
{-# INLINE unlines #-}
unlines :: (MonadIO m, IsStream t) => t m (Array Char) -> t m Char
unlines = unfoldLines A.read

-- | Flattens the stream of @Array Char@, after appending a separating
-- space to each string.
--
-- 'unwords' is an inverse operation to 'words'.
--
-- >>> S.toList $ unwords $ S.fromList ["unwords", "this", "string"]
-- "unwords this string"
--
--
-- > unwords = A.concat . (S.intersperse (A.fromList " "))
--
-- Note that, in general
--
-- > unwords . words /= id
{-# INLINE unwords #-}
unwords :: (MonadAsync m, IsStream t) => t m (Array Char) -> t m Char
unwords = AS.concat . (S.intersperse (A.fromList " "))
