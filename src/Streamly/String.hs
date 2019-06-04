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
    , stripStart
    , foldLines
    , foldWords
    , lines
    , words{-
    , unlines
    , unwords-}
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Char (isSpace, ord)
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import Streamly (IsStream)
import Prelude hiding (String, lines, words, unlines, unwords)
import Streamly.Fold (Fold)
import Streamly.Mem.Array (Array, toArray)

import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
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
{-# INLINE decodeUtf8 #-}
decodeUtf8 :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8 = D.fromStreamD . D.decodeUtf8 . D.toStreamD

{-# INLINE encodeUtf8 #-}
-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream.
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

{-# INLINE stripStart #-}
stripStart :: (Monad m, IsStream t) => t m Char -> t m Char
stripStart = S.dropWhile isSpace

{-# INLINE foldLines #-}
foldLines :: (Monad m, IsStream t) => t m Char -> Fold m Char b -> t m b
foldLines = flip (FL.splitSuffixBy (== '\n'))

{-# INLINE foldWords #-}
foldWords :: (Monad m, IsStream t) => t m Char -> Fold m Char b -> t m b
foldWords = flip (FL.wordsBy isSpace)

{-# INLINE lines #-}
lines :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
lines = FL.splitSuffixBy (== '\n') toArray

{-# INLINE words #-}
words :: (MonadIO m, IsStream t) => t m Char -> t m (Array Char)
words = FL.wordsBy isSpace toArray

{-
unlines :: IsStream t => t m (Array Char) -> t m Char
unlines = undefined

unwords :: IsStream t => t m (Array Char) -> t m Char
unwords = undefined
-}
