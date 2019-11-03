{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Streamly.Data.Internal.Unicode.Stream
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Unicode.Stream
    (
    -- * Construction (Decoding)
      decodeLatin1
    , decodeUtf8
    , decodeUtf8Lax
    , D.DecodeError(..)
    , D.DecodeState
    , D.CodePoint
    , decodeUtf8Either
    , resumeDecodeUtf8Either
    , decodeUtf8Arrays
    , decodeUtf8ArraysLenient

    -- * Elimination (Encoding)
    , encodeLatin1
    , encodeLatin1Lax
    , encodeUtf8
    {-
    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd
    -}
    -- * Transformation
    , stripStart
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
import Streamly (IsStream)
import Prelude hiding (String, lines, words, unlines, unwords)
import Streamly.Data.Fold (Fold)
import Streamly.Memory.Array (Array)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Streams.StreamD as D

-------------------------------------------------------------------------------
-- Encoding/Decoding Unicode Characters
-------------------------------------------------------------------------------

-- | Decode a stream of bytes to Unicode characters by mapping each byte to a
-- corresponding Unicode 'Char' in 0-255 range.
--
-- /Since: 0.7.0/
{-# INLINE decodeLatin1 #-}
decodeLatin1 :: (IsStream t, Monad m) => t m Word8 -> t m Char
decodeLatin1 = S.map (unsafeChr . fromIntegral)

-- | Encode a stream of Unicode characters to bytes by mapping each character
-- to a byte in 0-255 range. Throws an error if the input stream contains
-- characters beyond 255.
--
-- /Since: 0.7.0/
{-# INLINE encodeLatin1 #-}
encodeLatin1 :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeLatin1 = S.map convert
    where
    convert c =
        let codepoint = ord c
        in if codepoint > 255
           then error $ "Streamly.String.encodeLatin1 invalid \
                    \input char codepoint " ++ show codepoint
           else fromIntegral codepoint

-- | Like 'encodeLatin1' but silently truncates and maps input characters beyond
-- 255 to (incorrect) chars in 0-255 range. No error or exception is thrown
-- when such truncation occurs.
--
-- /Since: 0.7.0/
{-# INLINE encodeLatin1Lax #-}
encodeLatin1Lax :: (IsStream t, Monad m) => t m Char -> t m Word8
encodeLatin1Lax = S.map (fromIntegral . ord)

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- The incoming stream is truncated if an invalid codepoint is encountered.
--
-- /Since: 0.7.0/
{-# INLINE decodeUtf8 #-}
decodeUtf8 :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8 = D.fromStreamD . D.decodeUtf8 . D.toStreamD

-- |
--
-- /Internal/
{-# INLINE decodeUtf8Arrays #-}
decodeUtf8Arrays :: (MonadIO m, IsStream t) => t m (Array Word8) -> t m Char
decodeUtf8Arrays = D.fromStreamD . D.decodeUtf8Arrays . D.toStreamD

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
-- Any invalid codepoint encountered is replaced with the unicode replacement
-- character.
--
-- /Since: 0.7.0/
{-# INLINE decodeUtf8Lax #-}
decodeUtf8Lax :: (Monad m, IsStream t) => t m Word8 -> t m Char
decodeUtf8Lax = D.fromStreamD . D.decodeUtf8Lenient . D.toStreamD

-- |
--
-- /Internal/
{-# INLINE decodeUtf8Either #-}
decodeUtf8Either :: (Monad m, IsStream t)
    => t m Word8 -> t m (Either D.DecodeError Char)
decodeUtf8Either = D.fromStreamD . D.decodeUtf8Either . D.toStreamD

-- |
--
-- /Internal/
{-# INLINE resumeDecodeUtf8Either #-}
resumeDecodeUtf8Either
    :: (Monad m, IsStream t)
    => D.DecodeState
    -> D.CodePoint
    -> t m Word8
    -> t m (Either D.DecodeError Char)
resumeDecodeUtf8Either st cp =
    D.fromStreamD . D.resumeDecodeUtf8Either st cp . D.toStreamD

-- |
--
-- /Internal/
{-# INLINE decodeUtf8ArraysLenient #-}
decodeUtf8ArraysLenient ::
       (MonadIO m, IsStream t) => t m (Array Word8) -> t m Char
decodeUtf8ArraysLenient =
    D.fromStreamD . D.decodeUtf8ArraysLenient . D.toStreamD

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream.
--
-- /Since: 0.7.0/
{-# INLINE encodeUtf8 #-}
encodeUtf8 :: (Monad m, IsStream t) => t m Char -> t m Word8
encodeUtf8 = D.fromStreamD . D.encodeUtf8 . D.toStreamD

{-
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
--
-- /Internal/
{-# INLINE stripStart #-}
stripStart :: (Monad m, IsStream t) => t m Char -> t m Char
stripStart = S.dropWhile isSpace

-- | Fold each line of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>> S.toList $ lines FL.toList (S.fromList "lines\nthis\nstring\n\n\n")
-- ["lines", "this", "string", "", ""]
--
-- > lines = S.splitOnSuffix (== '\n')
--
-- /Internal/
{-# INLINE lines #-}
lines :: (Monad m, IsStream t) => Fold m Char b -> t m Char -> t m b
lines = S.splitOnSuffix (== '\n')

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

-- | Fold each word of the stream using the supplied 'Fold'
-- and stream the result.
--
-- >>>  S.toList $ words FL.toList (S.fromList "fold these     words")
-- ["fold", "these", "words"]
--
-- > words = S.wordsBy isSpace
--
-- /Internal/
{-# INLINE words #-}
words :: (Monad m, IsStream t) => Fold m Char b -> t m Char -> t m b
words = S.wordsBy isSpace

-- | Unfold a stream to character streams using the supplied 'Unfold'
-- and concat the results suffixing a newline character @\\n@ to each stream.
--
-- /Internal/
{-# INLINE unlines #-}
unlines :: (MonadIO m, IsStream t) => Unfold m a Char -> t m a -> t m Char
unlines = S.interposeSuffix '\n'

-- | Unfold the elements of a stream to character streams using the supplied
-- 'Unfold' and concat the results with a whitespace character infixed between
-- the streams.
--
-- /Internal/
{-# INLINE unwords #-}
unwords :: (MonadIO m, IsStream t) => Unfold m a Char -> t m a -> t m Char
unwords = S.interpose ' '
