-- |
-- Module      : Streamly.String
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Strings are simply character streams. Stream of 'Char' are the equivalent of
-- Haskell Strings but without any performance issues that are usually
-- associated with the 'String' type. We can use the usual stream processing
-- routines in these 'Char' streams. The processing of streams is very
-- efficient in terms of CPU and space usage. Streamly employs stream fusion
-- which makes all the stream operations extremly fast.
--
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
    ( String
    -- * Encoding and Decoding
    -- | Streams of 'Word8' read from a file handle can be decoded into a
    -- stream of unicode characters, similalrly before we write unicode text to
    -- a file we can use an encoding routine to convert it to a stream of bytes
    -- before writing.

    , encodeUtf8
    , decodeUtf8
    , fromHandleUtf8
    , toHandleUtf8

    -- * Line oriented IO
    , fromHandleLines
    , toHandleLines

    -- * Unicode aware operations
    , toCaseFold
    , toLower
    , toUpper
    , toTitle

    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripStart
    , stripEnd
    , foldLines
    , foldWords
    , lines
    , words
    , unlines
    , unwords
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Char (ord, chr)
import Data.Word (Word8, Word32)
import Streamly (IsStream, SerialT)
import Streamly.List (List)
import Prelude hiding (String, lines, words, unlines, unwords)
import System.IO (Handle)
import Streamly.Fold (Fold)
import Streamly.Array (Array)
import Streamly.FileIO (fromHandle, toHandle)

import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
import qualified Streamly.Array as A

type String = List Char

-------------------------------------------------------------------------------
-- Encoding/Decoding Characters
-------------------------------------------------------------------------------

-- decodeWith :: TextEncoding -> t m Word8 -> t m Char
-- decodeWith = undefined

-------------------------------------------------------------------------------
-- Encoding/Decoding Unicode Characters
-------------------------------------------------------------------------------

-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
decodeUtf8 :: IsStream t => t m Word8 -> t m Char
decodeUtf8 = undefined

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream.
encodeUtf8 :: IsStream t => t m Char -> t m Word8
encodeUtf8 = undefined

-------------------------------------------------------------------------------
-- IO with encoding/decoding
-------------------------------------------------------------------------------

{-# INLINE fromHandleUtf8 #-}
fromHandleUtf8 :: (IsStream t, MonadIO m) => Handle -> t m Char
fromHandleUtf8 = decodeUtf8 . fromHandle

{-# INLINE toHandleUtf8 #-}
toHandleUtf8 :: MonadIO m => Handle -> SerialT m Char -> m ()
toHandleUtf8 h s = toHandle h $ encodeUtf8 s

-------------------------------------------------------------------------------
-- Line buffered IO
-------------------------------------------------------------------------------

-- XXX which encoding is to be used for these?
--
-- | Read lines from a file handle and apply the specified fold to each line.
{-# INLINE fromHandleLines #-}
fromHandleLines :: (IsStream t, MonadIO m) => Handle -> Fold m Char b -> t m b
fromHandleLines h f = foldLines (fromHandleUtf8 h) f

-- | Write a byte stream in chunks ended by a linefeed byte ('\n)'. If the size
-- of the buffer exceeds defaultChunkSize and a linefeed is not found, the
-- buffer is written anyway.
--
{-# INLINE toHandleLines #-}
toHandleLines :: (IsStream t, MonadIO m) => Handle -> t m Char -> m ()
toHandleLines = undefined

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

stripStart :: IsStream t => t m Char -> t m Char
stripStart = undefined

stripEnd :: IsStream t => t m Char -> t m Char
stripEnd = undefined

foldLines :: IsStream t => t m Char -> Fold m Char b -> t m b
foldLines = undefined

foldWords :: IsStream t => t m Char -> Fold m Char b -> t m b
foldWords = undefined

lines :: IsStream t => t m Char -> t m (Array Char)
lines = undefined

words :: IsStream t => t m Char -> t m (Array Char)
words = undefined

unlines :: IsStream t => t m (Array Char) -> t m Char
unlines = undefined

unwords :: IsStream t => t m (Array Char) -> t m Char
unwords = undefined
