-- |
-- Module      : Streamly.Internal.Console.Stdio
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Console.Stdio
    (
    -- * Streams
      read
    , readChars
    , readChunks
    -- , getChunksLn
    -- , getStringsWith -- get strings using the supplied decoding
    -- , getStrings -- get strings of complete chars,
                  -- leave any partial chars for next string
    -- , getStringsLn -- get lines decoded as char strings

    -- * Unfolds
    , reader
    , chunkReader

    -- * Folds
    , write
    , writeChunks
    , writeErr
    , writeErrChunks

    -- * Stream writes
    , putBytes  -- Buffered (32K)
    , putChars
    , putChunks -- Unbuffered
    , putStringsWith
    , putStrings
    , putStringsLn
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import System.IO (stdin, stdout, stderr)
import Prelude hiding (read)

import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.Fold (Fold)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
    (intersperseMSuffix)
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Reads
-------------------------------------------------------------------------------

-- | Unfold standard input into a stream of 'Word8'.
--
{-# INLINE reader #-}
reader :: MonadIO m => Unfold m () Word8
reader = Unfold.lmap (\() -> stdin) Handle.reader

-- | Read a byte stream from standard input.
--
-- > read = Handle.read stdin
-- > read = Stream.unfold Stdio.reader ()
--
-- /Pre-release/
--
{-# INLINE read #-}
read :: MonadIO m => Stream m Word8
read = Handle.read stdin

-- | Read a character stream from Utf8 encoded standard input.
--
-- > readChars = Unicode.decodeUtf8 Stdio.read
--
-- /Pre-release/
--
{-# INLINE readChars #-}
readChars :: MonadIO m => Stream m Char
readChars = Unicode.decodeUtf8 read

-- | Unfolds standard input into a stream of 'Word8' arrays.
--
{-# INLINE chunkReader #-}
chunkReader :: MonadIO m => Unfold m () (Array Word8)
chunkReader = Unfold.lmap (\() -> stdin) Handle.chunkReader

-- | Read a stream of chunks from standard input.  The maximum size of a single
-- chunk is limited to @defaultChunkSize@. The actual size read may be less
-- than @defaultChunkSize@.
--
-- > readChunks = Handle.readChunks stdin
-- > readChunks = Stream.unfold Stdio.chunkReader ()
--
-- /Pre-release/
--
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Stream m (Array Word8)
readChunks = Handle.readChunks stdin

{-
-- | Read UTF8 encoded lines from standard input.
--
-- You may want to process the input byte stream directly using appropriate
-- folds for more efficient processing.
--
-- /Pre-release/
--
{-# INLINE getChunksLn #-}
getChunksLn :: MonadIO m => Stream m (Array Word8)
getChunksLn = (Stream.splitWithSuffix (== '\n') f) getChars

    -- XXX Need to implement Fold.unfoldMany, should be easy for
    -- non-terminating folds, but may be tricky for terminating folds. See
    -- Array Stream folds.
    where f = Fold.unfoldMany Unicode.readCharUtf8 Array.write
-}

-------------------------------------------------------------------------------
-- Writes
-------------------------------------------------------------------------------

-- | Fold a stream of 'Word8' to standard output.
--
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ()
write = Handle.write stdout

-- | Fold a stream of 'Word8' to standard error.
--
{-# INLINE writeErr #-}
writeErr :: MonadIO m => Fold m Word8 ()
writeErr = Handle.write stderr

-- | Write a stream of bytes to standard output.
--
-- > putBytes = Handle.putBytes stdout
-- > putBytes = Stream.fold Stdio.write
--
-- /Pre-release/
--
{-# INLINE putBytes #-}
putBytes :: MonadIO m => Stream m Word8 -> m ()
putBytes = Handle.putBytes stdout

-- | Encode a character stream to Utf8 and write it to standard output.
--
-- > putChars = Stdio.putBytes . Unicode.encodeUtf8
--
-- /Pre-release/
--
{-# INLINE putChars #-}
putChars :: MonadIO m => Stream m Char -> m ()
putChars = putBytes . Unicode.encodeUtf8

-- | Fold a stream of @Array Word8@ to standard output.
--
{-# INLINE writeChunks #-}
writeChunks :: MonadIO m => Fold m (Array Word8) ()
writeChunks = Handle.writeChunks stdout

-- | Fold a stream of @Array Word8@ to standard error.
--
{-# INLINE writeErrChunks #-}
writeErrChunks :: MonadIO m => Fold m (Array Word8) ()
writeErrChunks = Handle.writeChunks stderr

-- | Write a stream of chunks to standard output.
--
-- > putChunks = Handle.putChunks stdout
-- > putChunks = Stream.fold Stdio.writeChunks
--
-- /Pre-release/
--
{-# INLINE putChunks #-}
putChunks :: MonadIO m => Stream m (Array Word8) -> m ()
putChunks = Handle.putChunks stdout

-------------------------------------------------------------------------------
-- Line buffered
-------------------------------------------------------------------------------

-- XXX We need to write transformations as pipes so that they can be applied to
-- folds as well as unfolds/streams. Non-backtracking (one-to-one, one-to-many,
-- filters, reducers) transformations may be easy so we can possibly start with
-- those.
--
-- | Write a stream of strings to standard output using the supplied encoding.
-- Output is flushed to the device for each string.
--
-- /Pre-release/
--
{-# INLINE putStringsWith #-}
putStringsWith :: MonadIO m
    => (Stream m Char -> Stream m Word8) -> Stream m String -> m ()
putStringsWith encode = putChunks . Unicode.encodeStrings encode

-- | Write a stream of strings to standard output using UTF8 encoding.  Output
-- is flushed to the device for each string.
--
-- /Pre-release/
--
{-# INLINE putStrings #-}
putStrings :: MonadIO m => Stream m String -> m ()
putStrings = putStringsWith Unicode.encodeUtf8

-- | Like 'putStrings' but adds a newline at the end of each string.
--
-- XXX This is not portable, on Windows we need to use "\r\n" instead.
--
-- /Pre-release/
--
{-# INLINE putStringsLn #-}
putStringsLn :: MonadIO m => Stream m String -> m ()
putStringsLn =
      putChunks
    . Stream.intersperseMSuffix (return $ Array.fromList [10])
    . Unicode.encodeStrings Unicode.encodeUtf8
