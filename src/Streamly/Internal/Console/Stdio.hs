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
    -- * Read
      read
    , getBytes
    , getChars
    , readChunks
    , getChunks
    -- , getChunksLn
    -- , getStringsWith -- get strings using the supplied decoding
    -- , getStrings -- get strings of complete chars,
                  -- leave any partial chars for next string
    -- , getStringsLn -- get lines decoded as char strings

    -- * Write
    , write
    , writeErr
    , putBytes  -- Buffered (32K)
    , putChars
    , writeChunks
    , writeErrChunks
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

import Streamly.Internal.Data.Array.Unboxed.Type (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.Fold (Fold)

import qualified Streamly.Data.Array.Unboxed as Array (fromList)
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Data.Unfold as Unfold (lmap)
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Reads
-------------------------------------------------------------------------------

-- | Unfold standard input into a stream of 'Word8'.
--
-- @since 0.8.0
{-# INLINE read #-}
read :: MonadIO m => Unfold m () Word8
read = Unfold.lmap (\() -> stdin) Handle.read

-- | Read a byte stream from standard input.
--
-- > getBytes = Handle.getBytes stdin
-- > getBytes = Stream.unfold Stdio.read ()
--
-- /Pre-release/
--
{-# INLINE getBytes #-}
getBytes :: MonadIO m => Stream m Word8
getBytes = Handle.getBytes stdin

-- | Read a character stream from Utf8 encoded standard input.
--
-- > getChars = Unicode.decodeUtf8 Stdio.getBytes
--
-- /Pre-release/
--
{-# INLINE getChars #-}
getChars :: MonadIO m => Stream m Char
getChars = Unicode.decodeUtf8 getBytes

-- | Unfolds standard input into a stream of 'Word8' arrays.
--
-- @since 0.8.0
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m () (Array Word8)
readChunks = Unfold.lmap (\() -> stdin) Handle.readChunks

-- | Read a stream of chunks from standard input.  The maximum size of a single
-- chunk is limited to @defaultChunkSize@. The actual size read may be less
-- than @defaultChunkSize@.
--
-- > getChunks = Handle.getChunks stdin
-- > getChunks = Stream.unfold Stdio.readChunks ()
--
-- /Pre-release/
--
{-# INLINE getChunks #-}
getChunks :: MonadIO m => Stream m (Array Word8)
getChunks = Handle.getChunks stdin

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
-- @since 0.8.0
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ()
write = Handle.write stdout

-- | Fold a stream of 'Word8' to standard error.
--
-- @since 0.8.0
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
-- @since 0.8.0
{-# INLINE writeChunks #-}
writeChunks :: MonadIO m => Fold m (Array Word8) ()
writeChunks = Handle.writeChunks stdout

-- | Fold a stream of @Array Word8@ to standard error.
--
-- @since 0.8.0
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
    . Stream.intersperseSuffix (return $ Array.fromList [10])
    . Unicode.encodeStrings Unicode.encodeUtf8
