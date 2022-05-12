-- |
-- Module      : Streamly.Internal.Console.Stdio
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module works only with UTF-8 encoding.
--
-- The stream writing APIs are polymorphic on types that belong to the 'ToUTF8'
-- type class. This allows us to simply use the same APIs for streams, lists
-- and arrays.
--
-- Note that when using the buffered reading APIs keep in mind that these APIs
-- buffer outside the stdin Handle, therefore if the stream is consumed
-- partially, the data in the buffer may be discarded. Therefore, if we read
-- from the handle again we would have lost some data. Therefore, these APIs
-- can only be used if you want to process the full stream using the API. You
-- cannot stop the stream and start reading again.

module Streamly.Internal.Console.Stdio
    (
    -- * Standard Input
    -- ** Singleton reads
      getC
    -- , getS -- cannot be implemented outside the stdin Handle
    , getL
    , getChunk  -- Single raw chunk

    -- ** Unfolds
    , read
    , readChunks

    -- ** Raw Streams
    , getBytes
    , getChunks

    -- ** UTF-8 Char Streams
    , getChars
    -- These cannot be implemented outside the stdin handle
    -- , getStringsWith -- get strings using the supplied decoding
    -- , getStrings -- get character chunks as they become available
    -- , getLineChunks -- get line chunks as they become available
    , getLines -- get single lines decoded as char strings

    -- * Standard Output
    -- ** Singleton writes
    -- Since these are commonly used it is desirable that the names are short,
    -- are not visually similar to plural names etc (e.g. putChar) and do not
    -- conflict with Prelude (putStr).
    --
    -- Note: We can extend these to use encoding from the environment.
    --
    , putC      -- same as System.IO.putChar
    , putS      -- Ubuffered char chunk (echo -n)
    , putL      -- Unbuffered line (echo)
    , putChunk  -- Single raw chunk

    -- ** Folds
    , write
    , writeChunks

    -- * Raw Streams
    , putBytes  -- Buffered (32K) raw bytes
    , putChunks -- raw chunks

    -- ** Char Streams
    , putStringsWith
    -- putCharsWith
    -- putChunksWith

    -- ** UTF-8 Char Streams
    -- Multi APIs
    , putChars       -- Buffered utf8 chars
    , putStrings     -- utf8 char chunks
    , putLines       -- utf8 lines

    -- * Standard Error
    , writeErr
    , writeErrChunks

    -- Note: We can use putErr* for writing streams to stderr
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import System.IO (stdin, stdout, stderr)
import Prelude hiding (read)

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Array.Foreign.Type (Array(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream.ToStream (IsFold(..), IsUnfold(..))

import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode

-------------------------------------------------------------------------------
-- Reads
-------------------------------------------------------------------------------

-- | Read a byte array from standard input.
--
-- >>> getChunk n = Handle.getChunk n stdin
--
-- /Pre-release/
--
{-# INLINE getChunk #-}
getChunk :: MonadIO m => Int -> m (Array Word8)
getChunk n = Handle.getChunk n stdin

-- | Write a character on console.
--
-- >>> putC = liftIO . putChar
--
-- /Pre-release/
--
{-# INLINE getC #-}
getC :: MonadIO m => m Char
getC = liftIO getChar

-- XXX It may be more efficient if the handle returns a raw line Array Word8 or
-- even an Array Char.
--
-- | Read a line from console.
--
-- /Pre-release/
--
{-# INLINE_EARLY getL #-}
getL :: forall m a. (MonadIO m, IsFold m Char a) => m a
getL =
    liftIO getLine
        >>= (Stream.fold eliminator . (Stream.fromList :: String -> SerialT m Char))

{-# RULES "getLStr" getL = getLStr #-}
{-# INLINE getLStr #-}
getLStr :: MonadIO m => m String
getLStr = liftIO getLine

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
getBytes :: MonadIO m => SerialT m Word8
getBytes = Handle.getBytes stdin

-- | Read a character stream from Utf8 encoded standard input.
--
-- > getChars = Unicode.decodeUtf8 Stdio.getBytes
--
-- /Pre-release/
--
{-# INLINE getChars #-}
getChars :: MonadIO m => SerialT m Char
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
getChunks :: MonadIO m => SerialT m (Array Word8)
getChunks = Handle.getChunks stdin

{-
-- This implementation may not work if we want to terminate getLines and then
-- want read again from the handle. Since we are buffering outside the handle
-- we will lose the buffer on termination of the stream.
--
-- | Read UTF8 encoded lines from standard input.
--
-- You may want to process the input byte stream directly using appropriate
-- folds for more efficient processing.
--
-- /Pre-release/
--
{-# INLINE getLines #-}
getLines :: MonadIO m => SerialT m (Array Word8)
getLines = (Stream.splitWithSuffix (== '\n') f) getChars

    -- XXX Need to implement Fold.unfoldMany, should be easy for
    -- non-terminating folds, but may be tricky for terminating folds. See
    -- Array Stream folds.
    where f = Fold.unfoldMany Unicode.readCharUtf8 Array.write
-}

-- XXX Remove MonadAsync, by using Serial verison of repeatM.
--
-- | Read UTF8 encoded lines from standard input.
--
-- This API uses the buffering of the stdin handle, therefore, the stream can
-- be consumed partially without any issues.
--
-- You may want to process the input byte stream directly using appropriate
-- folds for more efficient processing.
--
-- /Pre-release/
--
{-# INLINE_EARLY getLines #-}
getLines :: forall m a. (MonadAsync m, IsFold m Char a) => SerialT m a
getLines =
    Stream.mapM (Stream.fold eliminator . (Stream.fromList :: String -> SerialT m Char))
    (Stream.repeatM (liftIO getLine) :: SerialT m String)

{-# RULES "getLinesStr" getLines = getLinesStr #-}
{-# INLINE getLinesStr #-}
getLinesStr :: MonadAsync m => SerialT m String
getLinesStr = Stream.repeatM (liftIO getLine)

-------------------------------------------------------------------------------
-- Writes
-------------------------------------------------------------------------------

-- | Write a byte array to standard output.
--
-- >>> putChunk = Handle.putChunk stdout
--
-- /Pre-release/
--
{-# INLINE putChunk #-}
putChunk :: MonadIO m => Array Word8 -> m ()
putChunk = Handle.putChunk stdout

-- | Write a character string on console.
--
-- It works on @t m Char@, 'String' ([Char]) or an @Array Char@ type.
--
-- /Pre-release/
--
{-# INLINE putS #-}
putS :: (MonadIO m, IsUnfold m a Char) => a -> m ()
putS x = putChunk =<< Array.fromStream (Unicode.encodeUtf8 $ Stream.unfold generator x)

-- | Write a character on console.
--
-- >>> putC = liftIO . putChar
--
-- /Pre-release/
--
{-# INLINE putC #-}
putC :: MonadIO m => Char -> m ()
putC = liftIO . putChar

-- | Write a character string on console followed by a newline character.
--
-- >>> putL x = putS x >> putC '\n'
--
-- /Pre-release/
--
{-# INLINE putL #-}
putL :: (MonadIO m, IsUnfold m a Char) => a -> m ()
putL x = putS x >> putC '\n'

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
putBytes :: MonadIO m => SerialT m Word8 -> m ()
putBytes = Handle.putBytes stdout

-- | Encode a character stream to Utf8 and write it to standard output.
--
-- >>> putChars = Stdio.putBytes . Unicode.encodeUtf8 . ToStream.toStream
--
-- /Pre-release/
--
{-# INLINE putChars #-}
putChars :: (MonadIO m, IsUnfold m a Char) => a -> m ()
putChars = putBytes . Unicode.encodeUtf8 . Stream.unfold generator

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
putChunks :: MonadIO m => SerialT m (Array Word8) -> m ()
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
putStringsWith :: (MonadIO m, IsUnfold m a Char)
    => (SerialT m Char -> SerialT m Word8) -> SerialT m a -> m ()
putStringsWith encode =
    putChunks . Serial.mapM (Array.fromStream . encode . Stream.unfold generator)

-- | Write a stream of strings to standard output using UTF8 encoding.  Output
-- is flushed to the device for each string.
--
-- >>> putStrings = putStringsWith Unicode.encodeUtf8
--
-- /Pre-release/
--
{-# INLINE putStrings #-}
putStrings :: (MonadIO m, IsUnfold m a Char) => SerialT m a -> m ()
putStrings = putStringsWith Unicode.encodeUtf8

-- | Like 'putStrings' but adds a newline at the end of each string.
--
-- XXX This is not portable, on Windows we need to use "\r\n" instead.
--
-- /Pre-release/
--
{-# INLINE putLines #-}
putLines :: (MonadIO m, IsUnfold m a Char) => SerialT m a -> m ()
putLines =
      putChunks
    . Stream.intersperseSuffix (return $ Array.fromList [10])
    . Serial.mapM (Array.fromStream . Unicode.encodeUtf8 . Stream.unfold generator)
