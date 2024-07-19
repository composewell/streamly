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
-- buffer outside the stdin Handle, therefore, if the stream is consumed
-- partially, the data in the buffer may be discarded. If we read from the
-- handle again we would have lost some data. Therefore, these APIs can only be
-- used if you want to process the full stream using the API. You cannot stop
-- the stream and start reading from the Handle again.
--
--
-- Plan:
--
-- Keep stdin stream in a global mutable reference.
-- Parse objects from stdin stream, splice back the leftover
-- The stream is binary, Unicode parsers decode it as utf8 first
-- Binary parsers directly parse objects from it
-- Can use parseBreak or consume entire stream directly
-- Cannot use the Handle backing the stream, directly.
-- Each output type can have its own module - String, Utf8, Text
-- The basic module could keep just binary stream stuff
-- Multiple threads reading from stdin?

module Streamly.Internal.Console.Stdio
    (
    -- * Standard Input
    {-
    -- ** Singleton reads
      getC -- getChar, and change getChars => getCharStream
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
    -}

    -- Note: We can use putErr* for writing streams to stderr
    )
where

#include "inline.hs"

import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef
import Data.Word (Word8)
import Prelude hiding (read)

import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.Fold (Fold)
-- import Streamly.Internal.Data.Stream.ToStream (IsFold(..), IsUnfold(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
    (intersperseMSuffix)
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode
import qualified System.IO as IO

-------------------------------------------------------------------------------
-- Reads
-------------------------------------------------------------------------------

-- | Read a stream of chunks from standard input.  The maximum size of a single
-- chunk is limited to @defaultChunkSize@. The actual size read may be less
-- than @defaultChunkSize@.
--
-- > getChunks = Handle.getChunks stdin
-- > getChunks = Stream.unfold Stdio.readChunks ()
--
-- /Pre-release/
--
{-# INLINE stdin #-}
stdin :: MonadIO m => SerialT m (Array Word8)
stdin = Handle.getChunks IO.stdin

{-# NOINLINE stdinM #-}
stdinM :: MonadIO m => IORef (SerialT m (Array Word8))
stdinM = unsafePerformIO (newIORef stdin)

{-# INLINE getStdin #-}
getStdin :: MonadIO m => m (SerialT m (Array Word8))
getStdin = liftIO $ readIORef stdinM

-- XXX Need locking for multithreaded access
withStdin :: MonadIO m =>
    (SerialT m (Array Word8) -> m (a, SerialT m (Array Word8)))
    -> m a
withStdin f = do
    r <- getStdin
    (res, str) <- f r
    liftIO $ writeIORef stdinM str
    return res

{-
-- stdin as a stream of chunks.
-- We can concat it if we want to read a stream of chars
-- However, after concating how do we go back to reading it as a stream of
-- chunks if we want to do so?

-- | Read a byte array from standard input.
--
-- >>> getChunk n = Handle.getChunk n stdin
--
-- /Pre-release/
--
{-# INLINE getChunk #-}
getChunk :: MonadIO m => Int -> m (Array Word8)
getChunk n = Handle.getChunk n IO.stdin
-}

-- XXX We could use a global mutable ref for stdin to avoid passing it around
-- in all the routines below.
type Stdin m = SerialT m (Array Word8)

-- | Read a character from console.
--
-- >>> getC = liftIO getChar
--
-- /Pre-release/
--
{-# INLINE getChr #-}
getChr :: (MonadIO m, MonadCatch m) =>
    SerialT m (Array Word8) -> m (Char, SerialT m (Array Word8))
getChr =
    ArrStream.parseBreak
        (Unicode.parseCharUtf8With Unicode.ErrorOnCodingFailure)

{-# INLINE getChrM #-}
getChrM :: (MonadIO m, MonadCatch m) => m Char
getChrM = withStdin getChr

{-
{-# INLINE getLStr #-}
getLStr :: MonadIO m => m String
getLStr = liftIO getLine
-}

-- XXX It may be more efficient if the handle returns a raw line Array Word8 or
-- even an Array Char.
--
-- | Read a line from console.
--
-- /Pre-release/
--
{-
{-# INLINE_EARLY getLn #-}
getLn :: forall m a. (MonadIO m, IsFold m Char a) => m a
getLn =
    liftIO getLine
        >>= (Stream.fold eliminator . (Stream.fromList :: String -> SerialT m Char))

{-# RULES "getLStr" getL = getLStr #-}

-- XXX Instead of unfolding () we should use getBytes

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
read = Handle.read IO.stdin

-- | Read a character stream from Utf8 encoded standard input.
--
-- > readChars = Unicode.decodeUtf8 Stdio.read
--
-- /Pre-release/
--
{-# INLINE readChars #-}
readChars :: MonadIO m => Stream m Char
readChars = Unicode.decodeUtf8 read

-- XXX Instead of unfolding () we should use getChunks

-- | Unfolds standard input into a stream of 'Word8' arrays.
--
{-# INLINE chunkReader #-}
chunkReader :: MonadIO m => Unfold m () (Array Word8)
chunkReader = Unfold.lmap (\() -> IO.stdin) Handle.chunkReader

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
{-
-- This implementation may not work if we want to terminate getLines and then
-- want to read again from the handle. Since we are buffering outside the
-- handle we will lose the buffer on termination of the stream.
--
-- | Read UTF8 encoded lines from standard input.
--
-- You may want to process the input byte stream directly using appropriate
-- folds for more efficient processing.
--
-- /Pre-release/
--
{-# INLINE getLines #-}
getLines :: MonadIO m => Stream m (Array Word8)
getLines = (Stream.splitWithSuffix (== '\n') f) getChars

    -- XXX Need to implement Fold.unfoldMany, should be easy for
    -- non-terminating folds, but may be tricky for terminating folds. See
    -- Array Stream folds.
    where f = Fold.unfoldMany Unicode.readCharUtf8 Array.write
-}

{-# INLINE getLinesStr #-}
getLinesStr :: MonadAsync m => SerialT m String
getLinesStr = Stream.repeatM (liftIO getLine)

-- XXX Instead of using a typeclass use a Fold argument? Like we do for many
-- other operations?
--
-- We can specialize to getStrLn, getArrLn, getUtf8Ln
--
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
    Stream.mapM
        (Stream.fold eliminator . (Stream.fromList :: String -> SerialT m Char))
        (Stream.repeatM (liftIO getLine) :: SerialT m String)

{-# RULES "getLinesStr" getLines = getLinesStr #-}
-}

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
putChunk = Handle.putChunk IO.stdout

{-
-- We can specialize to putStream, putArr, putString
--
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
-}

-- | Fold a stream of 'Word8' to standard output.
--
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ()
write = Handle.write IO.stdout

-- | Fold a stream of 'Word8' to standard error.
--
{-# INLINE writeErr #-}
writeErr :: MonadIO m => Fold m Word8 ()
writeErr = Handle.write IO.stderr

-- | Write a stream of bytes to standard output.
--
-- > putBytes = Handle.putBytes stdout
-- > putBytes = Stream.fold Stdio.write
--
-- /Pre-release/
--
{-# INLINE putBytes #-}
putBytes :: MonadIO m => Stream m Word8 -> m ()
putBytes = Handle.putBytes IO.stdout

{-
-- XXX putCharsWith unfold
--
-- | Encode a character stream to Utf8 and write it to standard output.
--
-- >>> putChars = Stdio.putBytes . Unicode.encodeUtf8 . ToStream.toStream
--
-- /Pre-release/
--
{-# INLINE putChars #-}
putChars :: (MonadIO m, IsUnfold m a Char) => a -> m ()
putChars = putBytes . Unicode.encodeUtf8 . Stream.unfold generator
-}

-- | Fold a stream of @Array Word8@ to standard output.
--
{-# INLINE writeChunks #-}
writeChunks :: MonadIO m => Fold m (Array Word8) ()
writeChunks = Handle.writeChunks IO.stdout

-- | Fold a stream of @Array Word8@ to standard error.
--
{-# INLINE writeErrChunks #-}
writeErrChunks :: MonadIO m => Fold m (Array Word8) ()
writeErrChunks = Handle.writeChunks IO.stderr

-- | Write a stream of chunks to standard output.
--
-- > putChunks = Handle.putChunks stdout
-- > putChunks = Stream.fold Stdio.writeChunks
--
-- /Pre-release/
--
{-# INLINE putChunks #-}
putChunks :: MonadIO m => Stream m (Array Word8) -> m ()
putChunks = Handle.putChunks IO.stdout

-------------------------------------------------------------------------------
-- Line buffered
-------------------------------------------------------------------------------

{-
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
    => (Stream m Char -> Stream m Word8) -> Stream m a -> m ()
putStringsWith encode =
    putChunks . Stream.mapM (Array.fromStream . encode . Stream.unfold generator)

-- | Write a stream of strings to standard output using UTF8 encoding.  Output
-- is flushed to the device for each string.
--
-- >>> putStrings = putStringsWith Unicode.encodeUtf8
--
-- /Pre-release/
--
{-# INLINE putStrings #-}
putStrings :: (MonadIO m, IsUnfold m a Char) => Stream m a -> m ()
putStrings = putStringsWith Unicode.encodeUtf8

-- | Like 'putStrings' but adds a newline at the end of each string.
--
-- XXX This is not portable, on Windows we need to use "\r\n" instead.
--
-- /Pre-release/
--
{-# INLINE putLines #-}
putLines :: (MonadIO m, IsUnfold m a Char) => Stream m a -> m ()
putLines =
      putChunks
    . Stream.intersperseMSuffix (return $ Array.fromList [10])
    . Unicode.encodeStrings Unicode.encodeUtf8
    -}
