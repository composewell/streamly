-- |
-- Module      : Streamly.Internal.Console.Stream
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

-- NOTES: Breaking a stream incurs a cost because of CPS and breaking of
-- fusion. For consuming a stream of chunks efficiently, the basic principle
-- is to use element folds on the chunk stream so that we can keep the source
-- as a chunk stream. If we convert the chunk stream into element stream and
-- then break it then  we need to pay the cost of breaking per element rather
-- than per chunk. So we have two options:
--
-- 1. Consume the original chunk stream using element folds. In this case we
-- cannot use stream expansion combinators as we are working on folds and we do
-- not have an efficient/fusible expansion facility in folds, as there is no
-- Skip input facility. We can use the fold composition facilities (fold Monad)
-- to compose folds. We can use foldBreak on the stream and then consume the
-- rest of the stream in an arbitrary way. We can also do the same inside fold
-- monad.
--
-- Basically expansion and merge will have to be supported by folds as well if
-- we want an entire gamut of operations.
--
-- 2. Make the original stream return the remaining stream (Stream a m r). A
-- concatMap or unfoldMany (using a producer type) can be used to expand the
-- stream. When the stream ends we will need to compose the result of the
-- internal stream with the external stream to return the remaining stream.
-- For example, if we are expanding a stream of Array Word8 into a stream of
-- Word8, the inner stream will have a leftover array which will need to be
-- added back to the outer array of streams. In case of parsers, it will be
-- even more complicated because we may even have leftover elements which may
-- have to be converted back to array.
--
-- 3. When combining a stream with a fold we can return the remaining stream if
-- the stream is leftover, or return a fold if it wants more input. So a
-- generalised fold runner would return "Either Stream Fold". Then we can use
-- the base monad to drive the computation incrementally using one chunk at a
-- time. In this case we do not have the problem of breaking the stream as it
-- is already broken. We are using a fundamentally broken-stream/fold paradigm.
--
-- For example, to process a stream of "Array Word8" we uncons one Array from
-- the parent stream. This array is unfolded to a stream of Word8 and we drive
-- a Word8 fold using the resulting stream. The fold would return either an
-- unfinished fold or the leftover stream. If the stream is leftover we can
-- either consume it using another fold or convert it back to an array using an
-- Array write fold.
--
module Streamly.Internal.Console.Stream
    (
    -- * Standard Input
      single
    , byteLine
    , foldBytes
    , line
    , foldChars
    , foldLines

    {-
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
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Prelude hiding (read)

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Array.Foreign.Type (Array(..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import Streamly.Internal.Data.Unfold (Unfold)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Parser (Parser)
-- import Streamly.Internal.Data.Stream.ToStream (IsFold(..), IsUnfold(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Array.Foreign as Array
import qualified Streamly.Internal.Data.Array.Stream.Foreign as ArrStream
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Unicode.Stream as Unicode
import qualified System.IO as IO

-- XXX These are general routines and are not specific to stdin/out. These can
-- go in appropriate general modules.

-- XXX Important we cannot mix these routines with any other IO routines e.g.
-- reading directly using the base System.IO routines.

-------------------------------------------------------------------------------
-- Reads
-------------------------------------------------------------------------------

-- XXX Partial function, should not fail for stdin stream
{-# INLINE single #-}
single :: Monad m => Fold m a a
single = fmap fromJust Fold.one

-- | Consume at most one line.
{-# INLINE byteLine #-}
byteLine :: Monad m => Fold m Word8 b -> Fold m Word8 b
byteLine = Fold.takeEndBy_ (== 10)

-- | Consume bytes from a stream.
--
-- > foldBytes single
-- > foldBytes (byteLine Fold.toList)
--
-- /Pre-release/
--
{-# INLINE foldBytes #-}
foldBytes :: MonadIO m =>
    Fold m Word8 b -> SerialT m (Array Word8) -> m (b, SerialT m (Array Word8))
foldBytes = ArrStream.foldBreak

-- XXX We can write "line" as a scanner or as Fold -> Fold combinator.
{-# INLINE line #-}
line :: Monad m => Fold m Char b -> Fold m Char b
line = Fold.takeEndBy_ (== '\n')

{-# INLINE decodeUtf8 #-}
decodeUtf8 :: MonadThrow m => Fold m Char b -> Fold m Word8 b
decodeUtf8 = Fold.many Unicode.writeCharUtf8'

-- | Consume a character stream from Utf8 encoded bytearray stream.
--
-- > foldChars single
-- > foldChars (line Fold.toList)
--
{-# INLINE foldChars #-}
foldChars :: (MonadIO m, MonadCatch m) =>
    Fold m Char b -> SerialT m (Array Word8) -> m (b, SerialT m (Array Word8))
foldChars f = ArrStream.foldBreak (decodeUtf8 f)

-- XXX Check fusion issues due to nested "many" combinator.

-- | decodeLines lineProducer lineConsumer
{-# INLINE decodeLines #-}
decodeLines :: MonadThrow m => Fold m Char b -> Fold m b c -> Fold m Word8 c
decodeLines ln = Fold.many (Fold.many Unicode.writeCharUtf8' (line ln))

-- | Consume a line stream from Utf8 encoded bytearray stream.
--
-- > foldLines Fold.toList (Fold.take 2 Fold.toList))
--
{-# INLINE foldLines #-}
foldLines :: (MonadIO m, MonadCatch m) =>
       Fold m Char b
    -> Fold m b c
    -> SerialT m (Array Word8)
    -> m (c, SerialT m (Array Word8))
foldLines ln f = ArrStream.foldBreak (decodeLines ln f)

-- | Consume bytearrays from a bytearray stream.
--
-- > foldChunks single
-- > foldChunks (Fold.take 2)
--
-- /Pre-release/
--
{-# INLINE foldChunks #-}
foldChunks :: MonadIO m =>
       Fold m (Array Word8) b
    -> SerialT m (Array Word8)
    -> m (b, SerialT m (Array Word8))
foldChunks = Stream.foldBreak

-------------------------------------------------------------------------------
-- Writes
-------------------------------------------------------------------------------

{-
-- | Write a byte array to standard output.
--
-- >>> putChunk = Handle.putChunk stdout
--
-- /Pre-release/
--
{-# INLINE putChunk #-}
putChunk :: MonadIO m => Array Word8 -> m ()
putChunk = Handle.putChunk IO.stdout

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

-- | Fold a stream of 'Word8' to standard output.
--
-- @since 0.8.0
{-# INLINE write #-}
write :: MonadIO m => Fold m Word8 ()
write = Handle.write IO.stdout

-- | Fold a stream of 'Word8' to standard error.
--
-- @since 0.8.0
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
putBytes :: MonadIO m => SerialT m Word8 -> m ()
putBytes = Handle.putBytes IO.stdout

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
-- @since 0.8.0
{-# INLINE writeChunks #-}
writeChunks :: MonadIO m => Fold m (Array Word8) ()
writeChunks = Handle.writeChunks IO.stdout

{-
-- | Fold a stream of @Array Word8@ to standard error.
--
-- @since 0.8.0
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
putChunks :: MonadIO m => SerialT m (Array Word8) -> m ()
putChunks = Handle.putChunks IO.stdout

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
    -}
