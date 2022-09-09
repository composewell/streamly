#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Network.Socket
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Network.Socket
    (
    SockSpec (..)
    -- * Use a socket
    , forSocketM
    , withSocket

    -- * Accept connections
    , accept
    , connections
    , connect
    , connectFrom

    -- * Read from connection
    , read
    , readWith
    -- , readUtf8
    -- , readLines
    -- , readFrames
    -- , readByChunks

    -- -- * Array Read
    -- , readArrayUpto
    -- , readChunksUpto
    , readChunk
    , readChunks
    , readChunksWith

    , toChunksWith
    , toChunks
    , toBytes

    -- * Write to connection
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    , writeWith
    , writeMaybesWith

    , putChunks
    , putBytesWith
    , putBytes

    -- -- * Array Write
    , writeChunk
    , writeChunks
    , writeChunksWith

    -- reading/writing datagrams

    -- * Deprecated
    , readWithBufferOf
    , readChunksWithBufferOf
    , writeWithBufferOf
    , writeChunksWithBufferOf
    )
where

import Control.Concurrent (threadWaitWrite, rtsSupportsBoundThreads)
import Control.Exception (onException)
import Control.Monad.Catch (MonadCatch, finally, MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forM_, when)
import Data.Maybe (isNothing, fromJust)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr, Ptr, castPtr)
import Streamly.Internal.Data.Unboxed (Unboxed)
import Network.Socket
       (Socket, SocketOption(..), Family(..), SockAddr(..),
        ProtocolNumber, withSocketsDo, SocketType(..), socket, bind,
        setSocketOption, sendBuf, recvBuf)
#if MIN_VERSION_network(3,1,0)
import Network.Socket (withFdSocket)
#else
import Network.Socket (fdSocket)
#endif
import Prelude hiding (read)

import qualified Network.Socket as Net

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Array.Unboxed.Type (Array(..))
import Streamly.Internal.Data.Array.Stream.Foreign (lpackArraysChunksOf)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream, mkStream, fromStreamD)
import Streamly.Internal.Data.Stream.Type (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Data.Array.Unboxed as A (read, length, writeN)
import qualified Streamly.Internal.Data.Array.Unboxed.Type as A
    (unsafeFreeze, asPtrUnsafe, byteLength, writeNUnsafe)
import qualified Streamly.Internal.Data.Array.Unboxed.Mut as MArray
    (Array(..), newPinnedArrayBytes, asPtrUnsafe)
import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
    (Stream(..), Step(..))
import qualified Streamly.Internal.Data.Unfold as UF
    (first, map, many)

-- | @'forSocketM' action socket@ runs the monadic computation @action@ passing
-- the socket handle to it.  The handle will be closed on exit from
-- 'forSocketM', whether by normal termination or by raising an exception.  If
-- closing the handle raises an exception, then this exception will be raised
-- by 'forSocketM' rather than any exception raised by 'action'.
--
-- @since 0.8.0
{-# INLINE forSocketM #-}
forSocketM :: (MonadMask m, MonadIO m) => (Socket -> m ()) -> Socket -> m ()
forSocketM f sk = finally (f sk) (liftIO (Net.close sk))

-- | Like 'forSocketM' but runs a streaming computation instead of a monadic
-- computation.
--
-- /Inhibits stream fusion/
--
-- /Internal/
{-# INLINE withSocket #-}
withSocket :: (IsStream t, MonadAsync m, MonadCatch m)
    => Socket -> (Socket -> t m a) -> t m a
withSocket sk f = S.finally (liftIO $ Net.close sk) (f sk)

-------------------------------------------------------------------------------
-- Accept (Unfolds)
-------------------------------------------------------------------------------

-- XXX Protocol specific socket options should be separated from socket level
-- options.
--
-- | Specify the socket protocol details.
data SockSpec = SockSpec
    {
      sockFamily :: !Family
    , sockType   :: !SocketType
    , sockProto  :: !ProtocolNumber
    , sockOpts   :: ![(SocketOption, Int)]
    }

initListener :: Int -> SockSpec -> SockAddr -> IO Socket
initListener listenQLen SockSpec{..} addr =
  withSocketsDo $ do
    sock <- socket sockFamily sockType sockProto
    use sock `onException` Net.close sock
    return sock

    where

    use sock = do
        mapM_ (\(opt, val) -> setSocketOption sock opt val) sockOpts
        bind sock addr
        Net.listen sock listenQLen

{-# INLINE listenTuples #-}
listenTuples :: MonadIO m
    => Unfold m (Int, SockSpec, SockAddr) (Socket, SockAddr)
listenTuples = Unfold step inject
    where
    inject (listenQLen, spec, addr) =
        liftIO $ initListener listenQLen spec addr

    step listener = do
        r <- liftIO (Net.accept listener `onException` Net.close listener)
        return $ D.Yield r listener

-- | Unfold a three tuple @(listenQLen, spec, addr)@ into a stream of connected
-- protocol sockets corresponding to incoming connections. @listenQLen@ is the
-- maximum number of pending connections in the backlog. @spec@ is the socket
-- protocol and options specification and @addr@ is the protocol address where
-- the server listens for incoming connections.
--
-- @since 0.7.0
{-# INLINE accept #-}
accept :: MonadIO m => Unfold m (Int, SockSpec, SockAddr) Socket
accept = UF.map fst listenTuples

{-# INLINE connectCommon #-}
connectCommon :: SockSpec -> Maybe SockAddr -> SockAddr -> IO Socket
connectCommon SockSpec{..} local remote = withSocketsDo $ do
    sock <- socket sockFamily sockType sockProto
    use sock `onException` Net.close sock
    return sock

    where

    use sock = do
        mapM_ (\(opt, val) -> setSocketOption sock opt val) sockOpts
        forM_ local (bind sock)
        Net.connect sock remote

-- | Connect to a remote host using the given socket specification and remote
-- address. Returns a connected socket or throws an exception.
--
-- /Pre-release/
--
{-# INLINE connect #-}
connect :: SockSpec -> SockAddr -> IO Socket
connect spec = connectCommon spec Nothing

-- | Connect to a remote host using the given socket specification, a local
-- address to bind to and a remote address to connect to. Returns a connected
-- socket or throws an exception.
--
-- /Pre-release/
--
{-# INLINE connectFrom #-}
connectFrom :: SockSpec -> SockAddr -> SockAddr -> IO Socket
connectFrom spec local = connectCommon spec (Just local)

-------------------------------------------------------------------------------
-- Listen (Streams)
-------------------------------------------------------------------------------

{-# INLINE recvConnectionTuplesWith #-}
recvConnectionTuplesWith :: MonadAsync m
    => Int -> SockSpec -> SockAddr -> Stream m (Socket, SockAddr)
recvConnectionTuplesWith tcpListenQ spec addr = S.unfoldrM step Nothing
    where
    step Nothing = do
        listener <- liftIO $ initListener tcpListenQ spec addr
        r <- liftIO (Net.accept listener `onException` Net.close listener)
        return $ Just (r, Just listener)

    step (Just listener) = do
        r <- liftIO (Net.accept listener `onException` Net.close listener)
        return $ Just (r, Just listener)

-- | Start a TCP stream server that listens for connections on the supplied
-- server address specification (address family, local interface IP address and
-- port). The server generates a stream of connected sockets.  The first
-- argument is the maximum number of pending connections in the backlog.
--
-- /Pre-release/
{-# INLINE connections #-}
connections :: MonadAsync m => Int -> SockSpec -> SockAddr -> Stream m Socket
connections tcpListenQ spec addr =
    fst <$> recvConnectionTuplesWith tcpListenQ spec addr

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

{-# INLINABLE readArrayUptoWith #-}
readArrayUptoWith
    :: (h -> Ptr Word8 -> Int -> IO Int)
    -> Int
    -> h
    -> IO (Array Word8)
readArrayUptoWith f size h = do
    arr <- MArray.newPinnedArrayBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    MArray.asPtrUnsafe arr $ \p -> do
        n <- f h p size
        let v = A.unsafeFreeze
                $ arr { MArray.arrEnd = n, MArray.arrBound = size }

        -- XXX shrink only if the diff is significant
        -- A.shrinkToFit v
        return v

-- | Read a byte array from a file handle up to a maximum of the requested
-- size. If no data is available on the handle it blocks until some data
-- becomes available. If data is available then it immediately returns that
-- data without blocking.
--
-- @since 0.8.0
{-# INLINABLE readChunk #-}
readChunk :: Int -> Socket -> IO (Array Word8)
readChunk = readArrayUptoWith recvBuf

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

waitWhen0 :: Int -> Socket -> IO ()
waitWhen0 0 s = when rtsSupportsBoundThreads $
#if MIN_VERSION_network(3,1,0)
    withFdSocket s $ \fd -> threadWaitWrite $ fromIntegral fd
#elif MIN_VERSION_network(3,0,0)
    fdSocket s >>= threadWaitWrite . fromIntegral
#else
    let fd = fdSocket s in threadWaitWrite $ fromIntegral fd
#endif
waitWhen0 _ _ = return ()

sendAll :: Socket -> Ptr Word8 -> Int -> IO ()
sendAll _ _ len | len <= 0 = return ()
sendAll s p len = do
    sent <- sendBuf s p len
    waitWhen0 sent s
    -- assert (sent <= len)
    when (sent >= 0) $ sendAll s (p `plusPtr` sent) (len - sent)

{-# INLINABLE writeArrayWith #-}
writeArrayWith :: Unboxed a
    => (h -> Ptr Word8 -> Int -> IO ())
    -> h
    -> Array a
    -> IO ()
writeArrayWith _ _ arr | A.length arr == 0 = return ()
writeArrayWith f h arr = A.asPtrUnsafe arr $ \ptr -> f h (castPtr ptr) aLen

    where

    aLen = A.byteLength arr

-- | Write an Array to a file handle.
--
-- @since 0.8.0
{-# INLINABLE writeChunk #-}
writeChunk :: Unboxed a => Socket -> Array a -> IO ()
writeChunk = writeArrayWith sendAll

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

{-# INLINE _readChunksUptoWith #-}
_readChunksUptoWith :: (IsStream t, MonadIO m)
    => (Int -> h -> IO (Array Word8))
    -> Int -> h -> t m (Array Word8)
_readChunksUptoWith f size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld _ stp -> do
        arr <- liftIO $ f size h
        if A.length arr == 0
        then stp
        else yld arr go

-- | @toChunksWith size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is limited to @size@.
-- 'fromHandleArraysUpto' ignores the prevailing 'TextEncoding' and 'NewlineMode'
-- on the 'Handle'.
{-# INLINE_NORMAL toChunksWith #-}
toChunksWith :: (IsStream t, MonadIO m)
    => Int -> Socket -> t m (Array Word8)
-- toChunksWith = _readChunksUptoWith readChunk
toChunksWith size h = fromStreamD (D.Stream step ())
    where
    {-# INLINE_LATE step #-}
    step _ _ = do
        arr <- liftIO $ readChunk size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr ()

-- XXX read 'Array a' instead of Word8
--
-- | @toChunks h@ reads a stream of arrays from socket handle @h@.
-- The maximum size of a single array is limited to @defaultChunkSize@.
--
-- @since 0.7.0
{-# INLINE toChunks #-}
toChunks :: (IsStream t, MonadIO m) => Socket -> t m (Array Word8)
toChunks = toChunksWith defaultChunkSize

-- | Unfold the tuple @(bufsize, socket)@ into a stream of 'Word8' arrays.
-- Read requests to the socket are performed using a buffer of size @bufsize@.
-- The size of an array in the resulting stream is always less than or equal to
-- @bufsize@.
--
-- @since 0.9.0
{-# INLINE_NORMAL readChunksWith #-}
readChunksWith :: MonadIO m => Unfold m (Int, Socket) (Array Word8)
readChunksWith = Unfold step return
    where
    {-# INLINE_LATE step #-}
    step (size, h) = do
        arr <- liftIO $ readChunk size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr (size, h)

-- | Same as 'readChunksWith'
--
-- @since 0.7.0
{-# DEPRECATED readChunksWithBufferOf "Please use 'readChunksWith' instead" #-}
{-# INLINE_NORMAL readChunksWithBufferOf #-}
readChunksWithBufferOf :: MonadIO m => Unfold m (Int, Socket) (Array Word8)
readChunksWithBufferOf = readChunksWith

-- | Unfolds a socket into a stream of 'Word8' arrays. Requests to the socket
-- are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize'.
--
-- @since 0.7.0
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m Socket (Array Word8)
readChunks = UF.first defaultChunkSize readChunksWith

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

{-
-- | @readWith bufsize handle@ reads a byte stream from a file
-- handle, reads are performed in chunks of up to @bufsize@.  The stream ends
-- as soon as EOF is encountered.
--
{-# INLINE readWith #-}
readWith :: (IsStream t, MonadIO m) => Int -> Handle -> t m Word8
readWith chunkSize h = A.flattenArrays $ readChunksUpto chunkSize h
-}

-- TODO
-- read :: (IsStream t, MonadIO m, Unboxed a) => Handle -> t m a
--
-- > read = 'readByChunks' defaultChunkSize
-- | Generate a stream of elements of the given type from a socket. The
-- stream ends when EOF is encountered.
--
-- @since 0.7.0
{-# INLINE toBytes #-}
toBytes :: (IsStream t, MonadIO m) => Socket -> t m Word8
toBytes = AS.concat . toChunks

-- | Unfolds the tuple @(bufsize, socket)@ into a byte stream, read requests
-- to the socket are performed using buffers of @bufsize@.
--
-- @since 0.9.0
{-# INLINE readWith #-}
readWith :: MonadIO m => Unfold m (Int, Socket) Word8
readWith = UF.many A.read readChunksWith

-- | Same as 'readWith'
--
-- @since 0.7.0
{-# DEPRECATED readWithBufferOf "Please use 'readWith' instead" #-}
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: MonadIO m => Unfold m (Int, Socket) Word8
readWithBufferOf = readWith

-- | Unfolds a 'Socket' into a byte stream.  IO requests to the socket are
-- performed in sizes of
-- 'Streamly.Internal.Data.Array.Unboxed.Type.defaultChunkSize'.
--
-- @since 0.7.0
{-# INLINE read #-}
read :: MonadIO m => Unfold m Socket Word8
read = UF.first defaultChunkSize readWith

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to a handle.
--
-- @since 0.7.0
{-# INLINE putChunks #-}
putChunks :: (MonadIO m, Unboxed a)
    => Socket -> Stream m (Array a) -> m ()
putChunks h = S.mapM_ (liftIO . writeChunk h)

-- | Write a stream of arrays to a socket.  Each array in the stream is written
-- to the socket as a separate IO request.
--
-- @since 0.7.0
{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, Unboxed a) => Socket -> Fold m (Array a) ()
writeChunks h = FL.drainBy (liftIO . writeChunk h)

-- | @writeChunksWith bufsize socket@ writes a stream of arrays to
-- @socket@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- Multiple arrays are coalesed as long as the total size remains below the
-- specified size.  It never splits an array, if a single array is bigger than
-- the specified size it emitted as it is.
--
-- @since 0.9.0
{-# INLINE writeChunksWith #-}
writeChunksWith :: (MonadIO m, Unboxed a)
    => Int -> Socket -> Fold m (Array a) ()
writeChunksWith n h = lpackArraysChunksOf n (writeChunks h)

-- | Same as 'writeChunksWith'
--
-- @since 0.7.0
{-# DEPRECATED writeChunksWithBufferOf "Please use 'writeChunksWith' instead" #-}
{-# INLINE writeChunksWithBufferOf #-}
writeChunksWithBufferOf :: (MonadIO m, Unboxed a)
    => Int -> Socket -> Fold m (Array a) ()
writeChunksWithBufferOf = writeChunksWith

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- | Like 'write' but provides control over the write buffer. Output will
-- be written to the IO device as soon as we collect the specified number of
-- input elements.
--
-- @since 0.9.0
{-# INLINE putBytesWith #-}
putBytesWith :: MonadIO m => Int -> Socket -> Stream m Word8 -> m ()
putBytesWith n h m = putChunks h $ AS.arraysOf n m

-- | Write a byte stream to a socket. Accumulates the input in chunks of
-- specified number of bytes before writing.
--
-- @since 0.9.0
{-# INLINE writeWith #-}
writeWith :: MonadIO m => Int -> Socket -> Fold m Word8 ()
writeWith n h = FL.chunksOf n (A.writeNUnsafe n) (writeChunks h)

-- | Same as 'writeWith'
--
-- @since 0.7.0
{-# DEPRECATED writeWithBufferOf "Please use 'writeWith' instead" #-}
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: MonadIO m => Int -> Socket -> Fold m Word8 ()
writeWithBufferOf = writeWith

-- | Write a stream of 'Maybe' values. Keep buffering the 'Just' values in an
-- array. Write the array to the 'Handle' as soon as a 'Nothing' is encountered
-- or the buffer size exceeds the specified limit.
--
-- /Pre-release/
{-# INLINE writeMaybesWith #-}
writeMaybesWith :: (MonadIO m )
    => Int -> Socket -> Fold m (Maybe Word8) ()
writeMaybesWith n h =
    let writeNJusts = FL.lmap fromJust $ A.writeN n
        writeOnNothing = FL.takeEndBy_ isNothing writeNJusts
    in FL.many writeOnNothing (writeChunks h)

-- > write = 'writeWith' defaultChunkSize
--
-- | Write a byte stream to a file handle. Combines the bytes in chunks of size
-- up to 'defaultChunkSize' before writing.  Note that the write behavior
-- depends on the 'IOMode' and the current seek position of the handle.
--
-- @since 0.7.0
{-# INLINE putBytes #-}
putBytes :: MonadIO m => Socket -> Stream m Word8 -> m ()
putBytes = putBytesWith defaultChunkSize

-- | Write a byte stream to a socket. Accumulates the input in chunks of
-- up to 'defaultChunkSize' bytes before writing.
--
-- @
-- write = 'writeWith' 'defaultChunkSize'
-- @
--
-- @since 0.7.0
{-# INLINE write #-}
write :: MonadIO m => Socket -> Fold m Word8 ()
write = writeWith defaultChunkSize

{-
{-# INLINE write #-}
write :: (MonadIO m, Unboxed a) => Handle -> Stream m a -> m ()
write = toHandleWith defaultChunkSize
-}

-------------------------------------------------------------------------------
-- IO with encoding/decoding Unicode characters
-------------------------------------------------------------------------------

{-
-- |
-- > readUtf8 = decodeUtf8 . read
--
-- Read a UTF8 encoded stream of unicode characters from a file handle.
--
-- @since 0.7.0
{-# INLINE readUtf8 #-}
readUtf8 :: (IsStream t, MonadIO m) => Handle -> t m Char
readUtf8 = decodeUtf8 . read

-- |
-- > writeUtf8 h s = write h $ encodeUtf8 s
--
-- Encode a stream of unicode characters to UTF8 and write it to the given file
-- handle. Default block buffering applies to the writes.
--
-- @since 0.7.0
{-# INLINE writeUtf8 #-}
writeUtf8 :: MonadIO m => Handle -> Stream m Char -> m ()
writeUtf8 h s = write h $ encodeUtf8 s

-- | Write a stream of unicode characters after encoding to UTF-8 in chunks
-- separated by a linefeed character @'\n'@. If the size of the buffer exceeds
-- @defaultChunkSize@ and a linefeed is not yet found, the buffer is written
-- anyway.  This is similar to writing to a 'Handle' with the 'LineBuffering'
-- option.
--
-- @since 0.7.0
{-# INLINE writeUtf8ByLines #-}
writeUtf8ByLines :: (IsStream t, MonadIO m) => Handle -> t m Char -> m ()
writeUtf8ByLines = undefined

-- | Read UTF-8 lines from a file handle and apply the specified fold to each
-- line. This is similar to reading a 'Handle' with the 'LineBuffering' option.
--
-- @since 0.7.0
{-# INLINE readLines #-}
readLines :: (IsStream t, MonadIO m) => Handle -> Fold m Char b -> t m b
readLines h f = foldLines (readUtf8 h) f

-------------------------------------------------------------------------------
-- Framing on a sequence
-------------------------------------------------------------------------------

-- | Read a stream from a file handle and split it into frames delimited by
-- the specified sequence of elements. The supplied fold is applied on each
-- frame.
--
-- @since 0.7.0
{-# INLINE readFrames #-}
readFrames :: (IsStream t, MonadIO m, Unboxed a)
    => Array a -> Handle -> Fold m a b -> t m b
readFrames = undefined -- foldFrames . read

-- | Write a stream to the given file handle buffering up to frames separated
-- by the given sequence or up to a maximum of @defaultChunkSize@.
--
-- @since 0.7.0
{-# INLINE writeByFrames #-}
writeByFrames :: (IsStream t, MonadIO m, Unboxed a)
    => Array a -> Handle -> t m a -> m ()
writeByFrames = undefined
-}
