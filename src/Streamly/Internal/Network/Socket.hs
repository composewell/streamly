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
    , acceptor

    -- * Connect
    , connect
    , connectFrom

    -- * Read from connection
    , getChunk

    -- ** Streams
    , read
    , readWith
    , readChunks
    , readChunksWith

    -- ** Unfolds
    , reader
    , readerWith
    , chunkReader
    , chunkReaderWith

    -- * Write to connection
    , putChunk

    -- ** Folds
    , write
    , writeWith
    , writeChunks
    , writeChunksWith
    , writeMaybesWith

    -- ** Stream writes
    , putChunks
    , putBytesWith
    , putBytes

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
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadCatch, finally, MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (isNothing, fromJust)
import Data.Word (Word8)
import Foreign.Ptr (plusPtr, Ptr, castPtr)
import Streamly.Data.MutByteArray (Unbox)
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

import Streamly.Internal.Data.Array (Array(..))
import Streamly.Data.Fold (Fold)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold (Unfold(..))
-- import Streamly.String (encodeUtf8, decodeUtf8, foldLines)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Data.Array as A
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Array as A
    ( unsafeFreeze, unsafePinnedAsPtr, byteLength, pinnedChunksOf,
      pinnedCreateOf, unsafePinnedCreateOf, lCompactGE )
import qualified Streamly.Internal.Data.MutArray as MArray
    (MutArray(..), unsafePinnedAsPtr, pinnedEmptyOf)
import qualified Streamly.Internal.Data.Stream as S (fromStreamK, Stream(..), Step(..))
import qualified Streamly.Internal.Data.StreamK as K (mkStream)

-- $setup
-- >>> :m
-- >>> import Streamly.Internal.System.IO (defaultChunkSize)
-- >>> import qualified Streamly.Internal.Network.Socket as Socket

-- | @'forSocketM' action socket@ runs the monadic computation @action@ passing
-- the socket handle to it.  The handle will be closed on exit from
-- 'forSocketM', whether by normal termination or by raising an exception.  If
-- closing the handle raises an exception, then this exception will be raised
-- by 'forSocketM' rather than any exception raised by 'action'.
--
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
withSocket :: (MonadIO m, MonadCatch m) =>
    Socket -> (Socket -> Stream m a) -> Stream m a
withSocket sk f = S.finallyIO (Net.close sk) (f sk)

-------------------------------------------------------------------------------
-- Accept (Unfolds)
-------------------------------------------------------------------------------

-- XXX Protocol specific socket options should be separated from socket level
-- options.
--
-- NOTE: the socket config is specified as a record and not by composing
-- functions because all the fields are mandatory except the sockOpts field.

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
        return $ S.Yield r listener

-- | Unfold a three tuple @(listenQLen, spec, addr)@ into a stream of connected
-- protocol sockets corresponding to incoming connections. @listenQLen@ is the
-- maximum number of pending connections in the backlog. @spec@ is the socket
-- protocol and options specification and @addr@ is the protocol address where
-- the server listens for incoming connections.
--
{-# INLINE acceptor #-}
acceptor :: MonadIO m => Unfold m (Int, SockSpec, SockAddr) Socket
acceptor = fmap fst listenTuples

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
recvConnectionTuplesWith :: MonadIO m
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
{-# INLINE accept #-}
accept :: MonadIO m => Int -> SockSpec -> SockAddr -> Stream m Socket
accept tcpListenQ spec addr =
    fst <$> recvConnectionTuplesWith tcpListenQ spec addr

-------------------------------------------------------------------------------
-- Array IO (Input)
-------------------------------------------------------------------------------

-- XXX add an API that compacts the arrays to an exact size.

{-# INLINABLE readArrayUptoWith #-}
readArrayUptoWith
    :: (h -> Ptr Word8 -> Int -> IO Int)
    -> Int
    -> h
    -> IO (Array Word8)
readArrayUptoWith f size h = do
    arr <- MArray.pinnedEmptyOf size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    MArray.unsafePinnedAsPtr arr $ \p -> do
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
{-# INLINABLE getChunk #-}
getChunk :: Int -> Socket -> IO (Array Word8)
getChunk = readArrayUptoWith recvBuf

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
writeArrayWith :: Unbox a
    => (h -> Ptr Word8 -> Int -> IO ())
    -> h
    -> Array a
    -> IO ()
writeArrayWith _ _ arr | A.length arr == 0 = return ()
writeArrayWith f h arr = A.unsafePinnedAsPtr arr $ \ptr -> f h (castPtr ptr) aLen

    where

    aLen = A.byteLength arr

-- | Write an Array to a socket.
--
{-# INLINABLE putChunk #-}
putChunk :: Unbox a => Socket -> Array a -> IO ()
putChunk = writeArrayWith sendAll

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

{-# INLINE _readChunksUptoWith #-}
_readChunksUptoWith :: (MonadIO m)
    => (Int -> h -> IO (Array Word8))
    -> Int -> h -> Stream m (Array Word8)
_readChunksUptoWith f size h = S.fromStreamK go
  where
    -- XXX use cons/nil instead
    go = K.mkStream $ \_ yld _ stp -> do
        arr <- liftIO $ f size h
        if A.length arr == 0
        then stp
        else yld arr go

-- | @readChunksWith bufsize socket@ reads a stream of arrays from @socket@.
-- The maximum size of a single array is limited to @bufsize@.
--
-- /Pre-release/
{-# INLINE_NORMAL readChunksWith #-}
readChunksWith :: MonadIO m => Int -> Socket -> Stream m (Array Word8)
-- readChunksWith = _readChunksUptoWith readChunk
readChunksWith size h = S.Stream step ()
    where
    {-# INLINE_LATE step #-}
    step _ _ = do
        arr <- liftIO $ getChunk size h
        return $
            case A.length arr of
                0 -> S.Stop
                _ -> S.Yield arr ()

-- | Read a stream of byte arrays from a socket. The maximum size of a single
-- array is limited to @defaultChunkSize@.
--
-- >>> readChunks = Socket.readChunksWith defaultChunkSize
--
-- /Pre-release/
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Socket -> Stream m (Array Word8)
readChunks = readChunksWith defaultChunkSize

-- | Unfold the tuple @(bufsize, socket)@ into a stream of 'Word8' arrays.
-- Read requests to the socket are performed using a buffer of size @bufsize@.
-- The size of an array in the resulting stream is always less than or equal to
-- @bufsize@.
--
{-# INLINE_NORMAL chunkReaderWith #-}
chunkReaderWith :: MonadIO m => Unfold m (Int, Socket) (Array Word8)
chunkReaderWith = Unfold step return
    where
    {-# INLINE_LATE step #-}
    step (size, h) = do
        arr <- liftIO $ getChunk size h
        return $
            case A.length arr of
                0 -> S.Stop
                _ -> S.Yield arr (size, h)

-- | Same as 'chunkReaderWith'
--
{-# DEPRECATED readChunksWithBufferOf "Please use 'chunkReaderWith' instead" #-}
{-# INLINE_NORMAL readChunksWithBufferOf #-}
readChunksWithBufferOf :: MonadIO m => Unfold m (Int, Socket) (Array Word8)
readChunksWithBufferOf = chunkReaderWith

-- | Unfolds a socket into a stream of 'Word8' arrays. Requests to the socket
-- are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.
--
{-# INLINE chunkReader #-}
chunkReader :: MonadIO m => Unfold m Socket (Array Word8)
chunkReader = UF.first defaultChunkSize chunkReaderWith

-------------------------------------------------------------------------------
-- Read File to Stream
-------------------------------------------------------------------------------

{-# INLINE concatChunks #-}
concatChunks :: (Monad m, Unbox a) => Stream m (Array a) -> Stream m a
concatChunks = S.unfoldMany A.reader

-- | Generate a byte stream from a socket using a buffer of the given size.
--
-- /Pre-release/
{-# INLINE readWith #-}
readWith :: MonadIO m => Int -> Socket -> Stream m Word8
readWith size = concatChunks . readChunksWith size

-- | Generate a byte stream from a socket.
--
-- >>> read = Socket.readWith defaultChunkSize
--
-- /Pre-release/
{-# INLINE read #-}
read :: MonadIO m => Socket -> Stream m Word8
read = readWith defaultChunkSize

-- | Unfolds the tuple @(bufsize, socket)@ into a byte stream, read requests
-- to the socket are performed using buffers of @bufsize@.
--
{-# INLINE readerWith #-}
readerWith :: MonadIO m => Unfold m (Int, Socket) Word8
readerWith = UF.many A.reader chunkReaderWith

-- | Same as 'readWith'
--
{-# DEPRECATED readWithBufferOf "Please use 'readerWith' instead" #-}
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: MonadIO m => Unfold m (Int, Socket) Word8
readWithBufferOf = readerWith

-- | Unfolds a 'Socket' into a byte stream.  IO requests to the socket are
-- performed in sizes of
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.
--
{-# INLINE reader #-}
reader :: MonadIO m => Unfold m Socket Word8
reader = UF.first defaultChunkSize readerWith

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to a handle.
--
{-# INLINE putChunks #-}
putChunks :: (MonadIO m, Unbox a)
    => Socket -> Stream m (Array a) -> m ()
putChunks h = S.fold (FL.drainMapM (liftIO . putChunk h))

-- | Write a stream of arrays to a socket.  Each array in the stream is written
-- to the socket as a separate IO request.
--
{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, Unbox a) => Socket -> Fold m (Array a) ()
writeChunks h = FL.drainMapM (liftIO . putChunk h)

-- | @writeChunksWith bufsize socket@ writes a stream of arrays to
-- @socket@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- Multiple arrays are coalesed as long as the total size remains below the
-- specified size.  It never splits an array, if a single array is bigger than
-- the specified size it emitted as it is.
--
{-# INLINE writeChunksWith #-}
writeChunksWith :: (MonadIO m, Unbox a)
    => Int -> Socket -> Fold m (Array a) ()
writeChunksWith n h = A.lCompactGE n (writeChunks h)

-- | Same as 'writeChunksWith'
--
{-# DEPRECATED writeChunksWithBufferOf "Please use 'writeChunksWith' instead" #-}
{-# INLINE writeChunksWithBufferOf #-}
writeChunksWithBufferOf :: (MonadIO m, Unbox a)
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
{-# INLINE putBytesWith #-}
putBytesWith :: MonadIO m => Int -> Socket -> Stream m Word8 -> m ()
putBytesWith n h m = putChunks h $ A.pinnedChunksOf n m

-- | Write a byte stream to a socket. Accumulates the input in chunks of
-- specified number of bytes before writing.
--
{-# INLINE writeWith #-}
writeWith :: MonadIO m => Int -> Socket -> Fold m Word8 ()
writeWith n h = FL.groupsOf n (A.unsafePinnedCreateOf n) (writeChunks h)

-- | Same as 'writeWith'
--
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
    let writeNJusts = FL.lmap fromJust $ A.pinnedCreateOf n
        writeOnNothing = FL.takeEndBy_ isNothing writeNJusts
    in FL.many writeOnNothing (writeChunks h)

-- | Write a byte stream to a file handle. Combines the bytes in chunks of size
-- up to 'defaultChunkSize' before writing.  Note that the write behavior
-- depends on the 'IOMode' and the current seek position of the handle.
--
{-# INLINE putBytes #-}
putBytes :: MonadIO m => Socket -> Stream m Word8 -> m ()
putBytes = putBytesWith defaultChunkSize

-- | Write a byte stream to a socket. Accumulates the input in chunks of
-- up to 'defaultChunkSize' bytes before writing.
--
-- >>> write = Socket.writeWith defaultChunkSize
--
{-# INLINE write #-}
write :: MonadIO m => Socket -> Fold m Word8 ()
write = writeWith defaultChunkSize
