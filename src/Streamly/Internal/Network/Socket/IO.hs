{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Network.Socket.IO
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Network.Socket.IO
    (
    -- * Use a socket
      handleWithM
    , readArrayOf
    , writeChunk
    )
where

import Control.Concurrent (threadWaitWrite, rtsSupportsBoundThreads)
import Control.Monad.Catch (finally, MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr, plusPtr, Ptr, castPtr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Network.Socket (Socket, sendBuf, recvBuf)
#if MIN_VERSION_network(3,1,0)
import Network.Socket (withFdSocket)
#else
import Network.Socket (fdSocket)
#endif
import Prelude hiding (read)

import qualified Network.Socket as Net

import Streamly.Internal.Memory.Array.Types (Array(..))

import qualified Streamly.Memory.Array as A

-- | @'handleWithM' socket act@ runs the monadic computation @act@ passing the
-- socket handle to it.  The handle will be closed on exit from 'handleWithM',
-- whether by normal termination or by raising an exception.  If closing the
-- handle raises an exception, then this exception will be raised by
-- 'handleWithM' rather than any exception raised by 'act'.
--
-- @since 0.7.0
{-# INLINE handleWithM #-}
handleWithM :: (MonadMask m, MonadIO m) => (Socket -> m ()) -> Socket -> m ()
handleWithM f sk = finally (f sk) (liftIO (Net.close sk))

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
    ptr <- mallocPlainForeignPtrBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    withForeignPtr ptr $ \p -> do
        n <- f h p size
        let v = Array
                { aStart = ptr
                , aEnd   = p `plusPtr` n
                , aBound = p `plusPtr` size
                }
        -- XXX shrink only if the diff is significant
        -- A.shrinkToFit v
        return v

-- | Read a 'ByteArray' from a file handle. If no data is available on the
-- handle it blocks until some data becomes available. If data is available
-- then it immediately returns that data without blocking. It reads a maximum
-- of up to the size requested.
{-# INLINABLE readArrayOf #-}
readArrayOf :: Int -> Socket -> IO (Array Word8)
readArrayOf = readArrayUptoWith recvBuf

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
writeArrayWith :: Storable a
    => (h -> Ptr Word8 -> Int -> IO ())
    -> h
    -> Array a
    -> IO ()
writeArrayWith _ _ arr | A.length arr == 0 = return ()
writeArrayWith f h Array{..} = withForeignPtr aStart $ \p ->
    f h (castPtr p) aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in aEnd `minusPtr` p

-- | Write an Array to a file handle.
--
-- @since 0.7.0
{-# INLINABLE writeChunk #-}
writeChunk :: Storable a => Socket -> Array a -> IO ()
writeChunk = writeArrayWith sendAll
