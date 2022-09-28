#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.FDIO
-- Copyright   : (c) 2019 Composewell Technologies
-- Copyright   : (c) 1994-2008 The University of Glasgow
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low level IO routines interfacing the operating system.
--

module Streamly.Internal.FileSystem.FDIO
    ( write
    , writeAll
    , writev
    , writevAll
    )
where

import Control.Monad (when)
import Streamly.Internal.System.IOVec.Type (IOVec)
#if !defined(mingw32_HOST_OS)
import Control.Concurrent (threadWaitWrite)
import Data.Int (Int64)
import Foreign.C.Error (throwErrnoIfMinus1RetryMayBlock)
import Foreign.C.Types (CBool(..))
import System.Posix.Internals (c_write, c_safe_write)
import Streamly.Internal.System.IOVec.Type (c_writev, c_safe_writev)
#endif

import Foreign.C.Types (CSize(..), CInt(..))
import Data.Word (Word8)
import Foreign.Ptr (plusPtr, Ptr)

import GHC.IO.FD (FD(..))

-------------------------------------------------------------------------------
-- IO Routines
-------------------------------------------------------------------------------

-- See System.POSIX.Internals in GHC base package

-------------------------------------------------------------------------------
-- Write without blocking the underlying OS thread
-------------------------------------------------------------------------------

#if !defined(mingw32_HOST_OS)

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

isNonBlocking :: FD -> Bool
isNonBlocking fd = fdIsNonBlocking fd /= 0

-- "poll"s the fd for data to become available or timeout
-- See cbits/inputReady.c in base package
foreign import ccall unsafe "fdReady"
    unsafe_fdReady :: CInt -> CBool -> Int64 -> CBool -> IO CInt

writeNonBlocking :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeNonBlocking loc !fd !buf !off !len
    | isNonBlocking fd = unsafe_write -- unsafe is ok, it can't block
    | otherwise   = do
        let isWrite = 1
            isSocket = 0
            msecs = 0
        r <- unsafe_fdReady (fdFD fd) isWrite msecs isSocket
        when (r == 0) $ threadWaitWrite (fromIntegral (fdFD fd))
        if threaded then safe_write else unsafe_write

    where

    do_write call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock loc call
                        (threadWaitWrite (fromIntegral (fdFD fd)))
    unsafe_write  = do_write (c_write (fdFD fd) (buf `plusPtr` off) len)
    safe_write    = do_write (c_safe_write (fdFD fd) (buf `plusPtr` off) len)

writevNonBlocking :: String -> FD -> Ptr IOVec -> Int -> IO CInt
writevNonBlocking loc !fd !iov !cnt
    | isNonBlocking fd = unsafe_write -- unsafe is ok, it can't block
    | otherwise   = do
        let isWrite = 1
            isSocket = 0
            msecs = 0
        r <- unsafe_fdReady (fdFD fd) isWrite msecs isSocket
        when (r == 0) $ threadWaitWrite (fromIntegral (fdFD fd))
        if threaded then safe_write else unsafe_write

    where

    do_write call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock loc call
                        (threadWaitWrite (fromIntegral (fdFD fd)))
    unsafe_write  = do_write (c_writev (fdFD fd) iov (fromIntegral cnt))
    safe_write    = do_write (c_safe_writev (fdFD fd) iov (fromIntegral cnt))

#else
writeNonBlocking :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeNonBlocking = undefined

writevNonBlocking :: String -> FD -> Ptr IOVec -> Int -> IO CInt
writevNonBlocking = undefined
#endif

-- Windows code is disabled for now
#if 0

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

foreign import WINDOWS_CCONV safe "recv"
   c_safe_recv :: CInt -> Ptr Word8 -> CInt -> CInt{-flags-} -> IO CInt

foreign import WINDOWS_CCONV safe "send"
   c_safe_send :: CInt -> Ptr Word8 -> CInt -> CInt{-flags-} -> IO CInt

blockingWriteRawBufferPtr :: String -> FD -> Ptr Word8-> Int -> CSize -> IO CInt
blockingWriteRawBufferPtr loc !fd !buf !off !len
  = throwErrnoIfMinus1Retry loc $ do
        let start_ptr = buf `plusPtr` off
            send_ret = c_safe_send  (fdFD fd) start_ptr (fromIntegral len) 0
            write_ret = c_safe_write (fdFD fd) start_ptr (fromIntegral len)
        r <- bool write_ret send_ret (fdIsSocket fd)
        when (r == -1) c_maperrno
        return r
      -- We don't trust write() to give us the correct errno, and
      -- instead do the errno conversion from GetLastError()
      -- ourselves. The main reason is that we treat ERROR_NO_DATA
      -- (pipe is closing) as EPIPE, whereas write() returns EINVAL
      -- for this case. We need to detect EPIPE correctly, because it
      -- shouldn't be reported as an error when it happens on stdout.
      -- As for send()'s case, Winsock functions don't do errno
      -- conversion in any case so we have to do it ourselves.
      -- That means we're doing the errno conversion no matter if the
      -- fd is from a socket or not.

-- NOTE: "safe" versions of the read/write calls for use by the threaded RTS.
-- These calls may block, but that's ok.

asyncWriteRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
asyncWriteRawBufferPtr loc !fd !buf !off !len = do
    (l, rc) <- asyncWrite (fromIntegral (fdFD fd)) (fdIsSocket_ fd)
                  (fromIntegral len) (buf `plusPtr` off)
    if l == (-1)
      then let sock_errno = c_maperrno_func (fromIntegral rc)
               non_sock_errno = Errno (fromIntegral rc)
               errno = bool non_sock_errno sock_errno (fdIsSocket fd)
           in  ioError (errnoToIOError loc errno Nothing Nothing)
      else return (fromIntegral l)

writeNonBlocking :: String -> FD -> Ptr Word8 -> Int -> CSize -> IO CInt
writeNonBlocking loc !fd !buf !off !len
    | threaded  = blockingWriteRawBufferPtr loc fd buf off len
    | otherwise = asyncWriteRawBufferPtr    loc fd buf off len

#endif

-- | @write FD buffer offset length@ tries to write data on the given
-- filesystem fd (cannot be a socket) up to sepcified length starting from the
-- given offset in the buffer. The write will not block the OS thread, it may
-- suspend the Haskell thread until write can proceed.  Returns the actual
-- amount of data written.
write :: FD -> Ptr Word8 -> Int -> CSize -> IO CInt
write = writeNonBlocking "Streamly.Internal.FileSystem.FDIO"

-- XXX sendAll for sockets has a similar code, we can deduplicate the two.
-- XXX we need to check the errno to determine if the loop should continue. For
-- example, write may return without writing all data if the process file-size
-- limit has reached, in that case keep writing in a loop is fruitless.
--
-- | Keep writing in a loop until all data in the buffer has been written.
writeAll :: FD -> Ptr Word8 -> Int -> IO ()
writeAll fd ptr bytes = do
    res <- write fd ptr 0 (fromIntegral bytes)
    let res' = fromIntegral res
    when (res' < bytes) $
      writeAll fd (ptr `plusPtr` res') (bytes - res')

-------------------------------------------------------------------------------
-- Vector IO
-------------------------------------------------------------------------------

-- | @write FD iovec count@ tries to write data on the given filesystem fd
-- (cannot be a socket) from an iovec with specified number of entries.  The
-- write will not block the OS thread, it may suspend the Haskell thread until
-- write can proceed.  Returns the actual amount of data written.
writev :: FD -> Ptr IOVec -> Int -> IO CInt
writev = writevNonBlocking "Streamly.Internal.FileSystem.FDIO"

-- XXX incomplete
-- | Keep writing an iovec in a loop until all the iovec entries are written.
writevAll :: FD -> Ptr IOVec -> Int -> IO ()
writevAll fd iovec count = do
    _res <- writev fd iovec count
    {-
    let res' = fromIntegral res
    totalBytes = countIOVecBytes
    if res' < totalBytes
     then do
        let iovec' = createModifiedIOVec
            count' = ...
        writeAll fd iovec' count'
     else return ()
    -}
    return ()
