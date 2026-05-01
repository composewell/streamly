-- |
-- Module      : Streamly.Internal.Syscall.Windows
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Haskell wrappers on some Windows system calls.

{-# LANGUAGE CPP #-}

module Streamly.Internal.Syscall.Windows
    (
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
      getCwd
#endif
    ) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#else
# define WINDOWS_CCONV ccall
#endif

import System.Win32.Types (DWORD, LPTSTR, UINT)
import qualified System.Win32 as Win32 (failWith)
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import Streamly.Internal.Data.MutByteArray.Type
    (MutByteArray, PinnedState(..), unsafeAsPtr)
import Streamly.Internal.Syscall.Common (retry)

foreign import WINDOWS_CCONV unsafe "GetCurrentDirectoryW"
  c_getCurrentDirectory :: DWORD -> LPTSTR -> IO UINT

foreign import ccall unsafe "windows.h GetLastError"
    c_GetLastError :: IO DWORD

-- GetCurrentDirectoryW returns the number of chars written (not including
-- null) on success, or the required buffer size (including null) if the
-- buffer is too small, or 0 on error.
tryGetCwd :: Int -> IO (Maybe (MutByteArray, Int))
tryGetCwd nchars = do
    arr <- MutByteArray.new' (nchars * 2)  -- UTF-16: 2 bytes per char
    unsafeAsPtr arr $ \ptr -> do
        r <- c_getCurrentDirectory (fromIntegral nchars) ptr
        if r == 0
        then c_GetLastError >>= Win32.failWith "getCwd"
        else if fromIntegral r >= nchars
             then return Nothing
             else return (Just (arr, fromIntegral r))

-- Start with a small buffer, doubling until GetCurrentDirectoryW succeeds.
getCwd :: IO (Array Word8)
getCwd = do
    let resize old = max (old * 2) 4096
    (arr, len) <- retry resize tryGetCwd (260 * 2)
    -- Note that the return value may be pinned or unpinned, users should not
    -- rely on that, if you want guarantee then clone the MBA.
    mba <- MutByteArray.rightSizeAs Unpinned (len * 2) arr
    return (Array mba 0 (fromIntegral (len * 2)))
#endif
