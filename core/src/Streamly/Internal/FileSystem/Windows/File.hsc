-- XXX When introducing platform specifc API, see Posix/File.hsc and design in
-- the same consistent way.
module Streamly.Internal.FileSystem.Windows.File
    (
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    -- * Handle based
      openFile
    , withFile
    -- , openBinaryFile
    -- , withBinaryFile
#endif
    ) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Exception (onException)
import Control.Monad (when, void)
import Streamly.Internal.FileSystem.WindowsPath (WindowsPath)
import System.IO (IOMode(..), Handle)

#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem
#else
import GHC.IO.Handle.FD (fdToHandle')
#include <fcntl.h>
#endif

import qualified Streamly.Internal.FileSystem.File.Common as File
import qualified Streamly.Internal.FileSystem.WindowsPath as Path

import Data.Bits
import Foreign.Ptr
import System.Win32 as Win32 hiding (createFile, failIfWithRetry)

#include <windows.h>

-------------------------------------------------------------------------------
-- Low level (fd returning) file opening APIs
-------------------------------------------------------------------------------

-- XXX Note for i386, stdcall is needed instead of ccall, see Win32
-- package/windows_cconv.h. We support only x86_64 for now.
foreign import ccall unsafe "windows.h CreateFileW"
  c_CreateFile :: LPCTSTR -> AccessMode -> ShareMode -> LPSECURITY_ATTRIBUTES -> CreateMode -> FileAttributeOrFlag -> HANDLE -> IO HANDLE

-- | like failIf, but retried on sharing violations. This is necessary for many
-- file operations; see
-- https://www.betaarchive.com/wiki/index.php/Microsoft_KB_Archive/316609
--
failIfWithRetry :: (a -> Bool) -> String -> IO a -> IO a
failIfWithRetry needRetry msg action = retryOrFail retries

    where

    delay = 100 * 1000 -- 100 ms

    -- KB article recommends 250/5
    retries = 20 :: Int

    -- retryOrFail :: Int -> IO a
    retryOrFail times
        | times <= 0 = errorWin msg
        | otherwise  = do
            ret <- action
            if not (needRetry ret)
            then return ret
            else do
                err_code <- getLastError
                if err_code == 32
                then do
                    threadDelay delay
                    retryOrFail (times - 1)
                else errorWin msg

createFile ::
       WindowsPath
    -> AccessMode
    -> ShareMode
    -> Maybe LPSECURITY_ATTRIBUTES
    -> CreateMode
    -> FileAttributeOrFlag
    -> Maybe Win32.HANDLE
    -> IO Win32.HANDLE
createFile name access share mb_attr mode flag mb_h =
  Path.asCWString name $ \c_name ->
      failIfWithRetry
        (== iNVALID_HANDLE_VALUE)
        (unwords ["CreateFile", Path.toString name])
        $ c_CreateFile
            c_name access share (maybePtr mb_attr) mode flag (maybePtr mb_h)

win2HsHandle :: WindowsPath -> IOMode -> Win32.HANDLE -> IO Handle
win2HsHandle _fp _iomode h = do
#if defined(__IO_MANAGER_WINIO__)
    Win32.hANDLEToHandle h
#else
    fd <- _open_osfhandle (fromIntegral (ptrToIntPtr h)) (#const _O_BINARY)
    fdToHandle' fd Nothing False (Path.toString _fp) _iomode True
#endif

fdToHandle :: WindowsPath -> IOMode -> Win32.HANDLE -> IO Handle
fdToHandle fp iomode h =
    win2HsHandle fp iomode h `onException` Win32.closeHandle h

openFileFd :: Bool -> WindowsPath -> IOMode -> IO Win32.HANDLE
openFileFd existing fp iomode = do
    h <- createFile
          fp
          accessMode
          shareMode
          Nothing
          (if existing then createModeExisting else createMode)
          fileAttr
          Nothing
    when (iomode == AppendMode )
        $ void $ Win32.setFilePointerEx h 0 Win32.fILE_END
    return h

    where

    accessMode =
        case iomode of
            ReadMode      -> Win32.gENERIC_READ
            WriteMode     -> Win32.gENERIC_WRITE
            AppendMode    -> Win32.gENERIC_WRITE .|. Win32.fILE_APPEND_DATA
            ReadWriteMode -> Win32.gENERIC_READ .|. Win32.gENERIC_WRITE

    writeShareMode :: ShareMode
    writeShareMode =
          Win32.fILE_SHARE_DELETE
      .|. Win32.fILE_SHARE_READ

    maxShareMode :: ShareMode
    maxShareMode =
          Win32.fILE_SHARE_DELETE
      .|. Win32.fILE_SHARE_READ
      .|. Win32.fILE_SHARE_WRITE

    shareMode =
        case iomode of
            ReadMode      -> Win32.fILE_SHARE_READ
            WriteMode     -> writeShareMode
            AppendMode    -> writeShareMode
            ReadWriteMode -> maxShareMode

    createMode =
        case iomode of
            ReadMode      -> Win32.oPEN_EXISTING
            WriteMode     -> Win32.cREATE_ALWAYS
            AppendMode    -> Win32.oPEN_ALWAYS
            ReadWriteMode -> Win32.oPEN_ALWAYS

    createModeExisting =
        case iomode of
            ReadMode      -> Win32.oPEN_EXISTING
            WriteMode     -> Win32.tRUNCATE_EXISTING
            AppendMode    -> Win32.oPEN_EXISTING
            ReadWriteMode -> Win32.oPEN_EXISTING

    fileAttr =
#if defined(__IO_MANAGER_WINIO__)
      (case ioSubSystem of
        IoPOSIX -> Win32.fILE_ATTRIBUTE_NORMAL
        IoNative -> Win32.fILE_ATTRIBUTE_NORMAL .|. Win32.fILE_FLAG_OVERLAPPED
      )
#else
      Win32.fILE_ATTRIBUTE_NORMAL
#endif

-------------------------------------------------------------------------------
-- base openFile compatible, Handle returning, APIs
-------------------------------------------------------------------------------

-- | Open a regular file, return a Handle. The file is locked, the Handle is
-- NOT set up to close the file on garbage collection.
{-# INLINE openFileHandle #-}
openFileHandle :: WindowsPath -> IOMode -> IO Handle
openFileHandle p x = openFileFd False p x >>= fdToHandle p x

-- | Like withFile in base package but using Path instead of FilePath.
-- Use hSetBinaryMode on the handle if you want to use binary mode.
withFile :: WindowsPath -> IOMode -> (Handle -> IO r) -> IO r
withFile = File.withFile False openFileHandle

-- | Like openFile in base package but using Path instead of FilePath.
-- Use hSetBinaryMode on the handle if you want to use binary mode.
openFile :: WindowsPath -> IOMode -> IO Handle
openFile = File.openFile False openFileHandle

{-
-- | Like withBinaryFile in base package but using Path instead of FilePath.
withBinaryFile :: WindowsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = File.withFile True openFileHandle

-- | Like openBinaryFile in base package but using Path instead of FilePath.
openBinaryFile :: WindowsPath -> IOMode -> IO Handle
openBinaryFile = File.openFile True openFileHandle
-}
#endif
