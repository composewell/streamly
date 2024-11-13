module Streamly.Internal.FileSystem.Windows.File
    (
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    -- openFile
    createFile
#endif
    ) where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (bracketOnError, try, SomeException, onException)
import Control.Monad (when, void)
import Control.Monad.Concurrent (threadDelay)
import Streamly.Internal.FileSystem.WindowsPath (WindowsPath)
import System.IO (IOMode(..), Handle)

#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem
#else
import GHC.IO.Handle.FD (fdToHandle')
#include <fcntl.h>
#endif

import qualified Streamly.Internal.FileSystem.WindowsPath as Path
import qualified Streamly.Internal.Data.Array as Array

import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.Win32 as Win32 hiding (createFile, failIfWithRetry)
import System.Win32.Types

#include <windows.h>

-------------------------------------------------------------------------------
-- Windows
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

withFilePath :: WindowsPath -> (LPTSTR -> IO a) -> IO a
withFilePath p act =
    Array.unsafePinnedAsPtr (Path.toChunk p) $ \ptr _ -> act (castPtr ptr)

createFile ::
       WindowsPath
    -> AccessMode
    -> ShareMode
    -> Maybe LPSECURITY_ATTRIBUTES
    -> CreateMode
    -> FileAttributeOrFlag
    -> Maybe HANDLE
    -> IO HANDLE
createFile name access share mb_attr mode flag mb_h =
  withFilePath name $ \ c_name ->
      failIfWithRetry
        (== iNVALID_HANDLE_VALUE)
        (unwords ["CreateFile", show name])
        $ c_CreateFile
            c_name access share (maybePtr mb_attr) mode flag (maybePtr mb_h)

maxShareMode :: ShareMode
maxShareMode =
      Win32.fILE_SHARE_DELETE
  .|. Win32.fILE_SHARE_READ
  .|. Win32.fILE_SHARE_WRITE

writeShareMode :: ShareMode
writeShareMode =
      Win32.fILE_SHARE_DELETE
  .|. Win32.fILE_SHARE_READ

#if !defined(__IO_MANAGER_WINIO__)
foreign import ccall "_open_osfhandle"
  _open_osfhandle :: CIntPtr -> CInt -> IO CInt
#endif

win2HsHandle :: WindowsPath -> IOMode -> Win32.HANDLE -> IO Handle
win2HsHandle _fp iomode h = do
    when (iomode == AppendMode )
        $ void $ Win32.setFilePointerEx h 0 Win32.fILE_END
#if defined(__IO_MANAGER_WINIO__)
    Win32.hANDLEToHandle h
#else
    fd <- _open_osfhandle (fromIntegral (ptrToIntPtr h)) (#const _O_BINARY)
    fdToHandle' fd Nothing False (Path.toString _fp) iomode True
#endif

toHandle :: WindowsPath -> IOMode -> Win32.HANDLE -> IO Handle
toHandle fp iomode h =
    win2HsHandle fp iomode h `onException` Win32.closeHandle h

{-
-- | Open a file and return the 'Handle'.
openFile :: WindowsPath -> IOMode -> IO Handle
openFile fp iomode = bracketOnError
    (createFile
      fp
      accessMode
      shareMode
      Nothing
      createMode
#if defined(__IO_MANAGER_WINIO__)
      (case ioSubSystem of
        IoPOSIX -> Win32.fILE_ATTRIBUTE_NORMAL
        IoNative -> Win32.fILE_ATTRIBUTE_NORMAL .|. Win32.fILE_FLAG_OVERLAPPED
      )
#else
      Win32.fILE_ATTRIBUTE_NORMAL
#endif
      Nothing)
    Win32.closeHandle
    (toHandle fp iomode)
 where
  accessMode = case iomode of
    ReadMode      -> Win32.gENERIC_READ
    WriteMode     -> Win32.gENERIC_WRITE
    AppendMode    -> Win32.gENERIC_WRITE .|. Win32.fILE_APPEND_DATA
    ReadWriteMode -> Win32.gENERIC_READ .|. Win32.gENERIC_WRITE

  createMode = case iomode of
    ReadMode      -> Win32.oPEN_EXISTING
    WriteMode     -> Win32.cREATE_ALWAYS
    AppendMode    -> Win32.oPEN_ALWAYS
    ReadWriteMode -> Win32.oPEN_ALWAYS

  shareMode = case iomode of
    ReadMode      -> Win32.fILE_SHARE_READ
    WriteMode     -> writeShareMode
    AppendMode    -> writeShareMode
    ReadWriteMode -> maxShareMode

-}
#endif
