-- |
-- Module      : Streamly.Internal.FileSystem.Windows.ReadDir
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.FileSystem.Windows.ReadDir
    (
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
      DirStream
    , openDirStream
    , closeDirStream
    , readDirStreamEither
    , eitherReader
    , reader
#endif
    )
where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord, isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C (CInt(..), CWchar(..), Errno(..), errnoToIOError, peekCWString)
import Numeric (showHex)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Stream (Step(..))
import Streamly.Internal.FileSystem.Path (Path)
import Streamly.Internal.FileSystem.WindowsPath (WindowsPath(..))
import System.IO.Error (ioeSetErrorString)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Unfold as UF (bracketIO)
import qualified Streamly.Internal.FileSystem.WindowsPath as Path
import qualified System.Win32 as Win32 (failWith)

import Foreign hiding (void)

#include <windows.h>

-- Note on A vs W suffix in APIs.
-- CreateFile vs. CreateFileW: CreateFile is a macro that expands to
-- CreateFileA or CreateFileW depending on whether Unicode support (UNICODE and
-- _UNICODE preprocessor macros) is enabled in your project. To ensure
-- consistent Unicode support, explicitly use CreateFileW.

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type BOOL = Bool
type DWORD = Word32

type UINT_PTR = Word
type ErrCode = DWORD
type LPCTSTR = Ptr CWchar
type WIN32_FIND_DATA = ()
type HANDLE = Ptr ()

------------------------------------------------------------------------------
-- Windows C APIs
------------------------------------------------------------------------------

-- XXX Note for i386, stdcall is needed instead of ccall, see Win32
-- package/windows_cconv.h. We support only x86_64 for now.
foreign import ccall unsafe "windows.h FindFirstFileW"
  c_FindFirstFileW :: LPCTSTR -> Ptr WIN32_FIND_DATA -> IO HANDLE

foreign import ccall unsafe "windows.h FindNextFileW"
  c_FindNextFileW :: HANDLE -> Ptr WIN32_FIND_DATA -> IO BOOL

foreign import ccall unsafe "windows.h FindClose"
  c_FindClose :: HANDLE -> IO BOOL

foreign import ccall unsafe "windows.h GetLastError"
  getLastError :: IO ErrCode

foreign import ccall unsafe "windows.h LocalFree"
  localFree :: Ptr a -> IO (Ptr a)

------------------------------------------------------------------------------
-- Haskell C APIs
------------------------------------------------------------------------------

foreign import ccall unsafe "maperrno_func" -- in base/cbits/Win32Utils.c
  c_maperrno_func :: ErrCode -> IO Errno

------------------------------------------------------------------------------
-- Error Handling
------------------------------------------------------------------------------

-- XXX getErrorMessage and castUINTPtrToPtr require c code, so left out for
-- now. Once we replace these we can remove dependency on Win32. We can
-- possibly implement these in Haskell by directly calling the Windows API.

foreign import ccall unsafe "getErrorMessage"
  getErrorMessage :: DWORD -> IO (Ptr CWchar)

foreign import ccall unsafe "castUINTPtrToPtr"
  castUINTPtrToPtr :: UINT_PTR -> Ptr a

failWith :: String -> ErrCode -> IO a
failWith fn_name err_code = do
  c_msg <- getErrorMessage err_code
  msg <- if c_msg == nullPtr
         then return $ "Error 0x" ++ Numeric.showHex err_code ""
         else do
             msg <- peekCWString c_msg
             -- We ignore failure of freeing c_msg, given we're already failing
             _ <- localFree c_msg
             return msg
  errno <- c_maperrno_func err_code
  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
      ioerror = errnoToIOError fn_name errno Nothing Nothing
                  `ioeSetErrorString` msg'
  throwIO ioerror

errorWin :: String -> IO a
errorWin fn_name = do
  err_code <- getLastError
  failWith fn_name err_code

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
  v <- act
  if p v then errorWin wh else return v

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = castUINTPtrToPtr maxBound

------------------------------------------------------------------------------
-- Dir stream implementation
------------------------------------------------------------------------------

-- XXX Define this as data and unpack three fields?
newtype DirStream =
    DirStream (HANDLE, IORef Bool, ForeignPtr WIN32_FIND_DATA)

openDirStream :: WindowsPath -> IO DirStream
openDirStream p = do
    let path = Path.unsafeAppend p $ Path.unsafeFromString "*"
    fp_finddata <- mallocForeignPtrBytes (# const sizeof(WIN32_FIND_DATAW) )
    withForeignPtr fp_finddata $ \dataPtr -> do
        handle <-
            Array.asCStringUnsafe (Path.toChunk path) $ \pathPtr -> do
                -- XXX Use getLastError to distinguish the case when no
                -- matching file is found. See the doc of FindFirstFileW.
                failIf
                    (== iNVALID_HANDLE_VALUE)
                    ("FindFirstFileW: " ++ Path.toString path)
                    $ c_FindFirstFileW (castPtr pathPtr) dataPtr
        ref <- newIORef True
        return $ DirStream (handle, ref, fp_finddata)

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream (h, _, _)) = void (c_FindClose h)

-- XXX Keep this in sync with the isMetaDir function in Posix readdir module.
isMetaDir :: Ptr CWchar -> IO Bool
isMetaDir dname = do
    -- XXX Assuming UTF16LE encoding
    c1 <- peek dname
    if (c1 /= fromIntegral (ord '.'))
    then return False
    else do
        c2 :: Word8 <- peekByteOff dname 1
        if (c2 == 0)
        then return True
        else if (c2 /= fromIntegral (ord '.'))
        then return False
        else do
            c3 :: Word8 <- peekByteOff dname 2
            if (c3 == 0)
            then return True
            else return False

readDirStreamEither :: DirStream -> IO (Maybe (Either WindowsPath WindowsPath))
readDirStreamEither (DirStream (h, ref, fdata)) =
    withForeignPtr fdata $ \ptr -> do
        firstTime <- readIORef ref
        if firstTime
        then do
            writeIORef ref False
            processEntry ptr
        else findNext ptr

    where

    mkPath :: Array Word8 -> WindowsPath
    mkPath = Path.unsafeFromChunk

    processEntry ptr = do
        let dname = #{ptr WIN32_FIND_DATAW, cFileName} ptr
        dattrs :: #{type DWORD} <-
            #{peek WIN32_FIND_DATAW, dwFileAttributes} ptr
        name <- Array.fromW16CString dname
        if (dattrs .&. (#const FILE_ATTRIBUTE_DIRECTORY) /= 0)
        then do
            isMeta <- isMetaDir dname
            if isMeta
            then findNext ptr
            else return (Just (Left (mkPath (Array.unsafeCast name))))
        else return (Just (Right (mkPath (Array.unsafeCast name))))

    findNext ptr = do
        retval <- liftIO $ c_FindNextFileW h ptr
        if (retval)
        then processEntry ptr
        else do
            err <- getLastError
            if err == (# const ERROR_NO_MORE_FILES )
            then return Nothing
            -- XXX Print the path in the error message
            else Win32.failWith "findNextFile" err

{-# INLINE streamEitherReader #-}
streamEitherReader :: MonadIO m =>
    Unfold m DirStream (Either Path Path)
streamEitherReader = Unfold step return
    where

    step strm = do
        r <- liftIO $ readDirStreamEither strm
        case r of
            Nothing -> return Stop
            Just x -> return $ Yield x strm

{-# INLINE streamReader #-}
streamReader :: MonadIO m => Unfold m DirStream Path
streamReader = fmap (either id id) streamEitherReader

--  | Read a directory emitting a stream with names of the children. Filter out
--  "." and ".." entries.
--
--  /Internal/

{-# INLINE reader #-}
reader :: (MonadIO m, MonadCatch m) => Unfold m Path Path
reader =
-- XXX Instead of using bracketIO for each iteration of the loop we should
-- instead yield a buffer of dir entries in each iteration and then use an
-- unfold and concat to flatten those entries. That should improve the
-- performance.
      UF.bracketIO openDirStream closeDirStream streamReader

-- | Read directories as Left and files as Right. Filter out "." and ".."
-- entries.
--
--  /Internal/
--
{-# INLINE eitherReader #-}
eitherReader :: (MonadIO m, MonadCatch m) =>
    Unfold m Path (Either Path Path)
eitherReader =
    -- XXX The measured overhead of bracketIO is not noticeable, if it turns
    -- out to be a problem for small filenames we can use getdents64 to use
    -- chunked read to avoid the overhead.
      UF.bracketIO openDirStream closeDirStream streamEitherReader
#endif
