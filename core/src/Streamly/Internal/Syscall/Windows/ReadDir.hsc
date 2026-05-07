-- |
-- Module      : Streamly.Internal.Syscall.Windows.ReadDir
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

{-# LANGUAGE UnliftedFFITypes #-}

module Streamly.Internal.Syscall.Windows.ReadDir
    (
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
      DirStream
    , openDirStream
    , closeDirStream
    , readDirStreamEither
    , readEitherChunks
    , readEitherByteChunks
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
import GHC.Base (Addr##)
import Foreign.C
    ( CInt(..), CSize(..), CWchar(..), Errno(..)
    , errnoToIOError, peekCWString
    )
import Fusion.Plugin.Types (Fuse(..))
import Numeric (showHex)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Stream (Stream(..), Step(..))
import Streamly.Internal.FileSystem.Path (Path)
import Streamly.Internal.FileSystem.WindowsPath (WindowsPath(..))
import System.IO.Error (ioeSetErrorString)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import qualified Streamly.Internal.Data.Unfold as UF (bracketIO)
import qualified Streamly.Internal.FileSystem.Path.Common as PathC
import qualified Streamly.Internal.FileSystem.WindowsPath as Path
import qualified System.Win32 as Win32 (failWith)

import Streamly.Internal.FileSystem.DirOptions
import Streamly.Internal.Syscall.Windows.Common (asCWString)
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
-- FFI imports/Haskell C APIs
------------------------------------------------------------------------------

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

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
-- Path string manipulation
------------------------------------------------------------------------------

foreign import ccall unsafe "wchar.h wcslen" c_wcslen
    :: Ptr CWchar -> IO CSize

foreign import ccall unsafe "wchar.h wcslen" c_wcslen_pinned
    :: Addr## -> IO CSize

-- This is defined here and not in Path module because wcslen is a platform
-- specific function and uses 32-bit wide chars on posix and 16-bit wide chars
-- on Windows. We cannot have it in WindowsPath module because that module is
-- plaform agnostic and works on Posix as well.
--
{-# INLINE appendW16CString #-}
appendW16CString :: WindowsPath -> Ptr CWchar -> IO WindowsPath
appendW16CString (WindowsPath arr) str =
    fmap WindowsPath
        $ PathC.appendCStringWith
            MutArray.emptyOf
            c_wcslen_pinned
            PathC.Windows
            arr
            (castPtr str)

------------------------------------------------------------------------------
-- Dir stream implementation
------------------------------------------------------------------------------

-- XXX Define this as data and unpack three fields?
newtype DirStream =
    DirStream (HANDLE, IORef Bool, ForeignPtr WIN32_FIND_DATA)

openDirStream :: WindowsPath -> IO DirStream
openDirStream p = do
    let path = Path.unsafeJoin p $ Path.unsafeFromString "*"
    fp_finddata <- mallocForeignPtrBytes (# const sizeof(WIN32_FIND_DATAW) )
    withForeignPtr fp_finddata $ \dataPtr -> do
        handle <-
            asCWString path $ \pathPtr -> do
                -- XXX Use getLastError to distinguish the case when no
                -- matching file is found. See the doc of FindFirstFileW.
                failIf
                    (== iNVALID_HANDLE_VALUE)
                    ("FindFirstFileW: " ++ Path.toString path)
                    $ c_FindFirstFileW pathPtr dataPtr
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

readDirStreamEither ::
    (ReadOptions -> ReadOptions) ->
    DirStream -> IO (Maybe (Either WindowsPath WindowsPath))
readDirStreamEither _ (DirStream (h, ref, fdata)) =
    withForeignPtr fdata $ \ptr -> do
        firstTime <- readIORef ref
        if firstTime
        then do
            writeIORef ref False
            processEntry ptr
        else findNext ptr

    where

    -- XXX: for a symlink the attribute may have a FILE_ATTRIBUTE_DIRECTORY if
    -- the symlink was created as a directory symlink, but it might have
    -- changed later. To find the real type of the symlink when we have
    -- followSymlinks option on we need to check if it is a
    -- FILE_ATTRIBUTE_REPARSE_POINT, we need to open the reparse point and find
    -- the type.

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
            else return (Just (Left (Path.unsafeFromArray name)))
        else return (Just (Right (Path.unsafeFromArray name)))

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
    (ReadOptions -> ReadOptions) ->
    Unfold m DirStream (Either Path Path)
streamEitherReader f = Unfold step return
    where

    step strm = do
        r <- liftIO $ readDirStreamEither f strm
        case r of
            Nothing -> return Stop
            Just x -> return $ Yield x strm

{-# INLINE streamReader #-}
streamReader :: MonadIO m => Unfold m DirStream Path
streamReader = fmap (either id id) (streamEitherReader id)

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
    (ReadOptions -> ReadOptions) -> Unfold m Path (Either Path Path)
eitherReader f =
    -- XXX The measured overhead of bracketIO is not noticeable, if it turns
    -- out to be a problem for small filenames we can use getdents64 to use
    -- chunked read to avoid the overhead.
      UF.bracketIO openDirStream closeDirStream (streamEitherReader f)

------------------------------------------------------------------------------
-- Chunked path-list reads
------------------------------------------------------------------------------

{-# ANN type ChunkStreamState Fuse #-}
data ChunkStreamState =
      ChunkStreamInit [WindowsPath] [WindowsPath] Int [WindowsPath] Int
    | ChunkStreamLoop
        WindowsPath -- current dir path
        [WindowsPath]  -- remaining dirs
        DirStream -- current dir stream
        [WindowsPath] -- dirs buffered
        Int    -- dir count
        [WindowsPath] -- files buffered
        Int -- file count

-- | Like 'readEitherByteChunks' but yields lists of 'WindowsPath' instead of
-- byte buffers. Directories are emitted as 'Left' and files as 'Right'. Meta
-- entries (\".\" and \"..\") are filtered out.
{-# INLINE readEitherChunks #-}
readEitherChunks
    :: MonadIO m
    => (ReadOptions -> ReadOptions)
    -> [WindowsPath] -> Stream m (Either [WindowsPath] [WindowsPath])
readEitherChunks _confMod alldirs =
    Stream step (ChunkStreamInit alldirs [] 0 [] 0)

    where

    -- We want to keep the dir batching as low as possible for better
    -- concurrency esp when the number of dirs is low.
    dirMax = 4
    fileMax = 1000

    -- Returns Just (dname, dattrs) on success, Nothing at end of stream.
    readNextEntry (DirStream (h, ref, fdata)) =
        withForeignPtr fdata $ \ptr -> do
            firstTime <- readIORef ref
            success <-
                if firstTime
                then writeIORef ref False >> return True
                else c_FindNextFileW h ptr
            if success
            then do
                let dname = #{ptr WIN32_FIND_DATAW, cFileName} ptr
                dattrs :: #{type DWORD} <-
                    #{peek WIN32_FIND_DATAW, dwFileAttributes} ptr
                return (Just (dname, dattrs))
            else do
                err <- getLastError
                if err == (# const ERROR_NO_MORE_FILES )
                then return Nothing
                else Win32.failWith "findNextFile" err

    step _ (ChunkStreamInit (x:xs) dirs ndirs files nfiles) = do
        ds <- liftIO $ openDirStream x
        return $ Skip (ChunkStreamLoop x xs ds dirs ndirs files nfiles)

    step _ (ChunkStreamInit [] [] _ [] _) =
        return Stop

    step _ (ChunkStreamInit [] [] _ files _) =
        return $ Yield (Right files) (ChunkStreamInit [] [] 0 [] 0)

    step _ (ChunkStreamInit [] dirs _ files _) =
        return $ Yield (Left dirs) (ChunkStreamInit [] [] 0 files 0)

    step _ st@(ChunkStreamLoop curdir xs ds dirs ndirs files nfiles) = do
        r <- liftIO $ readNextEntry ds
        case r of
            Just (dname, dattrs) ->
                if (dattrs .&. (#const FILE_ATTRIBUTE_DIRECTORY) /= 0)
                then do
                    isMeta <- liftIO $ isMetaDir dname
                    if isMeta
                    then return $ Skip st
                    else do
                        path <- liftIO $ appendW16CString curdir dname
                        let dirs1 = path : dirs
                            ndirs1 = ndirs + 1
                        if ndirs1 >= dirMax
                        then return $ Yield (Left dirs1)
                            (ChunkStreamLoop curdir xs ds [] 0 files nfiles)
                        else return $ Skip
                            (ChunkStreamLoop
                                curdir xs ds dirs1 ndirs1 files nfiles)
                else do
                    path <- liftIO $ appendW16CString curdir dname
                    let files1 = path : files
                        nfiles1 = nfiles + 1
                    if nfiles1 >= fileMax
                    then return $ Yield (Right files1)
                        (ChunkStreamLoop curdir xs ds dirs ndirs [] 0)
                    else return $ Skip
                        (ChunkStreamLoop
                            curdir xs ds dirs ndirs files1 nfiles1)
            Nothing -> do
                -- XXX Exception safety
                liftIO $ closeDirStream ds
                return $ Skip (ChunkStreamInit xs dirs ndirs files nfiles)

------------------------------------------------------------------------------
-- Chunked byte-buffered reads
------------------------------------------------------------------------------

-- Split a list in half.
splitHalf :: [a] -> ([a], [a])
splitHalf xxs = split xxs xxs

    where

    split (x:xs) (_:_:ys) =
        let (f, s) = split xs ys
         in (x:f, s)
    split xs _ = ([], xs)

{-# ANN type ChunkStreamByteState Fuse #-}
data ChunkStreamByteState =
      ChunkStreamByteInit
    | ChunkStreamByteStop
    | ChunkStreamByteLoop
        WindowsPath -- current dir path
        [WindowsPath]  -- remaining dirs
        DirStream -- current dir stream
        MutByteArray
        Int
    | ChunkStreamReallocBuf
        (Ptr CWchar) -- pending item name
        WindowsPath -- current dir path
        [WindowsPath]  -- remaining dirs
        DirStream -- current dir stream
        MutByteArray
        Int
    | ChunkStreamDrainBuf
        MutByteArray
        Int

-- NOTE: Unlike posix on Windows the file attribute to determine whether it is
-- a directory or not is always available so we do not need the code to handle
-- the case when they are not available, on Posix we need to use stat
-- explicitly in that case.

-- | This function may not traverse all the directories supplied and it may
-- traverse the directories recursively. Left contains those directories that
-- were not traversed by this function, these may be the directories that were
-- supplied as input as well as newly discovered directories during traversal.
-- To traverse the entire tree we have to iterate this function on the Left
-- output.
--
-- Right is a buffer containing UTF-16LE encoded directories and files
-- separated by newlines, with the parent path joined to each child name by a
-- backslash.
--
{-# INLINE readEitherByteChunks #-}
readEitherByteChunks :: MonadIO m =>
    (ReadOptions -> ReadOptions) ->
    [WindowsPath] -> Stream m (Either [WindowsPath] (Array Word8))
readEitherByteChunks _confMod alldirs =
    Stream step ChunkStreamByteInit

    where

    bufSize = 32000

    -- The output is UTF-16LE encoded. The format per entry is:
    -- dirPath ++ '\\' ++ name ++ '\n', where each character occupies 2 bytes.
    copyToBuf dstArr pos dirPath name = do
        nameLen <- fmap ((* 2) . fromIntegral) (liftIO $ c_wcslen name)
        MutByteArray.unsafeAsPtr dstArr (\ptr -> liftIO $ do
            let WindowsPath (Array dirArr start end) = dirPath
                dirLen = end - start
                endDir = pos + dirLen
                -- separator (2 bytes) + newline (2 bytes)
                endPos = endDir + nameLen + 4
                sepOff = ptr `plusPtr` endDir
                nameOff = sepOff `plusPtr` 2
                nlOff = nameOff `plusPtr` nameLen
            if (endPos < bufSize)
            then do
                MutByteArray.unsafePutSlice dirArr start dstArr pos dirLen
                -- '\\' as UTF-16LE: 0x5C 0x00
                poke sepOff (92 :: Word8)
                poke (sepOff `plusPtr` 1) (0 :: Word8)
                _ <- c_memcpy nameOff (castPtr name) (fromIntegral nameLen)
                -- '\n' as UTF-16LE: 0x0A 0x00
                poke nlOff (10 :: Word8)
                poke (nlOff `plusPtr` 1) (0 :: Word8)
                return (Just endPos)
            else return Nothing
            )

    -- Returns Just (dname, dattrs) on success, Nothing at end of stream. The
    -- returned dname pointer is valid until the next call to readNextEntry on
    -- the same DirStream.
    readNextEntry (DirStream (h, ref, fdata)) =
        withForeignPtr fdata $ \ptr -> do
            firstTime <- readIORef ref
            success <-
                if firstTime
                then writeIORef ref False >> return True
                else c_FindNextFileW h ptr
            if success
            then do
                let dname = #{ptr WIN32_FIND_DATAW, cFileName} ptr
                dattrs :: #{type DWORD} <-
                    #{peek WIN32_FIND_DATAW, dwFileAttributes} ptr
                return (Just (dname, dattrs))
            else do
                err <- getLastError
                if err == (# const ERROR_NO_MORE_FILES )
                then return Nothing
                else Win32.failWith "findNextFile" err

    step _ ChunkStreamByteInit = do
        mbarr <- liftIO $ MutByteArray.new' bufSize
        case alldirs of
            (x:xs) -> do
                ds <- liftIO $ openDirStream x
                return $ Skip $ ChunkStreamByteLoop x xs ds mbarr 0
            [] -> return Stop

    step _ ChunkStreamByteStop = return Stop

    step _ (ChunkStreamReallocBuf pending curdir xs ds mbarr pos) = do
        mbarr1 <- liftIO $ MutByteArray.new' bufSize
        r1 <- copyToBuf mbarr1 0 curdir pending
        case r1 of
            Just pos2 ->
                return $ Yield (Right (Array mbarr 0 pos))
                    -- When we come in this state we have emitted dirs
                    (ChunkStreamByteLoop curdir xs ds mbarr1 pos2)
            Nothing -> error "Dirname too big for bufSize"

    step _ (ChunkStreamDrainBuf mbarr pos) =
        if pos == 0
        then return Stop
        else return $ Yield (Right (Array mbarr 0 pos)) ChunkStreamByteStop

    step _ (ChunkStreamByteLoop icurdir ixs ids mbarr ipos) =
        goOuter icurdir ids ixs ipos

        where

        -- This is recursed only when we open the next dir.
        -- Encapsulates curdir and ds as static arguments.
        goOuter curdir ds = goInner

            where

            -- This is recursed each time we find a dir.
            -- Encapsulates dirs as static argument.
            goInner dirs = nextEntry

                where

                {-# INLINE nextEntry #-}
                nextEntry pos = do
                    r <- liftIO $ readNextEntry ds
                    case r of
                        Just (dname, dattrs) ->
                            handleDentry pos dname dattrs
                        Nothing -> handleEnd pos

                handleEnd pos = do
                    -- XXX Exception safety
                    liftIO $ closeDirStream ds
                    openNextDir pos

                openNextDir pos =
                    case dirs of
                        (x:xs) -> do
                            ds1 <- liftIO $ openDirStream x
                            goOuter x ds1 xs pos
                        [] ->
                            if pos == 0
                            then return Stop
                            else return
                                    $ Yield
                                        (Right (Array mbarr 0 pos))
                                        ChunkStreamByteStop

                splitAndRealloc pos dname xs1 =
                    case xs1 of
                        [] ->
                            return $ Skip
                                (ChunkStreamReallocBuf dname curdir
                                    [] ds mbarr pos)
                        _ -> do
                            let (h,t) = splitHalf xs1
                            return $ Yield (Left t)
                                (ChunkStreamReallocBuf dname curdir
                                    h ds mbarr pos)

                {-# INLINE handleFileEnt #-}
                handleFileEnt pos dname = do
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 -> nextEntry pos1
                        Nothing -> splitAndRealloc pos dname dirs

                {-# INLINE handleDirEnt #-}
                handleDirEnt pos dname = do
                    path <- liftIO $ appendW16CString curdir dname
                    let dirs1 = path : dirs
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 -> goInner dirs1 pos1
                        Nothing -> splitAndRealloc pos dname dirs1

                handleDentry pos dname dattrs =
                    if (dattrs .&. (#const FILE_ATTRIBUTE_DIRECTORY) /= 0)
                    then do
                        isMeta <- liftIO $ isMetaDir dname
                        if isMeta
                        then nextEntry pos
                        else handleDirEnt pos dname
                    else handleFileEnt pos dname
#endif
