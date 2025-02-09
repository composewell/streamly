-- |
-- Module      : Streamly.Internal.FileSystem.Posix.ReadDir
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.FileSystem.Posix.ReadDir
    (
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
      DirStream (..)
    , openDirStream
    , openDirStreamCString
    , closeDirStream
    , readDirStreamEither
    , readEitherChunks
    , readEitherByteChunks
    , readEitherByteChunksAt
    , eitherReader
    , reader
#endif
    )
where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord)
import Foreign (Ptr, Word8, nullPtr, peek, peekByteOff, castPtr, plusPtr)
import Foreign.C
    (resetErrno, Errno(..), getErrno, eINTR, throwErrno
    , throwErrnoIfMinus1Retry_, throwErrnoIfNullRetry, CInt(..), CString, CChar, CSize(..))
import Foreign.Storable (poke)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import Streamly.Internal.Data.Stream (Stream(..), Step(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.FileSystem.Path (Path)
import Streamly.Internal.FileSystem.Posix.Errno (throwErrnoPathIfNullRetry)
import Streamly.Internal.FileSystem.Posix.File
    (OpenMode(..), openFd, openFdAt, closeFd)
import Streamly.Internal.FileSystem.PosixPath (PosixPath(..))
import System.Posix.Types (Fd(..))

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import qualified Streamly.Internal.Data.Unfold as UF (bracketIO)
import qualified Streamly.Internal.FileSystem.Path.Common as PathC
import qualified Streamly.Internal.FileSystem.PosixPath as Path

#include <dirent.h>

-------------------------------------------------------------------------------

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent

newtype DirStream = DirStream (Ptr CDir)

-------------------------------------------------------------------------------

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

foreign import capi unsafe "dirent.h opendir"
    c_opendir :: CString  -> IO (Ptr CDir)

foreign import capi unsafe "dirent.h fdopendir"
    c_fdopendir :: CInt  -> IO (Ptr CDir)

-- XXX The "unix" package uses a wrapper over readdir __hscore_readdir (see
-- cbits/HsUnix.c in unix package) which uses readdir_r in some cases where
-- readdir is not known to be re-entrant. We are not doing that here. We are
-- assuming that readdir is re-entrant which may not be the case on some old
-- unix systems.
foreign import ccall unsafe "dirent.h readdir"
    c_readdir  :: Ptr CDir -> IO (Ptr CDirent)

foreign import ccall unsafe "lstat_is_directory"
    c_lstat_is_directory  :: CString -> IO CInt

-- | The CString must be pinned.
{-# INLINE openDirStreamCString #-}
openDirStreamCString :: CString -> IO DirStream
openDirStreamCString s = do
    -- XXX we do not decode the path here, just print it as cstring
    -- XXX pass lazy concat of "openDirStream: " ++ s
    dirp <- throwErrnoIfNullRetry "openDirStream" $ c_opendir s
    return (DirStream dirp)

-- XXX Path is not null terminated therefore we need to make a copy even if the
-- array is pinned.
-- {-# INLINE openDirStream #-}
openDirStream :: PosixPath -> IO DirStream
openDirStream p =
    Array.asCStringUnsafe (Path.toChunk p) $ \s -> do
        -- openDirStreamCString s
        dirp <- throwErrnoPathIfNullRetry "openDirStream" p $ c_opendir s
        return (DirStream dirp)

-- | Note that the supplied Fd is used by DirStream and when we close the
-- DirStream the fd will be closed.
openDirStreamAt :: Fd -> PosixPath -> IO DirStream
openDirStreamAt fd p = do
    fd1 <- openFdAt (Just fd) p ReadOnly
    -- liftIO $ putStrLn $ "opened: " ++ show fd1
    dirp <- throwErrnoPathIfNullRetry "openDirStreamAt" p
        $ c_fdopendir (fromIntegral fd1)
    -- XXX can we somehow clone fd1 instead of opening again?
    return (DirStream dirp)

-- | @closeDirStream dp@ calls @closedir@ to close
--   the directory stream @dp@.
closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream dirp) = do
  throwErrnoIfMinus1Retry_ "closeDirStream" (c_closedir dirp)

isMetaDir :: Ptr CChar -> IO Bool
isMetaDir dname = do
    -- XXX Assuming an encoding that maps "." to ".", this is true for
    -- UTF8.
    -- Load as soon as possible to optimize memory accesses
    c1 <- peek dname
    c2 :: Word8 <- peekByteOff dname 1
    if (c1 /= fromIntegral (ord '.'))
    then return False
    else do
        if (c2 == 0)
        then return True
        else do
            if (c2 /= fromIntegral (ord '.'))
            then return False
            else do
                c3 :: Word8 <- peekByteOff dname 2
                if (c3 == 0)
                then return True
                else return False

{-# NOINLINE lstatDname #-}
lstatDname :: PosixPath -> Ptr CChar -> IO (Bool, Bool)
lstatDname parent dname = do
    isMeta <- liftIO $ isMetaDir dname
    if isMeta
    then pure (True, True)
    else do
        -- XXX We can create a pinned array right here since the next call pins
        -- it anyway.
        path <- appendCString parent dname
        Array.asCStringUnsafe (Path.toChunk path) $ \cStr -> do
            res <- c_lstat_is_directory cStr
            case res of
                x | x == 1 -> pure (True, False)
                x | x == 0 -> pure (False, False)
                -- XXX Need to check if and how we should handle some errors
                -- like EACCES.
                _ -> throwErrno "checkIfDirectory"

-- | Checks if dname is a directory and additionaly returns if dname is a meta
-- directory.
{-# INLINE checkDirStatus #-}
checkDirStatus
    :: PosixPath -> Ptr CChar -> #{type unsigned char} -> IO (Bool, Bool)
#ifdef FORCE_LSTAT_READDIR
checkDirStatus parent dname _ = lstatDname parent dname
#else
checkDirStatus parent dname dtype =
    if dtype == (#const DT_DIR)
    then do
        isMeta <- liftIO $ isMetaDir dname
        pure (True, isMeta)
    else if dtype /= #const DT_UNKNOWN
         then pure (False, False)
         else lstatDname parent dname
#endif

-- XXX We can use getdents64 directly so that we can use array slices from the
-- same buffer that we passed to the OS. That way we can also avoid any
-- overhead of bracket.
-- XXX Make this as Unfold to avoid returning Maybe
-- XXX Or NOINLINE some parts and inline the rest to fuse it
-- {-# INLINE readDirStreamEither #-}
readDirStreamEither ::
    -- DirStream -> IO (Either (Rel (Dir Path)) (Rel (File Path)))
    (PosixPath, DirStream) -> IO (Maybe (Either PosixPath PosixPath))
readDirStreamEither (curdir, (DirStream dirp)) = loop

  where

  -- mkPath :: IsPath (Rel (a Path)) => Array Word8 -> Rel (a Path)
  -- {-# INLINE mkPath #-}
  mkPath :: Array Word8 -> PosixPath
  mkPath = Path.unsafeFromChunk

  loop = do
    resetErrno
    ptr <- c_readdir dirp
    if (ptr /= nullPtr)
    then do
        let dname = #{ptr struct dirent, d_name} ptr
        dtype :: #{type unsigned char} <- #{peek struct dirent, d_type} ptr
        -- dreclen :: #{type unsigned short} <- #{peek struct dirent, d_reclen} ptr
        -- It is possible to find the name length using dreclen and then use
        -- fromPtrN, but it is not straightforward because the reclen is
        -- padded to 8-byte boundary.
        name <- Array.fromCString (castPtr dname)
        (isDir, isMeta) <- checkDirStatus curdir dname dtype
        if isDir
        then do
            if isMeta
            then loop
            else return (Just (Left (mkPath name)))
        else return (Just (Right (mkPath name)))
    else do
        errno <- getErrno
        if (errno == eINTR)
        then loop
        else do
            let (Errno n) = errno
            if (n == 0)
            -- then return (Left (mkPath (Array.fromList [46])))
            then return Nothing
            else throwErrno "readDirStreamEither"

-- XXX We can make this code common with windows, the path argument would be
-- redundant for windows case though.
{-# INLINE streamEitherReader #-}
streamEitherReader :: MonadIO m =>
    Unfold m (PosixPath, DirStream) (Either Path Path)
streamEitherReader = Unfold step return
    where

    step s = do
        r <- liftIO $ readDirStreamEither s
        case r of
            Nothing -> return Stop
            Just x -> return $ Yield x s

{-# INLINE streamReader #-}
streamReader :: MonadIO m => Unfold m (PosixPath, DirStream) Path
streamReader = fmap (either id id) streamEitherReader

{-# INLINE before #-}
before :: PosixPath -> IO (PosixPath, DirStream)
before parent = (parent,) <$> openDirStream parent

{-# INLINE after #-}
after :: (PosixPath, DirStream) -> IO ()
after (_, dirStream) = closeDirStream dirStream

--  | Read a directory emitting a stream with names of the children. Filter out
--  "." and ".." entries.
--
--  /Internal/
--
{-# INLINE reader #-}
reader :: (MonadIO m, MonadCatch m) => Unfold m Path Path
reader =
    -- XXX Instead of using bracketIO for each iteration of the loop we should
    -- instead yield a buffer of dir entries in each iteration and then use an
    -- unfold and concat to flatten those entries. That should improve the
    -- performance.
    UF.bracketIO before after streamReader

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
    UF.bracketIO before after streamEitherReader

{-# INLINE appendCString #-}
appendCString :: PosixPath -> CString -> IO PosixPath
appendCString (PosixPath a) b = do
    arr <- PathC.appendCString PathC.Posix a b
    pure $ PosixPath arr

{-# ANN type ChunkStreamState Fuse #-}
data ChunkStreamState =
      ChunkStreamInit [PosixPath] [PosixPath] Int [PosixPath] Int
    | ChunkStreamLoop
        PosixPath -- current dir path
        [PosixPath]  -- remaining dirs
        (Ptr CDir) -- current dir
        [PosixPath] -- dirs buffered
        Int    -- dir count
        [PosixPath] -- files buffered
        Int -- file count

-- XXX We can use a fold for collecting files and dirs.
-- XXX We can write a two fold scan to buffer and yield whichever fills first
-- like foldMany, it would be foldEither.
{-# INLINE readEitherChunks #-}
readEitherChunks :: MonadIO m => [PosixPath] -> Stream m (Either [PosixPath] [PosixPath])
readEitherChunks alldirs =
    Stream step (ChunkStreamInit alldirs [] 0 [] 0)

    where

    -- We want to keep the dir batching as low as possible for better
    -- concurrency esp when the number of dirs is low.
    dirMax = 4
    fileMax = 1000

    step _ (ChunkStreamInit (x:xs) dirs ndirs files nfiles) = do
        DirStream dirp <- liftIO $ openDirStream x
        return $ Skip (ChunkStreamLoop x xs dirp dirs ndirs files nfiles)

    step _ (ChunkStreamInit [] [] _ [] _) =
        return Stop

    step _ (ChunkStreamInit [] [] _ files _) =
        return $ Yield (Right files) (ChunkStreamInit [] [] 0 [] 0)

    step _ (ChunkStreamInit [] dirs _ files _) =
        return $ Yield (Left dirs) (ChunkStreamInit [] [] 0 files 0)

    step _ st@(ChunkStreamLoop curdir xs dirp dirs ndirs files nfiles) = do
        liftIO resetErrno
        dentPtr <- liftIO $ c_readdir dirp
        if (dentPtr /= nullPtr)
        then do
            let dname = #{ptr struct dirent, d_name} dentPtr
            dtype :: #{type unsigned char} <-
                liftIO $ #{peek struct dirent, d_type} dentPtr

            (isDir, isMeta) <- liftIO $ checkDirStatus curdir dname dtype
            if isDir
            then do
                if isMeta
                then return $ Skip st
                else do
                     path <- liftIO $ appendCString curdir dname
                     let dirs1 = path : dirs
                         ndirs1 = ndirs + 1
                      in if ndirs1 >= dirMax
                         then return $ Yield (Left dirs1)
                            (ChunkStreamLoop curdir xs dirp [] 0 files nfiles)
                         else return $ Skip
                            (ChunkStreamLoop curdir xs dirp dirs1 ndirs1 files nfiles)
            else do
                 path <- liftIO $ appendCString curdir dname
                 let files1 = path : files
                     nfiles1 = nfiles + 1
                  in if nfiles1 >= fileMax
                     then return $ Yield (Right files1)
                        (ChunkStreamLoop curdir xs dirp dirs ndirs [] 0)
                     else return $ Skip
                        (ChunkStreamLoop curdir xs dirp dirs ndirs files1 nfiles1)
        else do
            errno <- liftIO getErrno
            if (errno == eINTR)
            then return $ Skip st
            else do
                let (Errno n) = errno
                -- XXX Exception safety
                liftIO $ closeDirStream (DirStream dirp)
                if (n == 0)
                then return $ Skip (ChunkStreamInit xs dirs ndirs files nfiles)
                else liftIO $ throwErrno "readEitherChunks"

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

-- See also cstringLength# in GHC.CString in ghc-prim
foreign import ccall unsafe "string.h strlen" c_strlen
    :: Ptr CChar -> IO CSize

{-# ANN type ChunkStreamByteState Fuse #-}
data ChunkStreamByteState =
      ChunkStreamByteInit0
    | ChunkStreamByteInit [PosixPath] [PosixPath] Int MutByteArray Int
    | ChunkStreamByteLoop
        PosixPath -- current dir path
        [PosixPath]  -- remaining dirs
        (Ptr CDir) -- current dir
        [PosixPath] -- dirs buffered
        Int    -- dir count
        MutByteArray
        Int
    | ChunkStreamByteLoopPending
        (Ptr CChar) -- pending item
        PosixPath -- current dir path
        [PosixPath]  -- remaining dirs
        (Ptr CDir) -- current dir
        MutByteArray
        Int

-- XXX Add follow-symlinks option.
-- XXX Detect cycles.

-- XXX We can also emit both files and directories together this will be
-- especially useful when we are emitting chunks.
--
-- Since we are separating paths by newlines, it cannot support newlines in
-- paths. Or we can return null separated paths as well. Provide a Mut array
-- API to replace the nulls with newlines in-place.
--
-- We can pass a fold to make this modular, but if we are passing readdir
-- managed memory then we will have to consume it immediately. Otherwise we can
-- use getdents64 directly and use GHC managed memory instead.

-- | Left is directories. Right is a buffer containing directories and files
-- separated by newlines.
{-# INLINE readEitherByteChunks #-}
readEitherByteChunks :: MonadIO m =>
    [PosixPath] -> Stream m (Either [PosixPath] (Array Word8))
readEitherByteChunks alldirs =
    Stream step (ChunkStreamByteInit0)

    where

    -- XXX A single worker may not have enough directories to list at once to
    -- fill up a large buffer. We need to change the concurrency model such
    -- that a worker should be able to pick up another dir from the queue
    -- without emitting an output until the buffer fills.
    --
    -- XXX A worker can also pick up multiple work items in one go. However, we
    -- also need to keep in mind that any kind of batching might have
    -- pathological cases where concurrency may be reduced.
    --
    -- XXX Alternatively, we can distribute the dir stream over multiple
    -- concurrent folds and return (monadic output) a stream of arrays created
    -- from the output channel, then consume that stream by using a monad bind.
    bufSize = 4000

    copyToBuf dstArr pos dirPath name = do
        nameLen <- fmap fromIntegral (liftIO $ c_strlen name)
        let PosixPath (Array dirArr start end) = dirPath
            dirLen = end - start
            -- XXX We may need to decode and encode the path if the
            -- output encoding differs from fs encoding.
            --
            -- Account for separator and newline bytes.
            byteCount = dirLen + nameLen + 2
        if pos + byteCount <= bufSize
        then do
            -- XXX append a path separator to a dir path
            -- We know it is already pinned.
            MutByteArray.unsafeAsPtr dstArr (\ptr -> liftIO $ do
                MutByteArray.unsafePutSlice  dirArr start dstArr pos dirLen
                let ptr1 = ptr `plusPtr` (pos + dirLen)
                    separator = 47 :: Word8
                poke ptr1 separator
                let ptr2 = ptr1 `plusPtr` 1
                _ <- c_memcpy ptr2 (castPtr name) (fromIntegral nameLen)
                let ptr3 = ptr2 `plusPtr` nameLen
                    newline = 10 :: Word8
                poke ptr3 newline
                )
            return (Just (pos + byteCount))
        else return Nothing

    step _ ChunkStreamByteInit0 = do
        mbarr <- liftIO $ MutByteArray.new' bufSize
        return $ Skip (ChunkStreamByteInit alldirs [] 0 mbarr 0)

    step _ (ChunkStreamByteInit (x:xs) dirs ndirs mbarr pos) = do
        DirStream dirp <- liftIO $ openDirStream x
        return $ Skip (ChunkStreamByteLoop x xs dirp dirs ndirs mbarr pos)

    step _ (ChunkStreamByteInit [] [] _ _ pos) | pos == 0 =
        return Stop

    step _ (ChunkStreamByteInit [] [] _ mbarr pos) =
        return $ Yield (Right (Array mbarr 0 pos)) (ChunkStreamByteInit [] [] 0 mbarr 0)

    step _ (ChunkStreamByteInit [] dirs _ mbarr pos) =
        return $ Yield (Left dirs) (ChunkStreamByteInit [] [] 0 mbarr pos)

    step _ (ChunkStreamByteLoopPending pending curdir xs dirp mbarr pos) = do
        mbarr1 <- liftIO $ MutByteArray.new' bufSize
        r1 <- copyToBuf mbarr1 0 curdir pending
        case r1 of
            Just pos2 ->
                return $ Yield (Right (Array mbarr 0 pos))
                    -- When we come in this state we have emitted dirs
                    (ChunkStreamByteLoop curdir xs dirp [] 0 mbarr1 pos2)
            Nothing -> error "Dirname too big for bufSize"

    step _ st@(ChunkStreamByteLoop curdir xs dirp dirs ndirs mbarr pos) = do
        liftIO resetErrno
        dentPtr <- liftIO $ c_readdir dirp
        if (dentPtr /= nullPtr)
        then do
            let dname = #{ptr struct dirent, d_name} dentPtr
            dtype :: #{type unsigned char} <-
                liftIO $ #{peek struct dirent, d_type} dentPtr

            -- XXX Skips come around the entire loop, does that impact perf
            -- because it has a StreamK in the middle.
            -- Keep the file check first as it is more likely
            (isDir, isMeta) <- liftIO $ checkDirStatus curdir dname dtype
            if not isDir
            then do
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 ->
                            return $ Skip
                                (ChunkStreamByteLoop curdir xs dirp dirs ndirs mbarr pos1)
                        Nothing -> do
                            -- XXX we do not need to yield the out dirs here
                            -- XXX But we should yield if the number of dirs
                            -- become more than a threshold.
                            if ndirs > 0
                            then
                                return $ Yield (Left dirs)
                                    (ChunkStreamByteLoopPending dname curdir xs dirp mbarr pos)
                            else
                                return $ Skip
                                    (ChunkStreamByteLoopPending dname curdir xs dirp mbarr pos)
            else do
                if isMeta
                then return $ Skip st
                else do
                    path <- liftIO $ appendCString curdir dname
                    let dirs1 = path : dirs
                        ndirs1 = ndirs + 1
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 ->
                            return $ Skip
                                (ChunkStreamByteLoop curdir xs dirp dirs1 ndirs1 mbarr pos1)
                        Nothing -> do
                            -- We know dirs1 in not empty here
                            -- XXX Yield only if dirs are more than a threshold
                            -- otherwise skip.
                            return $ Yield (Left dirs1)
                                (ChunkStreamByteLoopPending dname curdir xs dirp mbarr pos)
        else do
            errno <- liftIO getErrno
            if (errno == eINTR)
            then return $ Skip st
            else do
                let (Errno n) = errno
                -- XXX Exception safety
                liftIO $ closeDirStream (DirStream dirp)
                if (n == 0)
                then return $ Skip (ChunkStreamByteInit xs dirs ndirs mbarr pos)
                else liftIO $ throwErrno "readEitherByteChunks"

{-# ANN type ByteChunksAt Fuse #-}
data ByteChunksAt =
      ByteChunksAtInit0
    | ByteChunksAtInit
        Fd
        [PosixPath] -- input dirs
        -- (Handle, [PosixPath]) -- output dirs
        -- Int -- count of output dirs
        MutByteArray -- output files and dirs
        Int -- position in MutByteArray
    | ByteChunksAtLoop
        Fd
        (Ptr CDir) -- current dir stream
        PosixPath -- current dir path
        [PosixPath]  -- remaining dirs
        [PosixPath] -- output dirs
        Int    -- output dir count
        MutByteArray
        Int
    | ByteChunksAtRealloc
        (Ptr CChar) -- pending item
        Fd
        (Ptr CDir) -- current dir stream
        PosixPath -- current dir path
        [PosixPath]  -- remaining dirs
        [PosixPath] -- output dirs
        Int    -- output dir count
        MutByteArray
        Int

-- The advantage of readEitherByteChunks over readEitherByteChunksAt is that we
-- do not need to open the dir handles and thus requires less open fd.
{-# INLINE readEitherByteChunksAt #-}
readEitherByteChunksAt :: MonadIO m =>
       -- (parent dir path, child dir paths rel to parent)
       (PosixPath, [PosixPath])
    -> Stream m (Either (PosixPath, [PosixPath]) (Array Word8))
readEitherByteChunksAt (ppath, alldirs) =
    Stream step (ByteChunksAtInit0)

    where

    bufSize = 4000

    copyToBuf dstArr pos dirPath name = do
        nameLen <- fmap fromIntegral (liftIO $ c_strlen name)
        -- XXX prepend ppath to dirPath
        let PosixPath (Array dirArr start end) = dirPath
            dirLen = end - start
            -- XXX We may need to decode and encode the path if the
            -- output encoding differs from fs encoding.
            --
            -- Account for separator and newline bytes.
            byteCount = dirLen + nameLen + 2
        if pos + byteCount <= bufSize
        then do
            -- XXX append a path separator to a dir path
            -- We know it is already pinned.
            MutByteArray.unsafeAsPtr dstArr (\ptr -> liftIO $ do
                MutByteArray.unsafePutSlice  dirArr start dstArr pos dirLen
                let ptr1 = ptr `plusPtr` (pos + dirLen)
                    separator = 47 :: Word8
                poke ptr1 separator
                let ptr2 = ptr1 `plusPtr` 1
                _ <- c_memcpy ptr2 (castPtr name) (fromIntegral nameLen)
                let ptr3 = ptr2 `plusPtr` nameLen
                    newline = 10 :: Word8
                poke ptr3 newline
                )
            return (Just (pos + byteCount))
        else return Nothing

    step _ ByteChunksAtInit0 = do
        pfd <- liftIO $ openFd ppath ReadOnly
        mbarr <- liftIO $ MutByteArray.new' bufSize
        return $ Skip (ByteChunksAtInit pfd alldirs mbarr 0)

    step _ (ByteChunksAtInit ph (x:xs) mbarr pos) = do
        (DirStream dirp) <- liftIO $ openDirStreamAt ph x
        return $ Skip (ByteChunksAtLoop ph dirp x xs [] 0 mbarr pos)

    step _ (ByteChunksAtInit pfd [] _ 0) = do
        liftIO $ closeFd (pfd)
        return Stop

    step _ (ByteChunksAtInit pfd [] mbarr pos) = do
        return
            $ Yield
                (Right (Array mbarr 0 pos))
                (ByteChunksAtInit pfd [] mbarr 0)

    step _ (ByteChunksAtRealloc pending pfd dirp curdir xs dirs ndirs mbarr pos) = do
        mbarr1 <- liftIO $ MutByteArray.new' bufSize
        r1 <- copyToBuf mbarr1 0 curdir pending
        case r1 of
            Just pos2 ->
                return $ Yield (Right (Array mbarr 0 pos))
                    (ByteChunksAtLoop pfd dirp curdir xs dirs ndirs mbarr1 pos2)
            Nothing -> error "Dirname too big for bufSize"

    step _ st@(ByteChunksAtLoop pfd dirp curdir xs dirs ndirs mbarr pos) = do
        liftIO resetErrno
        dentPtr <- liftIO $ c_readdir dirp
        if (dentPtr /= nullPtr)
        then do
            let dname = #{ptr struct dirent, d_name} dentPtr
            dtype :: #{type unsigned char} <-
                liftIO $ #{peek struct dirent, d_type} dentPtr

            -- Keep the file check first as it is more likely
            (isDir, isMeta) <- liftIO $ checkDirStatus curdir dname dtype
            if not isDir
            then do
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 ->
                            return $ Skip
                                (ByteChunksAtLoop
                                    pfd dirp curdir xs dirs ndirs mbarr pos1)
                        Nothing ->
                            return $ Skip
                                (ByteChunksAtRealloc
                                    dname pfd dirp curdir xs dirs ndirs mbarr pos)
            else do
                if isMeta
                then return $ Skip st
                else do
                    arr <- Array.fromCString (castPtr dname)
                    let path = Path.unsafeFromChunk arr
                    let dirs1 = path : dirs
                        ndirs1 = ndirs + 1
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 ->
                            -- XXX When there is less parallelization at the
                            -- top of the tree, we should use smaller chunks.
                            {-
                            if ndirs > 64
                            then do
                                let fpath = Path.unsafeAppend ppath curdir
                                return $ Yield
                                    (Left (fpath, dirs1))
                                    (ByteChunksAtLoop pfd dirp curdir xs [] 0 mbarr pos1)
                            else
                            -}
                                return $ Skip
                                    (ByteChunksAtLoop
                                        pfd dirp curdir xs dirs1 ndirs1 mbarr pos1)
                        Nothing -> do
                            return $ Skip
                                (ByteChunksAtRealloc
                                    dname pfd dirp curdir xs dirs1 ndirs1 mbarr pos)
        else do
            errno <- liftIO getErrno
            if (errno == eINTR)
            then return $ Skip st
            else do
                let (Errno n) = errno
                -- XXX What if an exception occurs in the code before this?
                -- Should we attach a weak IORef to close the fd on GC.
                liftIO $ closeDirStream (DirStream dirp)
                if (n == 0)
                then
                    -- XXX Yielding on each dir completion may hurt perf when
                    -- there are many small directories. However, it may also
                    -- help parallelize more in IO bound case.
                    if ndirs > 0
                    then do
                        let fpath = Path.unsafeAppend ppath curdir
                        return $ Yield
                            (Left (fpath, dirs))
                            (ByteChunksAtInit pfd xs mbarr pos)
                    else return $ Skip (ByteChunksAtInit pfd xs mbarr pos)
                else liftIO $ throwErrno "readEitherByteChunks"
#endif
