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
      DirStream
    , openDirStream
    , closeDirStream
    , readDirStreamEither
    , readEitherChunks
    , readEitherByteChunks
#endif
    )
where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord)
import Foreign (Ptr, Word8, nullPtr, peek, peekByteOff, castPtr, plusPtr)
import Foreign.C
    (resetErrno, Errno(..), getErrno, eINTR, throwErrno
    , throwErrnoIfMinus1Retry_, CInt(..), CString, CChar, CSize(..))
import Foreign.C.Error (errnoToIOError)
import Foreign.Storable (poke)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import Streamly.Internal.FileSystem.PosixPath (PosixPath(..))
import Streamly.Internal.Data.Stream (Stream(..), Step(..))

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import qualified Streamly.Internal.FileSystem.PosixPath as Path

#include <dirent.h>

-------------------------------------------------------------------------------
-- From unix
-------------------------------------------------------------------------------

-- | as 'throwErrno', but exceptions include the given path when appropriate.
--
throwErrnoPath :: String -> PosixPath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    -- XXX toString uses strict decoding, may fail
    ioError (errnoToIOError loc errno Nothing (Just (Path.toString path)))

throwErrnoPathIfRetry :: (a -> Bool) -> String -> PosixPath -> IO a -> IO a
throwErrnoPathIfRetry pr loc rpath f =
  do
    res <- f
    if pr res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoPathIfRetry pr loc rpath f
          else throwErrnoPath loc rpath
      else return res

throwErrnoPathIfNullRetry :: String -> PosixPath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry loc path f =
  throwErrnoPathIfRetry (== nullPtr) loc path f

-------------------------------------------------------------------------------
-- import System.Posix.Directory (closeDirStream)
-- import System.Posix.Directory.Internals (DirStream(..), CDir, CDirent)
-- requires unix >= 2.8
-------------------------------------------------------------------------------

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent

newtype DirStream = DirStream (Ptr CDir)

-------------------------------------------------------------------------------

foreign import ccall unsafe "closedir"
   c_closedir :: Ptr CDir -> IO CInt

foreign import capi unsafe "dirent.h opendir"
    c_opendir :: CString  -> IO (Ptr CDir)

-- XXX The "unix" package uses a wrapper over readdir __hscore_readdir (see
-- cbits/HsUnix.c in unix package) which uses readdir_r in some cases where
-- readdir is not known to be re-entrant. We are not doing that here. We are
-- assuming that readdir is re-entrant which may not be the case on some old
-- unix systems.
foreign import ccall unsafe "dirent.h readdir"
    c_readdir  :: Ptr CDir -> IO (Ptr CDirent)

-- XXX Use openat instead of open so that we do not have to build and resolve
-- absolute paths.
--
-- XXX Path is not null terminated therefore we need to make a copy even if the
-- array is pinned.
-- {-# INLINE openDirStream #-}
openDirStream :: PosixPath -> IO DirStream
openDirStream p =
  Array.asCStringUnsafe (Path.toChunk p) $ \s -> do
    dirp <- throwErrnoPathIfNullRetry "openDirStream" p $ c_opendir s
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

-- XXX We can use getdents64 directly so that we can use array slices from the
-- same buffer that we passed to the OS. That way we can also avoid any
-- overhead of bracket.
-- XXX Make this as Unfold to avoid returning Maybe
-- XXX Or NOINLINE some parts and inline the rest to fuse it
-- {-# INLINE readDirStreamEither #-}
readDirStreamEither ::
    -- DirStream -> IO (Either (Rel (Dir Path)) (Rel (File Path)))
    DirStream -> IO (Maybe (Either PosixPath PosixPath))
readDirStreamEither (DirStream dirp) = loop

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
        if (dtype == #const DT_DIR)
        then do
            isMeta <- isMetaDir dname
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

    mkPath :: Array Word8 -> PosixPath
    mkPath = Path.unsafeFromChunk

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

            name <- Array.fromCString (castPtr dname)
            let path = Path.append curdir (mkPath name)

            if (dtype == (#const DT_DIR))
            then do
                isMeta <- liftIO $ isMetaDir dname
                if isMeta
                then return $ Skip st
                else let dirs1 = path : dirs
                         ndirs1 = ndirs + 1
                      in if ndirs1 >= dirMax
                         then return $ Yield (Left dirs1)
                            (ChunkStreamLoop curdir xs dirp [] 0 files nfiles)
                         else return $ Skip
                            (ChunkStreamLoop curdir xs dirp dirs1 ndirs1 files nfiles)
            else let files1 = path : files
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
                liftIO $ closeDirStream (DirStream dirp)
                if (n == 0)
                then return $ Skip (ChunkStreamInit xs dirs ndirs files nfiles)
                else liftIO $ throwErrno "readEitherChunks"

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

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

    mkPath :: Array Word8 -> PosixPath
    mkPath = Path.unsafeFromChunk

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
        mbarr <- liftIO $ MutByteArray.pinnedNew bufSize
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
        mbarr1 <- liftIO $ MutByteArray.pinnedNew bufSize
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
            if (dtype /= (#const DT_DIR))
            then do
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 ->
                            return $ Skip
                                (ChunkStreamByteLoop curdir xs dirp dirs ndirs mbarr pos1)
                        Nothing -> do
                            if ndirs > 0
                            then
                                return $ Yield (Left dirs)
                                    (ChunkStreamByteLoopPending dname curdir xs dirp mbarr pos)
                            else
                                return $ Skip
                                    (ChunkStreamByteLoopPending dname curdir xs dirp mbarr pos)
            else do
                isMeta <- liftIO $ isMetaDir dname
                if isMeta
                then return $ Skip st
                else do
                    name <- Array.fromCString (castPtr dname)
                    let path = Path.append curdir (mkPath name)
                        dirs1 = path : dirs
                        ndirs1 = ndirs + 1
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 ->
                            return $ Skip
                                (ChunkStreamByteLoop curdir xs dirp dirs1 ndirs1 mbarr pos1)
                        Nothing -> do
                            -- We know dirs1 in not empty here
                            return $ Yield (Left dirs1)
                                (ChunkStreamByteLoopPending dname curdir xs dirp mbarr pos)
        else do
            errno <- liftIO getErrno
            if (errno == eINTR)
            then return $ Skip st
            else do
                let (Errno n) = errno
                liftIO $ closeDirStream (DirStream dirp)
                if (n == 0)
                then return $ Skip (ChunkStreamByteInit xs dirs ndirs mbarr pos)
                else liftIO $ throwErrno "readEitherByteChunks"
#endif
