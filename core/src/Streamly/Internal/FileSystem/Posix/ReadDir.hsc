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
      ReadOptions
    , followSymlinks
    , ignoreNonExisting
    , ignoreLoopErrors
    , ignoreInAccessible
    , defaultReadOptions

    , readScanWith_
    , readScanWith
    , readPlusScanWith

    , DirStream (..)
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
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord)
import Foreign
    ( Ptr, Word8, nullPtr, peek, peekByteOff, castPtr, plusPtr, (.&.)
    , allocaBytes
    )
import Foreign.C
    ( resetErrno, throwErrno, throwErrnoIfMinus1Retry_, throwErrnoIfNullRetry
    , Errno(..), getErrno, eINTR, eNOENT, eACCES, eLOOP
    , CInt(..), CString, CChar, CSize(..)
    )
import Foreign.Storable (poke)
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.Stream (Stream(..), Step(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.FileSystem.Path (Path)
import Streamly.Internal.FileSystem.Posix.Errno (throwErrnoPathIfNullRetry)
import Streamly.Internal.FileSystem.Posix.File
    (defaultOpenFlags, openAt, close)
import Streamly.Internal.FileSystem.PosixPath (PosixPath(..))
import System.Posix.Types (Fd(..), CMode)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import qualified Streamly.Internal.Data.Unfold as UF (bracketIO)
import qualified Streamly.Internal.FileSystem.Path.Common as PathC
import qualified Streamly.Internal.FileSystem.PosixPath as Path

#include <dirent.h>
#include <sys/stat.h>

-------------------------------------------------------------------------------

data {-# CTYPE "DIR" #-} CDir
data {-# CTYPE "struct dirent" #-} CDirent
data {-# CTYPE "struct stat" #-} CStat

newtype DirStream = DirStream (Ptr CDir)

data ReadOptions =
    ReadOptions
    { _followSymlinks :: Bool
    , _ignoreSymlinkLoopErrors :: Bool
    , _ignoreNonExistingFiles :: Bool
    , _ignoreInAccessibleFiles :: Bool
    }

followSymlinks :: Bool -> ReadOptions -> ReadOptions
followSymlinks x opts = opts {_followSymlinks = x}

ignoreLoopErrors :: Bool -> ReadOptions -> ReadOptions
ignoreLoopErrors x opts = opts {_ignoreSymlinkLoopErrors = x}

ignoreNonExisting :: Bool -> ReadOptions -> ReadOptions
ignoreNonExisting x opts = opts {_ignoreNonExistingFiles = x}

ignoreInAccessible :: Bool -> ReadOptions -> ReadOptions
ignoreInAccessible x opts = opts {_ignoreInAccessibleFiles = x}

-- NOTE: The defaultReadOptions emulate the behaviour of "find".
--
defaultReadOptions :: ReadOptions
defaultReadOptions =
    ReadOptions
    { _followSymlinks = False
    , _ignoreSymlinkLoopErrors = False
    , _ignoreNonExistingFiles = False
    , _ignoreInAccessibleFiles = False
    }

-- | Minimal read without any metadata.
{-# INLINE readScanWith_ #-}
readScanWith_ :: -- (MonadIO m, MonadCatch m) =>
       Scanl m (Path, CString) a
    -> (ReadOptions -> ReadOptions)
    -> Path
    -> Stream m a
readScanWith_ = undefined

-- | Read with essential metadata. The scan takes the parent dir, the child
-- name, the child metadata and produces an output. The scan can do filtering,
-- formatting of the output, colorizing the output etc.
--
-- The options are to ignore errors encountered when reading a path, turn the
-- errors into a nil stream instead.
{-# INLINE readScanWith #-}
readScanWith :: -- (MonadIO m, MonadCatch m) =>
       Scanl m (Path, CString, Ptr CDirent) a
    -> (ReadOptions -> ReadOptions)
    -> Path
    -> Stream m a
readScanWith = undefined

-- NOTE: See  https://www.manpagez.com/man/2/getattrlistbulk/ for BSD/macOS.

-- | Read with full metadata.
{-# INLINE readPlusScanWith #-}
readPlusScanWith :: -- (MonadIO m, MonadCatch m) =>
       Scanl m (Path, CString, Ptr CStat) a
    -> (ReadOptions -> ReadOptions)
    -> Path
    -> Stream m a
readPlusScanWith = undefined

-------------------------------------------------------------------------------
-- readdir operations
-------------------------------------------------------------------------------

-- XXX Marking the calls "safe" has significant perf impact because it runs on
-- a separate OS thread. "unsafe" is faster but can block the GC if the system
-- call blocks. The effect could be signifcant if the file system is on NFS. Is
-- it possible to have a faster safe - where we know the function is safe but
-- we run it on the current thread, and if it blocks for longer we can snatch
-- the capability and enable GC?
--
-- IMPORTANT NOTE: Use capi FFI for all readdir APIs. This is required at
-- least on macOS for correctness. We saw random directory entries when ccall
-- was used on macOS 15.3. Looks like it was picking the wrong version of
-- dirent structure. Did not see the problem in CIs on macOS 14.7.2 though.
foreign import capi unsafe "closedir"
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
foreign import capi unsafe "dirent.h readdir"
    c_readdir  :: Ptr CDir -> IO (Ptr CDirent)

--------------------------------------------------------------------------------
-- Stat
--------------------------------------------------------------------------------

foreign import ccall unsafe "stat.h lstat"
    c_lstat :: CString -> Ptr CStat -> IO CInt

foreign import ccall unsafe "stat.h stat"
    c_stat :: CString -> Ptr CStat -> IO CInt

s_IFMT :: CMode
s_IFMT  = #{const S_IFMT}

s_IFDIR :: CMode
s_IFDIR = #{const S_IFDIR}

{-
s_IFREG :: CMode
s_IFREG = #{const S_IFREG}

s_IFLNK :: CMode
s_IFLNK = #{const S_IFLNK}
-}

-- NOTE: Using fstatat with a dirfd and relative path would be faster.
stat :: Bool -> CString -> IO (Either Errno CMode)
stat followSym cstr =
    allocaBytes #{size struct stat} $ \p_stat -> do
        resetErrno
        result <-
            if followSym
            then c_stat cstr p_stat
            else c_lstat cstr p_stat
        if result /= 0
        then do
            errno <- getErrno
            if errno == eINTR
            then stat followSym cstr
            else pure $ Left errno
        else do
            mode <- #{peek struct stat, st_mode} p_stat
            pure $ Right (mode .&. s_IFMT)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

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
    Array.asCStringUnsafe (Path.toArray p) $ \s -> do
        -- openDirStreamCString s
        dirp <- throwErrnoPathIfNullRetry "openDirStream" p $ c_opendir s
        return (DirStream dirp)

-- | Note that the supplied Fd is used by DirStream and when we close the
-- DirStream the fd will be closed.
openDirStreamAt :: Fd -> PosixPath -> IO DirStream
openDirStreamAt fd p = do
    -- XXX can pass O_DIRECTORY here, is O_NONBLOCK useful for dirs?
    -- Note this fd is not automatically closed, we have to take care of
    -- exceptions and closing the fd.
    fd1 <- openAt (Just fd) p defaultOpenFlags Nothing
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

-------------------------------------------------------------------------------
-- determining filetype
-------------------------------------------------------------------------------

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

data EntryType = EntryIsDir | EntryIsNotDir | EntryIgnored

{-# NOINLINE statEntryType #-}
statEntryType
    :: ReadOptions -> PosixPath -> Ptr CChar -> IO EntryType
statEntryType conf parent dname = do
    -- XXX We can create a pinned array right here since the next call pins
    -- it anyway.
    path <- appendCString parent dname
    Array.asCStringUnsafe (Path.toArray path) $ \cStr -> do
        res <- stat (_followSymlinks conf) cStr
        case res of
            Right mode -> pure $
                if (mode == s_IFDIR)
                then EntryIsDir
                else EntryIsNotDir
            Left errno -> do
                if errno == eNOENT
                then unless (_ignoreNonExistingFiles conf) $
                         throwErrno (errMsg path)
                else if errno == eACCES
                then unless (_ignoreInAccessibleFiles conf) $
                         throwErrno (errMsg path)
                else if errno == eLOOP
                then unless (_ignoreSymlinkLoopErrors conf) $
                         throwErrno (errMsg path)
                else throwErrno (errMsg path)
                pure $ EntryIgnored
    where

    errMsg path =
        let pathStr = Path.toString_ path
         in "statEntryType: " ++ pathStr

-- | Checks if dname is a directory, not dir or should be ignored.
{-# INLINE getEntryType #-}
getEntryType
    :: ReadOptions
    -> PosixPath -> Ptr CChar -> #{type unsigned char} -> IO EntryType
getEntryType conf parent dname dtype = do
    let needStat =
#ifdef FORCE_LSTAT_READDIR
            True
#else
            (dtype == (#const DT_LNK) && _followSymlinks conf)
                || dtype == #const DT_UNKNOWN
#endif

    if dtype /= (#const DT_DIR) && not needStat
    then pure EntryIsNotDir
    else do
        isMeta <- liftIO $ isMetaDir dname
        if isMeta
        then pure EntryIgnored
        else if dtype == (#const DT_DIR)
        then pure EntryIsDir
        else statEntryType conf parent dname

-------------------------------------------------------------------------------
-- streaming reads
-------------------------------------------------------------------------------

-- XXX We can use getdents64 directly so that we can use array slices from the
-- same buffer that we passed to the OS. That way we can also avoid any
-- overhead of bracket.
-- XXX Make this as Unfold to avoid returning Maybe
-- XXX Or NOINLINE some parts and inline the rest to fuse it
-- {-# INLINE readDirStreamEither #-}
readDirStreamEither ::
    -- DirStream -> IO (Either (Rel (Dir Path)) (Rel (File Path)))
    (ReadOptions -> ReadOptions) ->
    (PosixPath, DirStream) -> IO (Maybe (Either PosixPath PosixPath))
readDirStreamEither confMod (curdir, (DirStream dirp)) = loop

  where

  conf = confMod defaultReadOptions

  -- mkPath :: IsPath (Rel (a Path)) => Array Word8 -> Rel (a Path)
  -- {-# INLINE mkPath #-}
  mkPath :: Array Word8 -> PosixPath
  mkPath = Path.unsafeFromArray

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
        etype <- getEntryType conf curdir dname dtype
        case etype of
            EntryIsDir -> return (Just (Left (mkPath name)))
            EntryIsNotDir -> return (Just (Right (mkPath name)))
            EntryIgnored -> loop
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
    (ReadOptions -> ReadOptions) ->
    Unfold m (PosixPath, DirStream) (Either Path Path)
streamEitherReader confMod = Unfold step return
    where

    step s = do
        r <- liftIO $ readDirStreamEither confMod s
        case r of
            Nothing -> return Stop
            Just x -> return $ Yield x s

{-# INLINE streamReader #-}
streamReader
    :: MonadIO m
    => (ReadOptions -> ReadOptions) -> Unfold m (PosixPath, DirStream) Path
streamReader confMod = fmap (either id id) (streamEitherReader confMod)

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
reader :: (MonadIO m, MonadCatch m)
       => (ReadOptions -> ReadOptions) -> Unfold m Path Path
reader confMod =
    -- XXX Instead of using bracketIO for each iteration of the loop we should
    -- instead yield a buffer of dir entries in each iteration and then use an
    -- unfold and concat to flatten those entries. That should improve the
    -- performance.
    UF.bracketIO before after (streamReader confMod)

-- | Read directories as Left and files as Right. Filter out "." and ".."
-- entries.
--
--  /Internal/
--
{-# INLINE eitherReader #-}
eitherReader :: (MonadIO m, MonadCatch m) =>
    (ReadOptions -> ReadOptions) -> Unfold m Path (Either Path Path)
eitherReader confMod =
    -- XXX The measured overhead of bracketIO is not noticeable, if it turns
    -- out to be a problem for small filenames we can use getdents64 to use
    -- chunked read to avoid the overhead.
    UF.bracketIO before after (streamEitherReader confMod)

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
-- A fold may be useful to translate the output to whatever format we want, we
-- can add a prefix or we can colorize it. The Right output would be the output
-- of the fold which can be any type not just a Path.

-- XXX We can write a two fold scan to buffer and yield whichever fills first
-- like foldMany, it would be foldEither.
{-# INLINE readEitherChunks #-}
readEitherChunks
    :: MonadIO m
    => (ReadOptions -> ReadOptions)
    -> [PosixPath] -> Stream m (Either [PosixPath] [PosixPath])
readEitherChunks confMod alldirs =
    Stream step (ChunkStreamInit alldirs [] 0 [] 0)

    where

    conf = confMod defaultReadOptions

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

            etype <- liftIO $ getEntryType conf curdir dname dtype
            case etype of
                EntryIsDir -> do
                     path <- liftIO $ appendCString curdir dname
                     let dirs1 = path : dirs
                         ndirs1 = ndirs + 1
                      in if ndirs1 >= dirMax
                         then return $ Yield (Left dirs1)
                            (ChunkStreamLoop curdir xs dirp [] 0 files nfiles)
                         else return $ Skip
                            (ChunkStreamLoop curdir xs dirp dirs1 ndirs1 files nfiles)
                EntryIsNotDir -> do
                 path <- liftIO $ appendCString curdir dname
                 let files1 = path : files
                     nfiles1 = nfiles + 1
                  in if nfiles1 >= fileMax
                     then return $ Yield (Right files1)
                        (ChunkStreamLoop curdir xs dirp dirs ndirs [] 0)
                     else return $ Skip
                        (ChunkStreamLoop curdir xs dirp dirs ndirs files1 nfiles1)
                EntryIgnored -> return $ Skip st
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
        PosixPath -- current dir path
        [PosixPath]  -- remaining dirs
        (Ptr CDir) -- current dir stream
        MutByteArray
        Int
    | ChunkStreamReallocBuf
        (Ptr CChar) -- pending item
        PosixPath -- current dir path
        [PosixPath]  -- remaining dirs
        (Ptr CDir) -- current dir stream
        MutByteArray
        Int
    | ChunkStreamDrainBuf
        MutByteArray
        Int

-- XXX Detect cycles. ELOOP can be used to avoid cycles, but we can also detect
-- them proactively.

-- XXX Since we are separating paths by newlines, it cannot support newlines in
-- paths. Or we can return null separated paths as well. Provide a Mut array
-- API to replace the nulls with newlines in-place.
--
-- We can pass a fold to make this modular, but if we are passing readdir
-- managed memory then we will have to consume it immediately. Otherwise we can
-- use getdents64 directly and use GHC managed memory instead.
--
-- A fold may be useful to translate the output to whatever format we want, we
-- can add a prefix or we can colorize it.
--
-- XXX Use bufSize, recursive traversal, split strategy, output entries
-- separator as config options. When not using concurrently we do not need to
-- split the work at all.
--
-- XXX Currently we are quite aggressive in splitting the work because we have
-- no knowledge of whether we need to or not. But this leads to more overhead.
-- Instead, we can measure the coarse monotonic and process cpu time after
-- every n system calls or n iterations. If the cpu utilization is low then
-- yield the dirs otherwise dont. We can use an async thread for computing cpu
-- utilization periodically and all other threads can just read it from an
-- IORef. So this can be shared across all such consumers.

-- | This function may not traverse all the directories supplied and it may
-- traverse the directories recursively. Left contains those directories that
-- were not traversed by this function, these my be the directories that were
-- supplied as input as well as newly discovered directories during traversal.
-- To traverse the entire tree we have to iterate this function on the Left
-- output.
--
-- Right is a buffer containing directories and files separated by newlines.
--
{-# INLINE readEitherByteChunks #-}
readEitherByteChunks :: MonadIO m =>
    (ReadOptions -> ReadOptions) ->
    [PosixPath] -> Stream m (Either [PosixPath] (Array Word8))
readEitherByteChunks confMod alldirs =
    Stream step ChunkStreamByteInit

    where

    conf = confMod defaultReadOptions

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

    bufSize = 32000

    copyToBuf dstArr pos dirPath name = do
        nameLen <- fmap fromIntegral (liftIO $ c_strlen name)
        -- We know it is already pinned.
        MutByteArray.unsafeAsPtr dstArr (\ptr -> liftIO $ do
            -- XXX We may need to decode and encode the path if the
            -- output encoding differs from fs encoding.
            let PosixPath (Array dirArr start end) = dirPath
                dirLen = end - start
                endDir = pos + dirLen
                endPos = endDir + nameLen + 2 -- sep + newline
                sepOff = ptr `plusPtr` endDir -- separator offset
                nameOff = sepOff `plusPtr` 1  -- file name offset
                nlOff = nameOff `plusPtr` nameLen -- newline offset
                separator = 47 :: Word8
                newline = 10 :: Word8
            if (endPos < bufSize)
            then do
                -- XXX We can keep a trailing separator on the dir itself.
                MutByteArray.unsafePutSlice dirArr start dstArr pos dirLen
                poke sepOff separator
                _ <- c_memcpy nameOff (castPtr name) (fromIntegral nameLen)
                poke nlOff newline
                return (Just endPos)
            else return Nothing
            )

    step _ ChunkStreamByteInit = do
        mbarr <- liftIO $ MutByteArray.new' bufSize
        case alldirs of
            (x:xs) -> do
                DirStream dirp <- liftIO $ openDirStream x
                return $ Skip $ ChunkStreamByteLoop x xs dirp mbarr 0
            [] -> return Stop

    step _ ChunkStreamByteStop = return Stop

    step _ (ChunkStreamReallocBuf pending curdir xs dirp mbarr pos) = do
        mbarr1 <- liftIO $ MutByteArray.new' bufSize
        r1 <- copyToBuf mbarr1 0 curdir pending
        case r1 of
            Just pos2 ->
                return $ Yield (Right (Array mbarr 0 pos))
                    -- When we come in this state we have emitted dirs
                    (ChunkStreamByteLoop curdir xs dirp mbarr1 pos2)
            Nothing -> error "Dirname too big for bufSize"

    step _ (ChunkStreamDrainBuf mbarr pos) =
        if pos == 0
        then return Stop
        else return $ Yield (Right (Array mbarr 0 pos)) ChunkStreamByteStop

    step _ (ChunkStreamByteLoop icurdir ixs idirp mbarr ipos) = do
        goOuter icurdir idirp ixs ipos

        where

        -- This is recursed only when we open the next dir
        -- Encapsulates curdir and dirp as static arguments
        goOuter curdir dirp = goInner

            where

            -- This is recursed each time we find a dir
            -- Encapsulates dirs as static argument
            goInner dirs = nextEntry

                where

                {-# INLINE nextEntry #-}
                nextEntry pos = do
                    liftIO resetErrno
                    dentPtr <- liftIO $ c_readdir dirp
                    if dentPtr /= nullPtr
                    then handleDentry pos dentPtr
                    else handleErr pos

                openNextDir pos =
                    case dirs of
                        (x:xs) -> do
                            DirStream dirp1 <- liftIO $ openDirStream x
                            goOuter x dirp1 xs pos
                        [] ->
                            if pos == 0
                            then return Stop
                            else return
                                    $ Yield
                                        (Right (Array mbarr 0 pos))
                                        ChunkStreamByteStop

                handleErr pos = do
                    errno <- liftIO getErrno
                    if (errno /= eINTR)
                    then do
                        let (Errno n) = errno
                        liftIO $ closeDirStream (DirStream dirp)
                        if (n == 0)
                        then openNextDir pos
                        else liftIO $ throwErrno "readEitherByteChunks"
                    else nextEntry pos

                splitAndRealloc pos dname xs =
                    case xs of
                        [] ->
                            return $ Skip
                                (ChunkStreamReallocBuf dname curdir
                                    [] dirp mbarr pos)
                        _ -> do
                            let (h,t) = splitHalf xs
                            return $ Yield (Left t)
                                (ChunkStreamReallocBuf dname curdir
                                    h dirp mbarr pos)

                {-# INLINE handleFileEnt #-}
                handleFileEnt pos dname = do
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 -> nextEntry pos1
                        Nothing -> splitAndRealloc pos dname dirs

                {-# INLINE handleDirEnt #-}
                handleDirEnt pos dname = do
                    path <- liftIO $ appendCString curdir dname
                    let dirs1 = path : dirs
                    r <- copyToBuf mbarr pos curdir dname
                    case r of
                        Just pos1 -> goInner dirs1 pos1
                        Nothing -> splitAndRealloc pos dname dirs1

                handleDentry pos dentPtr = do
                    let dname = #{ptr struct dirent, d_name} dentPtr
                    dtype :: #{type unsigned char} <-
                        liftIO $ #{peek struct dirent, d_type} dentPtr

                    etype <- liftIO $ getEntryType conf curdir dname dtype
                    case etype of
                        EntryIsNotDir -> handleFileEnt pos dname
                        EntryIsDir -> handleDirEnt pos dname
                        EntryIgnored -> nextEntry pos

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
readEitherByteChunksAt :: MonadIO m => (ReadOptions -> ReadOptions) ->
       -- (parent dir path, child dir paths rel to parent)
       (PosixPath, [PosixPath])
    -> Stream m (Either (PosixPath, [PosixPath]) (Array Word8))
readEitherByteChunksAt confMod (ppath, alldirs) =
    Stream step (ByteChunksAtInit0)

    where
    conf = confMod defaultReadOptions

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
        -- Note this fd is not automatically closed, we have to take care of
        -- exceptions and closing the fd.
        pfd <- liftIO $ openAt Nothing ppath defaultOpenFlags Nothing
        mbarr <- liftIO $ MutByteArray.new' bufSize
        return $ Skip (ByteChunksAtInit pfd alldirs mbarr 0)

    step _ (ByteChunksAtInit ph (x:xs) mbarr pos) = do
        (DirStream dirp) <- liftIO $ openDirStreamAt ph x
        return $ Skip (ByteChunksAtLoop ph dirp x xs [] 0 mbarr pos)

    step _ (ByteChunksAtInit pfd [] _ 0) = do
        liftIO $ close (pfd)
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
            etype <- liftIO $ getEntryType conf curdir dname dtype
            case etype of
                EntryIsNotDir -> do
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
                EntryIsDir -> do
                    arr <- Array.fromCString (castPtr dname)
                    let path = Path.unsafeFromArray arr
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
                                let fpath = Path.unsafeExtend ppath curdir
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
                EntryIgnored ->  return $ Skip st
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
                        let fpath = Path.unsafeExtend ppath curdir
                        return $ Yield
                            (Left (fpath, dirs))
                            (ByteChunksAtInit pfd xs mbarr pos)
                    else return $ Skip (ByteChunksAtInit pfd xs mbarr pos)
                else liftIO $ throwErrno "readEitherByteChunks"
#endif
