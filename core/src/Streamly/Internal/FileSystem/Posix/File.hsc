module Streamly.Internal.FileSystem.Posix.File
    (
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

    -- * File open flags
      OpenFlags (..)
    , defaultOpenFlags

    -- * File status flags
    , setAppend
    , setNonBlock
    , setSync

    -- * File creation flags
    , setCloExec
    , setDirectory
    , setExcl
    , setNoCtty
    , setNoFollow
    -- setTmpFile
    , setTrunc

    -- * File create mode
    , defaultCreateMode

    -- ** User Permissions
    , setUr
    , setUw
    , setUx

    , clrUr
    , clrUw
    , clrUx

    -- ** Group Permissions
    , setGr
    , setGw
    , setGx

    , clrGr
    , clrGw
    , clrGx

    -- ** Other Permissions
    , setOr
    , setOw
    , setOx

    , clrOr
    , clrOw
    , clrOx

    -- ** Status bits
    , setSuid
    , setSgid
    , setSticky

    , clrSuid
    , clrSgid
    , clrSticky

    -- * Fd based Low Level
    , openAt
    , close

    -- * Handle based
    , openFile
    , withFile
    , openBinaryFile
    , withBinaryFile

    -- Re-exported
    , Fd
#endif
    ) where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Data.Bits ((.|.), (.&.), complement)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import GHC.IO.Handle.FD (fdToHandle)
import Streamly.Internal.FileSystem.Posix.Errno (throwErrnoPathIfMinus1Retry)
import Streamly.Internal.FileSystem.PosixPath (PosixPath)
import System.IO (IOMode(..), Handle)
import System.Posix.Types (Fd(..), CMode(..))

import qualified Streamly.Internal.FileSystem.File.Common as File
import qualified Streamly.Internal.FileSystem.PosixPath as Path

-- We want to remain close to the Posix C API. A function based API to set and
-- clear the modes is simple, type safe and directly mirrors the C API. It does
-- not require explicit mapping from Haskell ADT to C types, we can dirctly
-- manipulate the C type.

#include <fcntl.h>

-------------------------------------------------------------------------------
-- Create mode
-------------------------------------------------------------------------------

-- | Open flags, see posix open system call man page.
newtype FileMode = FileMode CMode

##define MK_MODE_API(name1,name2,x) \
{-# INLINE name1 #-}; \
name1 :: FileMode -> FileMode; \
name1 (FileMode mode) = FileMode (x .|. mode); \
{-# INLINE name2 #-}; \
name2 :: FileMode -> FileMode; \
name2 (FileMode mode) = FileMode (x .&. complement mode)

{-
#define S_ISUID  0004000
#define S_ISGID  0002000
#define S_ISVTX  0001000

#define S_IRWXU 00700
#define S_IRUSR 00400
#define S_IWUSR 00200
#define S_IXUSR 00100

#define S_IRWXG 00070
#define S_IRGRP 00040
#define S_IWGRP 00020
#define S_IXGRP 00010

#define S_IRWXO 00007
#define S_IROTH 00004
#define S_IWOTH 00002
#define S_IXOTH 00001

#define AT_FDCWD (-100)
-}

MK_MODE_API(setSuid,clrSuid,S_ISUID)
MK_MODE_API(setSgid,clrSgid,S_ISGID)
MK_MODE_API(setSticky,clrSticky,S_ISVTX)

-- MK_MODE_API(setUrwx,clrUrwx,S_IRWXU)
MK_MODE_API(setUr,clrUr,S_IRUSR)
MK_MODE_API(setUw,clrUw,S_IWUSR)
MK_MODE_API(setUx,clrUx,S_IXUSR)

-- MK_MODE_API(setGrwx,clrGrwx,S_IRWXU)
MK_MODE_API(setGr,clrGr,S_IRUSR)
MK_MODE_API(setGw,clrGw,S_IWUSR)
MK_MODE_API(setGx,clrGx,S_IXUSR)

-- MK_MODE_API(setOrwx,clrOrwx,S_IRWXU)
MK_MODE_API(setOr,clrOr,S_IRUSR)
MK_MODE_API(setOw,clrOw,S_IWUSR)
MK_MODE_API(setOx,clrOx,S_IXUSR)

-- Uses the same default mode as openFileWith in base
defaultCreateMode :: FileMode
defaultCreateMode = FileMode 0o666

-------------------------------------------------------------------------------
-- Open Flags
-------------------------------------------------------------------------------

-- | Open flags, see posix open system call man page.
newtype OpenFlags = OpenFlags CInt

##define MK_FLAG_API(name,x) \
{-# INLINE name #-}; \
name :: OpenFlags -> OpenFlags; \
name (OpenFlags flags) = OpenFlags (flags .|. x)

-- foreign import ccall unsafe "HsBase.h __hscore_o_rdonly" o_RDONLY :: CInt
-- These affect the first two bits in flags.
MK_FLAG_API(setReadOnly,#{const O_RDONLY})
MK_FLAG_API(setWriteOnly,#{const O_WRONLY})
MK_FLAG_API(setReadWrite,#{const O_RDWR})

##define MK_BOOL_FLAG_API(name,x) \
{-# INLINE name #-}; \
name :: Bool -> OpenFlags -> OpenFlags; \
name True (OpenFlags flags) = OpenFlags (flags .|. x); \
name False (OpenFlags flags) = OpenFlags (flags .&. complement x)

-- setCreat is internal only, do not export this. This is automatically set
-- when create mode is passed, otherwise cleared.
MK_BOOL_FLAG_API(setCreat,#{const O_CREAT})

MK_BOOL_FLAG_API(setExcl,#{const O_EXCL})
MK_BOOL_FLAG_API(setNoCtty,#{const O_NOCTTY})
MK_BOOL_FLAG_API(setTrunc,#{const O_TRUNC})
MK_BOOL_FLAG_API(setAppend,#{const O_APPEND})
MK_BOOL_FLAG_API(setNonBlock,#{const O_NONBLOCK})
MK_BOOL_FLAG_API(setDirectory,#{const O_DIRECTORY})
MK_BOOL_FLAG_API(setNoFollow,#{const O_NOFOLLOW})
MK_BOOL_FLAG_API(setCloExec,#{const O_CLOEXEC})
MK_BOOL_FLAG_API(setSync,#{const O_SYNC})

-- | Default values for the 'OpenFlags'.
--
-- By default a 0 value is used, no flag is set. See the open system call man
-- page.
defaultOpenFlags :: OpenFlags
defaultOpenFlags = OpenFlags 0

-------------------------------------------------------------------------------
-- Low level (fd returning) file opening APIs
-------------------------------------------------------------------------------

-- XXX Should we use interruptible open as in base openFile?
foreign import capi unsafe "fcntl.h openat"
   c_openat :: CInt -> CString -> CInt -> CMode -> IO CInt

-- | Open and optionally create (when create mode is specified) a file relative
-- to an optional directory file descriptor. If directory fd is not specified
-- then opens relative to the current directory.
-- {-# INLINE openAtCString #-}
openAtCString ::
       Maybe Fd -- ^ Optional directory file descriptor
    -> CString -- ^ Pathname to open
    -> OpenFlags -- ^ Append, exclusive, etc.
    -> Maybe FileMode -- ^ Create mode
    -> IO Fd
openAtCString fdMay path flags cmode =
    Fd <$> c_openat c_fd path flags1 mode

    where

    c_fd = maybe (#{const AT_FDCWD}) (\ (Fd fd) -> fd) fdMay
    FileMode mode = maybe defaultCreateMode id cmode
    OpenFlags flags1 = maybe flags (\_ -> setCreat True flags) cmode

-- | Open a file relative to an optional directory file descriptor.
--
-- Note: In Haskell, using an fd directly for IO may be problematic as blocking
-- file system operations on the file might block the capability and GC for
-- "unsafe" calls. "safe" calls may be more expensive. Also, you may have to
-- synchronize concurrent access via multiple threads.
--
{-# INLINE openAt #-}
openAt ::
       Maybe Fd -- ^ Optional directory file descriptor
    -> PosixPath -- ^ Pathname to open
    -> OpenFlags -- ^ Append, exclusive, truncate, etc.
    -> Maybe FileMode -- ^ Create mode
    -> IO Fd
openAt fdMay path flags cmode =
   Path.asCString path $ \cstr -> do
     throwErrnoPathIfMinus1Retry "openAt" path
        $ openAtCString fdMay cstr flags cmode


-- | Open a regular file, return an Fd.
--
-- Sets O_NOCTTY, O_NONBLOCK flags to be compatible with the base openFile
-- behavior. O_NOCTTY affects opening of terminal special files and O_NONBLOCK
-- affects fifo special files, and mandatory locking.
--
openFileFdWith :: OpenFlags -> PosixPath -> IOMode -> IO Fd
openFileFdWith oflags path iomode = do
    case iomode of
        ReadMode -> open1 (setReadOnly oflags1) Nothing
        WriteMode ->
            open1 (setWriteOnly oflags1) (Just defaultCreateMode)
        AppendMode ->
            open1
                ((setAppend True . setWriteOnly) oflags1)
                (Just defaultCreateMode)
        ReadWriteMode ->
            open1 (setReadWrite oflags) (Just defaultCreateMode)

    where

    oflags1 = setNoCtty True $ setNonBlock True oflags
    open1 = openAt Nothing path

openFileFd :: PosixPath -> IOMode -> IO Fd
openFileFd = openFileFdWith defaultOpenFlags

foreign import ccall unsafe "unistd.h close"
   c_close :: CInt -> IO CInt

close :: Fd -> IO ()
close (Fd fd) = throwErrnoIfMinus1_ ("close " ++ show fd) (c_close fd)

-------------------------------------------------------------------------------
-- base openFile compatible, Handle returning, APIs
-------------------------------------------------------------------------------

-- | Open a regular file, return a Handle. The file is locked, the Handle is
-- NOT set up to close the file on garbage collection.
{-# INLINE openFileHandle #-}
openFileHandle :: PosixPath -> IOMode -> IO Handle
openFileHandle p x = openFileFd p x >>= fdToHandle . fromIntegral

-- | Like openFile in base package but using Path instead of FilePath.
-- Use hSetBinaryMode on the handle if you want to use binary mode.
openFile :: PosixPath -> IOMode -> IO Handle
openFile = File.openFile False openFileHandle

-- | Like withFile in base package but using Path instead of FilePath.
-- Use hSetBinaryMode on the handle if you want to use binary mode.
withFile :: PosixPath -> IOMode -> (Handle -> IO r) -> IO r
withFile = File.withFile False openFileHandle

-- | Like openBinaryFile in base package but using Path instead of FilePath.
openBinaryFile :: PosixPath -> IOMode -> IO Handle
openBinaryFile = File.openFile True openFileHandle

-- | Like withBinaryFile in base package but using Path instead of FilePath.
withBinaryFile :: PosixPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = File.withFile True openFileHandle
#endif
