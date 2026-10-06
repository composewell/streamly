module Streamly.Internal.Syscall.Posix.File
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

import Control.Exception (onException)
import Control.Monad (when)
import Data.Bits ((.|.), (.&.), complement)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import GHC.IO.Device (IODeviceType(..))
import GHC.IO.Handle.FD (mkHandleFromFD)
import Streamly.Internal.Syscall.Posix.Errno (throwErrnoPathIfMinus1Retry)
import Streamly.Internal.FileSystem.PosixPath (PosixPath)
import System.IO (IOMode(..), Handle)
import System.Posix.Types (Fd(..), CMode(..))
##if defined(javascript_HOST_ARCH)
import System.Posix.Internals
    ( o_APPEND, o_CREAT, o_EXCL, o_NOCTTY, o_NONBLOCK, o_RDONLY, o_RDWR
    , o_TRUNC, o_WRONLY )
import qualified System.Posix.Internals as Posix (c_close, c_open)
##endif

import qualified GHC.IO.Device as Device
import qualified GHC.IO.FD as FD
import qualified Streamly.Internal.FileSystem.File.Common as File
import qualified Streamly.Internal.FileSystem.PosixPath as Path
import qualified System.Posix.Internals as Posix

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

-- The open of the JavaScript runtime interprets the flags using the values
-- in System.Posix.Internals, not those in the C headers, and ignores the
-- flags that it does not support.
##if defined(javascript_HOST_ARCH)
##define O_FLAG(c,js) (js)
##else
##define O_FLAG(c,js) (c)
##endif

-- These affect the first two bits in flags.
MK_FLAG_API(setReadOnly,O_FLAG(#{const O_RDONLY},o_RDONLY))
MK_FLAG_API(setWriteOnly,O_FLAG(#{const O_WRONLY},o_WRONLY))
MK_FLAG_API(setReadWrite,O_FLAG(#{const O_RDWR},o_RDWR))

##define MK_BOOL_FLAG_API(name,x) \
{-# INLINE name #-}; \
name :: Bool -> OpenFlags -> OpenFlags; \
name True (OpenFlags flags) = OpenFlags (flags .|. x); \
name False (OpenFlags flags) = OpenFlags (flags .&. complement x)

-- setCreat is internal only, do not export this. This is automatically set
-- when create mode is passed, otherwise cleared.
MK_BOOL_FLAG_API(setCreat,O_FLAG(#{const O_CREAT},o_CREAT))

MK_BOOL_FLAG_API(setExcl,O_FLAG(#{const O_EXCL},o_EXCL))
MK_BOOL_FLAG_API(setNoCtty,O_FLAG(#{const O_NOCTTY},o_NOCTTY))
MK_BOOL_FLAG_API(setTrunc,O_FLAG(#{const O_TRUNC},o_TRUNC))
MK_BOOL_FLAG_API(setAppend,O_FLAG(#{const O_APPEND},o_APPEND))
MK_BOOL_FLAG_API(setNonBlock,O_FLAG(#{const O_NONBLOCK},o_NONBLOCK))
MK_BOOL_FLAG_API(setDirectory,O_FLAG(#{const O_DIRECTORY},0))
MK_BOOL_FLAG_API(setNoFollow,O_FLAG(#{const O_NOFOLLOW},0))
MK_BOOL_FLAG_API(setCloExec,O_FLAG(#{const O_CLOEXEC},0))
MK_BOOL_FLAG_API(setSync,O_FLAG(#{const O_SYNC},0))

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
   c_openat_ :: CInt -> CString -> CInt -> CMode -> IO CInt

-- The JavaScript runtime opens a file synchronously when called via openat,
-- and closes it asynchronously when called by base. The callback of the
-- asynchronous close removes the descriptor from the runtime's table of open
-- files after the descriptor is freed, when a synchronous open has reused the
-- descriptor by then the new entry is removed, and closing the new file fails
-- with EINVAL. Open and close the way base does, asynchronously.
c_openat :: CInt -> CString -> CInt -> CMode -> IO CInt
##if defined(javascript_HOST_ARCH)
c_openat dirfd path flags mode
    | dirfd == #{const AT_FDCWD} = Posix.c_open path flags mode
    | otherwise = c_openat_ dirfd path flags mode
##else
c_openat = c_openat_
##endif

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


-- | The open flags and the create mode for opening a regular file in the given
-- 'IOMode'.
--
-- Sets O_NOCTTY, O_NONBLOCK flags to be compatible with the base openFile
-- behavior. O_NOCTTY affects opening of terminal special files and O_NONBLOCK
-- affects fifo special files, and mandatory locking.
--
openFileFlags :: OpenFlags -> IOMode -> (OpenFlags, Maybe FileMode)
openFileFlags oflags iomode =
    case iomode of
        ReadMode -> (setReadOnly oflags1, Nothing)
        WriteMode -> (setWriteOnly oflags1, Just defaultCreateMode)
        AppendMode ->
            ((setAppend True . setWriteOnly) oflags1, Just defaultCreateMode)
        ReadWriteMode -> (setReadWrite oflags1, Just defaultCreateMode)

    where

    oflags1 = setNoCtty True $ setNonBlock True oflags

foreign import ccall unsafe "unistd.h close"
   c_close_ :: CInt -> IO CInt

c_close :: CInt -> IO CInt
##if defined(javascript_HOST_ARCH)
c_close = Posix.c_close
##else
c_close = c_close_
##endif

close :: Fd -> IO ()
close (Fd fd) = throwErrnoIfMinus1_ ("close " ++ show fd) (c_close fd)

-------------------------------------------------------------------------------
-- base openFile compatible, Handle returning, APIs
-------------------------------------------------------------------------------

-- | Like openFile in base. open() can be slow, e.g. on NFS or FUSE file
-- systems, or block, so use an interruptible foreign call for it.
interruptibleOpen :: CString -> CInt -> CMode -> IO CInt
##if MIN_VERSION_base(4,16,0)
interruptibleOpen = Posix.c_interruptible_open
##else
interruptibleOpen = Posix.c_safe_open
##endif

-- | Open a file and return a Handle in binary mode. Like openFile in base,
-- the file is locked and a file opened in 'WriteMode' is truncated.
openFileHandle :: PosixPath -> IOMode -> IO Handle
openFileHandle path iomode = do
    let (flags, cmode) = openFileFlags defaultOpenFlags iomode
        OpenFlags flags1 = maybe flags (\_ -> setCreat True flags) cmode
        FileMode mode = maybe defaultCreateMode id cmode
    fd <- Path.asCString path $ \cstr ->
        throwErrnoPathIfMinus1Retry "openFile" path
            $ interruptibleOpen cstr flags1 mode
    (fD, fdType) <-
        FD.mkFD fd iomode Nothing False True `onException` c_close fd
    -- Like base, truncate after locking the file, ftruncate fails on special
    -- files like /dev/null.
    when (iomode == WriteMode && fdType == RegularFile)
        $ Device.setSize fD 0 `onException` Device.close fD
    mkHandleFromFD fD fdType (Path.toString path) iomode False Nothing
        `onException` Device.close fD

-- | Like openFile in base package but using Path instead of FilePath.
--
-- Unlike base, the Handle is in binary mode, there is no character encoding
-- or newline translation, it is the same as openBinaryFile.
-- Streamly encodes and decodes text explicitly, e.g. using
-- "Streamly.Unicode.Stream". Use 'System.IO.hSetEncoding' and
-- 'System.IO.hSetNewlineMode' on the Handle to read or write text using
-- System.IO functions like 'System.IO.hPutStr'.
openFile :: PosixPath -> IOMode -> IO Handle
openFile = File.openFile False openFileHandle

-- | Like withFile in base package but using Path instead of FilePath.
--
-- Unlike base, the Handle is in binary mode, there is no character encoding
-- or newline translation, it is the same as withBinaryFile.
-- Streamly encodes and decodes text explicitly, e.g. using
-- "Streamly.Unicode.Stream". Use 'System.IO.hSetEncoding' and
-- 'System.IO.hSetNewlineMode' on the Handle to read or write text using
-- System.IO functions like 'System.IO.hPutStr'.
withFile :: PosixPath -> IOMode -> (Handle -> IO r) -> IO r
withFile = File.withFile False openFileHandle

-- XXX This is the same as openFile, the Handle is in binary mode in both
-- cases, it can be removed.
-- | Like openBinaryFile in base package but using Path instead of FilePath.
openBinaryFile :: PosixPath -> IOMode -> IO Handle
openBinaryFile = File.openFile True openFileHandle

-- XXX This is the same as withFile, the Handle is in binary mode in both
-- cases, it can be removed.
-- | Like withBinaryFile in base package but using Path instead of FilePath.
withBinaryFile :: PosixPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = File.withFile True openFileHandle
#endif
