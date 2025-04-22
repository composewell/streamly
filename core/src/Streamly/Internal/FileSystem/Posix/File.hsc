module Streamly.Internal.FileSystem.Posix.File
    (
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
      OpenFlags (..)
    , OpenMode (..)
    , defaultOpenFlags

    -- * Fd based Low Level
    , openAtWith
    , openAt
    , open
    , close

    -- -- * Posix Fd based openFile
    -- , openFileFdWith
    -- , openFileFd

    -- * Handle based
    , openFile
    , withFile
    -- , openBinaryFile
    -- , withBinaryFile

    -- Re-exported
    , Fd
#endif
    ) where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Data.Bits ((.|.))
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import GHC.IO.Handle.FD (fdToHandle)
import Streamly.Internal.FileSystem.Posix.Errno (throwErrnoPathIfMinus1Retry)
import Streamly.Internal.FileSystem.PosixPath (PosixPath)
import System.IO (IOMode(..), Handle)
import System.Posix.Types (Fd(..), CMode(..), FileMode)

import qualified Streamly.Internal.FileSystem.File.Utils as File
import qualified Streamly.Internal.FileSystem.PosixPath as Path

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

-- | Open mode, see Posix open system call man page.
data OpenMode =
      ReadOnly -- ^ O_RDONLY
    | WriteOnly -- ^ O_WRONLY
    | ReadWrite -- ^ O_RDWR
    deriving (Read, Show, Eq, Ord)

-- | Open flags, see posix open system call man page.
data OpenFlags =
 OpenFlags {
    append    :: Bool,           -- ^ O_APPEND
    exclusive :: Bool,           -- ^ O_EXCL, Result is undefined if 'creat' is 'Nothing'.
    noctty    :: Bool,           -- ^ O_NOCTTY
    nonBlock  :: Bool,           -- ^ O_NONBLOCK
    trunc     :: Bool,           -- ^ O_TRUNC
    nofollow  :: Bool,           -- ^ O_NOFOLLOW
    creat     :: Maybe FileMode, -- ^ O_CREAT
    cloexec   :: Bool,           -- ^ O_CLOEXEC
    directory :: Bool,           -- ^ O_DIRECTORY
    sync      :: Bool            -- ^ O_SYNC
 }
 deriving (Read, Show, Eq, Ord)

-- | Default values for the 'OpenFlags'.
--
defaultOpenFlags :: OpenFlags
defaultOpenFlags =
    OpenFlags
    { append    = False
    , exclusive = False
    , noctty    = True
    , nonBlock  = True
    , trunc     = False
    , nofollow  = False
    , creat     = Nothing
    , cloexec   = False
    , directory = False
    , sync      = False
    }

-------------------------------------------------------------------------------
-- Low level (fd returning) file opening APIs
-------------------------------------------------------------------------------

foreign import capi unsafe "fcntl.h openat"
   c_openat :: CInt -> CString -> CInt -> CMode -> IO CInt

-- | Open and optionally create a file relative to an optional
-- directory file descriptor.
-- {-# INLINE openFdAtWith_ #-}
openAtWith_ ::
       OpenFlags -- ^ Append, exclusive, etc.
    -> Maybe Fd -- ^ Optional directory file descriptor
    -> CString -- ^ Pathname to open
    -> OpenMode -- ^ Read-only, read-write or write-only
    -> IO Fd
openAtWith_ OpenFlags{..} fdMay path how =
    Fd <$> c_openat c_fd path all_flags mode_w

    where

    c_fd = maybe (-100) (\ (Fd fd) -> fd) fdMay

    flags =
       (if append       then 1024    else 0) .|.
       (if exclusive    then 128     else 0) .|.
       (if noctty       then 256     else 0) .|.
       (if nonBlock     then 2048    else 0) .|.
       (if trunc        then 512     else 0) .|.
       (if nofollow     then 131072  else 0) .|.
       (if cloexec      then 524288  else 0) .|.
       (if directory    then 65536   else 0) .|.
       (if sync         then 1052672 else 0)

    open_mode =
        case how of
            ReadOnly  -> 0
            WriteOnly -> 1
            ReadWrite -> 2

    (creat_f, mode_w) =
        case creat of
            Nothing -> (0, 0)
            Just x  -> (64, x)

    all_flags = creat_f .|. flags .|. open_mode

-- | Open a file relative to an optional directory file descriptor.
--
-- {-# INLINE openAtWith #-}
openAtWith ::
       OpenFlags -- ^ Append, exclusive, truncate, etc.
    -> Maybe Fd -- ^ Optional directory file descriptor
    -> PosixPath -- ^ Pathname to open
    -> OpenMode -- ^ Read-only, read-write or write-only
    -> IO Fd
openAtWith flags fdMay name how =
   Path.asCString name $ \str -> do
     throwErrnoPathIfMinus1Retry "openAtWith" name
        $ openAtWith_ flags fdMay str how

{-# INLINE openAt #-}
openAt :: Maybe Fd -> PosixPath -> OpenMode -> IO Fd
openAt = openAtWith defaultOpenFlags

-- Note using an fd directly for IO may be problematic as direct blocking file
-- system operations on the file might block the capability and GC for "unsafe"
-- calls. "safe" calls may be more expensive. Also, you may have to synchronize
-- concurrent access via multiple threads.
{-# INLINE open #-}
open :: PosixPath -> OpenMode -> IO Fd
open = openAt Nothing

-- | Open a regular file, return an Fd.
openFileFdWith :: OpenFlags -> PosixPath -> IOMode -> IO Fd
openFileFdWith oflags fp iomode = do
    case iomode of
        ReadMode -> open1 ReadOnly  oflags
        WriteMode -> open1 WriteOnly oflags {trunc = True, creat = cflag}
        AppendMode -> open1 WriteOnly oflags {append = True, creat = cflag}
        ReadWriteMode -> open1 ReadWrite oflags {creat = cflag}

    where

    -- Use Nothing to open existing file only
    cflag = Just 0o666
    open1 mode flags = openAtWith flags Nothing fp mode

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

{-
-- | Like openBinaryFile in base package but using Path instead of FilePath.
openBinaryFile :: PosixPath -> IOMode -> IO Handle
openBinaryFile = File.openFile True openFileHandle

-- | Like withBinaryFile in base package but using Path instead of FilePath.
withBinaryFile :: PosixPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = File.withFile True openFileHandle
-}
#endif
