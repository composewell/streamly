module Streamly.Internal.FileSystem.Posix.File
    (
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
      OpenFlags (..)
    , OpenMode (..)
    , defaultOpenFlags
    , openFileWith
    , openFile

    , openFdAtWith
    , openFdAt
    , openFd
    , closeFd

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
import Streamly.Internal.FileSystem.PosixPath (PosixPath)
import System.IO (IOMode(..), Handle)
import GHC.IO.Handle.FD (fdToHandle)
import System.Posix.Types (Fd(..), CMode(..), FileMode)
import Streamly.Internal.FileSystem.Posix.Errno (throwErrnoPathIfMinus1Retry)

import qualified Streamly.Internal.FileSystem.PosixPath as Path
-- import qualified GHC.IO.FD as FD

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

-- XXX use oRDONLY, oWRONLY etc?
data OpenMode =
      ReadOnly -- ^ O_RDONLY
    | WriteOnly -- ^ O_WRONLY
    | ReadWrite -- ^ O_RDWR
    deriving (Read, Show, Eq, Ord)

-- XXX use oAPPEND, oEXCL, oNOCTTY etc?
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
    , noctty    = True -- XXX ?
    , nonBlock  = True -- XXX ?
    , trunc     = False
    , nofollow  = False
    , creat     = Nothing
    , cloexec   = False
    , directory = False
    , sync      = False
    }

foreign import capi unsafe "fcntl.h openat"
   c_openat :: CInt -> CString -> CInt -> CMode -> IO CInt

-- | Open and optionally create a file relative to an optional
-- directory file descriptor.
-- {-# INLINE openFdAtWith_ #-}
openFdAtWith_ ::
       OpenFlags -- ^ Append, exclusive, etc.
    -> Maybe Fd -- ^ Optional directory file descriptor
    -> CString -- ^ Pathname to open
    -> OpenMode -- ^ Read-only, read-write or write-only
    -> IO Fd
openFdAtWith_ OpenFlags{..} fdMay path how =
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
-- {-# INLINE openFdAtWith #-}
openFdAtWith ::
       OpenFlags -- ^ Append, exclusive, truncate, etc.
    -> Maybe Fd -- ^ Optional directory file descriptor
    -> PosixPath -- ^ Pathname to open
    -> OpenMode -- ^ Read-only, read-write or write-only
    -> IO Fd
openFdAtWith flags fdMay name how =
   Path.asCString name $ \str -> do
     throwErrnoPathIfMinus1Retry "openFdAt" name
        $ openFdAtWith_ flags fdMay str how

{-# INLINE openFdAt #-}
openFdAt :: Maybe Fd -> PosixPath -> OpenMode -> IO Fd
openFdAt = openFdAtWith defaultOpenFlags

{-# INLINE openFd #-}
openFd :: PosixPath -> OpenMode -> IO Fd
openFd = openFdAt Nothing

openFileWith :: OpenFlags -> PosixPath -> IOMode -> IO Handle
openFileWith df fp iomode = do
    r <-
        case iomode of
            ReadMode -> open ReadOnly  df
            WriteMode -> open WriteOnly df {trunc = True, creat = Just 0o666}
            AppendMode -> open WriteOnly df {append = True, creat = Just 0o666}
            ReadWriteMode -> open ReadWrite df {creat = Just 0o666}
    -- XXX Note we did not use mkFD here, are we locking the file?
    fdToHandle $ fromIntegral r

    where

    open mode flags = openFdAtWith flags Nothing fp mode

openFile :: PosixPath -> IOMode -> IO Handle
openFile = openFileWith defaultOpenFlags

foreign import ccall unsafe "unistd.h close"
   c_close :: CInt -> IO CInt

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ ("closeFd " ++ show fd) (c_close fd)

#endif
