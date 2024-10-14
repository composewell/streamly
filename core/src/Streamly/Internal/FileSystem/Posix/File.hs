module Streamly.Internal.FileSystem.Posix.File
    ( openExistingFile
    , openFile
    , openExistingFileWithCloseOnExec
    , openFileWithCloseOnExec
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Data.Bits ((.|.))
import Foreign.C.Error (getErrno, eINTR, errnoToIOError)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Streamly.Internal.FileSystem.PosixPath (PosixPath)
import System.IO (IOMode(..), Handle)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd(..), CMode(..), FileMode)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.FileSystem.PosixPath as Path

-------------------------------------------------------------------------------
-- Posix
-------------------------------------------------------------------------------

data OpenMode = ReadOnly | WriteOnly | ReadWrite
              deriving (Read, Show, Eq, Ord)

-- |Correspond to some of the int flags from C's fcntl.h.
data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool,           -- ^ O_APPEND
    exclusive :: Bool,           -- ^ O_EXCL, result is undefined if O_CREAT is False
                                 --
                                 -- __NOTE__: Result is undefined if 'creat' is 'Nothing'.
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

-- | Default values for the 'OpenFileFlags' type.
--
-- Each field of 'OpenFileFlags' is either 'False' or 'Nothing'
-- respectively.
defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False,
    nofollow  = False,
    creat     = Nothing,
    cloexec   = False,
    directory = False,
    sync      = False
  }

defaultExistingFileFlags :: OpenFileFlags
defaultExistingFileFlags = defaultFileFlags { noctty = True, nonBlock = True, creat = Nothing }

defaultFileFlags' :: OpenFileFlags
defaultFileFlags' = defaultFileFlags { noctty = True, nonBlock = True }

withFilePath :: PosixPath -> (CString -> IO a) -> IO a
withFilePath p = Array.asCStringUnsafe (Path.toChunk p)


-- |Open and optionally create a file relative to an optional
-- directory file descriptor.
openat_  :: Maybe Fd -- ^ Optional directory file descriptor
         -> CString -- ^ Pathname to open
         -> OpenMode -- ^ Read-only, read-write or write-only
         -> OpenFileFlags -- ^ Append, exclusive, etc.
         -> IO Fd
openat_ fdMay str how (OpenFileFlags appendFlag exclusiveFlag nocttyFlag
                                nonBlockFlag truncateFlag nofollowFlag
                                creatFlag cloexecFlag directoryFlag
                                syncFlag) =
    Fd <$> c_openat c_fd str all_flags mode_w
  where
    c_fd = maybe (-100) (\ (Fd fd) -> fd) fdMay
    all_flags  = creat .|. flags .|. open_mode

    flags =
       (if appendFlag       then (1024)    else 0) .|.
       (if exclusiveFlag    then (128)     else 0) .|.
       (if nocttyFlag       then (256)     else 0) .|.
       (if nonBlockFlag     then (2048)    else 0) .|.
       (if truncateFlag     then (512)     else 0) .|.
       (if nofollowFlag     then (131072)  else 0) .|.
       (if cloexecFlag      then (524288)  else 0) .|.
       (if directoryFlag    then (65536)   else 0) .|.
       (if syncFlag         then (1052672) else 0)

    (creat, mode_w) = case creatFlag of
                        Nothing -> (0,0)
                        Just x  -> ((64), x)

    open_mode = case how of
                   ReadOnly  -> (0)
                   WriteOnly -> (1)
                   ReadWrite -> (2)

foreign import capi unsafe "HsUnix.h openat"
   c_openat :: CInt -> CString -> CInt -> CMode -> IO CInt

-- |Open and optionally create this file.  See 'System.Posix.Files'
-- for information on how to use the 'FileMode' type.
openFd :: PosixPath
       -> OpenMode
       -> OpenFileFlags
       -> IO Fd
openFd = openFdAt Nothing

throwErrnoPathIfMinus1Retry :: (Eq a, Num a)
                            => String -> PosixPath -> IO a -> IO a
throwErrnoPathIfMinus1Retry loc path f = do
  throwErrnoPathIfRetry (== -1) loc path f

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

throwErrnoPath :: String -> PosixPath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    -- XXX What if this decode fails?
    -- The unix package catches this kind of an error
    let path' = Path.toString path
    ioError (errnoToIOError loc errno Nothing (Just path'))

-- | Open a file relative to an optional directory file descriptor.
--
-- Directory file descriptors can be used to avoid some race conditions when
-- navigating changing directory trees, or to retain access to a portion of the
-- directory tree that would otherwise become inaccessible after dropping
-- privileges.
openFdAt :: Maybe Fd -- ^ Optional directory file descriptor
         -> PosixPath -- ^ Pathname to open
         -> OpenMode -- ^ Read-only, read-write or write-only
         -> OpenFileFlags -- ^ Append, exclusive, truncate, etc.
         -> IO Fd
openFdAt fdMay name how flags =
   withFilePath name $ \str ->
     throwErrnoPathIfMinus1Retry "openFdAt" name $ openat_ fdMay str how flags

openExistingFile_ :: OpenFileFlags -> PosixPath -> IOMode -> IO Handle
openExistingFile_ df fp iomode = fdToHandle =<< case iomode of
  ReadMode      -> open ReadOnly  df
  WriteMode     -> open WriteOnly df { trunc = True }
  AppendMode    -> open WriteOnly df { append = True }
  ReadWriteMode -> open ReadWrite df
 where
  open = openFd fp

-- | Open an existing file and return the 'Handle'.
openExistingFile :: PosixPath -> IOMode -> IO Handle
openExistingFile = openExistingFile_ defaultExistingFileFlags

openExistingFileWithCloseOnExec :: PosixPath -> IOMode -> IO Handle
openExistingFileWithCloseOnExec = openExistingFile_ defaultExistingFileFlags { cloexec = True }

openFile_ :: OpenFileFlags -> PosixPath -> IOMode -> IO Handle
openFile_ df fp iomode = fdToHandle =<< case iomode of
  ReadMode      -> open ReadOnly  df
  WriteMode     -> open WriteOnly df { trunc = True, creat = Just 0o666 }
  AppendMode    -> open WriteOnly df { append = True, creat = Just 0o666 }
  ReadWriteMode -> open ReadWrite df { creat = Just 0o666 }
 where
  open = openFd fp

-- | Open a file and return the 'Handle'.
openFile :: PosixPath -> IOMode -> IO Handle
openFile = openFile_ defaultFileFlags'

openFileWithCloseOnExec :: PosixPath -> IOMode -> IO Handle
openFileWithCloseOnExec = openFile_ defaultFileFlags' { cloexec = True }
