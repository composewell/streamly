module Streamly.Internal.FileSystem.File.Utils
    ( openFile
    , withFile
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (mask, onException, try)
import Control.Monad (when)
import GHC.IO (catchException)
import GHC.IO.Exception (IOException(..))
import GHC.IO.Handle.Internals (handleFinalizer)
import Streamly.Internal.FileSystem.Path (Path)
import System.IO (IOMode(..), Handle, hSetBinaryMode, hClose)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Streamly.Internal.FileSystem.Windows.File as Platform
#else
import qualified Streamly.Internal.FileSystem.Posix.File as Platform
#endif

import qualified Streamly.Internal.FileSystem.Path as Path

#if MIN_VERSION_base(4,16,0)
import GHC.IO.Handle.Internals (addHandleFinalizer)
#else
import Control.Concurrent.MVar (MVar, addMVarFinalizer)
import GHC.IO.Handle.Internals (debugIO)
import GHC.IO.Handle.Types (Handle__, Handle(..))
#endif

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

#if !(MIN_VERSION_base(4,16,0))
type HandleFinalizer = FilePath -> MVar Handle__ -> IO ()

-- | Add a finalizer to a 'Handle'. Specifically, the finalizer
-- will be added to the 'MVar' of a file handle or the write-side
-- 'MVar' of a duplex handle. See Handle Finalizers for details.
addHandleFinalizer :: Handle -> HandleFinalizer -> IO ()
addHandleFinalizer handle finalizer = do
  debugIO $ "Registering finalizer: " ++ show filepath
  addMVarFinalizer mv (finalizer filepath mv)
  where
    !(filepath, !mv) = case handle of
      FileHandle fp m -> (fp, m)
      DuplexHandle fp _ write_m -> (fp, write_m)
#endif

addFilePathToIOError :: String -> Path -> IOException -> IOException
addFilePathToIOError fun fp ioe =
  let !str = Path.toString fp
   in ioe
        { ioe_location = fun
        , ioe_filename = Just str
        }

catchWith :: String -> Path -> IO a -> IO a
catchWith str path io =
    catchException io (ioError . addFilePathToIOError str path)

withOpenFile
    :: Bool
    -> Bool
    -> Bool
    -> Bool
    -> Path
    -> IOMode
    -> (Handle -> IO r)
    -> IO r
withOpenFile binary _existing _cloExec close_finally fp iomode action =
    mask $ \restore -> do
        {-
        hndl <- case (existing, cloExec) of
                  (True, False) -> Platform.openExistingFile fp iomode
                  (False, False) -> Platform.openFile fp iomode
                  (True, True) -> Platform.openExistingFileWithCloseOnExec fp iomode
                  (False, True) -> Platform.openFileWithCloseOnExec fp iomode
        -}
        hndl <- Platform.openFile fp iomode
        addHandleFinalizer hndl handleFinalizer
        when binary $ hSetBinaryMode hndl True
        r <- restore (action hndl) `onException` hClose hndl
        when close_finally $ hClose hndl
        pure r

-- XXX Write this using openFile instead?
withFile :: Path -> IOMode -> (Handle -> IO r) -> IO r
withFile path iomode act =
    (catchWith "withFile" path
        $ withOpenFile False False False True path iomode (try . act))
      >>= either ioError pure

openFile :: Path -> IOMode -> IO Handle
openFile path iomode =
    catchWith "openFile" path
        $ withOpenFile False False False False path iomode pure
