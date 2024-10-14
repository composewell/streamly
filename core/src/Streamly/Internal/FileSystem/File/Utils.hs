module Streamly.Internal.FileSystem.File.Utils
    ( openFile
    , withFile
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Exception (mask, onException, try)
import Control.Monad (when)
import GHC.IO (catchException, unsafePerformIO)
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
addFilePathToIOError fun fp ioe = unsafePerformIO $ do
  let fp' = Path.toString fp
  -- XXX Why is this important?
  -- deepseq will be introduced dependency because of this
  -- fp'' <- evaluate $ force fp'
  pure $ ioe{ ioe_location = fun, ioe_filename = Just fp' }

augmentError :: String -> Path -> IO a -> IO a
augmentError str osfp = flip catchException (ioError . addFilePathToIOError str osfp)

withOpenFile'
    :: Path
    -> IOMode -> Bool -> Bool -> Bool
    -> (Handle -> IO r) -> Bool -> IO r
withOpenFile' fp iomode binary existing cloExec action close_finally =
    mask $ \restore -> do
        hndl <- case (existing, cloExec) of
                  (True, False) -> Platform.openExistingFile fp iomode
                  (False, False) -> Platform.openFile fp iomode
                  (True, True) -> Platform.openExistingFileWithCloseOnExec fp iomode
                  (False, True) -> Platform.openFileWithCloseOnExec fp iomode
        addHandleFinalizer hndl handleFinalizer
        when binary $ hSetBinaryMode hndl True
        r <- restore (action hndl) `onException` hClose hndl
        when close_finally $ hClose hndl
        pure r

-- | Open a file and return the 'Handle'.
openFile :: Path -> IOMode -> IO Handle
openFile osfp iomode =
    augmentError "openFile" osfp $ withOpenFile' osfp iomode False False False pure False

-- | Run an action on a file.
--
-- The 'Handle' is automatically closed afther the action.
withFile :: Path -> IOMode -> (Handle -> IO r) -> IO r
withFile osfp iomode act = (augmentError "withFile" osfp
    $ withOpenFile' osfp iomode False False False (try . act) True)
  >>= either ioError pure
