module Streamly.Internal.FileSystem.File.Utils
    ( withFile
    , openFile
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

import qualified Streamly.Internal.FileSystem.Path as Path

#if MIN_VERSION_base(4,16,0)
import GHC.IO.Handle.Internals (addHandleFinalizer)
#else
import Control.Concurrent.MVar (MVar, addMVarFinalizer)
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
  addMVarFinalizer mv (finalizer filepath mv)
  where
    !(filepath, !mv) = case handle of
      FileHandle fp m -> (fp, m)
      DuplexHandle fp _ write_m -> (fp, write_m)
#endif

{-# INLINE withOpenFile #-}
withOpenFile
    :: Bool
    -> Bool
    -> (Path -> IOMode -> IO Handle)
    -> Path
    -> IOMode
    -> (Handle -> IO r)
    -> IO r
withOpenFile binary close_finally f fp iomode action =
    mask $ \restore -> do
        h <- f fp iomode
        -- XXX In case of withFile it will be closed anyway, so do we even need
        -- this?
        addHandleFinalizer h handleFinalizer
        when binary $ hSetBinaryMode h True
        r <- restore (action h) `onException` hClose h
        when close_finally $ hClose h
        pure r

addFilePathToIOError :: String -> Path -> IOException -> IOException
addFilePathToIOError fun fp ioe =
  let !str = Path.toString fp
   in ioe
        { ioe_location = fun
        , ioe_filename = Just str
        }

{-# INLINE catchWith #-}
catchWith :: String -> Path -> IO a -> IO a
catchWith str path io =
    catchException io (ioError . addFilePathToIOError str path)

{-# INLINE withFile #-}
withFile ::
    Bool
    -> (Path -> IOMode -> IO Handle)
    -> Path
    -> IOMode
    -> (Handle -> IO r)
    -> IO r
withFile binary f path iomode act =
     catchWith "withFile" path
        (withOpenFile binary True f path iomode (try . act))
      >>= either ioError pure

{-# INLINE openFile #-}
openFile ::
    Bool -> (Path -> IOMode -> IO Handle) -> Path -> IOMode -> IO Handle
openFile binary f path iomode =
    catchWith "openFile" path
        $ withOpenFile binary False f path iomode pure
