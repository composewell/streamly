-- |
-- Module      : Streamly.Internal.FileSystem.Dir
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.FileSystem.ReadDir
    (
      readDirStreamEither
    )
where

import Foreign (Ptr, Word8, nullPtr, peekByteOff)
import Foreign.C (resetErrno, Errno(..), getErrno, eINTR, throwErrno, CString)
import System.OsPath.Posix (PosixPath)
import System.Posix.Directory.Internals (DirStream(..), CDir, CDirent)
import System.Posix.PosixPath.FilePath (peekFilePath)

#include <dirent.h>

foreign import ccall unsafe "dirent.h readdir"
  c_readdir  :: Ptr CDir -> IO (Ptr CDirent)

foreign import ccall unsafe "__hscore_d_name"
  d_name :: Ptr CDirent -> IO CString

-- Left Dir, Right File
readDirStreamEither :: DirStream -> IO (Either PosixPath PosixPath)
readDirStreamEither (DirStream dirp) = loop

  where

  loop = do
    resetErrno
    ptr <- c_readdir dirp
    if (ptr /= nullPtr)
    then do
        dname <- d_name ptr
        dtype :: #{type unsigned char} <- #{peek struct dirent, d_type} ptr
        name <- peekFilePath dname
        return $
            if (dtype == #const DT_DIR)
            then (Left name)
            else (Right name)
    else do
        errno <- getErrno
        if (errno == eINTR)
        then loop
        else do
            let (Errno n) = errno
            if (n == 0)
            then return (Left mempty)
            else throwErrno "readDirStreamEither"
