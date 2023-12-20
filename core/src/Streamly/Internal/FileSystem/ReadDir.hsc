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

import Data.Char (ord)
import Foreign (Ptr, Word8, nullPtr, peek, peekByteOff)
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
        if (dtype == #const DT_DIR)
        then do
            -- XXX Assuming UTF8 encoding
            c1 <- peek dname
            if (c1 /= fromIntegral (ord '.'))
            then return (Left name)
            else do
                c2 :: Word8 <- peekByteOff dname 1
                if (c2 == 0)
                then loop
                else if (c2 /= fromIntegral (ord '.'))
                then return (Left name)
                else do
                    c3 :: Word8 <- peekByteOff dname 2
                    if (c3 == 0)
                    then loop
                    else return (Left name)
        else return (Right name)
    else do
        errno <- getErrno
        if (errno == eINTR)
        then loop
        else do
            let (Errno n) = errno
            if (n == 0)
            then return (Left mempty)
            else throwErrno "readDirStreamEither"
