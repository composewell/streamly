-- |
-- Module      : Streamly.Internal.FileSystem.Dir
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.FileSystem.ReadDir
    (
      openDirStream
    , readDirStreamEither
    )
where

import Data.Char (ord)
import Foreign (Ptr, Word8, nullPtr, peek, peekByteOff, castPtr)
import Foreign.C (resetErrno, Errno(..), getErrno, eINTR, throwErrno, CString)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.FileSystem.Path (Path)
import System.Posix.Directory.Internals (DirStream(..), CDir, CDirent)
import System.Posix.Error (throwErrnoPathIfNullRetry)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.FileSystem.Path as Path

#include <dirent.h>

foreign import capi unsafe "dirent.h opendir"
    c_opendir :: CString  -> IO (Ptr CDir)

foreign import ccall unsafe "dirent.h readdir"
    c_readdir  :: Ptr CDir -> IO (Ptr CDirent)

foreign import ccall unsafe "__hscore_d_name"
    d_name :: Ptr CDirent -> IO CString

-- XXX Path is not null terminated therefore we need to make a copy even if the
-- array is pinned.
-- {-# INLINE openDirStream #-}
openDirStream :: Path -> IO DirStream
openDirStream p =
  Array.asCStringUnsafe (Path.toChunk p) $ \s -> do
    -- XXX is toString always creating another copy or only in case of error?
    dirp <- throwErrnoPathIfNullRetry "openDirStream" "" $ c_opendir s
    return (DirStream dirp)

-- XXX We can use getdents64 directly so that we can use array slices from the
-- same buffer that we passed to the OS. That way we can also avoid any
-- overhead of bracket.
-- XXX Make this as Unfold to avoid returning Maybe
-- XXX Or NOINLINE some parts and inline the rest to fuse it
-- {-# INLINE readDirStreamEither #-}
readDirStreamEither ::
    -- DirStream -> IO (Either (Rel (Dir Path)) (Rel (File Path)))
    DirStream -> IO (Maybe (Either Path Path))
readDirStreamEither (DirStream dirp) = loop

  where

  -- mkPath :: IsPath (Rel (a Path)) => Array Word8 -> Rel (a Path)
  -- {-# INLINE mkPath #-}
  mkPath :: Array Word8 -> Path
  mkPath = Path.fromPathUnsafe . Path.fromChunkUnsafe

  loop = do
    resetErrno
    ptr <- c_readdir dirp
    if (ptr /= nullPtr)
    then do
        dname <- d_name ptr
        dtype :: #{type unsigned char} <- #{peek struct dirent, d_type} ptr
        -- dreclen :: #{type unsigned short} <- #{peek struct dirent, d_reclen} ptr
        -- It is possible to find the name length using dreclen and then use
        -- fromPtrN, but it is not straightforward because the reclen is
        -- padded to 8-byte boundary.
        let !name = Array.fromByteStr (castPtr dname)
        if (dtype == #const DT_DIR)
        then do
            -- XXX Assuming UTF8 encoding
            c1 <- peek dname
            if (c1 /= fromIntegral (ord '.'))
            then return (Just (Left (mkPath name)))
            else do
                c2 :: Word8 <- peekByteOff dname 1
                if (c2 == 0)
                then loop
                else if (c2 /= fromIntegral (ord '.'))
                then return (Just (Left (mkPath name)))
                else do
                    c3 :: Word8 <- peekByteOff dname 2
                    if (c3 == 0)
                    then loop
                    else return (Just (Left (mkPath name)))
        else return (Just (Right (mkPath name)))
    else do
        errno <- getErrno
        if (errno == eINTR)
        then loop
        else do
            let (Errno n) = errno
            if (n == 0)
            -- then return (Left (mkPath (Array.fromList [46])))
            then return Nothing
            else throwErrno "readDirStreamEither"
