-- |
-- Module      : Streamly.Internal.Syscall.Posix
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Haskell wrappers on some libc system calls.

{-# LANGUAGE UnliftedFFITypes #-}

module Streamly.Internal.Syscall.Posix
    (
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
      getCwd
    , setCwd
    , CStat
    , stat
    , s_IFDIR
#endif
    )
where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Data.Bits ((.&.))
import Data.Word (Word8)
import Foreign.C.Error
    (getErrno, eRANGE, eINTR, Errno, resetErrno, throwErrno, throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar(..), CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peekByteOff)
import GHC.Base (Addr##)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import Streamly.Internal.Data.MutByteArray.Type
    (MutByteArray, PinnedState(..), unsafeAsPtr)
import Streamly.Internal.Syscall.Common (retry)
import Streamly.Internal.FileSystem.PosixPath (PosixPath)
import qualified Streamly.Internal.FileSystem.PosixPath as Path
import System.Posix.Types (CMode)

#include <sys/stat.h>

-- Instead of using alloca for buffer we use a mutable bytearray which can be
-- directly used elsewhere in the program without copying.

foreign import ccall unsafe "getcwd"
   c_getcwd :: Ptr CChar -> CSize -> IO (Ptr CChar)

tryGetCwd :: Int -> IO (Maybe MutByteArray)
tryGetCwd bytes = do
    arr <- MutByteArray.new' bytes
    unsafeAsPtr arr $ \ptr -> do
        r <- c_getcwd ptr (fromIntegral bytes)
        if r /= nullPtr
        then return (Just arr)
        else do
            errno <- getErrno
            if errno == eRANGE
            then return Nothing
            else throwErrno "getCwd"

foreign import ccall unsafe "string.h strlen" c_strlen_pinned
    :: Addr## -> IO CSize

-- Start with a small buffer, doubling until getcwd succeeds.
getCwd :: IO (Array Word8)
getCwd = do
    let resize old = max (old * 2) 4096
    arr <- retry resize tryGetCwd 256
    len <- unsafeAsPtr arr $ \(Ptr addr##) ->
            c_strlen_pinned addr##
    -- Note that the return value may be pinned or unpinned, users should not
    -- rely on that, if you want guarantee then clone the MBA.
    mba <- MutByteArray.rightSizeAs Unpinned (fromIntegral len) arr
    return (Array mba 0 (fromIntegral len))

foreign import ccall unsafe "chdir"
    c_chdir :: CString -> IO CInt

setCwd :: PosixPath -> IO ()
setCwd path =
    Path.asCString path $
        throwErrnoIfMinus1_ "setCwd" . c_chdir

--------------------------------------------------------------------------------
-- Stat
--------------------------------------------------------------------------------

data {-# CTYPE "struct stat" #-} CStat

foreign import capi unsafe "sys/stat.h lstat"
    c_lstat :: CString -> Ptr CStat -> IO CInt

foreign import capi unsafe "sys/stat.h stat"
    c_stat :: CString -> Ptr CStat -> IO CInt

s_IFMT :: CMode
s_IFMT  = #{const S_IFMT}

s_IFDIR :: CMode
s_IFDIR = #{const S_IFDIR}

{-
s_IFREG :: CMode
s_IFREG = #{const S_IFREG}

s_IFLNK :: CMode
s_IFLNK = #{const S_IFLNK}
-}

-- NOTE: Using fstatat with a dirfd and relative path would be faster.
stat :: Bool -> CString -> IO (Either Errno CMode)
stat followSym cstr =
    allocaBytes #{size struct stat} $ \p_stat -> do
        resetErrno
        result <-
            if followSym
            then c_stat cstr p_stat
            else c_lstat cstr p_stat
        if result /= 0
        then do
            errno <- getErrno
            if errno == eINTR
            then stat followSym cstr
            else pure $ Left errno
        else do
            mode <- #{peek struct stat, st_mode} p_stat
            pure $ Right (mode .&. s_IFMT)
#endif
