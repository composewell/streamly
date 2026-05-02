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
#endif
    )
where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Data.Word (Word8)
import Foreign.C.Error (getErrno, eRANGE, throwErrno, throwErrnoIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar(..), CInt(..), CSize(..))
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Base (Addr#)
import GHC.Ptr (Ptr(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import Streamly.Internal.Data.MutByteArray.Type
    (MutByteArray, PinnedState(..), unsafeAsPtr)
import Streamly.Internal.Syscall.Common (retry)

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
    :: Addr# -> IO CSize

-- Start with a small buffer, doubling until getcwd succeeds.
getCwd :: IO (Array Word8)
getCwd = do
    let resize old = max (old * 2) 4096
    arr <- retry resize tryGetCwd 256
    len <- unsafeAsPtr arr $ \(Ptr addr#) ->
            c_strlen_pinned addr#
    -- Note that the return value may be pinned or unpinned, users should not
    -- rely on that, if you want guarantee then clone the MBA.
    mba <- MutByteArray.rightSizeAs Unpinned (fromIntegral len) arr
    return (Array mba 0 (fromIntegral len))

foreign import ccall unsafe "chdir"
    c_chdir :: CString -> IO CInt

setCwd :: Array Word8 -> IO ()
setCwd arr =
    Array.asCStringUnsafe arr $
        throwErrnoIfMinus1_ "setCwd" . c_chdir
#endif
