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
      getCwd
    ) where

import Foreign.C.Error (getErrno, eRANGE, throwErrno)
import Foreign.C.Types (CChar(..), CSize(..))
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Base (Addr#)
import GHC.Ptr (Ptr(..))
import qualified Streamly.Internal.Data.MutByteArray as MutByteArray
import Streamly.Internal.Data.MutByteArray.Type
    (MutByteArray, PinnedState(..), unsafeAsPtr)

-- Instead of using alloca for buffer we use a mutable bytearray which can be
-- directly used elsewhere in the program without copying.

foreign import ccall unsafe "getcwd"
   c_getcwd :: Ptr CChar -> CSize -> IO (Ptr CChar)

-- Retry an IO action, modifying the argument on each Nothing result.
-- Throws an exception if there is an unrecoverable error.
retry :: (a -> a) -> (a -> IO (Maybe b)) -> a -> IO b
retry modify f a = go a
    where
    go x = do
        r <- f x
        case r of
            Just v  -> return v
            Nothing -> go (modify x)

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
getCwd :: IO MutByteArray
getCwd = do
    let resize old = max (old * 2) 4096
    arr <- retry resize tryGetCwd 256
    len <- unsafeAsPtr arr $ \(Ptr addr#) ->
            c_strlen_pinned addr#
    -- Note that the return value may be pinned or unpinned, users should not
    -- rely on that, if you want guarantee then clone the MBA.
    MutByteArray.rightSizeAs Unpinned (fromIntegral len) arr
