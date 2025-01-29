-- |
-- Module      : Streamly.Internal.FileSystem.Posix.Errno
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.FileSystem.Posix.Errno
    (
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
      throwErrnoPath
    , throwErrnoPathIfRetry
    , throwErrnoPathIfNullRetry
    , throwErrnoPathIfMinus1Retry
#endif
    )
where

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Foreign (Ptr, nullPtr)
import Foreign.C (getErrno, eINTR)
import Foreign.C.Error (errnoToIOError)
import Streamly.Internal.FileSystem.PosixPath (PosixPath(..))

import qualified Streamly.Internal.FileSystem.PosixPath as Path

-------------------------------------------------------------------------------
-- From unix
-------------------------------------------------------------------------------

-- | Same as 'throwErrno', but exceptions include the given path when
-- appropriate.
--
throwErrnoPath :: String -> PosixPath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    -- XXX toString uses strict decoding, may fail
    ioError (errnoToIOError loc errno Nothing (Just (Path.toString path)))

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

throwErrnoPathIfNullRetry :: String -> PosixPath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry = throwErrnoPathIfRetry (== nullPtr)

throwErrnoPathIfMinus1Retry :: (Eq a, Num a) =>
    String -> PosixPath -> IO a -> IO a
throwErrnoPathIfMinus1Retry = throwErrnoPathIfRetry (== -1)
#endif
