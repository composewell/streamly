-- |
-- Module      : Streamly.Internal.FileSystem.PathIO
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Filesystem IO operations on native OS paths.

{-# LANGUAGE CPP #-}

module Streamly.Internal.FileSystem.PathIO
    ( getCurrentDirectory
    , setCurrentDirectory
    , makeAbsolute
    , resolveDotDots
    , resolve
    ) where

import System.IO.Error (modifyIOError, isDoesNotExistError, ioeSetFileName)
import Streamly.Internal.Syscall.Common (ioeAppendLocation, modifyIOErrorString)
import Streamly.Internal.FileSystem.Path
    (Path, unsafeFromArray, toArray, unsafeJoin, toString_)
import qualified Streamly.Internal.FileSystem.Path.Common as Common

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Streamly.Internal.Syscall.Windows as Syscall
#define OS_NAME Windows
#else
import qualified Streamly.Internal.Syscall.Posix as Syscall
#define OS_NAME Posix
#endif

------------------------------------------------------------------------------
-- Working directory
------------------------------------------------------------------------------

-- | Get the current working directory as a path.
getCurrentDirectory :: IO Path
getCurrentDirectory = modifyError $ do
    arr <- Syscall.getCwd
    return $ unsafeFromArray arr

    where

    modifyError =
          modifyIOError (ioeAppendLocation "getCurrentDirectory")
        . modifyIOErrorString
            isDoesNotExistError
            "Current working directory no longer exists"

-- | Set the current working directory.
setCurrentDirectory :: Path -> IO ()
setCurrentDirectory p = modifyError $
    Syscall.setCwd (toArray p)

    where

    modifyError =
        modifyIOError
            ( ioeAppendLocation "setCurrentDirectory"
            . ioeSetFilePath p )

ioeSetFilePath :: Path -> IOError -> IOError
ioeSetFilePath p e = ioeSetFileName e (toString_ p)

------------------------------------------------------------------------------
-- Resolving paths
------------------------------------------------------------------------------

-- | Convert a path to an absolute path.
--
-- If the input path is relative, it is interpreted relative to the current
-- working directory. If it is already absolute, it is returned unchanged.
--
-- This function:
--
-- * Does not eliminate @..@ segments.
-- * Does not resolve symbolic links.
-- * May return a path containing symbolic links.
--
-- Examples:
--
-- @
-- makeAbsolute "a/b"  -- => "/cwd/a/b"
-- makeAbsolute "/a/b" -- => "/a/b"
-- @
--
makeAbsolute :: Path -> IO Path
makeAbsolute p = modifyError $ do
    if Common.isAbsolute Common.OS_NAME (toArray p)
    then return p
    else unsafeJoin <$> getCurrentDirectory <*> pure p

    where

    modifyError =
        modifyIOError (ioeAppendLocation "makeAbsolute" . ioeSetFilePath p)

-- | Resolve @..@ segments using filesystem semantics.
--
-- This function:
--
-- * Eliminates @..@ segments.
-- * Resolves symbolic links as needed to do so correctly.
-- * May still return a path containing symbolic links.
--
-- This is a partial resolution step and does not guarantee a fully
-- canonical path.
resolveDotDots :: Path -> IO Path
resolveDotDots = undefined

-- | Resolve a path to its fully resolved, canonical form.
--
-- This function performs full filesystem resolution, the resulting path:
--
-- * Contains no @..@ segments.
-- * Contains no symbolic links.
-- * Is absolute.
--
-- A path segment preceding a ".." is fully resolved before the ".." is applied.
--
-- This is equivalent to POSIX @realpath(3)@.
--
-- Examples:
--
-- @
-- resolve "/tmp/link/../a" -- => "/real/location/a"
-- @
resolve :: Path -> IO Path
resolve = undefined
