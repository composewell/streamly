-- |
-- Module      : Streamly.Internal.FileSystem.DirOptions
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- See docs/Developer/FileSystem.DirIO.md for design notes.

module Streamly.Internal.FileSystem.DirOptions
    (
      ReadOptions (..)
    , followSymlinks
    , ignoreMissing
    , ignoreSymlinkLoops
    , ignoreInaccessible
    , defaultReadOptions
    )
where


-- | Options controlling the behavior of directory read.
data ReadOptions =
    ReadOptions
    { _followSymlinks :: Bool
    , _ignoreELOOP :: Bool
    , _ignoreENOENT :: Bool
    , _ignoreEACCESS :: Bool
    }

-- | Control how symbolic links are handled when determining the type
-- of a directory entry.
--
-- * If set to 'True', symbolic links are resolved before classification.
--   This means a symlink pointing to a directory will be treated as a
--   directory, and a symlink pointing to a file will be treated as a
--   non-directory.
--
-- * If set to 'False', all symbolic links are classified as non-directories,
--   without attempting to resolve their targets.
--
-- Enabling resolution may cause additional errors to occur due to
-- insufficient permissions, broken links, or symlink loops. Such errors
-- can be ignored or handled using the appropriate options.
--
-- The default is 'False'.
--
-- On Windows this option has no effect as of now, symlinks are not followed to
-- determine the type.
followSymlinks :: Bool -> ReadOptions -> ReadOptions
followSymlinks x opts = opts {_followSymlinks = x}

-- | When the 'followSymlinks' option is enabled and a directory entry is a
-- symbolic link, we resolve it to determine the type of the symlink target.
-- This option controls the behavior when encountering symlink loop errors
-- during resolution.
--
-- When set to 'True', symlink loop errors are ignored, and the type of the
-- entry is reported as not a directory. When set to 'False', the directory
-- read operation fails with an error.
--
-- The default is 'True'.
--
-- On Windows this option has no effect as of now, symlinks are not followed to
-- determine the type.
ignoreSymlinkLoops :: Bool -> ReadOptions -> ReadOptions
ignoreSymlinkLoops x opts = opts {_ignoreELOOP = x}

-- | When the 'followSymlinks' option is enabled and a directory entry is a
-- symbolic link, we resolve it to determine the type of the symlink target.
-- This option controls the behavior when encountering broken symlink errors
-- during resolution.
--
-- When set to 'True', broken symlink errors are ignored, and the type of the
-- entry is reported as not a directory. When set to 'False', the directory
-- read operation fails with an error.
--
-- The default is 'True'.
--
-- On Windows this option has no effect as of now, symlinks are not followed to
-- determine the type.
ignoreMissing :: Bool -> ReadOptions -> ReadOptions
ignoreMissing x opts = opts {_ignoreENOENT = x}

-- | When the 'followSymlinks' option is enabled and a directory entry is a
-- symbolic link, we resolve it to determine the type of the symlink target.
-- This option controls the behavior when encountering permission errors
-- during resolution.
--
-- When set to 'True', any permission errors are ignored, and the type of the
-- entry is reported as not a directory. When set to 'False', the directory
-- read operation fails with an error.
--
-- The default is 'True'.
--
-- On Windows this option has no effect as of now, symlinks are not followed to
-- determine the type.
ignoreInaccessible :: Bool -> ReadOptions -> ReadOptions
ignoreInaccessible x opts = opts {_ignoreEACCESS = x}

-- XXX find ignores errors when following symlinks, by default.
-- NOTE: The defaultReadOptions emulates the behaviour of "find".
--
defaultReadOptions :: ReadOptions
defaultReadOptions =
    ReadOptions
    { _followSymlinks = False
    , _ignoreELOOP = True
    , _ignoreENOENT = True
    , _ignoreEACCESS = True
    }
