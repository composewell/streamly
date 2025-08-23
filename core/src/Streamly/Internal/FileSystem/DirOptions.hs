-- |
-- Module      : Streamly.Internal.FileSystem.DirOptions
-- Copyright   : (c) 2024 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

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

-- NOTE: If we are following symlinks, then we want to determine the type of
-- the link destination not the link itself, so we need to use stat instead of
-- lstat for resolving the symlink.
--
-- For recursive traversal, instead of classifying the dirents using stat, we
-- can leave them unclassified, and deal with ENOTDIR when doing an opendir. We
-- can just ignore that error if it is not a dir. This way we do not need to do
-- stat at all. Or we can basically say don't try to determine the type of
-- symlinks and always try to read symlinks as dirs. We can have an option for
-- classifying symlinks or DT_UNKNOWN as potential dirs.

-- When resolving a symlink we may encounter errors only if the directory entry
-- is a symlink. If the directory entry is not a symlink then stat on it will
-- have permissions, it will not give ELOOP or ENOENT unless the file was
-- deleted or recreated after we read the dirent.

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
-- When set to 'True', symlink loop errors are ignored, and the type is
-- reported as not a directory. When set to 'False', the directory read
-- operation fails with an error.
--
-- The default is 'False'.
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
-- When set to 'True', broken symlink errors are ignored, and the type is
-- reported as not a directory. When set to 'False', the directory read
-- operation fails with an error.
--
-- The default is 'False'.
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
-- When set to 'True', any permission errors are ignored, and the type is
-- reported as not a directory. When set to 'False', the directory read
-- operation fails with an error.
--
-- The default is 'False'.
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
    , _ignoreELOOP = False
    , _ignoreENOENT = False
    , _ignoreEACCESS = False
    }
