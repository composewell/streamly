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

-- | Control the behavior when the stat call on a directory entry fails with a
-- symlink loop error (@ELOOP@), e.g. a symbolic link that points to itself.
--
-- The type of a directory entry is determined using a @stat@ or @lstat@
-- system call in the following cases:
--
-- * The entry is a symbolic link and 'followSymlinks' is enabled, @stat@
--   resolves the link to find the type of the target.
-- * readdir reports the type of the entry as unknown (@DT_UNKNOWN@), which
--   happens on file systems that do not report the type in the directory
--   entry. @lstat@ finds the type, or @stat@ if 'followSymlinks' is enabled.
--
-- @ELOOP@ occurs when too many symbolic links are encountered while
-- resolving the path.
--
-- When set to 'True', the error is ignored and the entry is skipped, it does
-- not appear in the output. When set to 'False', the directory read raises an
-- exception.
--
-- The default is 'True'.
--
-- On Windows this option has no effect as of now, the type is not determined
-- using stat.
ignoreSymlinkLoops :: Bool -> ReadOptions -> ReadOptions
ignoreSymlinkLoops x opts = opts {_ignoreELOOP = x}

-- | Control the behavior when the stat call on a directory entry fails
-- because the path does not exist (@ENOENT@).
--
-- The type of a directory entry is determined using a @stat@ or @lstat@
-- system call in the following cases:
--
-- * The entry is a symbolic link and 'followSymlinks' is enabled, @stat@
--   resolves the link to find the type of the target.
-- * readdir reports the type of the entry as unknown (@DT_UNKNOWN@), which
--   happens on file systems that do not report the type in the directory
--   entry. @lstat@ finds the type, or @stat@ if 'followSymlinks' is enabled.
--
-- @ENOENT@ occurs when the entry is a symbolic link whose target does not
-- exist, or when the entry is deleted after readdir returns it and before the
-- stat call.
--
-- When set to 'True', the error is ignored and the entry is skipped, it does
-- not appear in the output. When set to 'False', the directory read raises an
-- exception.
--
-- The default is 'True'.
--
-- On Windows this option has no effect as of now, the type is not determined
-- using stat.
--
-- This option does not apply to opening the directory being read. If the
-- directory does not exist, the directory read raises an exception.
ignoreMissing :: Bool -> ReadOptions -> ReadOptions
ignoreMissing x opts = opts {_ignoreENOENT = x}

-- | Control the behavior when the stat call on a directory entry fails with a
-- permission error (@EACCES@).
--
-- The type of a directory entry is determined using a @stat@ or @lstat@
-- system call in the following cases:
--
-- * The entry is a symbolic link and 'followSymlinks' is enabled, @stat@
--   resolves the link to find the type of the target.
-- * readdir reports the type of the entry as unknown (@DT_UNKNOWN@), which
--   happens on file systems that do not report the type in the directory
--   entry. @lstat@ finds the type, or @stat@ if 'followSymlinks' is enabled.
--
-- @EACCES@ occurs when search permission is denied on a directory in the
-- path of the entry, or of the target of a symbolic link.
--
-- When set to 'True', the error is ignored and the entry is skipped, it does
-- not appear in the output. When set to 'False', the directory read raises an
-- exception.
--
-- The default is 'True'.
--
-- On Windows this option has no effect as of now, the type is not determined
-- using stat.
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
