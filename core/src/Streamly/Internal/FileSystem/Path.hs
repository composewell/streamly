{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.FileSystem.Path
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- See docs/Developer/FileSystem.Path.md for design doc.
--
-- The API in this module is equivalent to or can emulate all or most of
-- the filepath package API. It has some differences from the filepath
-- package:
--
-- 1. The append operations follows path construction semantics rather than
-- path resolution and navigation based semantics used by the </> operation in
-- filepath package. Better have run time failures instead of silent problems.
-- 2. Empty paths are not allowed. Paths are validated before construction.
-- 3. The default Path type itself affords considerable safety regarding the
-- distinction of rooted or non-rooted paths, it also allows distinguishing
-- directory and file paths.
-- 4. It is designed to provide flexible typing to provide compile time safety
-- for rooted/non-rooted paths and file/dir paths. The Path type is just part
-- of that typed path ecosystem. Though the default Path type itself should be
-- enough for most cases.
-- 5. It leverages the streamly array module for most of the heavy lifting,
-- it is a thin wrapper on top of that, improving maintainability as well as
-- providing better performance. We can have pinned and unpinned paths, also
-- provide lower level operations for certain cases to interact more
-- efficiently with low level code.
-- 6. share name is part of the root when we split the root, this allows us to
-- treat the server and share name always in cases insensitive manner and the
-- remaining path can be normalized as case sensitive or insensitive.
--
-- It builds on arrays, has a richer API, consistent API, streaming ops where
-- it makes sense, performance is primary goal.
--
-- == Exception Handling
--
-- Path creation routines use MonadThrow which can be interpreted as an Either
-- type. It is rare to actually handle exceptions in path creation functions,
-- we would rather fix the issue, so partial functions should also be fine. But
-- there may be some cases where we are parsing paths from external inputs,
-- reading from a file etc where we may want to handle exceptions. We can
-- always create partial wrappers from these if that is convenient to use.
--
-- == Path and PathIO modules
--
-- The Path module contains purely path manipulation code. WindowsPath and
-- PosixPath have different type of handling but both the modules are available
-- in all plaforms, therefore, you can manipulate Windows paths on Posix and
-- vice-versa. Because of this the operations that require platform specific IO
-- system calls are placed in the PathIO module which is available only on the
-- current platform for which the library is built.

#define IS_PORTABLE

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define IS_WINDOWS
#include "Streamly/Internal/FileSystem/WindowsPath.hs"
#else
#include "Streamly/Internal/FileSystem/PosixPath.hs"
#endif
