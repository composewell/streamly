-- |
-- Module      : Streamly.FileSystem.Path
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- File system paths that are extensible, high-performance and preserve the OS
-- and filesystem encoding.
--
-- The 'Path' type is built on top of Streamly's 'Array' type, leveraging all
-- its operations and advantages—including support for both pinned and unpinned
-- representations. It is designed for extensibility and fine-grained type
-- safety. For type-safe adaptations, refer to the
-- "Streamly.Internal.FileSystem.Path.*" modules. The type also offers a
-- powerful and flexible path comparison mechanism.
--
-- 'Path' is interconvertible with the 'OsPath' type from the filepath package
-- at zero runtime cost. While the API is mostly compatible with that of the
-- filepath package, some differences exist due to a slightly different design
-- philosophy focused on enhanced safety.
--
-- = Rooted Paths vs Branches
--
-- To ensure the safety of the append operation, we distinguish between rooted
-- paths and branch-type paths. A path that starts from an explicit or implicit
-- root in the file system is called a rooted path or an anchored path. For
-- example, @\/usr\/bin@ is a rooted path starting from the explicit root
-- directory @/@. Similarly, @.\/bin@ is rooted implicitly, anchored at the
-- current directory. A path that is not rooted is called a branch or
-- unanchored path; for example, @local\/bin@ is a branch.
--
-- This distinction ensures the safety of the path append operation. You can
-- always append a branch to a rooted path or to another branch. However, it
-- does not make sense to append one rooted path to another. The default append
-- operation in the Path module checks for this and fails if the operation is
-- invalid. However, the programmer can force the unsafe behavior by using the
-- unsafe append operation. Alternatively, you can drop the root explicitly and
-- use the safe append.
--
-- Rooted vs branch distinction is a stricter form of relative vs absolute path
-- distinction. Essentially, paths relative to the current directory are also
-- treated in the same way as absolute paths, from the perspective of an append
-- operation. Only branch-type paths can be appended to any other path using
-- safe operations.
--
-- = File vs. Directory Paths
--
-- By default, a path with a trailing separator is implicitly considered a
-- directory path. However, the absence of a trailing separator does not
-- indicate whether the path is a file or a directory — it could be either.
-- Therefore, when using the @Path@ type, the append operation allows appending
-- to paths even if they lack a trailing separator.
--
-- = Compatibility with the filepath package
--
-- Any path type can be converted to the 'FilePath' type from the filepath
-- package by using the 'toString' operation. Operations to convert to and from
-- the 'OsPath' type at zero cost are provided in the @streamly-filepath@
-- package. Zero-cost interconversion is possible because the 'Path' type use
-- an underlying representation which is compatible with the 'OsPath' type.
--
-- = Path Creation Quasiquoter
--
-- The 'path' quasiquoter is useful in creating valid paths that are checked
-- during the compile time.

module Streamly.FileSystem.Path
    (
    -- * Type
      Path

    -- * Construction
    , fromChunk
    , fromString

    -- * Statically Verified String Literals
    -- | Quasiquoters.
    , path

    -- * Statically Verified Strings
    -- | Template Haskell expression splices.
    , pathE

    -- * Elimination
    , toChunk
    , toChars
    , toString
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    , asCString
#else
    , asCWString
#endif

    -- * Path Info
    , isRooted
    , isBranch

    -- * Separators
    , dropTrailingSeparators
    , hasTrailingSeparator
    , addTrailingSeparator

    -- * Joining
    , unsafeExtend
    , extend
    , extendByString

    -- * Splitting
    , splitRoot
    , splitPath
    , splitPath_
    , splitFile

    -- * Extension
    , splitExtension
    , dropExtension
    -- , addExtension
    -- , replaceExtension

    -- ** Path View
    , takeFileName
    , takeDirectory
    , takeExtension
    , takeBaseName

    -- * Equality
    , EqCfg
    , eqPath

#ifndef IS_WINDOWS
    -- ** Config options (Posix)
    , ignoreTrailingSeparators
    , ignoreCase
    , allowRelativeEquality
#endif
    )
where

{- Documentation on typed paths. We can add this back into the module level
 documentation when we introduce the typed paths.

-- = Rooted Paths vs Branches
--
-- /Flexible typing/: you can choose the level of type safety you want. 'Path'
-- is the basic path type which can represent a file, directory, absolute or
-- relative path with no restrictions. Depending on how much type safety you
-- want, you can choose appropriate type wrappers or a combination of those to
-- wrap the 'Path' type in stricter types.

-- The "Streamly.FileSystem.Path.Seg" module provides explicit types for path
-- segments, distinguishing rooted paths from branches. Rooted paths use the
-- @Rooted Path@ type, and branches use the @Branch Path@ type. If you use the
-- generic 'Path' type, append may fail at run time if you attempt to append
-- a rooted path to another rooted path. In contrast, using the @Rooted Path@
-- and @Branch Path@ types guarantees compile-time safety, preventing such errors.

-- = File vs. Directory Paths
--
-- Independent of the rooted or branch distinction, you can also make a
-- type-level distinction between file and directory nodes using the
-- "Streamly.FileSystem.Path.Node" module. The type @File Path@ represents a
-- file, whereas @Dir Path@ represents a directory. This distinction provides
-- safety against appending to file type paths — append operations are not
-- allowed on paths of type 'File'.

-- = Flexible Typing
--
-- You can use the 'Rooted', 'Branch', 'Dir', and 'File' types independently by
-- importing only the required modules. If you want both types of distinctions,
-- you can use them together via the "Streamly.FileSystem.Path.SegNode" module.
-- For example, @Rooted (Dir Path)@ represents a rooted path that is a
-- directory. You can append other paths only to paths that have a 'Dir' type,
-- and only a path of type 'Branch' can be appended.
--
-- You may choose to use the basic 'Path' type or any combination of the safer
-- types. You can upgrade or downgrade the safety level by converting between
-- types using the @adapt@ operation. When converting from a less restrictive
-- type to a more restrictive one, run-time checks are performed, and the
-- conversion may fail. However, converting from a more restrictive type to a
-- less restrictive one is always allowed.
--
-- = Extensibility
--
-- You can define your own newtype wrappers similar to 'File' or 'Dir' to
-- provide custom restrictions if you want.
--

-}

import Streamly.Internal.FileSystem.Path
