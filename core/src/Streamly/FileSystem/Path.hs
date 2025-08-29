{-# LANGUAGE CPP #-}
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
-- its operations — including support for both pinned and unpinned
-- representations. The API integrates with streams, prioritizes safety,
-- flexibility, and performance. It supports configurable equality for
-- cross-platform compatibility and user-defined path matching. It is designed
-- for extensibility and fine-grained type safety as well. For type-safe
-- adaptations, see the "Streamly.Internal.FileSystem.Path.*" modules.
--
-- 'Path' is interconvertible with the 'OsPath' type from the @filepath@
-- package at zero runtime cost. While the API is mostly compatible with that
-- of the @filepath@ package, some differences exist due to a slightly
-- different design philosophy focused on better safety.
--
-- = Rooted vs Unrooted Paths
--
-- To ensure the safety of the path append operation, we distinguish between
-- rooted paths and free path segments or unrooted paths. A path that starts
-- from an explicit or implicit file system root is called a rooted path or an
-- anchored path. For example, @\/usr\/bin@ is a rooted path with @/@ as an
-- explicit root directory. Similarly, @.\/bin@ is a rooted path with the
-- current directoy \".\" as an implicit root. A path that is not rooted is
-- called an unrooted path or unanchored path; for example, @local\/bin@ is an
-- unrooted path.
--
-- This distinction ensures the safety of the path append operation. You can
-- append only an unrooted path to another path, it does not make sense to
-- append a rooted path to another path. The default append operation in the
-- Path module checks for this and fails if the operation is invalid.
--
-- Rooted vs unrooted distinction is a stricter form of relative vs absolute
-- path distinction. In this model, for better safety, paths relative to the
-- current directory are also treated in the same way as absolute paths, from
-- the perspective of a path append operation. This is because the  meaning of
-- current directory is context dependent and dynamic, therefore, appending it
-- to another path is not allowed. Only unrooted path segments (e.g.
-- @local/bin@) can be appended to any other path using safe operations.
--
-- = File vs. Directory Paths
--
-- By default, a path with a trailing separator (e.g. @local/@) is implicitly
-- considered a directory path. However, the absence of a trailing separator
-- does not indicate whether the path is a file or a directory — it could be
-- either. Therefore, when using the @Path@ type, the append operation allows
-- appending to paths even if they lack a trailing separator.
--
-- = Compatibility with the filepath package
--
-- Any path type can be converted to the 'FilePath' type from the @filepath@
-- package by using the 'toString' operation. Operations to convert to and from
-- the 'OsPath' type at zero cost are provided in the @streamly-filepath@
-- package. Zero-cost interconversion is possible because the 'Path' type uses
-- an underlying representation which is compatible with the 'OsPath' type.
--
-- = Path Creation Quasiquoter
--
-- The 'path' quasiquoter is useful in creating valid paths that are checked
-- during the compile time.

module Streamly.FileSystem.Path
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Type
      Path
    , OsWord

    -- * Construction
    , validatePath
    , fromArray
    , fromString
    , fromString_

    -- * Statically Verified String Literals
    -- | Quasiquoters.
    , path

    -- * Statically Verified Strings
    -- | Template Haskell expression splices.
    , pathE

    -- * Elimination
    , toArray
    -- , toChars -- need fromChars as well
    , toString
    -- , asOsCString

    -- * Path Info
    , isRooted
    , isUnrooted

    -- * Joining
    , unsafeJoin
    , join
    , joinStr

    -- * Splitting root
    , splitRoot

    -- * Splitting path components
    , splitPath
    -- , splitPath_

    -- * Splitting file extension
    , splitExtension
    , takeExtension
    , dropExtension
    -- , addExtension
    -- , replaceExtension

    -- * Splitting file and dir
    , splitFile
    , takeFileName
    , takeDirectory
    , takeFileBase

    -- * Equality
    , EqCfg
    , ignoreCase
    , ignoreTrailingSeparators
    , allowRelativeEquality

    , eqPath
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

#include "DocTestFileSystemPath.hs"
