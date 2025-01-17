-- |
-- Module      : Streamly.FileSystem.Path
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- File system paths with flexible (gradual) typing, extensible,
-- high-performance, preserving the OS and filesystem encoding.
--
-- /Flexible/: you can choose the level of type safety you want. 'Path' is the
-- basic path type which can represent a file, directory, absolute or relative
-- path with no restrictions. Depending on how much type safety you want, you
-- can choose appropriate type wrappers or a combination of those to wrap the
-- 'Path' type.
--
-- = Rooted Paths vs Branches
--
-- For the safety of the path append operation we make the distinction of
-- rooted paths vs branches. A path that starts from some implicit or
-- explicit root in the file system is a rooted path, for example, @\/usr\/bin@
-- is a rooted path starting from an explicit file system root directory @/@.
-- Similarly, @.\/bin@ is a path with an implicit root, this path is hanging
-- from the current directory. A path that is not rooted is called a branch
-- e.g. @local\/bin@ is a branch.
--
-- This distinction affords safety to the path append operation. We can always
-- append a branch to a rooted path or to another branch. However, it does
-- not make sense to append a rooted path to another rooted path. The default
-- append operation in the Path module checks for this and fails if the
-- operation is incorrect. However, the programmer can force it by using the
-- unsafe version of the append operation. You can also drop the root
-- explicitly and use the safe append operation.
--
-- The "Streamly.FileSystem.Path.Seg" module provides explicit typing of path
-- segments e.g. rooted paths vs branches. Rooted paths are represented by the
-- @Rooted Path@ type and branches are represented by the @Branch Path@ type.
-- If you use the 'Path' type then append can fail if you try to append a
-- rooted path to another path, but if you use @Rooted Path@ and @Branch Path@
-- types then append can never fail at run time as the types would not allow it
-- at compile time.
--
-- = Absolute vs Relative Rooted Paths
--
-- Rooted paths can be absolute or relative. Absolute paths have an absolute
-- root e.g. @\/usr\/bin@. Relative paths have a dynamic or relative root e.g.
-- @.\/local\/bin@, or @.@, in these cases the root is current directory which
-- is not absolute but can change dynamically. Note that there is no type level
-- distinction for absolute and relative paths. The append operation requires a
-- distinction between Rooted and Branch only.
--
-- = File vs Directory Paths
--
-- Independent of the rooted or branch distinction you can also make a type
-- level distinction between file and directory type nodes using the
-- "Streamly.FileSystem.Path.Node" module. @File Path@ type represents a file
-- whereas @Dir Path@ represents a directory. This distinction provides safety
-- against appending a path to a file. Append operation does not allow
-- appending to 'File' types.
--
-- By default a path with a trailing separator is implicitly considered a
-- directory path. However, the absence of a trailing separator does not convey
-- any information, it could either be a directory or a file. Thus the append
-- operation allows appending to even paths that do not have a trailing
-- separator. However, when creating a typed path of 'File' type the conversion
-- fails unless we explicitly drop the trailing separator.
--
-- = Flexible Typing
--
-- You can use the 'Rooted', 'Branch' or 'Dir', 'File' types independent of
-- each other by using only the required module. If you want both types of
-- distinctions then you can use them together as well using the
-- "Streamly.FileSystem.Path.SegNode" module.  For example, the @Rooted (Dir
-- Path)@ represents a rooted path which is a directory. You can only append to
-- a path that has 'Dir' in it and you can only append a 'Branch' type.
--
-- You can choose to use just the basic 'Path' type or any combination of safer
-- types. You can upgrade or downgrade the safety by converting types using the
-- @adapt@ operation. Whenever a less restrictive path type is converted to a
-- more restrictive path type, the conversion involves run-time checks and it
-- may fail. However, a more restrictive path type can be freely converted to a
-- less restrictive one.
--
-- = Extensibility
--
-- You can define your own newtype wrappers similar to 'File' or 'Dir' to
-- provide custom restrictions if you want.
--
-- = Compatibility
--
-- Any path type can be converted to the 'FilePath' type using the 'toString'
-- operation. Operations to convert to and from 'OsPath' type at zero cost are
-- provided in the @streamly-filepath@ package. This is possible because the
-- types use an underlying representation which is compatible with the 'OsPath'
-- type.
--
-- = String Creation Quasiquoter
--
-- You may find the 'str' quasiquoter from "Streamly.Unicode.String" to be
-- useful in creating paths.
--

module Streamly.FileSystem.Path
    (
    -- * Type
      Path

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Construction
    , fromString

    -- * Statically Verified String Literals
    -- | Quasiquoters.
    , path

    -- * Statically Verified Strings
    -- | Template Haskell expression splices.
    , pathE

    -- * Elimination
    , toString

    -- * Operations
    -- , dropTrailingSeparators
    , isRooted
    , isBranch

    -- * Combinators
    , append
    )
where

import Streamly.Internal.FileSystem.Path
