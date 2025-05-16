{-# LANGUAGE TemplateHaskell #-}
-- For constraints on "append"
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

#if defined(IS_WINDOWS)
#define OS_NAME Windows
#define OS_PATH WindowsPath
#else
#define OS_NAME Posix
#define OS_PATH PosixPath
#endif

-- |
-- Module      : Streamly.Internal.FileSystem.OS_PATH.Seg
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- This module provides a type safe path append operation by distinguishing
-- paths between rooted paths and branches. Rooted paths are represented by the
-- @Rooted OS_PATH@ type and branches are represented by the @Branch OS_PATH@
-- type. Rooted paths are paths that are attached to specific roots in the file
-- system. Rooted paths could be absolute or relative e.g. @\/usr\/bin@,
-- @.\/local\/bin@, or @.@. Branches are a paths that are not attached to a
-- specific root e.g. @usr\/bin@, @local\/bin@, or @../bin@ are branches.
--
-- This distinction provides a safe path append operation which cannot fail.
-- These types do not allow appending a rooted path to any other path. Only
-- branches can be appended.
--
module Streamly.Internal.FileSystem.OS_PATH.Seg
    (
    -- * Types
      Rooted (..)
    , Branch (..)
    , IsSeg

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , rt
    , br

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , rtE
    , brE

    -- * Operations
    , extend
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Streamly.Internal.Data.Path (IsPath(..), PathException(..))
import Streamly.Internal.FileSystem.Path.Common (mkQ)
import Streamly.Internal.FileSystem.OS_PATH (OS_PATH(..))

import qualified Streamly.Internal.FileSystem.OS_PATH as OsPath

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath)
>>> import Streamly.Internal.FileSystem.PosixPath.Seg (Rooted, Branch, rt, br)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
>>> import qualified Streamly.Internal.FileSystem.PosixPath.Seg as Seg
-}

newtype Rooted a = Rooted a
newtype Branch a = Branch a

instance IsPath OS_PATH (Rooted OS_PATH) where
    unsafeFromPath = Rooted
    fromPath p =
        if OsPath.isRooted p
        then pure (Rooted p)
        -- XXX Add more detailed error msg with all valid examples.
        else throwM $ InvalidPath
                $ "Must be a specific location, not a path segment: "
                ++ OsPath.toString p
    toPath (Rooted p) = p

instance IsPath OS_PATH (Branch OS_PATH) where
    unsafeFromPath = Branch
    fromPath p =
        if OsPath.isBranch p
        then pure (Branch p)
        -- XXX Add more detailed error msg with all valid examples.
        else throwM $ InvalidPath
                $ "Must be a path segment, not a specific location: "
                ++ OsPath.toString p
    toPath (Branch p) = p

-- | Constraint to check if a type has Rooted or Branch annotations.
class IsSeg a

instance IsSeg (Rooted a)
instance IsSeg (Branch a)

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

liftRooted :: Rooted OS_PATH -> Q Exp
liftRooted (Rooted p) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Rooted OS_PATH |]

liftBranch :: Branch OS_PATH -> Q Exp
liftBranch (Branch p) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Branch OS_PATH |]

-- | Generates a Haskell expression of type @Rooted OS_PATH@.
--
rtE :: String -> Q Exp
rtE = either (error . show) liftRooted . OsPath.fromString

-- | Generates a Haskell expression of type @Branch OS_PATH@.
--
brE :: String -> Q Exp
brE = either (error . show) liftBranch . OsPath.fromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- | Generates a @Rooted Path@ type from a quoted literal.
--
-- >>> Path.toString ([rt|/usr|] :: Rooted PosixPath)
-- "/usr"
--
rt :: QuasiQuoter
rt = mkQ rtE

-- | Generates a @Branch Path@ type from a quoted literal.
--
-- >>> Path.toString ([br|usr|] :: Branch PosixPath)
-- "usr"
--
br :: QuasiQuoter
br = mkQ brE

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Branch path.

-- | Append a 'Branch' type path to a 'Rooted' path or 'Branch' path.
--
-- >>> Path.toString (Seg.extend [rt|/usr|] [br|bin|] :: Rooted PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Seg.extend [br|usr|] [br|bin|] :: Branch PosixPath)
-- "usr/bin"
--
{-# INLINE extend #-}
extend ::
    (
      IsSeg (a OS_PATH)
    , IsPath OS_PATH (a OS_PATH)
    ) => a OS_PATH -> Branch OS_PATH -> a OS_PATH
extend a (Branch c) =
    unsafeFromPath $ OsPath.unsafeExtend (toPath a) (toPath c)
