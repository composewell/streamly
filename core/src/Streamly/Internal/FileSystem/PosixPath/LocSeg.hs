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
-- Module      : Streamly.Internal.FileSystem.OS_PATH.LocSeg
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- This module provides a type safe path append operation by distinguishing
-- paths between locations and segments. Locations are represented by the @Loc
-- OS_PATH@ type and path segments are represented by the @Seg OS_PATH@ type.
-- Locations are paths pointing to specific objects in the file system absolute
-- or relative e.g. @\/usr\/bin@, @.\/local\/bin@, or @.@. Segments are a
-- sequence of path components without any reference to a location e.g.
-- @usr\/bin@, @local\/bin@, or @../bin@ are segments.
--
-- This distinction provides a safe append operation on paths which cannot
-- fail. These types do not allow appending a location to a path segment or to
-- another location. Only path segments can be appended.
--
module Streamly.Internal.FileSystem.OS_PATH.LocSeg
    (
    -- * Types
      Loc (..)
    , Seg (..)
    , IsLocSeg

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , loc
    , seg

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , locE
    , segE

    -- * Operations
    , append
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
>>> import Streamly.Internal.FileSystem.PosixPath.LocSeg (Loc, Seg, loc, seg)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
>>> import qualified Streamly.Internal.FileSystem.PosixPath.LocSeg as PathLS
-}

newtype Loc a = Loc a
newtype Seg a = Seg a

instance IsPath OS_PATH (Loc OS_PATH) where
    unsafeFromPath = Loc
    fromPath p =
        if OsPath.isRooted p
        then pure (Loc p)
        -- XXX Add more detailed error msg with all valid examples.
        else throwM $ InvalidPath
                $ "Must be a specific location, not a path segment: "
                ++ OsPath.toString p
    toPath (Loc p) = p

instance IsPath OS_PATH (Seg OS_PATH) where
    unsafeFromPath = Seg
    fromPath p =
        if OsPath.isBranch p
        then pure (Seg p)
        -- XXX Add more detailed error msg with all valid examples.
        else throwM $ InvalidPath
                $ "Must be a path segment, not a specific location: "
                ++ OsPath.toString p
    toPath (Seg p) = p

-- | Constraint to check if a type has Loc or Seg annotations.
class IsLocSeg a

instance IsLocSeg (Loc a)
instance IsLocSeg (Seg a)

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

liftLoc :: Loc OS_PATH -> Q Exp
liftLoc (Loc p) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Loc OS_PATH |]

liftSeg :: Seg OS_PATH -> Q Exp
liftSeg (Seg p) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Seg OS_PATH |]

-- | Generates a Haskell expression of type @Loc OS_PATH@.
--
locE :: String -> Q Exp
locE = either (error . show) liftLoc . OsPath.fromString

-- | Generates a Haskell expression of type @Seg OS_PATH@.
--
segE :: String -> Q Exp
segE = either (error . show) liftSeg . OsPath.fromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- | Generates a @Loc Path@ type from a quoted literal.
--
-- >>> Path.toString ([loc|/usr|] :: Loc PosixPath)
-- "/usr"
--
loc :: QuasiQuoter
loc = mkQ locE

-- | Generates a @Seg Path@ type from a quoted literal.
--
-- >>> Path.toString ([seg|usr|] :: Seg PosixPath)
-- "usr"
--
seg :: QuasiQuoter
seg = mkQ segE

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Seg path.

-- | Append a 'Seg' path to a 'Loc' or 'Seg' path.
--
-- >>> Path.toString (PathLS.append [loc|/usr|] [seg|bin|] :: Loc PosixPath)
-- "/usr/bin"
-- >>> Path.toString (PathLS.append [seg|usr|] [seg|bin|] :: Seg PosixPath)
-- "usr/bin"
--
{-# INLINE append #-}
append ::
    (
      IsLocSeg (a OS_PATH)
    , IsPath OS_PATH (a OS_PATH)
    ) => a OS_PATH -> Seg OS_PATH -> a OS_PATH
append a (Seg c) = unsafeFromPath $ OsPath.unsafeAppend (toPath a) (toPath c)
