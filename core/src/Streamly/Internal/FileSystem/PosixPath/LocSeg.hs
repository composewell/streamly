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

    -- * Construction
    -- ** From String
    , locFromString -- locString?
    , segFromString

    -- ** Statically Verified String Literals
    -- | Quasiquoters.
    , loc
    , seg

    -- ** Statically Verified Strings
    -- | Template Haskell expression splices.
    , locExp
    , segExp

    -- * Operations
    , append
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Data.Word (Word8)
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.FileSystem.Path.Common (OS(..), mkQ)
import Streamly.Internal.FileSystem.OS_PATH (OS_PATH(..))

import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.FileSystem.OS_PATH as OsPath

import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

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
    fromPath p = pure (Loc p)
    toPath (Loc p) = p

instance IsPath OS_PATH (Seg OS_PATH) where
    unsafeFromPath = Seg
    fromPath p = pure (Seg p)
    toPath (Seg p) = p

-- | Constraint to check if a type has Loc or Seg annotations.
class IsLocSeg a

instance IsLocSeg (Loc a)
instance IsLocSeg (Seg a)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

locFromChunk :: MonadThrow m => Array Word8 -> m (Loc OS_PATH)
locFromChunk arr = do
    if Common.isLocation Posix arr
    then pure $ Loc (OS_PATH arr)
    -- XXX Add more detailed error msg with all valid examples.
    else throwM $ InvalidPath "Must be a specific location, not a path segment"

locFromString :: MonadThrow m => String -> m (Loc OS_PATH)
locFromString s = do
    OS_PATH arr <- OsPath.fromString s
    locFromChunk arr

segFromChunk :: MonadThrow m => Array Word8 -> m (Seg OS_PATH)
segFromChunk arr = do
    if Common.isSegment Posix arr
    then pure $ Seg (OS_PATH arr)
    -- XXX Add more detailed error msg with all valid examples.
    else throwM $ InvalidPath "Must be a path segment, not a specific location"

segFromString :: MonadThrow m => String -> m (Seg OS_PATH)
segFromString s = do
    OS_PATH arr <- OsPath.fromString s
    segFromChunk arr

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

liftLoc :: Loc OS_PATH -> Q Exp
liftLoc (Loc p) =
    [| Loc (OsPath.unsafeFromString $(lift $ OsPath.toString p)) |]

liftSeg :: Seg OS_PATH -> Q Exp
liftSeg (Seg p) =
    [| Seg (OsPath.unsafeFromString $(lift $ OsPath.toString p)) |]

-- | Generates a Haskell expression of type @Loc OS_PATH@.
--
locExp :: String -> Q Exp
locExp = either (error . show) liftLoc . locFromString

-- | Generates a Haskell expression of type @Seg OS_PATH@.
--
segExp :: String -> Q Exp
segExp = either (error . show) liftSeg . segFromString

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
loc = mkQ locExp

-- | Generates a @Seg Path@ type from a quoted literal.
--
-- >>> Path.toString ([seg|usr|] :: Seg PosixPath)
-- "usr"
--
seg :: QuasiQuoter
seg = mkQ segExp

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
