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
-- Module      : Streamly.Internal.FileSystem.OS_PATH.FileDir
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- This module provides a type safe path append operation by distinguishing
-- paths between files and directories. Files are represented by the @File
-- OS_PATH@ type and directories are represented by the @Dir OS_PATH@ type.
--
-- This distinction provides safety against appending a path to a file. Append
-- operation allows appending to only 'Dir' types.
--
module Streamly.Internal.FileSystem.OS_PATH.FileDir
    (
    -- * Types
      File (..)
    , Dir (..)
    , IsFileDir

    -- * Construction
    , dirFromString -- dirString?
    , fileFromString

    -- ** Statically Verified String Literals
    -- | Quasiquoters.
    , dir
    , file

    -- ** Statically Verified Strings
    -- | Template Haskell expression splices.
    , dirExp
    , fileExp

    -- * Operations
    , append
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.FileSystem.Path.Common (OS(..), mkQ)
import Streamly.Internal.FileSystem.OS_PATH (OS_PATH(..))

import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.FileSystem.OS_PATH as OsPath

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath)
>>> import Streamly.Internal.FileSystem.PosixPath.FileDir (File, Dir, file, dir)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
>>> import qualified Streamly.Internal.FileSystem.PosixPath.FileDir as PathFD
-}

newtype File a = File a
newtype Dir a = Dir a

-- | Constraint to check if a type uses 'File' or 'Dir' as the outermost
-- constructor.
class IsFileDir a

instance IsFileDir (File a)
instance IsFileDir (Dir a)

instance IsPath OS_PATH (File OS_PATH) where
    unsafeFromPath = File
    fromPath p = pure (File p)
    toPath (File p) = p

instance IsPath OS_PATH (Dir OS_PATH) where
    unsafeFromPath = Dir
    fromPath p = pure (Dir p)
    toPath (Dir p) = p

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

-- | Any valid path could be a directory.
dirFromString :: MonadThrow m => String -> m (Dir OS_PATH)
dirFromString s = Dir <$> OsPath.fromString s

-- | Cannot have "." or ".." as last component.
fileFromString :: MonadThrow m => String -> m (File OS_PATH)
fileFromString s = do
    r@(OS_PATH _arr) <- OsPath.fromString s
    -- XXX take it from the array
    let s1 = reverse $ takeWhile (not . Common.isSeparator Posix) (reverse s)
     in if s1 == "."
        then throwM $ InvalidPath "A file name cannot be \".\""
        else if s1 == ".."
        then throwM $ InvalidPath "A file name cannot be \"..\""
        else (pure . File) r

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftDir :: Dir OS_PATH -> Q Exp
liftDir (Dir p) =
    [| Dir (OsPath.unsafeFromString $(lift $ OsPath.toString p)) |]

liftFile :: File OS_PATH -> Q Exp
liftFile (File p) =
    [| File (OsPath.unsafeFromString $(lift $ OsPath.toString p)) |]

-- | Generates a Haskell expression of type @Dir OS_PATH@.
--
dirExp :: String -> Q Exp
dirExp = either (error . show) liftDir . dirFromString

-- | Generates a Haskell expression of type @File OS_PATH@.
--
fileExp :: String -> Q Exp
fileExp = either (error . show) liftFile . fileFromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- | Generates a @Dir OS_PATH@ type from a quoted literal.
--
-- >>> Path.toString ([dir|usr|] :: Dir PosixPath)
-- "usr"
--
dir :: QuasiQuoter
dir = mkQ dirExp

-- | Generates a @File OS_PATH@ type from a quoted literal.
--
-- >>> Path.toString ([file|usr|] :: File PosixPath)
-- "usr"
--
file :: QuasiQuoter
file = mkQ fileExp

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Seg path.

-- | Append a 'Dir' or 'File' path to a 'Dir' path.
--
-- >>> Path.toString (PathFD.append [dir|/usr|] [dir|bin|] :: Dir PosixPath)
-- "/usr/bin"
-- >>> Path.toString (PathFD.append [dir|/usr|] [file|bin|] :: File PosixPath)
-- "/usr/bin"
--
-- Fails if the second path is a specific location and not a path segment.
--
{-# INLINE append #-}
append :: (IsPath OS_PATH (a OS_PATH), IsFileDir (a OS_PATH)) =>
    Dir OS_PATH -> a OS_PATH -> a OS_PATH
append (Dir a) b =
    unsafeFromPath $ OsPath.unsafeAppend (toPath a) (toPath b)
