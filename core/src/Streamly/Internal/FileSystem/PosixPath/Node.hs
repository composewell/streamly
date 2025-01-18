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
-- Module      : Streamly.Internal.FileSystem.OS_PATH.Node
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
module Streamly.Internal.FileSystem.OS_PATH.Node
    (
    -- * Types
      File (..)
    , Dir (..)
    , IsNode

    -- * Statically Verified Path Literals
    -- | Quasiquoters.
    , dir
    , file

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , dirE
    , fileE

    -- * Operations
    , append
    )
where

import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Streamly.Internal.Data.Path (IsPath(..))
import Streamly.Internal.FileSystem.Path.Common (OS(..), mkQ)
import Streamly.Internal.FileSystem.OS_PATH (OS_PATH(..))

import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.FileSystem.OS_PATH as OsPath

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath)
>>> import Streamly.Internal.FileSystem.PosixPath.Node (File, Dir, file, dir)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
>>> import qualified Streamly.Internal.FileSystem.PosixPath.Node as Node
-}

newtype File a = File a
newtype Dir a = Dir a

-- | Constraint to check if a type uses 'File' or 'Dir' as the outermost
-- constructor.
class IsNode a

instance IsNode (File a)
instance IsNode (Dir a)

instance IsPath OS_PATH (File OS_PATH) where
    unsafeFromPath = File

    fromPath p@(OS_PATH arr) = do
        !_ <- Common.validateFile OS_NAME arr
        pure $ File p

    toPath (File p) = p

instance IsPath OS_PATH (Dir OS_PATH) where
    unsafeFromPath = Dir
    fromPath p = pure (Dir p)
    toPath (Dir p) = p

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftDir :: Dir OS_PATH -> Q Exp
liftDir (Dir p) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Dir OS_PATH |]

liftFile :: File OS_PATH -> Q Exp
liftFile (File p) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: File OS_PATH |]

-- | Generates a Haskell expression of type @Dir OS_PATH@.
--
dirE :: String -> Q Exp
dirE = either (error . show) liftDir . OsPath.fromString

-- | Generates a Haskell expression of type @File OS_PATH@.
--
fileE :: String -> Q Exp
fileE = either (error . show) liftFile . OsPath.fromString

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
dir = mkQ dirE

-- | Generates a @File OS_PATH@ type from a quoted literal.
--
-- >>> Path.toString ([file|usr|] :: File PosixPath)
-- "usr"
--
file :: QuasiQuoter
file = mkQ fileE

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Seg path.

-- | Append a 'Dir' or 'File' path to a 'Dir' path.
--
-- >>> Path.toString (Node.append [dir|/usr|] [dir|bin|] :: Dir PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Node.append [dir|/usr|] [file|bin|] :: File PosixPath)
-- "/usr/bin"
--
-- Fails if the second path is a specific location and not a path segment.
--
{-# INLINE append #-}
append :: (IsPath OS_PATH (a OS_PATH), IsNode (a OS_PATH)) =>
    Dir OS_PATH -> a OS_PATH -> a OS_PATH
append (Dir a) b =
    unsafeFromPath $ OsPath.unsafeAppend (toPath a) (toPath b)
