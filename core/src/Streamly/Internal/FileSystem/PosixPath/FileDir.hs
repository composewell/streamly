{-# LANGUAGE TemplateHaskell #-}
-- For constraints on "append"
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Streamly.Internal.FileSystem.PosixPath.FileDir
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.FileSystem.PosixPath.FileDir
    (
    -- * Path Types
      File (..)
    , Dir (..)
    , IsFileDir
    , IsPath (..)

    -- * Construction
    , dirFromString
    , fileFromString

    -- * Statically Verified String Literals
    -- | Quasiquoters.
    , dir
    , file

    -- * Statically Verified Strings
    -- | Template Haskell expression splices.
    , mkDir
    , mkFile

    -- * Operations
    , append
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.FileSystem.Path.Common (OS(..), mkQ)
import Streamly.Internal.FileSystem.PosixPath (PosixPath(..))

import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.FileSystem.PosixPath as Posix

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.FileSystem.Path.Posix as Path
-}

newtype File a = File a
newtype Dir a = Dir a

-- | Constraint to check if a type use 'File' or 'Dir' as the outermost
-- constructor.
class IsFileDir a

instance IsFileDir (File a)
instance IsFileDir (Dir a)

instance IsPath PosixPath (File PosixPath) where
    unsafeFromPath = File
    fromPath p = pure (File p)
    toPath (File p) = p

instance IsPath PosixPath (Dir PosixPath) where
    unsafeFromPath = Dir
    fromPath p = pure (Dir p)
    toPath (Dir p) = p

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

-- | Any valid path could be a directory.
dirFromString :: MonadThrow m => String -> m (Dir PosixPath)
dirFromString s = Dir <$> Posix.fromString s

-- | Cannot have "." or ".." as last component.
fileFromString :: MonadThrow m => String -> m (File PosixPath)
fileFromString s = do
    r@(PosixPath _arr) <- Posix.fromString s
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

liftDir :: Quote m => Dir PosixPath -> m Exp
liftDir p =
    [| Dir (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p))) |]

liftFile :: Quote m => File PosixPath -> m Exp
liftFile p =
    [| File (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p))) |]

-- | Generates an @Dir Path@ type.
--
mkDir :: String -> Q Exp
mkDir = either (error . show) liftDir . dirFromString

-- | Generates an @File Path@ type.
--
mkFile :: String -> Q Exp
mkFile = either (error . show) liftFile . fileFromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- | Generates a @Dir PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([dir|usr|] :: Dir PosixPath)
-- "usr"
--
dir :: QuasiQuoter
dir = mkQ mkDir

-- | Generates a @File PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([dir|usr|] :: Dir PosixPath)
-- "usr"
--
file :: QuasiQuoter
file = mkQ mkFile

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Seg path.

-- | Append a path to a 'Dir' path.
--
-- >>> Path.toString (Path.append [dir|/usr|] [dir|bin|] :: Dir PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.append [dir|/usr|] [file|bin|] :: File PosixPath)
-- "/usr/bin"
--
-- Fails if the second path is a specific location and not a path segment.
--
{-# INLINE append #-}
append :: (IsPath PosixPath (a PosixPath), IsFileDir (a PosixPath)) =>
    Dir PosixPath -> a PosixPath -> a PosixPath
append (Dir a) b =
    unsafeFromPath $ Posix.unsafeAppend (toPath a) (toPath b)
