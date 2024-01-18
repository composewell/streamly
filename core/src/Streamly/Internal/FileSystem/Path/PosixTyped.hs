{-# LANGUAGE TemplateHaskell #-}
-- For constraints on "append"
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.FileSystem.Path.Posix
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- When @Abs/Rel@ and @File/Dir@ both are present, @Abs/Rel@ must be
-- outermost constructors and @File/Dir@ as inner. Thus the types File (Abs
-- a) or Dir (Abs a) are not allowed but Abs (Dir a) and Abs (File a) are
-- allowed.

module Streamly.Internal.FileSystem.Path.PosixTyped
    (
    -- * Path Types
      HasDir

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Statically Verified String Literals
    -- quasiquoters
    , absdir
    , reldir
    , absfile
    , relfile

    -- * Statically Verified Strings
    -- XXX Do we need these if we have quasiquoters? These may be useful if we
    -- are generating strings statically using methods other than literals or
    -- if we are doing some text processing on strings before using them.
    -- TH macros
    , mkAbsDir
    , mkRelDir
    , mkAbsFile
    , mkRelFile

    -- * Operations
    , append
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.FileSystem.Path.Common (mkQ)
import Streamly.Internal.FileSystem.Path.Posix (PosixPath(..))
import Streamly.Internal.FileSystem.Path.PosixAbsRel (Abs(..), Rel(..), IsAbsRel)
import Streamly.Internal.FileSystem.Path.PosixFileDir (File(..), Dir(..))

import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.FileSystem.Path.Posix as Posix
import qualified Streamly.Internal.FileSystem.Path.PosixAbsRel as AbsRel

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path
import Prelude hiding (abs)

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.FileSystem.Path.Posix as Path
-}

-- Note that (Abs a) may also be a directory if "a" is (Dir b), but it can also
-- be a file if "a" is (File b). Therefore, the constraints are put on a more
-- spspecific type e.g. (Abs PosixPath) may be a dir.

-- | Constraint to check if a type represents a directory.
class HasDir a

instance HasDir (Dir a)
instance HasDir (Abs (Dir a))
instance HasDir (Rel (Dir a))

instance IsPath PosixPath (Abs (File PosixPath)) where
    unsafeFromPath p = Abs (File p)
    fromPath p = pure (Abs (File p))
    toPath (Abs (File p)) = p

instance IsPath PosixPath (Abs (Dir PosixPath)) where
    unsafeFromPath p = Abs (Dir p)
    fromPath p = pure (Abs (Dir p))
    toPath (Abs (Dir p)) = p

instance IsPath PosixPath (Rel (File PosixPath)) where
    unsafeFromPath p = Rel (File p)
    fromPath p = pure (Rel (File p))
    toPath (Rel (File p)) = p

instance IsPath PosixPath (Rel (Dir PosixPath)) where
    unsafeFromPath p = Rel (Dir p)
    fromPath p = pure (Rel (Dir p))
    toPath (Rel (Dir p)) = p

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency. If the argument path is already verfied for a property, we
-- should not verify it again e.g. if we adapt (Abs path) as (Abs (Dir path))
-- then we should not verify it to be Abs again.

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one.
--
-- You can only upgrade or downgrade type safety. Converting Abs to Rel or File
-- to Dir will definitely fail.
adapt :: (MonadThrow m, IsPath PosixPath a, IsPath PosixPath b) => a -> m b
adapt p = fromPath (toPath p :: PosixPath)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

absDirFromString :: MonadThrow m => String -> m (Abs (Dir PosixPath))
absDirFromString s = AbsRel.absFromString s >>= adapt

relDirFromString :: MonadThrow m => String -> m (Rel (Dir PosixPath))
relDirFromString s = AbsRel.relFromString s >>= adapt

absFileFromString :: MonadThrow m => String -> m (Abs (File PosixPath))
absFileFromString s = Posix.fromString s >>= adapt

relFileFromString :: MonadThrow m => String -> m (Rel (File PosixPath))
relFileFromString s = Posix.fromString s >>= adapt

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftAbsDir :: Quote m => Abs (Dir PosixPath) -> m Exp
liftAbsDir p =
    [| Abs (Dir (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

liftRelDir :: Quote m => Rel (Dir PosixPath) -> m Exp
liftRelDir p =
    [| Rel (Dir (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

liftAbsFile :: Quote m => Abs (File PosixPath) -> m Exp
liftAbsFile p =
    [| Abs (File (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

liftRelFile :: Quote m => Rel (File PosixPath) -> m Exp
liftRelFile p =
    [| Rel (File (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

-- | Generates an @Abs (Dir Path)@ type.
--
mkAbsDir :: String -> Q Exp
mkAbsDir = either (error . show) liftAbsDir . absDirFromString

-- | Generates an @Rel (Dir Path)@ type.
--
mkRelDir :: String -> Q Exp
mkRelDir = either (error . show) liftRelDir . relDirFromString

-- | Generates an @Abs (File Path)@ type.
--
mkAbsFile :: String -> Q Exp
mkAbsFile = either (error . show) liftAbsFile . absFileFromString

-- | Generates an @Rel (File Path)@ type.
--
mkRelFile :: String -> Q Exp
mkRelFile = either (error . show) liftRelFile . relFileFromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- XXX Change to "dirloc"?

-- | Generates an @Abs (Dir PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([absdir|/usr|] :: Abs (Dir PosixPath))
-- "/usr"
--
absdir :: QuasiQuoter
absdir = mkQ mkAbsDir

-- | Generates a @Rel (Dir PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([reldir|usr|] :: Rel (Dir PosixPath))
-- "usr"
--
reldir :: QuasiQuoter
reldir = mkQ mkRelDir

-- XXX Change to "fileloc"?

-- | Generates an @Abs (File PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([absfile|/usr|] :: Abs (File PosixPath))
-- "/usr"
--
absfile :: QuasiQuoter
absfile = mkQ mkAbsFile

-- | Generates an @Rel (File PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([relfile|usr|] :: Rel (File PosixPath))
-- "usr"
--
relfile :: QuasiQuoter
relfile = mkQ mkRelFile

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Rel path.

-- If you are not using File/Dir annotations then this is the only API you need
-- to combine paths.

-- | Use this API to combine paths when the first path is @Abs@ or @Rel@.
-- Second path must be @Rel@, if the second path is just @Dir@ or @File@ you
-- can wrap it in @Rel@ first.
--
-- If the first path is absolute then the return type is also absolute.
--
-- >>> Path.toString (Path.append [abs|/usr|] [rel|bin|] :: Abs PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.append [rel|usr|] [rel|bin|] :: Rel PosixPath)
-- "usr/bin"
--
-- If the second path does not have File or Dir information then the return
-- type too cannot have it.
--
-- >>> Path.toString (Path.append [absdir|/usr|] [rel|bin|] :: Abs PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.append [reldir|usr|] [rel|bin|] :: Rel PosixPath)
-- "usr/bin"
--
-- If the second path has 'File' or 'Dir' information then the return type
-- also has it.
--
-- >>> Path.toString (Path.append [abs|/usr|] [reldir|bin|] :: Abs (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [abs|/usr|] [relfile|bin|] :: Abs (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [rel|usr|] [reldir|bin|] :: Rel (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (Path.append [rel|usr|] [relfile|bin|] :: Rel (File PosixPath))
-- "usr/bin"
--
-- >>> Path.toString (Path.append [absdir|/usr|] [reldir|bin|] :: Abs (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [absdir|/usr|] [relfile|bin|] :: Abs (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [reldir|usr|] [reldir|bin|] :: Rel (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (Path.append [reldir|usr|] [relfile|bin|] :: Rel (File PosixPath))
-- "usr/bin"
--
-- Type error cases:
--
-- >> Path.append [dir|/usr|] [rel|bin|] -- first arg must be Abs/Rel
-- >> Path.append [file|/usr|] [rel|bin|] -- first arg must be Abs/Rel
-- >> Path.append [absfile|/usr|] [rel|bin|] -- first arg must be a dir
-- >> Path.append [abs|/usr|] [abs|/bin|] -- second arg must be rel
-- >> Path.append [abs|/usr|] [dir|bin|] -- second arg must be rel
-- >> Path.append [abs|/usr|] [file|bin|] -- second arg must be rel
--
{-# INLINE append #-}
append ::
    (
      IsAbsRel (a b)
    , HasDir (a b)
    , IsPath PosixPath (a b)
    , IsPath PosixPath c
    , IsPath PosixPath (a c)
    ) => a b -> Rel c -> a c
append a (Rel c) = unsafeFromPath $ Posix.unsafeAppend (toPath a) (toPath c)
