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
-- When @Loc/Seg@ and @File/Dir@ both are present, @Loc/Seg@ must be
-- outermost constructors and @File/Dir@ as inner. Thus the types File (Loc
-- a) or Dir (Loc a) are not allowed but Loc (Dir a) and Loc (File a) are
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
    , dirloc
    , dirseg
    , fileloc
    , fileseg

    -- * Statically Verified Strings
    -- XXX Do we need these if we have quasiquoters? These may be useful if we
    -- are generating strings statically using methods other than literals or
    -- if we are doing some text processing on strings before using them.
    -- TH macros
    , mkDirLoc
    , mkDirSeg
    , mkFileLoc
    , mkFileSeg

    -- * Operations
    , append
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.FileSystem.Path.Common (mkQ)
import Streamly.Internal.FileSystem.Path.Posix (PosixPath(..))
import Streamly.Internal.FileSystem.Path.PosixAbsRel (Loc(..), Seg(..), IsLocSeg)
import Streamly.Internal.FileSystem.Path.PosixFileDir (File(..), Dir(..))

import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.FileSystem.Path.Posix as Posix
import qualified Streamly.Internal.FileSystem.Path.PosixAbsRel as AbsRel

import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path
import Prelude hiding (abs)

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.FileSystem.Path.Posix as Path
-}

-- Note that (Loc a) may also be a directory if "a" is (Dir b), but it can also
-- be a file if "a" is (File b). Therefore, the constraints are put on a more
-- spspecific type e.g. (Loc PosixPath) may be a dir.

-- | Constraint to check if a type represents a directory.
class HasDir a

instance HasDir (Dir a)
instance HasDir (Loc (Dir a))
instance HasDir (Seg (Dir a))

instance IsPath PosixPath (Loc (File PosixPath)) where
    unsafeFromPath p = Loc (File p)
    fromPath p = pure (Loc (File p))
    toPath (Loc (File p)) = p

instance IsPath PosixPath (Loc (Dir PosixPath)) where
    unsafeFromPath p = Loc (Dir p)
    fromPath p = pure (Loc (Dir p))
    toPath (Loc (Dir p)) = p

instance IsPath PosixPath (Seg (File PosixPath)) where
    unsafeFromPath p = Seg (File p)
    fromPath p = pure (Seg (File p))
    toPath (Seg (File p)) = p

instance IsPath PosixPath (Seg (Dir PosixPath)) where
    unsafeFromPath p = Seg (Dir p)
    fromPath p = pure (Seg (Dir p))
    toPath (Seg (Dir p)) = p

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency. If the argument path is already verfied for a property, we
-- should not verify it again e.g. if we adapt (Loc path) as (Loc (Dir path))
-- then we should not verify it to be Loc again.

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one.
--
-- You can only upgrade or downgrade type safety. Converting Loc to Seg or File
-- to Dir will definitely fail.
adapt :: (MonadThrow m, IsPath PosixPath a, IsPath PosixPath b) => a -> m b
adapt p = fromPath (toPath p :: PosixPath)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

dirLocFromString :: MonadThrow m => String -> m (Loc (Dir PosixPath))
dirLocFromString s = AbsRel.locFromString s >>= adapt

dirSegFromString :: MonadThrow m => String -> m (Seg (Dir PosixPath))
dirSegFromString s = AbsRel.segFromString s >>= adapt

fileLocFromString :: MonadThrow m => String -> m (Loc (File PosixPath))
fileLocFromString s = Posix.fromString s >>= adapt

fileSegFromString :: MonadThrow m => String -> m (Seg (File PosixPath))
fileSegFromString s = Posix.fromString s >>= adapt

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftDirLoc :: Quote m => Loc (Dir PosixPath) -> m Exp
liftDirLoc p =
    [| Loc (Dir (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

liftDirSeg :: Quote m => Seg (Dir PosixPath) -> m Exp
liftDirSeg p =
    [| Seg (Dir (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

liftFileLoc :: Quote m => Loc (File PosixPath) -> m Exp
liftFileLoc p =
    [| Loc (File (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

liftFileSeg :: Quote m => Seg (File PosixPath) -> m Exp
liftFileSeg p =
    [| Seg (File (PosixPath (Common.unsafePosixFromString $(lift $ Posix.toString p)))) |]

-- | Generates an @Loc (Dir Path)@ type.
--
mkDirLoc :: String -> Q Exp
mkDirLoc = either (error . show) liftDirLoc . dirLocFromString

-- | Generates an @Seg (Dir Path)@ type.
--
mkDirSeg :: String -> Q Exp
mkDirSeg = either (error . show) liftDirSeg . dirSegFromString

-- | Generates an @Loc (File Path)@ type.
--
mkFileLoc :: String -> Q Exp
mkFileLoc = either (error . show) liftFileLoc . fileLocFromString

-- | Generates an @Seg (File Path)@ type.
--
mkFileSeg :: String -> Q Exp
mkFileSeg = either (error . show) liftFileSeg . fileSegFromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- XXX Change to "dirloc"?

-- | Generates an @Loc (Dir PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([dirloc|/usr|] :: Loc (Dir PosixPath))
-- "/usr"
--
dirloc :: QuasiQuoter
dirloc = mkQ mkDirLoc

-- | Generates a @Seg (Dir PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([dirseg|usr|] :: Seg (Dir PosixPath))
-- "usr"
--
dirseg :: QuasiQuoter
dirseg = mkQ mkDirSeg

-- XXX Change to "fileloc"?

-- | Generates an @Loc (File PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([fileloc|/usr|] :: Loc (File PosixPath))
-- "/usr"
--
fileloc :: QuasiQuoter
fileloc = mkQ mkFileLoc

-- | Generates an @Seg (File PosixPath)@ type from a quoted literal.
--
-- >>> Path.toString ([fileseg|usr|] :: Seg (File PosixPath))
-- "usr"
--
fileseg :: QuasiQuoter
fileseg = mkQ mkFileSeg

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Seg path.

-- If you are not using File/Dir annotations then this is the only API you need
-- to combine paths.

-- | Use this API to combine paths when the first path is @Loc@ or @Seg@.
-- Second path must be @Seg@, if the second path is just @Dir@ or @File@ you
-- can wrap it in @Seg@ first.
--
-- If the first path is absolute then the return type is also absolute.
--
-- >>> Path.toString (Path.append [abs|/usr|] [rel|bin|] :: Loc PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.append [rel|usr|] [rel|bin|] :: Seg PosixPath)
-- "usr/bin"
--
-- If the second path does not have File or Dir information then the return
-- type too cannot have it.
--
-- >>> Path.toString (Path.append [dirloc|/usr|] [rel|bin|] :: Loc PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.append [dirseg|usr|] [rel|bin|] :: Seg PosixPath)
-- "usr/bin"
--
-- If the second path has 'File' or 'Dir' information then the return type
-- also has it.
--
-- >>> Path.toString (Path.append [abs|/usr|] [dirseg|bin|] :: Loc (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [abs|/usr|] [fileseg|bin|] :: Loc (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [rel|usr|] [dirseg|bin|] :: Seg (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (Path.append [rel|usr|] [fileseg|bin|] :: Seg (File PosixPath))
-- "usr/bin"
--
-- >>> Path.toString (Path.append [dirloc|/usr|] [dirseg|bin|] :: Loc (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [dirloc|/usr|] [fileseg|bin|] :: Loc (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.append [dirseg|usr|] [dirseg|bin|] :: Seg (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (Path.append [dirseg|usr|] [fileseg|bin|] :: Seg (File PosixPath))
-- "usr/bin"
--
-- Type error cases:
--
-- >> Path.append [dir|/usr|] [rel|bin|] -- first arg must be Loc/Seg
-- >> Path.append [file|/usr|] [rel|bin|] -- first arg must be Loc/Seg
-- >> Path.append [fileloc|/usr|] [rel|bin|] -- first arg must be a dir
-- >> Path.append [abs|/usr|] [abs|/bin|] -- second arg must be rel
-- >> Path.append [abs|/usr|] [dir|bin|] -- second arg must be rel
-- >> Path.append [abs|/usr|] [file|bin|] -- second arg must be rel
--
{-# INLINE append #-}
append ::
    (
      IsLocSeg (a b)
    , HasDir (a b)
    , IsPath PosixPath (a b)
    , IsPath PosixPath c
    , IsPath PosixPath (a c)
    ) => a b -> Seg c -> a c
append a (Seg c) = unsafeFromPath $ Posix.unsafeAppend (toPath a) (toPath c)
