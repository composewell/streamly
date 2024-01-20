{-# LANGUAGE TemplateHaskell #-}
-- For constraints on "append"
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#if defined(IS_WINDOWS)
#define OS_NAME Windows
#define OS_PATH WindowsPath
#else
#define OS_NAME Posix
#define OS_PATH PosixPath
#endif

-- |
-- Module      : Streamly.Internal.FileSystem.OS_PATH.Typed
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- When @Loc/Seg@ and @File/Dir@ both are present, @Loc/Seg@ must be
-- outermost constructors and @File/Dir@ as inner. Thus the types File (Loc
-- a) or Dir (Loc a) are not allowed but Loc (Dir a) and Loc (File a) are
-- allowed.

module Streamly.Internal.FileSystem.OS_PATH.Typed
    (
    -- * Types
      HasDir

    -- * Construction
    -- ** From String
    , dirLocFromString -- dirLocString?
    , dirSegFromString
    , fileLocFromString
    , fileSegFromString

    -- ** Statically Verified String Literals
    -- | Quasiquoters.
    , dirloc
    , dirseg
    , fileloc
    , fileseg

    -- ** Statically Verified Strings
    -- | Template Haskell expression splices.
    , dirLocExp
    , dirSegExp
    , fileLocExp
    , fileSegExp

    -- * Operations
    , append
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.FileSystem.Path.Common (mkQ)
import Streamly.Internal.FileSystem.OS_PATH (OS_PATH(..))
import Streamly.Internal.FileSystem.OS_PATH.LocSeg (Loc(..), Seg(..))
import Streamly.Internal.FileSystem.OS_PATH.FileDir (File(..), Dir(..))

import qualified Streamly.Internal.FileSystem.OS_PATH as OS_NAME
import qualified Streamly.Internal.FileSystem.OS_PATH.LocSeg as LocSeg

import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath)
>>> import Streamly.Internal.FileSystem.PosixPath.FileDir (Dir, File, dir, file)
>>> import Streamly.Internal.FileSystem.PosixPath.LocSeg (Loc, Seg, loc, seg)
>>> import Streamly.Internal.FileSystem.PosixPath.Typed (dirloc, dirseg, fileloc, fileseg)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
>>> import qualified Streamly.Internal.FileSystem.PosixPath.Typed as PathTyp
-}

-- Note that (Loc a) may also be a directory if "a" is (Dir b), but it can also
-- be a file if "a" is (File b). Therefore, the constraints are put on a more
-- spspecific type e.g. (Loc OS_PATH) may be a dir.

-- | Constraint to check if a type represents a directory.
class HasDir a

instance HasDir (Dir a)
instance HasDir (Loc (Dir a))
instance HasDir (Seg (Dir a))

instance IsPath OS_PATH (Loc (File OS_PATH)) where
    unsafeFromPath p = Loc (File p)
    fromPath p = pure (Loc (File p))
    toPath (Loc (File p)) = p

instance IsPath OS_PATH (Loc (Dir OS_PATH)) where
    unsafeFromPath p = Loc (Dir p)
    fromPath p = pure (Loc (Dir p))
    toPath (Loc (Dir p)) = p

instance IsPath OS_PATH (Seg (File OS_PATH)) where
    unsafeFromPath p = Seg (File p)
    fromPath p = pure (Seg (File p))
    toPath (Seg (File p)) = p

instance IsPath OS_PATH (Seg (Dir OS_PATH)) where
    unsafeFromPath p = Seg (Dir p)
    fromPath p = pure (Seg (Dir p))
    toPath (Seg (Dir p)) = p

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

dirLocFromString :: MonadThrow m => String -> m (Loc (Dir OS_PATH))
dirLocFromString s = LocSeg.locFromString s >>= OS_NAME.adapt

dirSegFromString :: MonadThrow m => String -> m (Seg (Dir OS_PATH))
dirSegFromString s = LocSeg.segFromString s >>= OS_NAME.adapt

fileLocFromString :: MonadThrow m => String -> m (Loc (File OS_PATH))
fileLocFromString s = OS_NAME.fromString s >>= OS_NAME.adapt

fileSegFromString :: MonadThrow m => String -> m (Seg (File OS_PATH))
fileSegFromString s = OS_NAME.fromString s >>= OS_NAME.adapt

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftDirLoc :: Loc (Dir OS_PATH) -> Q Exp
liftDirLoc (Loc (Dir p)) =
    [| Loc (Dir (OS_NAME.unsafeFromString $(lift $ OS_NAME.toString p))) |]

liftDirSeg :: Seg (Dir OS_PATH) -> Q Exp
liftDirSeg (Seg (Dir p)) =
    [| Seg (Dir (OS_NAME.unsafeFromString $(lift $ OS_NAME.toString p))) |]

liftFileLoc :: Loc (File OS_PATH) -> Q Exp
liftFileLoc (Loc (File p)) =
    [| Loc (File (OS_NAME.unsafeFromString $(lift $ OS_NAME.toString p))) |]

liftFileSeg :: Seg (File OS_PATH) -> Q Exp
liftFileSeg (Seg (File p)) =
    [| Seg (File (OS_NAME.unsafeFromString $(lift $ OS_NAME.toString p))) |]

-- | Generates a Haskell expression of type @Loc (Dir OS_PATH)@.
--
dirLocExp :: String -> Q Exp
dirLocExp = either (error . show) liftDirLoc . dirLocFromString

-- | Generates a Haskell expression of type @Seg (Dir OS_PATH)@.
--
dirSegExp :: String -> Q Exp
dirSegExp = either (error . show) liftDirSeg . dirSegFromString

-- | Generates a Haskell expression of type @Loc (File OS_PATH)@.
--
fileLocExp :: String -> Q Exp
fileLocExp = either (error . show) liftFileLoc . fileLocFromString

-- | Generates a Haskell expression of type @Seg (File OS_PATH)@.
--
fileSegExp :: String -> Q Exp
fileSegExp = either (error . show) liftFileSeg . fileSegFromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- XXX Change to "dirloc"?

-- | Generates an @Loc (Dir OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([dirloc|/usr|] :: Loc (Dir PosixPath))
-- "/usr"
--
dirloc :: QuasiQuoter
dirloc = mkQ dirLocExp

-- | Generates a @Seg (Dir OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([dirseg|usr|] :: Seg (Dir PosixPath))
-- "usr"
--
dirseg :: QuasiQuoter
dirseg = mkQ dirSegExp

-- XXX Change to "fileloc"?

-- | Generates an @Loc (File OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([fileloc|/usr|] :: Loc (File PosixPath))
-- "/usr"
--
fileloc :: QuasiQuoter
fileloc = mkQ fileLocExp

-- | Generates an @Seg (File OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([fileseg|usr|] :: Seg (File PosixPath))
-- "usr"
--
fileseg :: QuasiQuoter
fileseg = mkQ fileSegExp

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Seg path.

{-
-- If the first path is 'Loc' then the return type is also 'Loc'.
--
-- If the second path does not have 'File' or 'Dir' information then the return
-- type too cannot have it.
--
-- >> Path.toString (PathTyp.append [dirloc|/usr|] [seg|bin|] :: Loc PosixPath)
-- "/usr/bin"
-- >> Path.toString (PathTyp.append [dirseg|usr|] [seg|bin|] :: Seg PosixPath)
-- "usr/bin"
--
-- >> Path.toString (PathTyp.append [loc|/usr|] [seg|bin|] :: Loc PosixPath)
-- "/usr/bin"
-- >> Path.toString (PathTyp.append [seg|usr|] [seg|bin|] :: Seg PosixPath)
-- "usr/bin"
--
-- If the second path has 'File' or 'Dir' information then the return type
-- also has it.
--
-- >> Path.toString (PathTyp.append [loc|/usr|] [dirseg|bin|] :: Loc (Dir PosixPath))
-- "/usr/bin"
-- >> Path.toString (PathTyp.append [loc|/usr|] [fileseg|bin|] :: Loc (File PosixPath))
-- "/usr/bin"
-- >> Path.toString (PathTyp.append [seg|usr|] [dirseg|bin|] :: Seg (Dir PosixPath))
-- "usr/bin"
-- >> Path.toString (PathTyp.append [seg|usr|] [fileseg|bin|] :: Seg (File PosixPath))
-- "usr/bin"
--
-- Type error cases:
--
-- >> PathTyp.append [dir|/usr|] [seg|bin|] -- first arg must be Loc/Seg
-- >> PathTyp.append [file|/usr|] [seg|bin|] -- first arg must be Loc/Seg
-- >> PathTyp.append [fileloc|/usr|] [seg|bin|] -- first arg must be a dir
-- >> PathTyp.append [loc|/usr|] [loc|/bin|] -- second arg must be seg
-- >> PathTyp.append [loc|/usr|] [dir|bin|] -- second arg must be seg
-- >> PathTyp.append [loc|/usr|] [file|bin|] -- second arg must be seg
--
{-# INLINE append #-}
append ::
    (
      IsLocSeg (a b)
    , HasDir (a b)
    , IsPath OS_PATH (a b)
    , IsPath OS_PATH c
    , IsPath OS_PATH (a c)
    ) => a b -> Seg c -> a c
append a (Seg c) = unsafeFromPath $ OS_NAME.unsafeAppend (toPath a) (toPath c)
-}

-- | Append a path segment to a directory.
--
-- >>> Path.toString (PathTyp.append [dirloc|/usr|] [dirseg|bin|] :: Loc (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (PathTyp.append [dirloc|/usr|] [fileseg|bin|] :: Loc (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (PathTyp.append [dirseg|usr|] [dirseg|bin|] :: Seg (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (PathTyp.append [dirseg|usr|] [fileseg|bin|] :: Seg (File PosixPath))
-- "usr/bin"
--
{-# INLINE append #-}
append ::
    (
      IsPath OS_PATH (a (Dir OS_PATH))
    , IsPath OS_PATH (b OS_PATH)
    , IsPath OS_PATH (a (b OS_PATH))
    ) => a (Dir PosixPath) -> Seg (b PosixPath) -> a (b PosixPath)
append a (Seg c) = unsafeFromPath $ OS_NAME.unsafeAppend (toPath a) (toPath c)
