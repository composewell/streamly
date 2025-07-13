{-# LANGUAGE TemplateHaskell #-}
-- For constraints on "extend"
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
-- Module      : Streamly.Internal.FileSystem.OS_PATH.SegNode
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- When @Rooted/Unrooted@ and @File/Dir@ both are present, @Rooted/Unrooted@ must be
-- outermost constructors and @File/Dir@ as inner. Thus the types File (Rooted
-- a) or Dir (Rooted a) are not allowed but Rooted (Dir a) and Rooted (File a) are
-- allowed.

module Streamly.Internal.FileSystem.OS_PATH.SegNode
    (
    -- * Statically Verified Path Literals
    -- | Quasiquoters.
      rtdir
    , urdir
    , rtfile
    , urfile

    -- * Statically Verified Path Strings
    -- | Template Haskell expression splices.
    , rtdirE
    , urdirE
    , rtfileE
    , urfileE

    -- * Operations
    , extend
    )
where

import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.FileSystem.Path.Common (mkQ)
import Streamly.Internal.FileSystem.OS_PATH (OS_PATH(..))
import Streamly.Internal.FileSystem.OS_PATH.Seg (Rooted(..), Unrooted(..))
import Streamly.Internal.FileSystem.OS_PATH.Node (File(..), Dir(..))

import qualified Streamly.Internal.FileSystem.OS_PATH as OsPath

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

{- $setup
>>> :m
>>> :set -XQuasiQuotes

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath)
>>> import Streamly.Internal.FileSystem.PosixPath.Node (Dir, File, dir, file)
>>> import Streamly.Internal.FileSystem.PosixPath.Seg (Rooted, Unrooted, rt, ur)
>>> import Streamly.Internal.FileSystem.PosixPath.SegNode (rtdir, urdir, rtfile, urfile)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
>>> import qualified Streamly.Internal.FileSystem.PosixPath.SegNode as SegNode
-}

-- Note that (Rooted a) may also be a directory if "a" is (Dir b), but it can also
-- be a file if "a" is (File b). Therefore, the constraints are put on a more
-- specific type e.g. (Rooted OS_PATH) may be a dir.

{-
-- | Constraint to check if a type represents a directory.
class HasDir a

instance HasDir (Dir a)
instance HasDir (Rooted (Dir a))
instance HasDir (Unrooted (Dir a))
-}

-- Design notes:
--
-- There are two ways in which we can lift or upgrade a lower level path to a
-- higher level one. Lift each type directly from the base path e.g. Rooted (Dir
-- PosixPath) can be created directly from PosixPath. This allows us to do dir
-- checks and loc checks at the same time in a monolithic manner. But this also
-- makes us do the Dir checks again if we are lifting from Dir to Rooted. This
-- leads to less complicated constraints, more convenient type conversions.
--
-- Another alternative is to lift one segment at a time, so we lift PosixPath
-- to Dir and then Dir to Rooted. This way the checks are serialized, we perform
-- the dir checks first and then Rooted checks, we cannot combine them together.
-- The advantage is that when lifting from Dir to Rooted we do not need to do the
-- Dir checks. The disadvantage is less convenient conversion because of
-- stronger typing, we will need two steps - fromPath . fromPath and toPath .
-- toPath to upgrade or downgrade instead of just adapt.
--
{-
instance IsPath (File OS_PATH) (Rooted (File OS_PATH)) where
    unsafeFromPath = Rooted
    fromPath (File p) = do
        _ :: Rooted OS_PATH <- fromPath p
        pure $ Rooted (File p)
    toPath (Rooted p) = p

instance IsPath (Rooted OS_PATH) (Rooted (File OS_PATH)) where
    unsafeFromPath = Rooted
    fromPath (File p) = do
        _ :: File OS_PATH <- fromPath p
        pure $ Rooted (File p)
    toPath (Rooted p) = p
-}

-- Assuming that lifting from Dir/File to Rooted/Unrooted is not common and even if it
-- is then the combined cost of doing Dir/Rooted checks would be almost the same
-- as individual checks, we take the first approach.

instance IsPath OS_PATH (Rooted (File OS_PATH)) where
    unsafeFromPath p = Rooted (File p)
    fromPath p = do
        _ :: File OS_PATH <- fromPath p
        _ :: Rooted OS_PATH <- fromPath p
        pure $ Rooted (File p)
    toPath (Rooted (File p)) = p

instance IsPath OS_PATH (Rooted (Dir OS_PATH)) where
    unsafeFromPath p = Rooted (Dir p)
    fromPath p = do
        _ :: Dir OS_PATH <- fromPath p
        _ :: Rooted OS_PATH <- fromPath p
        pure $ Rooted (Dir p)
    toPath (Rooted (Dir p)) = p

instance IsPath OS_PATH (Unrooted (File OS_PATH)) where
    unsafeFromPath p = Unrooted (File p)
    fromPath p = do
        _ :: File OS_PATH <- fromPath p
        _ :: Unrooted OS_PATH <- fromPath p
        pure $ Unrooted (File p)
    toPath (Unrooted (File p)) = p

instance IsPath OS_PATH (Unrooted (Dir OS_PATH)) where
    unsafeFromPath p = Unrooted (Dir p)
    fromPath p = do
        _ :: Dir OS_PATH <- fromPath p
        _ :: Unrooted OS_PATH <- fromPath p
        pure $ Unrooted (Dir p)
    toPath (Unrooted (Dir p)) = p

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftRootedDir :: Rooted (Dir OS_PATH) -> Q Exp
liftRootedDir (Rooted (Dir p)) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Rooted (Dir OS_PATH)|]

liftUnrootedDir :: Unrooted (Dir OS_PATH) -> Q Exp
liftUnrootedDir (Unrooted (Dir p)) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Unrooted (Dir OS_PATH) |]

liftRootedFile :: Rooted (File OS_PATH) -> Q Exp
liftRootedFile (Rooted (File p)) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Rooted (File OS_PATH)|]

liftUnrootedFile :: Unrooted (File OS_PATH) -> Q Exp
liftUnrootedFile (Unrooted (File p)) =
    [| OsPath.unsafeFromString $(lift $ OsPath.toString p) :: Unrooted (File OS_PATH)|]

-- | Generates a Haskell expression of type @Rooted (Dir OS_PATH)@.
--
rtdirE :: String -> Q Exp
rtdirE = either (error . show) liftRootedDir . OsPath.fromString

-- | Generates a Haskell expression of type @Unrooted (Dir OS_PATH)@.
--
urdirE :: String -> Q Exp
urdirE = either (error . show) liftUnrootedDir . OsPath.fromString

-- | Generates a Haskell expression of type @Rooted (File OS_PATH)@.
--
rtfileE :: String -> Q Exp
rtfileE = either (error . show) liftRootedFile . OsPath.fromString

-- | Generates a Haskell expression of type @Unrooted (File OS_PATH)@.
--
urfileE :: String -> Q Exp
urfileE = either (error . show) liftUnrootedFile . OsPath.fromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- | Generates a @Rooted (Dir OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([rtdir|/usr|] :: Rooted (Dir PosixPath))
-- "/usr"
--
rtdir :: QuasiQuoter
rtdir = mkQ rtdirE

-- | Generates a @Unrooted (Dir OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([urdir|usr|] :: Unrooted (Dir PosixPath))
-- "usr"
--
urdir :: QuasiQuoter
urdir = mkQ urdirE

-- | Generates a @Rooted (File OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([rtfile|/x.txt|] :: Rooted (File PosixPath))
-- "/x.txt"
--
rtfile :: QuasiQuoter
rtfile = mkQ rtfileE

-- | Generates a @Unrooted (File OS_PATH)@ type from a quoted literal.
--
-- >>> Path.toString ([urfile|x.txt|] :: Unrooted (File PosixPath))
-- "x.txt"
--
urfile :: QuasiQuoter
urfile = mkQ urfileE

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Unrooted path.

{-
-- If the first path is 'Rooted' then the return type is also 'Rooted'.
--
-- If the second path does not have 'File' or 'Dir' information then the return
-- type too cannot have it.
--
-- >> Path.toString (SegNode.extend [rtdir|/usr|] [br|bin|] :: Rooted PosixPath)
-- "/usr/bin"
-- >> Path.toString (SegNode.extend [urdir|usr|] [br|bin|] :: Unrooted PosixPath)
-- "usr/bin"
--
-- >> Path.toString (SegNode.extend [rt|/usr|] [br|bin|] :: Rooted PosixPath)
-- "/usr/bin"
-- >> Path.toString (SegNode.extend [br|usr|] [br|bin|] :: Unrooted PosixPath)
-- "usr/bin"
--
-- If the second path has 'File' or 'Dir' information then the return type
-- also has it.
--
-- >> Path.toString (SegNode.extend [rt|/usr|] [urdir|bin|] :: Rooted (Dir PosixPath))
-- "/usr/bin"
-- >> Path.toString (SegNode.extend [rt|/usr|] [urfile|bin|] :: Rooted (File PosixPath))
-- "/usr/bin"
-- >> Path.toString (SegNode.extend [br|usr|] [urdir|bin|] :: Unrooted (Dir PosixPath))
-- "usr/bin"
-- >> Path.toString (SegNode.extend [br|usr|] [urfile|bin|] :: Unrooted (File PosixPath))
-- "usr/bin"
--
-- Type error cases:
--
-- >> SegNode.extend [dir|/usr|] [br|bin|] -- first arg must be Rooted/Unrooted
-- >> SegNode.extend [file|/usr|] [br|bin|] -- first arg must be Rooted/Unrooted
-- >> SegNode.extend [rtfile|/usr|] [br|bin|] -- first arg must be a dir
-- >> SegNode.extend [rt|/usr|] [rt|/bin|] -- second arg must be seg
-- >> SegNode.extend [rt|/usr|] [dir|bin|] -- second arg must be seg
-- >> SegNode.extend [rt|/usr|] [file|bin|] -- second arg must be seg
--
{-# INLINE extend #-}
extend ::
    (
      IsSeg (a b)
    , HasDir (a b)
    , IsPath OS_PATH (a b)
    , IsPath OS_PATH c
    , IsPath OS_PATH (a c)
    ) => a b -> Unrooted c -> a c
extend a (Unrooted c) = unsafeFromPath $ OS_NAME.unsafeExtend (toPath a) (toPath c)
-}

-- | Append a branch type path to a directory.
--
-- >>> Path.toString (SegNode.extend [rtdir|/usr|] [urdir|bin|] :: Rooted (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (SegNode.extend [rtdir|/usr|] [urfile|bin|] :: Rooted (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (SegNode.extend [urdir|usr|] [urdir|bin|] :: Unrooted (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (SegNode.extend [urdir|usr|] [urfile|bin|] :: Unrooted (File PosixPath))
-- "usr/bin"
--
{-# INLINE extend #-}
extend ::
    (
      IsPath OS_PATH (a (Dir OS_PATH))
    , IsPath OS_PATH (b OS_PATH)
    , IsPath OS_PATH (a (b OS_PATH))
    ) => a (Dir OS_PATH) -> Unrooted (b OS_PATH) -> a (b OS_PATH)
extend p1 (Unrooted p2) =
    unsafeFromPath $ OsPath.unsafeExtend (toPath p1) (toPath p2)
