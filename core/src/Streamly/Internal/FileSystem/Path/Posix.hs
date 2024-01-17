{-# LANGUAGE TemplateHaskell #-}
-- For constraints on "combine" and "combineDir"
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Streamly.Internal.FileSystem.Path.Posix
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- = File System Tree
--
-- A filesystem is a tree when there are no hard links or symbolic links. But
-- in the presence of symlinks it could be a DAG or a graph, because directory
-- symlinks can create cycles.
--
-- = Absolute vs Relative Paths
--
-- A path that does not refer to a particular location but defines steps to go
-- from any place to some other place is a relative path. Absolute paths can
-- never be appended to a path whereas relative paths can be appended.
--
-- Absolute: @/a/b/c@
-- Relative @e/f/g@
--
-- An absolute path refers to a particular location, it has a notion of a root
-- which could be implicit or explicit. A root refers to a specific known
-- location.
--
-- = Appending Paths
--
-- We can only append a relative path to any path. But there can be
-- complications when paths have implicit references.
--
-- = Comparing Paths
--
-- Each component of the path is the same then paths are same. But there can be
-- complications when paths have implicit references.
--
-- = Implicit Roots (.)
--
-- On Posix and Windows "." implicitly refers to the current directory. On
-- Windows a path like @/Users/@ has the drive reference implicit. Such
-- references are contextual and may have different meanings at different
-- times.
--
-- @./bin@ may refer to a different location depending on what "." is
-- referring to. Thus we should not allow @./bin@ to be appended to another
-- path, @bin@ can be appended though. Similarly, we cannot compare @./bin@
-- with @./bin@ and say that they are equal because they may be referring to
-- different locations depending on in what context the paths were created.
--
-- The same arguments apply to paths with implicit drive on Windows.
--
-- We can treat @.\/bin\/ls@ as an absolute path with "." as an implicit root.
-- The relative path is "bin/ls" which represents steps from somewhere to
-- somewhere else rather than a particular location. We can also call @./bin@
-- as a "located path" as it points to particular location rather than "steps"
-- from one place to another. If we want to append such paths we need to first
-- make them explicitly relative by dropping the implicit root. Or we can use
-- unsafeAppend to force it anyway or unsafeCast to convert absolute to
-- relative.
--
-- On these absolute (located/Loc) paths if we use takeRoot, it should return
-- RootCurDir, RootCurDrive and @Root Path@ to distinguish @./@, @/@, @C:/@. We
-- could represent them by different types but that would make the types even more
-- complicated. So runtime checks are are a good balance.
--
-- Path comparison should return EqTrue, EqFalse or EqUnknown. If we compare
-- these absolute/located paths having implicit roots then result should be
-- EqUnknown or maybe we can just return False?. @./bin@ and @./bin@ should be
-- treated as paths with different roots/drives but same relative path. The
-- programmer can explicitly drop the root and compare the relative paths if
-- they want to check literal equality.
--
-- Note that a trailing . or a . in the middle of a path is different as it
-- refers to a known name.
--
-- = Ambiguous References (..)
--
-- ".." in a path refers to the parent directory relative to the current path.
-- For an absolute root directory ".." refers to the root itself because you
-- cannot go futher up.
--
-- When resolving ".." it always resolves to the parent of a directory as
-- stored in the dierctory entry. So if we landed in a directory via a symlink,
-- ".." can take us back to a different directory and not to the symlink
-- itself. Thus @a\/b/..@ may not be the same as @a/@. Shells like bash keep
-- track of the old paths explicitly, so you may not see this behavior when
-- using a shell.
--
-- For this reason we cannot process ".." in the path statically. However, if
-- the components of two paths are exactly the same then they will always
-- resolve to the same target. But two paths with different components could
-- also point to the same target. So if there are ".." in the path we cannot
-- definitively say if they are the same without resolving them.
--
module Streamly.Internal.FileSystem.Path.Posix
    (
    -- * Path Types
      PosixPath (..)
    , File
    , Dir
    , Abs
    , Rel
    , IsAbsRel
    , NotAbsRel
    , IsDir

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Construction
    , fromChunk
    , unsafeFromChunk
    , fromChars
    , fromString
    , unsafeFromString

    -- * Statically Verified String Literals
    -- quasiquoters
    , path
    , abs
    , rel
    , dir
    , file
    , absdir
    , reldir
    , absfile
    , relfile

    -- * Statically Verified Strings
    -- XXX Do we need these if we have quasiquoters? These may be useful if we
    -- are generating strings statically using methods other than literals or
    -- if we are doing some text processing on strings before using them.
    -- TH macros
    , mkPath
    , mkAbs
    , mkRel
    , mkDir
    , mkFile
    , mkAbsDir
    , mkRelDir
    , mkAbsFile
    , mkRelFile

    -- * Elimination
    , toChunk
    , toChars
    , toString

    -- * Parsing
    , dropTrailingSeparators

    -- * Operations
    , unsafeAppend
    , append
    , combine
    , combineDir
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.FileSystem.Path.Common (mkQ)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path.Common as Common

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

newtype PosixPath = PosixPath (Array Word8)

-- XXX Swap the order of IsPath arguments?

instance IsPath PosixPath PosixPath where
    unsafeFromPath = id
    fromPath = pure
    toPath = id

instance IsPath PosixPath (File PosixPath) where
    unsafeFromPath = File
    fromPath p = pure (File p)
    toPath (File p) = p

instance IsPath PosixPath (Dir PosixPath) where
    unsafeFromPath = Dir
    fromPath p = pure (Dir p)
    toPath (Dir p) = p

instance IsPath PosixPath (Abs PosixPath) where
    unsafeFromPath = Abs
    fromPath p = pure (Abs p)
    toPath (Abs p) = p

instance IsPath PosixPath (Rel PosixPath) where
    unsafeFromPath = Rel
    fromPath p = pure (Rel p)
    toPath (Rel p) = p

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
-- Path parsing utilities
------------------------------------------------------------------------------

-- | If the path is @//@ the result is @/@. If it is @a//@ then the result is
-- @a@.
{-# INLINE dropTrailingSeparators #-}
dropTrailingSeparators :: PosixPath -> PosixPath
dropTrailingSeparators (PosixPath arr) =
    PosixPath (Common.dropTrailingSeparators Common.Posix arr)

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- A chunk is essentially an untyped Array i.e. Array Word8.  We can either use
-- the term ByteArray for that or just Chunk. The latter is shorter and we have
-- been using it consistently in streamly. We use "bytes" for a stream of
-- bytes.

-- | /Unsafe/: The user is responsible to maintain the invariants mentioned in
-- the definition of the 'Path' type. On Windows, the array passed must be a
-- multiple of 2 bytes as the underlying representation uses 'Word16'.
{-# INLINE unsafeFromChunk #-}
unsafeFromChunk :: Array Word8 -> PosixPath
-- XXX add asserts to check safety
unsafeFromChunk = PosixPath . Common.unsafeFromChunk

-- | It may fail if the byte array contains null characters.
--
-- Throws 'InvalidPath'.
fromChunk :: MonadThrow m => Array Word8 -> m PosixPath
fromChunk = fmap PosixPath . Common.fromChunk

-- XXX Should be a Fold instead?

-- | Encode a Unicode string to 'Path' using strict UTF-8 encoding. It fails if
-- the stream contains null characters or invalid unicode characters.
--
-- Unicode normalization is not done. If normalization is needed the user can
-- normalize it and use the fromChunk API.
fromChars :: MonadThrow m => Stream Identity Char -> m PosixPath
fromChars = fmap PosixPath . Common.posixFromChars

unsafeFromString :: [Char] -> PosixPath
unsafeFromString = PosixPath . Common.unsafePosixFromString

-- | See fromChars.
--
-- >> fromString = fromChars . Stream.fromList
--
fromString :: MonadThrow m => [Char] -> m PosixPath
fromString = fromChars . Stream.fromList

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

absFromString :: MonadThrow m => String -> m (Abs PosixPath)
absFromString s = fromString s >>= adapt

dirFromString :: MonadThrow m => String -> m (Dir PosixPath)
dirFromString s = fromString s >>= adapt

absDirFromString :: MonadThrow m => String -> m (Abs (Dir PosixPath))
absDirFromString s = fromString s >>= adapt

relFromString :: MonadThrow m => String -> m (Rel PosixPath)
relFromString s = fromString s >>= adapt

relDirFromString :: MonadThrow m => String -> m (Rel (Dir PosixPath))
relDirFromString s = fromString s >>= adapt

fileFromString :: MonadThrow m => String -> m (File PosixPath)
fileFromString s = fromString s >>= adapt

absFileFromString :: MonadThrow m => String -> m (Abs (File PosixPath))
absFileFromString s = fromString s >>= adapt

relFileFromString :: MonadThrow m => String -> m (Rel (File PosixPath))
relFileFromString s = fromString s >>= adapt

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftPath :: Quote m => PosixPath -> m Exp
liftPath p =
    [| PosixPath (Common.unsafePosixFromString $(lift $ toString p)) |]

liftRel :: Quote m => Rel PosixPath -> m Exp
liftRel p =
    [| Rel (PosixPath (Common.unsafePosixFromString $(lift $ toString p))) |]

liftAbs :: Quote m => Abs PosixPath -> m Exp
liftAbs p =
    [| Abs (PosixPath (Common.unsafePosixFromString $(lift $ toString p))) |]

liftDir :: Quote m => Dir PosixPath -> m Exp
liftDir p =
    [| Dir (PosixPath (Common.unsafePosixFromString $(lift $ toString p))) |]

liftAbsDir :: Quote m => Abs (Dir PosixPath) -> m Exp
liftAbsDir p =
    [| Abs (Dir (PosixPath (Common.unsafePosixFromString $(lift $ toString p)))) |]

liftRelDir :: Quote m => Rel (Dir PosixPath) -> m Exp
liftRelDir p =
    [| Rel (Dir (PosixPath (Common.unsafePosixFromString $(lift $ toString p)))) |]

liftFile :: Quote m => File PosixPath -> m Exp
liftFile p =
    [| File (PosixPath (Common.unsafePosixFromString $(lift $ toString p))) |]

liftAbsFile :: Quote m => Abs (File PosixPath) -> m Exp
liftAbsFile p =
    [| Abs (File (PosixPath (Common.unsafePosixFromString $(lift $ toString p)))) |]

liftRelFile :: Quote m => Rel (File PosixPath) -> m Exp
liftRelFile p =
    [| Rel (File (PosixPath (Common.unsafePosixFromString $(lift $ toString p)))) |]

-- | Generates a 'Path' type.
--
mkPath :: String -> Q Exp
mkPath = either (error . show) liftPath . fromString

-- | Generates an @Abs Path@ type.
--
mkAbs :: String -> Q Exp
mkAbs = either (error . show) liftAbs . absFromString

-- | Generates an @Rel Path@ type.
--
mkRel :: String -> Q Exp
mkRel = either (error . show) liftRel . relFromString

-- | Generates an @Dir Path@ type.
--
mkDir :: String -> Q Exp
mkDir = either (error . show) liftDir . dirFromString

-- | Generates an @File Path@ type.
--
mkFile :: String -> Q Exp
mkFile = either (error . show) liftFile . fileFromString

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

-- | Generates a 'PosixPath' type from a quoted literal.
--
-- >>> Path.toString ([path|/usr|] :: PosixPath)
-- "/usr/bin"
--
path :: QuasiQuoter
path = mkQ mkPath

-- XXX Change to "loc"?

-- | Generates an @Abs PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([abs|/usr|] :: Abs PosixPath)
-- "/usr"
--
abs :: QuasiQuoter
abs = mkQ mkAbs

-- | Generates a @Rel PosixPath@ type from a quoted literal.
--
-- >>> Path.toString ([rel|usr|] :: Rel PosixPath)
-- "usr"
--
rel :: QuasiQuoter
rel = mkQ mkRel

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

------------------------------------------------------------------------------
-- Eimination
------------------------------------------------------------------------------

-- | Convert the path to an array of bytes.
toChunk :: PosixPath -> Array Word8
toChunk (PosixPath arr) = Common.toChunk arr

-- | Decode the path to a stream of Unicode chars using strict UTF-8 decoding.
toChars :: (Monad m, IsPath PosixPath p) => p -> Stream m Char
toChars p = let (PosixPath arr) = toPath p in Common.posixToChars arr

-- XXX When showing append a "/" to dir types?

-- | Decode the path to a Unicode string using strict UTF-8 decoding.
toString :: IsPath PosixPath a => a -> [Char]
toString = runIdentity . Stream.toList . toChars

------------------------------------------------------------------------------
-- Operations on Path
------------------------------------------------------------------------------

-- XXX This can be generalized to an Array intersperse operation

{-# INLINE unsafeAppend #-}
unsafeAppend :: PosixPath -> PosixPath -> PosixPath
unsafeAppend (PosixPath a) (PosixPath b) =
    PosixPath $ Common.unsafeAppend Common.Posix Common.posixToString a b

-- | Append a 'Path' to another. Fails if the second path is absolute.
--
-- >>> Path.toString $ Path.append [path|/usr|] [path|bin|]
-- "/usr/bin"
--
-- Also see 'combine'.
append :: PosixPath -> PosixPath -> PosixPath
append (PosixPath a) (PosixPath b) =
    PosixPath $ Common.append Common.Posix Common.posixToString a b

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
-- >>> Path.toString (Path.combine [abs|/usr|] [rel|bin|] :: Abs PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.combine [rel|usr|] [rel|bin|] :: Rel PosixPath)
-- "usr/bin"
--
-- If the second path does not have File or Dir information then the return
-- type too cannot have it.
--
-- >>> Path.toString (Path.combine [absdir|/usr|] [rel|bin|] :: Abs PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.combine [reldir|usr|] [rel|bin|] :: Rel PosixPath)
-- "usr/bin"
--
-- If the second path has 'File' or 'Dir' information then the return type
-- also has it.
--
-- >>> Path.toString (Path.combine [abs|/usr|] [reldir|bin|] :: Abs (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.combine [abs|/usr|] [relfile|bin|] :: Abs (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.combine [rel|usr|] [reldir|bin|] :: Rel (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (Path.combine [rel|usr|] [relfile|bin|] :: Rel (File PosixPath))
-- "usr/bin"
--
-- >>> Path.toString (Path.combine [absdir|/usr|] [reldir|bin|] :: Abs (Dir PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.combine [absdir|/usr|] [relfile|bin|] :: Abs (File PosixPath))
-- "/usr/bin"
-- >>> Path.toString (Path.combine [reldir|usr|] [reldir|bin|] :: Rel (Dir PosixPath))
-- "usr/bin"
-- >>> Path.toString (Path.combine [reldir|usr|] [relfile|bin|] :: Rel (File PosixPath))
-- "usr/bin"
--
-- Type error cases:
--
-- >> Path.combine [dir|/usr|] [rel|bin|] -- first arg must be Abs/Rel
-- >> Path.combine [file|/usr|] [rel|bin|] -- first arg must be Abs/Rel
-- >> Path.combine [absfile|/usr|] [rel|bin|] -- first arg must be a dir
-- >> Path.combine [abs|/usr|] [abs|/bin|] -- second arg must be rel
-- >> Path.combine [abs|/usr|] [dir|bin|] -- second arg must be rel
-- >> Path.combine [abs|/usr|] [file|bin|] -- second arg must be rel
--
-- Also see 'combineDir'.
{-# INLINE combine #-}
combine ::
    (
      IsAbsRel (a b)
    , IsDir (a b)
    , IsPath PosixPath (a b)
    , IsPath PosixPath c
    , IsPath PosixPath (a c)
    ) => a b -> Rel c -> a c
combine a (Rel c) = unsafeFromPath $ unsafeAppend (toPath a) (toPath c)

-- | Use this API when you are appending to a 'Dir' path without 'Abs' or 'Rel'
-- annotation.The second argument can only be either 'Dir' or 'File' without
-- 'Abs' or 'Rel.
--
-- >>> Path.toString (Path.combineDir [dir|/usr|] [dir|bin|] :: Dir PosixPath)
-- "/usr/bin"
-- >>> Path.toString (Path.combineDir [dir|/usr|] [file|bin|] :: File PosixPath)
-- "/usr/bin"
--
-- If your second path is @Rel Dir@ or @Rel File@ then you can remove the @Rel@
-- annotation before using this API.
--
-- Type error cases:
--
-- >> Path.combineDir [dir|/usr|] [abs|bin|] -- second arg cannot be abs/rel
-- >> Path.combineDir [dir|/usr|] [rel|bin|] -- second arg cannot be abs/rel
--
{-# INLINE combineDir #-}
combineDir :: (IsPath PosixPath (a b), NotAbsRel (a b)) =>
    Dir PosixPath -> a b -> a b
combineDir (Dir a) b =
    unsafeFromPath $ unsafeAppend (toPath a) (toPath b)
