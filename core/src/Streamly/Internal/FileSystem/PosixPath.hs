{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.FileSystem.PosixPath
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
-- = Location and Segments
--
-- We make two distinctions for paths, a path could refer to a location or it
-- could refer to a segment or segments.
--
-- A path that refers to a particular object in the file system  is called a
-- location e.g. /usr is a location, . is a location, ./bin is a location. A
-- location could be absolute e.g. /usr or it could be relative e.g. ./bin . A
-- location always has two components, a specific "root" which could be
-- explicit or implicit, and a path segment relative to the root. A location
-- with a fixed root is known as an absolute location whereas a location with
-- an implicit root e.g. "./bin" is known as a relative location.
--
-- A path that does not refer to a particular location but defines steps to go
-- from some place to another is a path segment. For example, "local/bin" is a
-- path segment whereas "./local/bin" is a location.
--
-- Locations can never be appended to another location or to a path segment
-- whereas a segment can be appended.
--
-- = Comparing Paths
--
-- We can compare two absolute locations or path segments but we cannot compare
-- two relative locations. If each component of the path is the same then the
-- paths are considered to be equal.
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
-- = Exception Handling
--
-- Path creation routines use MonadThrow which can be interpreted as an Either
-- type. It is rare to actually handle exceptions in path creation functions,
-- we would rather fix the issue, so partial functions should also be fine. But
-- there may be some cases where we are parsing paths from external inputs,
-- reading from a file etc where we may want to handle exceptions. We can
-- always create partial wrappers from these if that is convenient to use.
--
module Streamly.Internal.FileSystem.PosixPath
    (
    -- * Path Types
      PosixPath (..)

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

    -- * Statically Verified Strings
    -- XXX Do we need these if we have quasiquoters? These may be useful if we
    -- are generating strings statically using methods other than literals or
    -- if we are doing some text processing on strings before using them.
    -- TH macros
    , mkPath

    -- * Elimination
    , toChunk
    , toChars
    , toString

    -- * Parsing
    , dropTrailingSeparators

    -- * Operations
    , unsafeAppend
    , append
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

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency. If the argument path is already verfied for a property, we
-- should not verify it again e.g. if we adapt (Loc path) as (Loc (Dir path))
-- then we should not verify it to be Loc again.

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one. This can be used to upgrade or downgrade type safety.
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

-- | /Unsafe/: The user is responsible to make sure that the cases mentioned in
-- 'fromChars' are satisfied.
{-# INLINE unsafeFromChunk #-}
unsafeFromChunk :: Array Word8 -> PosixPath
-- XXX add asserts to check safety
unsafeFromChunk = PosixPath . Common.unsafeFromChunk

-- | See 'fromChars' for failure cases.
--
fromChunk :: MonadThrow m => Array Word8 -> m PosixPath
fromChunk = fmap PosixPath . Common.fromChunk

-- XXX Should be a Fold instead?

-- | Encode a Unicode string to 'Path' using strict UTF-8 encoding. Fails with
-- 'InvalidPath' exception if:
--
-- * the stream contains null characters
-- * the stream contains invalid unicode characters
-- * the stream is empty, should have at least one char
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
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?

liftPath :: Quote m => PosixPath -> m Exp
liftPath p =
    [| PosixPath (Common.unsafePosixFromString $(lift $ toString p)) |]

-- | Generates a 'Path' type.
--
mkPath :: String -> Q Exp
mkPath = either (error . show) liftPath . fromString

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

------------------------------------------------------------------------------
-- Eimination
------------------------------------------------------------------------------

-- | Convert the path to an array of bytes.
toChunk :: PosixPath -> Array Word8
toChunk (PosixPath arr) = Common.toChunk arr

-- | Decode the path to a stream of Unicode chars using strict UTF-8 decoding.
toChars :: (Monad m, IsPath PosixPath p) => p -> Stream m Char
toChars p = let (PosixPath arr) = toPath p in Common.posixToChars arr

-- XXX When showing, append a "/" to dir types?

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

-- | Append a 'Path' to another. Fails if the second path refers to a location
-- and not a path segment.
--
-- >>> Path.toString $ Path.append [path|/usr|] [path|bin|]
-- "/usr/bin"
--
append :: PosixPath -> PosixPath -> PosixPath
append (PosixPath a) (PosixPath b) =
    PosixPath $ Common.append Common.Posix Common.posixToString a b
