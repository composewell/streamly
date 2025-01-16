{-# LANGUAGE TemplateHaskell #-}

-- Anything other than windows (Linux/macOS/FreeBSD) is Posix
#if defined(IS_WINDOWS)
#define IS_WINDOWS
#define OS_NAME Windows
#define OS_PATH WindowsPath
#define WORD_TYPE Word16
#define UNICODE_ENCODER encodeUtf16le'
#define UNICODE_DECODER decodeUtf16le'
#define CODEC_NAME UTF-16LE
#define SEPARATORS @/, \\@
#else
#define OS_NAME Posix
#define OS_PATH PosixPath
#define WORD_TYPE Word8
#define UNICODE_ENCODER encodeUtf8'
#define UNICODE_DECODER decodeUtf8'
#define CODEC_NAME UTF-8
#define SEPARATORS @/@
#endif

-- |
-- Module      : Streamly.Internal.FileSystem.OS_PATH
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- This module implements a OS_PATH type representing a file system path for
-- OS_NAME operating systems. The only assumption about the encoding of the
-- path is that it maps the characters SEPARATORS and @.@ to WORD_TYPE
-- representing their ASCII values. Operations are provided to encode and
-- decode using CODEC_NAME encoding.

module Streamly.Internal.FileSystem.OS_PATH
    (
    -- * Type
      OS_PATH (..)

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Construction
    , isValid
    , fromChunk
    , unsafeFromChunk
    , fromChars
    , fromString
    , unsafeFromString

    -- * Statically Verified String Literals
    -- | Quasiquoters.
    , path

    -- * Statically Verified Strings
    -- | Template Haskell expression splices.

    -- Note: We expose these eventhough we have quasiquoters as these TH helpers
    -- are more powerful. They are useful if we are generating strings
    -- statically using methods other than literals or if we are doing some text
    -- processing on strings before using them.
    , pathE

    -- * Elimination
    , toChunk
    , toChars
    , toString

    -- * Operations
    , dropTrailingSeparators
    , isRooted
    , isBranch

    -- * Combinators
    -- Do we need to export the separator functions? They are not essential if
    -- operations to split and combine paths are provided. If someone wants to
    -- work on paths at low level then they know what they are.
    -- , isPrimarySeparator
    -- , isSeparator
    , unsafeAppend
    , append
    , splitRoot
    , splitPath
    , splitPath_
    , splitFile
    , splitExtension
    , eqPath
    , eqPathStrict
    , eqPathBytes
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
#if defined(IS_WINDOWS)
import Data.Word (Word16)
#endif
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.FileSystem.Path.Common (mkQ)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

-- XXX docspec does not process CPP

{- $setup
>>> :m
>>> :set -XQuasiQuotes
>>> import qualified Streamly.Data.Stream as Stream

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath, path)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
-}

-- Path must not contain null char as system calls treat the path as a null
-- terminated C string. Also, they return null terminated strings as paths.
-- XXX Maintain the Array with null termination? To avoid copying the path for
-- null termination when passing to system calls. Path appends will have to
-- handle the null termination.
--
-- XXX On Windows several characters other than null are not allowed but we do
-- not validate that yet when parsing a path.
-- Path components may have limits.
-- Total path length may have a limit.

-- | A type representing file system paths on OS_NAME.
--
-- A OS_PATH is validated before construction unless unsafe constructors are used
-- to create it. Rules and invariants maintained by the safe construction
-- methods are as follows:
--
-- * Does not contain a null character.
-- * Does not have a trailing separator except in the root path.
-- * Does not have a trailing @.@ component.
-- * Does not have consecutive separators except in UNC prefixes on Windows.
-- * Does not contain @\/.\/@ path components except in a UNC prefix on
--   Windows.
--
-- Note that in some cases the file system may perform unicode normalization on
-- paths (e.g. Apple HFS), it may cause surprising results as the path used by
-- the user may not have the same bytes as later returned by the file system.
newtype OS_PATH = OS_PATH (Array WORD_TYPE)

-- Show instance prints raw bytes without any decoding for rountdtripping. We
-- can use a Lax Utf8 decoding and print it as a string for convenience? Should
-- we print raw path as a string instead, may be useful for ascii chars but
-- utf8 encoded chars may be unprintable.  Better use toString if you want to
-- pretty print the path.
{-
instance Show OS_PATH where
    show (OS_PATH x) = show x
-}

-- XXX The Eq instance needs to make sure that the paths are equivalent. If we
-- normalize the paths we can do a byte comparison. However, on windows paths
-- are case insensitive but the case is preserved, therefore, we cannot
-- normalize and need to do case insensitive comparison.

instance IsPath OS_PATH OS_PATH where
    unsafeFromPath = id
    fromPath = pure
    toPath = id

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency. If the argument path is already verfied for a property, we
-- should not verify it again e.g. if we adapt (Loc path) as (Loc (Dir path))
-- then we should not verify it to be Loc again.

-- XXX castPath?

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one. This can be used to upgrade or downgrade type safety.
adapt :: (MonadThrow m, IsPath OS_PATH a, IsPath OS_PATH b) => a -> m b
adapt p = fromPath (toPath p :: OS_PATH)

------------------------------------------------------------------------------
-- Path parsing utilities
------------------------------------------------------------------------------

-- XXX rather have a dropEmptySegments?

-- | If the path is @//@ the result is @/@. If it is @a//@ then the result is
-- @a@.
{-# INLINE dropTrailingSeparators #-}
dropTrailingSeparators :: OS_PATH -> OS_PATH
dropTrailingSeparators (OS_PATH arr) =
    OS_PATH (Common.dropTrailingSeparators Common.OS_NAME arr)

isValid :: OS_PATH -> Bool
isValid (OS_PATH a) = Common.isValid Common.OS_NAME a

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- A chunk is essentially an untyped Array i.e. Array Word8.  We can either use
-- the term ByteArray for that or just Chunk. The latter is shorter and we have
-- been using it consistently in streamly. We use "bytes" for a stream of
-- bytes.

-- | /Unsafe/: The user is responsible to make sure that the cases mentioned in
-- OS_PATH are satisfied.
{-# INLINE unsafeFromChunk #-}
unsafeFromChunk :: IsPath OS_PATH a => Array Word8 -> a
unsafeFromChunk = unsafeFromPath . OS_PATH . Common.unsafeFromChunk

-- XXX mkPath?

-- | See 'fromChars' for failure cases.
--
fromChunk :: (MonadThrow m, IsPath OS_PATH a) => Array Word8 -> m a
fromChunk arr = Common.fromChunk arr >>= fromPath . OS_PATH

-- XXX Should be a Fold instead?

-- | Encode a Unicode string to OS_PATH using strict CODEC_NAME encoding. Fails with
-- 'InvalidPath' exception if:
--
-- * the stream is empty, should have at least one char
-- * the stream contains null characters
-- * the stream contains invalid unicode characters
#if defined(IS_WINDOWS)
-- * the stream contains characters not allowed in paths
#endif
--
-- Unicode normalization is not done. If normalization is needed the user can
-- normalize it and use the fromChunk API.
fromChars :: (MonadThrow m, IsPath OS_PATH a) => Stream Identity Char -> m a
fromChars s =
    Common.fromChars (== '\0') Unicode.UNICODE_ENCODER s
        >>= fromPath . OS_PATH

unsafeFromString :: IsPath OS_PATH a => [Char] -> a
unsafeFromString =
      unsafeFromPath
    . OS_PATH
    . Common.unsafeFromChars (== '\0') Unicode.UNICODE_ENCODER
    . Stream.fromList

-- | See fromChars.
--
-- >>> fromString = Path.fromChars . Stream.fromList
--
fromString :: (MonadThrow m, IsPath OS_PATH a) => [Char] -> m a
fromString = fromChars . Stream.fromList

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?
--
-- XXX Make this polymorphic and reusable in other modules.

liftPath :: OS_PATH -> Q Exp
liftPath p =
    [| unsafeFromString $(lift $ toString p) :: OS_PATH |]

-- | Generates a Haskell expression of type OS_PATH from a String.
--
pathE :: String -> Q Exp
pathE = either (error . show) liftPath . fromString

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Define folds or parsers to parse the paths.
-- XXX Build these on top of the str quasiquoter so that we get interpolation
-- for free. Interpolated vars if any have to be of appropriate type depending
-- on the context so that we can splice them safely.

-- | Generates a OS_PATH type from a quoted literal.
--
-- >>> Path.toString ([path|/usr/bin|] :: PosixPath)
-- "/usr/bin"
--
path :: QuasiQuoter
path = mkQ pathE

------------------------------------------------------------------------------
-- Eimination
------------------------------------------------------------------------------

-- XXX unPath?

-- | Convert the path to an array of bytes.
toChunk :: IsPath OS_PATH a => a -> Array Word8
toChunk p = let OS_PATH arr = toPath p in Common.toChunk arr

-- | Decode the path to a stream of Unicode chars using strict CODEC_NAME decoding.
toChars :: (Monad m, IsPath OS_PATH a) => a -> Stream m Char
toChars p =
    let (OS_PATH arr) =
            toPath p in Common.toChars Unicode.UNICODE_DECODER arr

-- XXX When showing, append a "/" to dir types?

-- | Decode the path to a Unicode string using strict CODEC_NAME decoding.
toString :: IsPath OS_PATH a => a -> [Char]
toString = runIdentity . Stream.toList . toChars

------------------------------------------------------------------------------
-- Operations on Path
------------------------------------------------------------------------------

#ifdef IS_WINDOWS
-- | A path that is attached to a root. "C:\" is considered an absolute root
-- and "." as a dynamic root. ".." is not considered a root, "../x" or "x/y"
-- are not rooted paths.
--
-- Absolute locations:
--
-- * @C:\\@ local drive
-- * @\\\\server\\@ UNC server
-- * @\\\\?\\C:\\@ Long UNC local drive
-- * @\\\\?\\UNC\\@ Long UNC remote server
-- * @\\\\.\\@ DOS local device namespace
-- * @\\\\??\\@ DOS global namespace
--
-- Relative locations:
--
-- * @\\@ relative to current drive root
-- * @.\\@ relative to current directory
-- * @C:file@ relative to current directory in drive
#else
-- | A path that is attached to a root e.g. "/x" or "./x" are rooted paths. "/"
-- is considered an absolute root and "." as a dynamic root. ".." is not
-- considered a root, "../x" or "x/y" are not rooted paths.
--
-- * An absolute rooted path: @/@ starting from file system root
-- * A dynamic rooted path: @./@ relative to current directory
#endif
isRooted :: OS_PATH -> Bool
isRooted (OS_PATH arr) = Common.isRooted Common.OS_NAME arr

-- | A path that is not attached to a root e.g. @a\/b\/c@ or @..\/b\/c@.
--
-- >>> isBranch = not . Path.isRooted
--
isBranch :: OS_PATH -> Bool
isBranch = not . isRooted

-- XXX This can be generalized to an Array intersperse operation
-- XXX This can work on a polymorphic IsPath type.

{-# INLINE unsafeAppend #-}
unsafeAppend :: OS_PATH -> OS_PATH -> OS_PATH
unsafeAppend (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.unsafeAppend
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | Append a OS_PATH to another. Fails if the second path refers to a location
-- and not a path segment.
--
-- >>> Path.toString $ Path.append [path|/usr|] [path|bin|]
-- "/usr/bin"
--
append :: OS_PATH -> OS_PATH -> OS_PATH
append (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

------------------------------------------------------------------------------
-- Splitting path
------------------------------------------------------------------------------

splitRoot :: OS_PATH -> (OS_PATH, OS_PATH)
splitRoot (OS_PATH a) =
    bimap OS_PATH OS_PATH $ Common.splitRoot Common.OS_NAME a

{-# INLINE splitPath #-}
splitPath :: Monad m => OS_PATH -> Stream m OS_PATH
splitPath (OS_PATH a) = fmap OS_PATH $ Common.splitPath Common.OS_NAME a

{-# INLINE splitPath_ #-}
splitPath_ :: Monad m => OS_PATH -> Stream m OS_PATH
splitPath_ (OS_PATH a) = fmap OS_PATH $ Common.splitPath_ Common.OS_NAME a

splitFile :: OS_PATH -> (OS_PATH, OS_PATH)
splitFile (OS_PATH a) =
    bimap OS_PATH OS_PATH $ Common.splitFile Common.OS_NAME a

splitExtension :: OS_PATH -> (OS_PATH, OS_PATH)
splitExtension (OS_PATH a) =
    bimap OS_PATH OS_PATH $ Common.splitExtension Common.OS_NAME a

------------------------------------------------------------------------------
-- Path equality
------------------------------------------------------------------------------

eqPath :: OS_PATH -> OS_PATH -> Bool
#ifdef IS_WINDOWS
eqPath (OS_PATH a) (OS_PATH b) = Common.eqWindowsPath a b
#else
eqPath (OS_PATH a) (OS_PATH b) = Common.eqPosixPath a b
#endif

eqPathStrict :: OS_PATH -> OS_PATH -> Bool
eqPathStrict (OS_PATH a) (OS_PATH b) = Common.eqPathStrict Common.OS_NAME a b

eqPathBytes :: OS_PATH -> OS_PATH -> Bool
eqPathBytes (OS_PATH a) (OS_PATH b) = Common.eqPathBytes a b
