{-# LANGUAGE TemplateHaskell #-}

-- Anything other than windows (Linux/macOS/FreeBSD) is Posix
#if defined(IS_WINDOWS)
#define OS_NAME Windows
#define OS_PATH WindowsPath
#define WORD_TYPE Word16
#define UNICODE_ENCODER encodeUtf16le'
#define UNICODE_DECODER decodeUtf16le'
#define UNICODE_DECODER_LAX decodeUtf16le
#define CODEC_NAME UTF-16LE
#define SEPARATORS @/, \\@
#else
#define OS_NAME Posix
#define OS_PATH PosixPath
#define WORD_TYPE Word8
#define UNICODE_ENCODER encodeUtf8'
#define UNICODE_DECODER decodeUtf8'
#define UNICODE_DECODER_LAX decodeUtf8
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

    -- * Validation
    , validatePath
    , isValidPath
#ifdef IS_WINDOWS
    , validatePath'
    , isValidPath'
#endif

    -- * Construction
    , fromChunk
    , unsafeFromChunk
    , fromChars
    , fromString
    , unsafeFromString
    -- , fromCString#
    -- , fromW16CString#
    , readRaw

    -- * Statically Verified String Literals
    -- | Quasiquoters.
    , path

    -- * Statically Verified Strings
    -- | Template Haskell expression splices.

    -- Note: We expose these even though we have quasiquoters as these TH
    -- helpers are more powerful. They are useful if we are generating strings
    -- statically using methods other than literals or if we are doing some
    -- text processing on strings before using them.
    , pathE

    -- * Elimination
    , toChunk
    , toChars
    , toChars_
    , toString
#ifndef IS_WINDOWS
    , asCString
#else
    , asCWString
#endif
    , toString_
    , showRaw

    -- * Separators
    -- Do we need to export the separator functions? They are not essential if
    -- operations to split and combine paths are provided. If someone wants to
    -- work on paths at low level then they know what they are.
    -- , isPrimarySeparator
    -- , isSeparator
    , dropTrailingSeparators

    -- * Tests
    , isRooted
    , isBranch

    -- * Joining
    , unsafeAppend
    , append
    , append'
#ifndef IS_WINDOWS
    , appendCString
    , appendCString'
#endif

    -- * Splitting
    , splitRoot
    , splitPath
    , splitPath_
    , splitFile
    , splitExtension

    -- * Equality
    , eqPath
    , EqCfg(..)
    , eqCfg
    , eqPathWith
    , eqPathBytes
    )
where

import Control.Monad.Catch (MonadThrow(..))
import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)
import Data.Word (Word8)
#if defined(IS_WINDOWS)
import Data.Word (Word16)
#endif
#ifndef IS_WINDOWS
import Foreign.C (CString)
#else
import Foreign.C (CWString)
#endif
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.FileSystem.Path.Common (mkQ, EqCfg(..), eqCfg)

import qualified Streamly.Internal.Data.Array as Array
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
>>> import Data.Maybe (fromJust)
>>> import qualified Streamly.Data.Stream as Stream

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath, path)
>>> import qualified Streamly.Internal.Data.Array as Array
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path
>>> import qualified Streamly.Unicode.Stream as Unicode

>>> import Data.Either (Either, isLeft)
>>> import Control.Exception (SomeException, evaluate, try)

>>> rawFromString = Array.fromPureStream . Unicode.encodeUtf8' . Stream.fromList
>>> pack = fromJust . Path.fromString
>>> fails action = (try (evaluate action) :: IO (Either SomeException String)) >>= return . isLeft
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
-- A OS_PATH is validated before construction unless unsafe constructors are
-- used to create it. For validations performed by the safe construction
-- methods see the 'fromChars' function.
--
-- Note that in some cases the file system may perform unicode normalization on
-- paths (e.g. Apple HFS), it may cause surprising results as the path used by
-- the user may not have the same bytes as later returned by the file system.
newtype OS_PATH = OS_PATH (Array WORD_TYPE)

-- XXX The Eq instance may be provided but it will require some sensible
-- defaults for comparison. For example, should we use case sensitive or
-- insensitive comparison? It depends on the underlying file system. For now
-- now we have eqPath operations for equality comparison.

instance IsPath OS_PATH OS_PATH where
    unsafeFromPath = id
    fromPath = pure
    toPath = id

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency. If the argument path is already verfied for a property, we
-- should not verify it again e.g. if we adapt (Rooted path) as (Rooted (Dir
-- path)) then we should not verify it to be Rooted again.

-- XXX castPath?

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one. This can be used to upgrade or downgrade type safety.
adapt :: (MonadThrow m, IsPath OS_PATH a, IsPath OS_PATH b) => a -> m b
adapt p = fromPath (toPath p :: OS_PATH)

------------------------------------------------------------------------------
-- Path parsing utilities
------------------------------------------------------------------------------

-- | If the path is @//@ the result is @/@. If it is @dir//@ then the result is
-- @dir@. On Windows "c:" and "c:/" are different paths, therefore, we do not
-- drop the trailing separator from "c:/".
--
-- Note that a path with trailing separators may implicitly be considered as a
-- directory by some applications. So dropping it may change the dir nature of
-- the path.
--
-- >>> f a = Path.toString $ Path.dropTrailingSeparators (pack a)
-- >>> f "./"
-- "."
--
{-# INLINE dropTrailingSeparators #-}
dropTrailingSeparators :: OS_PATH -> OS_PATH
dropTrailingSeparators (OS_PATH arr) =
    OS_PATH (Common.dropTrailingSeparators Common.OS_NAME arr)

-- | Throws an exception if the path is not valid. See 'isValidPath' for the
-- list of validations.
#ifndef IS_WINDOWS
validatePath :: MonadThrow m => Array Word8 -> m ()
#else
validatePath :: MonadThrow m => Array Word16 -> m ()
#endif
validatePath = Common.validatePath Common.OS_NAME

#ifndef IS_WINDOWS
-- | Check if the filepath is valid i.e. does the operating system or the file
-- system allow such a path in listing or creating files?
--
-- >>> isValid = Path.isValidPath . rawFromString
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
--
isValidPath :: Array Word8 -> Bool
isValidPath = Common.isValidPath Common.OS_NAME
#endif

-- Note: CPP gets confused by the prime suffix, so we have to put the CPP
-- macros on the next line to get it to work.

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- A chunk is essentially an untyped Array i.e. Array Word8.  We can either use
-- the term ByteArray for that or just Chunk. The latter is shorter and we have
-- been using it consistently in streamly. We use "bytes" for a stream of
-- bytes.

-- | /Unsafe/: The user is responsible to make sure that the path is valid as
-- per 'isValidPath'.
--
{-# INLINE unsafeFromChunk #-}
#ifndef IS_WINDOWS
unsafeFromChunk :: IsPath OS_PATH a => Array Word8 -> a
#else
unsafeFromChunk :: IsPath OS_PATH a => Array Word16 -> a
#endif
unsafeFromChunk =
#ifndef DEBUG
    unsafeFromPath . OS_PATH . Common.unsafeFromChunk
#else
    fromJust . fromChunk
#endif

-- XXX mkPath?

-- | Convert a byte array into a Path.
-- Throws 'InvalidPath' if 'isValidPath' fails on the path.
--
#ifndef IS_WINDOWS
fromChunk :: (MonadThrow m, IsPath OS_PATH a) => Array Word8 -> m a
#else
fromChunk :: (MonadThrow m, IsPath OS_PATH a) => Array Word16 -> m a
#endif
fromChunk arr = Common.fromChunk Common.OS_NAME arr >>= fromPath . OS_PATH

-- XXX Should be a Fold instead?

-- | Encode a Unicode character stream to OS_PATH using strict CODEC_NAME encoding.
--
-- * Throws 'InvalidPath' if 'isValidPath' fails on the path
-- * Fails if the stream contains invalid unicode characters
--
-- We do not sanitize the path i.e. we do not remove duplicate separators,
-- redundant @.@ segments, trailing separators etc because that would require
-- unnecessary checks and modifications to the path which may not be used ever
-- for any useful purpose, it is only needed for path equality and can be done
-- during the equality check.
--
-- On Windows it accepts a share root even if a path does not follow it.
--
-- Unicode normalization is not done. If normalization is needed the user can
-- normalize it and then use the 'fromChunk' API.
{-# INLINE fromChars #-}
fromChars :: (MonadThrow m, IsPath OS_PATH a) => Stream Identity Char -> m a
fromChars s =
    Common.fromChars Common.OS_NAME Unicode.UNICODE_ENCODER s
        >>= fromPath . OS_PATH

-- | Like 'fromString' but does not perform any validations mentioned under
-- 'isValidPath'. Fails only if unicode encoding fails.
unsafeFromString :: IsPath OS_PATH a => [Char] -> a
unsafeFromString =
#ifndef DEBUG
      unsafeFromPath
    . OS_PATH
    . Common.unsafeFromChars Unicode.UNICODE_ENCODER
    . Stream.fromList
#else
    fromJust . fromString
#endif

-- | Convert astring to OS_PATH. See 'fromChars' for failure cases and
-- semantics.
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

-- | Generates a Haskell expression of type OS_PATH from a String. Equivalent
-- to using 'fromString' on the string passed.
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

-- | Generates a OS_PATH type from a quoted literal. Equivalent to using
-- 'fromString' on the static literal.
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
{-# INLINE toChars #-}
toChars :: (Monad m, IsPath OS_PATH a) => a -> Stream m Char
toChars p =
    let (OS_PATH arr) =
            toPath p in Common.toChars Unicode.UNICODE_DECODER arr

-- | Decode the path to a stream of Unicode chars using lax CODEC_NAME decoding.
toChars_ :: (Monad m, IsPath OS_PATH a) => a -> Stream m Char
toChars_ p =
    let (OS_PATH arr) =
            toPath p in Common.toChars Unicode.UNICODE_DECODER_LAX arr

-- XXX When showing, append a "/" to dir types?

-- | Decode the path to a Unicode string using strict CODEC_NAME decoding.
toString :: IsPath OS_PATH a => a -> [Char]
toString = runIdentity . Stream.toList . toChars

-- | Decode the path to a Unicode string using lax CODEC_NAME decoding.
toString_ :: IsPath OS_PATH a => a -> [Char]
toString_ = runIdentity . Stream.toList . toChars_

-- | Show the path as raw characters without any specific decoding.
--
-- See also: 'readRaw'.
--
showRaw :: IsPath OS_PATH a => a -> [Char]
showRaw p =
    let (OS_PATH arr) =
            toPath p in show arr

#ifndef IS_WINDOWS
-- | Parse a raw array of bytes as a path type.
--
-- >>> readRaw = fromJust . Path.fromChunk . read
--
-- >>> arr = rawFromString "hello"
-- >>> Path.showRaw $ (Path.readRaw $ show arr :: Path.PosixPath)
-- "fromList [104,101,108,108,111]"
--
-- See also: 'showRaw'.
readRaw :: IsPath OS_PATH a => [Char] -> a
readRaw = fromJust . fromChunk . read
#endif

-- We cannot show decoded path in the Show instance as it may not always
-- succeed and it depends on the encoding which we may not even know. The
-- encoding may depend on the OS and the file system. Also we need Show and
-- Read to be inverses. The best we can provide is to show the bytes as
-- Hex or decimal values.
{-
instance Show OS_PATH where
    show (OS_PATH x) = show x
-}

#ifndef IS_WINDOWS
-- | Use the path as a pinned CString. Useful for using a PosixPath in
-- system calls on Posix.
{-# INLINE asCString #-}
asCString :: OS_PATH -> (CString -> IO a) -> IO a
asCString p = Array.asCStringUnsafe (toChunk p)
#else
-- | Use the path as a pinned CWString. Useful for using a WindowsPath in
-- system calls on Windows.
{-# INLINE asCWString #-}
asCWString :: OS_PATH -> (CWString -> IO a) -> IO a
asCWString p = Array.asCWString (toChunk p)
#endif

------------------------------------------------------------------------------
-- Operations on Path
------------------------------------------------------------------------------

#ifndef IS_WINDOWS
-- | A path that is attached to a root e.g. "\/x" or ".\/x" are rooted paths.
-- "\/" is considered an absolute root and "." as a dynamic root. ".." is not
-- considered a root, "..\/x" or "x\/y" are not rooted paths.
--
-- >>> isRooted = Path.isRooted . pack
--
-- >>> isRooted "/"
-- True
-- >>> isRooted "/x"
-- True
-- >>> isRooted "."
-- True
-- >>> isRooted "./x"
-- True
--
isRooted :: OS_PATH -> Bool
isRooted (OS_PATH arr) = Common.isRooted Common.OS_NAME arr
#endif

-- | A path that is not attached to a root e.g. @a\/b\/c@ or @..\/b\/c@.
--
-- >>> isBranch = not . Path.isRooted
--
-- >>> isBranch = Path.isBranch . pack
--
-- >>> isBranch "x"
-- True
-- >>> isBranch "x/y"
-- True
-- >>> isBranch ".."
-- True
-- >>> isBranch "../x"
-- True
--
isBranch :: OS_PATH -> Bool
isBranch = not . isRooted

-- XXX This can be generalized to an Array intersperse operation
-- XXX This can work on a polymorphic IsPath type.

-- | Like 'append' but does not check if any of the path is empty or if the
-- second path is rooted.
--
-- >>> append a b = Path.toString $ Path.unsafeAppend (pack a) (pack b)
--
-- >>> append "x" "y"
-- "x/y"
-- >>> append "x/" "y"
-- "x/y"
-- >>> append "x" "/y"
-- "x/y"
-- >>> append "x/" "/y"
-- "x/y"
--
{-# INLINE unsafeAppend #-}
unsafeAppend :: OS_PATH -> OS_PATH -> OS_PATH
unsafeAppend (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.unsafeAppend
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- XXX Should we fail if the first path does not have a trailing separator i.e.
-- it is not a directory?

#ifndef IS_WINDOWS
-- | Append a OS_PATH to another. Fails if the second path refers to a rooted
-- path. Use 'unsafeAppend' to avoid failure if you know it is ok to append the
-- path or use the typesafe Streamly.FileSystem.OS_PATH.Seg module.
--
-- >>> f a b = Path.toString $ Path.append a b
--
-- >>> f [path|/usr|] [path|bin|]
-- "/usr/bin"
-- >>> f [path|/usr/|] [path|bin|]
-- "/usr/bin"
-- >>> fails (f [path|/usr|] [path|/bin|])
-- True
--
append :: OS_PATH -> OS_PATH -> OS_PATH
append (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | A stricter version of 'append' which requires the first path to be a
-- directory like path i.e. with a trailing separator.
--
-- >>> f a b = Path.toString $ Path.append' a b
--
-- >>> fails $ f [path|/usr|] [path|bin|]
-- True
--
append' ::
    OS_PATH -> OS_PATH -> OS_PATH
append'
    (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append'
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b
#endif

-- XXX This can be pure, like append.
-- XXX add appendW16CString for Windows?

#ifndef IS_WINDOWS
-- | Append a separator and a CString to the Array. This is like 'unsafeAppend'
-- but always inserts a separator between the two paths even if the first path
-- has a trailing separator or second path has a leading separator.
--
appendCString :: OS_PATH -> CString -> IO OS_PATH
appendCString (OS_PATH a) str =
    fmap OS_PATH
        $ Common.appendCString
            Common.OS_NAME a str

-- | Like 'appendCString' but creates a pinned path.
--
appendCString' ::
    OS_PATH -> CString -> IO OS_PATH
appendCString'
    (OS_PATH a) str =
    fmap OS_PATH
        $ Common.appendCString'
            Common.OS_NAME a str
#endif

------------------------------------------------------------------------------
-- Splitting path
------------------------------------------------------------------------------

#ifndef IS_WINDOWS
-- | If a path is rooted then separate the root and the remaining path,
-- otherwise root is returned as empty. If the path is rooted then the non-root
-- part is guaranteed to not start with a separator.
--
-- >>> toList (a,b) = (Path.toString a, Path.toString b)
-- >>> split = toList . Path.splitRoot . pack
--
-- >>> split "/"
-- ("/","")
--
-- >>> split "."
-- (".","")
--
-- >>> split "./"
-- ("./","")
--
-- >>> split "/home"
-- ("/","home")
--
-- >>> split "//"
-- ("//","")
--
-- >>> split "./home"
-- ("./","home")
--
-- >>> split "home"
-- ("","home")
--
splitRoot :: OS_PATH -> (OS_PATH, OS_PATH)
splitRoot (OS_PATH a) =
    bimap OS_PATH OS_PATH $ Common.splitRoot Common.OS_NAME a

-- | Split the path components keeping separators between path components
-- attached to the dir part. Redundant separators are removed, only the first
-- one is kept. Separators are not added either e.g. "." and ".." may not have
-- trailing separators if the original path does not.
--
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath . pack
--
-- >>> split "."
-- ["."]
--
-- >>> split "././"
-- ["./"]
--
-- >>> split "./a/b/."
-- ["./","a/","b/"]
--
-- >>> split ".."
-- [".."]
--
-- >>> split "../"
-- ["../"]
--
-- >>> split "a/.."
-- ["a/",".."]
--
-- >>> split "/"
-- ["/"]
--
-- >>> split "//"
-- ["/"]
--
-- >>> split "/x"
-- ["/","x"]
--
-- >>> split "/./x/"
-- ["/","x/"]
--
-- >>> split "/x/./y"
-- ["/","x/","y"]
--
-- >>> split "/x/../y"
-- ["/","x/","../","y"]
--
-- >>> split "/x///y"
-- ["/","x/","y"]
--
-- >>> split "/x/\\y"
-- ["/","x/","\\y"]
--
{-# INLINE splitPath #-}
splitPath :: Monad m => OS_PATH -> Stream m OS_PATH
splitPath (OS_PATH a) = fmap OS_PATH $ Common.splitPath Common.OS_NAME a

-- | Split a path into components separated by the path separator. "."
-- components in the path are ignored except when in the leading position.
-- Trailing separators in non-root components are dropped.
--
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath_ . pack
--
-- >>> split "."
-- ["."]
--
-- >>> split "././"
-- ["."]
--
-- >>> split ".//"
-- ["."]
--
-- >>> split "//"
-- ["/"]
--
-- >>> split "//x/y/"
-- ["/","x","y"]
--
-- >>> split "./a"
-- [".","a"]
--
-- >>> split "a/."
-- ["a"]
--
-- >>> split "/"
-- ["/"]
--
-- >>> split "/x"
-- ["/","x"]
--
-- >>> split "/./x/"
-- ["/","x"]
--
-- >>> split "/x/./y"
-- ["/","x","y"]
--
-- >>> split "/x/../y"
-- ["/","x","..","y"]
--
-- >>> split "/x///y"
-- ["/","x","y"]
--
-- >>> split "/x/\\y"
-- ["/","x","\\y"]
--
{-# INLINE splitPath_ #-}
splitPath_ :: Monad m => OS_PATH -> Stream m OS_PATH
splitPath_ (OS_PATH a) = fmap OS_PATH $ Common.splitPath_ Common.OS_NAME a
#endif

-- | Split a multi-component path into (dir, file) if its last component can be
-- a file i.e.:
--
-- * the path does not end with a separator
-- * the path does not end with a . or .. component
--
-- Split a single component into ("", path) if it can be a file i.e. it is not
-- a path root, "." or "..".
--
-- If the path cannot be a file then (path, "") is returned.
--
-- >>> toList (a,b) = (Path.toString a, Path.toString b)
-- >>> split = toList . Path.splitFile . pack
--
-- >>> split "/"
-- ("/","")
--
-- >>> split "."
-- (".","")
--
-- >>> split "/."
-- ("/.","")
--
-- >>> split ".."
-- ("..","")
--
-- >>> split "//"
-- ("//","")
--
-- >>> split "/home"
-- ("/","home")
--
-- >>> split "./home"
-- ("./","home")
--
-- >>> split "home"
-- ("","home")
--
-- >>> split "x/"
-- ("x/","")
--
-- >>> split "x/y"
-- ("x/","y")
--
-- >>> split "x//y"
-- ("x//","y")
--
-- >>> split "x/./y"
-- ("x/./","y")
splitFile :: OS_PATH -> (OS_PATH, OS_PATH)
splitFile (OS_PATH a) =
    bimap OS_PATH OS_PATH $ Common.splitFile Common.OS_NAME a

#ifndef IS_WINDOWS
-- | For the purposes of this function a file is considered to have an
-- extension if the file name can be broken down into a non-empty filename
-- followed by an extension separator (usually ".") followed by a non-empty
-- extension with at least one character other than the extension separator
-- characters. The shortest suffix obtained by this rule, starting with the
-- extension separator is returned as the extension and the remaining prefix
-- part as the filename.
--
-- A directory name does not have an extension.
--
-- >>> toList (a,b) = (Path.toString a, Path.toString b)
-- >>> split = toList . Path.splitExtension . pack
--
-- >>> split "/"
-- ("/","")
--
-- >>> split "."
-- (".","")
--
-- >>> split ".."
-- ("..","")
--
-- >>> split "x"
-- ("x","")
--
-- >>> split "/x"
-- ("/x","")
--
-- >>> split "x/"
-- ("x/","")
--
-- >>> split "./x"
-- ("./x","")
--
-- >>> split "x/."
-- ("x/.","")
--
-- >>> split "x/y."
-- ("x/y.","")
--
-- >>> split "/x.y"
-- ("/x",".y")
--
-- >>> split "/x.y." -- XXX should it be .y.?
-- ("/x.y.","")
--
-- >>> split "/x.y.." -- XXX should it be .y..?
-- ("/x.y..","")
--
-- >>> split "x/.y"
-- ("x/.y","")
--
-- >>> split ".x"
-- (".x","")
--
-- >>> split "x."
-- ("x.","")
--
-- >>> split ".x.y"
-- (".x",".y")
--
-- >>> split "x/y.z"
-- ("x/y",".z")
--
-- >>> split "x.y.z"
-- ("x.y",".z")
--
-- >>> split "x..y"
-- ("x.",".y")
--
-- >>> split "..."
-- ("...","")
--
-- >>> split "..x"
-- (".",".x")
--
-- >>> split "...x"
-- ("..",".x")
--
-- >>> split "x/y.z/"
-- ("x/y.z/","")
--
-- >>> split "x/y"
-- ("x/y","")
--
splitExtension :: OS_PATH -> (OS_PATH, OS_PATH)
splitExtension (OS_PATH a) =
    bimap OS_PATH OS_PATH $ Common.splitExtension Common.OS_NAME a
#endif

------------------------------------------------------------------------------
-- Path equality
------------------------------------------------------------------------------

#ifndef IS_WINDOWS
-- | Checks two paths for logical equality. It performs some normalizations on
-- the paths before comparing them, specifically it drops redundant path
-- separators between path segments and redundant "\/.\/" components between
-- segments.
--
-- Equality semantics followed by this routine are listed below. If it returns
-- equal then the paths are definitely equal, if it returns unequal then the
-- paths may still be equal using some relaxed equality criterion.
--
-- * paths with a leading "." and without a leading "." e.g. ".\/x\/y"
-- and "x\/y" are treated as unequal. The first one is a dynamically rooted path
-- and the second one is a free path segment.
--
-- * An absolute path and a path relative to "." may be equal depending on the
-- meaning of ".", however this routine treats them as unequal.
--
-- * Two paths starting with a leading "." may not actually be equal even if
-- they are literally equal. We return unequal even though they may be equal
-- sometimes.
--
-- * Two paths having ".." components may be equal after processing the ".."
-- components even if we determined them to be unequal. However, if we
-- determined them to be equal then they must be equal.
--
-- * A path with a trailing slash and a path without are treated as unequal
-- e.g. "x" is not the same as "x\/". The latter is a directory.
--
-- In short, for strict equality both the paths must be absolute or both must
-- be path segments without a leading root component (e.g. x\/y). Also, both
-- must be files or both must be directories.
--
-- >>> :{
--  eq a b = Path.eqPath (pack a) (pack b)
-- :}
--
-- >>> eq "/x"  "//x"
-- True
--
-- >>> eq "x//y"  "x/y"
-- True
--
-- >>> eq "x/./y"  "x/y"
-- True
--
-- >>> eq "./x"  "x"
-- False
--
-- >>> eq "x/"  "x"
-- False
--
-- >>> eq "x"  "x"
-- True
--
-- >>> eq "x"  "X"
-- False
--
-- >>> eq ".."  ".."
-- True
--
-- >>> eq "."  "."
-- False
--
-- >>> eq "./x"  "./x"
-- False
--
eqPath :: OS_PATH -> OS_PATH -> Bool
eqPath (OS_PATH a) (OS_PATH b) =
    Common.eqPath Unicode.UNICODE_DECODER
        Common.OS_NAME a b

-- | Like 'eqPath' but we can control the equality options.
--
-- >>> :{
--  cfg = Path.eqCfg
--      { Path.ignoreTrailingSeparators = True
--      , Path.allowRelativeEquality = True
--      }
--  eq a b = Path.eqPathWith cfg (pack a) (pack b)
-- :}
--
-- >>> eq "."  "."
-- True
--
-- >>> eq "./"  ".//"
-- True
--
-- >>> eq "./x"  "./x"
-- True
--
-- >>> eq "./x"  "x"
-- True
--
-- >>> eq "x/"  "x"
-- True
--
-- >>> eq "x/"  "X"
-- False
--
-- >>> eq "x//y"  "x/y"
-- True
--
-- >>> eq "x/./y"  "x/y"
-- True
--
-- >>> eq "x"  "x"
-- True
--
eqPathWith :: EqCfg -> OS_PATH -> OS_PATH -> Bool
eqPathWith cfg (OS_PATH a) (OS_PATH b) =
    Common.eqPathWith Unicode.UNICODE_DECODER
        Common.OS_NAME cfg a b
#endif

-- | Check two paths for byte level equality. This is the most strict path
-- equality check.
--
-- >>> eqPath a b = Path.eqPathBytes (pack a) (pack b)
--
-- >>> eqPath "x//y"  "x//y"
-- True
--
-- >>> eqPath "x//y"  "x/y"
-- False
--
-- >>> eqPath "x/./y"  "x/y"
-- False
--
-- >>> eqPath "x\\y" "x/y"
-- False
--
-- >>> eqPath "./file"  "file"
-- False
--
-- >>> eqPath "file/"  "file"
-- False
--
eqPathBytes :: OS_PATH -> OS_PATH -> Bool
eqPathBytes (OS_PATH a) (OS_PATH b) = Common.eqPathBytes a b
