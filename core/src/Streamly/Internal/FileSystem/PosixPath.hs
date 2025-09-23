{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#if defined(IS_PORTABLE)
#define OS_PATH_TYPE Path
#define OS_WORD_TYPE OsWord
#define OS_CSTRING_TYPE OsCString
#define AS_OS_CSTRING asOsCString
#elif defined(IS_WINDOWS)
#define OS_PATH_TYPE WindowsPath
#define OS_WORD_TYPE Word16
#define OS_CSTRING_TYPE CWString
#define AS_OS_CSTRING asCWString
#else
#define OS_PATH_TYPE PosixPath
#define OS_WORD_TYPE Word8
#define OS_CSTRING_TYPE CString
#define AS_OS_CSTRING asCString
#endif

-- Anything other than windows (Linux/macOS/FreeBSD) is Posix
#if defined(IS_WINDOWS)
#define OS_NAME Windows
#define OS_PATH WindowsPath
#define OS_WORD Word16
#define OS_CSTRING CWString
#define UNICODE_ENCODER encodeUtf16le'
#define UNICODE_DECODER decodeUtf16le'
#define UNICODE_DECODER_LAX decodeUtf16le
#define CODEC_NAME UTF-16LE
#define SEPARATORS @/, \\@
#else
#define OS_NAME Posix
#define OS_PATH PosixPath
#define OS_WORD Word8
#define OS_CSTRING CString
#define UNICODE_ENCODER encodeUtf8'
#define UNICODE_DECODER decodeUtf8'
#define UNICODE_DECODER_LAX decodeUtf8
#define CODEC_NAME UTF-8
#define SEPARATORS @/@
#endif


-- XXX Do not start a module haddock comment here as this file gets included in
-- Path.hs and then we have duplicate module level comment in that file,
-- generating a haddock warning.

-- Module      : Streamly.Internal.FileSystem.OS_PATH_TYPE
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- This module implements a OS_PATH_TYPE type representing a file system path for
-- OS_NAME operating systems. The only assumption about the encoding of the
-- path is that it maps the characters SEPARATORS and @.@ to OS_WORD_TYPE
-- representing their ASCII values. Operations are provided to encode and
-- decode using CODEC_NAME encoding.
--
-- This module has APIs that are equivalent to or can emulate all or most of
-- the filepath package APIs. It has some differences from the filepath
-- package:
--
-- * Empty paths are not allowed. Paths are validated before construction.
-- * The default Path type itself affords considerable safety regarding the
-- distinction of rooted or non-rooted paths, it also allows distinguishing
-- directory and file paths.
-- * It is designed to provide flexible typing to provide compile time safety
-- for rooted/non-rooted paths and file/dir paths. The Path type is just part
-- of that typed path ecosystem. Though the default Path type itself should be
-- enough for most cases.
-- * It leverages the streamly array module for most of the heavy lifting,
-- it is a thin wrapper on top of that, improving maintainability as well as
-- providing better performance. We can have pinned and unpinned paths, also
-- provide lower level operations for certain cases to interact more
-- efficinetly with low level code.

module Streamly.Internal.FileSystem.OS_PATH_TYPE
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Type
#if defined(IS_PORTABLE)
      OS_WORD_TYPE
    , OS_CSTRING_TYPE
    , OS_PATH_TYPE
#else
      OS_PATH_TYPE (..)
#endif
    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Conversion to OsWord
    , charToWord
    , wordToChar

    -- * Validation
    , validatePath
    , isValidPath
#ifdef IS_WINDOWS
    , validatePath'
    , isValidPath'
#endif

    -- * Construction
    , fromArray
    , unsafeFromArray
    , fromChars
    , fromString
    , fromString_
    , encodeString
    , unsafeFromString
    -- , fromCString#
    -- , fromCWString#
    , readArray

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
    , toArray
    , toChars
    , toChars_
    , toString
    , AS_OS_CSTRING
    , toString_
    , showArray

    -- * Separators
    -- Do we need to export the separator char functions? They are not
    -- essential if operations to split and combine paths are provided. If
    -- someone wants to work on paths at low level then they know what they
    -- are. We should export the OsWord based operations to work with arrays.
    , separator
    , isSeparator
    , extSeparator

    -- * Dir or non-dir paths

    -- You do not need these, instead use eqPath with ignoreTrailingSeparators.
    , dropTrailingSeparators
    , hasTrailingSeparator
    , addTrailingSeparator

    -- * Path Segment Types
    , isRooted
    , isUnrooted

    -- * Joining
    , joinStr
 -- , concat
    , unsafeJoin
#ifndef IS_WINDOWS
    , joinCStr
    , joinCStr'
#endif
    , join
    , joinDir
    , unsafeJoinPaths

    -- * Splitting
    -- | Note: you can use 'unsafeJoin' as a replacement for the joinDrive
    -- function in the filepath package.
    , splitRoot
    , splitPath
    , splitPath_
    , splitFile

    , splitFirst
    , splitLast

    -- ** Extension
    , splitExtension
    , dropExtension
    , addExtension
    , replaceExtension

    -- ** Path View
    , takeFileName
    , takeDirectory
 -- , takeDirectory_ -- drops the trailing /
    , takeExtension
    , takeFileBase

    -- * Equality
    , EqCfg
    , ignoreTrailingSeparators
    , ignoreCase
    , allowRelativeEquality
    , eqPath
    , eqPathBytes
    , normalize
    )
where

import Control.Exception (throw)
import Control.Monad.Catch (MonadThrow(..))
import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust, isJust)
#ifndef IS_WINDOWS
import Data.Word (Word8)
import Foreign.C (CString)
#else
import Data.Word (Word16)
import Foreign.C (CWString)
#endif
import Language.Haskell.TH.Syntax (lift)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.FileSystem.Path.Common (mkQ, EqCfg(..))

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.Path.Common as Common
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Streamly.Internal.Data.Path

#if defined(IS_PORTABLE)
import Streamly.Internal.FileSystem.OS_PATH (OS_PATH(..))
#endif

-- NOTES about C preprocessor use.
--
-- docspec comment lines cannot use CPP macros, docspec does not expand them
-- before running tests.
--
-- We cannot use a CPP conditional inside haddock comments because the
-- conditional line replaced by a blank line by CPP and this breaks the haddock
-- comment. Therefore if the comment is slightly different on a different
-- platform we duplicate the entire comment inside a conditional.

#ifdef IS_PORTABLE
#include "DocTestFileSystemPath.hs"
#elif defined(IS_WINDOWS)
#include "DocTestFileSystemWindowsPath.hs"
#else
#include "DocTestFileSystemPosixPath.hs"
#endif

#if defined(IS_PORTABLE)
type OS_PATH_TYPE = OS_PATH
type OS_WORD_TYPE = OS_WORD
type OS_CSTRING_TYPE = OS_CSTRING
#else
-- | A type representing file system paths on OS_NAME.
--
-- A OS_PATH_TYPE is validated before construction unless unsafe constructors are
-- used to create it. For validations performed by the safe construction
-- methods see the 'fromChars' function.
--
-- Note that in some cases the file system may perform unicode normalization on
-- paths (e.g. Apple HFS), it may cause surprising results as the path used by
-- the user may not have the same bytes as later returned by the file system.
newtype OS_PATH = OS_PATH (Array OS_WORD_TYPE)

-- XXX The Eq instance may be provided but it will require some sensible
-- defaults for comparison. For example, should we use case sensitive or
-- insensitive comparison? It depends on the underlying file system. For now
-- now we have eqPath operations for equality comparison.

instance IsPath OS_PATH OS_PATH where
    unsafeFromPath = id
    fromPath = pure
    toPath = id
#endif

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency. If the argument path is already verfied for a property, we
-- should not verify it again e.g. if we adapt (Rooted path) as (Rooted (Dir
-- path)) then we should not verify it to be Rooted again.

-- XXX castPath?

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one. This can be used to upgrade or downgrade type safety.
adapt :: (MonadThrow m, IsPath OS_PATH_TYPE a, IsPath OS_PATH_TYPE b) => a -> m b
adapt p = fromPath (toPath p :: OS_PATH_TYPE)

------------------------------------------------------------------------------
-- Char to word
------------------------------------------------------------------------------

-- | Unsafe, truncates the Char to Word8 on Posix and Word16 on Windows.
charToWord :: Char -> OS_WORD_TYPE
charToWord = Common.charToWord

-- | Unsafe, should be a valid character.
wordToChar :: OS_WORD_TYPE -> Char
wordToChar = Common.wordToChar

------------------------------------------------------------------------------
-- Separators
------------------------------------------------------------------------------

-- | The primary path separator word: @/@ on POSIX and @\\@ on Windows.
-- Windows also supports @/@ as a valid separator. Use 'isSeparator' to check
-- for any valid path separator.
{-# INLINE separator #-}
separator :: OS_WORD_TYPE
separator = charToWord $ Common.primarySeparator Common.OS_NAME

-- | On POSIX, only @/@ is a path separator, whereas on Windows both @/@ and
-- @\\@ are valid separators.
{-# INLINE isSeparator #-}
isSeparator :: OS_WORD_TYPE -> Bool
isSeparator = Common.isSeparatorWord Common.OS_NAME

-- | File extension separator word.
{-# INLINE extSeparator #-}
extSeparator :: OS_WORD_TYPE
extSeparator = Common.extensionWord

------------------------------------------------------------------------------
-- Path parsing utilities
------------------------------------------------------------------------------

-- XXX We can have prime suffixed versions where it drops or adds separator
-- unconditionally. Alternatively, we can convert the path to array and use
-- array operations instead.

-- | Remove all trailing path separators from the given 'Path'.
--
-- Instead of this operation you may want to use 'eqPath' with
-- 'ignoreTrailingSeparators' option.
--
-- This operation is careful not to alter the semantic meaning of the path.
-- For example, on Windows:
--
--   * Dropping the separator from "C:/" would change the meaning of the path
--     from referring to the root of the C: drive to the current directory on C:.
--   * If a path ends with a separator immediately after a colon (e.g., "C:/"),
--     the separator will not be removed.
--
-- If the input path is invalid, the behavior is not fully guaranteed:
--
--   * The separator may still be dropped.
--   * In some cases, dropping the separator may make an invalid path valid
--     (e.g., "C:\\\\" or "C:\\/").
--
-- This operation may convert a path that implicitly refers to a directory
-- into one that does not.
--
-- Typically, if the path is @dir//@, the result is @dir@. Special cases include:
--
--   * On POSIX: dropping from @"//"@ yields @"/"@.
--   * On Windows: dropping from @"C://"@ results in @"C:/"@.
--
-- Examples:
--
-- >>> f = Path.toString . Path.dropTrailingSeparators . Path.fromString_
-- >>> f "./"
-- "."
--
-- >> f "//"  -- On POSIX
-- "/"
--
{-# INLINE dropTrailingSeparators #-}
dropTrailingSeparators :: OS_PATH_TYPE -> OS_PATH_TYPE
dropTrailingSeparators (OS_PATH arr) =
    OS_PATH (Common.dropTrailingSeparators Common.OS_NAME arr)

-- On windows a share name can also be reported to have a trailing separator,
-- but that is not a valid Path.

-- | Returns 'True' if the path ends with a trailing separator.
--
-- This typically indicates that the path is a directory, though this is not
-- guaranteed in all cases.
--
-- Example:
--
-- >>> Path.hasTrailingSeparator (Path.fromString_ "foo/")
-- True
--
-- >>> Path.hasTrailingSeparator (Path.fromString_ "foo")
-- False
{-# INLINE hasTrailingSeparator #-}
hasTrailingSeparator :: OS_PATH_TYPE -> Bool
hasTrailingSeparator (OS_PATH arr) =
    Common.hasTrailingSeparator Common.OS_NAME arr

-- | Add a trailing path separator to a path if it doesn't already have one.
--
-- Instead of this operation you may want to use 'eqPath' with
-- 'ignoreTrailingSeparators' option.
--
-- This function avoids modifying the path if doing so would change its meaning
-- or make it invalid. For example, on Windows:
--
--   * Adding a separator to "C:" would change it from referring to the current
--     directory on the C: drive to the root directory.
--   * Adding a separator to "\\" could turn it into a UNC share path, which may
--     not be intended.
--   * If the path ends with a colon (e.g., "C:"), a separator is not added.
--
-- This operation typically makes the path behave like an implicit directory path.
{-# INLINE addTrailingSeparator #-}
addTrailingSeparator :: OS_PATH_TYPE -> OS_PATH_TYPE
addTrailingSeparator p@(OS_PATH _arr) =
#ifdef IS_WINDOWS
    if Array.unsafeGetIndexRev 0 _arr == Common.charToWord ':'
    then p
    else unsafeJoin p sep
#else
    unsafeJoin p sep
#endif

    where

    sep = fromJust $ fromString [Common.primarySeparator Common.OS_NAME]

-- Path must not contain null char as system calls treat the path as a null
-- terminated C string. Also, they return null terminated strings as paths.
--
-- XXX Maintain the Array with null termination? To avoid copying the path for
-- null termination when passing to system calls. Path appends will have to
-- handle the null termination.

#ifndef IS_WINDOWS
-- | Checks whether the filepath is valid; i.e., whether the operating system
-- permits such a path for listing or creating files. These validations are
-- operating system specific and file system independent. Throws an exception
-- with a detailed explanation if the path is invalid.
--
-- >>> isValid = isJust . Path.validatePath . Path.encodeString
--
-- Validations:
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
--
-- Other than these there may be maximum path component length and maximum path
-- length restrictions enforced by the OS as well as the filesystem which we do
-- not validate.
--
#else
-- | Checks whether the filepath is valid; i.e., whether the operating system
-- permits such a path for listing or creating files. These validations are
-- operating system specific and file system independent. Throws an exception
-- with a detailed explanation if the path is invalid.
--
-- >>> isValid = isJust . Path.validatePath . Path.encodeString
--
-- General validations:
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
--
-- Windows invalid characters:
--
-- >>> isValid "c::"
-- False
-- >>> isValid "c:\\x:y"
-- False
-- >>> isValid "x*"
-- False
-- >>> isValid "x\ty" -- control characters
-- False
--
-- Windows invalid path components:
--
-- >>> isValid "pRn.txt"
-- False
-- >>> isValid " pRn .txt"
-- False
-- >>> isValid "c:\\x\\pRn"
-- False
-- >>> isValid "c:\\x\\pRn.txt"
-- False
-- >>> isValid "c:\\pRn\\x"
-- False
-- >>> isValid "c:\\ pRn \\x"
-- False
-- >>> isValid "pRn.x.txt"
-- False
--
-- Windows drive root validations:
--
-- >>> isValid "c:"
-- True
-- >>> isValid "c:a\\b"
-- True
-- >>> isValid "c:\\"
-- True
-- >>> isValid "c:\\\\"
-- False
-- >>> isValid "c:\\/"
-- False
-- >>> isValid "c:\\\\x"
-- False
-- >>> isValid "c:\\/x"
-- False
--
-- Mixing path separators:
-- >>> isValid "/x\\y"
-- True
-- >>> isValid "\\/" -- ?
-- True
-- >>> isValid "/\\" -- ?
-- True
-- >>> isValid "\\/x/y" -- ?
-- True
-- >>> isValid "/x/\\y" -- ?
-- True
-- >>> isValid "/x\\/y" -- ?
-- True
--
-- Windows share path validations:
--
-- >>> isValid "\\"
-- True
-- >>> isValid "\\\\"
-- False
-- >>> isValid "\\\\\\"
-- False
-- >>> isValid "\\\\x"
-- False
-- >>> isValid "\\\\x\\"
-- True
-- >>> isValid "\\\\x\\y"
-- True
-- >>> isValid "//x/y"
-- True
-- >>> isValid "\\\\prn\\y"
-- False
-- >>> isValid "\\\\x\\\\"
-- False
-- >>> isValid "\\\\x\\\\x"
-- False
-- >>> isValid "\\\\\\x"
-- False
--
-- Windows short UNC path validations:
--
-- >>> isValid "\\\\?\\c:"
-- False
-- >>> isValid "\\\\?\\c:\\"
-- True
-- >>> isValid "\\\\?\\c:x"
-- False
-- >>> isValid "\\\\?\\c:\\\\" -- XXX validate this
-- False
-- >>> isValid "\\\\?\\c:\\x"
-- True
-- >>> isValid "\\\\?\\c:\\\\\\"
-- False
-- >>> isValid "\\\\?\\c:\\\\x"
-- False
--
-- Windows long UNC path validations:
--
-- >>> isValid "\\\\?\\UnC\\x" -- UnC treated as share name
-- True
-- >>> isValid "\\\\?\\UNC\\x" -- XXX fix
-- False
-- >>> isValid "\\\\?\\UNC\\c:\\x"
-- True
--
-- DOS local/global device namespace
--
-- >>> isValid "\\\\.\\x"
-- True
-- >>> isValid "\\\\??\\x"
-- True
--
-- Other than these there may be maximum path component length and maximum path
-- length restrictions enforced by the OS as well as the filesystem which we do
-- not validate.
--
#endif
validatePath :: MonadThrow m => Array OS_WORD_TYPE -> m ()
validatePath = Common.validatePath Common.OS_NAME

-- | Returns 'True' if the filepath is valid:
--
-- >>> isValidPath = isJust . Path.validatePath
--
isValidPath :: Array OS_WORD_TYPE -> Bool
isValidPath = isJust . validatePath

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
-- per 'validatePath'.
--
{-# INLINE unsafeFromArray #-}
unsafeFromArray :: Array OS_WORD_TYPE -> OS_PATH_TYPE
unsafeFromArray =
#ifndef DEBUG
    OS_PATH . Common.unsafeFromArray
#else
    fromJust . fromArray
#endif

#ifndef IS_WINDOWS
-- | Convert an encoded array of OS_WORD_TYPE into a value of type
-- OS_PATH_TYPE. The path is validated using 'validatePath'.
--
-- Each OS_WORD_TYPE should be encoded such that:
--
-- * The input does not contain a NUL word.
-- * Values from 1-128 are assumed to be ASCII characters.
--
-- Apart from the above, there are no restrictions on the encoding.
--
-- To bypass path validation checks, use 'unsafeFromArray'.
--
-- Throws 'InvalidPath' if 'validatePath' fails on the resulting path.
--
#else
-- | Convert an encoded array of OS_WORD_TYPE into a value of type
-- OS_PATH_TYPE. The path is validated using 'validatePath'.
--
-- Each OS_WORD_TYPE should be encoded such that:
--
-- * The input does not contain a NUL word.
-- * The OS_WORD_TYPE is encoded with little-endian ordering.
-- * Values from 1-128 are assumed to be ASCII characters.
--
-- Apart from the above, there are no restrictions on the encoding.
--
-- To bypass path validation checks, use 'unsafeFromArray'.
--
-- Throws 'InvalidPath' if 'validatePath' fails on the resulting path.
--
#endif
fromArray :: MonadThrow m => Array OS_WORD_TYPE -> m OS_PATH_TYPE
fromArray arr = OS_PATH <$> Common.fromArray Common.OS_NAME arr

-- XXX Should be a Fold instead?

-- | Like 'fromString' but a streaming operation.
--
-- >>> fromString = Path.fromChars . Stream.fromList
--
-- We do not sanitize the path i.e. we do not remove duplicate separators,
-- redundant @.@ segments, trailing separators etc because that would require
-- unnecessary checks and modifications to the path which may not be used ever
-- for any useful purpose, it is only needed for path equality and can be done
-- during the equality check.
--
-- Unicode normalization is not done. If normalization is needed the user can
-- normalize it and then use the 'fromArray' API.
{-# INLINE fromChars #-}
fromChars :: MonadThrow m => Stream Identity Char -> m OS_PATH_TYPE
fromChars s =
    OS_PATH <$> Common.fromChars Common.OS_NAME Unicode.UNICODE_ENCODER s

-- | Create an array from a path string using strict CODEC_NAME encoding. The
-- path is not validated, therefore, it may not be valid according to
-- 'validatePath'.
--
-- Same as @toArray . unsafeFromString@.
encodeString :: [Char] -> Array OS_WORD_TYPE
encodeString =
      Common.unsafeFromChars Unicode.UNICODE_ENCODER
    . Stream.fromList

-- | Like 'fromString' but does not perform any validations mentioned under
-- 'validatePath'. Fails only if unicode encoding fails.
unsafeFromString :: [Char] -> OS_PATH_TYPE
unsafeFromString =
#ifndef DEBUG
      OS_PATH
    . encodeString
#else
    fromJust . fromString
#endif

-- | Encode a Unicode character string to OS_PATH_TYPE using strict CODEC_NAME
-- encoding. The path is validated using 'validatePath'.
--
-- * Throws 'InvalidPath' if 'validatePath' fails on the path
-- * Fails if the stream contains invalid unicode characters
--
fromString :: MonadThrow m => [Char] -> m OS_PATH_TYPE
fromString = fromChars . Stream.fromList

-- | Like fromString but a pure and partial function that throws an
-- 'InvalidPath' exception.
fromString_ :: [Char] -> OS_PATH_TYPE
fromString_ x =
        case fromString x of
            Left e -> throw e
            Right v -> v

-- | Append a separator followed by the supplied string to a path.
--
--  Throws 'InvalidPath' if the resulting path is not a valid path as per
--  'validatePath'.
--
joinStr :: OS_PATH_TYPE -> [Char] -> OS_PATH_TYPE
joinStr (OS_PATH a) b =
    OS_PATH $
        Common.append Common.OS_NAME
            (Common.toString Unicode.UNICODE_DECODER) a (encodeString b)


------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- XXX We can lift the array directly, ByteArray has a lift instance. Does that
-- work better?
--
-- XXX Make this polymorphic and reusable in other modules.

liftPath :: OS_PATH_TYPE -> Q Exp
liftPath p =
    [| unsafeFromString $(lift $ toString p) :: OS_PATH |]

-- | Generates a Haskell expression of type OS_PATH_TYPE from a String. Equivalent
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

#ifdef IS_PORTABLE
-- | Generates a OS_PATH_TYPE type from a quoted literal. Equivalent to using
-- 'fromString' on the static literal.
--
-- >>> Path.toString ([path|/usr/bin|] :: Path)
-- "/usr/bin"
--
#endif
path :: QuasiQuoter
path = mkQ pathE

------------------------------------------------------------------------------
-- Eimination
------------------------------------------------------------------------------

-- | Convert the path to an array.
toArray :: OS_PATH_TYPE -> Array OS_WORD_TYPE
toArray (OS_PATH arr) = arr

-- | Decode the path to a stream of Unicode chars using strict CODEC_NAME decoding.
{-# INLINE toChars #-}
toChars :: Monad m => OS_PATH_TYPE -> Stream m Char
toChars (OS_PATH arr) = Common.toChars Unicode.UNICODE_DECODER arr

-- | Decode the path to a stream of Unicode chars using lax CODEC_NAME decoding.
toChars_ :: Monad m => OS_PATH_TYPE -> Stream m Char
toChars_ (OS_PATH arr) = Common.toChars Unicode.UNICODE_DECODER_LAX arr

-- XXX When showing, append a "/" to dir types?

-- | Decode the path to a Unicode string using strict CODEC_NAME decoding.
toString :: OS_PATH_TYPE -> [Char]
toString = runIdentity . Stream.toList . toChars

-- | Decode the path to a Unicode string using lax CODEC_NAME decoding.
toString_ :: OS_PATH_TYPE -> [Char]
toString_ = runIdentity . Stream.toList . toChars_

-- | Show the path as raw characters without any specific decoding.
--
-- See also: 'readArray'.
--
showArray :: OS_PATH_TYPE -> [Char]
showArray (OS_PATH arr) = show arr

#ifndef IS_WINDOWS
#ifdef IS_PORTABLE
-- | Parse a raw array of bytes as a path type.
--
-- >>> readArray = fromJust . Path.fromArray . read
--
-- >>> arr = Path.encodeString "hello"
-- >>> Path.showArray $ (Path.readArray $ show arr :: Path.Path)
-- "fromList [104,101,108,108,111]"
--
-- See also: 'showArray'.
#endif
readArray :: [Char] -> OS_PATH_TYPE
readArray = fromJust . fromArray . read
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
{-# INLINE AS_OS_CSTRING #-}
AS_OS_CSTRING :: OS_PATH_TYPE -> (OS_CSTRING_TYPE -> IO a) -> IO a
AS_OS_CSTRING p = Array.asCStringUnsafe (toArray p)
#else
-- | Use the path as a pinned CWString. Useful for using a WindowsPath in
-- system calls on Windows.
{-# INLINE AS_OS_CSTRING #-}
AS_OS_CSTRING :: OS_PATH_TYPE -> (OS_CSTRING_TYPE -> IO a) -> IO a
AS_OS_CSTRING p = Array.asCWString (toArray p)
#endif

------------------------------------------------------------------------------
-- Operations on Path
------------------------------------------------------------------------------

#ifndef IS_WINDOWS
-- | A path that is attached to a root e.g. "\/x" or ".\/x" are rooted paths.
-- "\/" is considered an absolute root and "." as a dynamic root. ".." is not
-- considered a root, "..\/x" or "x\/y" are not rooted paths.
--
-- >>> isRooted = Path.isRooted . Path.fromString_
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
isRooted :: OS_PATH_TYPE -> Bool
isRooted (OS_PATH arr) = Common.isRooted Common.OS_NAME arr
#endif

-- | A path that is not attached to a root e.g. @a\/b\/c@ or @..\/b\/c@.
--
-- >>> isUnrooted = not . Path.isRooted
--
-- >>> isUnrooted = Path.isUnrooted . Path.fromString_
--
-- >>> isUnrooted "x"
-- True
-- >>> isUnrooted "x/y"
-- True
-- >>> isUnrooted ".."
-- True
-- >>> isUnrooted "../x"
-- True
--
isUnrooted :: OS_PATH_TYPE -> Bool
isUnrooted = not . isRooted

#ifndef IS_WINDOWS
-- | Like 'join' but does not check if the second path is rooted.
--
-- >>> f a b = Path.toString $ Path.unsafeJoin (Path.fromString_ a) (Path.fromString_ b)
--
-- >>> f "x" "y"
-- "x/y"
-- >>> f "x/" "y"
-- "x/y"
-- >>> f "x" "/y"
-- "x/y"
-- >>> f "x/" "/y"
-- "x/y"
--
{-# INLINE unsafeJoin #-}
unsafeJoin :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
unsafeJoin (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.unsafeAppend
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- If you want to avoid runtime failure use the typesafe
-- Streamly.FileSystem.OS_PATH_TYPE.Seg module.

-- | Append a separator followed by another path to a OS_PATH_TYPE. Fails if
-- the second path is a rooted path. Use 'unsafeJoin' to avoid failure if you
-- know it is ok to append the rooted path.
--
-- >>> f a b = Path.toString $ Path.join a b
--
-- >>> f [path|/usr|] [path|bin|]
-- "/usr/bin"
-- >>> f [path|/usr/|] [path|bin|]
-- "/usr/bin"
-- >>> fails (f [path|/usr|] [path|/bin|])
-- True
--
join :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
join (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | A stricter version of 'join' which requires the first path to be a
-- directory like path i.e. having a trailing separator.
--
-- >>> f a b = Path.toString $ Path.joinDir a b
--
-- >>> fails $ f [path|/usr|] [path|bin|]
-- True
--
joinDir ::
    OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
joinDir
    (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append'
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b
#endif

-- XXX This can be pure, like append.
-- XXX add appendCWString for Windows?

#ifndef IS_WINDOWS
-- | Append a separator and a CString to the Array. This is like 'unsafeJoin'
-- but always inserts a separator between the two paths even if the first path
-- has a trailing separator or second path has a leading separator.
--
joinCStr :: OS_PATH_TYPE -> CString -> IO OS_PATH_TYPE
joinCStr (OS_PATH a) str =
    fmap OS_PATH
        $ Common.appendCString
            Common.OS_NAME a str

-- | Like 'joinCStr' but creates a pinned path.
--
joinCStr' ::
    OS_PATH_TYPE -> CString -> IO OS_PATH_TYPE
joinCStr'
    (OS_PATH a) str =
    fmap OS_PATH
        $ Common.appendCString'
            Common.OS_NAME a str
#endif

-- See unsafeJoinPaths in the Common path module, we need to avoid MonadIo from
-- that to implement this.

-- | Join paths by path separator. Does not check if the paths being appended
-- are rooted or branches. Note that splitting and joining may not give exactly
-- the original path but an equivalent path.
--
-- /Unimplemented/
unsafeJoinPaths :: [OS_PATH_TYPE] -> OS_PATH_TYPE
unsafeJoinPaths = undefined

------------------------------------------------------------------------------
-- Splitting path
------------------------------------------------------------------------------

#ifndef IS_WINDOWS
-- | If a path is rooted then separate the root and the remaining path,
-- otherwise return 'Nothing'. The non-root
-- part is guaranteed to NOT start with a separator.
--
-- Some filepath package equivalent idioms:
--
-- >>> splitDrive = Path.splitRoot
-- >>> joinDrive = Path.unsafeJoin
-- >>> takeDrive = fmap fst . Path.splitRoot
-- >>> dropDrive x = Path.splitRoot x >>= snd
-- >>> hasDrive = isJust . Path.splitRoot
-- >>> isDrive = isNothing . dropDrive
--
-- >>> toList (a,b) = (Path.toString a, fmap Path.toString b)
-- >>> split = fmap toList . Path.splitRoot . Path.fromString_
--
-- >>> split "/"
-- Just ("/",Nothing)
--
-- >>> split "."
-- Just (".",Nothing)
--
-- >>> split "./"
-- Just ("./",Nothing)
--
-- >>> split "/home"
-- Just ("/",Just "home")
--
-- >>> split "//"
-- Just ("//",Nothing)
--
-- >>> split "./home"
-- Just ("./",Just "home")
--
-- >>> split "home"
-- Nothing
--
splitRoot :: OS_PATH_TYPE -> Maybe (OS_PATH_TYPE, Maybe OS_PATH_TYPE)
splitRoot (OS_PATH x) =
    let (a,b) = Common.splitRoot Common.OS_NAME x
     in if Array.null a
        then Nothing
        else if Array.null b
        then Just (OS_PATH a, Nothing)
        else Just (OS_PATH a, Just (OS_PATH b))

-- | Split the path components keeping separators between path components
-- attached to the dir part. Redundant separators are removed, only the first
-- one is kept. Separators are not added either e.g. "." and ".." may not have
-- trailing separators if the original path does not.
--
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath . Path.fromString_
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
splitPath :: Monad m => OS_PATH_TYPE -> Stream m OS_PATH_TYPE
splitPath (OS_PATH a) = fmap OS_PATH $ Common.splitPath Common.OS_NAME a

-- | Split a path into components separated by the path separator. "."
-- components in the path are ignored except when in the leading position.
-- Trailing separators in non-root components are dropped.
--
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath_ . Path.fromString_
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
splitPath_ :: Monad m => OS_PATH_TYPE -> Stream m OS_PATH_TYPE
splitPath_ (OS_PATH a) = fmap OS_PATH $ Common.splitPath_ Common.OS_NAME a
#endif

-- | If the path does not look like a directory then return @Just (Maybe dir,
-- file)@ otherwise return 'Nothing'. The path is not a directory if:
--
-- * the path does not end with a separator
-- * the path does not end with a . or .. component
--
-- Splits a single component path into @Just (Nothing, path)@ if it does not
-- look like a dir.
--
-- >>> toList (a,b) = (fmap Path.toString a, Path.toString b)
-- >>> split = fmap toList . Path.splitFile . Path.fromString_
--
-- >>> split "/"
-- Nothing
--
-- >>> split "."
-- Nothing
--
-- >>> split "/."
-- Nothing
--
-- >>> split ".."
-- Nothing
--
-- >> split "//" -- Posix
-- Nothing
--
-- >>> split "/home"
-- Just (Just "/","home")
--
-- >>> split "./home"
-- Just (Just "./","home")
--
-- >>> split "home"
-- Just (Nothing,"home")
--
-- >>> split "x/"
-- Nothing
--
-- >>> split "x/y"
-- Just (Just "x/","y")
--
-- >>> split "x//y"
-- Just (Just "x//","y")
--
-- >>> split "x/./y"
-- Just (Just "x/./","y")
splitFile :: OS_PATH_TYPE -> Maybe (Maybe OS_PATH_TYPE, OS_PATH_TYPE)
splitFile (OS_PATH a) =
    fmap (bimap (fmap OS_PATH) OS_PATH) $ Common.splitFile Common.OS_NAME a

-- | Split the path into the first component and rest of the path. Treats the
-- entire root or share name, if present, as the first component.
--
-- /Unimplemented/
splitFirst :: OS_PATH_TYPE -> (OS_PATH_TYPE, Maybe OS_PATH_TYPE)
splitFirst (OS_PATH a) =
    bimap OS_PATH (fmap OS_PATH) $ Common.splitHead Common.OS_NAME a

-- | Split the path into the last component and rest of the path. Treats the
-- entire root or share name, if present, as the first component.
--
-- >>> basename = snd . Path.splitLast -- Posix basename
-- >>> dirname = fst . Path.splitLast -- Posix dirname
--
-- /Unimplemented/
splitLast :: OS_PATH_TYPE -> (Maybe OS_PATH_TYPE, OS_PATH_TYPE)
splitLast (OS_PATH a) =
    bimap (fmap OS_PATH) OS_PATH $ Common.splitTail Common.OS_NAME a

#ifndef IS_WINDOWS
-- Note: In the cases of "x.y." and "x.y.." we return no extension rather
-- than ".y." or ".y.." as extensions. That is they considered to have no
-- extension.

-- | Returns @Just(filename, extension)@ if an extension is present otherwise
-- returns 'Nothing'.
--
-- A file name is considered to have an extension if the file name can be
-- split into a non-empty filename followed by the extension separator "."
-- followed by a non-empty extension with at least one character in addition to
-- the extension separator.
-- The shortest suffix obtained by this rule, starting with the
-- extension separator, is returned as the extension and the remaining prefix
-- part as the filename.
--
-- A directory name does not have an extension.
--
-- If you want a @splitExtensions@, you can use splitExtension until the
-- extension returned is Nothing. @dropExtensions@, @isExtensionOf@ can be
-- implemented similarly.
--
-- >>> toList (a,b) = (Path.toString a, Path.toString b)
-- >>> split = fmap toList . Path.splitExtension . Path.fromString_
--
-- >>> split "/"
-- Nothing
--
-- >>> split "."
-- Nothing
--
-- >>> split ".."
-- Nothing
--
-- >>> split "x"
-- Nothing
--
-- >>> split "/x"
-- Nothing
--
-- >>> split "x/"
-- Nothing
--
-- >>> split "./x"
-- Nothing
--
-- >>> split "x/."
-- Nothing
--
-- >>> split "x/y."
-- Nothing
--
-- >>> split "/x.y"
-- Just ("/x",".y")
--
-- >>> split "/x.y."
-- Nothing
--
-- >>> split "/x.y.."
-- Nothing
--
-- >>> split "x/.y"
-- Nothing
--
-- >>> split ".x"
-- Nothing
--
-- >>> split "x."
-- Nothing
--
-- >>> split ".x.y"
-- Just (".x",".y")
--
-- >>> split "x/y.z"
-- Just ("x/y",".z")
--
-- >>> split "x.y.z"
-- Just ("x.y",".z")
--
-- >>> split "x..y"
-- Just ("x.",".y")
--
-- >>> split "..."
-- Nothing
--
-- >>> split "..x"
-- Just (".",".x")
--
-- >>> split "...x"
-- Just ("..",".x")
--
-- >>> split "x/y.z/"
-- Nothing
--
-- >>> split "x/y"
-- Nothing
--
splitExtension :: OS_PATH_TYPE -> Maybe (OS_PATH_TYPE, OS_PATH_TYPE)
splitExtension (OS_PATH a) =
    fmap (bimap OS_PATH OS_PATH) $ Common.splitExtension Common.OS_NAME a
#endif

-- | Take the extension of a file if it has one.
--
-- >>> takeExtension = fmap snd . Path.splitExtension
-- >>> hasExtension = isJust . Path.splitExtension
--
-- >>> fmap Path.toString $ Path.takeExtension [path|/home/user/file.txt|]
-- Just ".txt"
--
-- See 'splitExtension' for more examples.
takeExtension :: OS_PATH_TYPE -> Maybe OS_PATH_TYPE
takeExtension = fmap snd . splitExtension

-- | Drop the extension of a file if it has one.
--
-- >>> dropExtension p = maybe p fst $ Path.splitExtension p
--
-- >>> Path.toString $ Path.dropExtension [path|/home/user/file.txt|]
-- "/home/user/file"
--
dropExtension :: OS_PATH_TYPE -> OS_PATH_TYPE
dropExtension orig@(OS_PATH a) =
    maybe orig (OS_PATH . fst) $ Common.splitExtension Common.OS_NAME a

-- | Add an extension to a file path. If a non-empty extension does not start
-- with a leading dot then a dot is inserted, otherwise the extension is
-- concatenated with the path.
--
-- It is an error to add an extension to a path with a trailing separator.
--
-- /Unimplemented/
addExtension :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
addExtension (OS_PATH _a) = undefined

-- /Unimplemented/
replaceExtension :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
replaceExtension (OS_PATH _a) = undefined

------------------------------------------------------------------------------
-- Path View
------------------------------------------------------------------------------

-- | Extracts the file name component (with extension) from a OS_PATH_TYPE, if
-- present.
--
-- >>> takeFileName = fmap snd . Path.splitFile
-- >>> replaceDirectory p x = fmap (flip Path.join x) (takeFileName p)
--
-- >>> fmap Path.toString $ Path.takeFileName [path|/home/user/file.txt|]
-- Just "file.txt"
-- >>> fmap Path.toString $ Path.takeFileName [path|/home/user/|]
-- Nothing
--
-- See 'splitFile' for more examples.
--
takeFileName :: OS_PATH_TYPE -> Maybe OS_PATH_TYPE
takeFileName = fmap snd . splitFile

-- | Extracts the file name dropping the extension, if any, from a
-- OS_PATH_TYPE.
--
-- >>> takeFileBase = fmap Path.dropExtension . Path.takeFileName
--
-- >>> fmap Path.toString $ Path.takeFileBase [path|/home/user/file.txt|]
-- Just "file"
-- >>> fmap Path.toString $ Path.takeFileBase [path|/home/user/file|]
-- Just "file"
-- >>> fmap Path.toString $ Path.takeFileBase [path|/home/user/.txt|]
-- Just ".txt"
-- >>> fmap Path.toString $ Path.takeFileBase [path|/home/user/|]
-- Nothing
--
-- See 'splitFile' for more examples.
--
takeFileBase :: OS_PATH_TYPE -> Maybe OS_PATH_TYPE
takeFileBase = fmap dropExtension . takeFileName

-- | Returns the parent directory of the given OS_PATH_TYPE, if any.
--
-- >>> takeDirectory x = Path.splitFile x >>= fst
-- >>> replaceFileName p x = fmap (flip Path.join x) (takeDirectory p)
--
-- To get an equivalent to takeDirectory from filepath use
-- 'dropTrailingSeparators' on the result.
--
-- >>> fmap Path.toString $ Path.takeDirectory [path|/home/user/file.txt|]
-- Just "/home/user/"
-- >>> fmap Path.toString $ Path.takeDirectory [path|file.txt|]
-- Nothing
--
takeDirectory :: OS_PATH_TYPE -> Maybe OS_PATH_TYPE
takeDirectory x = splitFile x >>= fst

------------------------------------------------------------------------------
-- Path equality
------------------------------------------------------------------------------

#ifndef IS_WINDOWS
-- | Default equality check configuration.
--
-- >>> :{
-- eqCfg =
--       Path.ignoreTrailingSeparators False
--     . Path.ignoreCase False
--     . Path.allowRelativeEquality False
-- :}
--
#else
-- | Default equality check configuration.
--
-- >>> :{
-- eqCfg =
--       Path.ignoreTrailingSeparators False
--     . Path.ignoreCase True
--     . Path.allowRelativeEquality False
-- :}
--
#endif
eqCfg :: EqCfg
eqCfg = Common.EqCfg
    { _ignoreTrailingSeparators = False
    , _allowRelativeEquality = False
#ifndef IS_WINDOWS
    , _ignoreCase = False
#else
    , _ignoreCase = True
#endif
    }

-- | When set to 'False' (default):
--
-- >>> cfg = Path.ignoreTrailingSeparators False
-- >>> eq a b = Path.eqPath cfg (Path.fromString_ a) (Path.fromString_ b)
--
-- >>> eq "x/"  "x"
-- False
--
-- When set to 'True':
--
-- >>> cfg = Path.ignoreTrailingSeparators True
-- >>> eq a b = Path.eqPath cfg (Path.fromString_ a) (Path.fromString_ b)
--
-- >>> eq "x/"  "x"
-- True
--
-- /Default/: False
ignoreTrailingSeparators :: Bool -> EqCfg -> EqCfg
ignoreTrailingSeparators val conf = conf { _ignoreTrailingSeparators = val }

-- | When set to 'False', comparison is case sensitive.
--
-- /Posix Default/: False
--
-- /Windows Default/: True
ignoreCase :: Bool -> EqCfg -> EqCfg
ignoreCase val conf = conf { _ignoreCase = val }

-- Note: ignoreLeadingDot or similar names are not good because we want to
-- convey that when it is False "./x" and "./x" are not strictly equal.
-- Similarly, "treatDotRootsEqual" has a problem with the "./x" and "x"
-- comparison, there is not dor root in the second path.

-- | Allow relative paths to be treated as equal. When this is 'False' relative
-- paths will never match even if they are literally equal e.g. "./x" will not
-- match "./x" because the meaning of "." in both cases could be different
-- depending on what the user meant by current directory in each case.
--
-- When set to 'False' (default):
--
-- >>> cfg = Path.allowRelativeEquality False
-- >>> eq a b = Path.eqPath cfg (Path.fromString_ a) (Path.fromString_ b)
-- >>> eq "."  "."
-- False
-- >>> eq "./x"  "./x"
-- False
-- >>> eq "./x"  "x"
-- False
--
-- When set to 'False' (default):
--
-- >>> cfg = Path.allowRelativeEquality True
-- >>> eq a b = Path.eqPath cfg (Path.fromString_ a) (Path.fromString_ b)
-- >>> eq "."  "."
-- True
-- >>> eq "./x"  "./x"
-- True
-- >>> eq "./x"  "x"
-- True
--
-- >>> eq "./x"  "././x"
-- True
--
-- /Default/: False
allowRelativeEquality :: Bool -> EqCfg -> EqCfg
allowRelativeEquality val conf = conf { _allowRelativeEquality = val }

#ifndef IS_WINDOWS
-- | Checks whether two paths are logically equal. This function takes a
-- configuration modifier to customize the notion of equality. For using the
-- default configuration pass 'id' as the modifier. For details about the
-- defaults, see 'EqCfg'.
--
-- eqPath performs some normalizations on the paths before comparing them,
-- specifically it drops redundant path separators between path segments and
-- redundant "\/.\/" components between segments.
--
-- Default config options use strict equality, for strict equality both the
-- paths must be absolute or both must be path segments without a leading root
-- component (e.g. x\/y). Also, both must be files or both must be directories.
--
-- In addition to the default config options, the following equality semantics
-- are used:
--
-- * An absolute path and a path relative to "." may be equal depending on the
-- meaning of ".", however this routine treats them as unequal, it does not
-- resolve the "." to a concrete path.
--
-- * Two paths having ".." components may be equal after processing the ".."
-- components even if we determined them to be unequal. However, if we
-- determined them to be equal then they must be equal.
--
-- Using default config with case sensitive comparision, if eqPath returns
-- equal then the paths are definitely equal, if it returns unequal then the
-- paths may still be equal under some relaxed equality criterion.
--
-- >>> :{
--  eq a b = Path.eqPath id (Path.fromString_ a) (Path.fromString_ b)
-- :}
--
-- >>> eq "x"  "x"
-- True
-- >>> eq ".."  ".."
-- True
--
-- Non-trailing separators and non-leading "." segments are ignored:
--
-- >>> eq "/x"  "//x"
-- True
-- >>> eq "x//y"  "x/y"
-- True
-- >>> eq "x/./y"  "x/y"
-- True
-- >>> eq "x/y/."  "x/y"
-- True
--
-- Leading dot, relative paths are not equal by default:
--
-- >>> eq "."  "."
-- False
-- >>> eq "./x"  "./x"
-- False
-- >>> eq "./x"  "x"
-- False
--
-- Trailing separators are significant by default:
--
-- >>> eq "x/"  "x"
-- False
--
-- Match is case sensitive by default:
--
-- >>> eq "x"  "X"
-- False
--
eqPath :: (EqCfg -> EqCfg) -> OS_PATH_TYPE -> OS_PATH_TYPE -> Bool
eqPath cfg (OS_PATH a) (OS_PATH b) =
    Common.eqPath Unicode.UNICODE_DECODER
        Common.OS_NAME (cfg eqCfg) a b
#endif

-- | Check two paths for byte level equality. This is the most strict path
-- equality check.
--
-- >>> eqPath a b = Path.eqPathBytes (Path.fromString_ a) (Path.fromString_ b)
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
eqPathBytes :: OS_PATH_TYPE -> OS_PATH_TYPE -> Bool
eqPathBytes (OS_PATH a) (OS_PATH b) = Common.eqPathBytes a b

-- | Convert the path to an equivalent but standard format for reliable
-- comparison. This can be implemented if required. Usually, the equality
-- operations should be enough and this may not be needed.
--
-- /Unimplemented/
normalize :: EqCfg -> OS_PATH_TYPE -> OS_PATH_TYPE
normalize _cfg (OS_PATH _a) = undefined
