{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.FileSystem.Path.Posix
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.FileSystem.Path.Posix
    (
    {-
    -- * OS
      OS (..)

    -- * Path Types
    , Path (..)
    , File
    , Dir
    , Abs
    , Rel

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Construction
    , fromChunk
    , unsafeFromChunk
    , fromString
    , fromChars

    -- * Statically Verified Literals
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
    , toString
    , toChars

    -- * Operations
    -- Do we need to export the separator functions? They are not essential if
    -- operations to split and combine paths are provided. If someone wants to
    -- work on paths at low level then they know what they are.
    -- , primarySeparator
    -- , isSeparator
    , append
    , unsafeAppend
    , appendRel
    , dropTrailingSeparators
    , isRelativeRaw
    , isAbsoluteRaw
    -}
    )
where

#include "assert.hs"

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.Char (ord, isAlpha)
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Data.Word (Word16)
#endif
import GHC.Base (unsafeChr)
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Stream (Stream)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Streamly.Internal.FileSystem.Path.Common -- (IsPath(..), File(..), Dir)
import qualified Streamly.Internal.FileSystem.Path.Common as Common

import Prelude hiding (abs)

newtype PosixPath = PosixPath (Array Word8)

{-
instance IsPath PosixPath PosixPath where
    unsafeFromPath = id
    fromPath = pure
    toPath = id
-}

instance IsPath File PosixPath where
    unsafeFromPath p = File p
    fromPath p = pure (File p)
    toPath (File p) = p

instance IsPath Dir PosixPath where
    unsafeFromPath p = Dir p
    fromPath p = pure (Dir p)
    toPath (Dir p) = p

instance IsPath Abs PosixPath where
    unsafeFromPath p = Abs p
    fromPath p = pure (Abs p)
    toPath (Abs p) = p

instance IsPath Rel PosixPath where
    unsafeFromPath p = Rel p
    fromPath p = pure (Rel p)
    toPath (Rel p) = p

instance IsPath Abs (File PosixPath) where
    unsafeFromPath p = Abs p
    fromPath p = pure (Abs p)
    toPath (Abs (File p)) = File p

instance IsPath Abs (Dir PosixPath) where
    unsafeFromPath p = Abs p
    fromPath p = pure (Abs p)
    toPath (Abs (Dir p)) = Dir p

instance IsPath Rel (File PosixPath) where
    unsafeFromPath p = Rel p
    fromPath p = pure (Rel p)
    toPath (Rel (File p)) = File p

instance IsPath Rel (Dir PosixPath) where
    unsafeFromPath p = Rel p
    fromPath p = pure (Rel p)
    toPath (Rel (Dir p)) = Dir p

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency.

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one.
adapt :: (MonadThrow m, IsPath a p, IsPath b p) => a p -> m (b p)
adapt p = fromPath $ toPath p

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
unsafeFromChunk arr = PosixPath (Array.castUnsafe arr)

-- | On Posix it may fail if the byte array contains null characters. On
-- Windows the array passed must be a multiple of 2 bytes as the underlying
-- representation uses 'Word16'.
--
-- Throws 'InvalidPath'.
fromChunk :: MonadThrow m => Array Word8 -> m PosixPath
fromChunk arr =
    case Array.cast arr of
        Nothing ->
            -- XXX Windows only message.
            throwM
                $ InvalidPath
                $ "Encoded path length " ++ show (Array.byteLength arr)
                    ++ " is not a multiple of 16-bit."
        Just x -> pure (PosixPath x)

{-
-- | Convert 'Path' to an array of bytes.
toChunk :: Path -> Array Word8
toChunk (Path arr) = Array.asBytes arr

-- | Encode a Unicode char stream to 'Path' using strict UTF-8 encoding on
-- Posix. On Posix it may fail if the stream contains null characters.
-- TBD: Use UTF16LE on Windows.
--
-- Unicode normalization is not done. If normalization is needed the user can
-- normalize it and use the fromChunk API.
fromChars :: MonadThrow m => Stream Identity Char -> m Path
fromChars s =
    let n = runIdentity $ Stream.fold Fold.length s
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
     in pure $ Path (Array.fromPureStreamN n (Unicode.encodeUtf16le' s))
#else
     in pure $ Path (Array.fromPureStreamN n (Unicode.encodeUtf8' s))
#endif

-- | Decode the path to a stream of Unicode chars using strict UTF-8 decoding
-- on Posix.
-- TBD: Use UTF16LE on Windows.
toChars :: Monad m => Path -> Stream m Char
toChars (Path arr) =
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    Unicode.decodeUtf16le' $ Array.read arr
#else
    Unicode.decodeUtf8' $ Array.read arr
#endif

-- | Encode a Unicode string to 'Path' using strict UTF-8 encoding on Posix.
-- On Posix it may fail if the stream contains null characters.
-- TBD: Use UTF16LE on Windows.
fromString :: MonadThrow m => [Char] -> m Path
fromString = fromChars . Stream.fromList

-- | Decode the path to a Unicode string using strict UTF-8 decoding on Posix.
-- TBD: Use UTF16LE on Windows.
toString :: Path -> [Char]
toString = runIdentity . Stream.toList . toChars

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX Build these on top of the str quasiquoter so that we get the
-- interpolation for free.

-- | Generates a 'Path' type from an interpolated string literal.
--
-- /Unimplemented/
path :: QuasiQuoter
path = undefined

-- | Generates an @Abs Path@ type from an interpolated string literal.
--
-- /Unimplemented/
abs :: QuasiQuoter
abs = undefined

-- | Generates an @Rel Path@ type from an interpolated string literal.
--
-- /Unimplemented/
rel :: QuasiQuoter
rel = undefined

-- | Generates an @Dir Path@ type from an interpolated string literal.
--
-- /Unimplemented/
dir :: QuasiQuoter
dir = undefined

-- | Generates an @File Path@ type from an interpolated string literal.
--
-- /Unimplemented/
file :: QuasiQuoter
file = undefined

-- | Generates an @Abs (Dir Path)@ type from an interpolated string literal.
--
-- /Unimplemented/
absdir :: QuasiQuoter
absdir = undefined

-- | Generates an @Rel (Dir Path)@ type from an interpolated string literal.
--
-- /Unimplemented/
reldir :: QuasiQuoter
reldir = undefined

-- | Generates an @Abs (File Path)@ type from an interpolated string literal.
--
-- /Unimplemented/
absfile :: QuasiQuoter
absfile = undefined

-- | Generates an @Rel (File Path)@ type from an interpolated string literal.
--
-- /Unimplemented/
relfile :: QuasiQuoter
relfile = undefined

------------------------------------------------------------------------------
-- Statically Verified Strings
------------------------------------------------------------------------------

-- | Generates a 'Path' type.
--
-- /Unimplemented/
mkPath :: String -> Q Exp
mkPath = undefined

-- | Generates an @Abs Path@ type.
--
-- /Unimplemented/
mkAbs :: String -> Q Exp
mkAbs = undefined

-- | Generates an @Rel Path@ type.
--
-- /Unimplemented/
mkRel :: String -> Q Exp
mkRel = undefined

-- | Generates an @Dir Path@ type.
--
-- /Unimplemented/
mkDir :: String -> Q Exp
mkDir = undefined

-- | Generates an @File Path@ type.
--
-- /Unimplemented/
mkFile :: String -> Q Exp
mkFile = undefined

-- | Generates an @Abs (Dir Path)@ type.
--
-- /Unimplemented/
mkAbsDir :: String -> Q Exp
mkAbsDir = undefined

-- | Generates an @Rel (Dir Path)@ type.
--
-- /Unimplemented/
mkRelDir :: String -> Q Exp
mkRelDir = undefined

-- | Generates an @Abs (File Path)@ type.
--
-- /Unimplemented/
mkAbsFile :: String -> Q Exp
mkAbsFile = undefined

-- | Generates an @Rel (File Path)@ type.
--
-- /Unimplemented/
mkRelFile :: String -> Q Exp
mkRelFile = undefined

------------------------------------------------------------------------------
-- Operations
------------------------------------------------------------------------------

posixSeparator :: Char
posixSeparator = '/'

windowsSeparator :: Char
windowsSeparator = '\\'

-- XXX We can use Enum type class to include the Char type as well so that the
-- functions can work on Array Word8/Word16/Char but that may be slow.

-- | Unsafe, may tructate to shorter word types, can only be used safely for
-- characters that fit in the given word size.
charToWord :: Integral a => Char -> a
charToWord c =
    let n = ord c
     in assert (n <= 255) (fromIntegral n)

-- | Unsafe, should be a valid character.
wordToChar :: Integral a => a -> Char
wordToChar = unsafeChr . fromIntegral

-- | Index a word in an array and convert it to Char.
unsafeIndexChar :: (Unbox a, Integral a) => Int -> Array a -> Char
unsafeIndexChar i a = wordToChar (Array.getIndexUnsafe i a)

-- Portable definition for exporting.

-- | Primary path separator character, @/@ on Posix and @\\@ on Windows.
-- Windows supports @/@ too as a separator. Please use 'isSeparator' for
-- testing if a char is a separator char.
_primarySeparator :: Char
_primarySeparator = posixSeparator

------------------------------------------------------------------------------
-- Path parsing utilities
------------------------------------------------------------------------------

-- | On Posix only @/@ is a path separator but in windows it could be either
-- @/@ or @\\@.
{-# INLINE isSeparator #-}
isSeparator :: OS -> Char -> Bool
isSeparator Windows c = (c == windowsSeparator) || (c == posixSeparator)
isSeparator Posix c = (c == posixSeparator)

{-# INLINE isSeparatorWord #-}
isSeparatorWord :: Integral a => OS -> a -> Bool
isSeparatorWord os = isSeparator os . wordToChar

countTrailingBy :: Unbox a => (a -> Bool) -> Array a -> Int
countTrailingBy p arr =
      runIdentity
    $ Stream.fold Fold.length
    $ Stream.takeWhile p
    $ Array.readRev arr

-- | If the path is @//@ the result is @/@. If it is @a//@ then the result is
-- @a@.
dropTrailingBy :: Unbox a => (a -> Bool) -> Array a -> Array a
dropTrailingBy p arr@(Array barr start end) =
    if end - start > 0
    then
        let n = countTrailingBy p arr
         in Array barr start (max 1 (end - n))
    else arr

-- | If the path is @//@ the result is @/@. If it is @a//@ then the result is
-- @a@.
{-# INLINE dropTrailingSeparators #-}
dropTrailingSeparators :: OS -> Path -> Path
dropTrailingSeparators os (Path arr) =
    Path (dropTrailingBy (isSeparator os . wordToChar) arr)

-- | @C:...@
hasDrive :: (Unbox a, Integral a) => Array a -> Bool
hasDrive a =
    if Array.byteLength a < 2
    then False
    -- Check colon first for quicker return
    else if (unsafeIndexChar 1 a /= ':')
    then False
    -- XXX If we found a colon anyway this cannot be a valid path unless it has
    -- a drive prefix. colon is not a valid path character.
    -- XXX check isAlpha perf
    else if not (isAlpha (unsafeIndexChar 0 a))
    then False -- XXX if we are here it is not a valid path
    else True

-- | On windows, the path starts with a separator.
isAbsoluteInDrive :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteInDrive a =
    -- Assuming the path is not empty.
    isSeparator Windows (wordToChar (Array.getIndexUnsafe 0 a))

-- | @C:\...@
isAbsoluteDrive :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteDrive a =
    if Array.byteLength a < 3
    then False
    -- Check colon first for quicker return
    else if (unsafeIndexChar 1 a /= ':')
    then False
    else if not (isSeparator Windows (unsafeIndexChar 2 a))
    then False
    -- XXX If we found a colon anyway this cannot be a valid path unless it has
    -- a drive prefix. colon is not a valid path character.
    -- XXX check isAlpha perf
    else if not (isAlpha (unsafeIndexChar 0 a))
    then False -- XXX if we are here it is not a valid path
    else True

-- | @\\\\...@
isAbsoluteUNC :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteUNC a =
    if Array.byteLength a < 2
    then False
    else if (unsafeIndexChar 0 a /= '\\')
    then False
    else if (unsafeIndexChar 1 a /= '\\')
    then False
    else True

-- | On Posix, a path starting with a separator is an absolute path.
--
-- On Windows:
-- * @C:\\@ local absolute
-- * @C:@ local relative
-- * @\\@ local relative to current drive root
-- * @\\\\@ UNC network path
-- * @\\\\?\\C:\\@ Long UNC local path
-- * @\\\\?\\UNC\\@ Long UNC server path
-- * @\\\\.\\@ DOS local device namespace
-- * @\\\\??\\@ DOS global namespace
isAbsoluteRaw :: (Unbox a, Integral a) => OS -> Array a -> Bool
isAbsoluteRaw Posix a =
    -- Assuming path is not empty.
    isSeparator Posix (wordToChar (Array.getIndexUnsafe 0 a))
isAbsoluteRaw Windows a = isAbsoluteDrive a || isAbsoluteUNC a

isRelativeRaw :: (Unbox a, Integral a) => OS -> Array a -> Bool
isRelativeRaw os = not . isAbsoluteRaw os

------------------------------------------------------------------------------
-- Operations of Path
------------------------------------------------------------------------------

-- XXX This can be generalized to an Array intersperse operation

{-# INLINE doAppend #-}
doAppend :: (Unbox a, Integral a) => OS -> Array a -> Array a -> Array a
doAppend os a b = unsafePerformIO $ do
    let lenA = Array.byteLength a
        lenB = Array.byteLength b
    assertM (lenA /= 0 && lenB /= 0)
    assertM (countTrailingBy (isSeparatorWord os) a == 0)
    let len = lenA + 1 + lenB
    arr <- MutArray.new len
    arr1 <- MutArray.spliceUnsafe arr (Array.unsafeThaw a)
    arr2 <- MutArray.snocUnsafe arr1 (charToWord posixSeparator)
    arr3 <- MutArray.spliceUnsafe arr2 (Array.unsafeThaw b)
    return (Array.unsafeFreeze arr3)

{-# INLINE withAppendCheck #-}
withAppendCheck :: OS -> Path -> a -> a
withAppendCheck Posix p2@(Path arr) f =
    if isAbsoluteRaw Posix arr
    then error $ "append: cannot append absolute path " ++ toString p2
    else f
withAppendCheck Windows p2@(Path arr) f =
    if isAbsoluteInDrive arr
    then error $ "append: cannot append drive absolute path " ++ toString p2
    else if hasDrive arr
    then error $ "append: cannot append path with drive " ++ toString p2
    else if isAbsoluteUNC arr
    then error $ "append: cannot append absolute UNC path " ++ toString p2
    else f

-- | Does not check if any of the path is empty or if the second path is
-- absolute.
{-# INLINE unsafeAppendOS #-}
unsafeAppendOS :: OS -> Path -> Path -> Path
unsafeAppendOS os (Path a) p2@(Path b) =
    assert (withAppendCheck os p2 True) (Path $ doAppend os a b)

{-# INLINE unsafeAppend #-}
unsafeAppend :: Path -> Path -> Path
unsafeAppend = unsafeAppendOS currentOS

{-# INLINE appendOS #-}
appendOS :: OS -> Path -> Path -> Path
appendOS os (Path a) p2@(Path b) =
    withAppendCheck os p2 (Path $ doAppend os a b)

-- | Append a 'Path' to another. Fails if the second path is absolute.
--
-- Also see 'appendRel'.
append :: Path -> Path -> Path
append = appendOS currentOS

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Rel path.

-- | Append a 'Rel' 'Path' to a 'Dir' 'Path'. Never fails.
--
-- Also see 'append'.
{-# INLINE appendRel #-}
appendRel :: (IsPath (a (Dir Path)), IsPath b, IsPath (a b)) =>
    (a (Dir Path)) -> Rel b -> a b
appendRel a (Rel b) = unsafeFromPath $ unsafeAppend (toPath a) (toPath b)
-}
