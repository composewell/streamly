{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.FileSystem.Path.Common
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.FileSystem.Path.Common
    (
    -- * Path Types
      File (..)
    , Dir (..)
    , Abs (..)
    , Rel (..)
    , PathException (..)

    -- * Conversions
    , IsPath (..)
    , adapt

    -- * Construction
    , fromChunk
    , unsafeFromChunk
    , fromString
    -- , fromChars

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
    -- , toChars

    -- * Operations
    -- Do we need to export the separator functions? They are not essential if
    -- operations to split and combine paths are provided. If someone wants to
    -- work on paths at low level then they know what they are.
    -- , primarySeparator
    -- , isSeparator
    , dropTrailingSeparators
    , isRelative
    , isAbsolute

    , append
    , unsafeAppend
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
-- import qualified Streamly.Internal.Unicode.Stream as Unicode

import Prelude hiding (abs)

data OS = Windows | Posix

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Exceptions thrown by path operations.
data PathException =
    InvalidPath String
  | InvalidAbsPath String
  | InvalidRelPath String
  | InvalidFilePath String
  | InvalidDirPath String
    deriving (Show,Eq)

instance Exception PathException

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- XXX Do we need a type for file or dir Name as names cannot have the
-- separator char and there may be other restrictions on names? For example,
-- length restriction.  A file name cannot be "." or "..". We can use the types
-- "File Name" and "Dir Name" to represent names. Also, file systems may put
-- limits on names. Can have an IsName type class with members Name, (File
-- Name), (Dir Name).

-- | A type representing a file path.
newtype File a = File a

-- | A type representing a directory path.
newtype Dir a = Dir a

-- | A type representing absolute paths.
newtype Abs a = Abs a

-- | A type representing relative paths.
newtype Rel a = Rel a

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- XXX call it IsBase? It is a more abstract concept and can be used for URLs
-- as well, but those can also be called paths. But if we make it more abstract
-- then File/Dir will have to be called something like Leaf/Branch which will
-- become more obscure.

-- | A member of 'IsPath' knows how to convert to and from the base path type.
class IsPath f a where
    -- | Like 'fromPath' but does not check the properties of 'Path'. The user
    -- is responsible to maintain the invariants mentioned in the definition of
    -- 'Path' type otherwise surprising behavior may result.
    --
    -- Provides performance and simplicity when we know that the properties of
    -- the path are already verified, for example, when we get the path from
    -- the file system or the OS APIs.
    unsafeFromPath :: a -> f a

    -- | Convert a raw 'Path' to other forms of well-typed paths. It may fail
    -- if the path does not satisfy the properties of the target type.
    --
    -- Path components may have limits.
    -- Total path length may have a limit.
    fromPath :: MonadThrow m => a -> m (f a)

    -- | Convert a well-typed path to a raw 'Path'. Never fails.
    toPath :: f a -> a

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
unsafeFromChunk :: Array Word8 -> Array a
-- XXX add asserts to check safety
unsafeFromChunk = Array.castUnsafe

-- | On Posix it may fail if the byte array contains null characters. On
-- Windows the array passed must be a multiple of 2 bytes as the underlying
-- representation uses 'Word16'.
--
-- Throws 'InvalidPath'.
fromChunk :: (MonadThrow m, Unbox a) => Array Word8 -> m (Array a)
fromChunk arr =
    case Array.cast arr of
        Nothing ->
            -- XXX Windows only message.
            throwM
                $ InvalidPath
                $ "Encoded path length " ++ show (Array.byteLength arr)
                    ++ " is not a multiple of 16-bit."
        Just x -> pure x

-- | Convert 'Path' to an array of bytes.
toChunk :: Array a -> Array Word8
toChunk = Array.asBytes

-- | Encode a Unicode string to 'Path' using strict UTF-8 encoding on Posix.
-- On Posix it may fail if the stream contains null characters.
-- TBD: Use UTF16LE on Windows.
fromString :: MonadThrow m =>
    (Stream m Char -> m (Array a)) -> [Char] -> m (Array a)
fromString f = f . Stream.fromList

-- | Decode the path to a Unicode string using strict UTF-8 decoding on Posix.
-- TBD: Use UTF16LE on Windows.
toString :: (Array a -> Stream Identity Char) -> Array a -> [Char]
toString f = runIdentity . Stream.toList . f

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
dropTrailingSeparators :: (Unbox a, Integral a) => OS -> Array a -> Array a
dropTrailingSeparators os =
    dropTrailingBy (isSeparator os . wordToChar)

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
isAbsolute :: (Unbox a, Integral a) => OS -> Array a -> Bool
isAbsolute Posix a =
    -- Assuming path is not empty.
    isSeparator Posix (wordToChar (Array.getIndexUnsafe 0 a))
isAbsolute Windows a = isAbsoluteDrive a || isAbsoluteUNC a

isRelative :: (Unbox a, Integral a) => OS -> Array a -> Bool
isRelative os = not . isAbsolute os

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
withAppendCheck :: (Unbox b, Integral b) =>
    OS -> (Array b -> String) -> Array b -> a -> a
withAppendCheck Posix toStr arr f =
    if isAbsolute Posix arr
    then error $ "append: cannot append absolute path " ++ toStr arr
    else f
withAppendCheck Windows toStr arr f =
    if isAbsoluteInDrive arr
    then error $ "append: cannot append drive absolute path " ++ toStr arr
    else if hasDrive arr
    then error $ "append: cannot append path with drive " ++ toStr arr
    else if isAbsoluteUNC arr
    then error $ "append: cannot append absolute UNC path " ++ toStr arr
    else f

-- | Does not check if any of the path is empty or if the second path is
-- absolute.
{-# INLINE unsafeAppend #-}
unsafeAppend :: (Unbox a, Integral a) =>
    OS -> (Array a -> String) -> Array a -> Array a -> Array a
unsafeAppend os toStr a b =
    assert (withAppendCheck os toStr b True) (doAppend os a b)

{-# INLINE append #-}
append :: (Unbox a, Integral a) =>
    OS -> (Array a -> String) -> Array a -> Array a -> Array a
append os toStr a b =
    withAppendCheck os toStr b (doAppend os a b)

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Rel path.

{-
-- | Append a 'Rel' 'Path' to a 'Dir' 'Path'. Never fails.
--
-- Also see 'append'.
{-# INLINE appendRel #-}
appendRel :: (IsPath a (Dir p), IsPath b, IsPath (a b)) =>
    (a (Dir p)) -> Rel b -> a b
appendRel a (Rel b) = unsafeFromPath $ unsafeAppend (toPath a) (toPath b)
-}
