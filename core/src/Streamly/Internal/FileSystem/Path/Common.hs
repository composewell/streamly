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
    -- * Types
      OS (..)
    -- * Conversions
    , IsPath (..)

    -- * Construction
    , fromChunk
    , unsafeFromChunk
    , posixFromChars
    , posixFromString
    , unsafePosixFromString

    -- * Quasiquoters
    , mkQ

    -- * Elimination
    , toChunk
    , toString
    , posixToChars
    , posixToString

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

import Control.Monad.Catch (MonadThrow(..))
import Data.Char (ord, isAlpha)
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Stream (Stream)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Streamly.Internal.Data.Path

import Prelude hiding (abs)

data OS = Windows | Posix

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

unsafePosixFromChars :: Stream Identity Char -> Array Word8
unsafePosixFromChars s =
    let n = runIdentity $ Stream.fold Fold.length s
     in Array.fromPureStreamN n (Unicode.encodeUtf8' s)

unsafePosixFromString :: String -> Array Word8
unsafePosixFromString = unsafePosixFromChars . Stream.fromList

-- XXX Need Fold.Tee3, Tee4 etc for better efficiency composition.
-- XXX Sanitize the path - remove duplicate separators, . segments, trailing .
-- XXX Writing a custom fold for parsing a Posix path may be better for
-- efficient bulk parsing when needed. We need the same code to validate a
-- Chunk where we do not need to create an array.
posixFromChars :: MonadThrow m => Stream Identity Char -> m (Array Word8)
posixFromChars s =
    let lengths = Fold.tee Fold.length (Fold.takeEndBy_ (== '\0') Fold.length)
        (n, n1) = runIdentity $ Stream.fold lengths s
        arr = Array.fromPureStreamN n (Unicode.encodeUtf8' s)
        sample = Stream.takeWhile (/= '\0') s
     in
        if n1 < n
        then throwM $ InvalidPath $ "Path contains a NULL char at position: "
                ++ show n1 ++ " after " ++ runIdentity (Stream.toList sample)
        else pure arr

-- | Encode a Unicode string to 'Path' using strict UTF-8 encoding on Posix.
-- On Posix it may fail if the stream contains null characters.
fromString ::
    (Stream Identity Char -> m (Array a)) -> [Char] -> m (Array a)
fromString f = f . Stream.fromList

posixFromString :: MonadThrow m => [Char] -> m (Array Word8)
posixFromString = fromString posixFromChars

-- | Decode the path to a Unicode string using strict UTF-8 decoding on Posix.
toString :: (Array a -> Stream Identity Char) -> Array a -> [Char]
toString f = runIdentity . Stream.toList . f

posixToChars :: Monad m => Array Word8 -> Stream m Char
posixToChars arr = Unicode.decodeUtf8' $ Array.read arr

posixToString :: Array Word8 -> [Char]
posixToString = toString posixToChars

------------------------------------------------------------------------------
-- Statically Verified Literals
------------------------------------------------------------------------------

-- XXX pass the quote name for errors?
mkQ :: (String -> Q Exp) -> QuasiQuoter
mkQ f =
  QuasiQuoter
  { quoteExp  = f
  , quotePat  = err "pattern"
  , quoteType = err "type"
  , quoteDec  = err "declaration"
  }

  where

  err x = \_ -> fail $ "QuasiQuote used as a " ++ x
    ++ ", can be used only as an expression"

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

-- | path is @.@ or starts with @./@.
isRelativeToCurDir :: (Unbox a, Integral a) => Array a -> Bool
isRelativeToCurDir a =
    -- Assuming the path is not empty.
    if wordToChar (Array.getIndexUnsafe 0 a) /= '.'
    then False
    else if Array.byteLength a < 2
    then True
    else isSeparator Windows (wordToChar (Array.getIndexUnsafe 1 a))

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

-- | On Posix and Windows,
-- * a path starting with a separator
-- * current dir "." or relative to current dir "./"
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
--
-- * @C:file@ a path relative to curdir.
isAbsolute :: (Unbox a, Integral a) => OS -> Array a -> Bool
isAbsolute Posix a =
    -- Assuming path is not empty.
    isSeparator Posix (wordToChar (Array.getIndexUnsafe 0 a))
        || isRelativeToCurDir a
isAbsolute Windows a =
    isAbsoluteDrive a
        || isAbsoluteInDrive a
        || hasDrive a
        || isAbsoluteUNC a
        || isRelativeToCurDir a

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
withAppendCheck os toStr arr f =
    if isAbsolute os arr
    then error $ "append: cannot append absolute or located path " ++ toStr arr
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
