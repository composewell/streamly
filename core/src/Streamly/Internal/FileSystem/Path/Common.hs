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

    -- * Construction
    , fromChunk
    , unsafeFromChunk
    , fromChars
    , unsafeFromChars

    -- * Quasiquoters
    , mkQ

    -- * Elimination
    , toChunk
    , toString
    , toChars

    -- * Operations
    , primarySeparator
    , isSeparator
    , dropTrailingSeparators
    , isSegment
    , isLocation
    , maybeFile
    , isAbsolute
    , isRelativeWithDrive

    , append
    , unsafeAppend

    -- * Utilities
    , wordToChar
    , charToWord
    , unsafeIndexChar
    )
where

#include "assert.hs"

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..))
import Data.Char (ord, isAlpha)
import Data.Functor.Identity (Identity(..))
#ifdef DEBUG
import Data.Maybe (fromJust)
#endif
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutByteArray (Unbox(..))
import Streamly.Internal.Data.Path (PathException(..))
import Streamly.Internal.Data.Stream (Stream)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Stream as Stream

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
unsafeFromChunk ::
#ifdef DEBUG
    Unbox a =>
#endif
    Array Word8 -> Array a
unsafeFromChunk =
#ifndef DEBUG
    Array.castUnsafe
#else
    fromJust . fromChunk
#endif

-- XXX Also check for invalid chars on windows.

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

unsafeFromChars :: (Unbox a) =>
       (Char -> Bool)
    -> (Stream Identity Char -> Stream Identity a)
    -> Stream Identity Char
    -> Array a
unsafeFromChars _p encode s =
#ifndef DEBUG
    let n = runIdentity $ Stream.fold Fold.length s
     in Array.fromPureStreamN n (encode s)
#else
     fromJust (fromChars _p encode s)
#endif

-- XXX Sanitize the path - remove duplicate separators, . segments, trailing .
-- XXX Writing a custom fold for parsing a Posix path may be better for
-- efficient bulk parsing when needed. We need the same code to validate a
-- Chunk where we do not need to create an array.
fromChars :: (MonadThrow m, Unbox a) =>
       (Char -> Bool)
    -> (Stream Identity Char -> Stream Identity a)
    -> Stream Identity Char
    -> m (Array a)
fromChars p encode s =
    -- XXX on windows terminate at first invalid char
    let lengths = Fold.tee Fold.length (Fold.takeEndBy_ p Fold.length)
        (n, n1) = runIdentity $ Stream.fold lengths s
        arr = Array.fromPureStreamN n (encode s)
        sample = Stream.takeWhile p s
     in
        if n <= 0
        then throwM $ InvalidPath "Path cannot be empty."
        else if n1 < n
        then throwM $ InvalidPath $ "Path contains a NULL char at position: "
                ++ show n1 ++ " after " ++ runIdentity (Stream.toList sample)
        else pure arr

toChars :: (Monad m, Unbox a) => (Stream m a -> Stream m Char) -> Array a -> Stream m Char
toChars decode arr = decode $ Array.read arr

toString :: Unbox a => (Stream Identity a -> Stream Identity Char) -> Array a -> [Char]
toString decode = runIdentity . Stream.toList . toChars decode

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

  err x _ = fail $ "QuasiQuote used as a " ++ x
    ++ ", can be used only as an expression"

------------------------------------------------------------------------------
-- Parsing Operations
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- Separator parsing
------------------------------------------------------------------------------

posixSeparator :: Char
posixSeparator = '/'

windowsSeparator :: Char
windowsSeparator = '\\'

-- | Primary path separator character, @/@ on Posix and @\\@ on Windows.
-- Windows supports @/@ too as a separator. Please use 'isSeparator' for
-- testing if a char is a separator char.
{-# INLINE primarySeparator #-}
primarySeparator :: OS -> Char
primarySeparator Posix = posixSeparator
primarySeparator Windows = windowsSeparator

-- | On Posix only @/@ is a path separator but in windows it could be either
-- @/@ or @\\@.
{-# INLINE isSeparator #-}
isSeparator :: OS -> Char -> Bool
isSeparator Windows c = (c == windowsSeparator) || (c == posixSeparator)
isSeparator Posix c = c == posixSeparator

{-# INLINE isSeparatorWord #-}
isSeparatorWord :: Integral a => OS -> a -> Bool
isSeparatorWord os = isSeparator os . wordToChar

------------------------------------------------------------------------------
-- Path normalization
------------------------------------------------------------------------------

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
--
-- Note that a path with trailing separators may implicitly be considered as a
-- directory by some applications. So dropping it may change the dir nature of
-- the path.
{-# INLINE dropTrailingSeparators #-}
dropTrailingSeparators :: (Unbox a, Integral a) => OS -> Array a -> Array a
dropTrailingSeparators os =
    dropTrailingBy (isSeparator os . wordToChar)

-- XXX We implicitly consider "./" as a rooted path. We can provide an API to
-- drop all leading "." to make it a segment from a rooted path.

------------------------------------------------------------------------------
-- Drive parsing
------------------------------------------------------------------------------

-- | @C:...@, does not check array length.
{-# INLINE unsafeHasDrive #-}
unsafeHasDrive :: (Unbox a, Integral a) => Array a -> Bool
unsafeHasDrive a
    -- Check colon first for quicker return
    | unsafeIndexChar 1 a /= ':' = False
    -- XXX If we found a colon anyway this cannot be a valid path unless it has
    -- a drive prefix. colon is not a valid path character.
    -- XXX check isAlpha perf
    | not (isAlpha (unsafeIndexChar 0 a)) = False
    | otherwise = True

-- | A path that starts with a alphabet followed by a colon e.g. @C:...@.
hasDrive :: (Unbox a, Integral a) => Array a -> Bool
hasDrive a = Array.byteLength a >= 2 && unsafeHasDrive a

-- | A path that contains only an alphabet followed by a colon e.g. @C:@.
isDrive :: (Unbox a, Integral a) => Array a -> Bool
isDrive a = Array.byteLength a == 2 && unsafeHasDrive a

------------------------------------------------------------------------------
-- Relative or Absolute
------------------------------------------------------------------------------

-- | A path relative to cur dir it is either @.@ or starts with @./@.
isRelativeCurDir :: (Unbox a, Integral a) => Array a -> Bool
isRelativeCurDir a
    -- Assuming the path is not empty.
    | wordToChar (Array.getIndexUnsafe 0 a) /= '.' = False
    | Array.byteLength a < 2 = True
    | otherwise = isSeparator Windows (wordToChar (Array.getIndexUnsafe 1 a))

-- | The path starting with a separator. On Windows this is relative to current
-- drive while on Posix this is absolute path as there is only one drive.
isRelativeCurDrive :: (Unbox a, Integral a) => OS -> Array a -> Bool
isRelativeCurDrive os a =
    -- Assuming the path is not empty.
    isSeparator os (wordToChar (Array.getIndexUnsafe 0 a))

-- | @C:@ or @C:a...@.
isRelativeWithDrive :: (Unbox a, Integral a) => Array a -> Bool
isRelativeWithDrive a =
    hasDrive a
        && (  Array.byteLength a < 3
           || not (isSeparator Windows (unsafeIndexChar 2 a))
           )

-- | @C:\...@. Note that "C:" or "C:a" is not absolute.
isAbsoluteWithDrive :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteWithDrive a =
    Array.byteLength a >= 3
        && unsafeHasDrive a
        && isSeparator Windows (unsafeIndexChar 2 a)

-- | @\\\\...@
isAbsoluteUNC :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteUNC a
    | Array.byteLength a < 2 = False
    | unsafeIndexChar 0 a /= '\\' = False
    | unsafeIndexChar 1 a /= '\\' = False
    | otherwise = True

-- | Note that on Windows a path starting with a separator is relative to
-- current drive while on Posix this is absolute path as there is only one
-- drive.
isAbsolute :: (Unbox a, Integral a) => OS -> Array a -> Bool
isAbsolute Posix arr =
    isRelativeCurDrive Posix arr
isAbsolute Windows arr =
    isAbsoluteWithDrive arr || isAbsoluteUNC arr

------------------------------------------------------------------------------
-- Location or Segment
------------------------------------------------------------------------------

-- XXX API for static processing of .. (normalizeParentRefs)
--
-- Note: paths starting with . or .. are ambiguous and can be considered
-- segments or rooted. We consider a path starting with "." as rooted, when
-- someone uses "./x" they explicitly mean x in the current directory whereas
-- just "x" can be taken to mean a path segment without any specific root.
-- However, in typed paths the programmer can convey the meaning whether they
-- mean it as a segment or a rooted path. So even "./x" can potentially be used
-- as a segment which can just mean "x".
--
-- XXX For the untyped Path we can allow appending "./x" to other paths. We can
-- leave this to the programmer. In typed paths we can allow "./x" in segments.
--
-- XXX C:\\ is invalid, \\share\ is invalid?
-- XXX Empty path can be taken to mean "." except in case of UNC paths

-- | Rooted paths on Posix and Windows,
-- * @/...@ a path starting with a separator
-- * @.@ current dir
-- * @./...@ a location relative to current dir
--
-- Rooted paths on Windows:
-- * @C:@ local drive cur dir location
-- * @C:a\\b@ local drive relative to cur dir location
-- * @C:\\@ local drive absolute location
-- * @\\@ local path relative to current drive
-- * @\\\\share\\@ UNC network location
-- * @\\\\?\\C:\\@ Long UNC local path
-- * @\\\\?\\UNC\\@ Long UNC server location
-- * @\\\\.\\@ DOS local device namespace
-- * @\\\\??\\@ DOS global namespace
--
isLocation :: (Unbox a, Integral a) => OS -> Array a -> Bool
isLocation Posix a =
    isRelativeCurDrive Posix a
        || isRelativeCurDir a
isLocation Windows a =
    isRelativeCurDrive Windows a
        || isRelativeCurDir a
        || hasDrive a -- curdir-in-drive relative, drive absolute
        || isAbsoluteUNC a

isSegment :: (Unbox a, Integral a) => OS -> Array a -> Bool
isSegment os = not . isLocation os

------------------------------------------------------------------------------
-- File or Dir
------------------------------------------------------------------------------

-- | Returns () if the path can be a valid file, otherwise throws an
-- exception.
maybeFile :: (MonadThrow m, Unbox a, Integral a) => OS -> Array a -> m ()
maybeFile os arr = do
    s1 <-
            Stream.toList
                $ Stream.take 3
                $ Stream.takeWhile (not . isSeparator os)
                $ fmap wordToChar
                $ Array.readRev arr
    -- XXX On posix we just need to check last 3 bytes of the array
    -- XXX Display the path in the exception messages.
    case s1 of
        [] -> throwM $ InvalidPath "A file name cannot have a trailing separator"
        '.' : xs ->
            case xs of
                [] -> throwM $ InvalidPath "A file name cannot have a trailing \".\""
                '.' : [] ->
                    throwM $ InvalidPath "A file name cannot have a trailing \"..\""
                _ -> pure ()
        _ -> pure ()

    case os of
        Windows ->
            -- XXX We can exclude a UNC root as well but just the UNC root is
            -- not even a valid path.
            when (isDrive arr)
                $ throwM $ InvalidPath "A drive root is not a valid file name"
        Posix -> pure ()

------------------------------------------------------------------------------
-- Operations of Path
------------------------------------------------------------------------------

-- XXX This can be generalized to an Array intersperse operation

{-# INLINE doAppend #-}
doAppend :: (Unbox a, Integral a) => OS -> Array a -> Array a -> Array a
doAppend os a b = unsafePerformIO $ do
    let lenA = Array.byteLength a
        lenB = Array.byteLength b
    assertM(lenA /= 0 && lenB /= 0)
    assertM(countTrailingBy (isSeparatorWord os) a == 0)
    let len = lenA + 1 + lenB
    arr <- MutArray.emptyOf len
    arr1 <- MutArray.unsafeSplice arr (Array.unsafeThaw a)
    arr2 <- MutArray.unsafeSnoc arr1 (charToWord (primarySeparator os))
    arr3 <- MutArray.unsafeSplice arr2 (Array.unsafeThaw b)
    return (Array.unsafeFreeze arr3)

{-# INLINE withAppendCheck #-}
withAppendCheck :: (Unbox b, Integral b) =>
    OS -> (Array b -> String) -> Array b -> a -> a
withAppendCheck os toStr arr f =
    if isLocation os arr
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
