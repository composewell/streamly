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

    , append
    , unsafeAppend

    -- * Path normalization

    -- Most of these helpers are exposed because we want to test them using
    -- docspec.

    , readDriveShareName
    , readDriveLetter
    , readDriveUNC
    , readDriveShare
    , spanDrive
    , normalizeDrive
    , splitPath
    , isNotFileLocation
    , normalizedEq

    -- * Utilities
    , wordToChar
    , charToWord
    , unsafeIndexChar
    )
where

#include "assert.hs"

{- $setup
>>> :m

>>> import Data.Functor.Identity (runIdentity)
>>> import System.IO.Unsafe (unsafePerformIO)
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Unicode.Stream as Unicode
>>> import qualified Streamly.Internal.Data.Array as Array
>>> import qualified Streamly.Internal.FileSystem.Path.Common as Common
>>> import qualified Streamly.Internal.Unicode.Stream as Unicode

>>> packPosix = unsafePerformIO . Stream.fold Array.create . Unicode.encodeUtf8' . Stream.fromList
>>> unpackPosix = runIdentity . Stream.toList . Unicode.decodeUtf8' . Array.read

>>> packWindows = unsafePerformIO . Stream.fold Array.create . Unicode.encodeUtf16le' . Stream.fromList
>>> unpackWindows = runIdentity . Stream.toList . Unicode.decodeUtf16le' . Array.read
-}

import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord, isAlpha, isAsciiLower, isAsciiUpper, toUpper)
import Data.Function ((&))
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

-- | Primary path separator character, @/@ on Posix and @\\@ on Windows.
-- Windows supports @/@ too as a separator. Please use 'isSeparator' for
-- testing if a char is a separator char.
{-# INLINE primarySeparator #-}
primarySeparator :: OS -> Char
primarySeparator Posix = posixSeparator
primarySeparator Windows = windowsSeparator

------------------------------------------------------------------------------
-- Path parsing utilities
------------------------------------------------------------------------------

-- | On Posix only @/@ is a path separator but in windows it could be either
-- @/@ or @\\@.
{-# INLINE isSeparator #-}
isSeparator :: OS -> Char -> Bool
isSeparator Windows c = (c == windowsSeparator) || (c == posixSeparator)
isSeparator Posix c = c == posixSeparator

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
isCurDirRelativeLocation :: (Unbox a, Integral a) => Array a -> Bool
isCurDirRelativeLocation a
    -- Assuming the path is not empty.
    | wordToChar (Array.getIndexUnsafe 0 a) /= '.' = False
    | Array.byteLength a < 2 = True
    | otherwise = isSeparator Windows (wordToChar (Array.getIndexUnsafe 1 a))

-- | @C:...@
hasDrive :: (Unbox a, Integral a) => Array a -> Bool
hasDrive a
    | Array.byteLength a < 2 = False
    -- Check colon first for quicker return
    | unsafeIndexChar 1 a /= ':' = False
    -- XXX If we found a colon anyway this cannot be a valid path unless it has
    -- a drive prefix. colon is not a valid path character.
    -- XXX check isAlpha perf
    | not (isAlpha (unsafeIndexChar 0 a)) = False
    | otherwise = True

-- | On windows, the path starts with a separator.
isCurDriveRelativeLocation :: (Unbox a, Integral a) => Array a -> Bool
isCurDriveRelativeLocation a =
    -- Assuming the path is not empty.
    isSeparator Windows (wordToChar (Array.getIndexUnsafe 0 a))

-- | @C:\...@
isLocationDrive :: (Unbox a, Integral a) => Array a -> Bool
isLocationDrive a
    | Array.byteLength a < 3 = False
    -- Check colon first for quicker return
    | unsafeIndexChar 1 a /= ':' = False
    | not (isSeparator Windows (unsafeIndexChar 2 a)) = False
    -- XXX If we found a colon anyway this cannot be a valid path unless it has
    -- a drive prefix. colon is not a valid path character.
    -- XXX check isAlpha perf
    | not (isAlpha (unsafeIndexChar 0 a)) = False
    | otherwise = True

-- | @\\\\...@
isAbsoluteUNCLocation :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteUNCLocation a
    | Array.byteLength a < 2 = False
    | unsafeIndexChar 0 a /= '\\' = False
    | unsafeIndexChar 1 a /= '\\' = False
    | otherwise = True

-- | On Posix and Windows,
-- * a path starting with a separator, an absolute location
-- * current dir "." or a location relative to current dir "./"
--
-- On Windows:
-- * @C:\\@ local absolute
-- * @C:@ local relative
-- * @\\@ local relative to current drive root
-- * @\\\\@ UNC network location
-- * @\\\\?\\C:\\@ Long UNC local path
-- * @\\\\?\\UNC\\@ Long UNC server location
-- * @\\\\.\\@ DOS local device namespace
-- * @\\\\??\\@ DOS global namespace
--
-- * @C:file@ a path relative to curdir.
isLocation :: (Unbox a, Integral a) => OS -> Array a -> Bool
isLocation Posix a =
    -- Assuming path is not empty.
    isSeparator Posix (wordToChar (Array.getIndexUnsafe 0 a))
        || isCurDirRelativeLocation a
isLocation Windows a =
    isLocationDrive a
        || isCurDriveRelativeLocation a
        || hasDrive a -- curdir-in-drive relative, drive absolute
        || isAbsoluteUNCLocation a
        || isCurDirRelativeLocation a

isSegment :: (Unbox a, Integral a) => OS -> Array a -> Bool
isSegment os = not . isLocation os

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

--------------------------------------------------------------------------------
-- Path equality helpers
--------------------------------------------------------------------------------

{-# INLINE ordIntegral #-}
ordIntegral :: Integral a => Char -> a
ordIntegral = fromIntegral . ord

{-# INLINE isSeparatorIntegral #-}
isSeparatorIntegral :: Integral a => OS -> a -> Bool
isSeparatorIntegral os =
    isSeparator os . unsafeChr . fromIntegral

{-# INLINE primarySeparatorIntegral #-}
primarySeparatorIntegral :: Integral a => OS -> a
primarySeparatorIntegral = ordIntegral . primarySeparator

{-# INLINE isLetter #-}
isLetter :: Char -> Bool
isLetter l = isAsciiLower l || isAsciiUpper l

{-# INLINE countUntilSeperator #-}
countUntilSeperator :: (Monad m, Unbox a, Integral a) => OS -> Array a -> m Int
countUntilSeperator os arr =
    Stream.fold
          (Fold.takeEndBy_ (not . isSeparatorIntegral os) Fold.length)
          (Array.read arr)

--------------------------------------------------------------------------------
-- Path equality windows specific
--------------------------------------------------------------------------------

-- |
-- >>> readDriveLetter = fmap (\(a, b) -> (unpackWindows a, unpackWindows b)) . Common.readDriveLetter . packWindows
--
-- >>> readDriveLetter "c:"
-- Just ("c:","")
--
-- >>> readDriveLetter "F:\\"
-- Just ("F:\\","")
--
-- >>> readDriveLetter "F:\\\\\\"
-- Just ("F:\\\\\\","")
--
-- >>> readDriveLetter "F:\\\\Desktop"
-- Just ("F:\\\\","Desktop")
--
-- >>> readDriveLetter "F:\\/./Desktop"
-- Just ("F:\\/","./Desktop")
--
-- >>> readDriveLetter "\\Desktop"
-- Nothing
--
readDriveLetter :: (Unbox a, Integral a) => Array a -> Maybe (Array a, Array a)
readDriveLetter arr
    | arrLen >= 2, isLetter x, c == ':' = Just $
        if arrLen >= 3 && isSeparator Windows (unsafeIndexChar 2 arr)
        then runIdentity $ do
            let afterDrive = Array.getSliceUnsafe 2 (arrLen - 2) arr
            i <- countUntilSeperator Windows afterDrive
            pure
                ( Array.getSliceUnsafe 0 (2 + i) arr
                , Array.getSliceUnsafe (2 + i) (arrLen - i - 2) arr
                )
        else ( Array.getSliceUnsafe 0 2 arr
             , Array.getSliceUnsafe 2 (arrLen - 2) arr
             )
    | otherwise = Nothing
    where
    arrLen = Array.length arr
    x = unsafeIndexChar 0 arr
    c = unsafeIndexChar 1 arr

-- |
-- >>> readDriveShareName = (\(a, b) -> (unpackWindows a, unpackWindows b)) . Common.readDriveShareName . packWindows
--
-- >>> readDriveShareName ""
-- ("","")
--
-- >>> readDriveShareName "Desktop/Folder"
-- ("Desktop/","Folder")
--
-- >>> readDriveShareName "Desktop//\\\\\\Folder"
-- ("Desktop/","/\\\\\\Folder")
--
-- >>> readDriveShareName "Desktop"
-- ("Desktop","")
--
readDriveShareName :: (Unbox a, Integral a) => Array a -> (Array a, Array a)
readDriveShareName arr = runIdentity $ do
    i <- Stream.fold (Fold.takeEndBy isSep Fold.length) (Array.read arr)
    pure (Array.getSliceUnsafe 0 i arr, Array.getSliceUnsafe i (arrLen - i) arr)
    where
    isSep = isSeparatorIntegral Windows
    arrLen = Array.length arr

-- |
-- >>> readDriveShare = fmap (\(a, b) -> (unpackWindows a, unpackWindows b)) . Common.readDriveShare . packWindows
--
-- >>> readDriveShare ""
-- Nothing
--
-- >>> readDriveShare "Desktop"
-- Nothing
--
-- >>> readDriveShare "\\/"
-- Just ("\\/","")
--
-- >>> readDriveShare "\\\\localhost\\Desktop"
-- Just ("\\\\localhost\\","Desktop")
--
-- >>> readDriveShare "\\\\localhost"
-- Just ("\\\\localhost","")
--
-- >>> readDriveShare "\\\\localhost/"
-- Just ("\\\\localhost/","")
--
readDriveShare :: (Unbox a, Integral a) => Array a -> Maybe (Array a, Array a)
readDriveShare arr
    | arrLen >= 2 && isSep s1 && isSep s2 =
        let (a, b) =
                readDriveShareName (Array.getSliceUnsafe 2 (arrLen - 2) arr)
         in Just (Array.getSliceUnsafe 0 (2 + Array.length a) arr, b)
    | otherwise = Nothing
    where
    isSep = isSeparator Windows
    arrLen = Array.length arr
    s1 = unsafeIndexChar 0 arr
    s2 = unsafeIndexChar 1 arr

-- |
-- >>> readDriveUNC = fmap (\(a, b) -> (unpackWindows a, unpackWindows b)) . Common.readDriveUNC . packWindows
--
-- >>> readDriveUNC ""
-- Nothing
--
-- >>> readDriveUNC "Desktop"
-- Nothing
--
-- >>> readDriveUNC "\\/?\\c:"
-- Just ("\\/?\\c:","")
--
-- >>> readDriveUNC "\\/?\\F:/"
-- Just ("\\/?\\F:/","")
--
-- >>> readDriveUNC "\\/?\\F:/\\\\Desktop"
-- Just ("\\/?\\F:/\\\\","Desktop")
--
-- >>> readDriveUNC "\\/?\\uNc/"
-- Just ("\\/?\\uNc/","")
--
-- >>> readDriveUNC "\\/?\\uNc/\\Desktop"
-- Just ("\\/?\\uNc/\\","Desktop")
--
-- >>> readDriveUNC "\\/?\\uNc/Desktop\\Folder"
-- Just ("\\/?\\uNc/Desktop\\","Folder")
--
readDriveUNC
    :: (Unbox a, Integral a) => Array a -> Maybe (Array a, Array a)
readDriveUNC arr
    | arrLen >= 4, q == '?', all isSep [s1,s2,s3] =
        if arrLen >= 8 && map toUpper [u, n, c] == "UNC" && isSep s4
        then
            let (a, b) =
                    readDriveShareName (Array.getSliceUnsafe 8 (arrLen - 8) arr)
             in Just (Array.getSliceUnsafe 0 (8 + Array.length a) arr, b)
        else
            case readDriveLetter (Array.getSliceUnsafe 4 (arrLen - 4) arr) of
                Nothing -> Nothing
                Just (a, b) ->
                    Just (Array.getSliceUnsafe 0 (4 + Array.length a) arr, b)
    | otherwise = Nothing
    where
    isSep = isSeparator Windows
    arrLen = Array.length arr

    s1 = unsafeIndexChar 0 arr
    s2 = unsafeIndexChar 1 arr
    q = unsafeIndexChar 2 arr
    s3 = unsafeIndexChar 3 arr
    u = unsafeIndexChar 4 arr
    n = unsafeIndexChar 5 arr
    c = unsafeIndexChar 6 arr
    s4 = unsafeIndexChar 7 arr

-- |
-- >>> spanDrive = (\(a, b) -> (unpackWindows a, unpackWindows b)) . Common.spanDrive . packWindows
--
-- >>> spanDrive "F:\\/./Desktop"
-- ("F:\\/","./Desktop")
--
-- >>> spanDrive "\\\\localhost\\Desktop"
-- ("\\\\localhost\\","Desktop")
--
-- >>> spanDrive "\\/?\\uNc/Desktop\\Folder"
-- ("\\/?\\uNc/Desktop\\","Folder")
--
-- >>> spanDrive "\\local/device"
-- ("\\","local/device")
--
-- >>> spanDrive "\\."
-- ("\\",".")
--
-- >>> spanDrive "file"
-- ("","file")
--
-- >>> spanDrive "c:/file"
-- ("c:/","file")
--
-- >>> spanDrive "c:\\file"
-- ("c:\\","file")
--
-- >>> spanDrive "\\\\shared\\test"
-- ("\\\\shared\\","test")
--
-- >>> spanDrive "\\\\shared"
-- ("\\\\shared","")
--
-- >>> spanDrive "\\\\?\\UNC\\shared\\file"
-- ("\\\\?\\UNC\\shared\\","file")
--
-- >>> spanDrive "\\\\?\\UNCshared\\file"
-- ("\\\\?\\","UNCshared\\file")
--
-- >>> spanDrive "\\\\?\\d:\\file"
-- ("\\\\?\\d:\\","file")
--
-- >>> spanDrive "/d"
-- ("\\","d")
--
spanDrive :: (Unbox a, Integral a) => Array a -> (Array a, Array a)
spanDrive arr | Just res <- readDriveLetter arr = res
spanDrive arr | Just res <- readDriveUNC arr = res
spanDrive arr | Just res <- readDriveShare arr = res
spanDrive arr = runIdentity $ do
    i <- countUntilSeperator Windows arr
    pure
        $ if i > 0
          then ( Array.fromListN 1 [primarySeparatorIntegral Windows]
               , Array.getSliceUnsafe i (arrLen - i) arr
               )
          else (Array.empty, Array.getSliceUnsafe i (arrLen - i) arr)
    where
    arrLen = Array.length arr

-- XXX Should we normalize uNc to UNC?
-- XXX What about uNcshared vs UNCshared?
-- |
-- >>> normalizeDrive = unpackWindows . Common.normalizeDrive . packWindows
--
-- >>> normalizeDrive ""
-- ""
--
-- >>> normalizeDrive "F:\\/"
-- "F:\\"
--
-- >>> normalizeDrive "\\\\localhost/"
-- "\\\\localhost\\"
--
-- >>> normalizeDrive "\\/?\\uNc/Desktop\\"
-- "\\\\?\\UNC\\Desktop\\"
--
-- >>> normalizeDrive "\\"
-- "\\"
--
normalizeDrive :: (Unbox a, Integral a) => Array a -> Array a
normalizeDrive arr | Array.null arr = Array.empty
normalizeDrive arr
    | Just (drv, _) <- readDriveLetter arrSRep =
        let drvLen = Array.length drv
         in
           if drvLen == 0
           then error "normalizeDrive: impossible"
           else
               let x = ordIntegral $ toUpper $ unsafeIndexChar 0 drv
                in if drvLen == 2
                   then Array.fromListN 2 [x, ordIntegral ':']
                   else Array.fromListN 3 [x, ordIntegral ':', primarySeparatorIntegral Windows]
    | otherwise = arrSRep
    where
    canonicalizeSeperator x =
        if isSeparatorIntegral Windows x
        then primarySeparatorIntegral Windows
        else x
    arrSRep =
        unsafePerformIO
            $ Stream.fold Array.create
            $ fmap canonicalizeSeperator
            $ Array.read arr

--------------------------------------------------------------------------------
-- Path equality posix specific
--------------------------------------------------------------------------------

-- Posix specific function.
isAbsoluteLocation :: (Integral a, Unbox a) => Array a -> Bool
isAbsoluteLocation arr = arrLen > 0 && firstChar == primarySeparator Posix
    where
    arrLen = Array.length arr
    firstChar = unsafeIndexChar 0 arr

--------------------------------------------------------------------------------
-- Path equality common operations
--------------------------------------------------------------------------------

-- |
-- >>> :{
--  splitPath Common.Posix = Stream.toList . fmap unpackPosix . Common.splitPath Common.Posix . packPosix
--  splitPath Common.Windows = Stream.toList . fmap unpackWindows . Common.splitPath Common.Windows . packWindows
-- :}
--
-- >>> splitPath Common.Posix "home//user/./..////\\directory/."
-- ["home","user","..","\\directory"]
--
-- >>> splitPath Common.Windows "home//user/./..////\\directory/."
-- ["home","user","..","directory"]
--
{-# INLINE splitPath #-}
splitPath
    :: forall a m. (Unbox a, Integral a, MonadIO m)
    => OS -> Array a -> Stream m (Array a)
splitPath os arr =
    Stream.indexOnSuffix (isSeparatorIntegral os) (Array.read arr)
        & Stream.filter (not . shouldFilterOut)
        & fmap (\(i, len) -> Array.getSliceUnsafe i len arr)

    where

    shouldFilterOut (off, len) =
        len == 0 ||
            (len == 1 && unsafeIndexChar off arr == '.')

-- |
-- >>> :{
--  isNotFileLocation Common.Posix = Common.isNotFileLocation Common.Posix . packPosix
--  isNotFileLocation Common.Windows = Common.isNotFileLocation Common.Windows . packWindows
-- :}
--
-- >>> isNotFileLocation Common.Posix ""
-- False
--
-- >>> isNotFileLocation Common.Posix "/"
-- True
--
-- >>> isNotFileLocation Common.Posix "/."
-- True
--
-- >>> isNotFileLocation Common.Posix "./."
-- True
--
-- >>> isNotFileLocation Common.Posix "home/user"
-- False
--
-- >>> isNotFileLocation Common.Windows "\\"
-- True
--
-- >>> isNotFileLocation Common.Windows "\\."
-- True
--
-- >>> isNotFileLocation Common.Windows ""
-- False
--
-- >>> isNotFileLocation Common.Windows "home\\user"
-- False
--
-- >>> isNotFileLocation Common.Windows "/home/user/"
-- True
--
isNotFileLocation :: (Integral a, Unbox a) => OS -> Array a -> Bool
isNotFileLocation os arr =
    (arrLen == 0)
        || (arrLen > 0 && (isSeparator os lastChar))
        || (arrLen > 1 && isSeparator os sndlastChar && lastChar == '.')

    where
    arrLen = Array.length arr
    lastChar = unsafeIndexChar (arrLen - 1) arr
    sndlastChar = unsafeIndexChar (arrLen - 2) arr

-- |
-- >>> :{
--  normalizedEq Common.Posix a b = Common.normalizedEq Common.Posix (packPosix a) (packPosix b)
--  normalizedEq Common.Windows a b = Common.normalizedEq Common.Windows (packWindows a) (packWindows b)
-- :}
--
-- >>> normalizedEq Common.Posix "/file/\\test////"  "/file/\\test/"
-- True
--
-- >>> normalizedEq Common.Posix "/file/./test"  "/file/test"
-- True
--
-- >>> normalizedEq Common.Posix "/test/file/../bob/fred/"  "/test/file/../bob/fred/"
-- True
--
-- >>> normalizedEq Common.Posix "../bob/fred/"  "../bob/fred/"
-- True
--
-- >>> normalizedEq Common.Posix "/a/../c"  "/a/../c"
-- True
--
-- >>> normalizedEq Common.Posix "./bob/fred/"  "bob/fred/"
-- True
--
-- >>> normalizedEq Common.Posix "./"  "./"
-- True
--
-- >>> normalizedEq Common.Posix "./."  "./"
-- True
--
-- >>> normalizedEq Common.Posix "/./"  "/"
-- True
--
-- >>> normalizedEq Common.Posix "/"  "/"
-- True
--
-- >>> normalizedEq Common.Posix "bob/fred/."  "bob/fred/"
-- True
--
-- >>> normalizedEq Common.Posix "//home"  "/home"
-- True
--
-- >>> normalizedEq Common.Windows "c:\\file/bob\\" "C:\\file\\bob\\"
-- True
--
-- >>> normalizedEq Common.Windows "c:\\" "C:\\"
-- True
--
-- >>> normalizedEq Common.Windows "C:.\\" "C:"
-- True
--
-- >>> normalizedEq Common.Windows "\\\\server\\test" "\\\\server\\test"
-- True
--
-- >>> normalizedEq Common.Windows "//server/test" "\\\\server\\test"
-- True
--
-- >>> normalizedEq Common.Windows "c:/file" "C:\\file"
-- True
--
-- >>> normalizedEq Common.Windows "/file" "\\file"
-- True
--
-- >>> normalizedEq Common.Windows "\\" "\\"
-- True
--
-- >>> normalizedEq Common.Windows "/./" "\\"
-- True
--
normalizedEq :: (Integral a, Unbox a) => OS -> Array a -> Array a -> Bool
normalizedEq Posix a b = unsafePerformIO $ do
    let absA = isAbsoluteLocation a
        absB = isAbsoluteLocation b
        notFA = isNotFileLocation Posix a
        notFB = isNotFileLocation Posix b
    if absA == absB && notFA == notFB
    then Stream.eqBy Array.byteEq (splitPath Posix a) (splitPath Posix b)
    else pure False
normalizedEq Windows a b = unsafePerformIO $ do
    let (da, pa) = spanDrive a
        (db, pb) = spanDrive b
        nFA = isNotFileLocation Windows pa
        nFB = isNotFileLocation Windows pb
    if nFA == nFB && Array.byteEq (normalizeDrive da) (normalizeDrive db)
    then Stream.eqBy Array.byteEq (splitPath Windows pa) (splitPath Windows pb)
    else pure False
