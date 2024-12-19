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
    , readDriveShareName
    , readDriveLetter
    , readDriveUNC
    , readDriveShare
    , splitDrive
    , normalizeDrive
    , normalizePath
    , normalize

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
-- Path normalization
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
-- >>> :{
--  splitDrive Common.Posix = (\(a, b) -> (unpackPosix a, unpackPosix b)) . Common.splitDrive Common.Posix . packPosix
--  splitDrive Common.Windows = (\(a, b) -> (unpackWindows a, unpackWindows b)) . Common.splitDrive Common.Windows . packWindows
-- :}
--
-- >>> splitDrive Common.Posix ""
-- ("","")
--
-- >>> splitDrive Common.Posix "/"
-- ("/","")
--
-- >>> splitDrive Common.Posix "./"
-- ("","./")
--
-- >>> splitDrive Common.Posix "/home/usr"
-- ("/","home/usr")
--
-- >>> splitDrive Common.Posix "/////home/usr"
-- ("/","home/usr")
--
-- >>> splitDrive Common.Posix "/test"
-- ("/","test")
--
-- >>> splitDrive Common.Posix "//test"
-- ("/","test")
--
-- >>> splitDrive Common.Posix "test/file"
-- ("","test/file")
--
-- >>> splitDrive Common.Posix "file"
-- ("","file")
--
-- >>> splitDrive Common.Windows "F:\\/./Desktop"
-- ("F:\\/","./Desktop")
--
-- >>> splitDrive Common.Windows "\\\\localhost\\Desktop"
-- ("\\\\localhost\\","Desktop")
--
-- >>> splitDrive Common.Windows "\\/?\\uNc/Desktop\\Folder"
-- ("\\/?\\uNc/Desktop\\","Folder")
--
-- >>> splitDrive Common.Windows "\\local/device"
-- ("\\","local/device")
--
-- >>> splitDrive Common.Windows "\\."
-- ("\\",".")
--
-- >>> splitDrive Common.Windows "file"
-- ("","file")
--
-- >>> splitDrive Common.Windows "c:/file"
-- ("c:/","file")
--
-- >>> splitDrive Common.Windows "c:\\file"
-- ("c:\\","file")
--
-- >>> splitDrive Common.Windows "\\\\shared\\test"
-- ("\\\\shared\\","test")
--
-- >>> splitDrive Common.Windows "\\\\shared"
-- ("\\\\shared","")
--
-- >>> splitDrive Common.Windows "\\\\?\\UNC\\shared\\file"
-- ("\\\\?\\UNC\\shared\\","file")
--
-- >>> splitDrive Common.Windows "\\\\?\\UNCshared\\file"
-- ("\\\\?\\","UNCshared\\file")
--
-- >>> splitDrive Common.Windows "\\\\?\\d:\\file"
-- ("\\\\?\\d:\\","file")
--
-- >>> splitDrive Common.Windows "/d"
-- ("\\","d")
--
splitDrive :: (Unbox a, Integral a) => OS -> Array a -> (Array a, Array a)
splitDrive Windows arr | Just res <- readDriveLetter arr = res
splitDrive Windows arr | Just res <- readDriveUNC arr = res
splitDrive Windows arr | Just res <- readDriveShare arr = res
splitDrive os arr = runIdentity $ do
    i <- countUntilSeperator os arr
    pure
        $ if i > 0
          then ( Array.fromListN 1 [primarySeparatorIntegral os]
               , Array.getSliceUnsafe i (arrLen - i) arr
               )
          else (Array.empty, Array.getSliceUnsafe i (arrLen - i) arr)
    where
    arrLen = Array.length arr

-- XXX Should we normalize uNc to UNC?
-- XXX What about uNcshared vs UNCshared?
-- |
-- >>> :{
--  normalizeDrive Common.Posix = unpackPosix . Common.normalizeDrive Common.Posix . packPosix
--  normalizeDrive Common.Windows = unpackWindows . Common.normalizeDrive Common.Windows . packWindows
-- :}
--
-- >>> normalizeDrive Common.Posix ""
-- ""
--
-- >>> normalizeDrive Common.Posix "/"
-- "/"
--
-- >>> normalizeDrive Common.Windows ""
-- ""
--
-- >>> normalizeDrive Common.Windows "F:\\/"
-- "F:\\"
--
-- >>> normalizeDrive Common.Windows "\\\\localhost/"
-- "\\\\localhost\\"
--
-- >>> normalizeDrive Common.Windows "\\/?\\uNc/Desktop\\"
-- "\\\\?\\UNC\\Desktop\\"
--
-- >>> normalizeDrive Common.Windows "\\"
-- "\\"
--
normalizeDrive :: (Unbox a, Integral a) => OS -> Array a -> Array a
normalizeDrive _ arr | Array.null arr = Array.empty
normalizeDrive Posix _ =
    Array.fromListN 1 [primarySeparatorIntegral Posix]
normalizeDrive Windows arr
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

-- |
-- >>> :{
--  normalizePath Common.Posix = unpackPosix . Common.normalizePath Common.Posix . packPosix
--  normalizePath Common.Windows = unpackWindows . Common.normalizePath Common.Windows . packWindows
-- :}
--
-- >>> normalizePath Common.Posix ""
-- ""
--
-- >>> normalizePath Common.Posix "/"
-- ""
--
-- >>> normalizePath Common.Posix "/."
-- ""
--
-- >>> normalizePath Common.Posix "/home/usr/"
-- "home/usr/"
--
-- >>> normalizePath Common.Posix "/////home/usr/."
-- "home/usr/"
--
-- >>> normalizePath Common.Windows "./Desktop/"
-- "Desktop\\"
--
-- >>> normalizePath Common.Windows "\\Desktop\\Folder/."
-- "Desktop\\Folder\\"
--
-- >>> normalizePath Common.Windows "\\Desktop\\Folder/"
-- "Desktop\\Folder\\"
--
-- >>> normalizePath Common.Windows "\\Desktop\\File"
-- "Desktop\\File"
--
-- >>> normalizePath Common.Windows "."
-- ""
--
-- >>> normalizePath Common.Windows ""
-- ""
--
{-# INLINE normalizePath #-}
normalizePath :: forall a. (Unbox a, Integral a) => OS -> Array a -> Array a
normalizePath os arr =
    Array.unsafeFreeze $ unsafePerformIO $ do
        let workSliceStream = MutArray.read workSliceMut
        mid <-
            Stream.indexOnSuffix (isSeparatorIntegral os) workSliceStream
                & Stream.filter (not . shouldFilterOut)
                & Stream.mapM (\(i, len) -> getSliceWithSepSuffix os i len)
                & Stream.fold (Fold.foldlM' (combine os) initBufferM)
        case os of
            Posix -> pure mid
            Windows ->
                let midLen = MutArray.length mid in
                pure $ case midLen of
                    ml | ml >= 2 ->
                        let lastElem = Array.getIndexUnsafe (arrLen - 1) arr
                            lastButOne = Array.getIndexUnsafe (arrLen - 2) arr
                         in if (isSeparatorIntegral Windows lastButOne
                                  && lastElem == dotElem)
                                   || isSeparatorIntegral Windows lastElem
                            then mid
                            else MutArray.unsafeGetSlice 0 (midLen - 1) mid
                    ml | ml >= 1 ->
                        let lastElem = Array.getIndexUnsafe (arrLen - 1) arr
                         in if isSeparatorIntegral Windows lastElem
                            then mid
                            else MutArray.unsafeGetSlice 0 (midLen - 1) mid
                    _ -> mid

    where

    (dotElem :: a) = ordIntegral '.'
    arrLen = Array.length arr

    workSlice = arr
    workSliceMut = Array.unsafeThaw workSlice
    workSliceElemLen = Array.length workSlice

    shouldFilterOut (off, len) =
        len == 0 ||
            (len == 1 && Array.getIndexUnsafe off workSlice == dotElem)

    getSliceWithSepSuffix Posix i len
        | i + len == workSliceElemLen =
            pure $ MutArray.unsafeGetSlice i len workSliceMut
    getSliceWithSepSuffix Posix i len =
        pure $ MutArray.unsafeGetSlice i (len + 1) workSliceMut
    getSliceWithSepSuffix Windows i len =
        pure $ MutArray.unsafeGetSlice i len workSliceMut

    combine Posix b a = MutArray.unsafeSplice b a
    combine Windows b a = do
        b1 <- MutArray.unsafeSplice b a
        MutArray.unsafeSnoc b1 (primarySeparatorIntegral Windows)

    initBufferM = MutArray.emptyOf (arrLen + 1)


-- |
-- >>> :{
--  normalize Common.Posix = unpackPosix . Common.normalize Common.Posix . packPosix
--  normalize Common.Windows = unpackWindows . Common.normalize Common.Windows . packWindows
-- :}
--
-- >>> normalize Common.Posix ""
-- ""
--
-- >>> normalize Common.Posix "/"
-- "/"
--
-- >>> normalize Common.Posix "/path/to///file"
-- "/path/to/file"
--
-- >>> normalize Common.Posix "/path/to///folder/"
-- "/path/to/folder/"
--
-- >>> normalize Common.Posix "/path/to/././folder/."
-- "/path/to/folder/"
--
-- >>> normalize Common.Posix "/path/to/./../folder/."
-- "/path/to/../folder/"
--
-- >>> normalize Common.Posix "/file/\\test////"
-- "/file/\\test/"
--
-- >>> normalize Common.Posix "/file/./test"
-- "/file/test"
--
-- >>> normalize Common.Posix "/test/file/../bob/fred/"
-- "/test/file/../bob/fred/"
--
-- >>> normalize Common.Posix "../bob/fred/"
-- "../bob/fred/"
--
-- >>> normalize Common.Posix "/a/../c"
-- "/a/../c"
--
-- >>> normalize Common.Posix "./bob/fred/"
-- "bob/fred/"
--
-- >>> normalize Common.Posix "."
-- "."
--
-- >>> normalize Common.Posix "./"
-- "./"
--
-- >>> normalize Common.Posix "./."
-- "./"
--
-- >>> normalize Common.Posix "/./"
-- "/"
--
-- >>> normalize Common.Posix "/"
-- "/"
--
-- >>> normalize Common.Posix "bob/fred/."
-- "bob/fred/"
--
-- >>> normalize Common.Posix "//home"
-- "/home"
--
-- >>> normalize Common.Windows "."
-- "."
--
-- >>> normalize Common.Windows "\\\\?\\c:\\"
-- "\\\\?\\c:\\"
--
-- >>> normalize Common.Windows "c:\\file/bob\\"
-- "C:\\file\\bob\\"
--
-- >>> normalize Common.Windows "c:\\file/bob\\"
-- "C:\\file\\bob\\"
--
-- >>> normalize Common.Windows "c:\\"
-- "C:\\"
--
-- >>> normalize Common.Windows "c:\\\\\\\\"
-- "C:\\"
--
-- >>> normalize Common.Windows "C:.\\"
-- "C:"
--
-- >>> normalize Common.Windows "\\\\server\\test"
-- "\\\\server\\test"
--
-- >>> normalize Common.Windows "//server/test"
-- "\\\\server\\test"
--
-- >>> normalize Common.Windows "c:/file"
-- "C:\\file"
--
-- >>> normalize Common.Windows "\\file"
-- "\\file"
--
-- >>> normalize Common.Windows "/file"
-- "\\file"
--
-- >>> normalize Common.Windows "/./"
-- "\\"
--
-- >>> normalize Common.Windows "/file/\\test////"
-- "\\file\\test\\"
--
-- >>> normalize Common.Windows "/file/./test"
-- "\\file\\test"
--
-- >>> normalize Common.Windows "/test/file/../bob/fred/"
-- "\\test\\file\\..\\bob\\fred\\"
--
-- >>> normalize Common.Windows "../bob/fred/"
-- "..\\bob\\fred\\"
--
-- >>> normalize Common.Windows "/a/../c"
-- "\\a\\..\\c"
--
-- >>> normalize Common.Windows "./bob/fred/"
-- "bob\\fred\\"
--
-- >>> normalize Common.Windows "./"
-- ".\\"
--
-- >>> normalize Common.Windows "./."
-- ".\\"
--
-- >>> normalize Common.Windows "/./"
-- "\\"
--
-- >>> normalize Common.Windows "/"
-- "\\"
--
-- >>> normalize Common.Windows "bob/fred/."
-- "bob\\fred\\"
--
-- >>> normalize Common.Windows "//home"
-- "\\\\home"
--
{-# INLINE normalize #-}
normalize :: forall a. (Unbox a, Integral a) => OS -> Array a -> Array a
normalize os arr =
    let (a, b) = splitDrive os arr
        drv = normalizeDrive os a
        pth = normalizePath os b
        drvLen = Array.length drv
        pthLen = Array.length pth
        arrLen = Array.length arr
     in if drvLen == 0 && pthLen == 0 && arrLen > 0
        then
            if arrLen >= 2
            then
                let x = unsafeIndexChar 0 arr
                    y = unsafeIndexChar 1 arr
                in
                  if x == '.' && isSeparator os y
                  then Array.fromListN 2
                           [ordIntegral '.', primarySeparatorIntegral os]
                  else Array.fromListN 1 [ordIntegral '.']
            else Array.fromListN 1 [ordIntegral '.']
        else if drvLen == 0
        then pth
        else if pthLen == 0
        then drv
        else Array.unsafeFreeze $ unsafePerformIO $ do
            let x = unsafeIndexChar (drvLen - 1) drv
            if isSeparator os x
            then do
                marr <- MutArray.emptyOf (drvLen + pthLen)
                marr1 <- MutArray.unsafeSplice marr (Array.unsafeThaw drv)
                MutArray.unsafeSplice marr1 (Array.unsafeThaw pth)
            else do
                marr <- MutArray.emptyOf (drvLen + pthLen + 1)
                marr1 <- MutArray.unsafeSplice marr (Array.unsafeThaw drv)
                marr2 <-
                    MutArray.unsafeSnoc
                        marr1 (ordIntegral (primarySeparator os))
                MutArray.unsafeSplice marr2 (Array.unsafeThaw pth)
