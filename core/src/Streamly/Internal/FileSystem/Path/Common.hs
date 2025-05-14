{-# LANGUAGE UnliftedFFITypes #-}
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

    -- * Validation
    , isValidPath
    , isValidPath'
    , validatePath
    , validatePath'
    , validateFile

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

    -- * Separators
    , primarySeparator
    , isSeparator
    , dropTrailingSeparators
    , hasTrailingSeparator
    , hasLeadingSeparator

    -- * Tests
    , isBranch
    , isRooted
    , isAbsolute
 -- , isRelative -- not isAbsolute
    , isRootRelative -- XXX hasRelativeRoot
    , isRelativeWithDrive -- XXX hasRelativeDriveRoot
    , hasDrive

    -- * Joining
    , append
    , append'
    , unsafeAppend
    , appendCString
    , appendCString'
    , unsafeJoinPaths
 -- , joinRoot -- XXX append should be enough

    -- * Splitting

    -- Note: splitting the search path does not belong here, it is shell aware
    -- operation. search path is separated by : and : is allowed in paths on
    -- posix. Shell would escape it which needs to be handled.

    , splitRoot
 -- , dropRoot
 -- , dropRelRoot -- if relative then dropRoot
    , splitHead
    , splitTail
    , splitPath
    , splitPath_

    -- * Dir and File
    , splitFile
    , splitDir

    -- * Extensions
    , extensionWord
    , splitExtension
    , splitExtensionBy
 -- , addExtension

    -- * Equality
 -- , processParentRefs
    , normalizeSeparators
 -- , normalize -- separators and /./ components (split/combine)
    , eqPathBytes
    , EqCfg(..)
    , eqCfg
    , eqPathWith
    , eqPath
 -- , commonPrefix -- common prefix of two paths
 -- , eqPrefix -- common prefix is equal to first path
 -- , dropPrefix

    -- * Utilities
    , wordToChar
    , charToWord
    , unsafeIndexChar

    -- * Internal
    , unsafeSplitTopLevel
    , unsafeSplitDrive
    , unsafeSplitUNC
    , splitCompact
    , splitWithFilter
    )
where

#include "assert.hs"

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (chr, ord, isAlpha, toUpper)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8, Word16)
import Foreign (castPtr)
import Foreign.C (CString, CSize(..))
import GHC.Base (unsafeChr, Addr#)
import GHC.Ptr (Ptr(..))
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.MutArray (MutArray)
import Streamly.Internal.Data.MutByteArray (Unbox(..))
import Streamly.Internal.Data.Path (PathException(..))
import Streamly.Internal.Data.Stream (Stream)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.List as List
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Unicode.Stream as Unicode

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

data OS = Windows | Posix deriving Eq

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

------------------------------------------------------------------------------
-- Array utils
------------------------------------------------------------------------------

-- | Index a word in an array and convert it to Char.
unsafeIndexChar :: (Unbox a, Integral a) => Int -> Array a -> Char
unsafeIndexChar i a = wordToChar (Array.unsafeGetIndex i a)

-- XXX put this in array module, we can have Array.fold and Array.foldM
foldArr :: Unbox a => Fold.Fold Identity a b -> Array a -> b
foldArr f arr = runIdentity $ Array.foldM f arr

{-# INLINE countLeadingBy #-}
countLeadingBy :: Unbox a => (a -> Bool) -> Array a -> Int
countLeadingBy p = foldArr (Fold.takeEndBy_ (not . p) Fold.length)

countTrailingBy :: Unbox a => (a -> Bool) -> Array a -> Int
countTrailingBy p = Array.foldRev (Fold.takeEndBy_ (not . p) Fold.length)

------------------------------------------------------------------------------
-- Separator parsing
------------------------------------------------------------------------------

extensionWord :: Integral a => a
extensionWord = charToWord '.'

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
isSeparator Posix c = c == posixSeparator
isSeparator Windows c = (c == windowsSeparator) || (c == posixSeparator)

{-# INLINE isSeparatorWord #-}
isSeparatorWord :: Integral a => OS -> a -> Bool
isSeparatorWord os = isSeparator os . wordToChar

------------------------------------------------------------------------------
-- Separator normalization
------------------------------------------------------------------------------

-- | If the path is @//@ the result is @/@. If it is @a//@ then the result is
-- @a@. On Windows "c:" and "c:/" are different paths, therefore, we do not
-- drop the trailing separator from "c:/" or for that matter a separator
-- preceded by a ':'.
{-# INLINE dropTrailingBy #-}
dropTrailingBy :: (Unbox a, Integral a) =>
    OS -> (a -> Bool) -> Array a -> Array a
dropTrailingBy os p arr =
    let len = Array.length arr
        n = countTrailingBy p arr
        arr1 = fst $ Array.unsafeBreakAt (len - n) arr
     in if n == 0
        then arr
        else if n == len -- "////"
        then fst $ Array.unsafeBreakAt 1 arr
        -- "c:////"
        else if (os == Windows)
                && (Array.unsafeGetIndex (len - n - 1) arr == charToWord ':')
        then fst $ Array.unsafeBreakAt (len - n + 1) arr
        else arr1

{-# INLINE compactTrailingBy #-}
compactTrailingBy :: Unbox a => (a -> Bool) -> Array a -> Array a
compactTrailingBy p arr =
    let len = Array.length arr
        n = countTrailingBy p arr
     in if n <= 1
        then arr
        else fst $ Array.unsafeBreakAt (len - n + 1) arr

{-# INLINE dropTrailingSeparators #-}
dropTrailingSeparators :: (Unbox a, Integral a) => OS -> Array a -> Array a
dropTrailingSeparators os =
    dropTrailingBy os (isSeparator os . wordToChar)

-- | A path starting with a separator.
hasLeadingSeparator :: (Unbox a, Integral a) => OS -> Array a -> Bool
hasLeadingSeparator os a
    | Array.null a = False -- empty path should not occur
    | isSeparatorWord os (Array.unsafeGetIndex 0 a) = True
    | otherwise = False

{-# INLINE hasTrailingSeparator #-}
hasTrailingSeparator :: (Integral a, Unbox a) => OS -> Array a -> Bool
hasTrailingSeparator os path =
    let e = Array.getIndexRev 0 path
     in case e of
            Nothing -> False
            Just x -> isSeparatorWord os x

{-# INLINE toDefaultSeparator #-}
toDefaultSeparator :: Integral a => a -> a
toDefaultSeparator x =
    if isSeparatorWord Windows x
    then charToWord (primarySeparator Windows)
    else x

-- | Change all separators in the path to default separator on windows.
{-# INLINE normalizeSeparators #-}
normalizeSeparators :: (Integral a, Unbox a) => Array a -> Array a
normalizeSeparators a =
    -- XXX We can check and return the original array if no change is needed.
    Array.fromPureStreamN (Array.length a)
        $ fmap toDefaultSeparator
        $ Array.read a

------------------------------------------------------------------------------
-- Windows drive parsing
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
hasDrive a = Array.length a >= 2 && unsafeHasDrive a

-- | A path that contains only an alphabet followed by a colon e.g. @C:@.
isDrive :: (Unbox a, Integral a) => Array a -> Bool
isDrive a = Array.length a == 2 && unsafeHasDrive a

------------------------------------------------------------------------------
-- Relative or Absolute
------------------------------------------------------------------------------

-- | A path relative to cur dir it is either @.@ or starts with @./@.
isRelativeCurDir :: (Unbox a, Integral a) => OS -> Array a -> Bool
isRelativeCurDir os a
    | len == 0 = False -- empty path should not occur
    | wordToChar (Array.unsafeGetIndex 0 a) /= '.' = False
    | len < 2 = True
    | otherwise = isSeparatorWord os (Array.unsafeGetIndex 1 a)

    where

    len = Array.length a

-- | A non-UNC path starting with a separator.
-- Note that "\\/share/x" is treated as "C:/share/x".
isRelativeCurDriveRoot :: (Unbox a, Integral a) => Array a -> Bool
isRelativeCurDriveRoot a
    | len == 0 = False -- empty path should not occur
    | len == 1 && sep0 = True
    | sep0 && c0 /= c1 = True -- "\\/share/x" is treated as "C:/share/x".
    | otherwise = False

    where

    len = Array.length a
    c0 = Array.unsafeGetIndex 0 a
    c1 = Array.unsafeGetIndex 1 a
    sep0 = isSeparatorWord Windows c0

-- | @C:@ or @C:a...@.
isRelativeWithDrive :: (Unbox a, Integral a) => Array a -> Bool
isRelativeWithDrive a =
    hasDrive a
        && (  Array.length a < 3
           || not (isSeparator Windows (unsafeIndexChar 2 a))
           )

isRootRelative :: (Unbox a, Integral a) => OS -> Array a -> Bool
isRootRelative Posix a = isRelativeCurDir Posix a
isRootRelative Windows a =
    isRelativeCurDir Windows a
        || isRelativeCurDriveRoot a
        || isRelativeWithDrive a

-- | @C:\...@. Note that "C:" or "C:a" is not absolute.
isAbsoluteWithDrive :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteWithDrive a =
    Array.length a >= 3
        && unsafeHasDrive a
        && isSeparator Windows (unsafeIndexChar 2 a)

-- | @\\\\...@ or @//...@
isAbsoluteUNC :: (Unbox a, Integral a) => Array a -> Bool
isAbsoluteUNC a
    | Array.length a < 2 = False
    | isSeparatorWord Windows c0 && c0 == c1 = True
    | otherwise = False

    where

    c0 = Array.unsafeGetIndex 0 a
    c1 = Array.unsafeGetIndex 1 a

-- XXX rename to isRootAbsolute

-- | Note that on Windows a path starting with a separator is relative to
-- current drive while on Posix this is absolute path as there is only one
-- drive.
isAbsolute :: (Unbox a, Integral a) => OS -> Array a -> Bool
isAbsolute Posix arr =
    hasLeadingSeparator Posix arr
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
-- XXX Empty path can be taken to mean "." except in case of UNC paths

isRooted :: (Unbox a, Integral a) => OS -> Array a -> Bool
isRooted Posix a =
    hasLeadingSeparator Posix a
        || isRelativeCurDir Posix a
isRooted Windows a =
    hasLeadingSeparator Windows a
        || isRelativeCurDir Windows a
        || hasDrive a -- curdir-in-drive relative, drive absolute

isBranch :: (Unbox a, Integral a) => OS -> Array a -> Bool
isBranch os = not . isRooted os

------------------------------------------------------------------------------
-- Split root
------------------------------------------------------------------------------

unsafeSplitPrefix :: (Unbox a, Integral a) =>
    OS -> Int -> Array a -> (Array a, Array a)
unsafeSplitPrefix os prefixLen arr =
    Array.unsafeBreakAt cnt arr

    where

    afterDrive = snd $ Array.unsafeBreakAt prefixLen arr
    n = countLeadingBy (isSeparatorWord os) afterDrive
    cnt = prefixLen + n

-- Note: We can have normalized splitting functions to normalize as we split
-- for efficiency. But then we will have to allocate new arrays instead of
-- slicing which can make it inefficient.

-- | Split a path prefixed with a separator into (drive, path) tuple.
--
-- >>> toListPosix (a,b) = (unpackPosix a, unpackPosix b)
-- >>> splitPosix = toListPosix . Common.unsafeSplitTopLevel Common.Posix . packPosix
--
-- >>> toListWin (a,b) = (unpackWindows a, unpackWindows b)
-- >>> splitWin = toListWin . Common.unsafeSplitTopLevel Common.Windows . packWindows
--
-- >>> splitPosix "/"
-- ("/","")
--
-- >>> splitPosix "//"
-- ("//","")
--
-- >>> splitPosix "/home"
-- ("/","home")
--
-- >>> splitPosix "/home/user"
-- ("/","home/user")
--
-- >>> splitWin "\\"
-- ("\\","")
--
-- >>> splitWin "\\home"
-- ("\\","home")
unsafeSplitTopLevel :: (Unbox a, Integral a) =>
    OS -> Array a -> (Array a, Array a)
-- Note on Windows we should be here only when the path starts with exactly one
-- separator, otherwise it would be UNC path. But on posix multiple separators
-- are valid.
unsafeSplitTopLevel os = unsafeSplitPrefix os 1

-- In some cases there is no valid drive component e.g. "\\a\\b", though if we
-- consider relative roots then we could use "\\" as the root in this case. In
-- other cases there is no valid path component e.g. "C:" or "\\share\\" though
-- the latter is not a valid path and in the former case we can use "." as the
-- path component.

-- | Split a path prefixed with drive into (drive, path) tuple.
--
-- >>> toList (a,b) = (unpackPosix a, unpackPosix b)
-- >>> split = toList . Common.unsafeSplitDrive . packPosix
--
-- >>> split "C:"
-- ("C:","")
--
-- >>> split "C:a"
-- ("C:","a")
--
-- >>> split "C:\\"
-- ("C:\\","")
--
-- >>> split "C:\\\\" -- this is invalid path
-- ("C:\\\\","")
--
-- >>> split "C:\\\\a" -- this is invalid path
-- ("C:\\\\","a")
--
-- >>> split "C:\\/a/b" -- is this valid path?
-- ("C:\\/","a/b")
unsafeSplitDrive :: (Unbox a, Integral a) => Array a -> (Array a, Array a)
unsafeSplitDrive = unsafeSplitPrefix Windows 2

-- | Skip separators and then parse the next path segment.
-- Return (segment offset, segment length).
parseSegment :: (Unbox a, Integral a) => Array a -> Int -> (Int, Int)
parseSegment arr sepOff = (segOff, segCnt)

    where

    arr1 = snd $ Array.unsafeBreakAt sepOff arr
    sepCnt = countLeadingBy (isSeparatorWord Windows) arr1
    segOff = sepOff + sepCnt

    arr2 = snd $ Array.unsafeBreakAt segOff arr
    segCnt = countLeadingBy (not . isSeparatorWord Windows) arr2

-- XXX We can split a path as "root, . , rest" or "root, /, rest".
-- XXX We can remove the redundant path separator after the root. With that
-- joining root vs other paths will become similar. But there are some special
-- cases e.g. (1) "C:a" does not have a separator, can we make this "C:.\\a"?
-- (2) In case of "/home" we have "/" as root - while joining root and path we
-- should not add another separator between root and path - thus joining root
-- and path in this case is anyway special.

-- | Split a path prefixed with "\\" into (drive, path) tuple.
--
-- >>> toList (a,b) = (unpackPosix a, unpackPosix b)
-- >>> split = toList . Common.unsafeSplitUNC . packPosix
--
-- >> split ""
-- ("","")
--
-- >>> split "\\\\"
-- ("\\\\","")
--
-- >>> split "\\\\server"
-- ("\\\\server","")
--
-- >>> split "\\\\server\\"
-- ("\\\\server\\","")
--
-- >>> split "\\\\server\\home"
-- ("\\\\server\\","home")
--
-- >>> split "\\\\?\\c:"
-- ("\\\\?\\c:","")
--
-- >>> split "\\\\?\\c:/"
-- ("\\\\?\\c:/","")
--
-- >>> split "\\\\?\\c:\\home"
-- ("\\\\?\\c:\\","home")
--
-- >>> split "\\\\?\\UNC/"
-- ("\\\\?\\UNC/","")
--
-- >>> split "\\\\?\\UNC\\server"
-- ("\\\\?\\UNC\\server","")
--
-- >>> split "\\\\?\\UNC/server\\home"
-- ("\\\\?\\UNC/server\\","home")
--
unsafeSplitUNC :: (Unbox a, Integral a) => Array a -> (Array a, Array a)
unsafeSplitUNC arr =
    if cnt1 == 1 && unsafeIndexChar 2 arr == '?'
    then do
        if uncLen == 3
                && unsafeIndexChar uncOff arr == 'U'
                && unsafeIndexChar (uncOff + 1) arr == 'N'
                && unsafeIndexChar (uncOff + 2) arr == 'C'
        then unsafeSplitPrefix Windows (serverOff + serverLen) arr
        else unsafeSplitPrefix Windows sepOff1 arr
    else unsafeSplitPrefix Windows sepOff arr

    where

    arr1 = snd $ Array.unsafeBreakAt 2 arr
    cnt1 = countLeadingBy (not . isSeparatorWord Windows) arr1
    sepOff = 2 + cnt1

    -- XXX there should be only one separator in a valid path?
    -- XXX it should either be UNC or two letter drive in a valid path
    (uncOff, uncLen) = parseSegment arr sepOff
    sepOff1 = uncOff + uncLen
    (serverOff, serverLen) = parseSegment arr sepOff1

-- XXX should we make the root Maybe? Both components will have to be Maybe to
-- avoid an empty path.
-- XXX Should we keep the trailing separator in the directory components?

{-# INLINE splitRoot #-}
splitRoot :: (Unbox a, Integral a) => OS -> Array a -> (Array a, Array a)
-- NOTE: validatePath depends on splitRoot splitting the path without removing
-- any redundant chars etc. It should just split and do nothing else.
-- XXX We can put an assert here "arrLen == rootLen + stemLen".
-- XXX assert (isValidPath path == isValidPath root)
--
-- NOTE: we cannot drop the trailing "/" on the root even if we want to -
-- because "c:/" will become "c:" and the two are not equivalent.
splitRoot Posix arr
    | isRooted Posix arr
        = unsafeSplitTopLevel Posix arr
    | otherwise = (Array.empty, arr)
splitRoot Windows arr
    | isRelativeCurDriveRoot arr || isRelativeCurDir Windows arr
        = unsafeSplitTopLevel Windows arr
    | hasDrive arr = unsafeSplitDrive arr
    | isAbsoluteUNC arr = unsafeSplitUNC arr
    | otherwise = (Array.empty, arr)

------------------------------------------------------------------------------
-- Split path
------------------------------------------------------------------------------

-- | Raw split an array on path separartor word using a filter to filter out
-- some splits.
{-# INLINE splitWithFilter #-}
splitWithFilter
    :: (Unbox a, Integral a, Monad m)
    => ((Int, Int) -> Bool)
    -> Bool
    -> OS
    -> Array a
    -> Stream m (Array a)
splitWithFilter filt withSep os arr =
      f (isSeparatorWord os) (Array.read arr)
    & Stream.filter filt
    & fmap (\(i, len) -> Array.unsafeSliceOffLen i len arr)

    where

    f = if withSep then Stream.indexEndBy else Stream.indexEndBy_

-- | Split a path on separator chars and compact contiguous separators and
-- remove /./ components. Note this does not treat the path root in a special
-- way.
{-# INLINE splitCompact #-}
splitCompact
    :: (Unbox a, Integral a, Monad m)
    => Bool
    -> OS
    -> Array a
    -> Stream m (Array a)
splitCompact withSep os arr =
    splitWithFilter (not . shouldFilterOut) withSep os arr

    where

    sepFilter (off, len) =
        ( len == 1
        && isSeparator os (unsafeIndexChar off arr)
        )
        ||
        -- Note, last component may have len == 2 but second char may not
        -- be slash, so we need to check for slash explicitly.
        --
        ( len == 2
        && unsafeIndexChar off arr == '.'
        && isSeparator os (unsafeIndexChar (off + 1) arr)
        )

    {-# INLINE shouldFilterOut #-}
    shouldFilterOut (off, len) =
        len == 0
            -- Note this is needed even when withSep is true - for the last
            -- component case.
            || (len == 1 && unsafeIndexChar off arr == '.')
            -- XXX Ensure that these are statically removed by GHC when withSep
            -- is False.
            || (withSep && sepFilter (off, len))

{-# INLINE splitPathUsing #-}
splitPathUsing
    :: (Unbox a, Integral a, Monad m)
    => Bool
    -> OS
    -> Array a
    -> Stream m (Array a)
splitPathUsing withSep os arr =
    let stream = splitCompact withSep os rest
    in if Array.null root
       then stream
       else Stream.cons root1 stream

    where

    -- We should not filter out a leading '.' on Posix or Windows.
    -- We should not filter out a '.' in the middle of a UNC root on windows.
    -- Therefore, we split the root and treat it in a special way.
    (root, rest) = splitRoot os arr
    root1 =
        if withSep
        then compactTrailingBy (isSeparator os . wordToChar) root
        else dropTrailingSeparators os root

{-# INLINE splitPath_ #-}
splitPath_
    :: (Unbox a, Integral a, Monad m)
    => OS -> Array a -> Stream m (Array a)
splitPath_ = splitPathUsing False

{-# INLINE splitPath #-}
splitPath
    :: (Unbox a, Integral a, Monad m)
    => OS -> Array a -> Stream m (Array a)
splitPath = splitPathUsing True

-- | Split the first non-empty path component.
--
-- /Unimplemented/
{-# INLINE splitHead #-}
splitHead :: -- (Unbox a, Integral a) =>
    OS -> Array a -> (Array a, Array a)
splitHead _os _arr = undefined

-- | Split the last non-empty path component.
--
-- /Unimplemented/
{-# INLINE splitTail #-}
splitTail :: -- (Unbox a, Integral a) =>
    OS -> Array a -> (Array a, Array a)
splitTail _os _arr = undefined

------------------------------------------------------------------------------
-- File or Dir
------------------------------------------------------------------------------

-- | Returns () if the path can be a valid file, otherwise throws an
-- exception.
validateFile :: (MonadThrow m, Unbox a, Integral a) => OS -> Array a -> m ()
validateFile os arr = do
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

{-# INLINE splitFile #-}
splitFile :: (Unbox a, Integral a) => OS -> Array a -> (Array a, Array a)
splitFile os arr =
    let p x =
            if os == Windows
            then x == charToWord ':' || isSeparatorWord os x
            else isSeparatorWord os x
        -- XXX Use Array.revBreakEndBy?
        fileLen = runIdentity
                $ Stream.fold (Fold.takeEndBy_ p Fold.length)
                $ Array.readRev arr
        arrLen = Array.length arr
        baseLen = arrLen - fileLen
        (base, file) = Array.unsafeBreakAt baseLen arr
        fileFirst = Array.unsafeGetIndex 0 file
        fileSecond = Array.unsafeGetIndex 1 file
     in
        if fileLen > 0
            -- exclude the file == '.' case
            && not (fileLen == 1 && fileFirst == charToWord '.')
            -- exclude the file == '..' case
            && not (fileLen == 2
                && fileFirst == charToWord '.'
                && fileSecond == charToWord '.')
        then
            if baseLen <= 0
            then (Array.empty, arr)
            else (Array.unsafeSliceOffLen 0 baseLen base, file) -- "/"
        else (arr, Array.empty)

-- | Split a multi-component path into (dir, last component). If the path has a
-- single component and it is a root then return (path, "") otherwise return
-- ("", path).
--
-- Split a single component into (dir, "") if it can be a dir i.e. it is either
-- a path root, "." or ".." or has a trailing separator.
--
-- The only difference between splitFile and splitDir:
--
-- >> splitFile "a/b/"
-- ("a/b/", "")
-- >> splitDir "a/b/"
-- ("a/", "b/")
--
-- This is equivalent to splitPath and keeping the last component but is usually
-- faster.
--
-- >>> toList (a,b) = (unpackPosix a, unpackPosix b)
-- >>> splitPosix = toList . Common.splitDir Common.Posix . packPosix
--
-- >> splitPosix "/"
-- ("/","")
--
-- >> splitPosix "."
-- (".","")
--
-- >> splitPosix "/."
-- ("/.","")
--
-- >> splitPosix "/x"
-- ("/","x")
--
-- >> splitPosix "/x/"
-- ("/","x/")
--
-- >> splitPosix "//"
-- ("//","")
--
-- >> splitPosix "./x"
-- ("./","x")
--
-- >> splitPosix "x"
-- ("","x")
--
-- >> splitPosix "x/"
-- ("x/","")
--
-- >> splitPosix "x/y"
-- ("x/","y")
--
-- >> splitPosix "x/y/"
-- ("x/","y/")
--
-- >> splitPosix "x/y//"
-- ("x/","y//")
--
-- >> splitPosix "x//y"
-- ("x//","y")
--
-- >> splitPosix "x/./y"
-- ("x/./","y")
--
-- /Unimplemented/
{-# INLINE splitDir #-}
splitDir :: -- (Unbox a, Integral a) =>
    OS -> Array a -> (Array a, Array a)
splitDir _os _arr = undefined

------------------------------------------------------------------------------
-- Split extensions
------------------------------------------------------------------------------

-- | Like split extension but we can specify the extension char to be used.
{-# INLINE splitExtensionBy #-}
splitExtensionBy :: (Unbox a, Integral a) =>
    a -> OS -> Array a -> (Array a, Array a)
splitExtensionBy c os arr =
    let p x = x == c || isSeparatorWord os x
        -- XXX Use Array.revBreakEndBy_
        extLen = runIdentity
                $ Stream.fold (Fold.takeEndBy p Fold.length)
                $ Array.readRev arr
        arrLen = Array.length arr
        baseLen = arrLen - extLen
        -- XXX We can use reverse split operation on the array
        res@(base, ext) = Array.unsafeBreakAt baseLen arr
        baseLast = Array.unsafeGetIndexRev 0 base
        extFirst = Array.unsafeGetIndex 0 ext
     in
        -- For an extension to be present the path must be at least 3 chars.
        -- non-empty base followed by extension char followed by non-empty
        -- extension.
        if arrLen > 2
            -- If ext is empty, then there is no extension and we should not
            -- strip an extension char if any at the end of base.
            && extLen > 1
            && extFirst == c
            -- baseLast is always either base name char or '/' unless empty
            -- if baseLen is 0 then we have not found an extension.
            && baseLen > 0
            -- If baseLast is '/' then base name is empty which means it is a
            -- dot file and there is no extension.
            && not (isSeparatorWord os baseLast)
            -- On Windows if base is 'c:.' or a UNC path ending in '/c:.' then
            -- it is a dot file, no extension.
            && not (os == Windows && baseLast == charToWord ':')
        then res
        else (arr, Array.empty)

{-# INLINE splitExtension #-}
splitExtension :: (Unbox a, Integral a) => OS -> Array a -> (Array a, Array a)
splitExtension = splitExtensionBy extensionWord

{-
-- Instead of this keep calling splitExtension until there is no more extension
-- returned.
{-# INLINE splitAllExtensionsBy #-}
splitAllExtensionsBy :: (Unbox a, Integral a) =>
    Bool -> a -> OS -> Array a -> (Array a, Array a)
-- If the isFileName arg is true, it means that the path supplied does not have
-- any separator chars, so we can do it more efficiently.
splitAllExtensionsBy isFileName extChar os arr =
    let file =
            if isFileName
            then arr
            else snd $ splitFile os arr
        fileLen = Array.length file
        arrLen = Array.length arr
        baseLen = foldArr (Fold.takeEndBy_ (== extChar) Fold.length) file
        extLen = fileLen - baseLen
     in
        -- XXX unsafeBreakAt itself should use Array.empty in case of no split
        if fileLen > 0 && extLen > 1 && extLen /= fileLen
        then (Array.unsafeBreakAt (arrLen - extLen) arr)
        else (arr, Array.empty)

-- |
--
-- TODO: This function needs to be consistent with splitExtension. It should
-- strip all valid extensions by that definition.
--
-- splitAllExtensions "x/y.tar.gz" gives ("x/y", ".tar.gz")
--
-- >>> toList (a,b) = (unpackPosix a, unpackPosix b)
-- >>> splitPosix = toList . Common.splitAllExtensions Common.Posix . packPosix
--
-- >>> toListWin (a,b) = (unpackWindows a, unpackWindows b)
-- >>> splitWin = toListWin . Common.splitAllExtensions Common.Windows . packWindows
--
-- >>> splitPosix "/"
-- ("/","")
--
-- >>> splitPosix "."
-- (".","")
--
-- >>> splitPosix "x"
-- ("x","")
--
-- >>> splitPosix "/x"
-- ("/x","")
--
-- >>> splitPosix "x/"
-- ("x/","")
--
-- >>> splitPosix "./x"
-- ("./x","")
--
-- >>> splitPosix "x/."
-- ("x/.","")
--
-- >>> splitPosix "x/y."
-- ("x/y.","")
--
-- >>> splitPosix "/x.y"
-- ("/x",".y")
--
-- >>> splitPosix "x/.y"
-- ("x/.y","")
--
-- >>> splitPosix ".x"
-- (".x","")
--
-- >>> splitPosix "x."
-- ("x.","")
--
-- >>> splitPosix ".x.y"
-- (".x",".y")
--
-- >>> splitPosix "x/y.z"
-- ("x/y",".z")
--
-- >>> splitPosix "x.y.z"
-- ("x",".y.z")
--
-- >>> splitPosix "x..y" -- ??
-- ("x.",".y")
--
-- >>> splitPosix ".."
-- ("..","")
--
-- >>> splitPosix "..."
-- ("...","")
--
-- >>> splitPosix "...x"
-- ("...x","")
--
-- >>> splitPosix "x/y.z/"
-- ("x/y.z/","")
--
-- >>> splitPosix "x/y"
-- ("x/y","")
--
-- >>> splitWin "x:y"
-- ("x:y","")
--
-- >>> splitWin "x:.y"
-- ("x:.y","")
--
{-# INLINE splitAllExtensions #-}
splitAllExtensions :: (Unbox a, Integral a) =>
    OS -> Array a -> (Array a, Array a)
splitAllExtensions = splitAllExtensionsBy False extensionWord
-}

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

{-# INLINE isInvalidPathChar #-}
isInvalidPathChar :: Integral a => OS -> a -> Bool
isInvalidPathChar Posix x = x == 0
isInvalidPathChar Windows x =
    -- case should be faster than list search
    case x of
        34 -> True -- '"'
        42 -> True -- '*'
        58 -> True -- ':'
        60 -> True -- '<'
        62 -> True -- '>'
        63 -> True -- '?'
        124 -> True -- '|'
        _ -> x <= charToWord '\US'

countLeadingValid :: (Unbox a, Integral a) => OS -> Array a -> Int
countLeadingValid os path =
    let f = Fold.takeEndBy_ (isInvalidPathChar os) Fold.length
     in foldArr f path

-- XXX Supply it an array for checking and use a more efficient prefix matching
-- check.

-- | Only for windows.
isInvalidPathComponent :: Integral a => [[a]]
isInvalidPathComponent = fmap (fmap charToWord)
    [ "CON","PRN","AUX","NUL","CLOCK$"
    , "COM1","COM2","COM3","COM4","COM5","COM6","COM7","COM8","COM9"
    , "LPT1","LPT2","LPT3","LPT4","LPT5","LPT6","LPT7","LPT8","LPT9"
    ]

{- HLINT ignore "Use when" -}
validatePathWith :: (MonadThrow m, Integral a, Unbox a) =>
    Bool -> OS -> Array a -> m ()
validatePathWith _ Posix path =
    let pathLen = Array.length path
        validLen = countLeadingValid Posix path
     in if pathLen == 0
        then throwM $ InvalidPath "Empty path"
        else if pathLen /= validLen
        then throwM $ InvalidPath
            $ "Null char found after " ++ show validLen ++ " characters."
        else pure ()
validatePathWith allowRoot Windows path
  | Array.null path = throwM $ InvalidPath "Empty path"
  | otherwise = do
        if hasDrive path && postDriveSep > 1 -- "C://"
        then throwM $ InvalidPath
            "More than one separators between drive root and the path"
        else if isAbsoluteUNC path
        then
            if postDriveSep > 1 -- "///x"
            then throwM $ InvalidPath
                "Path starts with more than two separators"
            else if invalidRootComponent -- "//prn/x"
            then throwM $ InvalidPath
                -- XXX print the invalid component name
                "Special filename component found in share root"
            else if rootEndSeps /= 1 -- "//share//x"
            then throwM $ InvalidPath
                $ "Share name is needed and exactly one separator is needed "
                ++ "after the share root"
            else if not allowRoot && Array.null stem -- "//share/"
            then throwM $ InvalidPath
                "the share root must be followed by a non-empty path"
            else pure ()
        else pure ()

        if stemLen /= validStemLen -- "x/x>y"
        then throwM $ InvalidPath
            $ "Disallowed char found after "
            ++ show (rootLen + validStemLen)
            ++ " characters. The invalid char is: "
            ++ show (chr (fromIntegral invalidVal))
            ++ " [" ++ show invalidVal ++ "]"
        else if invalidComponent -- "x/prn/y"
        -- XXX print the invalid component name
        then throwM $ InvalidPath "Disallowed Windows filename in path"
        else pure ()

    where

    postDrive = snd $ Array.unsafeBreakAt 2 path
    postDriveSep = countLeadingBy (isSeparatorWord Windows) postDrive

    -- XXX check invalid chars in the path root as well - except . and '?'?
    (root, stem) = splitRoot Windows path
    rootLen = Array.length root
    stemLen = Array.length stem
    validStemLen = countLeadingValid Windows stem
    invalidVal = fromIntegral (Array.unsafeGetIndex validStemLen stem) :: Word16

    rootEndSeps  = countTrailingBy (isSeparatorWord Windows) root

    -- TBD: We are not currently validating the sharenames against disallowed
    -- file names. Apparently windows does not allow even sharenames with those
    -- names. To match against sharenames we will have to strip the separators
    -- and drive etc from the root. Or we can use the parsing routines
    -- themselves to validate.
    toUp w16 =
        if w16 < 256
        then charToWord $ toUpper (wordToChar w16)
        else w16

    -- Should we strip all space chars as in Data.Char.isSpace?
    isSpace x = x == charToWord ' '

    -- XXX instead of using a list based check, pass the array to the checker.
    -- We do not need to upcase the array, it can be done in the checker. Thus
    -- we do not need to create a new array, the original slice can be checked.
    getBaseName x =
          runIdentity
        $ Stream.toList
        $ fmap toUp
        $ Array.read
        $ Array.dropAround isSpace
        $ fst $ Array.breakEndBy_ (== extensionWord) x

    components =
          runIdentity
        . Stream.toList
        . fmap getBaseName
        . splitCompact False Windows

    invalidRootComponent =
        List.any (`List.elem` isInvalidPathComponent) (components root)
    invalidComponent =
        List.any (`List.elem` isInvalidPathComponent) (components stem)

-- | A valid root, share root or a valid path.
{-# INLINE validatePath #-}
validatePath :: (MonadThrow m, Integral a, Unbox a) => OS -> Array a -> m ()
validatePath = validatePathWith True

-- | Like validatePath but on Windows only full paths are allowed, path roots
-- only are not allowed. Thus "//x/" is not valid.
{-# INLINE validatePath' #-}
validatePath' :: (MonadThrow m, Integral a, Unbox a) => OS -> Array a -> m ()
validatePath' = validatePathWith False

{-# INLINE isValidPath #-}
isValidPath :: (Integral a, Unbox a) => OS -> Array a -> Bool
isValidPath os path =
    case validatePath os path of
        Nothing -> False
        Just _ -> True

-- |
-- >>> isValidWin = Common.isValidPath' Common.Windows . packWindows
--
-- The following roots allowed by isValidPath are not allowed:
--
-- >>> isValidWin "\\\\x\\"
-- False
-- >>> isValidWin "\\\\?\\UNC\\x"
-- False
{-# INLINE isValidPath' #-}
isValidPath' :: (Integral a, Unbox a) => OS -> Array a -> Bool
isValidPath' os path =
    case validatePath' os path of
        Nothing -> False
        Just _ -> True

{-# INLINE unsafeFromChunk #-}
unsafeFromChunk :: Array a -> Array a
unsafeFromChunk = id

{-# INLINE fromChunk #-}
fromChunk :: forall m a. (MonadThrow m, Unbox a, Integral a) =>
    OS -> Array a -> m (Array a)
fromChunk os arr = validatePath os arr >> pure arr
{-
    let arr1 = Array.unsafeCast arr :: Array a
     in validatePath os arr1 >> pure arr1
fromChunk Windows arr =
    case Array.cast arr of
        Nothing ->
            throwM
                $ InvalidPath
                $ "Encoded path length " ++ show (Array.byteLength arr)
                    ++ " is not a multiple of 16-bit."
        Just x -> validatePath Windows x >> pure x
-}

-- | Convert 'Path' to an array of bytes.
{-# INLINE toChunk #-}
toChunk :: Array a -> Array Word8
toChunk = Array.asBytes

{-# INLINE unsafeFromChars #-}
unsafeFromChars :: (Unbox a) =>
       (Stream Identity Char -> Stream Identity a)
    -> Stream Identity Char
    -> Array a
unsafeFromChars encode s =
    let n = runIdentity $ Stream.fold Fold.length s
     in Array.fromPureStreamN n (encode s)

-- XXX Writing a custom fold for parsing a Posix path may be better for
-- efficient bulk parsing when needed. We need the same code to validate a
-- Chunk where we do not need to create an array.
{-# INLINE fromChars #-}
fromChars :: (MonadThrow m, Unbox a, Integral a) =>
       OS
    -> (Stream Identity Char -> Stream Identity a)
    -> Stream Identity Char
    -> m (Array a)
fromChars os encode s =
    let arr = unsafeFromChars encode s
     in fromChunk os (Array.unsafeCast arr)

{-# INLINE toChars #-}
toChars :: (Monad m, Unbox a) =>
    (Stream m a -> Stream m Char) -> Array a -> Stream m Char
toChars decode arr = decode $ Array.read arr

{-# INLINE toString #-}
toString :: Unbox a =>
    (Stream Identity a -> Stream Identity Char) -> Array a -> [Char]
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
-- Operations of Path
------------------------------------------------------------------------------

-- See also cstringLength# in GHC.CString in ghc-prim
foreign import ccall unsafe "string.h strlen" c_strlen_pinned
    :: Addr# -> IO CSize

{-# INLINE appendCStringWith #-}
appendCStringWith ::
       (Int -> IO (MutArray Word8))
    -> OS
    -> Array Word8
    -> CString
    -> IO (Array Word8)
appendCStringWith create os a b@(Ptr addrB#) = do
    let lenA = Array.length a
    lenB <- fmap fromIntegral $ c_strlen_pinned addrB#
    assertM(lenA /= 0 && lenB /= 0)
    let len = lenA + 1 + lenB
    arr <- create len
    arr1 <- MutArray.unsafeSplice arr (Array.unsafeThaw a)
    arr2 <- MutArray.unsafeSnoc arr1 (charToWord (primarySeparator os))
    arr3 :: MutArray.MutArray Word8 <-
        MutArray.unsafeAppendPtrN arr2 (castPtr b) lenB
    return (Array.unsafeFreeze arr3)

{-# INLINE appendCString #-}
appendCString :: OS -> Array Word8 -> CString -> IO (Array Word8)
appendCString = appendCStringWith MutArray.emptyOf

{-# INLINE appendCString' #-}
appendCString' :: OS -> Array Word8 -> CString -> IO (Array Word8)
appendCString' = appendCStringWith MutArray.emptyOf'

{-# INLINE doAppend #-}
doAppend :: (Unbox a, Integral a) => OS -> Array a -> Array a -> Array a
doAppend os a b = unsafePerformIO $ do
    let lenA = Array.length a
        lenB = Array.length b
    assertM(lenA /= 0 && lenB /= 0)
    let lastA = Array.unsafeGetIndexRev 0 a
        sepA = isSeparatorWord os lastA
        sepB = isSeparatorWord os (Array.unsafeGetIndex 0 b)
    let len = lenA + 1 + lenB
    arr <- MutArray.emptyOf len
    arr1 <- MutArray.unsafeSplice arr (Array.unsafeThaw a)
    arr2 <-
            if     lenA /= 0
                && lenB /= 0
                && not sepA
                && not sepB
                && not (os == Windows && lastA == charToWord ':')
            then MutArray.unsafeSnoc arr1 (charToWord (primarySeparator os))
            else pure arr1
    -- Note: if the last char on the first array is ":" and first char on the
    -- second array is "/" then we cannot drop the "/". We drop only if both
    -- are separators excluding ":".
    let arrB =
            if sepA && sepB
            then snd $ Array.unsafeBreakAt 1 b
            else b
    arr3 <- MutArray.unsafeSplice arr2 (Array.unsafeThaw arrB)
    return (Array.unsafeFreeze arr3)

{-# INLINE withAppendCheck #-}
withAppendCheck :: (Unbox b, Integral b) =>
    OS -> (Array b -> String) -> Array b -> a -> a
withAppendCheck os toStr arr f =
    if isRooted os arr
    then error $ "append: cannot append a rooted path " ++ toStr arr
    else f

{-# INLINE unsafeAppend #-}
unsafeAppend :: (Unbox a, Integral a) =>
    OS -> (Array a -> String) -> Array a -> Array a -> Array a
unsafeAppend os _toStr = doAppend os

{-# INLINE append #-}
append :: (Unbox a, Integral a) =>
    OS -> (Array a -> String) -> Array a -> Array a -> Array a
append os toStr a b = withAppendCheck os toStr b (doAppend os a b)

{-# INLINE append' #-}
append' :: (Unbox a, Integral a) =>
    OS -> (Array a -> String) -> Array a -> Array a -> Array a
append' os toStr a b =
    let hasSep = countTrailingBy (isSeparatorWord os) a > 0
        hasColon =
               os == Windows
            && Array.getIndexRev 0 a == Just (charToWord ':')
     in if hasSep || hasColon
        then withAppendCheck os toStr b (doAppend os a b)
        else error
                $ "append': first path must be dir like i.e. must have a "
                ++ "trailing separator or colon on windows: " ++ toStr a

-- XXX MonadIO?

-- | Join paths by path separator. Does not check if the paths being appended
-- are rooted or path segments. Note that splitting and joining may not give
-- exactly the original path but an equivalent, normalized path.
{-# INLINE unsafeJoinPaths #-}
unsafeJoinPaths
    :: (Unbox a, Integral a, MonadIO m)
    => OS -> Stream m (Array a) -> m (Array a)
unsafeJoinPaths os =
    -- XXX This can be implemented more efficiently using an Array intersperse
    -- operation. Which can be implemented by directly copying arrays rather
    -- than converting them to stream first. Also fromStreamN would be more
    -- efficient if we have to use streams.
    -- XXX We can remove leading and trailing separators first, if any, except
    -- the leading separator from the first path. But it is not necessary.
    -- Instead we can avoid adding a separator if it is already present.
    Array.fromStream . Array.concatSepBy (charToWord $ primarySeparator os)

------------------------------------------------------------------------------
-- Equality
------------------------------------------------------------------------------

eqPathBytes :: Array a -> Array a -> Bool
eqPathBytes = Array.byteEq

-- On posix macOs can have case insensitive comparison. On Windows also
-- case sensitive behavior may depend on the file system being used.

-- Use eq prefix?

-- | Options for path comparison operation. By default path comparison uses a
-- strict criteria for equality. The following options are provided to
-- control the strictness.
data EqCfg =
    EqCfg
    { ignoreTrailingSeparators :: Bool -- ^ Allows "x\/" == "x"
    , ignoreCase :: Bool               -- ^ Allows "x" == \"X\"
    , allowRelativeEquality :: Bool
    -- ^ A leading dot is ignored, thus ".\/x" == ".\/x" and ".\/x" == "x".
    -- On Windows allows "\/x" == \/x" and "C:x == C:x"

    -- , resolveParentReferences -- "x\/..\/y" == "y"
    -- , noIgnoreRedundantSeparators -- "x\/\/y" \/= "x\/y"
    -- , noIgnoreRedundantDot -- "x\/.\/" \/= "x"
    }

-- | Default equality check configuration.
--
-- > :{
-- > eqCfg = EqCfg
-- >     { ignoreTrailingSeparators = False
-- >     , ignoreCase = False
-- >     , allowRelativeEquality = False
-- >     }
-- > :}
--
eqCfg :: EqCfg
eqCfg = EqCfg
    { ignoreTrailingSeparators = False
    , ignoreCase = False
    , allowRelativeEquality = False
    }

data PosixRoot = PosixRootAbs | PosixRootRel deriving Eq

data WindowsRoot =
      WindowsRootPosix -- /x or ./x
    | WindowsRootNonPosix -- C:... or \\...
    deriving Eq

-- | Change to upper case and replace separators by primary separator
{-# INLINE normalizeCaseAndSeparators #-}
normalizeCaseAndSeparators :: Monad m => Array Word16 -> Stream m Char
normalizeCaseAndSeparators =
      fmap toUpper
    . Unicode.decodeUtf16le'
    . fmap toDefaultSeparator
    . Array.read

{-# INLINE normalizeCaseWith #-}
normalizeCaseWith :: (Monad m, Unbox a) =>
    (Stream m a -> Stream m Char) -> Array a -> Stream m Char
normalizeCaseWith decoder =
      fmap toUpper
    . decoder
    . Array.read

eqWindowsRootStrict :: (Unbox a, Integral a) =>
    Bool -> Array a -> Array a -> Bool
eqWindowsRootStrict ignCase a b =
    let f = normalizeCaseAndSeparators
     in if ignCase
        then
            -- XXX We probably do not want to equate UNC with UnC etc.
            runIdentity
                $ Stream.eqBy (==)
                    (f $ Array.unsafeCast a) (f $ Array.unsafeCast b)
        else
            runIdentity
                $ Stream.eqBy (==)
                    (fmap toDefaultSeparator $ Array.read a)
                    (fmap toDefaultSeparator $ Array.read b)

{-# INLINE eqRootStrict #-}
eqRootStrict :: (Unbox a, Integral a) =>
    Bool -> OS -> Array a -> Array a -> Bool
eqRootStrict _ Posix a b =
    -- a can be "/" and b can be "//"
    -- We call this only when the roots are either absolute or null.
    Array.null a == Array.null b
eqRootStrict ignCase Windows a b = eqWindowsRootStrict ignCase a b

-- | Compare Posix roots or Windows roots without a drive or share name.
{-# INLINE eqPosixRootLax #-}
eqPosixRootLax :: (Unbox a, Integral a) => Array a -> Array a -> Bool
eqPosixRootLax a b = getRoot a == getRoot b

    where

    -- Can only be either "", '.', './' or '/' (or Windows separators)
    getRoot arr =
        if Array.null arr || unsafeIndexChar 0 arr == '.'
        then PosixRootRel
        else PosixRootAbs

{-# INLINABLE eqRootLax #-}
eqRootLax :: (Unbox a, Integral a) => Bool -> OS -> Array a -> Array a -> Bool
eqRootLax _ Posix a b = eqPosixRootLax a b
eqRootLax ignCase Windows a b =
    let aType = getRootType a
        bType = getRootType b
     in aType == bType
        && (
            (aType == WindowsRootPosix && eqPosixRootLax a b)
            || eqWindowsRootStrict ignCase a b
           )

    where

    getRootType arr =
        if isAbsoluteUNC arr || hasDrive arr
        then WindowsRootNonPosix
        else WindowsRootPosix

{-# INLINE eqComponentsWith #-}
eqComponentsWith :: (Unbox a, Integral a) =>
       Bool
    -> (Stream Identity a -> Stream Identity Char)
    -> OS
    -> Array a
    -> Array a
    -> Bool
eqComponentsWith ignCase decoder os a b =
    if ignCase
    then
        let streamEq x y = runIdentity $ Stream.eqBy (==) x y
            toComponents = fmap (normalizeCaseWith decoder) . splitPath_ os
        -- XXX check perf/fusion
         in runIdentity
                $ Stream.eqBy streamEq (toComponents a) (toComponents b)
    else
        runIdentity
            $ Stream.eqBy
                Array.byteEq (splitPath_ os a) (splitPath_ os b)

-- XXX can we do something like SpecConstr for such functions e.g. without
-- inlining the function we can use two copies one for allowRelativeEquality
-- True and other for False and so on for other values of PathEq.

{-# INLINE eqPathWith #-}
eqPathWith :: (Unbox a, Integral a) =>
    (Stream Identity a -> Stream Identity Char)
    -> OS -> EqCfg -> Array a -> Array a -> Bool
eqPathWith decoder os EqCfg{..} a b =
    let (rootA, stemA) = splitRoot os a
        (rootB, stemB) = splitRoot os b

        eqRelative =
               if allowRelativeEquality
               then eqRootLax ignoreCase os rootA rootB
               else (not (isRootRelative os rootA)
                    && not (isRootRelative os rootB))
                    && eqRootStrict ignoreCase os rootA rootB

        -- XXX If one ends in a "." and the other ends in ./ (and same for ".."
        -- and "../") then they can be equal. We can append a slash in these two
        -- cases before comparing.
        eqTrailingSep =
            ignoreTrailingSeparators
                || hasTrailingSeparator os a == hasTrailingSeparator os b

     in
           eqRelative
        && eqTrailingSep
        && eqComponentsWith ignoreCase decoder os stemA stemB

{-# INLINE eqPath #-}
eqPath :: (Unbox a, Integral a) =>
    (Stream Identity a -> Stream Identity Char)
        -> OS -> Array a -> Array a -> Bool
eqPath decoder os = eqPathWith decoder os eqCfg
