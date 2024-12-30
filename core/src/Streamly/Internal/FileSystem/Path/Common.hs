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
    , splitRoot
 -- , dropRoot
 -- , joinRoot
    , splitPath
    , unsafeJoinPaths
 -- , processParentRefs

    -- * Utilities
    , wordToChar
    , charToWord
    , unsafeIndexChar

    -- * Internal
    , unsafeSplitTopLevel
    , unsafeSplitDrive
    , unsafeSplitUNC
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

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord, isAlpha)
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

countWhile :: (a -> Bool) -> Stream Identity a -> Int
countWhile p =
      runIdentity
    . Stream.fold Fold.length
    . Stream.takeWhile p

{-# INLINE countLeadingBy #-}
countLeadingBy :: Unbox a => (a -> Bool) -> Array a -> Int
countLeadingBy p = countWhile p . Array.read

countTrailingBy :: Unbox a => (a -> Bool) -> Array a -> Int
countTrailingBy p = countWhile p . Array.readRev

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
    | wordToChar (Array.getIndexUnsafe 0 a) /= '.' = False
    | len < 2 = True
    | otherwise = isSeparatorWord os (Array.getIndexUnsafe 1 a)

    where

    len = Array.length a

-- | A path starting with a separator.
hasLeadingSeparator :: (Unbox a, Integral a) => OS -> Array a -> Bool
hasLeadingSeparator os a
    | Array.length a == 0 = False -- empty path should not occur
    | isSeparatorWord os (Array.getIndexUnsafe 0 a) = True
    | otherwise = False

-- | A non-UNC path starting with a separator.
isRelativeCurDriveRoot :: (Unbox a, Integral a) => Array a -> Bool
isRelativeCurDriveRoot a
    | len == 0 = False -- empty path should not occur
    | len == 1 && sep0 = True
    | sep0 && c0 /= c1 = True
    | otherwise = False

    where

    len = Array.length a
    c0 = Array.getIndexUnsafe 0 a
    c1 = Array.getIndexUnsafe 1 a
    sep0 = isSeparatorWord Windows c0

-- | @C:@ or @C:a...@.
isRelativeWithDrive :: (Unbox a, Integral a) => Array a -> Bool
isRelativeWithDrive a =
    hasDrive a
        && (  Array.length a < 3
           || not (isSeparator Windows (unsafeIndexChar 2 a))
           )

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

    c0 = Array.getIndexUnsafe 0 a
    c1 = Array.getIndexUnsafe 1 a

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
--
-- XXX "//share/x" works in powershell. But mixed forward and backward slashes
-- do not work, it is treated as a path relative to current drive e.g.
-- "\\/share/x" is treated as "C:/share/x".
--
-- Invalid paths:
-- "C:\\\\"
-- "C:\\\\x"
-- "\\\\"
-- "\\\\share"
-- "\\\\share\\"
-- "\\\\share\\\\"
-- "\\\\share\\\\x"
-- "\\\\?\\c:"
-- "\\\\?\\c:\\\\\\"

-- | Any path that starts with a separator, @./@ or a drive prefix is a rooted
-- path.
--
-- Rooted paths on Posix and Windows,
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
    hasLeadingSeparator Posix a
        || isRelativeCurDir Posix a
isLocation Windows a =
    hasLeadingSeparator Windows a
        || isRelativeCurDir Windows a
        || hasDrive a -- curdir-in-drive relative, drive absolute

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
    let lenA = Array.length a
        lenB = Array.length b
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

------------------------------------------------------------------------------
-- Splitting
------------------------------------------------------------------------------

unsafeSplitPrefix :: (Unbox a, Integral a) =>
    OS -> Int -> Array a -> (Array a, Array a)
unsafeSplitPrefix os prefixLen arr = (drive, path)

    where

    len = Array.length arr
    -- XXX Array.readFrom may be useful here
    afterDrive = Array.getSliceUnsafe prefixLen (len - prefixLen) arr
    n = countLeadingBy (isSeparatorWord os) afterDrive
    cnt = prefixLen + n
    drive = Array.getSliceUnsafe 0 cnt arr
    path = Array.getSliceUnsafe cnt (len - cnt) arr

-- XXX We can produce a normalized result for the drive during split.

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
--
-- XXX Note, on windows C:\\\\x is an invalid path.

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
parseSegment :: (Unbox a, Integral a) => Array a -> Int -> Int -> (Int, Int)
parseSegment arr len sepOff = (segOff, segCnt)

    where

    arr1 = Array.getSliceUnsafe sepOff (len - sepOff) arr
    sepCnt = countLeadingBy (isSeparatorWord Windows) arr1
    segOff = sepOff + sepCnt

    arr2 = Array.getSliceUnsafe segOff (len - segOff) arr
    segCnt = countLeadingBy (not . isSeparatorWord Windows) arr2

-- XXX We can split a path as "root, . , rest" or "root, /, rest".
-- XXX We can remove the redundant path separator after the root. With that
-- joining root vs other paths will become similar. But there are some special
-- cases e.g. "C:a" does not have a separator, can we make this "C:.\\a"? In
-- case of "/home" we have "/" as root and we cannot add another separator
-- between this and the rest of the path.

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

    len = Array.length arr
    arr1 = Array.getSliceUnsafe 2 (len - 2) arr
    cnt1 = countLeadingBy (not . isSeparatorWord Windows) arr1
    sepOff = 2 + cnt1

    -- XXX there should be only one separator in a valid path?
    -- XXX it should either be UNC or two letter drive in a valid path
    (uncOff, uncLen) = parseSegment arr len sepOff
    sepOff1 = uncOff + uncLen
    (serverOff, serverLen) = parseSegment arr len sepOff1

-- XXX should we make the root Maybe? Both components will have to be Maybe to
-- avoid an empty path.

-- | If a path is rooted then separate the root and the remaining path
-- otherwise root is returned as empty.
--
-- >>> toList (a,b) = (unpackPosix a, unpackPosix b)
-- >>> splitPosix = toList . Common.splitRoot Common.Posix . packPosix
--
-- >>> splitPosix "/"
-- ("/","")
--
-- >>> splitPosix "."
-- (".","")
--
-- >>> splitPosix "/home"
-- ("/","home")
--
-- >>> splitPosix "//"
-- ("//","")
--
-- >>> splitPosix "./home"
-- ("./","home")
--
-- >>> splitPosix "home"
-- ("","home")
--
{-# INLINE splitRoot #-}
splitRoot :: (Unbox a, Integral a) => OS -> Array a -> (Array a, Array a)
splitRoot Posix arr
    | isLocation Posix arr
        = unsafeSplitTopLevel Posix arr
    | otherwise = (Array.empty, arr)
splitRoot Windows arr
    | isRelativeCurDriveRoot arr || isRelativeCurDir Windows arr
        = unsafeSplitTopLevel Windows arr
    | hasDrive arr = unsafeSplitDrive arr
    | isAbsoluteUNC arr = unsafeSplitUNC arr
    | otherwise = (Array.empty, arr)

-- | Split a path into components separated by the path separator. "."
-- components in the path are ignored except in the leading position. Multiple
-- consecutive separators are ignored.
--
-- >>> :{
--  splitPosix = Stream.toList . fmap unpackPosix . Common.splitPath Common.Posix . packPosix
--  splitWin = Stream.toList . fmap unpackWindows . Common.splitPath Common.Windows . packWindows
-- :}
--
-- >>> splitPosix "."
-- ["."]
--
-- >>> splitPosix "././"
-- ["./"]
--
-- >>> splitPosix "./a/b/."
-- ["./","a","b"]
--
-- >>> splitPosix "/"
-- ["/"]
--
-- >>> splitPosix "/home"
-- ["/","home"]
--
-- >>> splitWin "/home"
-- ["/","home"]
--
-- >>> splitPosix "home//user/./..////\\directory/."
-- ["home","user","..","\\directory"]
--
-- >>> splitWin "home//user/./..////\\directory/."
-- ["home","user","..","directory"]
--
{-# INLINE splitPath #-}
splitPath
    :: (Unbox a, Integral a, MonadIO m)
    => OS -> Array a -> Stream m (Array a)
splitPath os arr =
    let stream =
              Stream.indexEndBy_ (isSeparatorWord os) (Array.read rest)
            & Stream.filter (not . shouldFilterOut)
            & fmap (\(i, len) -> Array.getSliceUnsafe i len rest)

    in if Array.length root == 0
       then stream
       else Stream.cons root stream

    where

    (root, rest) = splitRoot os arr

    shouldFilterOut (off, len) =
        len == 0 ||
            (len == 1 && unsafeIndexChar off rest == '.')

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
    -- XXX We can remove leading and trailing separators first, if any except
    -- the leading separator from the first path. But it is not necessary.
    -- Instead we can avoid adding a separator if it is already present.
    Array.fromStream . Array.concatSepBy (charToWord $ primarySeparator os)
