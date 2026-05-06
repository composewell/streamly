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
    , validatePath
    , validatePath'
    , validateFile

    -- * Construction
    , fromArray
    , unsafeFromArray
    , fromChars
    , unsafeFromChars

    -- * Quasiquoters
    , mkQ

    -- * Elimination
    , toString
    , toChars

    -- * Separators
    , primarySeparator
    , isSeparator
    , isSeparatorWord
    , dropTrailingSeparators
    , dropTrailingBy
    , hasTrailingSeparator
    , hasLeadingSeparator

    -- XXX isRooted/isAbsolute can include the absolute roots, / on unix and
    -- drives and shares on Windows. Remove isRooted. Whereas isAnchored in
    -- addition to roots can also include the local anchors like fixed drive or
    -- absolute dir within a drive. On posix isAbsolute and isAnchored would be
    -- identical.

    -- * Tests
    , isBranch
    , isRooted
    , isAbsolute
 -- , isRelative -- not isAbsolute
    , hasRelativeRoot
    , isRelativeWithDrive -- XXX hasRelativeDriveRoot
    , hasDrive

    -- * Joining
    , append
    , append'
    , unsafeAppend
    , appendCString
    , appendCString'
    , appendCWString
    , appendCWString'
    , unsafeJoinPaths
 -- , joinRoot -- XXX append should be enough, see joinRootBody

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
    -- , splitPathRaw -- do not drop any separators or . components
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
    , usePrimarySeparator
    , eqPathBytes
    , EqCfg(..)
    , eqPath

    -- * Normalization
    , collapseSeparators
    , dropDotSegments
    , collapseDotDots
    , normalise -- same spelling as in the filepath package

    -- * Path prefix
    , takeCommonPrefix
    , stripPrefix

    -- * Utilities
    , wordToChar
    , charToWord
    , unsafeIndexChar

    -- * Internal
    , unsafeSplitLeadingSep
    , unsafeSplitDrive
    , unsafeSplitUNC
    , splitPathBodyNormalized
    , splitWithFilter
    )
where

#include "assert.hs"

import Control.Monad (when)
import Control.Monad.Catch (MonadThrow(..))
import Data.Char (chr, ord, isAlpha, toUpper)
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8, Word16)
import Foreign (castPtr)
import Foreign.C (CString, CSize(..), CWchar, CWString)
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
>>> import Streamly.Internal.FileSystem.Path (ignoreTrailingSeparators, allowRelativeEquality, ignoreCase)

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

-- XXX Windows is supported only on little endian machines so generally we do
-- not need covnersion from LE to BE format unless we want to manipulate
-- windows paths on big-endian machines.

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

-- NOTE: See detailed notes on normalization in the normalization section.

-- | If the path is @//@ the result is @/@. If it is @a//@ then the result is
-- @a@. On Windows "c:" and "c:/" are different paths, therefore, we do not
-- drop the trailing separator from "c:/" or for that matter a separator
-- preceded by a ':'.
--
-- Can't use any arbitrary predicate "p", the logic in this depends on assuming
-- that it is a path separator.
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
        then
            -- Even though "//" is not allowed as a valid path.
            -- We still handle that case in this low level function.
            if os == Windows
                && n >= 2
                && Array.unsafeGetIndex 0 arr == Array.unsafeGetIndex 1 arr
            then fst $ Array.unsafeBreakAt 2 arr -- make it "//" share name
            else fst $ Array.unsafeBreakAt 1 arr
        -- "c:////" - keep one "/" after colon in ".*:///" otherwise it will
        -- change the meaning. "c:/" may also appear, in the middle e.g.
        -- in UNC paths.
        else if (os == Windows)
                && (Array.unsafeGetIndex (len - n - 1) arr == charToWord ':')
        then fst $ Array.unsafeBreakAt (len - n + 1) arr
        else arr1

-- XXX we cannot compact "//" to "/" on windows
{-# INLINE collapseTrailingBy #-}
collapseTrailingBy :: Unbox a => (a -> Bool) -> Array a -> Array a
collapseTrailingBy p arr =
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
{-# INLINE usePrimarySeparator #-}
usePrimarySeparator :: (Integral a, Unbox a) => Array a -> Array a
usePrimarySeparator a =
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
-- Relative or Absolute Paths
------------------------------------------------------------------------------
--
-- Relative (no external state except cwd)
-- RelFree     -- x, ./x  -- both curdir and curdrive are unspecified
--
-- AnchPath -- partially relative, dir or drive are specified (Windows only)
-- Anchored paths can be classified into two categories:
-- AnchorDrv, AnchorDir?
-- RelCurDirOnly    -- C:x  -- drive specified, path relative to current dir on that drive
-- RelCurDriveOnly  -- \x   -- absolute path on current drive (root-relative)
--
-- AbsPath -- fully anchored, both drive and dir are specified
-- AbsDrive    -- C:\x
-- AbsUNC      -- \\server\share\x
-- AbsDevice   -- \\?\..., \Device\...
--
-- On Posix only these categories exist:
-- RelPath : RelFree
-- AbsPath : AbsDrive ("/x")
-- AnchPath : None
--
-- data PathType
--   = AbsPath
--   | RelPath
--   | AnchPath
--
-- When appending, do not insert a separator after a bare drive (C:). For all
-- practical purposes a bare "C:" can be treated as "C:." and then we do not
-- need this special treatement wrt separators.
--
--    C: </> x -> C:x
--
--------------------------------------------------------------------------------
-- PATH NAVIGATION SEMANTICS (follow)
--------------------------------------------------------------------------------
--
-- "follow" navigates first path followed by the second. In other words,
-- "follow p1 p2" interprets p2 in the context of p1.
--
-- Operationally:
--   cd (follow p1 p2)  ==  cd p1; cd p2
--
-- That is, p2 is resolved relative to the location denoted by p1.
-- The two paths denote a sequence of resolution operations, we resolve p1 and
-- then we resolve p2 with respect to p1.
--
-- Note that this operation is total and never results in an error.
--
-- Rules:
--
-- 1. If p2 is Relative:
--
--    Absolute </> Relative -> Absolute
--    Relative </> Relative -> Relative
--    Anchored </> Relative -> Anchored
--
--    (p2 is appended to p1)
--
-- 2. If p2 is Absolute:
--
--    Any </> Absolute -> Absolute (p2 wins)
--
-- 3. If p2 is RelCurDirOnly (C:y), if the drive is the same then combine
-- otherwise take the second path. If the drive is not specified then it is
-- considered to be different.
--
--    C:   </> C:y -> C:y    -- C: equiv C:.
--    C:x  </> C:y -> C:x/y
--    C:/x </> C:y -> C:/x/y
--    D:x  </> C:y -> C:y
--
--    /x    </> C:y -> C:y
--    x     </> C:y -> C:y
--
--    The "cd" semantics can be incorrect for the last two if we assume the
--    drive of the first path to be same as the second.
--
-- 4. If p2 is RelCurDriveOnly (\y), discard LHS, if LHS has drive keep the drive:
--
--    C:    </> \y -> C:\y    -- C: equiv C:.
--    C:/   </> \y -> C:\y
--    C:/x  </> \y -> C:\y
--    C:x   </> \y -> C:\y
--    \x    </> \y -> \y
--    x     </> \y -> \y
--
--    For the first 3 cases above, UNC behaves the same as a drive root:
--
--    \\server\share\x </> \y -> \\server\share\y
--
-- These are based on how python 'ntpath' module behaves.
--
--------------------------------------------------------------------------------
-- PATH CONSTRUCTION SEMANTICS (append)
--------------------------------------------------------------------------------
--
-- append constructs paths structurally. The second argument must be such that
-- it can be interpreted relative to the first. While "follow" is total,
-- "append" is partial and can result in runtime errors.
--
-- append p r extends p with the segments of r.
--
-- Rules:
--
-- 1. Always valid if r is relative:
--
--    appendAbs :: AbsPath -> RelPath -> AbsPath
--    appendRel :: RelPath -> RelPath -> RelPath
--    appendAnch :: AnchPath -> RelPath -> AnchPath
--
-- 2. Never valid if r is AbsPath:
--
--    /   </> /x      -> error  -- can be allowed, but no exception
--    p   </> AbsPath -> error
--
-- 3. Identity:
--
--    "." is the empty relative path, it is identity of composition:
--
--    appendAbs p "." == p
--    appendRel p "." == p
--    appendRel "." p == p
--    appendAnch p "." == p
--
-- 4. Associativity (via RelPath):
--
--    append (append p a) b == append p (a <> b)
--
-- Notes:
--
-- - "." is not an anchor; it is the identity element of relative paths.
-- - On Windows AnchoredPath can only start with "\" or "C:", it cannot start
-- with "C:\" as that would make it an AbsPath.
--
--------------------------------------------------------------------------------
-- Handling Anchored Paths
--------------------------------------------------------------------------------
--
-- If second path is Anchored, and has the same Anchor as the first path, then
-- strip the Anchor into a Maybe Drive and a Relative or / Anchored path and
-- then apply the same rules as above considering the / Anchored path as
-- absolute.
--
-- 1. If p2 is RelCurDirOnly (C:y) (Anchored), if both the paths have drive and
-- it is the same then combine otherwise it is runtime error.
--
--    C:\x </> C:y -> C:\x\y
--    C:   </> C:y -> C:y    -- C: equiv C:.
--    C:x  </> C:y -> C:x\y
--
--    D:x  </> C:y -> error
--    \x   </> C:y -> error
--    x    </> C:y -> error
--
-- 2. If p2 is RelCurDriveOnly (\y) (Anchored). p2 is absolute within the
-- drive, therefore, similar to the absolute path rules, not allowed.
--
--    C:\   </> \y -> error    -- can be allowed, but no exceptions
--    C:\x  </> \y -> error
--    C:    </> \y -> error    -- C: is equiv C:. which is a relative path
--    C:x   </> \y -> error
--    \x    </> \y -> error
--    x     </> \y -> error
--
--    For the first 3 cases above, UNC behaves the same as a drive root:
--
--------------------------------------------------------------------------------
-- Typed paths
--------------------------------------------------------------------------------
--
-- Types:
--
-- appendAbs :: AbsPath -> RelPath -> AbsPath
-- appendRel :: RelPath -> RelPath -> RelPath
--
-- They can be combined into a single operation using an IsPath typeclass.
--
-- Windows specific:
--
-- appendAnch :: AnchPath -> RelPath -> AnchPath
--
-- To append Anchored paths remove the anchor first:
--
-- splitAnchor :: AnchPath -> (Maybe Drive, p)
--
-- where p is either RelPath or AnchPath (e.g. /x) type.
--
--  combineAnch :: AnchPath -> AnchPath -> Maybe AnchPath
--      splitAnchor ->
--          if both have a common drive
--          then
--              if second path splits to (_, RelPath)
--              then Just
--              else Nothing
--          else Nothing
--
--------------------------------------------------------------------------------
-- SUMMARY
--------------------------------------------------------------------------------
--
-- follow = resolution (contextual, may override)
-- append = construction (structural, no override)
--
-- follow models filesystem navigation semantics
-- append models path construction semantics

-- | A path relative to cur dir i.e. either equal to @.@ or starts with @./@.
-- It has a leading dot component.
isRelativeCurDir :: (Unbox a, Integral a) => OS -> Array a -> Bool
isRelativeCurDir os a
    | len == 0 = False -- empty path should not occur
    | wordToChar (Array.unsafeGetIndex 0 a) /= '.' = False
    | len < 2 = True
    | otherwise = isSeparatorWord os (Array.unsafeGetIndex 1 a)

    where

    len = Array.length a

-- | A path starting with a separator but not starting with EXACTLY two
-- separators. Such a path is relative to current drive root. Note that
-- "\\/share/x" is treated as "C:/share/x" because UNC or share name starts
-- with EXACTLY two separators, more than two separators are just a sequence of
-- separators which can be collpased into a single separator.
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

-- | Has a leading drive letter not followed by a separator.
-- @C:@ or @C:a...@.
isRelativeWithDrive :: (Unbox a, Integral a) => Array a -> Bool
isRelativeWithDrive a =
    hasDrive a
        && (  Array.length a < 3
           || not (isSeparator Windows (unsafeIndexChar 2 a))
           )

-- | A path that either:
-- * starts with a dot component (".", "./x")
-- * has leading drive not followed by separator ("C:", "C:x")
-- * has a leading separator but not exactly two leading separators (/x)
hasRelativeRoot :: (Unbox a, Integral a) => OS -> Array a -> Bool
hasRelativeRoot Posix a = isRelativeCurDir Posix a
hasRelativeRoot Windows a =
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

-- | Note that on Windows a non-UNC path starting with a separator is relative
-- to current drive while on Posix this is an absolute path as there is only
-- one drive.
isAbsolute :: (Unbox a, Integral a) => OS -> Array a -> Bool
isAbsolute Posix arr =
    hasLeadingSeparator Posix arr
isAbsolute Windows arr =
    isAbsoluteWithDrive arr || isAbsoluteUNC arr

------------------------------------------------------------------------------
-- Location or Segment
------------------------------------------------------------------------------

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

-- NOTE: See notes in the normalization section about the POSIX "//" being
-- special but we do not treat it in a special manner.

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
-- >>> splitPosix = toListPosix . Common.unsafeSplitLeadingSep Common.Posix . packPosix
--
-- >>> toListWin (a,b) = (unpackWindows a, unpackWindows b)
-- >>> splitWin = toListWin . Common.unsafeSplitLeadingSep Common.Windows . packWindows
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
unsafeSplitLeadingSep :: (Unbox a, Integral a) =>
    OS -> Array a -> (Array a, Array a)
-- Note on Windows we should be here only when the path starts with exactly one
-- separator, otherwise it would be UNC path. But on posix multiple separators
-- are valid.
unsafeSplitLeadingSep os = unsafeSplitPrefix os 1

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
        = unsafeSplitLeadingSep Posix arr
    | otherwise = (Array.empty, arr)
splitRoot Windows arr
    | isRelativeCurDriveRoot arr || isRelativeCurDir Windows arr
        = unsafeSplitLeadingSep Windows arr
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

-- | Split a path on separator chars and collapse contiguous separators and
-- remove /./ components.
--
-- Note do not pass paths with drives that may have multiple separators it will
-- blindly collapse those changing the meaning.
{-# INLINE splitPathBodyNormalized #-}
splitPathBodyNormalized
    :: (Unbox a, Integral a, Monad m)
    => Bool
    -> OS
    -> Array a
    -> Stream m (Array a)
splitPathBodyNormalized withSep os arr =
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

-- | Split a path into its components.
--
-- Usage:
-- @
-- splitPathUsing withSep ignoreLeading os arr
-- @
--
-- if withSep == True then keep the trailing separators.
--
-- if ignoreLeading == True we drop all leading separators and relative paths.
-- Example behaviour (psuedo-code):
-- @
-- > f = splitPathUsing (withSep = False) (ignoreLeading = True)
-- > f "./a/b/c" == ["a","b","c"]
-- > f "./a/./b/c" == ["a","b","c"]
-- > f "/a/./b/c" == ["a","b","c"]
-- > f "/./a/./b/c" == ["a","b","c"]
-- > f "././a/./b/c" == ["a","b","c"]
-- > f "a/./b/c" == ["a","b","c"]
-- @
--
-- We can safely set @ignoreLeading = True@ if we splitRoot prior and only pass
-- the stem of the path to this function.
{-# INLINE splitPathUsing #-}
splitPathUsing
    :: (Unbox a, Integral a, Monad m)
    => Bool
    -> Bool
    -> OS
    -> Array a
    -> Stream m (Array a)
splitPathUsing withSep ignoreLeading os arr =
    let stream = splitPathBodyNormalized withSep os body
    in if ignoreLeading || Array.null root
       then stream
       else Stream.cons root1 stream

    where

    -- We should not filter out a leading '.' on Posix or Windows.
    -- We should not filter out a '.' in the middle of a UNC root on windows.
    -- Therefore, we split the root and treat it in a special way.
    (root, body) = splitRoot os arr
    root1 =
        if withSep
        then collapseTrailingBy (isSeparator os . wordToChar) root
        else dropTrailingSeparators os root

{-# INLINE splitPath_ #-}
splitPath_
    :: (Unbox a, Integral a, Monad m)
    => OS -> Array a -> Stream m (Array a)
splitPath_ = splitPathUsing False False

{-# INLINE splitPath #-}
splitPath
    :: (Unbox a, Integral a, Monad m)
    => OS -> Array a -> Stream m (Array a)
splitPath = splitPathUsing True False

-- | Split the first non-empty path component.
--
-- /Unimplemented/
{-# INLINE splitHead #-}
splitHead :: -- (Unbox a, Integral a) =>
    OS -> Array a -> (Array a, Maybe (Array a))
splitHead _os _arr = undefined

-- | Split the last non-empty path component.
--
-- /Unimplemented/
{-# INLINE splitTail #-}
splitTail :: -- (Unbox a, Integral a) =>
    OS -> Array a -> (Maybe (Array a), Array a)
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
splitFile :: (Unbox a, Integral a) =>
    OS -> Array a -> Maybe (Maybe (Array a), Array a)
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
            then Just (Nothing, arr)
            else Just (Just $ Array.unsafeSliceOffLen 0 baseLen base, file) -- "/"
        else Nothing

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
    a -> OS -> Array a -> Maybe (Array a, Array a)
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
        then Just res
        else Nothing

{-# INLINE splitExtension #-}
splitExtension :: (Unbox a, Integral a) => OS -> Array a -> Maybe (Array a, Array a)
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
        . splitPathBodyNormalized False Windows

    invalidRootComponent =
        List.any (`List.elem` isInvalidPathComponent) (components root)
    invalidComponent =
        List.any (`List.elem` isInvalidPathComponent) (components stem)

-- | A valid root, share root or a valid path.
{-# INLINE validatePath #-}
validatePath :: (MonadThrow m, Integral a, Unbox a) => OS -> Array a -> m ()
validatePath = validatePathWith True

{-# INLINE validatePath' #-}
validatePath' :: (MonadThrow m, Integral a, Unbox a) => OS -> Array a -> m ()
validatePath' = validatePathWith False

{-# INLINE unsafeFromArray #-}
unsafeFromArray :: Array a -> Array a
unsafeFromArray = id

{-# INLINE fromArray #-}
fromArray :: forall m a. (MonadThrow m, Unbox a, Integral a) =>
    OS -> Array a -> m (Array a)
fromArray os arr = validatePath os arr >> pure arr
{-
    let arr1 = Array.unsafeCast arr :: Array a
     in validatePath os arr1 >> pure arr1
fromArray Windows arr =
    case Array.cast arr of
        Nothing ->
            throwM
                $ InvalidPath
                $ "Encoded path length " ++ show (Array.byteLength arr)
                    ++ " is not a multiple of 16-bit."
        Just x -> validatePath Windows x >> pure x
-}

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
     in fromArray os (Array.unsafeCast arr)

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

-- XXX Scan this entire module for pushing array operations to MutArray or
-- MutByteArray modules.

{-# INLINE appendCStringWith #-}
appendCStringWith :: (Unbox a, Integral a) =>
       (Int -> IO (MutArray a))
    -> (Addr# -> IO CSize)
    -> OS
    -> Array a
    -> Ptr a
    -> IO (Array a)
appendCStringWith create strlen os origArr origStr@(Ptr strAddr#) = do
    let countArr = Array.length origArr
    countStr <- fmap fromIntegral $ strlen strAddr#
    assertM(countArr /= 0 && countStr /= 0)
    let count = countArr + 1 + countStr
    arr <- create count
    arr1 <- MutArray.unsafeSplice arr (Array.unsafeThaw origArr)
    arr2 <- MutArray.unsafeSnoc arr1 (charToWord (primarySeparator os))
    arr3 <- MutArray.unsafeAppendPtrN arr2 origStr countStr
    return (Array.unsafeFreeze arr3)

-- See also cstringLength# in GHC.CString in ghc-prim
foreign import ccall unsafe "string.h strlen" c_strlen_pinned
    :: Addr# -> IO CSize

{-# INLINE appendCString #-}
appendCString :: OS -> Array Word8 -> CString -> IO (Array Word8)
appendCString os arr cstr =
    appendCStringWith MutArray.emptyOf c_strlen_pinned os arr (castPtr cstr)

{-# INLINE appendCString' #-}
appendCString' :: OS -> Array Word8 -> CString -> IO (Array Word8)
appendCString' os arr cstr =
    appendCStringWith MutArray.emptyOf' c_strlen_pinned os arr (castPtr cstr)

foreign import ccall unsafe "wchar.h wcslen" c_wcslen_pinned
    :: Addr# -> IO CSize

-- | NOTE: CWchar is 16-bit wide on Windows and 32-bit wide on Posix. wcslen is
-- available on both Posix and Windows and counts accordingly in units of
-- 2-bytes or 4-bytes.
{-# INLINE appendCWString #-}
appendCWString :: OS -> Array CWchar -> CWString -> IO (Array CWchar)
appendCWString = appendCStringWith MutArray.emptyOf c_wcslen_pinned

{-# INLINE appendCWString' #-}
appendCWString' :: OS -> Array CWchar -> CWString -> IO (Array CWchar)
appendCWString' = appendCStringWith MutArray.emptyOf' c_wcslen_pinned

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

-- XXX instead of Stream Identity we can use a list instead.

-- | Join paths by path separator. Does not check if the paths being appended
-- are rooted or path segments. Note that splitting and joining may not give
-- exactly the original path but an equivalent, normalized path.
--
-- Truly unsafe, pitfalls:
--  [x, /y]  => x//y -- normally not be allowed under append semantics
--  [x, C:y]  => x/C:y -- normally not allowed under append semantics
--  [C:x, C:y]  => C:x/C:y -- normally not allowed under append semantics
{-# INLINE unsafeJoinPaths #-}
unsafeJoinPaths
    :: (Unbox a, Integral a)
    => OS -> Stream Identity (Array a) -> Array a
unsafeJoinPaths os =
    -- XXX This can be implemented more efficiently using an Array intersperse
    -- operation. Which can be implemented by directly copying arrays rather
    -- than converting them to stream first. Also fromStreamN would be more
    -- efficient if we have to use streams.
    -- XXX We can remove leading and trailing separators first, if any, except
    -- the leading separator from the first path. But it is not necessary.
    -- Instead we can avoid adding a separator if it is already present.
    Array.fromPureStream . Array.concatSepBy (charToWord $ primarySeparator os)

-- | Join an already-normalised root with an already-normalised body. Either
-- side may be empty; an empty piece is dropped instead of triggering the
-- separator-injection logic in 'append'.
{-# INLINE joinRootBody #-}
joinRootBody :: (Unbox a, Integral a) => OS -> Array a -> Array a -> Array a
joinRootBody os root body
    | Array.null body = root
    | Array.null root = body
    | otherwise = doAppend os root body

------------------------------------------------------------------------------
-- Normalization and comparison of paths
------------------------------------------------------------------------------

-- Windows literal paths
-- ---------------------
--
-- Windows "Literal" Paths (\\?\): When you prefix a path with \\?\, you are
-- telling the Windows APIs to turn off all "normalization".
--
-- Object Manager Paths: On Windows, paths like \??\C:\ or
-- \Device\HarddiskVolume1\ have very specific rules about separators.
--
-- We should splitPathRaw instead of splitPath on such paths to be able to
-- reconstruct the path back if needed.
--
-- POSIX //
-- --------
--
-- On POSIX a path starting with exactly two slashes ("//x") is
-- implementation-defined.
--
-- See https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html
--
-- If a pathname begins with two successive <slash> characters, the first
-- component following the leading <slash> characters may be interpreted in an
-- implementation-defined manner, although more than two leading <slash>
-- characters shall be treated as a single <slash> character.
--
-- This is rarely or historically used on Posix but may be of importance in
-- portable cygwin style paths where a UNC path \\server\share\file gets
-- converted to Posix style //server/share/file .
--
-- If we want this behavior on Posix we can treat the path as a Windows path
-- and use Windows path operations on it.

------------------------------------------------------------------------------
-- Building blocks
------------------------------------------------------------------------------

-- NOTE: splitPath already cleans up redundant separators and dot components,
-- so we just need to split and join the path components. The functions below
-- are for cases where we want to apply only one such normalization in
-- isolation.

-- | Collapse consecutive path separators into a single separator.
--
-- Keeps only the first separator from a run of consecutive separators, does
-- not change the separator type on Windows. Does not remove trailing
-- separators.
--
-- Note that this literally collapses all consecutive separators without any
-- special treatment for the root, e.g. the leading @\\\\@ in a UNC path on
-- Windows will also get collapsed into a single @\\@.
collapseSeparators :: (Unbox a, Integral a) => OS -> Array a -> Array a
collapseSeparators os arr =
    -- XXX We could check for redundant separators first and return the input
    -- unchanged when nothing changes. Likely worth it for the common case.
    Array.fromPureStream
        $ Stream.uniqBy eq
        $ Array.read arr

    where

    eq prev curr = isSeparatorWord os prev && isSeparatorWord os curr

-- | Remove @.@ (current directory) segments from a path.
--
-- Note that this literally removes any @\/.\/@ components without any special
-- treatment for the root; if for some reason such a component appears in the
-- root portion it will also be stripped. Normally @.@ components do not
-- appear in the root portion.
--
-- /Unimplemented/
dropDotSegments :: Array a -> Array a
-- XXX There is no need to decode the path; we only need to find ".", which is
-- ASCII and so can be matched by comparing the binary word directly. Use a
-- variant of splitPath that preserves separators ("splitPathRaw") so that we
-- do not touch the separators here. Return the original array unchanged when
-- there is nothing to drop.
dropDotSegments = undefined

-- | Collapse @..@ segments lexically.
--
-- A @..@ following a path segment cancels that segment, e.g. @a\/b\/..\/c@
-- becomes @a\/c@. This removes parent directory references without
-- resolving symlinks, so it is /unsafe/ in the presence of symbolic links.
-- It is useful to remove @..@ segments without performing IO, on
-- non-existent paths or paths known to not contain symlinks.
--
-- For an absolute path, leading @..@ segments are dropped because @\/..@ is
-- equivalent to @\/@. For a relative path, leading @..@ segments are kept
-- because there is no way to go above the starting point lexically.
--
-- Note: as a side effect of splitting and re-joining, redundant separators
-- and @.@ segments in the body of the path are also removed, and separators
-- are normalised to the primary separator on Windows.
collapseDotDots :: (Unbox a, Integral a) => OS -> Array a -> Array a
collapseDotDots os p =
    let (root, body) = splitRoot os p
        -- A root "blocks" leading ".." segments when there is no way to go
        -- above it lexically: any leading-separator root (e.g. "/", "\\",
        -- "\\\\server\\share\\") and an absolute drive root on Windows
        -- ("C:\\..."). A non-rooted path or a drive-only root like "C:" or
        -- "./" can still be prefixed by "..".
        rootBlocksDotDot =
            hasLeadingSeparator os root
                || (os == Windows && isAbsoluteWithDrive root)
        comps =
              runIdentity
            $ Stream.toList
            $ splitPathBodyNormalized False os body
        -- Stack stores components in reverse order (newest at the head).
        step stack comp
            | isDotDotComp comp =
                case stack of
                    -- Nothing to cancel: drop ".." if the root blocks it,
                    -- otherwise keep it as a leading "..".
                    [] -> [comp | not rootBlocksDotDot]
                    -- Cannot cancel a previous "..".
                    top : rest
                        | isDotDotComp top -> comp : stack
                        | otherwise -> rest
            | otherwise = comp : stack
        processed = List.reverse (List.foldl' step [] comps)
        body' = unsafeJoinPaths os (Stream.fromList processed)
     in if Array.null root && Array.null body' && not (Array.null body)
        -- All segments of a non-rooted path cancelled out; this collapses
        -- to "." (current dir) rather than the empty path.
        then Array.fromList [ charToWord '.' ]
        else joinRootBody os root body'

    where

    isDotDotComp a =
        Array.length a == 2
            && Array.unsafeGetIndex 0 a == charToWord '.'
            && Array.unsafeGetIndex 1 a == charToWord '.'

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

------------------------------------------------------------------------------
-- Equality
------------------------------------------------------------------------------

-- NOTE: These functions mirror the corresponding normalization functions
-- below, when making changes to one the other should be kept in sync.

eqPathBytes :: Array a -> Array a -> Bool
eqPathBytes = Array.byteEq

-- On posix, macOs can have case insensitive comparison. On Windows also
-- case sensitive behavior may depend on the file system being used.

-- | Options for path comparison operation. By default path comparison uses a
-- strict criteria for equality. The following options are provided to
-- control the strictness.
--
-- The default configuration is as follows:
--
-- >>> :{
-- defaultMod = ignoreTrailingSeparators False
--            . ignoreCase False
--            . allowRelativeEquality False
-- :}
--
data EqCfg =
    EqCfg
    { _ignoreTrailingSeparators :: Bool -- ^ Allows "x\/" == "x"
    , _ignoreCase :: Bool               -- ^ Allows "x" == \"X\"
    -- XXX _compareRelative, default True
    , _allowRelativeEquality :: Bool
    -- ^ A leading dot is ignored, thus ".\/x" == ".\/x" and ".\/x" == "x".
    -- On Windows allows "\/x" == \/x" and "C:x == C:x"

    -- , collapseDotDotSegments -- "x\/..\/y" == "y"
    -- , collapseSeparators -- "x\/\/y" \/= "x\/y"
    -- , dropDotSegments -- "x\/.\/" \/= "x"
    -- , strictPosix -- //home /= /home
    }

-- PlainRoot is Absolute on Posix and relative to a drive on Windows.
data PlainRoot =
      PlainRootAbs -- The "/" in a path starting with "/" but not "//"
    | PlainRootRel -- The "." or "" in a path not starting with / or drive in windows
        deriving Eq

data WindowsRoot =
      WindowsPlainRoot -- /x or ./x
    | WindowsDriveRoot -- C:... or \\...
    deriving Eq

-- | Here we must pass a path i.e. either a drive root or a UNC path, it must
-- not be a plain root. If not then this function will not work correctly e.g.
-- it might change \/ to // making the path a share name from a normal path.
eqWindowsRootWithDrive :: (Unbox a, Integral a) =>
    Bool -> Array a -> Array a -> Bool
eqWindowsRootWithDrive ignCase a b =
     -- XXX we should not normalize Windows literal paths in any case
     if ignCase
     then
        let f = normalizeCaseAndSeparators . Array.unsafeCast
            -- XXX We probably do not want to translate UnC etc. to UNC.
            -- Such a path should either be rejected in splitRoot or we
            -- should not translate that here.
            -- XXX if so write test cases for that.
         in runIdentity $ Stream.eqBy (==) (f a) (f b)
     else
        let f = fmap toDefaultSeparator . Array.read
            -- XXX should we ignore case for drives anyway? irrespective of the
            -- remaining path. Are there case sensitive filesystems on windows?
            -- Are drives ever case sensitive?
            -- XXX if so write test cases for that.
         in runIdentity $ Stream.eqBy (==) (f a) (f b)

-- | We should call this only when the roots are either both absolute or both
-- null otherwise it may not function correctly.
{-# INLINE eqAbsOrNullRoots #-}
eqAbsOrNullRoots :: (Unbox a, Integral a) =>
    Bool -> OS -> Array a -> Array a -> Bool
eqAbsOrNullRoots _ Posix a b =
    -- a can be "/" and b can be "//"
    Array.null a == Array.null b
eqAbsOrNullRoots ignCase Windows a b = eqWindowsRootWithDrive ignCase a b

-- | Can only be either "", '.', './' or '/' (or Windows separators)
getPlainRootType :: (Unbox a, Integral a) => Array a -> PlainRoot
getPlainRootType arr =
    if Array.null arr || unsafeIndexChar 0 arr == '.'
    then PlainRootRel
    else PlainRootAbs

-- | Compare Posix or Windows roots without a drive or share name.
-- i.e. roots starting with "/" or "" or "."
{-# INLINE eqPlainRootLax #-}
eqPlainRootLax :: (Unbox a, Integral a) => Array a -> Array a -> Bool
eqPlainRootLax a b = getPlainRootType a == getPlainRootType b

getWindowsRootType :: (Unbox a, Integral a) => Array a -> WindowsRoot
getWindowsRootType arr =
    if isAbsoluteUNC arr || hasDrive arr
    then WindowsDriveRoot
    else WindowsPlainRoot

eqWindowsRootLax :: (Unbox a, Integral a) => Bool -> Array a -> Array a -> Bool
eqWindowsRootLax ignCase a b =
    let aType = getWindowsRootType a
        bType = getWindowsRootType b
     in aType == bType
        && (
            (aType == WindowsPlainRoot && eqPlainRootLax a b)
            || eqWindowsRootWithDrive ignCase a b
           )

{-# INLINABLE eqRootLax #-}
eqRootLax :: (Unbox a, Integral a) => Bool -> OS -> Array a -> Array a -> Bool
eqRootLax _ Posix a b = eqPlainRootLax a b
eqRootLax ignCase Windows a b = eqWindowsRootLax ignCase a b

eqRootStrict :: (Unbox a, Integral a) => Bool -> OS -> Array a -> Array a -> Bool
eqRootStrict ignCase os rootA rootB =
   (not (hasRelativeRoot os rootA) && not (hasRelativeRoot os rootB))
        && eqAbsOrNullRoots ignCase os rootA rootB

{-# INLINE eqComponentsWith #-}
eqComponentsWith :: (Unbox a, Integral a) =>
       EqCfg
    -> (Stream Identity a -> Stream Identity Char)
    -> OS
    -> Array a
    -> Array a
    -> Bool
eqComponentsWith EqCfg{..} decoder os a b =
    if _ignoreCase
    then
        let streamEq x y = runIdentity $ Stream.eqBy (==) x y
            toComponents = fmap (normalizeCaseWith decoder) . splitter os
        -- XXX check perf/fusion
         in runIdentity
                $ Stream.eqBy streamEq (toComponents a) (toComponents b)
    else
        runIdentity
            $ Stream.eqBy
                Array.byteEq (splitter os a) (splitter os b)
    where

    splitter = splitPathUsing False _allowRelativeEquality

-- XXX can we do something like SpecConstr for such functions e.g. without
-- inlining the function we can use two copies one for _allowRelativeEquality
-- True and other for False and so on for other values of PathEq.

{-# INLINE eqPath #-}
eqPath :: (Unbox a, Integral a) =>
    (Stream Identity a -> Stream Identity Char)
    -> OS -> EqCfg -> Array a -> Array a -> Bool
eqPath decoder os eqCfg@(EqCfg{..}) a b =
    let (rootA, stemA) = splitRoot os a
        (rootB, stemB) = splitRoot os b

        eqRelative =
               if _allowRelativeEquality
               then eqRootLax _ignoreCase os rootA rootB
               else eqRootStrict _ignoreCase os rootA rootB

        -- XXX If one ends in a "." and the other ends in ./ (and same for
        -- ending with ".." and "../") then they can be equal. We can append a
        -- slash in these two cases before comparing.
        eqTrailingSep =
            _ignoreTrailingSeparators
                || hasTrailingSeparator os a == hasTrailingSeparator os b

     in
           eqRelative
        && eqTrailingSep
        && eqComponentsWith eqCfg decoder os stemA stemB

------------------------------------------------------------------------------
-- Normalization
------------------------------------------------------------------------------

-- NOTE: These functions mirror the corresponding equality functions above,
-- when making changes to one the other should be kept in sync.

-- | Here we must pass a path i.e. either a drive root or a UNC path, it must
-- not be a plain root. If not then this function will not work correctly e.g.
-- it might change \/ to // making the path a share name from a normal path.
normaliseWindowsDriveRoot :: (Unbox a, Integral a) =>
    Bool -> Array a -> Array a
normaliseWindowsDriveRoot ignCase a =
     -- XXX we should not normalize Windows literal paths in any case
    let stream =
            if ignCase
            -- XXX We probably do not want to translate UnC etc. to UNC.
            -- Such a path should either be rejected in splitRoot or we
            -- should not translate that here.
            -- XXX if so write test cases for that.
            then fmap charToWord
                    $ normalizeCaseAndSeparators
                    $ Array.unsafeCast a
            else fmap toDefaultSeparator $ Array.read a
     in Array.fromPureStream stream

-- We have already deduplicated the separators in splitRoot
{-# INLINE normalisePlainRoot #-}
normalisePlainRoot :: (Unbox a, Integral a) => OS -> Array a -> Array a
normalisePlainRoot os a =
    case getPlainRootType a of
        PlainRootRel -> Array.empty
        PlainRootAbs -> Array.fromList [ charToWord (primarySeparator os) ]

{-# INLINABLE normaliseRoot #-}
normaliseRoot :: (Unbox a, Integral a) => Bool -> OS -> Array a -> Array a
normaliseRoot _ Posix a = normalisePlainRoot Posix a
normaliseRoot ignCase Windows a =
    case getWindowsRootType a of
        WindowsPlainRoot -> normalisePlainRoot Windows a
        WindowsDriveRoot -> normaliseWindowsDriveRoot ignCase a

{-# INLINE normaliseComponents #-}
normaliseComponents :: (Unbox a, Integral a) =>
       EqCfg
    -> (Stream Identity a -> Stream Identity Char)
    -> OS
    -> Array a
    -> Array a
normaliseComponents EqCfg{..} decoder os a =
    let normalizeCase =
              Array.fromPureStream
            . fmap charToWord
            . normalizeCaseWith decoder
        f = if _ignoreCase then fmap normalizeCase else id
     in unsafeJoinPaths os $ f $ splitter os a

    where

    -- Note: when 'a' is the body extracted via 'splitRoot', the
    -- 'ignoreLeading' flag is irrelevant - the body has no root to drop.
    -- We have to use withSep = False here because if we keep the separators,
    -- the separators will have to be normalized to primary separator.
    splitter = splitPathUsing False _allowRelativeEquality

{-# INLINE appendTrailingSep #-}
appendTrailingSep :: (Unbox a, Integral a) => OS -> Array a -> Array a
appendTrailingSep os arr =
    Array.fromPureStream
        $ Stream.append
            (Array.read arr)
            (Stream.fromPure (charToWord (primarySeparator os)))

-- | Convert the path to an equivalent but standard format for reliable
-- comparison.
--
-- This collapses redundant separators and removes @.@ components, normalises
-- the root (including separator style on Windows) and optionally folds case
-- per the 'EqCfg' options. It does /not/ collapse @..@ segments; for that
-- use 'collapseDotDots' explicitly, since that operation is unsafe in the
-- presence of symlinks.
--
-- A trailing separator is preserved unless 'EqCfg' has
-- @_ignoreTrailingSeparators@ set, in which case it is dropped.
normalise :: (Unbox a, Integral a) =>
       (Stream Identity a -> Stream Identity Char)
    -> OS -> EqCfg -> Array a -> Array a
normalise decoder os eqCfg@EqCfg{..} p =
    -- NOTE: _allowRelativeEquality impacts comparison but not normalization
    -- of the root.
    let (root, body) = splitRoot os p
        -- XXX We are writing the array multiple times, for root, for body and
        -- then for adding a separator. We can either use a mutarray or stream
        -- all the normalized parts once to create array only once.
        nRoot = normaliseRoot _ignoreCase os root
        nBody = normaliseComponents eqCfg decoder os body
        result = joinRootBody os nRoot nBody
        -- The body's trailing separator is dropped by normaliseComponents;
        -- restore it if it was present in the input and the caller wants it
        -- kept.
        keepTrailingSep =
               not _ignoreTrailingSeparators
            && not (Array.null nBody)
            && hasTrailingSeparator os p
            && not (hasTrailingSeparator os result)
     in if keepTrailingSep
        then appendTrailingSep os result
        else result

------------------------------------------------------------------------------
-- Path prefix
------------------------------------------------------------------------------

-- | Return the longest common, non-empty, prefix of two paths. Path prefix is
-- compared using normalization. Dot dot components are not collapsed because
-- of symlink possiblities.
--
-- The result, if present, is a valid path that is a prefix of both inputs.
-- Returns 'Nothing' if there is no common prefix or if the common prefix is empty.
takeCommonPrefix :: (Unbox a, Integral a) =>
       (Stream Identity a -> Stream Identity Char)
    -> OS -> EqCfg -> Array a -> Array a -> Maybe (Array a)
takeCommonPrefix decoder os EqCfg{..} a b =
    let (rootA, bodyA) = splitRoot os a
        (rootB, bodyB) = splitRoot os b

        rootsMatch =
            if _allowRelativeEquality
            then eqRootLax _ignoreCase os rootA rootB
            else eqRootStrict _ignoreCase os rootA rootB

        commonRoot = normaliseRoot _ignoreCase os rootA

        compEq x y =
            if _ignoreCase
            then runIdentity $ Stream.eqBy (==)
                    (normalizeCaseWith decoder x)
                    (normalizeCaseWith decoder y)
            else Array.byteEq x y

        commonBody =
            unsafeJoinPaths os
                $ Stream.takeCommonPrefixBy compEq
                    (splitPathBodyNormalized False os bodyB)
                    (splitPathBodyNormalized False os bodyA)

        result = joinRootBody os commonRoot commonBody

    in if not rootsMatch || Array.null result
       then Nothing
       else Just result

-- | @stripPrefix decoder os cfg prefix path@
-- Strip a non-empty prefix from a path.
--
-- If the first argument is a prefix of the second, returns the remainder
-- after the path segment boundary. Returns 'Nothing' if the prefix is not
-- present or does not align on a path segment boundary.
--
-- prefix is compared with the path using the supplied normalization config. If
-- a match is found it removed from the supplied path. If not then Nothing is
-- returned.
stripPrefix :: (Unbox a, Integral a) =>
       (Stream Identity a -> Stream Identity Char)
    -> OS -> EqCfg -> Array a -> Array a -> Maybe (Array a)
stripPrefix decoder os EqCfg{..} prefix path =
    let (rootPre, bodyPre)   = splitRoot os prefix
        (rootPath, bodyPath) = splitRoot os path

        rootsMatch =
            if _allowRelativeEquality
            then eqRootLax _ignoreCase os rootPre rootPath
            else eqRootStrict _ignoreCase os rootPre rootPath

        compEq x y =
            if _ignoreCase
            then runIdentity $ Stream.eqBy (==)
                    (normalizeCaseWith decoder x)
                    (normalizeCaseWith decoder y)
            else Array.byteEq x y

        remainder =
            fmap (unsafeJoinPaths os)
                $ runIdentity
                $ Stream.stripPrefixBy compEq
                    (splitPathBodyNormalized False os bodyPre)
                    (splitPathBodyNormalized False os bodyPath)

    in if not rootsMatch
       then Nothing
       else remainder >>= \arr -> if Array.null arr then Nothing else Just arr
