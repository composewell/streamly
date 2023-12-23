{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.FileSystem.Path
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Well typed and flexible file systems paths, preserving the OS and filesystem
-- encoding.
--
-- You can choose the level of type safety you want. 'Path' is the basic path
-- type which can represent a file, directory, absolute or relative path with
-- no restrictions. Depending on how much type safety you want you can choose
-- appropriate type wrappers to wrap 'Path'. @File Path@ mandates the path to
-- be a file whereas @Abs (File Path)@ mandates it to be an absolute path
-- representing a file.
--
-- You can upgrade or downgrade the safety. Whenever a less restrictive path
-- type is converted to a more restrctive path type the conversion involves
-- checks and it may fail. However, a more restrictive path type can be freely
-- converted to a less restrictive one.
--
-- See the @streamly-filepath@ package for interworking with the 'OsPath' type.
-- The 'Path' type can be  converted to and from 'OsPath' type at zero cost
-- since the underlying representation of both is the same.

-- Conventions: A trailing separator on a path indicates that it is a directory.
-- However, the absence of a trailing separator does not convey any
-- information, it could either be a directory or a file.

-- You may also find the 'str' quasiquoter from "Streamly.Unicode.String" to be
-- useful in creating paths.
--
module Streamly.Internal.FileSystem.Path
    (
    -- * Path Types
      Path
    , File
    , Dir
    , Abs
    , Rel

    -- * Conversions
    , IsPath (..)
    , adaptPath

    -- * Construction
    , fromChunk
    , fromChunkUnsafe
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
    , extendPath
    , extendDir
    )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Data.Word (Word16)
#endif
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Stream (Stream)
import System.IO.Unsafe (unsafePerformIO)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Prelude hiding (abs)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#define WORD_TYPE Word16
#define SEPARATOR 92
#else
#define WORD_TYPE Word8
#define SEPARATOR 47
#endif

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

-- XXX Path must not contain null char on Posix. System calls consider the path
-- as null terminated.
-- XXX Maintain the Array with null termination because Unix system calls
-- require a null terminated string, also they return null terminated strings
-- as paths. Implementation of path append will have to handle the null
-- termination. Or we can choose to always copy the array when using it in
-- system calls.

-- XXX The eq instance needs to make sure that the paths are equivalent. If we
-- normalize the paths we can do a byte comparison. However, on windows paths
-- are case insensitive but the case is preserved, therefore, we cannot
-- normalize and need to do case insensitive comparison.

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | A type representing file system paths for directories or files.
newtype Path = Path (Array WORD_TYPE) -- deriving Eq

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

-- | A member of 'IsPath' knows how to convert to and from the 'Path' type.
class IsPath a where
    -- | Like 'fromPath' but does not check the properties of 'Path'. Provides
    -- performance and simplicity when we know that the properties of the path
    -- are already verified, for example, when we get the path from the file
    -- system or the OS APIs.
    fromPathUnsafe :: Path -> a

    -- | Convert a raw 'Path' to other forms of well-typed paths. It may fail
    -- if the path does not satisfy the properties of the target type.
    --
    -- Path components may have limits.
    -- Total path length may have a limit.
    fromPath :: MonadThrow m => Path -> m a

    -- | Convert a well-typed path to a raw 'Path'. Never fails.
    toPath :: a -> Path

instance IsPath Path where
    fromPathUnsafe = id
    fromPath = pure
    toPath = id

instance IsPath (File Path) where
    fromPathUnsafe p = File p
    fromPath p = pure (File p)
    toPath (File p) = p

instance IsPath (Dir Path) where
    fromPathUnsafe p = Dir p
    fromPath p = pure (Dir p)
    toPath (Dir p) = p

instance IsPath (Abs Path) where
    fromPathUnsafe p = Abs p
    fromPath p = pure (Abs p)
    toPath (Abs p) = p

instance IsPath (Rel Path) where
    fromPathUnsafe p = Rel p
    fromPath p = pure (Rel p)
    toPath (Rel p) = p

instance IsPath (Abs (File Path)) where
    fromPathUnsafe p = Abs (File p)
    fromPath p = pure (Abs (File p))
    toPath (Abs (File p)) = p

instance IsPath (Abs (Dir Path)) where
    fromPathUnsafe p = Abs (Dir p)
    fromPath p = pure (Abs (Dir p))
    toPath (Abs (Dir p)) = p

instance IsPath (Rel (File Path)) where
    fromPathUnsafe p = Rel (File p)
    fromPath p = pure (Rel (File p))
    toPath (Rel (File p)) = p

instance IsPath (Rel (Dir Path)) where
    fromPathUnsafe p = Rel (Dir p)
    fromPath p = pure (Rel (Dir p))
    toPath (Rel (Dir p)) = p

-- XXX Use rewrite rules to eliminate intermediate conversions for better
-- efficiency.

-- | Convert a path type to another path type. This operation may fail with a
-- 'PathException' when converting a less restrictive path type to a more
-- restrictive one.
adaptPath :: (MonadThrow m, IsPath a, IsPath b) => a -> m b
adaptPath p = fromPath $ toPath p

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- A chunk is essentially an untyped Array i.e. Array Word8.  We can either use
-- the term ByteArray for that or just Chunk. The latter is shorter and we have
-- been using it consistently in streamly.

-- | /Unsafe/: On Posix, a path cannot contain null characters. On Windows, the
-- array passed must be a multiple of 2 bytes as the underlying representation
-- uses 'Word16'.
{-# INLINE fromChunkUnsafe #-}
fromChunkUnsafe :: Array Word8 -> Path
fromChunkUnsafe arr = Path (Array.castUnsafe arr)

-- | On Posix it may fail if the byte array contains null characters. On
-- Windows the array passed must be a multiple of 2 bytes as the underlying
-- representation uses 'Word16'.
--
-- Throws 'InvalidPath'.
fromChunk :: MonadThrow m => Array Word8 -> m Path
fromChunk arr =
    case Array.cast arr of
        Nothing ->
            -- XXX Windows only message.
            throwM
                $ InvalidPath
                $ "Encoded path length " ++ show (Array.byteLength arr)
                    ++ " is not a multiple of 16-bit."
        Just x -> pure (Path x)

-- | Convert 'Path' to an array of bytes.
toChunk :: Path -> Array Word8
toChunk (Path arr) = Array.asBytes arr

-- | Encode a Unicode char stream to 'Path' using strict UTF-8 encoding on
-- Posix. On Posix it may fail if the stream contains null characters.
-- TBD: Use UTF16LE on Windows.
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

separator :: WORD_TYPE
separator = SEPARATOR

-- If we append an absolute path it may fail with an error if the 'Path'
-- implementation stores absolute path information (a leading separator char).
-- However, the implementation may choose to store the path as a list of
-- components in which case we cannot distinguish an absolute path from
-- relative.

-- | Like 'extendDir' but for the less restrictive 'Path' type which will always
-- create a syntactically valid 'Path' type but it may not be semantically valid
-- because we may append an absolute path or we may append to a file path.
-- The onus lies on the user to ensure that the first path is not a file and
-- the second path is not absolute.
extendPath :: Path -> Path -> Path
extendPath (Path a) (Path b) =
    let len = Array.byteLength a + 1 + Array.byteLength b
        -- XXX Check the leading separator or drive identifier. However,
        -- checking the drive letter may add an additional overhead (can it be
        -- arbitrarily long?), if it is significant we may want to have a
        -- separate combinePathChecked API for that.
        --
        -- Also, do not add a separator char if the first path has a trailing
        -- separator.
        newArr = unsafePerformIO $ do
            arr <- MutArray.new len
            arr1 <- MutArray.spliceUnsafe arr (Array.unsafeThaw a)
            arr2 <- MutArray.snocUnsafe arr1 separator
            arr3 <- MutArray.spliceUnsafe arr2 (Array.unsafeThaw b)
            return (Array.unsafeFreeze arr3)
        in Path newArr

-- The only safety we need for paths is: (1) The first path can only be a Dir
-- type path, and (2) second path can only be a Rel path.
-- Can this be coerced to create unsafe versions?

-- | Extend a directory path by appending a relative path to it. This is the
-- equivalent to the @</>@ operator from the @filepath@ package.
{-# INLINE extendDir #-}
extendDir :: (IsPath (a (Dir Path)), IsPath b, IsPath (a b)) =>
    (a (Dir Path)) -> Rel b -> a b
extendDir a (Rel b) =
    fromPathUnsafe $ extendPath (toPath a) (toPath b)
