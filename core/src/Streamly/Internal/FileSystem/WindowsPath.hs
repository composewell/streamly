#define IS_WINDOWS
#include "Streamly/Internal/FileSystem/PosixPath.hs"

{- $setup
>>> :m
>>> :set -XQuasiQuotes
>>> import Data.Maybe (fromJust)
>>> import Data.Word (Word16)
>>> import qualified Streamly.Data.Stream as Stream

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.WindowsPath (WindowsPath, path)
>>> import Streamly.Internal.Data.Array (Array)
>>> import qualified Streamly.Internal.Data.Array as Array
>>> import qualified Streamly.Internal.FileSystem.WindowsPath as Path
>>> import qualified Streamly.Internal.Unicode.Stream as Unicode

>>> import Data.Either (Either, isLeft)
>>> import Control.Exception (SomeException, evaluate, try)

>>> pack = fromJust . Path.fromString
>>> fails action = (try (evaluate action) :: IO (Either SomeException String)) >>= return . isLeft
-}

-- Note: We can use powershell for testing path validity.
-- "//share/x" works in powershell.
-- But mixed forward and backward slashes do not work, it is treated as a path
-- relative to current drive e.g. "\\/share/x" is treated as "C:/share/x".
--
-- XXX Note: Windows may have case sensitive behavior depending on the file
-- system being used. Does it impact any of the case insensitive validations
-- below?

-- | Check if the filepath is valid i.e. does the operating system or the file
-- system allow such a path in listing or creating files?
--
-- >>> isValid = Path.isValidPath . Path.rawFromString
--
-- >>> isValid ""
-- False
-- >>> isValid "\0"
-- False
--
-- Windows invalid characters:
--
-- >>> isValid "c::"
-- False
-- >>> isValid "c:\\x:y"
-- False
-- >>> isValid "x*"
-- False
-- >>> isValid "x\ty"
-- False
--
-- Windows invalid path components:
--
-- >>> isValid "pRn.txt"
-- False
-- >>> isValid " pRn .txt"
-- False
-- >>> isValid "c:\\x\\pRn"
-- False
-- >>> isValid "c:\\x\\pRn.txt"
-- False
-- >>> isValid "c:\\pRn\\x"
-- False
-- >>> isValid "c:\\ pRn \\x"
-- False
-- >>> isValid "pRn.x.txt"
-- False
--
-- Windows drive root validations:
--
-- >>> isValid "c:"
-- True
-- >>> isValid "c:a\\b"
-- True
-- >>> isValid "c:\\"
-- True
-- >>> isValid "c:\\\\"
-- False
-- >>> isValid "c:\\/"
-- False
-- >>> isValid "c:\\\\x"
-- False
-- >>> isValid "c:\\/x"
-- False
-- >>> isValid "\\/x/y"
-- True
--
-- Windows share path validations:
--
-- >>> isValid "\\"
-- True
-- >>> isValid "\\\\"
-- False
-- >>> isValid "\\\\\\"
-- False
-- >>> isValid "\\\\x"
-- False
-- >>> isValid "\\\\x\\"
-- True
-- >>> isValid "\\\\x\\y"
-- True
-- >>> isValid "//x/y"
-- True
-- >>> isValid "\\\\prn\\y"
-- False
-- >>> isValid "\\\\x\\\\"
-- False
-- >>> isValid "\\\\x\\\\x"
-- False
-- >>> isValid "\\\\\\x"
-- False
--
-- Windows short UNC path validations:
--
-- >>> isValid "\\\\?\\c:"
-- False
-- >>> isValid "\\\\?\\c:\\"
-- True
-- >>> isValid "\\\\?\\c:x"
-- False
-- >>> isValid "\\\\?\\c:\\\\" -- XXX validate this
-- False
-- >>> isValid "\\\\?\\c:\\x"
-- True
-- >>> isValid "\\\\?\\c:\\\\\\"
-- False
-- >>> isValid "\\\\?\\c:\\\\x"
-- False
--
-- Windows long UNC path validations:
--
-- >>> isValid "\\\\?\\UnC\\x" -- UnC treated as share name
-- True
-- >>> isValid "\\\\?\\UNC\\x" -- XXX fix
-- False
-- >>> isValid "\\\\?\\UNC\\c:\\x"
-- True
--
-- DOS local/global device namespace
--
-- >>> isValid "\\\\.\\x"
-- True
-- >>> isValid "\\\\??\\x"
-- True
isValidPath :: Array OS_WORD_TYPE -> Bool
isValidPath = Common.isValidPath Common.OS_NAME

-- | Like 'validatePath' on but more strict. A share root must be followed by a
-- non-empty path. Thus "\/\/x\/" is not considered a valid path.
validatePath' ::
    MonadThrow m => Array OS_WORD_TYPE -> m ()
validatePath' = Common.validatePath' Common.Windows

-- | Like 'isValidPath' but more strict, see validatePath' for differences.
isValidPath' ::
    Array OS_WORD_TYPE -> Bool
isValidPath' = Common.isValidPath' Common.Windows

-- | Read a raw array of OS_WORD_TYPE as a path type.
--
-- >>> readRaw = fromJust . Path.fromChunk . read
--
-- >>> arr :: Array Word16 = Path.rawFromString "hello"
-- >>> Path.showRaw $ (Path.readRaw $ show arr :: Path.WindowsPath)
-- "fromList [104,101,108,108,111]"
--
-- See also: 'showRaw'.
readRaw :: IsPath OS_PATH_TYPE a => [Char] -> a
readRaw = fromJust . fromChunk . read

-- | A path that is attached to a root. "C:\\" is considered an absolute root
-- and "." as a dynamic root. ".." is not considered a root, "..\/x" or "x\/y"
-- are not rooted paths.
--
-- Absolute locations:
--
-- * @C:\\@ local drive
-- * @\\\\share\\@ UNC share
-- * @\\\\?\\C:\\@ Long UNC local drive
-- * @\\\\?\\UNC\\@ Long UNC remote server
-- * @\\\\.\\@ DOS local device namespace
-- * @\\\\??\\@ DOS global namespace
--
-- Relative locations:
--
-- * @\\@ relative to current drive root
-- * @.\\@ relative to current directory
-- * @C:@ current directory in drive
-- * @C:file@ relative to current directory in drive
--
-- >>> isRooted = Path.isRooted . fromJust . Path.fromString
--
-- Common to Windows and Posix:
--
-- >>> isRooted "/"
-- True
-- >>> isRooted "/x"
-- True
-- >>> isRooted "."
-- True
-- >>> isRooted "./x"
-- True
--
-- Windows specific:
--
-- >>> isRooted "c:"
-- True
-- >>> isRooted "c:x"
-- True
-- >>> isRooted "c:/"
-- True
-- >>> isRooted "//x/y"
-- True
--
isRooted :: OS_PATH_TYPE -> Bool
isRooted (OS_PATH arr) = Common.isRooted Common.OS_NAME arr

-- | Like 'extend' but does not check if any of the path is empty or if the
-- second path is rooted.
--
-- >>> f a b = Path.toString $ Path.unsafeExtend (pack a) (pack b)
--
-- >>> f "x" "y"
-- "x\\y"
-- >>> f "x/" "y"
-- "x/y"
-- >>> f "x" "/y"
-- "x/y"
-- >>> f "x/" "/y"
-- "x/y"
--
-- Note "c:" and "/x" are both rooted paths, therefore, 'extend' cannot be used
-- to join them. Similarly for joining "//x/" and "/y". For these cases use
-- 'unsafeExtend'. 'unsafeExtend' can be used as a replacement for the
-- joinDrive function from the filepath package.
--
-- >>> f "c:" "/x"
-- "c:/x"
-- >>> f "//x/" "/y"
-- "//x/y"
--
{-# INLINE unsafeExtend #-}
unsafeExtend :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
unsafeExtend (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.unsafeAppend
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | Append a OS_PATH_TYPE to another. Fails if the second path refers to a rooted
-- path. If you want to avoid runtime failure use the typesafe
-- Streamly.FileSystem.OS_PATH_TYPE.Seg module. Use 'unsafeExtend' to avoid failure
-- if you know it is ok to append the path.
--
-- Usually, append joins two paths using a separator between the paths. On
-- Windows, joining a drive "c:" with path "x" does not add a separator between
-- the two because "c:x" is different from "c:/x".
--
-- Note "c:" and "/x" are both rooted paths, therefore, 'extend' cannot be used
-- to join them. Similarly for joining "//x/" and "/y". For these cases use
-- 'unsafeExtend'.
--
-- >>> f a b = Path.toString $ Path.extend a b
--
-- >>> f [path|x|] [path|y|]
-- "x\\y"
-- >>> f [path|x/|] [path|y|]
-- "x/y"
-- >>> f [path|c:|] [path|x|]
-- "c:x"
-- >>> f [path|c:/|] [path|x|]
-- "c:/x"
-- >>> f [path|//x/|] [path|y|]
-- "//x/y"
--
-- >>> fails $ f [path|c:|] [path|/|]
-- True
-- >>> fails $ f [path|c:|] [path|/x|]
-- True
-- >>> fails $ f [path|c:/|] [path|/x|]
-- True
-- >>> fails $ f [path|//x/|] [path|/y|]
-- True
extend :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
extend (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | A stricter version of 'extend' which requires the first path to be a
-- directory like path i.e. with a trailing separator.
--
-- >>> f a b = Path.toString $ Path.extendDir a b
--
-- >>> fails $ f [path|x|] [path|y|]
-- True
--
extendDir ::
    OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
extendDir
    (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append'
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | See the eqPath documentation in the
-- "Streamly.Internal.FileSystem.PosixPath" module for details.
--
-- On Windows the following is different:
--
-- * paths are normalized by replacing forward slash path separators by
-- backslashes.
-- * the comparison is case sensitive.
--
-- >>> :{
--  eq a b = Path.eqPath id (pack a) (pack b)
-- :}
--
-- The cases that are different from Posix:
--
-- >>> eq "x\\y" "x/y"
-- True
--
-- >>> eq "x"  "X"
-- False
--
-- >>> eq "c:"  "C:"
-- False
--
-- >>> eq "c:"  "c:"
-- False
--
-- >>> eq "c:x"  "c:x"
-- False
--
-- >>> :{
--  cfg = Path.ignoreTrailingSeparators True
--      . Path.ignoreCase True
--      . Path.allowRelativeEquality True
--  eq a b = Path.eqPath cfg (pack a) (pack b)
-- :}
--
-- >>> eq "./x"  "x"
-- True
--
-- >>> eq "X/"  "x"
-- True
--
-- >>> eq "C:x"  "c:X"
-- True
--
-- >>> eq ".\\x"  "./X"
-- True
--
-- >>> eq "x//y"  "x/y"
-- True
--
-- >>> eq "x/./y"  "x/y"
-- True
--
-- >>> eq "x"  "x"
-- True
--
eqPath :: (EqCfg -> EqCfg) -> OS_PATH_TYPE -> OS_PATH_TYPE -> Bool
eqPath cfg (OS_PATH a) (OS_PATH b) =
    Common.eqPath Unicode.UNICODE_DECODER
        Common.OS_NAME cfg a b

-- | If a path is rooted then separate the root and the remaining path,
-- otherwise root is returned as empty. If the path is rooted then the non-root
-- part is guaranteed to not start with a separator.
--
-- See "Streamly.Internal.FileSystem.PosixPath" module for common examples. We
-- provide some Windows specific examples here.
--
-- >>> toList (a,b) = (Path.toString a, fmap Path.toString b)
-- >>> split = fmap toList . Path.splitRoot . pack
--
-- >>> split "c:"
-- Just ("c:",Nothing)
--
-- >>> split "c:/"
-- Just ("c:/",Nothing)
--
-- >>> split "//x/"
-- Just ("//x/",Nothing)
--
-- >>> split "//x/y"
-- Just ("//x/",Just "y")
--
splitRoot :: OS_PATH_TYPE -> Maybe (OS_PATH_TYPE, Maybe OS_PATH_TYPE)
splitRoot (OS_PATH x) =
    let (a,b) = Common.splitRoot Common.OS_NAME x
     in if Array.null a
        then Nothing
        else if Array.null b
        then Just (OS_PATH a, Nothing)
        else Just (OS_PATH a, Just (OS_PATH b))

-- | Split a path into components separated by the path separator. "."
-- components in the path are ignored except when in the leading position.
-- Trailing separators in non-root components are dropped.
--
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath_ . pack
--
-- >>> split "c:x"
-- ["c:","x"]
--
-- >>> split "c:/" -- Note, c:/ is not the same as c:
-- ["c:/"]
--
-- >>> split "c:/x"
-- ["c:/","x"]
--
-- >>> split "//x/y/"
-- ["//x","y"]
--
-- >>> split "./a"
-- [".","a"]
--
-- >>> split "c:./a"
-- ["c:","a"]
--
-- >>> split "a/."
-- ["a"]
--
-- >>> split "/x"
-- ["/","x"]
--
-- >>> split "/x/\\y"
-- ["/","x","y"]
--
-- >>> split "\\x/\\y"
-- ["\\","x","y"]
--
{-# INLINE splitPath_ #-}
splitPath_ :: Monad m => OS_PATH_TYPE -> Stream m OS_PATH_TYPE
splitPath_ (OS_PATH a) = fmap OS_PATH $ Common.splitPath_ Common.OS_NAME a

-- | Split the path components keeping separators between path components
-- attached to the dir part. Redundant separators are removed, only the first
-- one is kept, but separators are not changed to the default on Windows.
-- Separators are not added either e.g. "." and ".." may not have trailing
-- separators if the original path does not.
--
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath . pack
--
-- >>> split "/x"
-- ["/","x"]
--
-- >>> split "/x/\\y"
-- ["/","x/","y"]
--
-- >>> split "\\x/\\y" -- this is not valid, multiple seps after share?
-- ["\\","x/","y"]
--
{-# INLINE splitPath #-}
splitPath :: Monad m => OS_PATH_TYPE -> Stream m OS_PATH_TYPE
splitPath (OS_PATH a) = fmap OS_PATH $ Common.splitPath Common.OS_NAME a

-- | See "Streamly.Internal.FileSystem.PosixPath" module for detailed
-- documentation and examples. We provide some Windows specific examples here.
--
-- Note: On Windows we cannot create a file named "prn." or "prn..". Thus it
-- considers anything starting with and including the first "." as the
-- extension and the part before it as the filename. Our definition considers
-- "prn." as a filename without an extension.
--
-- >>> toList (a,b) = (Path.toString a, Path.toString b)
-- >>> split = fmap toList . Path.splitExtension . pack
--
-- >>> split "x:y"
-- Nothing
--
-- >>> split "x:.y"
-- Nothing
--
splitExtension :: OS_PATH_TYPE -> Maybe (OS_PATH_TYPE, OS_PATH_TYPE)
splitExtension (OS_PATH a) =
    fmap (bimap OS_PATH OS_PATH) $ Common.splitExtension Common.OS_NAME a
