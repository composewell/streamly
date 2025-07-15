{-# LANGUAGE CPP #-}
#define IS_WINDOWS
#include "Streamly/Internal/FileSystem/PosixPath.hs"

-- XXX Move these functions to PosixPath.hs and use CPP conditionals for
-- documentation differences, definitions are identical.

-- Note: We can use powershell for testing path validity.
-- "//share/x" works in powershell.
-- But mixed forward and backward slashes do not work, it is treated as a path
-- relative to current drive e.g. "\\/share/x" is treated as "C:/share/x".
--
-- XXX Note: Windows may have case sensitive behavior depending on the file
-- system being used. Does it impact any of the case insensitive validations
-- below?
--
-- XXX ADS - alternate data stream syntax - file.txt:stream .

-- | Like 'validatePath' but more strict. The path must refer to a file system
-- object. For example, a share root itself is not a valid file system object.
-- it must be followed by a non-empty path.
--
-- >>> isValid = isJust . Path.validatePath' . Path.encodeString
--
-- >>> isValid "\\\\"
-- False
-- >>> isValid "\\\\server\\"
-- False
-- >>> isValid "\\\\server\\x"
-- True
-- >>> isValid "\\\\?\\UNC\\server"
-- False
--
validatePath' ::
    MonadThrow m => Array OS_WORD_TYPE -> m ()
validatePath' = Common.validatePath' Common.Windows

-- | Like 'isValidPath' but more strict.
--
-- >>> isValidPath' = isJust . Path.validatePath'
--
isValidPath' ::
    Array OS_WORD_TYPE -> Bool
isValidPath' = isJust . validatePath'

-- | Read a raw array of OS_WORD_TYPE as a path type.
--
-- >>> readArray = fromJust . Path.fromArray . read
--
-- >>> arr :: Array Word16 = Path.encodeString "hello"
-- >>> Path.showArray $ (Path.readArray $ show arr :: Path.WindowsPath)
-- "fromList [104,101,108,108,111]"
--
-- See also: 'showArray'.
readArray :: IsPath OS_PATH_TYPE a => [Char] -> a
readArray = fromJust . fromArray . read

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

-- | Like 'join' but does not check if any of the path is empty or if the
-- second path is rooted.
--
-- >>> f a b = Path.toString $ Path.unsafeJoin (Path.fromString_ a) (Path.fromString_ b)
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
-- Note "c:" and "/x" are both rooted paths, therefore, 'join' cannot be used
-- to join them. Similarly for joining "//x/" and "/y". For these cases use
-- 'unsafeJoin'. 'unsafeJoin' can be used as a replacement for the
-- joinDrive function from the filepath package.
--
-- >>> f "c:" "/x"
-- "c:/x"
-- >>> f "//x/" "/y"
-- "//x/y"
--
{-# INLINE unsafeJoin #-}
unsafeJoin :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
unsafeJoin (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.unsafeAppend
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | Append a OS_PATH_TYPE to another. Fails if the second path refers to a rooted
-- path. If you want to avoid runtime failure use the typesafe
-- Streamly.FileSystem.OS_PATH_TYPE.Seg module. Use 'unsafeJoin' to avoid failure
-- if you know it is ok to append the path.
--
-- Usually, append joins two paths using a separator between the paths. On
-- Windows, joining a drive "c:" with path "x" does not add a separator between
-- the two because "c:x" is different from "c:/x".
--
-- Note "c:" and "/x" are both rooted paths, therefore, 'join' cannot be used
-- to join them. Similarly for joining "//x/" and "/y". For these cases use
-- 'unsafeJoin'.
--
-- >>> f a b = Path.toString $ Path.join a b
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
join :: OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
join (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | A stricter version of 'join' which requires the first path to be a
-- directory like path i.e. with a trailing separator.
--
-- >>> f a b = Path.toString $ Path.joinDir a b
--
-- >>> fails $ f [path|x|] [path|y|]
-- True
--
joinDir ::
    OS_PATH_TYPE -> OS_PATH_TYPE -> OS_PATH_TYPE
joinDir
    (OS_PATH a) (OS_PATH b) =
    OS_PATH
        $ Common.append'
            Common.OS_NAME (Common.toString Unicode.UNICODE_DECODER) a b

-- | See the eqPath documentation in the
-- "Streamly.Internal.FileSystem.PosixPath" module for details.
--
-- On Windows, the following is different:
--
-- * paths are normalized by replacing forward slash path separators by
-- backslashes.
-- * default configuration uses case-insensitive comparison.
--
-- >>> :{
--  eq a b = Path.eqPath id (Path.fromString_ a) (Path.fromString_ b)
-- :}
--
-- The cases that are different from Posix:
--
-- >>> eq "x\\y" "x/y"
-- True
--
-- >>> eq "x"  "X"
-- True
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
--  eq a b = Path.eqPath cfg (Path.fromString_ a) (Path.fromString_ b)
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
        Common.OS_NAME (cfg eqCfg) a b

-- | If a path is rooted then separate the root and the remaining path,
-- otherwise root is returned as empty. If the path is rooted then the non-root
-- part is guaranteed to not start with a separator.
--
-- See "Streamly.Internal.FileSystem.PosixPath" module for common examples. We
-- provide some Windows specific examples here.
--
-- >>> toList (a,b) = (Path.toString a, fmap Path.toString b)
-- >>> split = fmap toList . Path.splitRoot . Path.fromString_
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
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath_ . Path.fromString_
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
-- >>> split = Stream.toList . fmap Path.toString . Path.splitPath . Path.fromString_
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
-- >>> split = fmap toList . Path.splitExtension . Path.fromString_
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
