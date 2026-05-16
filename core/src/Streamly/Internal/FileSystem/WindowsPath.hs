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
readArray :: [Char] -> OS_PATH_TYPE
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

-- | Append a OS_PATH_TYPE to another. Fails if the second path refers to a
-- rooted path. If you want to avoid runtime failure use the typesafe
-- Streamly.FileSystem.OS_PATH_TYPE.Seg module. Use 'unsafeJoin' to avoid
-- failure if you know it is ok to append the path.
--
-- Usually, it joins two paths using a separator between the paths. On
-- Windows, joining a drive "c:" with path "x" does not add a separator between
-- the two because "c:x" is different from "c:/x".
--
-- Note "c:" and "/x" are both rooted paths, therefore, 'join' cannot be used
-- to join them. Similarly for joining "//x/" and "/y". For these cases use
-- 'unsafeJoin'.
--
-- >>> f a b = Path.toString $ Path.join a b
--
-- When second path is relative to current directory.
--
-- >>> f [path|x|] [path|y|]
-- "x\\y"
-- >>> f [path|c:|] [path|y|]
-- "c:y"
-- >>> f [path|c:x|] [path|y|]
-- "c:x\\y"
-- >>> f [path|\x|] [path|y|]
-- "\\x\\y"
-- >>> f [path|c:/|] [path|y|]
-- "c:/y"
-- >>> f [path|//x/|] [path|y|]
-- "//x/y"
--
-- When second path is relative to current directory in a specific drive.
-- TODO: fix these.
--
-- >> f [path|c:/x|] [path|c:y|]
-- "c:/x/y"
-- >> f [path|c:|] [path|c:y|]
-- "c:y"
-- >> f [path|c:x|] [path|c:y|]
-- "c:x/y"
-- >> fails $ f [path|d:x|] [path|c:y|]
-- True
-- >> fails $ f [path|/x|] [path|c:y|]
-- True
-- >> fails $ f [path|x|] [path|c:y|]
-- True
--
-- When second path is relative to current drive.
--
-- >>> fails $ f [path|c:/|] [path|/y|]
-- True
-- >>> fails $ f [path|c:/x|] [path|/y|]
-- True
-- >>> fails $ f [path|c:|] [path|/y|]
-- True
-- >>> fails $ f [path|c:|] [path|/y|]
-- True
-- >>> fails $ f [path|c:x|] [path|/y|]
-- True
-- >>> fails $ f [path|/x|] [path|/y|]
-- True
-- >>> fails $ f [path|x|] [path|/y|]
-- True
-- >>> fails $ f [path|//x/|] [path|/y|]
-- True
--
-- When second path is absolute.
--
-- >>> fails $ f [path|c:|] [path|c:/|]
-- True
--
-- Original separator is kept if present:
--
-- >>> f [path|x/|] [path|y|]
-- "x/y"
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
-- * forward slash and backslash are treated as equivalent path separators.
-- * the drive letter and UNC server/share name are /always/ compared
--   case-insensitively, regardless of the 'ignoreCase' setting, because
--   Windows file systems treat these case-insensitively. Only the rest of
--   the path follows the 'ignoreCase' setting.
-- * verbatim @\\\\?\\@ device paths are compared byte-for-byte. They bypass
--   all normalisation, including case folding, separator translation and
--   trailing separator handling, regardless of the 'EqCfg' settings.
--
-- >>> :{
--  eq a b = Path.eqPath id (Path.fromString_ a) (Path.fromString_ b)
-- :}
--
-- Separators are interchangeable:
--
-- >>> eq "x\\y" "x/y"
-- True
--
-- The default is case-sensitive for non-root path components, matching Posix:
--
-- >>> eq "x"  "X"
-- False
--
-- Drive-only paths are relative and so are not equal under the default
-- (which has @allowRelativeEquality@ set to 'False'):
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
-- Drive letters compare case-insensitively when paths are absolute:
--
-- >>> eq "c:/x"  "C:/x"
-- True
--
-- UNC authority is also case-insensitive:
--
-- >>> eq "\\\\Server\\Share\\x" "\\\\server\\share\\x"
-- True
--
-- Verbatim @\\\\?\\@ paths are compared byte-for-byte, so they are
-- case-sensitive even on the drive letter and authority parts:
--
-- >>> eq "\\\\?\\C:\\x" "\\\\?\\c:\\x"
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
-- Even with @ignoreCase True@ the verbatim path is not case-folded:
--
-- >>> eq "\\\\?\\C:\\x" "\\\\?\\c:\\x"
-- False
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

------------------------------------------------------------------------------
-- Packing paths into a UTF-8 byte array
------------------------------------------------------------------------------

{-# ANN type PackPathsState Fuse #-}
data PackPathsState =
    PackPathsState !MutByteArray !Int !Int -- buf, pos, cap

-- XXX We can allocate the array on the first input.

-- | Encodes a stream of file paths into a contiguous byte array with UTF-8
-- encoding, appending the given separator byte after each path.
--
-- The input 'WindowsPath' is UTF-16LE encoded; this fold decodes the
-- Word16s and re-encodes them as UTF-8. Invalid code units (lone or invalid
-- surrogates, or input underflow on a high surrogate at end of path) are
-- replaced with the Unicode replacement character U+FFFD.
--
-- Commonly used separators:
--
-- * @0@ : NUL-terminated paths
-- * @10@ : newline-separated paths
--
-- The first argument specifies the initial output buffer size in bytes, if the
-- size is close to the block size it may be rounded to the block size. The
-- buffer is grown further only if even one path can not fit into it. The fold
-- terminates when no more space is left in the buffer to accomodate more
-- paths.
{-# INLINE packPathsEndBy #-}
packPathsEndBy ::
       MonadIO m => Int -> Word8 -> Fold m OS_PATH_TYPE (Array Word8)
packPathsEndBy bufBytes sep = Fold step initial extract extract

    where

    initialCap = MutByteArray.roundUpLargeArray (max 1 bufBytes)

    initial
        | bufBytes < 0 =
            error
                $ "packPathsEndBy: size [" ++ show bufBytes
                ++ "] must be a natural number"
        | otherwise = do
            mbarr <- liftIO $ MutByteArray.new' initialCap
            return $ Fold.Partial (PackPathsState mbarr 0 initialCap)

    -- UTF-8 encode a code point at dst[off..]; returns the offset past the
    -- bytes written.
    {-# INLINE encodeCodePoint #-}
    encodeCodePoint dst off cp
      | cp < 0x80 = do
            MutByteArray.pokeAt off dst (fromIntegral cp :: Word8)
            return (off + 1)
      | cp < 0x800 = do
            MutByteArray.pokeAt off dst
                (fromIntegral ((cp `shiftR` 6) + 0xC0) :: Word8)
            MutByteArray.pokeAt (off + 1) dst
                (fromIntegral ((cp .&. 0x3F) + 0x80) :: Word8)
            return (off + 2)
      | cp < 0x10000 = do
            MutByteArray.pokeAt off dst
                (fromIntegral ((cp `shiftR` 12) + 0xE0) :: Word8)
            MutByteArray.pokeAt (off + 1) dst
                (fromIntegral (((cp `shiftR` 6) .&. 0x3F) + 0x80) :: Word8)
            MutByteArray.pokeAt (off + 2) dst
                (fromIntegral ((cp .&. 0x3F) + 0x80) :: Word8)
            return (off + 3)
      | otherwise = do
            MutByteArray.pokeAt off dst
                (fromIntegral ((cp `shiftR` 18) + 0xF0) :: Word8)
            MutByteArray.pokeAt (off + 1) dst
                (fromIntegral (((cp `shiftR` 12) .&. 0x3F) + 0x80) :: Word8)
            MutByteArray.pokeAt (off + 2) dst
                (fromIntegral (((cp `shiftR` 6) .&. 0x3F) + 0x80) :: Word8)
            MutByteArray.pokeAt (off + 3) dst
                (fromIntegral ((cp .&. 0x3F) + 0x80) :: Word8)
            return (off + 4)

    -- U+FFFD as UTF-8: EF BF BD
    {-# INLINE writeReplacement #-}
    writeReplacement dst off = do
        MutByteArray.pokeAt off dst (0xEF :: Word8)
        MutByteArray.pokeAt (off + 1) dst (0xBF :: Word8)
        MutByteArray.pokeAt (off + 2) dst (0xBD :: Word8)
        return (off + 3)

    -- Convert UTF-16LE Word16s in src[srcOff..srcEnd) (byte offsets) to UTF-8
    -- bytes in dst starting at dstOff. Returns the dst offset past the last
    -- byte written. The caller must ensure dst has at least 3 * srcWordLen
    -- bytes of remaining capacity starting at dstOff (worst-case expansion).
    convert src srcOff srcEnd dst = go srcOff
      where
        go !sOff !dOff
          | sOff >= srcEnd = return dOff
          | otherwise = do
                w :: Word16 <- MutByteArray.peekAt sOff src
                if w < 0xD800 || w > 0xDFFF
                    then do
                        dOff' <- encodeCodePoint dst dOff (fromIntegral w :: Int)
                        go (sOff + 2) dOff'
                    else if w <= 0xDBFF
                        -- high surrogate
                        then do
                            let sOff' = sOff + 2
                            if sOff' >= srcEnd
                                then do
                                    -- input underflow at end of path
                                    writeReplacement dst dOff
                                else do
                                    wLow :: Word16
                                        <- MutByteArray.peekAt sOff' src
                                    if wLow >= 0xDC00 && wLow <= 0xDFFF
                                        then do
                                            let hi = fromIntegral
                                                        (w - 0xD800) :: Int
                                                lo = fromIntegral
                                                        (wLow - 0xDC00) :: Int
                                                cp = 0x10000
                                                   + ((hi `shiftL` 10) .|. lo)
                                            dOff'
                                                <- encodeCodePoint dst dOff cp
                                            go (sOff + 4) dOff'
                                        else do
                                            -- invalid low surrogate
                                            dOff'
                                                <- writeReplacement dst dOff
                                            go (sOff + 2) dOff'
                        else do
                            -- lone low surrogate
                            dOff' <- writeReplacement dst dOff
                            go (sOff + 2) dOff'

    step (PackPathsState buf pos cap) (OS_PATH (Array src start end)) =
        liftIO $ do
            let srcByteLen = end - start
                srcWordLen = srcByteLen `shiftR` 1
                -- Worst case UTF-8 expansion: each Word16 -> 3 bytes, plus
                -- one separator byte.
                needed = pos + 3 * srcWordLen + 1
            (buf1, cap1) <-
                if cap >= needed
                then return (buf, cap)
                else do
                    b <- MutByteArray.reallocSliceAs
                            MutByteArray.Pinned needed buf 0 pos
                    return (b, needed)
            pos1 <- convert src start end buf1 pos
            MutByteArray.pokeAt pos1 buf1 sep
            let pos2 = pos1 + 1
                utf8Len = pos2 - pos
            return
                $ if cap1 - pos2 < max 128 (2 * utf8Len)
                  then Fold.Done (Array buf1 0 pos2)
                  else Fold.Partial (PackPathsState buf1 pos2 cap1)

    extract (PackPathsState buf pos _) = return (Array buf 0 pos)
