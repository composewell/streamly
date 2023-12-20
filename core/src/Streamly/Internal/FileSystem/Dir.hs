#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.Dir
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC

module Streamly.Internal.FileSystem.Dir
    (
    -- * Streams
      read

    -- read not just the names but also the inode attrs of the children. This
    -- abstraction makes sense because when we read the dir contents we also
    -- get the inodes, and it is cheaper to get the attrs from the inodes
    -- instead of resolving the paths and get those. This abstraction may be
    -- less portable as different platforms may have different attrs. To
    -- optimize, we can also add a filter/pattern/parser on the names of the
    -- children that we want to read. We can call that readAttrsWith? Or just
    -- have the default readAttrs do that? Usually we won't need that, so it
    -- may be better to keep that a separate API.
    -- , readAttrs

    -- recursive read requires us to read the attributes of the children to
    -- determine if something is a dirctory or not. Therefore, it may be a good
    -- idea to have a low level routine that also spits out the attributes of
    -- the files, we get that for free. We can also add a filter/pattern/parser
    -- on the names of the children that we want to read.
    --, readAttrsRecursive -- Options: acyclic, follow symlinks
    , readFiles
    , readDirs
    , readEither
    , readEitherPaths

    -- We can implement this in terms of readAttrsRecursive without losing
    -- perf.
    -- , readEitherRecursive -- Options: acyclic, follow symlinks
    -- , readAncestors -- read the parent chain using the .. entry.
    -- , readAncestorsAttrs

    -- * Unfolds
    -- | Use the more convenient stream APIs instead of unfolds where possible.
    , reader
    , fileReader
    , dirReader
    , eitherReader
    , eitherReaderPaths

      {-
    , toStreamWithBufferOf

    , readChunks
    , readChunksWithBufferOf

    , toChunksWithBufferOf
    , toChunks

    , write
    , writeWithBufferOf

    -- Byte stream write (Streams)
    , fromStream
    , fromStreamWithBufferOf

    -- -- * Array Write
    , writeArray
    , writeChunks
    , writeChunksWithBufferOf

    -- -- * Array stream Write
    , fromChunks
    , fromChunksWithBufferOf
    -}
    -- * Deprecated
    , toStream
    , toEither
    , toFiles
    , toDirs
    )
where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (bimap)
import Data.Either (isRight, isLeft, fromLeft, fromRight)
import Data.Function ((&))
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold (Step(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.FileSystem.ReadDir (readDirStreamEither)
#if (defined linux_HOST_OS) || (defined darwin_HOST_OS) || (defined freebsd_HOST_OS)
import qualified System.OsPath.Posix as OsPathPosix
import System.Posix.Directory.PosixPath (DirStream, closeDirStream)
import qualified System.Posix.Directory.PosixPath as PosixPath
#elif defined(mingw32_HOST_OS)
import qualified System.Win32 as Win32
#else
#error "Unsupported architecture"
#endif
import qualified System.OsPath as OsPath
import System.OsPath (OsPath, (</>))
import System.OsString.Internal.Types as OsString
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold as UF (mapM2, bracketIO)
import qualified Streamly.Data.Stream as S
import qualified System.Directory.OsPath as Dir

import Prelude hiding (read)

{-
{-# INLINABLE readArrayUpto #-}
readArrayUpto :: Int -> Handle -> IO (Array Word8)
readArrayUpto size h = do
    ptr <- mallocPlainForeignPtrBytes size
    -- ptr <- mallocPlainForeignPtrAlignedBytes size (alignment (undefined :: Word8))
    withForeignPtr ptr $ \p -> do
        n <- hGetBufSome h p size
        let v = Array
                { aStart = ptr
                , arrEnd   = p `plusPtr` n
                , arrBound = p `plusPtr` size
                }
        -- XXX shrink only if the diff is significant
        shrinkToFit v

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-- | @toChunksWithBufferOf size h@ reads a stream of arrays from file handle @h@.
-- The maximum size of a single array is specified by @size@. The actual size
-- read may be less than or equal to @size@.
{-# INLINE _toChunksWithBufferOf #-}
_toChunksWithBufferOf :: MonadIO m => Int -> Handle -> Stream m (Array Word8)
_toChunksWithBufferOf size h = go
  where
    -- XXX use cons/nil instead
    go = mkStream $ \_ yld _ stp -> do
        arr <- liftIO $ readArrayUpto size h
        if A.length arr == 0
        then stp
        else yld arr go

-- | @toChunksWithBufferOf size handle@ reads a stream of arrays from the file
-- handle @handle@.  The maximum size of a single array is limited to @size@.
-- The actual size read may be less than or equal to @size@.
--
-- @since 0.7.0
{-# INLINE_NORMAL toChunksWithBufferOf #-}
toChunksWithBufferOf :: MonadIO m => Int -> Handle -> Stream m (Array Word8)
toChunksWithBufferOf size h = D.fromStreamD (D.Stream step ())
  where
    {-# INLINE_LATE step #-}
    step _ _ = do
        arr <- liftIO $ readArrayUpto size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr ()

-- | Unfold the tuple @(bufsize, handle)@ into a stream of 'Word8' arrays.
-- Read requests to the IO device are performed using a buffer of size
-- @bufsize@.  The size of an array in the resulting stream is always less than
-- or equal to @bufsize@.
--
-- @since 0.7.0
{-# INLINE_NORMAL readChunksWithBufferOf #-}
readChunksWithBufferOf :: MonadIO m => Unfold m (Int, Handle) (Array Word8)
readChunksWithBufferOf = Unfold step return
    where
    {-# INLINE_LATE step #-}
    step (size, h) = do
        arr <- liftIO $ readArrayUpto size h
        return $
            case A.length arr of
                0 -> D.Stop
                _ -> D.Yield arr (size, h)

-- XXX read 'Array a' instead of Word8
--
-- | @toChunks handle@ reads a stream of arrays from the specified file
-- handle.  The maximum size of a single array is limited to
-- @defaultChunkSize@. The actual size read may be less than or equal to
-- @defaultChunkSize@.
--
-- > toChunks = toChunksWithBufferOf defaultChunkSize
--
-- @since 0.7.0
{-# INLINE toChunks #-}
toChunks :: MonadIO m => Handle -> Stream m (Array Word8)
toChunks = toChunksWithBufferOf defaultChunkSize

-- | Unfolds a handle into a stream of 'Word8' arrays. Requests to the IO
-- device are performed using a buffer of size
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'. The
-- size of arrays in the resulting stream are therefore less than or equal to
-- 'Streamly.Internal.Data.Array.Type.defaultChunkSize'.
--
-- @since 0.7.0
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m Handle (Array Word8)
readChunks = UF.first readChunksWithBufferOf defaultChunkSize

-------------------------------------------------------------------------------
-- Read a Directory to Stream
-------------------------------------------------------------------------------

-- TODO for concurrent streams implement readahead IO. We can send multiple
-- read requests at the same time. For serial case we can use async IO. We can
-- also control the read throughput in mbps or IOPS.

-- | Unfolds the tuple @(bufsize, handle)@ into a byte stream, read requests
-- to the IO device are performed using buffers of @bufsize@.
--
-- @since 0.7.0
{-# INLINE readWithBufferOf #-}
readWithBufferOf :: MonadIO m => Unfold m (Int, Handle) Word8
readWithBufferOf = UF.many readChunksWithBufferOf A.read

-- | @toStreamWithBufferOf bufsize handle@ reads a byte stream from a file
-- handle, reads are performed in chunks of up to @bufsize@.
--
-- /Pre-release/
{-# INLINE toStreamWithBufferOf #-}
toStreamWithBufferOf :: MonadIO m => Int -> Handle -> Stream m Word8
toStreamWithBufferOf chunkSize h = AS.concat $ toChunksWithBufferOf chunkSize h
-}

-- read child node names from a dir filtering out . and ..
--
-- . and .. are an implementation artifact, and should probably not be used in
-- user level abstractions.
--
-- . does not seem to have any useful purpose. If we have the path of the dir
-- then we will resolve it to get the inode of the dir so the . entry would be
-- redundant. If we have the inode of the dir to read the dir then it is
-- redundant. Is this for cross check when doing fsck?
--
-- For .. we have the readAncestors API, we should not have this in the
-- readChildren API.

-- XXX exception handling

#if (defined linux_HOST_OS) || (defined darwin_HOST_OS) || (defined freebsd_HOST_OS)

openDirStream :: OsPath -> IO DirStream
openDirStream filename =
    PosixPath.openDirStream $ OsString.getOsString filename

readDirStream :: DirStream -> IO OsPath
readDirStream dirStream = do
    filepath <- PosixPath.readDirStream dirStream
    return $ OsString.OsString filepath

{-# INLINE streamReader #-}
streamReader :: (MonadIO m, MonadThrow m) => Unfold m DirStream OsPath
streamReader = Unfold step return
    where

    step strm = do
        -- XXX Use readDirStreamMaybe
        file <- liftIO $ readDirStream strm
        if file == mempty
        then return Stop
        else return $ Yield file strm

{-# INLINE streamEitherReader #-}
streamEitherReader :: MonadIO m =>
    Unfold m DirStream (Either OsPath OsPath)
streamEitherReader = Unfold step return
    where

    toOsPath = either (Left . OsString) (Right . OsString)
    step strm = do
        file <- liftIO $ readDirStreamEither strm
        if file == Left mempty
        then return Stop
        else return $ Yield (toOsPath file) strm

#elif defined(mingw32_HOST_OS)
openDirStream :: String -> IO (Win32.HANDLE, Win32.FindData)
openDirStream = Win32.findFirstFile

closeDirStream :: (Win32.HANDLE, Win32.FindData) -> IO ()
closeDirStream (h, _) = Win32.findClose h

{-# INLINE streamReader #-}
streamReader :: MonadIO m => Unfold m (Win32.HANDLE, Win32.FindData) OsPath
streamReader = Unfold step return

    where

    step (h, fdat) = do
        more <- liftIO $ Win32.findNextFile h fdat
        if more
        then do
            filepath <- liftIO $ Win32.getFindDataFileName fdat
            filename <- OsPath.encodeUtf filepath
            return $ Yield filename (h, fdat)
        else return Stop
#endif

--  | Read a directory emitting a stream with names of the children. Filter out
--  "." and ".." entries.
--
--  /Internal/

{-# INLINE reader #-}
reader :: (MonadIO m, MonadCatch m) => Unfold m OsPath OsPath
reader =
    let
        dot = OsPath.unsafeFromChar '.'
    in
-- XXX Instead of using bracketIO for each iteration of the loop we should
-- instead yield a buffer of dir entries in each iteration and then use an
-- unfold and concat to flatten those entries. That should improve the
-- performance.
      UF.bracketIO openDirStream closeDirStream streamReader
    & UF.filter (\x -> x /= OsPath.pack [dot] && x /= OsPath.pack [dot, dot])

-- XXX We can use a more general mechanism to filter the contents of a
-- directory. We can just stat each child and pass on the stat information. We
-- can then use that info to do a general filtering. "find" like filters can be
-- created.

-- | Read directories as Left and files as Right. Filter out "." and ".."
-- entries.
--
--  /Internal/
--
{-# INLINE eitherReader #-}
eitherReader :: (MonadIO m, MonadCatch m) =>
    Unfold m OsPath (Either OsPath OsPath)
eitherReader =
    -- XXX The measured overhead of bracketIO is not noticeable, if it turns
    -- out to be a problems for small filenames we can use getdents64 to use
    -- chunked read to avoid the overhead.
      UF.bracketIO openDirStream closeDirStream streamEitherReader

{-# INLINE eitherReaderPaths #-}
eitherReaderPaths ::(MonadIO m, MonadCatch m) => Unfold m OsPath (Either OsPath OsPath)
eitherReaderPaths =
    -- XXX Do not resolve the children again
    UF.mapM2 (\dir -> return . bimap (dir </>) (dir </>)) eitherReader

--
-- | Read files only.
--
--  /Internal/
--
{-# INLINE fileReader #-}
fileReader :: (MonadIO m, MonadCatch m) => Unfold m OsPath OsPath
fileReader = fmap (fromRight undefined) $ UF.filter isRight eitherReader

-- | Read directories only. Filter out "." and ".." entries.
--
--  /Internal/
--
{-# INLINE dirReader #-}
dirReader :: (MonadIO m, MonadCatch m) => Unfold m OsPath OsPath
dirReader = fmap (fromLeft undefined) $ UF.filter isLeft eitherReader

-- | Raw read of a directory.
--
-- /Pre-release/
{-# INLINE read #-}
read :: (MonadIO m, MonadCatch m) => OsPath -> Stream m OsPath
read = S.unfold reader

{-# DEPRECATED toStream "Please use 'read' instead" #-}
{-# INLINE toStream #-}
toStream :: (MonadIO m, MonadCatch m) => OsPath -> Stream m OsPath
toStream = read

-- | Read directories as Left and files as Right. Filter out "." and ".."
-- entries. The output contains the names of the directories and files.
--
-- /Pre-release/
{-# INLINE readEither #-}
readEither :: (MonadIO m, MonadCatch m) => OsPath -> Stream m (Either OsPath OsPath)
readEither = S.unfold eitherReader

-- | Like 'readEither' but prefix the names of the files and directories with
-- the supplied directory path.
{-# INLINE readEitherPaths #-}
readEitherPaths :: (MonadIO m, MonadCatch m) => OsPath -> Stream m (Either OsPath OsPath)
readEitherPaths dir = fmap (bimap (dir </>) (dir </>)) $ readEither dir

{-# DEPRECATED toEither "Please use 'readEither' instead" #-}
{-# INLINE toEither #-}
toEither :: (MonadIO m, MonadCatch m) => OsPath -> Stream m (Either OsPath OsPath)
toEither = readEither

-- | Read files only.
--
--  /Internal/
--
{-# INLINE readFiles #-}
readFiles :: (MonadIO m, MonadCatch m) => OsPath -> Stream m OsPath
readFiles = S.unfold fileReader

{-# DEPRECATED toFiles "Please use 'readFiles' instead" #-}
{-# INLINE toFiles #-}
toFiles :: (MonadIO m, MonadCatch m) => OsPath -> Stream m OsPath
toFiles = readFiles

-- | Read directories only.
--
--  /Internal/
--
{-# INLINE readDirs #-}
readDirs :: (MonadIO m, MonadCatch m) => OsPath -> Stream m OsPath
readDirs = S.unfold dirReader

{-# DEPRECATED toDirs "Please use 'readDirs' instead" #-}
{-# INLINE toDirs #-}
toDirs :: (MonadIO m, MonadCatch m) => OsPath -> Stream m OsPath
toDirs = readDirs

{-
-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Array IO (output)
-------------------------------------------------------------------------------

-- | Write an 'Array' to a file handle.
--
-- @since 0.7.0
{-# INLINABLE writeArray #-}
writeArray :: Storable a => Handle -> Array a -> IO ()
writeArray _ arr | A.length arr == 0 = return ()
writeArray h Array{..} = withForeignPtr aStart $ \p -> hPutBuf h p aLen
    where
    aLen =
        let p = unsafeForeignPtrToPtr aStart
        in arrEnd `minusPtr` p

-------------------------------------------------------------------------------
-- Stream of Arrays IO
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream of arrays to a handle.
--
-- @since 0.7.0
{-# INLINE fromChunks #-}
fromChunks :: (MonadIO m, Storable a)
    => Handle -> Stream m (Array a) -> m ()
fromChunks h m = S.mapM_ (liftIO . writeArray h) m

-- | @fromChunksWithBufferOf bufsize handle stream@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- The chunk size is only a maximum and the actual writes could be smaller as
-- we do not split the arrays to fit exactly to the specified size.
--
-- @since 0.7.0
{-# INLINE fromChunksWithBufferOf #-}
fromChunksWithBufferOf :: (MonadIO m, Storable a)
    => Int -> Handle -> Stream m (Array a) -> m ()
fromChunksWithBufferOf n h xs = fromChunks h $ AS.compact n xs

-- | @fromStreamWithBufferOf bufsize handle stream@ writes @stream@ to @handle@
-- in chunks of @bufsize@.  A write is performed to the IO device as soon as we
-- collect the required input size.
--
-- @since 0.7.0
{-# INLINE fromStreamWithBufferOf #-}
fromStreamWithBufferOf :: MonadIO m => Int -> Handle -> Stream m Word8 -> m ()
fromStreamWithBufferOf n h m = fromChunks h $ S.pinnedChunksOf n m
-- fromStreamWithBufferOf n h m = fromChunks h $ AS.chunksOf n m

-- > write = 'writeWithBufferOf' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Type.defaultChunkSize' before writing.
--
-- NOTE: This may perform better than the 'write' fold, you can try this if you
-- need some extra perf boost.
--
-- @since 0.7.0
{-# INLINE fromStream #-}
fromStream :: MonadIO m => Handle -> Stream m Word8 -> m ()
fromStream = fromStreamWithBufferOf defaultChunkSize

-- | Write a stream of arrays to a handle. Each array in the stream is written
-- to the device as a separate IO request.
--
-- @since 0.7.0
{-# INLINE writeChunks #-}
writeChunks :: (MonadIO m, Storable a) => Handle -> Fold m (Array a) ()
writeChunks h = FL.drainBy (liftIO . writeArray h)

-- | @writeChunksWithBufferOf bufsize handle@ writes a stream of arrays
-- to @handle@ after coalescing the adjacent arrays in chunks of @bufsize@.
-- We never split an array, if a single array is bigger than the specified size
-- it emitted as it is. Multiple arrays are coalesed as long as the total size
-- remains below the specified size.
--
-- @since 0.7.0
{-# INLINE writeChunksWithBufferOf #-}
writeChunksWithBufferOf :: (MonadIO m, Storable a)
    => Int -> Handle -> Fold m (Array a) ()
writeChunksWithBufferOf n h = lpackArraysChunksOf n (writeChunks h)

-- GHC buffer size dEFAULT_FD_BUFFER_SIZE=8192 bytes.
--
-- XXX test this
-- Note that if you use a chunk size less than 8K (GHC's default buffer
-- size) then you are advised to use 'NOBuffering' mode on the 'Handle' in case you
-- do not want buffering to occur at GHC level as well. Same thing applies to
-- writes as well.

-- | @writeWithBufferOf reqSize handle@ writes the input stream to @handle@.
-- Bytes in the input stream are collected into a buffer until we have a chunk
-- of @reqSize@ and then written to the IO device.
--
-- @since 0.7.0
{-# INLINE writeWithBufferOf #-}
writeWithBufferOf :: MonadIO m => Int -> Handle -> Fold m Word8 ()
writeWithBufferOf n h = FL.groupsOf n (pinnedWriteNUnsafe n) (writeChunks h)

-- > write = 'writeWithBufferOf' A.defaultChunkSize
--
-- | Write a byte stream to a file handle. Accumulates the input in chunks of
-- up to 'Streamly.Internal.Data.Array.Type.defaultChunkSize' before writing
-- to the IO device.
--
-- @since 0.7.0
{-# INLINE write #-}
write :: MonadIO m => Handle -> Fold m Word8 ()
write = writeWithBufferOf defaultChunkSize
-}
