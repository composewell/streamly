#include "inline.hs"

-- |
-- Module      : Streamly.Internal.FileSystem.DirIO
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
--  API Design notes:
--
-- The paths returned by "read" can be absolute (/usr/bin/ls), relative to
-- current directory (./bin/ls) or path segments relative to current dir
-- (bin/ls). To accomodate all the cases we can provide a prefix to attach
-- to the paths being generated. Alternatively, we could take the approach
-- of the higher layer doing that, but it is more efficient to allocate the
-- path buffer once rather than modifying it later. We can do this by
-- passing a fold to transform the output.
--
-- Also it may be more efficient to apply a filter to the paths right here
-- instead of applying it in a layer above. Cut the output at the source
-- rather than generate and then discard it later. We can do this by
-- passing a fold to filter the input.
--
-- When reading a symlink directory we can resolve the symlink and read the
-- destination directory or we can just emit the file it is pointing to and
-- the read can happen next at the higher level, in the traversal logic
-- (concatIterate). Not sure if one approach has any significant perf impact
-- over the other. Similar thinking applies to a mount point as well. Also, if
-- we resolve the symlinks in concatIterate, then each resolution will be
-- counted as depth level increment whereas if we resolve that at lower level
-- then it won't. We can do this by passing an option to modify the behavior.
--
-- When resolving cyclic directory symlinks one way to curtail it is ELOOP
-- which gives up if it encounters too many level. Another way is to use
-- the inode information to check if we are traversing an already traversed
-- inode, this is in general helpful in a graph traversal. We can ignore
-- ELOOP by passing an option but it may be inefficient because we may
-- encounter the loop from any node in the cycle.
--
-- If we encounter an error reading a directory because of permission
-- issues should we ignore it in this low level API or catch it in the
-- higher level traversal functionality? Similarly, if there are broken
-- symlinks, where to handle the error? Need to check performance when
-- handling it in ListDir. Suppressing the error at the lower level may be
-- more efficient than propagating it up and then handling it there. We can
-- do this by passing an option.
--
-- Returning the metadata:
--
-- Specific scans can be used to return the metadata in the output stream if
-- needed. However, we may need three different APIs:
-- one with fast metadata, and
-- another with full metadata. In the two cases the fold input would be
-- different.
--
-- * readMinimal: read only the path names, no metadata
-- * readStandard: read the path and minimal metadata
-- * readFull: read full metadata
--
-- NOTE: Full metadata can be read by mapping a stat call to a stream of paths
-- rather than via readdir API. Does it help the performance to do it in the
-- readdir API?

-- Design pattern:
--
-- By passing a scan we can process the output right at the source and produce
-- a cooked output. Otherwise we may have to produce a stream of intermediate
-- structures which may have more per item overhead and that overhead may not
-- get eliminated by fusion. For example, a fold can directly write the CString
-- from readdir to the output buffer whereas if we output the Path then we will
-- incur an overhead of intermediate structure.

module Streamly.Internal.FileSystem.DirIO
    (
    -- XXX Create a Metadata or Meta module for stat, access, getxattr, chmod,
    -- chown, utime, rename operations.
    --
    -- * Metadata
    -- getMetadata GetMetadata (followSymlinks, noAutoMount - see fstatat)

    -- * Configuration
      ReadOptions
    , defaultReadOptions
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    , followSymlinks
    , ignoreNonExisting
    , ignoreLoopErrors
    , ignoreInAccessible
#endif

    -- * Streams
    , read

    -- Is there a benefit in providing a low level recursive read or
    -- concatIterate is good enough? Could be more efficient for non-concurrent
    -- reads by using a local loop. Or during concurrent reads use
    -- non-concurrent reads as we go deeper down in the tree.
    -- , readAttrsRecursive

    , readFiles
    , readDirs
    , readEither
    , readEitherPaths
    , readEitherChunks

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
    )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor (bimap)
import Data.Either (isRight, isLeft, fromLeft, fromRight)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.FileSystem.Path (Path)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Streamly.Internal.Data.Fold as Fold
import Streamly.Internal.FileSystem.Windows.ReadDir
    (eitherReader, reader, ReadOptions, defaultReadOptions)
#else
import Streamly.Internal.FileSystem.Posix.ReadDir
    ( readEitherChunks, eitherReader, reader, ReadOptions, defaultReadOptions
    , followSymlinks, ignoreNonExisting, ignoreLoopErrors, ignoreInAccessible
    )
#endif
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold as UF (mapM2)
import qualified Streamly.Internal.FileSystem.Path as Path

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

-- XXX We can use a more general mechanism to filter the contents of a
-- directory. We can just stat each child and pass on the stat information. We
-- can then use that info to do a general filtering. "find" like filters can be
-- created.

{-# INLINE eitherReaderPaths #-}
eitherReaderPaths ::(MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Unfold m Path (Either Path Path)
eitherReaderPaths f =
    let (</>) = Path.join
     in UF.mapM2 (\dir -> return . bimap (dir </>) (dir </>)) (eitherReader f)

--
-- | Read files only.
--
--  /Internal/
--
{-# INLINE fileReader #-}
fileReader :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Unfold m Path Path
fileReader f = fmap (fromRight undefined) $ UF.filter isRight (eitherReader f)

-- | Read directories only. Filter out "." and ".." entries.
--
--  /Internal/
--
{-# INLINE dirReader #-}
dirReader :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Unfold m Path Path
dirReader f = fmap (fromLeft undefined) $ UF.filter isLeft (eitherReader f)

-- | Raw read of a directory.
--
-- /Pre-release/
{-# INLINE read #-}
read :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Path -> Stream m Path
read f = S.unfold (reader f)

-- | Read directories as Left and files as Right. Filter out "." and ".."
-- entries. The output contains the names of the directories and files.
--
-- /Pre-release/
{-# INLINE readEither #-}
readEither :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Path -> Stream m (Either Path Path)
readEither f = S.unfold (eitherReader f)

-- | Like 'readEither' but prefix the names of the files and directories with
-- the supplied directory path.
{-# INLINE readEitherPaths #-}
readEitherPaths :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Path -> Stream m (Either Path Path)
readEitherPaths f dir =
    let (</>) = Path.join
     in fmap (bimap (dir </>) (dir </>)) $ readEither f dir

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
-- XXX Implement a custom version of readEitherChunks (like for Posix) for
-- windows as well. Also implement readEitherByteChunks.
--
-- XXX For a fast custom implementation of traversal, the Right could be the
-- final array chunk including all files and dirs to be written to IO. The Left
-- could be list of dirs to be traversed.
--
-- This is a generic (but slower?) version of readEitherChunks using
-- eitherReaderPaths.
{-# INLINE readEitherChunks #-}
readEitherChunks :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    [Path] -> Stream m (Either [Path] [Path])
readEitherChunks f dirs =
    -- XXX Need to use a take to limit the group size. There will be separate
    -- limits for dir and files groups.
     S.groupsWhile grouper collector
        $ S.unfoldEach (eitherReaderPaths f)
        $ S.fromList dirs

    where

    -- XXX We can use a refold "Either dirs files" and yield the one that fills
    -- and pass the remainder to the next Refold.
    grouper first next =
        case first of
            Left _ -> isLeft next
            Right _ -> isRight next

    collector = Fold.foldl' step (Right [])

    step b x =
        case x of
            Left x1 ->
                case b of
                    Right _ -> Left [x1] -- initial
                    _ -> either (\xs -> Left (x1:xs)) Right b
            Right x1 -> fmap (x1:) b
#endif

-- | Read files only.
--
--  /Internal/
--
{-# INLINE readFiles #-}
readFiles :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Path -> Stream m Path
readFiles f = S.unfold (fileReader f)

-- | Read directories only.
--
--  /Internal/
--
{-# INLINE readDirs #-}
readDirs :: (MonadIO m, MonadCatch m) => (ReadOptions -> ReadOptions) ->
    Path -> Stream m Path
readDirs f = S.unfold (dirReader f)

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
