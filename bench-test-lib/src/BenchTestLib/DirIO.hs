-- |
-- Module      : BenchTestLib.DirIO
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE QuasiQuotes #-}

module BenchTestLib.DirIO
    ( createDirStucture
    , listDirUnfoldDfs
    , listDirUnfoldBfs
    , listDirUnfoldBfsRev
    , listDirConcatDfs
    , listDirConcatBfs
    , listDirConcatBfsRev
    , listDirAppend
    , listDirInterleave
    , listDirPar
    , listDirParInterleaved
    , listDirParOrdered
    , listDirChunkDfs
    , listDirChunkBfs
    , listDirChunkBfsRev
    , listDirChunkAppend
    , listDirChunkInterleave
    , listDirChunkPar
    , listDirChunkParInterleaved
    , listDirChunkParOrdered
    , listDirChunkFoldDfs
    , listDirChunkFoldBfs
    , listDirChunkFoldBfsRev
    , listDirChunkFoldAppend
    , listDirChunkFoldInterleave
    , listDirChunkFoldPar
    , listDirChunkFoldParInterleaved
    , listDirChunkFoldParOrdered
    , listDirByteChunked
    , listDirByteChunkedFold
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Foldable (for_)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Maybe (fromJust)
import Data.Word (Word8)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Data.Word (Word16)
#endif
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)
import Streamly.FileSystem.Path (Path)
import Streamly.Unicode.String (str)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.DirIO as Dir
import qualified Streamly.Internal.FileSystem.Path as Path
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified Streamly.Internal.Data.Array as Array (unsafeCast)
import qualified Streamly.Unicode.Stream as Unicode
#endif

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

concatIterateWith :: Monad m =>
                           (a -> Stream m a)
                           -> (StreamK.StreamK m a
                               -> StreamK.StreamK m a -> StreamK.StreamK m a)
                           -> Stream m a
                           -> Stream m a
concatIterateWith nxt f =
      StreamK.toStream
    . StreamK.concatIterateWith f (StreamK.fromStream . nxt)
    . StreamK.fromStream

mergeIterateWith :: Monad m =>
                          (a -> Stream m a)
                          -> (StreamK.StreamK m a
                              -> StreamK.StreamK m a -> StreamK.StreamK m a)
                          -> Stream m a
                          -> Stream m a
mergeIterateWith nxt f =
      StreamK.toStream
    . StreamK.mergeIterateWith f (StreamK.fromStream . nxt)
    . StreamK.fromStream

streamDir
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either Path b -> Stream IO (Either Path Path)
streamDir f = either (Dir.readEitherPaths f) (const Stream.nil)

unfoldDir
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Unfold IO (Either Path b) (Either Path Path)
unfoldDir f = Unfold.either (Dir.eitherReaderPaths f) Unfold.nil

streamDirMaybe
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either Path b -> Maybe (Stream IO (Either Path Path))
streamDirMaybe f = either (Just . Dir.readEitherPaths f) (const Nothing)

_streamDirByteChunked
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Stream IO (Either [Path] (Array Word8))
_streamDirByteChunked f = either (Dir.readEitherByteChunks f) (const Stream.nil)

streamDirByteChunkedMaybe
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Maybe (Stream IO (Either [Path] (Array Word8)))
streamDirByteChunkedMaybe f =
    either (Just . Dir.readEitherByteChunks f) (const Nothing)

-- | Like 'streamDirByteChunkedMaybe' but uses 'Dir.readEitherFold' with
-- 'Path.packPathsEndBy' to produce newline-terminated byte chunks.
streamDirByteChunkedFoldMaybe
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Maybe (Stream IO (Either [Path] (Array Word8)))
streamDirByteChunkedFoldMaybe f =
    either
        (Just . flip (Dir.readEitherFold f) (Path.packPathsEndBy 32000 10))
        (const Nothing)

streamDirChunkedMaybe
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Maybe (Stream IO (Either [Path] [Path]))
streamDirChunkedMaybe f = either (Just . Dir.readEitherChunks f) (const Nothing)

streamDirChunked
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Stream IO (Either [Path] [Path])
streamDirChunked f = either (Dir.readEitherChunks f) (const Stream.nil)

-- | Same shape as 'streamDirChunkedMaybe' but uses 'Dir.readEitherFold' with
-- 'Fold.toList', which should be equivalent in coverage to
-- 'Dir.readEitherChunks' for traversal tests.
streamDirChunkedFoldMaybe
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Maybe (Stream IO (Either [Path] [Path]))
streamDirChunkedFoldMaybe f =
    either (Just . flip (Dir.readEitherFold f) Fold.toList) (const Nothing)

streamDirChunkedFold
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Stream IO (Either [Path] [Path])
streamDirChunkedFold f =
    either (flip (Dir.readEitherFold f) Fold.toList) (const Stream.nil)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

createDirStucture :: FilePath -> Int -> Int -> IO [FilePath]
createDirStucture root d w = do
    ref <- newIORef [root]
    createDirStucture_ ref root d w
    readIORef ref
    where
    createDirStucture_ _ _ depth _ | depth <= 0 = pure ()
    createDirStucture_ ref parentDir depth width = do
        for_ [1..width] $ \i -> do
            let iStr = show i
                subDir = [str|#{parentDir}|] </> [str|dir_#{iStr}|]
            createDirectoryIfMissing True subDir
            modifyIORef' ref (subDir:)
            createDirStucture_ ref subDir (depth - 1) width

-- Fastest implementation. On Windows the underlying readEitherByteChunks
-- emits UTF-16LE bytes (each char as 2 bytes, separator '\\' and newline '\n'
-- also encoded as 2 bytes). Chunks are split at entry boundaries, so each
-- chunk can be transcoded independently to UTF-8 without splitting surrogate
-- pairs. On Posix the bytes are already UTF-8.
{-# INLINE toUtf8Chunks #-}
toUtf8Chunks :: Stream IO (Array Word8) -> Stream IO (Array Word8)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toUtf8Chunks = Stream.mapM utf16leChunkToUtf8

utf16leChunkToUtf8 :: Array Word8 -> IO (Array Word8)
utf16leChunkToUtf8 arr =
    Stream.fold Array.create
        $ Unicode.encodeUtf8
        $ Unicode.decodeUtf16le
        $ Array.read (Array.unsafeCast arr :: Array Word16)
#else
toUtf8Chunks = id
#endif

{-# INLINE listDirByteChunked #-}
listDirByteChunked :: FilePath -> Stream IO (Array Word8)
listDirByteChunked inp =
    toUtf8Chunks
        $ Stream.catRights
        $ Stream.concatIterate (streamDirByteChunkedMaybe id)
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])

-- | Like 'listDirByteChunked' but uses 'Dir.readEitherFold' with
-- 'Path.packPathsEndBy' to compact paths into newline-terminated byte chunks.
{-# INLINE listDirByteChunkedFold #-}
listDirByteChunkedFold :: FilePath -> Stream IO (Array Word8)
listDirByteChunkedFold inp = do
     Stream.catRights
        $ Stream.concatIterate (streamDirByteChunkedFoldMaybe id)
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])

-- Faster than the listDir implementation below
{-# INLINE listDirChunkedWith #-}
listDirChunkedWith
    :: (Stream IO (Either [Path] b) -> Stream IO (Either [Path] [Path]))
    -> [Char] -> Stream IO Word8
listDirChunkedWith act inp = do
     Stream.unfoldEachEndBy 10 Array.reader
        $ fmap Path.toUtf8Array
        $ Stream.unfoldEach Unfold.fromList
        $ fmap (either id id)
        $ act
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])

-- | Variant of 'listDirChunkedWith' for the 'readEitherFold' variants. The
-- fold receives both directory and file entries, so its 'Right' output already
-- contains every path; the 'Left' chunks are only used to drive further
-- traversal and must be discarded here to avoid emitting directory paths
-- twice.
{-# INLINE listDirChunkedFoldWith #-}
listDirChunkedFoldWith
    :: (Stream IO (Either [Path] b) -> Stream IO (Either [Path] [Path]))
    -> [Char] -> Stream IO Word8
listDirChunkedFoldWith act inp = do
     Stream.unfoldEachEndBy 10 Array.reader
        $ fmap Path.toUtf8Array
        $ Stream.unfoldEach Unfold.fromList
        $ Stream.catRights
        $ act
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])

{-# INLINE listDirWith #-}
listDirWith
    :: (Stream IO (Either Path Path) -> Stream IO (Either Path Path))
    -> [Char] -> Stream IO Word8
listDirWith act inp = do
     Stream.unfoldEachEndBy 10 Array.reader
        $ fmap (Path.toUtf8Array . either id id)
        $ act
        $ Stream.fromPure (Left (fromJust $ Path.fromString inp))

--------------------------------------------------------------------------------
-- Non chunked
--------------------------------------------------------------------------------

{-# INLINE listDirUnfoldDfs #-}
listDirUnfoldDfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirUnfoldDfs f = listDirWith (Stream.unfoldIterate (unfoldDir f))

{-# INLINE listDirUnfoldBfs #-}
listDirUnfoldBfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirUnfoldBfs f = listDirWith (Stream.bfsUnfoldIterate (unfoldDir f))

{-# INLINE listDirUnfoldBfsRev #-}
listDirUnfoldBfsRev
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirUnfoldBfsRev f = listDirWith (Stream.altBfsUnfoldIterate (unfoldDir f))

{-# INLINE listDirConcatDfs #-}
listDirConcatDfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirConcatDfs f = listDirWith (Stream.concatIterate (streamDirMaybe f))

{-# INLINE listDirConcatBfs #-}
listDirConcatBfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirConcatBfs f = listDirWith (Stream.bfsConcatIterate (streamDirMaybe f))

{-# INLINE listDirConcatBfsRev #-}
listDirConcatBfsRev
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirConcatBfsRev f =
    listDirWith (Stream.altBfsConcatIterate (streamDirMaybe f))

{-# INLINE listDirAppend #-}
listDirAppend
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirAppend f = listDirWith (concatIterateWith (streamDir f) StreamK.append)

{-# INLINE listDirInterleave #-}
listDirInterleave
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirInterleave f =
    listDirWith (mergeIterateWith (streamDir f) StreamK.interleave)

{-# INLINE listDirPar #-}
listDirPar
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirPar f = listDirWith (Stream.parConcatIterate id (streamDir f))

{-# INLINE listDirParInterleaved #-}
listDirParInterleaved
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirParInterleaved f =
    listDirWith
        (Stream.parConcatIterate (Stream.interleaved True) (streamDir f))

{-# INLINE listDirParOrdered #-}
listDirParOrdered
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirParOrdered f =
    listDirWith (Stream.parConcatIterate (Stream.ordered True) (streamDir f))

--------------------------------------------------------------------------------
-- Chunked
--------------------------------------------------------------------------------

{-# INLINE listDirChunkDfs #-}
listDirChunkDfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkDfs f =
    listDirChunkedWith (Stream.concatIterate (streamDirChunkedMaybe f))

{-# INLINE listDirChunkBfs #-}
listDirChunkBfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkBfs f =
    listDirChunkedWith (Stream.bfsConcatIterate (streamDirChunkedMaybe f))

{-# INLINE listDirChunkBfsRev #-}
listDirChunkBfsRev
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkBfsRev f =
    listDirChunkedWith (Stream.altBfsConcatIterate (streamDirChunkedMaybe f))

{-# INLINE listDirChunkAppend #-}
listDirChunkAppend
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkAppend f =
    listDirChunkedWith (concatIterateWith (streamDirChunked f) StreamK.append)

{-# INLINE listDirChunkInterleave #-}
listDirChunkInterleave
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkInterleave f =
    listDirChunkedWith
        (mergeIterateWith (streamDirChunked f) StreamK.interleave)

{-# INLINE listDirChunkPar #-}
listDirChunkPar
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkPar f =
    listDirChunkedWith (Stream.parConcatIterate id (streamDirChunked f))

{-# INLINE listDirChunkParInterleaved #-}
listDirChunkParInterleaved
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkParInterleaved f =
    listDirChunkedWith
        (Stream.parConcatIterate (Stream.interleaved True) (streamDirChunked f))

{-# INLINE listDirChunkParOrdered #-}
listDirChunkParOrdered
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkParOrdered f =
    listDirChunkedWith
        (Stream.parConcatIterate (Stream.ordered True) (streamDirChunked f))

--------------------------------------------------------------------------------
-- Chunked via readEitherFold + Fold.toList
--------------------------------------------------------------------------------

-- These mirror the listDirChunk* variants above but exercise 'readEitherFold'
-- with 'Fold.toList'. Since the fold receives all entries (dirs and files),
-- they use 'listDirChunkedFoldWith' which keeps only the 'Right' output and
-- discards the 'Left' traversal-driver chunks.

{-# INLINE listDirChunkFoldDfs #-}
listDirChunkFoldDfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldDfs f =
    listDirChunkedFoldWith (Stream.concatIterate (streamDirChunkedFoldMaybe f))

{-# INLINE listDirChunkFoldBfs #-}
listDirChunkFoldBfs
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldBfs f =
    listDirChunkedFoldWith
        (Stream.bfsConcatIterate (streamDirChunkedFoldMaybe f))

{-# INLINE listDirChunkFoldBfsRev #-}
listDirChunkFoldBfsRev
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldBfsRev f =
    listDirChunkedFoldWith
        (Stream.altBfsConcatIterate (streamDirChunkedFoldMaybe f))

{-# INLINE listDirChunkFoldAppend #-}
listDirChunkFoldAppend
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldAppend f =
    listDirChunkedFoldWith
        (concatIterateWith (streamDirChunkedFold f) StreamK.append)

{-# INLINE listDirChunkFoldInterleave #-}
listDirChunkFoldInterleave
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldInterleave f =
    listDirChunkedFoldWith
        (mergeIterateWith (streamDirChunkedFold f) StreamK.interleave)

{-# INLINE listDirChunkFoldPar #-}
listDirChunkFoldPar
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldPar f =
    listDirChunkedFoldWith
        (Stream.parConcatIterate id (streamDirChunkedFold f))

{-# INLINE listDirChunkFoldParInterleaved #-}
listDirChunkFoldParInterleaved
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldParInterleaved f =
    listDirChunkedFoldWith
        (Stream.parConcatIterate
            (Stream.interleaved True) (streamDirChunkedFold f))

{-# INLINE listDirChunkFoldParOrdered #-}
listDirChunkFoldParOrdered
    :: (Dir.ReadOptions -> Dir.ReadOptions) -> [Char] -> Stream IO Word8
listDirChunkFoldParOrdered f =
    listDirChunkedFoldWith
        (Stream.parConcatIterate
            (Stream.ordered True) (streamDirChunkedFold f))
