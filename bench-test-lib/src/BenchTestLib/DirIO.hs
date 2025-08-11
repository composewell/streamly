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
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    , listDirByteChunked
#endif
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Maybe (fromJust)
import Data.Word (Word8)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Streamly.Data.Array (Array)
#endif
import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)
import Streamly.FileSystem.Path (Path)
import Streamly.Unicode.String (str)
import System.Directory (createDirectoryIfMissing)
import Data.Foldable (for_)

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.DirIO as Dir
import qualified Streamly.FileSystem.Path as Path
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import qualified Streamly.Internal.FileSystem.Posix.ReadDir as Dir
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

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
_streamDirByteChunked
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Stream IO (Either [Path] (Array Word8))
_streamDirByteChunked f = either (Dir.readEitherByteChunks f) (const Stream.nil)

streamDirByteChunkedMaybe
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Maybe (Stream IO (Either [Path] (Array Word8)))
streamDirByteChunkedMaybe f =
    either (Just . Dir.readEitherByteChunks f) (const Nothing)
#endif

streamDirChunkedMaybe
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Maybe (Stream IO (Either [Path] [Path]))
streamDirChunkedMaybe f = either (Just . Dir.readEitherChunks f) (const Nothing)

streamDirChunked
    :: (Dir.ReadOptions -> Dir.ReadOptions)
    -> Either [Path] b -> Stream IO (Either [Path] [Path])
streamDirChunked f = either (Dir.readEitherChunks f) (const Stream.nil)

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
                subDir = [str|#{parentDir}/dir_#{iStr}|]
            createDirectoryIfMissing True subDir
            modifyIORef' ref (subDir:)
            createDirStucture_ ref subDir (depth - 1) width

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
-- Fastest implementation, only works for posix as of now.
{-# INLINE listDirByteChunked #-}
listDirByteChunked :: FilePath -> Stream IO (Array Word8)
listDirByteChunked inp = do
     Stream.catRights
        $ Stream.concatIterate (streamDirByteChunkedMaybe id)
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])
#endif

-- Faster than the listDir implementation below
{-# INLINE listDirChunkedWith #-}
listDirChunkedWith
    :: (Stream IO (Either [Path] b) -> Stream IO (Either [Path] [Path]))
    -> [Char] -> Stream IO Word8
listDirChunkedWith act inp = do
     Stream.unfoldEachEndBy 10 Array.reader
        $ fmap (Array.asBytes . Path.toArray)
        $ Stream.unfoldEach Unfold.fromList
        $ fmap (either id id)
        $ act
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])

{-# INLINE listDirWith #-}
listDirWith
    :: (Stream IO (Either Path Path) -> Stream IO (Either Path Path))
    -> [Char] -> Stream IO Word8
listDirWith act inp = do
     Stream.unfoldEachEndBy 10 Array.reader
        $ fmap (Array.asBytes . Path.toArray . either id id)
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
