-- |
-- Module      : BenchTestLib.DirIO
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

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

import Data.Maybe (fromJust)
import Data.Word (Word8)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Streamly.Data.Array (Array)
#endif
import Streamly.Data.Stream (Stream)
import Streamly.Data.Unfold (Unfold)
import Streamly.FileSystem.Path (Path)
import System.Process (callCommand)

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Array as Array
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.FileSystem.DirIO as Dir
import qualified Streamly.FileSystem.Path as Path
import qualified Streamly.Internal.FileSystem.Path as Path (toChunk)
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

streamDir :: Either Path b -> Stream IO (Either Path Path)
streamDir = either Dir.readEitherPaths (const Stream.nil)

unfoldDir :: Unfold IO (Either Path b) (Either Path Path)
unfoldDir = Unfold.either Dir.eitherReaderPaths Unfold.nil

streamDirMaybe :: Either Path b -> Maybe (Stream IO (Either Path Path))
streamDirMaybe = either (Just . Dir.readEitherPaths) (const Nothing)

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
_streamDirByteChunked
    :: Either [Path] b -> Stream IO (Either [Path] (Array Word8))
_streamDirByteChunked = either Dir.readEitherByteChunks (const Stream.nil)

streamDirByteChunkedMaybe
    :: Either [Path] b -> Maybe (Stream IO (Either [Path] (Array Word8)))
streamDirByteChunkedMaybe =
    either (Just . Dir.readEitherByteChunks) (const Nothing)
#endif

streamDirChunkedMaybe :: Either [Path] b -> Maybe (Stream IO (Either [Path] [Path]))
streamDirChunkedMaybe = either (Just . Dir.readEitherChunks) (const Nothing)

streamDirChunked :: Either [Path] b -> Stream IO (Either [Path] [Path])
streamDirChunked = either Dir.readEitherChunks (const Stream.nil)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

createDirStucture :: FilePath -> IO ()
createDirStucture dirRoot = do
    let cmd =
            "bench-test-lib/create_dir_structure.sh  " ++ dirRoot ++ " 5 5"
    callCommand ("mkdir -p " ++ dirRoot)
    callCommand cmd

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
-- Fastest implementation, only works for posix as of now.
listDirByteChunked :: FilePath -> Stream IO (Array Word8)
listDirByteChunked inp = do
     Stream.catRights
        $ Stream.concatIterateDfs streamDirByteChunkedMaybe
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])
#endif

-- Faster than the listDir implementation below
listDirChunkedWith
    :: (Stream IO (Either [Path] b) -> Stream IO (Either [Path] [Path]))
    -> [Char] -> Stream IO Word8
listDirChunkedWith act inp = do
     Stream.unfoldEachEndBy 10 Array.reader
        $ fmap Path.toChunk
        $ Stream.unfoldEach Unfold.fromList
        $ fmap (either id id)
        $ act
        $ Stream.fromPure (Left [fromJust $ Path.fromString inp])

listDirWith
    :: (Stream IO (Either Path Path) -> Stream IO (Either Path Path))
    -> [Char] -> Stream IO Word8
listDirWith act inp = do
     Stream.unfoldEachEndBy 10 Array.reader
        $ fmap (Path.toChunk . either id id)
        $ act
        $ Stream.fromPure (Left (fromJust $ Path.fromString inp))

#define DEF_LIST_DIR(x,y); \
x :: [Char] -> Stream IO Word8;\
x = listDirWith (y)

DEF_LIST_DIR(listDirUnfoldDfs, Stream.unfoldIterateDfs unfoldDir)
DEF_LIST_DIR(listDirUnfoldBfs, Stream.unfoldIterateBfs unfoldDir)
DEF_LIST_DIR(listDirUnfoldBfsRev, Stream.unfoldIterateBfsRev unfoldDir)
DEF_LIST_DIR(listDirConcatDfs, Stream.concatIterateDfs streamDirMaybe)
DEF_LIST_DIR(listDirConcatBfs, Stream.concatIterateBfs streamDirMaybe)
DEF_LIST_DIR(listDirConcatBfsRev, Stream.concatIterateBfsRev streamDirMaybe)
DEF_LIST_DIR(listDirAppend, concatIterateWith streamDir StreamK.append)
DEF_LIST_DIR(listDirInterleave, mergeIterateWith streamDir StreamK.interleave)
DEF_LIST_DIR(listDirPar, Stream.parConcatIterate id streamDir)
DEF_LIST_DIR(listDirParInterleaved, Stream.parConcatIterate (Stream.interleaved True) streamDir)
DEF_LIST_DIR(listDirParOrdered, Stream.parConcatIterate (Stream.ordered True) streamDir)

#define DEF_LIST_DIR_CHUNKED(x,y); \
x :: [Char] -> Stream IO Word8;\
x = listDirChunkedWith (y)

DEF_LIST_DIR_CHUNKED(listDirChunkDfs, Stream.concatIterateDfs streamDirChunkedMaybe)
DEF_LIST_DIR_CHUNKED(listDirChunkBfs, Stream.concatIterateBfs streamDirChunkedMaybe)
DEF_LIST_DIR_CHUNKED(listDirChunkBfsRev, Stream.concatIterateBfsRev streamDirChunkedMaybe)
DEF_LIST_DIR_CHUNKED(listDirChunkAppend, concatIterateWith streamDirChunked StreamK.append)
DEF_LIST_DIR_CHUNKED(listDirChunkInterleave, mergeIterateWith streamDirChunked StreamK.interleave)
DEF_LIST_DIR_CHUNKED(listDirChunkPar, Stream.parConcatIterate id streamDirChunked)
DEF_LIST_DIR_CHUNKED(listDirChunkParInterleaved, Stream.parConcatIterate (Stream.interleaved True) streamDirChunked)
DEF_LIST_DIR_CHUNKED(listDirChunkParOrdered, Stream.parConcatIterate (Stream.ordered True) streamDirChunked)
