-- |
-- Module      : Streamly.Test.FileSystem.DirIO
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Exception (try, IOException)
import Data.Word (Word8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Streamly.Data.Array (Array)
#endif
import System.Directory (createDirectoryLink)

import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Unicode.Stream as Unicode (lines)
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.StreamK as StreamK
import qualified Streamly.FileSystem.DirIO as Dir

import Prelude hiding (last, length)
import BenchTestLib.DirIO

import Test.Hspec as H

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.DirIO"

testCorrectness
    :: [FilePath] -> Stream.Stream IO Word8 -> Expectation
testCorrectness expectation lister = do
    reality <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream
            $ Unicode.lines Fold.toList
            $ Unicode.decodeUtf8 lister
    reality `shouldBe` expectation

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
testCorrectnessByteChunked
    :: [FilePath] -> Stream.Stream IO (Array Word8) -> Expectation
testCorrectnessByteChunked expectation lister = do
    reality <-
         Stream.fold Fold.toList
             $ StreamK.toStream
             $ StreamK.sortBy compare
             $ StreamK.fromStream
             $ Unicode.lines Fold.toList
             $ Unicode.decodeUtf8Chunks lister
    reality `shouldBe` expectation
#endif

ignoringError :: IO a -> IO ()
ignoringError act = do
    (_ :: Either IOException a) <- try act
    pure ()

testSymLinkFollow :: Spec
testSymLinkFollow = do
    let fp = "benchmark-tmp/dir-structure-small-sym"
    -- We create and use a different directory tree here for these tests because
    -- of convinence.
    pathsUnsorted <- runIO $ createDirStucture fp 2 3
    paths <-
        runIO
            $ Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream $ Stream.fromList pathsUnsorted
    runIO $ ignoringError $ do
        createDirectoryLink "./dir_1" (fp ++ "/sym-link-1")
        createDirectoryLink "./dir_1/dir_2" (fp ++ "/sym-link-2")
        createDirectoryLink "./broken_link" (fp ++ "/sym-link-3")
    let answerFollowSym =
            (fp ++ "/sym-link-1")
                : (fp ++ "/sym-link-1/dir_1")
                : (fp ++ "/sym-link-1/dir_2")
                : (fp ++ "/sym-link-1/dir_3")
                : (fp ++ "/sym-link-2")
                : paths
        answerNoFollowSym =
            (fp ++ "/sym-link-1")
                : (fp ++ "/sym-link-2")
                : (fp ++ "/sym-link-3")
                : paths
    sortedAnswerFollowSym <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream $ Stream.fromList answerFollowSym
    sortedAnswerNoFollowSym <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream $ Stream.fromList answerNoFollowSym
    describe "Symlink" $ do
        it "followSymlinks True" $
            testCorrectness
                sortedAnswerFollowSym
                (listDirUnfoldDfs
                     (Dir.followSymlinks True . Dir.ignoreMissing True)
                     fp)
        it "followSymlinks False" $
            testCorrectness
                sortedAnswerNoFollowSym
                (listDirUnfoldDfs
                     (Dir.followSymlinks False . Dir.ignoreMissing True)
                     fp)

-- | List the current directory recursively
main :: IO ()
main = do
    setLocaleEncoding utf8

    let smallTree = "benchmark-tmp/dir-structure-small"
        bigTree = "benchmark-tmp/dir-structure-big"
    pathsSmallUnsorted <- createDirStucture smallTree 2 3
    pathsBigUnsorted <- createDirStucture bigTree 5 5

    pathsSmall <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream $ Stream.fromList pathsSmallUnsorted
    pathsBig <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream $ Stream.fromList pathsBigUnsorted

    hspec $
      describe moduleName $ do
        describe "Sanity" $ do
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
            it "listDirByteChunked" $
                testCorrectnessByteChunked
                    (tail pathsBig) (listDirByteChunked bigTree)
#endif
            -- NOTE: The BFS traversal fails with:
            -- openDirStream: resource exhausted (Too many open files)
            -- if a bigger directory tree is used
            it "listDirUnfoldDfs" $
               testCorrectness pathsBig (listDirUnfoldDfs id bigTree)
            it "listDirUnfoldBfs" $
               testCorrectness pathsSmall (listDirUnfoldBfs id smallTree)
            it "listDirUnfoldBfsRev" $
               testCorrectness pathsSmall (listDirUnfoldBfsRev id smallTree)
            it "listDirConcatDfs" $
               testCorrectness pathsBig (listDirConcatDfs id bigTree)
            it "listDirConcatBfs" $
               testCorrectness pathsSmall (listDirConcatBfs id smallTree)
            it "listDirConcatBfsRev" $
               testCorrectness pathsSmall (listDirConcatBfsRev id smallTree)
            it "listDirAppend" $
               testCorrectness pathsBig (listDirAppend id bigTree)
            it "listDirInterleave" $
               testCorrectness pathsBig (listDirInterleave id bigTree)
            it "listDirPar" $
               testCorrectness pathsBig (listDirPar id bigTree)
            it "listDirParInterleaved" $
               testCorrectness pathsBig (listDirParInterleaved id bigTree)
            it "listDirParOrdered" $
               testCorrectness pathsBig (listDirParOrdered id bigTree)
            it "listDirChunkDfs" $
               testCorrectness pathsBig (listDirChunkDfs id bigTree)
            it "listDirChunkBfs" $
               testCorrectness pathsSmall (listDirChunkBfs id smallTree)
            it "listDirChunkBfsRev" $
               testCorrectness pathsSmall (listDirChunkBfsRev id smallTree)
            it "listDirChunkAppend" $
               testCorrectness pathsBig (listDirChunkAppend id bigTree)
            it "listDirChunkInterleave" $
               testCorrectness pathsBig (listDirChunkInterleave id bigTree)
            it "listDirChunkPar" $
               testCorrectness pathsBig (listDirChunkPar id bigTree)
            it "listDirChunkParInterleaved" $
               testCorrectness pathsBig (listDirChunkParInterleaved id bigTree)
            it "listDirChunkParOrdered" $
               testCorrectness pathsBig (listDirChunkParOrdered id bigTree)
        testSymLinkFollow
