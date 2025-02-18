-- |
-- Module      : Streamly.Test.FileSystem.DirIO
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Word (Word8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Streamly.Data.Array (Array)
#endif

import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Unicode.Stream as Unicode (lines)
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.StreamK as StreamK

import Prelude hiding (last, length)
import BenchTestLib.DirIO

import Test.Hspec as H

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.DirIO"

testCorrectness
    :: Stream.Stream IO [Char] -> Stream.Stream IO Word8 -> Expectation
testCorrectness strmBase lister = do
    let strm =
            StreamK.toStream
                $ StreamK.sortBy compare
                $ StreamK.fromStream
                $ Unicode.lines Fold.toList
                $ Unicode.decodeUtf8 lister
    Stream.eqBy (==) strm strmBase `shouldReturn` True

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
testCorrectnessByteChunked
    :: Stream.Stream IO [Char] -> Stream.Stream IO (Array Word8) -> Expectation
testCorrectnessByteChunked strmBase lister = do
    let strm =
            StreamK.toStream
                $ StreamK.sortBy compare
                $ StreamK.fromStream
                $ Unicode.lines Fold.toList
                $ Unicode.decodeUtf8Chunks lister
    Stream.eqBy (==) strm strmBase `shouldReturn` True
#endif

-- | List the current directory recursively
main :: IO ()
main = do
    setLocaleEncoding utf8

    let smallTree = "benchmark-tmp/dir-structure-small"
        bigTree = "benchmark-tmp/dir-structure-big"
    resSmall <- createDirStucture smallTree 2 3
    resBig <- createDirStucture bigTree 5 5

    strmBaseCacheSmall <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream $ Stream.fromList resSmall
    strmBaseCacheBig <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream $ Stream.fromList resBig
    let strmBaseSmall = Stream.fromList strmBaseCacheSmall
    let strmBaseBig = Stream.fromList strmBaseCacheBig

    hspec $
        describe moduleName $ do
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
            it "listDirByteChunked" $
                testCorrectnessByteChunked
                    (Stream.drop 1 strmBaseBig) (listDirByteChunked bigTree)
#endif
            -- NOTE: The BFS traversal fails with:
            -- openDirStream: resource exhausted (Too many open files)
            -- if a bigger directory tree is used
            it "listDirUnfoldDfs" $
               testCorrectness strmBaseBig (listDirUnfoldDfs bigTree)
            it "listDirUnfoldBfs" $
               testCorrectness strmBaseSmall (listDirUnfoldBfs smallTree)
            it "listDirUnfoldBfsRev" $
               testCorrectness strmBaseSmall (listDirUnfoldBfsRev smallTree)
            it "listDirConcatDfs" $
               testCorrectness strmBaseBig (listDirConcatDfs bigTree)
            it "listDirConcatBfs" $
               testCorrectness strmBaseSmall (listDirConcatBfs smallTree)
            it "listDirConcatBfsRev" $
               testCorrectness strmBaseSmall (listDirConcatBfsRev smallTree)
            it "listDirAppend" $
               testCorrectness strmBaseBig (listDirAppend bigTree)
            it "listDirInterleave" $
               testCorrectness strmBaseBig (listDirInterleave bigTree)
            it "listDirPar" $
               testCorrectness strmBaseBig (listDirPar bigTree)
            it "listDirParInterleaved" $
               testCorrectness strmBaseBig (listDirParInterleaved bigTree)
            it "listDirParOrdered" $
               testCorrectness strmBaseBig (listDirParOrdered bigTree)
            it "listDirChunkDfs" $
               testCorrectness strmBaseBig (listDirChunkDfs bigTree)
            it "listDirChunkBfs" $
               testCorrectness strmBaseSmall (listDirChunkBfs smallTree)
            it "listDirChunkBfsRev" $
               testCorrectness strmBaseSmall (listDirChunkBfsRev smallTree)
            it "listDirChunkAppend" $
               testCorrectness strmBaseBig (listDirChunkAppend bigTree)
            it "listDirChunkInterleave" $
               testCorrectness strmBaseBig (listDirChunkInterleave bigTree)
            it "listDirChunkPar" $
               testCorrectness strmBaseBig (listDirChunkPar bigTree)
            it "listDirChunkParInterleaved" $
               testCorrectness strmBaseBig (listDirChunkParInterleaved bigTree)
            it "listDirChunkParOrdered" $
               testCorrectness strmBaseBig (listDirChunkParOrdered bigTree)
