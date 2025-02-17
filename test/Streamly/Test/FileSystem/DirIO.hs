-- |
-- Module      : Streamly.Test.FileSystem.DirIO
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Word (Word8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Streamly.Unicode.String (str)
import System.Process (readCreateProcess, shell)
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

#define IT(x,sb,dr) \
it " x " $ \
    testCorrectness sb (x dr)

-- | List the current directory recursively
main :: IO ()
main = do
    setLocaleEncoding utf8

    let smallTree = "benchmark-tmp/dir-structure-small"
        bigTree = "benchmark-tmp/dir-structure-big"
    createDirStucture smallTree 2 3
    createDirStucture bigTree 5 5

    findResBig <- readCreateProcess (shell [str|find #{bigTree}|]) ""
    findResSmall <- readCreateProcess (shell [str|find #{smallTree}|]) ""

    strmBaseCacheSmall <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream
            $ Unicode.lines Fold.toList $ Stream.fromList findResSmall
    strmBaseCacheBig <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream
            $ Unicode.lines Fold.toList $ Stream.fromList findResBig
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
            IT(listDirUnfoldDfs,strmBaseBig,bigTree)
            IT(listDirUnfoldBfs,strmBaseSmall,smallTree)
            IT(listDirUnfoldBfsRev,strmBaseSmall,smallTree)
            IT(listDirConcatDfs,strmBaseBig,bigTree)
            IT(listDirConcatBfs,strmBaseSmall,smallTree)
            IT(listDirConcatBfsRev,strmBaseSmall,smallTree)
            IT(listDirAppend,strmBaseBig,bigTree)
            IT(listDirInterleave,strmBaseBig,bigTree)
            IT(listDirPar,strmBaseBig,bigTree)
            IT(listDirParInterleaved,strmBaseBig,bigTree)
            IT(listDirParOrdered,strmBaseBig,bigTree)
            IT(listDirChunkDfs,strmBaseBig,bigTree)
            IT(listDirChunkBfs,strmBaseSmall,smallTree)
            IT(listDirChunkBfsRev,strmBaseSmall,smallTree)
            IT(listDirChunkAppend,strmBaseBig,bigTree)
            IT(listDirChunkInterleave,strmBaseBig,bigTree)
            IT(listDirChunkPar,strmBaseBig,bigTree)
            IT(listDirChunkParInterleaved,strmBaseBig,bigTree)
            IT(listDirChunkParOrdered,strmBaseBig,bigTree)
