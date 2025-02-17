-- |
-- Module      : Streamly.Benchmark.FileSystem.DirIO
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

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Streamly.Benchmark.Common (o_1_space_prefix)

import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Fold as Fold

import Prelude hiding (last, length)
import Test.Tasty.Bench
import BenchTestLib.DirIO

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.DirIO"

#define BENCH(x,fp) \
bench " x " $ nfIO $ \
    Stream.fold Fold.drain $ x fp

-- | List the current directory recursively
main :: IO ()
main = do
    setLocaleEncoding utf8

    let smallTree = "benchmark-tmp/dir-structure-small"
        bigTree = "benchmark-tmp/dir-structure-big"
    createDirStucture smallTree 2 3
    createDirStucture bigTree 5 5

    defaultMain
        [ bgroup (o_1_space_prefix moduleName)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
           $ bench "listDirByteChunked" (nfIO $
               Stream.fold Fold.drain $ listDirByteChunked bigTree) :
#endif
            -- NOTE: The BFS traversal fails with:
            -- openDirStream: resource exhausted (Too many open files)
            -- if a bigger directory tree is used
            [ BENCH(listDirUnfoldDfs,bigTree)
            , BENCH(listDirUnfoldDfs,bigTree)
            , BENCH(listDirUnfoldBfs,smallTree)
            , BENCH(listDirUnfoldBfsRev,smallTree)
            , BENCH(listDirConcatDfs,bigTree)
            , BENCH(listDirConcatBfs,smallTree)
            , BENCH(listDirConcatBfsRev,smallTree)
            , BENCH(listDirAppend,bigTree)
            , BENCH(listDirInterleave,bigTree)
            , BENCH(listDirPar,bigTree)
            , BENCH(listDirParInterleaved,bigTree)
            , BENCH(listDirParOrdered,bigTree)
            , BENCH(listDirChunkDfs,bigTree)
            , BENCH(listDirChunkBfs,smallTree)
            , BENCH(listDirChunkBfsRev,smallTree)
            , BENCH(listDirChunkAppend,bigTree)
            , BENCH(listDirChunkInterleave,bigTree)
            , BENCH(listDirChunkPar,bigTree)
            , BENCH(listDirChunkParInterleaved,bigTree)
            , BENCH(listDirChunkParOrdered,bigTree)
            ]
        ]
