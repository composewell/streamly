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

#define BENCH(x) \
bench " x " $ nfIO $ \
    Stream.fold Fold.drain $ x dirRoot

-- | List the current directory recursively
main :: IO ()
main = do
    setLocaleEncoding utf8

    let dirRoot = "benchmark-tmp/dir-structure"
    createDirStucture dirRoot

    defaultMain
        [ bgroup (o_1_space_prefix moduleName)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
           $ (bench "listDirByteChunked" $ nfIO $
               Stream.fold Fold.drain $ listDirByteChunked dirRoot) :
#endif
            [ BENCH(listDirUnfoldDfs)
              -- NOTE: The BFS traversal fails with:
              -- openDirStream: resource exhausted (Too many open files)
            , BENCH(listDirUnfoldDfs)
            -- , BENCH(listDirUnfoldBfs)
            -- , BENCH(listDirUnfoldBfsRev)
            , BENCH(listDirConcatDfs)
            -- , BENCH(listDirConcatBfs)
            -- , BENCH(listDirConcatBfsRev)
            , BENCH(listDirAppend)
            , BENCH(listDirInterleave)
            , BENCH(listDirPar)
            , BENCH(listDirParInterleaved)
            , BENCH(listDirParOrdered)
            , BENCH(listDirChunkDfs)
            -- , BENCH(listDirChunkBfs)
            -- , BENCH(listDirChunkBfsRev)
            , BENCH(listDirChunkAppend)
            , BENCH(listDirChunkInterleave)
            , BENCH(listDirChunkPar)
            , BENCH(listDirChunkParInterleaved)
            , BENCH(listDirChunkParOrdered)
            ]
        ]
