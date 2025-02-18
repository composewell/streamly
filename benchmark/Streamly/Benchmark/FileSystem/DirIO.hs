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
            [ bench "listDirUnfoldDfs" $ nfIO $
                  Stream.fold Fold.drain $ listDirUnfoldDfs bigTree
            , bench "listDirUnfoldBfs" $ nfIO $
                  Stream.fold Fold.drain $ listDirUnfoldBfs smallTree
            , bench "listDirUnfoldBfsRev" $ nfIO $
                  Stream.fold Fold.drain $ listDirUnfoldBfsRev smallTree
            , bench "listDirConcatDfs" $ nfIO $
                  Stream.fold Fold.drain $ listDirConcatDfs bigTree
            , bench "listDirConcatBfs" $ nfIO $
                  Stream.fold Fold.drain $ listDirConcatBfs smallTree
            , bench "listDirConcatBfsRev" $ nfIO $
                  Stream.fold Fold.drain $ listDirConcatBfsRev smallTree
            , bench "listDirAppend" $ nfIO $
                  Stream.fold Fold.drain $ listDirAppend bigTree
            , bench "listDirInterleave" $ nfIO $
                  Stream.fold Fold.drain $ listDirInterleave bigTree
            , bench "listDirPar" $ nfIO $
                  Stream.fold Fold.drain $ listDirPar bigTree
            , bench "listDirParInterleaved" $ nfIO $
                  Stream.fold Fold.drain $ listDirParInterleaved bigTree
            , bench "listDirParOrdered" $ nfIO $
                  Stream.fold Fold.drain $ listDirParOrdered bigTree
            , bench "listDirChunkDfs" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkDfs bigTree
            , bench "listDirChunkBfs" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkBfs smallTree
            , bench "listDirChunkBfsRev" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkBfsRev smallTree
            , bench "listDirChunkAppend" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkAppend bigTree
            , bench "listDirChunkInterleave" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkInterleave bigTree
            , bench "listDirChunkPar" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkPar bigTree
            , bench "listDirChunkParInterleaved" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkParInterleaved bigTree
            , bench "listDirChunkParOrdered" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkParOrdered bigTree
            ]
        ]
