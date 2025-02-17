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

import Control.Monad (void)
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
    void $ createDirStucture smallTree 2 3
    void $ createDirStucture bigTree 5 5

    defaultMain
        [ bgroup (o_1_space_prefix moduleName)
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
           $ bench "listDirByteChunked (big)" (nfIO $
               Stream.fold Fold.drain $ listDirByteChunked bigTree) :
#endif
            -- NOTE: The BFS traversal fails with:
            -- openDirStream: resource exhausted (Too many open files)
            -- if a bigger directory tree is used
            [ bench "listDirUnfoldDfs (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirUnfoldDfs id bigTree
            , bench "listDirUnfoldBfs (small)" $ nfIO $
                  Stream.fold Fold.drain $ listDirUnfoldBfs id smallTree
            , bench "listDirUnfoldBfsRev (small)" $ nfIO $
                  Stream.fold Fold.drain $ listDirUnfoldBfsRev id smallTree
            , bench "listDirConcatDfs (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirConcatDfs id bigTree
            , bench "listDirConcatBfs (small)" $ nfIO $
                  Stream.fold Fold.drain $ listDirConcatBfs id smallTree
            , bench "listDirConcatBfsRev (small)" $ nfIO $
                  Stream.fold Fold.drain $ listDirConcatBfsRev id smallTree
            , bench "listDirAppend (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirAppend id bigTree
            , bench "listDirInterleave (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirInterleave id bigTree
            , bench "listDirPar (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirPar id bigTree
            , bench "listDirParInterleaved (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirParInterleaved id bigTree
            , bench "listDirParOrdered (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirParOrdered id bigTree
            , bench "listDirChunkDfs (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkDfs id bigTree
            , bench "listDirChunkBfs (small)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkBfs id smallTree
            , bench "listDirChunkBfsRev (small)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkBfsRev id smallTree
            , bench "listDirChunkAppend (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkAppend id bigTree
            , bench "listDirChunkInterleave (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkInterleave id bigTree
            , bench "listDirChunkPar (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkPar id bigTree
            , bench "listDirChunkParInterleaved (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkParInterleaved id bigTree
            , bench "listDirChunkParOrdered (big)" $ nfIO $
                  Stream.fold Fold.drain $ listDirChunkParOrdered id bigTree
            ]
        ]
