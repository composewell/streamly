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

#define IT(x) \
it " x " $ \
    testCorrectness strmBase (x dirRoot)

-- | List the current directory recursively
main :: IO ()
main = do
    setLocaleEncoding utf8
    let dirRoot = "benchmark-tmp/dir-structure"
    createDirStucture dirRoot
    findRes <- readCreateProcess ((shell [str|find #{dirRoot}|])) ""
    strmBaseCache <-
        Stream.fold Fold.toList
            $ StreamK.toStream
            $ StreamK.sortBy compare
            $ StreamK.fromStream
            $ Unicode.lines Fold.toList $ Stream.fromList findRes
    let strmBase = Stream.fromList strmBaseCache
    hspec $
        describe moduleName $ do
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
            it "listDirByteChunked" $
                testCorrectnessByteChunked
                    (Stream.drop 1 strmBase) (listDirByteChunked dirRoot)
#endif
            -- NOTE: The BFS traversal fails with:
            -- openDirStream: resource exhausted (Too many open files)
            IT(listDirUnfoldDfs)
            -- IT(listDirUnfoldBfs)
            -- IT(listDirUnfoldBfsRev)
            IT(listDirConcatDfs)
            -- IT(listDirConcatBfs)
            -- IT(listDirConcatBfsRev)
            IT(listDirAppend)
            IT(listDirInterleave)
            IT(listDirPar)
            IT(listDirParInterleaved)
            IT(listDirParOrdered)
            IT(listDirChunkDfs)
            -- IT(listDirChunkBfs)
            -- IT(listDirChunkBfsRev)
            IT(listDirChunkAppend)
            IT(listDirChunkInterleave)
            IT(listDirChunkPar)
            IT(listDirChunkParInterleaved)
            IT(listDirChunkParOrdered)
