-- |
-- Module      : Main
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}

import Control.DeepSeq (NFData)
import System.IO (openFile, IOMode(..), Handle, hClose)
import System.Process.Typed (shell, runProcess_)

import Data.IORef
import Gauge

import qualified Streamly.Benchmark.FileIO.Stream as BFS
import qualified Streamly.Benchmark.FileIO.Array as BFA

-- Input and output file handles
data Handles = Handles Handle Handle

scratchDir :: String
scratchDir = "benchmark/scratch/"

outfile :: String
outfile = scratchDir ++ "out.txt"

blockSize, blockCount :: Int
blockSize = 32768
blockCount = 3200

#ifdef DEVBUILD
-- This is a 500MB text file for text processing benchmarks.  We cannot
-- have it in the repo, therefore we use it locally with DEVBUILD
-- conditional (enabled by "dev" cabal flag). Some tests that depend on
-- this file are available only in DEVBUILD mode.
infile :: String
infile = "benchmark/text-processing/gutenberg-500.txt"

fileSize :: Int
fileSize = blockSize * blockCount

#else
infile :: String
infile = scratchDir ++ "in-100MB.txt"
#endif

main :: IO ()
main = do
#ifndef DEVBUILD
    -- XXX will this work on windows/msys?
    let cmd = "mkdir -p " ++ scratchDir
                ++ "; test -e " ++ infile
                ++ " || { echo \"creating input file " ++ infile
                ++ "\" && dd if=/dev/random of=" ++ infile
                ++ " bs=" ++ show blockSize
                ++ " count=" ++ show blockCount
                ++ ";}"

    runProcess_ (shell cmd)
#endif
    inHandle <- openFile infile ReadMode
    outHandle <- openFile outfile WriteMode
    href <- newIORef $ Handles inHandle outHandle
    devNull <- openFile "/dev/null" WriteMode

    defaultMain
        [ bgroup "readArray"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                BFA.last inh
            -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
            -- wc uses lseek to just determine the file size rather than reading
            -- and counting characters.
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                BFA.countBytes inh
            , mkBench "linecount" href $ do
                Handles inh _ <- readIORef href
                BFA.countLines inh
            , mkBench "wordcount" href $ do
                Handles inh _ <- readIORef href
                BFA.countWords inh
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                BFA.sumBytes inh
            , mkBench "cat" href $ do
                Handles inh _ <- readIORef href
                BFA.cat devNull inh
           , mkBench "catBracket" href $ do
               Handles inh _ <- readIORef href
               BFA.catBracket devNull inh
           , mkBench "catBracketStream" href $ do
               Handles inh _ <- readIORef href
               BFA.catBracketStream devNull inh
           , mkBench "catOnException" href $ do
               Handles inh _ <- readIORef href
               BFA.catOnException devNull inh
           , mkBench "read-utf8" href $ do
               Handles inh _ <- readIORef href
               BFA.decodeUtf8Lenient inh
            ]
        , bgroup "readStream"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                BFS.last inh
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                BFS.countBytes inh
            , mkBench "linecount" href $ do
                Handles inh _ <- readIORef href
                BFS.countLines inh
            , mkBench "linecountU" href $ do
                Handles inh _ <- readIORef href
                BFS.countLinesU inh
            , mkBench "wordcount" href $ do
                Handles inh _ <- readIORef href
                BFS.countWords inh
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                BFS.sumBytes inh
            , mkBench "cat" href $ do
                Handles inh _ <- readIORef href
                BFS.cat devNull inh
            , mkBench "catStream" href $ do
                Handles inh _ <- readIORef href
                BFS.catStreamWrite devNull inh
#ifdef DEVBUILD
           , mkBench "catOnException" href $ do
               Handles inh _ <- readIORef href
               BFS.catOnException devNull inh
           , mkBench "catHandle" href $ do
               Handles inh _ <- readIORef href
               BFS.catHandle devNull inh
           , mkBench "catFinally" href $ do
               Handles inh _ <- readIORef href
               BFS.catFinally devNull inh
           , mkBench "catBracketStream" href $ do
               Handles inh _ <- readIORef href
               BFS.catBracketStream devNull inh
           , mkBench "catBracket" href $ do
               Handles inh _ <- readIORef href
               BFS.catBracket devNull inh
#endif
           , mkBench "read-word8" href $ do
               Handles inh _ <- readIORef href
               BFS.readWord8 inh
           , mkBench "read-char8" href $ do
               Handles inh _ <- readIORef href
               BFS.decodeChar8 inh
           , mkBench "read-utf8" href $ do
               Handles inh _ <- readIORef href
               BFS.decodeUtf8Lenient inh
            ]
        , bgroup "copyArray"
            [ mkBench "copy" href $ do
                Handles inh outh <- readIORef href
                BFA.copy inh outh
            ]
#ifdef DEVBUILD
        -- This takes a little longer therefore put under the dev conditional
        , bgroup "copyStream"
            [ mkBench "fromToHandle" href $ do
                Handles inh outh <- readIORef href
                BFS.copy inh outh
            ]
        -- This needs an ascii file, as decode just errors out.
        , bgroup "decode-encode"
           [ mkBench "char8" href $ do
               Handles inh outh <- readIORef href
               BFS.copyCodecChar8 inh outh
           , mkBench "utf8-arrays" href $ do
               Handles inh outh <- readIORef href
               BFA.copyCodecUtf8Lenient inh outh
           , mkBench "utf8" href $ do
               Handles inh outh <- readIORef href
               BFS.copyCodecUtf8Lenient inh outh
           ]
        , bgroup "grouping"
            [ mkBench "chunksOf (single chunk)" href $ do
                Handles inh _ <- readIORef href
                BFS.chunksOf fileSize inh

            , mkBench "chunksOf 1" href $ do
                Handles inh _ <- readIORef href
                BFS.chunksOf 1 inh
            , mkBench "chunksOf 10" href $ do
                Handles inh _ <- readIORef href
                BFS.chunksOf 10 inh
            , mkBench "chunksOf 1000" href $ do
                Handles inh _ <- readIORef href
                BFS.chunksOf 1000 inh
            ]
        , bgroup "group-ungroup"
            [ mkBench "lines-unlines" href $ do
                Handles inh outh <- readIORef href
                BFS.linesUnlinesCopy inh outh
            , mkBench "lines-unlines-arrays" href $ do
                Handles inh outh <- readIORef href
                BFA.linesUnlinesCopy inh outh
            , mkBench "words-unwords" href $ do
                Handles inh outh <- readIORef href
                BFS.wordsUnwordsCopy inh outh
            , mkBench "words-unwords-word8" href $ do
                Handles inh outh <- readIORef href
                BFS.wordsUnwordsCopyWord8 inh outh
            , mkBench "words-unwords-arrays" href $ do
                Handles inh outh <- readIORef href
                BFA.wordsUnwordsCopy inh outh
            ]

        , bgroup "splitting"
            [ bgroup "predicate"
                [ mkBench "splitOn \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOn inh
                , mkBench "splitOnSuffix \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSuffix inh
                , mkBench "wordsBy isSpace (word count)" href $ do
                    Handles inh _ <- readIORef href
                    BFS.wordsBy inh
                ]

            , bgroup "empty-pattern"
                [ mkBench "splitOnSeq \"\"" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "" inh
                , mkBench "splitOnSuffixSeq \"\"" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSuffixSeq "" inh
                ]
            , bgroup "short-pattern"
                [ mkBench "splitOnSeq \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "\n" inh
                , mkBench "splitOnSuffixSeq \\n (line count)" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSuffixSeq "\n" inh
                , mkBench "splitOnSeq a" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "a" inh
                , mkBench "splitOnSeq \\r\\n" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "\r\n" inh
                , mkBench "splitOnSuffixSeq \\r\\n)" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSuffixSeq "\r\n" inh
                , mkBench "splitOnSeq aa" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "aa" inh
                , mkBench "splitOnSeq aaaa" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "aaaa" inh
                , mkBench "splitOnSeq abcdefgh" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "abcdefgh" inh
                , mkBench "splitOnSeqUtf8 abcdefgh" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeqUtf8 "abcdefgh" inh
                ]
            , bgroup "long-pattern"
                [ mkBench "splitOnSeq abcdefghi" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "abcdefghi" inh
                , mkBench "splitOnSeq catcatcatcatcat" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "catcatcatcatcat" inh
                , mkBench "splitOnSeq abc...xyz" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh
                , mkBench "splitOnSuffixSeq abc...xyz" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
                , mkBench "splitOnSeqUtf8 abc...xyz" href $ do
                    Handles inh _ <- readIORef href
                    BFS.splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh
                ]
            ]
#endif
        ]

    where

    mkBench :: NFData b => String -> IORef Handles -> IO b -> Benchmark
    mkBench name ref action =
        bench name $ perRunEnv (do
                (Handles inh outh) <- readIORef ref
                hClose inh
                hClose outh
                inHandle <- openFile infile ReadMode
                outHandle <- openFile outfile WriteMode
                writeIORef ref (Handles inHandle outHandle)
            )
            (\_ -> action)
