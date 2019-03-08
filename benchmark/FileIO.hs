-- |
-- Module      : Main
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}

import qualified Streamly.Prelude as S
import qualified Streamly.FileIO as IO
import qualified Streamly.Array as A
import qualified Streamly.Foldl as FL

import Control.DeepSeq (NFData)
import Gauge
import System.Process.Typed (shell, runProcess_)
import System.IO (openFile, IOMode(..), Handle, hClose)
#ifdef DEVBUILD
import System.IO (hSeek, SeekMode(..))
#endif
import Data.IORef

data Handles = Handles Handle Handle

scratchDir :: String
scratchDir = "benchmark/scratch/"

infile :: String
infile = scratchDir ++ "in-100MB.txt"

outfile :: String
outfile = scratchDir ++ "out.txt"

blockSize, blockCount, fileSize :: Int
blockSize = 32768
blockCount = 3200
fileSize = blockSize * blockCount

main :: IO ()
main = do
    let cmd = "mkdir -p " ++ scratchDir
                ++ "; test -e " ++ infile
                ++ " || { echo \"creating input file " ++ infile
                ++ "\" && dd if=/dev/random of=" ++ infile
                ++ " bs=" ++ show blockSize
                ++ " count=" ++ show blockCount
                ++ ";}"

    -- XXX this will work only on Unix systems
    runProcess_ (shell cmd)
    inHandle <- openFile infile ReadMode
    outHandle <- openFile outfile WriteMode
    href <- newIORef $ Handles inHandle outHandle

-- This is a 500MB file for text processing benchmarks.  We cannot have it in
-- the repo.
#ifdef DEVBUILD
    inText <- openFile "benchmark/text-processing/gutenberg-500.txt" ReadMode
#endif

    defaultMain
        [ bgroup "readArray"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                let s = A.readHandleChunksOf IO.defaultChunkSize inh
                lc <- S.last s
                return $ case lc of
                    Nothing -> Nothing
                    Just c -> A.last c
            , mkBench "length" href $ do
                Handles inh _ <- readIORef href
                let s = A.readHandleChunksOf IO.defaultChunkSize inh
                S.sum (S.map A.length s)
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                let s = A.readHandleChunksOf IO.defaultChunkSize inh
                S.foldl' (\acc arr -> acc + A.foldl' (+) 0 arr) 0 s
            ]
        , bgroup "readStream"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                S.last $ IO.fromHandle inh
            , mkBench "length" href $ do
                Handles inh _ <- readIORef href
                S.length $ IO.fromHandle inh
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                S.sum $ IO.fromHandle inh
            , mkBench "arrayGroupsOf-single" href $ do
                Handles inh _ <- readIORef href
                S.length $ S.arrayGroupsOf fileSize (IO.fromHandle inh)
            , mkBench "foldGroupsOf-single" href $ do
                Handles inh _ <- readIORef href
                S.length $ S.foldGroupsOf (FL.toArrayN fileSize)
                                fileSize (IO.fromHandle inh)
            ]
        , bgroup "copyArray"
            [ mkBench "copy" href $ do
                Handles inh outh <- readIORef href
                let s = A.readHandleChunksOf IO.defaultChunkSize inh
                A.concatToHandle outh s
            ]
        , bgroup "copyStream"
            -- XXX copies only 32k
            [ mkBench "fromToHandle" href $ do
                Handles inh outh <- readIORef href
                IO.toHandle outh (IO.fromHandle inh)
            ]
        -- Note: this cannot be fairly compared with GNU wc -c or wc -m as it
        -- wc uses lseek to just determine the file size rather than reading
        -- and counting characters.
#ifdef DEVBUILD
        , bgroup "wordCount"
            [ mkBenchText "chunked byte count" inText $ do
                let s = A.readHandleChunksOf IO.defaultChunkSize inText
                S.sum (S.map A.length s) >>= print
            , mkBenchText "streamed byte count" inText $ do
                S.length $ IO.fromHandle inText
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

#ifdef DEVBUILD
    mkBenchText name h action =
        bench name $ perRunEnv (hSeek h AbsoluteSeek 0) (\_ -> action)
#endif
