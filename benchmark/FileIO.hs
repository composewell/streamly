-- |
-- Module      : Main
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}

import Data.Char (ord, chr)
import qualified Streamly.Prelude as S
import qualified Streamly.FileIO as IO
import qualified Streamly.Array as A
import qualified Streamly.Fold as FL

import Control.DeepSeq (NFData)
import Gauge
import System.Process.Typed (shell, runProcess_)
import System.IO (openFile, IOMode(..), Handle, hClose)
#ifdef DEVBUILD
import System.IO (hSeek, SeekMode(..))
#endif
import Data.IORef

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

{-# INLINE isSpace #-}
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

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
-- the repo, therefore we use it only with DEVBUILD.
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
            {-
            , mkBench "arrayGroupsOf-single" href $ do
                Handles inh _ <- readIORef href
                S.length $ S.arrayGroupsOf fileSize (IO.fromHandle inh)
            -}
            , mkBench "groupsOf(one-group)" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf fileSize (FL.toArrayN fileSize)
                                (IO.fromHandle inh)
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
        -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
        -- wc uses lseek to just determine the file size rather than reading
        -- and counting characters.
#ifdef DEVBUILD
        , bgroup "wordCount"
            [ mkBenchText "chunked byte count" inText $ do
                let s = A.readHandleChunksOf IO.defaultChunkSize inText
                S.sum (S.map A.length s) >>= print
            , mkBenchText "streamed byte count" inText $ do
                S.length $ IO.fromHandle inText
            , mkBenchText "streamed line count (tokensWhen)" inText $ do
                (S.length $ FL.tokensWhen (== fromIntegral (ord '\n')) FL.drain
                    $ IO.fromHandle inText) >>= print
            , mkBenchText "streamed line count (splitOn)" inText $ do
                (S.length $ FL.splitOn (A.singleton (fromIntegral (ord '\n'))) FL.drain
                    $ IO.fromHandle inText) >>= print
            , mkBenchText "streamed word count" inText $ do
                (S.length $ FL.wordsWhen (\x -> isSpace $ chr (fromIntegral x)) FL.drain $
                    IO.fromHandle inText) >>= print
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
