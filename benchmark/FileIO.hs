-- |
-- Module      : Main
-- Copyright   : (c) 2019 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}

import Control.DeepSeq (NFData)
import Data.Functor.Identity (runIdentity)
import System.IO (openFile, IOMode(..), Handle, hClose)
import System.Process.Typed (shell, runProcess_)

import Data.IORef
import Gauge

import qualified Streamly.FileSystem.File as File
import qualified Streamly.Mem.Array as A
import qualified Streamly.Prelude as S

#ifdef DEVBUILD
import Data.Char (ord, chr)
import System.IO (hSeek, SeekMode(..))
import qualified Streamly.Fold as FL
#endif

-- Input and output file handles
data Handles = Handles Handle Handle

scratchDir :: String
scratchDir = "benchmark/scratch/"

infile :: String
infile = scratchDir ++ "in-100MB.txt"

outfile :: String
outfile = scratchDir ++ "out.txt"

blockSize, blockCount :: Int
blockSize = 32768
blockCount = 3200

#ifdef DEVBUILD
fileSize :: Int
fileSize = blockSize * blockCount

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

-- Code copied from base/Data.Char to INLINE it
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
#endif

main :: IO ()
main = do
    -- XXX will this work on windows/msys?
    let cmd = "mkdir -p " ++ scratchDir
                ++ "; test -e " ++ infile
                ++ " || { echo \"creating input file " ++ infile
                ++ "\" && dd if=/dev/random of=" ++ infile
                ++ " bs=" ++ show blockSize
                ++ " count=" ++ show blockCount
                ++ ";}"

    runProcess_ (shell cmd)
    inHandle <- openFile infile ReadMode
    outHandle <- openFile outfile WriteMode
    href <- newIORef $ Handles inHandle outHandle

-- This is a 500MB text file for text processing benchmarks.  We cannot have it
-- in the repo, therefore we use it locally with DEVBUILD conditional (enabled
-- by "dev" cabal flag).
#ifdef DEVBUILD
    inText <- openFile "benchmark/text-processing/gutenberg-500.txt" ReadMode
#endif

    defaultMain
        [ bgroup "readArray"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                let s = File.readArrays inh
                larr <- S.last s
                return $ case larr of
                    Nothing -> Nothing
                    Just arr -> A.readIndex arr (A.length arr - 1)
            -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
            -- wc uses lseek to just determine the file size rather than reading
            -- and counting characters.
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                let s = File.readArrays inh
                S.sum (S.map A.length s)
            , mkBench "sum" href $ do
                let foldlArr' f z = runIdentity . S.foldl' f z . A.read
                Handles inh _ <- readIORef href
                let s = File.readArrays inh
                S.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s
            ]
        , bgroup "readStream"
            [ mkBench "last" href $ do
                Handles inh _ <- readIORef href
                S.last $ File.read inh
            , mkBench "length (bytecount)" href $ do
                Handles inh _ <- readIORef href
                S.length $ File.read inh
            , mkBench "sum" href $ do
                Handles inh _ <- readIORef href
                S.sum $ File.read inh
            ]
        , bgroup "copyArray"
            [ mkBench "copy" href $ do
                Handles inh outh <- readIORef href
                let s = File.readArrays inh
                File.writeArrays outh s
            ]
#ifdef DEVBUILD
        -- This takes a little longer therefore put under the dev conditional
        , bgroup "copyStream"
            [ mkBench "fromToHandle" href $ do
                Handles inh outh <- readIORef href
                File.write outh (File.read inh)
            ]
        , bgroup "grouping"
            [ mkBench "groupsOf 1 (toArray)" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf fileSize (A.toArrayN fileSize)
                                (File.read inh)

            , mkBench "groupsOf 1" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf 1 FL.drain (File.read inh)
            , mkBench "groupsOf 10" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf 10 FL.drain (File.read inh)
            , mkBench "groupsOf 1000" href $ do
                Handles inh _ <- readIORef href
                S.length $ FL.groupsOf 1000 FL.drain (File.read inh)
            ]

        , let lf = fromIntegral (ord '\n')
              lfarr = A.fromList [lf]
              isSp = isSpace . chr . fromIntegral
              toarr = A.fromList . map (fromIntegral . ord)
          in bgroup "splitting"
            [ bgroup "predicate"
                [ mkBenchText "splitBy \\n (line count)" inText $ do
                    (S.length $ FL.splitBy (== lf) FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitSuffixBy \\n (line count)" inText $ do
                    (S.length $ FL.splitSuffixBy (== lf) FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "wordsBy isSpace (word count)" inText $ do
                    (S.length $ FL.wordsBy isSp FL.drain
                        $ File.read inText) >>= print
                ]

            , bgroup "empty-pattern"
                [ mkBenchText "splitOn \"\"" inText $ do
                    (S.length $ FL.splitOn (A.fromList []) FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitSuffixOn \"\"" inText $ do
                    (S.length $ FL.splitSuffixOn (A.fromList []) FL.drain
                        $ File.read inText) >>= print
                ]
            , bgroup "short-pattern"
                [ mkBenchText "splitOn \\n (line count)" inText $ do
                    (S.length $ FL.splitOn lfarr FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitSuffixOn \\n (line count)" inText $ do
                    (S.length $ FL.splitSuffixOn lfarr FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitOn a" inText $ do
                    (S.length $ FL.splitOn (toarr "a") FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitOn \\r\\n" inText $ do
                    (S.length $ FL.splitOn (toarr "\r\n") FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitSuffixOn \\r\\n)" inText $ do
                    (S.length $ FL.splitSuffixOn (toarr "\r\n") FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitOn aa" inText $ do
                    (S.length $ FL.splitOn (toarr "aa") FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitOn aaaa" inText $ do
                    (S.length $ FL.splitOn (toarr "aaaa") FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitOn abcdefgh" inText $ do
                    (S.length $ FL.splitOn (toarr "abcdefgh") FL.drain
                        $ File.read inText) >>= print
                ]
            , bgroup "long-pattern"
                [ mkBenchText "splitOn abcdefghi" inText $ do
                    (S.length $ FL.splitOn (toarr "abcdefghi") FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitOn catcatcatcatcat" inText $ do
                    (S.length $ FL.splitOn (toarr "catcatcatcatcat") FL.drain
                        $ File.read inText) >>= print
                , mkBenchText "splitOn abc...xyz" inText $ do
                    (S.length $ FL.splitOn
                                    (toarr "abcdefghijklmnopqrstuvwxyz")
                                    FL.drain
                            $ File.read inText) >>= print
                , mkBenchText "splitSuffixOn abc...xyz" inText $ do
                    (S.length $ FL.splitSuffixOn
                                    (toarr "abcdefghijklmnopqrstuvwxyz")
                                    FL.drain
                            $ File.read inText) >>= print
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

#ifdef DEVBUILD
    mkBenchText name h action =
        bench name $ perRunEnv (hSeek h AbsoluteSeek 0) (\_ -> action)
#endif
