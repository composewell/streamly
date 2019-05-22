import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array as A
import qualified Streamly.FileSystem.File as File

import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), stdout, Handle, hSeek, SeekMode(..))

cat :: Handle -> IO ()
cat src = File.writeArrays stdout $ File.readArraysUpto (256*1024) src

cp :: Handle -> Handle -> IO ()
cp src dst = File.writeArrays dst $ File.readArraysUpto (256*1024) src

ord' :: Num a => Char -> a
ord' = (fromIntegral . ord)

wcl :: Handle -> IO ()
wcl src = print =<< (S.length
    $ FL.splitSuffixBy (== ord' '\n') FL.drain
    $ File.read src)

grepc :: String -> Handle -> IO ()
grepc pat src = print . (subtract 1) =<< (S.length
    $ FL.splitOn (A.fromList (map ord' pat)) FL.drain
    $ File.read src)

avgll :: Handle -> IO ()
avgll src = print =<< (FL.foldl' avg
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ File.read src)
    where avg = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

llhisto :: Handle -> IO ()
llhisto src = print =<< (FL.foldl' (FL.classify FL.length)
    $ S.map bucket
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ File.read src)
    where
    bucket n = let i = n `div` 10 in if i > 9 then (9,n) else (i,n)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    let rewind = hSeek src AbsoluteSeek 0

    rewind >> putStrLn "cat"    >> cat src          -- Unix cat program
    rewind >> putStr "wcl "     >> wcl src          -- Unix wc -l program
    rewind >> putStr "grepc "   >> grepc "aaaa" src -- Unix grep -c program
    rewind >> putStr "avgll "   >> avgll src        -- get average line length
    rewind >> putStr "llhisto " >> llhisto src      -- get line length histogram

    dst <- openFile "dst-xyz.txt" WriteMode
    rewind >> putStr "cp " >> cp src dst       -- Unix cp program
