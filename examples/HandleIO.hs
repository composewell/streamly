import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array as A
import qualified Streamly.FileSystem.Handle as FH
import qualified System.IO as FH
-- import qualified Streamly.FileSystem.FD as FH
import qualified Streamly.String as SS
import Data.Word (Word8)

import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (IOMode(..), hSeek, SeekMode(..))

cat :: FH.Handle -> IO ()
cat src = FH.writeArrays FH.stdout $ FH.readArraysOfUpto (256*1024) src
{-
-- byte stream version
cat src =
      FH.writeByChunksOf (1024*1024) FH.stdout
    $ FH.readByChunksUpto (16*1024) src
-}

cp :: FH.Handle -> FH.Handle -> IO ()
cp src dst = FH.writeArrays dst $ FH.readArraysOfUpto (256*1024) src
{-
-- byte stream version
cp src dst =
      FH.writeByChunksOf (1024*1024) dst
    $ FH.readByChunksUpto (16*1024) src
-}

ord' :: Num a => Char -> a
ord' = (fromIntegral . ord)

wcl :: FH.Handle -> IO ()
wcl src = print =<< (S.length
    $ A.splitArraysOn 10
    $ FH.readArrays src)
{-
-- Char stream version
wcl src = print =<< (S.length
    $ flip SS.foldLines FL.drain
    $ SS.decodeChar8
    $ FH.read src)
-}

grepc :: String -> FH.Handle -> IO ()
grepc pat src = print . (subtract 1) =<< (S.length
    $ FL.splitOn (A.fromList (map ord' pat)) FL.drain
    $ FH.read src)

avgll :: FH.Handle -> IO ()
avgll src = print =<< (FL.foldl' avg
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ FH.read src)
    where avg = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

llhisto :: FH.Handle -> IO ()
llhisto src = print =<< (FL.foldl' (FL.classify FL.length)
    $ S.map bucket
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ FH.read src)
    where
    bucket n = let i = n `div` 10 in if i > 9 then (9,n) else (i,n)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- FH.openFile name ReadMode
    let rewind = hSeek src AbsoluteSeek 0

    rewind >> putStrLn "cat"    >> cat src          -- Unix cat program
    rewind >> putStr "wcl "     >> wcl src          -- Unix wc -l program
    rewind >> putStr "grepc "   >> grepc "aaaa" src -- Unix grep -c program
    rewind >> putStr "avgll "   >> avgll src        -- get average line length
    rewind >> putStr "llhisto " >> llhisto src      -- get line length histogram

    dst <- FH.openFile "dst-xyz.txt" WriteMode
    rewind >> putStr "cp " >> cp src dst       -- Unix cp program
