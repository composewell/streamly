import qualified Control.Category as Cat
import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
import qualified Streamly.Pipe as Pipe
import qualified Streamly.Mem.Array as A
import qualified Streamly.FileSystem.Handle as FH
import Streamly
import GHC.Magic (noinline)
import Control.Monad.IO.Class

import Data.Word (Word8)
import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), stdout, Handle, hSeek, SeekMode(..))

cat :: Handle -> IO ()
cat src = FH.writeArrays stdout $ FH.readArraysUpto (256*1024) src

cp :: Handle -> Handle -> IO ()
cp src dst = FH.writeArrays dst $ FH.readArraysUpto (256*1024) src

ord' :: Num a => Char -> a
ord' = (fromIntegral . ord)

wcl :: Handle -> IO ()
wcl src = print =<< (S.length
    $ FL.splitSuffixBy (== ord' '\n') FL.drain
    $ FH.read src)

wcl1 :: Handle -> IO ()
wcl1 src = print =<<
    -- (S.drain $ S.transform (Pipe.splitBy (== ord' '\n') (Pipe.extend (S.yield 13 :: SerialT IO Word8) Cat.id) Cat.id) $ FH.read src)
    -- (S.length $ S.transform (Pipe.splitBy (== ord' '\n') Cat.id Cat.id) $ FH.read src)
    -- (S.length $ S.transform (Pipe.splitBy (== ord' '\n') (Pipe.map (+1)) Cat.id) $ FH.read src)
    -- (S.length $ S.transform (Pipe.splitBy (== ord' '\n') Pipe.drain Cat.id) $ FH.read src)
    (S.length $ S.transform (Pipe.extend (S.yield (A.fromList [13]) :: SerialT IO (A.Array Word8)) Cat.id) $ FH.readArrays src)

ul :: Handle -> Handle -> IO ()
ul src dst =
    -- (FH.write dst $ S.transform (Pipe.splitBy (== ord' '\n') (Pipe.extend (S.yield 10 :: SerialT IO Word8) Cat.id) Cat.id) $ FH.read src)
    -- (FH.write dst $ S.transform (Pipe.splitBy (== ord' '\n') Cat.id Cat.id) $ FH.read src)
    -- (FH.write dst $ FH.read src)
    (FH.writeArrays) dst
--        S.drain
         -- $ S.concatMap id
         -- $ S.concatMap (\x -> x <> S.yield (A.fromList $ map (fromIntegral .  ord) "\n"))
         -- $ S.transform (Pipe.extend (S.yield (S.yield (A.fromList $ map (fromIntegral . ord) "\n")) :: SerialT m1 (A.Array Word8)) Cat.id)
            -- $ S.mapM (\x -> A.spliceArrays (x <> S.yield (A.fromList $ map (fromIntegral .  ord) "\n")))
            $ S.mapM A.spliceArrays
            $ S.intersperseM (return (S.yield (A.fromList $ map (fromIntegral .  ord) "\n")))
            $ FL.groupsRollingBy f FL.toStream
            $ A.lines
            $ FH.readArrays src

            -- XXX write a foldLines for array chunks
            -- XXX write an lintersperse for folds for unlines
            -- XXX write a writeArrays fold to write a stream of arrays to file
            -- XXX Coalesce stream of arrays into bigger arrays. we can do this
            -- without reallocating more than once. If the last chunks gets us
            -- beyond 32K we can skip it and start next chunk.
    where

        f arr1 _ =
            case A.last arr1 of
                Nothing -> True
                Just x -> x /= fromIntegral (ord '\n')
{-
    ( FL.foldl' ( FL.transform
              --    (Pipe.splitBy (== ord' '\n') Pipe.drain Cat.id)
                  Pipe.drain
                  FL.length)
    $ FH.read src
    )
    -}

grepc :: String -> Handle -> IO ()
grepc pat src = print . (subtract 1) =<< (S.length
    $ FL.splitOn (A.fromList (map ord' pat)) FL.drain
    $ FH.read src)

avgll :: Handle -> IO ()
avgll src = print =<< (FL.foldl' avg
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ FH.read src)
    where avg = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

llhisto :: Handle -> IO ()
llhisto src = print =<< (FL.foldl' (FL.classify FL.length)
    $ S.map bucket
    $ FL.splitSuffixBy (== ord' '\n') FL.length
    $ FH.read src)
    where
    bucket n = let i = n `div` 10 in if i > 9 then (9,n) else (i,n)

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- openFile name ReadMode
    let rewind = hSeek src AbsoluteSeek 0

{-
    rewind >> putStrLn "cat"    >> cat src          -- Unix cat program
    rewind >> putStr "wcl "     >> wcl src          -- Unix wc -l program
    rewind >> putStr "grepc "   >> grepc "aaaa" src -- Unix grep -c program
    rewind >> putStr "avgll "   >> avgll src        -- get average line length
    rewind >> putStr "llhisto " >> llhisto src      -- get line length histogram

    dst <- openFile "dst-xyz.txt" WriteMode
    rewind >> putStr "cp " >> cp src dst       -- Unix cp program
    -}
    -- rewind >> putStr "wcl1 "     >> wcl1 src          -- Unix wc -l program
    rewind >> putStr "ul" >> ul src stdout        -- Unlines . lines
