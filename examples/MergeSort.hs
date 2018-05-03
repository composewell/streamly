{-# LANGUAGE FlexibleContexts    #-}

module Streamly.Examples.MergeSortedStreams where

import Data.Word
import System.Random (getStdGen, randoms)
import Data.List (sort)
import Streamly
import Streamly.Prelude (once)
import qualified Streamly.Prelude as A

getSorted :: Stream Word16
getSorted = do
    g <- once getStdGen
    let ls = take 100000 (randoms g) :: [Word16]
    foldMap return (sort ls)

-- | merge two streams generating the elements from each in parallel
mergeAsync :: Ord a => Stream a -> Stream a -> Stream a
mergeAsync a b = do
    x <- once $ async a
    y <- once $ async b
    merge x y

merge :: Ord a => Stream a -> Stream a -> Stream a
merge a b = do
    a1 <- once $ A.uncons a
    case a1 of
        Nothing -> b
        Just (x, ma) -> do
            b1 <- once $ A.uncons b
            case b1 of
                Nothing -> return x <> ma
                Just (y, mb) ->
                    if (y < x)
                        then (return y) <> merge (return x <> ma) mb
                        else (return x) <> merge ma (return y <> mb)

mergeSortedStreams :: IO ()
mergeSortedStreams = do
    xs <- A.toList $ mergeAsync getSorted getSorted
    putStrLn $ show $ length xs
