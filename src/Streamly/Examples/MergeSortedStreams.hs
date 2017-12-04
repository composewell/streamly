{-# LANGUAGE FlexibleContexts    #-}

module Streamly.Examples.MergeSortedStreams where

import Data.Word
import System.Random (getStdGen, randoms)
import Data.List (sort)
import Streamly
import qualified Streamly.Prelude as A

getSorted :: MonadIO m => StreamT m Word16
getSorted = do
    g <- liftIO getStdGen
    let ls = take 100000 (randoms g) :: [Word16]
    foldMapWith (<>) return (sort ls)

mergeAsync :: (Ord a, MonadAsync m)
    => StreamT m a -> StreamT m a -> StreamT m a
mergeAsync a b = do
    x <- lift $ async a
    y <- lift $ async b
    merge x y

merge :: (Ord a, MonadAsync m) => StreamT m a -> StreamT m a -> StreamT m a
merge a b = do
    a1 <- lift $ A.uncons a
    case a1 of
        Nothing -> b
        Just (x, ma) -> do
            b1 <- lift $ A.uncons b
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
