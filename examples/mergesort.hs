#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE FlexibleContexts    #-}

import Data.Word
import System.Random
import Data.List (sort)
import Asyncly
import qualified Asyncly.Prelude as A

getSorted = do
    g <- liftIO getStdGen
    let ls = take 100000 (randoms g) :: [Word16]
    foldMapWith (<>) return (sort ls)

mergeAsync :: (Ord a, MonadAsync m)
    => AsyncT m a -> AsyncT m a -> AsyncT m a
mergeAsync a b = do
    x <- lift $ async a
    y <- lift $ async b
    merge x y

merge a b = do
    x <- lift $ A.uncons a
    case x of
        Nothing -> b
        Just (va, ma) -> do
            y <- lift $ A.uncons b
            case y of
                Nothing -> return va <> ma
                Just (vb, mb) ->
                    if (vb < va)
                        then (return vb) <> merge (return va <> ma) mb
                        else (return va) <> merge ma (return vb <> mb)

main = do
    xs <- toList $ mergeAsync getSorted getSorted
    putStrLn $ show $ length xs
