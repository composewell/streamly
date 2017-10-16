#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE FlexibleContexts    #-}

import Data.Word
import System.Random
import Data.List (sort)
import qualified Asyncly as A

getSorted = do
    g <- A.liftIO getStdGen
    let ls = take 100000 (randoms g) :: [Word16]
    A.foldMapWith (A.<>) return (sort ls)

mergeAsync :: (Ord a, A.MonadAsync m)
    => A.AsyncT m a -> A.AsyncT m a -> A.AsyncT m a
mergeAsync a b = do
    x <- A.lift $ A.async a
    y <- A.lift $ A.async b
    merge x y

merge a b = do
    x <- A.lift $ A.next a
    case x of
        Nothing -> b
        Just (va, ma) -> do
            y <- A.lift $ A.next b
            case y of
                Nothing -> return va A.<> ma
                Just (vb, mb) ->
                    if (vb < va)
                        then (return vb) A.<> merge (return va A.<> ma) mb
                        else (return va) A.<> merge ma (return vb A.<> mb)

main = do
    xs <- A.toList $ mergeAsync getSorted getSorted
    putStrLn $ show $ length xs
