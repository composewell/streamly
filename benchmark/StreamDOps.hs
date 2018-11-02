-- |
-- Module      : StreamDOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StreamDOps where

import Control.Monad (when)
import Data.Maybe (isJust)
import Prelude
        (Monad, Int, (+), ($), (.), return, (>), even, (<=), div,
         subtract, undefined, Maybe(..), not, mapM_, (>>=),
         maxBound, fmap, odd, (==))
import qualified Prelude as P

import qualified Streamly.Streams.StreamD as S

value, maxValue :: Int
value = 100000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.Stream m a

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Monad m => Int -> Stream m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrMN m n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + m
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Stream m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceFromEnum #-}
sourceFromEnum :: Monad m => Int -> Stream m Int
sourceFromEnum n = S.enumFromStepN n 1 value

{-# INLINE sourceFromList #-}
sourceFromList :: Monad m => Int -> Stream m Int
sourceFromList n = S.fromList [n..n+value]

{-# INLINE source #-}
source :: Monad m => Int -> Stream m Int
source = sourceUnfoldrM

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

{-# INLINE toNull #-}
toNull :: Monad m => Stream m Int -> m ()
toNull = runStream

{-# INLINE uncons #-}
{-# INLINE nullTail #-}
{-# INLINE headTail #-}
uncons, nullTail, headTail
    :: Monad m
    => Stream m Int -> m ()

uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> m ()
tail s = S.tail s >>= mapM_ tail

nullTail s = do
    r <- S.null s
    when (not r) $ S.tail s >>= mapM_ nullTail

headTail s = do
    h <- S.head s
    when (isJust h) $ S.tail s >>= mapM_ headTail

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
toList = S.toList

{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
foldl  = S.foldl' (+) 0

{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN
    :: Monad m
    => Int -> (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
composeN n f =
    case n of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined

{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE fmap #-}
{-# INLINE mapM #-}
{-# INLINE mapMaybe #-}
{-# INLINE mapMaybeM #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE takeWhileMTrue #-}
{-# INLINE dropOne #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileMTrue #-}
{-# INLINE dropWhileFalse #-}
scan, map, fmap, mapM, mapMaybe, mapMaybeM, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, takeWhileMTrue, dropOne,
    dropAll, dropWhileTrue, dropWhileMTrue, dropWhileFalse
    :: Monad m
    => Int -> Stream m Int -> m ()

scan          n = composeN n $ S.scanl' (+) 0
fmap          n = composeN n $ Prelude.fmap (+1)
map           n = composeN n $ S.map (+1)
mapM          n = composeN n $ S.mapM return
mapMaybe      n = composeN n $ S.mapMaybe
    (\x -> if Prelude.odd x then Nothing else Just x)
mapMaybeM     n = composeN n $ S.mapMaybeM
    (\x -> if Prelude.odd x then return Nothing else return $ Just x)
filterEven    n = composeN n $ S.filter even
filterAllOut  n = composeN n $ S.filter (> maxValue)
filterAllIn   n = composeN n $ S.filter (<= maxValue)
takeOne       n = composeN n $ S.take 1
takeAll       n = composeN n $ S.take maxValue
takeWhileTrue n = composeN n $ S.takeWhile (<= maxValue)
takeWhileMTrue n = composeN n $ S.takeWhileM (return . (<= maxValue))
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxValue)
dropWhileMTrue n = composeN n $ S.dropWhileM (return . (<= maxValue))
dropWhileFalse n = composeN n $ S.dropWhile (> maxValue)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

iterStreamLen, maxIters :: Int
iterStreamLen = 10
maxIters = 10000

{-# INLINE iterateSource #-}
iterateSource
    :: Monad m
    => (Stream m Int -> Stream m Int) -> Int -> Int -> Stream m Int
iterateSource g i n = f i (sourceUnfoldrMN iterStreamLen n)
    where
        f (0 :: Int) m = g m
        f x m = g (f (x P.- 1) m)

{-# INLINE iterateMapM #-}
{-# INLINE iterateScan #-}
{-# INLINE iterateFilterEven #-}
{-# INLINE iterateTakeAll #-}
{-# INLINE iterateDropOne #-}
{-# INLINE iterateDropWhileFalse #-}
{-# INLINE iterateDropWhileTrue #-}
iterateMapM, iterateScan, iterateFilterEven, iterateTakeAll, iterateDropOne,
    iterateDropWhileFalse, iterateDropWhileTrue
    :: Monad m
    => Int -> Stream m Int

-- this is quadratic
iterateScan            = iterateSource (S.scanl' (+) 0) (maxIters `div` 10)
iterateDropWhileFalse  = iterateSource (S.dropWhile (> maxValue))
                                       (maxIters `div` 10)

iterateMapM            = iterateSource (S.mapM return) maxIters
iterateFilterEven      = iterateSource (S.filter even) maxIters
iterateTakeAll         = iterateSource (S.take maxValue) maxIters
iterateDropOne         = iterateSource (S.drop 1) maxIters
iterateDropWhileTrue   = iterateSource (S.dropWhile (<= maxValue)) maxIters

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE eqBy #-}
eqBy :: (Monad m, P.Eq a) => S.Stream m a -> m P.Bool
eqBy src = S.eqBy (==) src src

{-# INLINE cmpBy #-}
cmpBy :: (Monad m, P.Ord a) => S.Stream m a -> m P.Ordering
cmpBy src = S.cmpBy P.compare src src

{-# INLINE zip #-}
zip :: Monad m => Stream m Int -> m ()
zip src = transform $ S.zipWith (,) src src

{-
{-# INLINE concat #-}
concat _n     = return ()
-}

-------------------------------------------------------------------------------
-- Mixed Composition
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
{-# INLINE dropMap #-}
{-# INLINE dropScan #-}
{-# INLINE takeDrop #-}
{-# INLINE takeScan #-}
{-# INLINE takeMap #-}
{-# INLINE filterDrop #-}
{-# INLINE filterTake #-}
{-# INLINE filterScan #-}
{-# INLINE filterMap #-}
scanMap, dropMap, dropScan, takeDrop, takeScan, takeMap, filterDrop,
    filterTake, filterScan, filterMap
    :: Monad m => Int -> Stream m Int -> m ()

scanMap    n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0
dropMap    n = composeN n $ S.map (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scanl' (+) 0 . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take maxValue
takeScan   n = composeN n $ S.scanl' (+) 0 . S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) . S.take maxValue
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxValue)
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) . S.filter (<= maxValue)
