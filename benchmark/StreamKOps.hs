-- |
-- Module      : StreamKOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StreamKOps where

import Control.Monad (when)
import Data.Maybe (isJust)
import Prelude
       (Monad, Int, (+), ($), (.), return, even, (>), (<=), div,
        subtract, undefined, Maybe(..), not, mapM_, (>>=),
        maxBound)
import qualified Prelude as P

import qualified Streamly.Streams.StreamK as S
import qualified Streamly.Streams.Prelude as SP
import qualified Streamly.SVar as S

value, maxValue :: Int
value = 100000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
{-# INLINE uncons #-}
{-# INLINE nullTail #-}
{-# INLINE headTail #-}
{-# INLINE zip #-}
{-# INLINE concat #-}
toNull, uncons, nullTail, headTail, zip, concat
    :: Monad m
    => Stream m Int -> m ()

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.Stream m a

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> Stream m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: S.MonadAsync m => Int -> Stream m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: S.MonadAsync m => Int -> Int -> Stream m Int
sourceUnfoldrMN m n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + m
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-
{-# INLINE sourceFromEnum #-}
sourceFromEnum :: Monad m => Int -> Stream m Int
sourceFromEnum n = S.enumFromStepN n 1 value
-}

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: Int -> Stream m Int
sourceFromFoldable n = S.fromFoldable [n..n+value]

{-
{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: S.MonadAsync m => Int -> Stream m Int
sourceFromFoldableM n = S.fromFoldableM (Prelude.fmap return [n..n+value])
-}

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: Int -> Stream m Int
sourceFoldMapWith n = SP.foldMapWith S.serial S.yield [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: Monad m => Int -> Stream m Int
sourceFoldMapWithM n = SP.foldMapWith S.serial (S.yieldM . return) [n..n+value]

{-# INLINE source #-}
source :: S.MonadAsync m => Int -> Stream m Int
source = sourceUnfoldrM

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

toNull = runStream
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE init #-}
init :: (Monad m, S.IsStream t) => t m a -> m ()
init s = do
    t <- S.init s
    mapM_ S.runStream t

{-# INLINE tail #-}
tail :: (Monad m, S.IsStream t) => t m a -> m ()
tail s = S.tail s >>= mapM_ tail

nullTail s = do
    r <- S.null s
    when (not r) $ S.tail s >>= mapM_ nullTail

headTail s = do
    h <- S.head s
    when (isJust h) $ S.tail s >>= mapM_ headTail

toList = S.toList
foldl  = S.foldl' (+) 0
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
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropOne #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileFalse #-}
scan, map, fmap, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropOne,
    dropWhileTrue, dropWhileFalse
    :: Monad m
    => Int -> Stream m Int -> m ()

{-# INLINE mapM #-}
mapM :: S.MonadAsync m => Int -> Stream m Int -> m ()

scan           n = composeN n $ S.scanl' (+) 0
map            n = composeN n $ P.fmap (+1)
fmap           n = composeN n $ P.fmap (+1)
mapM           n = composeN n $ S.mapM return
filterEven     n = composeN n $ S.filter even
filterAllOut   n = composeN n $ S.filter (> maxValue)
filterAllIn    n = composeN n $ S.filter (<= maxValue)
takeOne        n = composeN n $ S.take 1
takeAll        n = composeN n $ S.take maxValue
takeWhileTrue  n = composeN n $ S.takeWhile (<= maxValue)
dropOne        n = composeN n $ S.drop 1
dropAll        n = composeN n $ S.drop maxValue
dropWhileTrue  n = composeN n $ S.dropWhile (<= maxValue)
dropWhileFalse n = composeN n $ S.dropWhile (<= 1)

-------------------------------------------------------------------------------
-- Iteration
-------------------------------------------------------------------------------

iterStreamLen, maxIters :: Int
iterStreamLen = 10
maxIters = 10000

{-# INLINE iterateSource #-}
iterateSource
    :: S.MonadAsync m
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
    :: S.MonadAsync m
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

zip src       = transform $ S.zipWith (,) src src
concat _n     = return ()

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
