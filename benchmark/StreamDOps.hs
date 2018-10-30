-- |
-- Module      : StreamDOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}

module StreamDOps where

import Control.Monad (when)
import Data.Maybe (isJust)
import Prelude
        (Monad, Int, (+), ($), (.), return, (>), even, (<=),
         subtract, undefined, Maybe(..), not, mapM_, (>>=),
         maxBound)

import qualified Streamly.Streams.StreamD as S

value, maxValue :: Int
value = 100000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
{-# INLINE nullTail #-}
{-# INLINE headTail #-}
{-# INLINE zip #-}
{-
{-# INLINE concat #-}
-}
uncons, nullTail, headTail, zip
    -- concat,
    :: Monad m
    => Stream m Int -> m ()

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE toNull #-}
toNull :: Monad m => Stream m Int -> m ()

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

toNull = runStream

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
{-# INLINE mapM #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
scan, map, mapM, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue
    :: Monad m
    => Int -> Stream m Int -> m ()

scan          n = composeN n $ S.scanl' (+) 0
map           n = composeN n $ S.map (+1)
mapM          n = composeN n $ S.mapM return
filterEven    n = composeN n $ S.filter even
filterAllOut  n = composeN n $ S.filter (> maxValue)
filterAllIn   n = composeN n $ S.filter (<= maxValue)
takeOne       n = composeN n $ S.take 1
takeAll       n = composeN n $ S.take maxValue
takeWhileTrue n = composeN n $ S.takeWhile (<= maxValue)
dropAll       n = composeN n $ S.drop maxValue
dropWhileTrue n = composeN n $ S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip src       = transform $ S.zipWith (,) src src
-- concat _n     = return ()

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
