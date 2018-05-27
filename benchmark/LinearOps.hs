-- |
-- Module      : BenchmarkOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}

module LinearOps where

import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined, Maybe(..), Monoid, foldMap)

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

value, appendValue, maxValue :: Int
value = 1000000
appendValue = 100000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
{-# INLINE toList #-}
{-# INLINE foldl #-}
{-# INLINE last #-}
{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE filterEven #-}
{-# INLINE mapM #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE zip #-}
{-# INLINE concat #-}
{-# INLINE composeMapM #-}
{-# INLINE composeAllInFilters #-}
{-# INLINE composeAllOutFilters #-}
{-# INLINE composeMapAllInFilter #-}
scan, map, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Stream m Int -> m ()

mapM, composeMapM :: S.MonadAsync m => Stream m Int -> m ()
toList :: Monad m => Stream m Int -> m [Int]
foldl :: Monad m => Stream m Int -> m Int
last :: Monad m => Stream m Int -> m (Maybe Int)

toNull :: Monad m => (t m Int -> S.SerialT m Int) -> t m Int -> m ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.SerialT m a

{-# INLINE source #-}
source :: S.IsStream t => Int -> t m Int
source n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: S.IsStream t => Int -> t m Int
sourceFromFoldable n = S.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: (S.IsStream t, S.MonadAsync m) => Int -> t m Int
sourceFromFoldableM n = S.fromFoldableM (Prelude.fmap return [n..n+value])

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (S.IsStream t, Monad (t m), S.Semigroup (t m Int))
    => Int -> t m Int
sourceFoldMapWith n = S.foldMapWith (S.<>) return [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: (S.IsStream t, Monad m, S.Semigroup (t m Int))
    => Int -> t m Int
sourceFoldMapWithM n = S.foldMapWith (S.<>) (S.once . return) [n..n+value]

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> t m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

toNull t = runStream . t
toList = S.toList
foldl  = S.foldl' (+) 0
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

scan          = transform . S.scanl' (+) 0
map           = transform . fmap (+1)
mapM          = transform . S.mapM return
filterEven    = transform . S.filter even
filterAllOut  = transform . S.filter (> maxValue)
filterAllIn   = transform . S.filter (<= maxValue)
takeOne       = transform . S.take 1
takeAll       = transform . S.take maxValue
takeWhileTrue = transform . S.takeWhile (<= maxValue)
dropAll       = transform . S.drop maxValue
dropWhileTrue = transform . S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip src       = transform $ (S.zipWith (,) src src)
concat _n     = return ()

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE append #-}
append
    :: (Monoid (t m Int), Monad m, Monad (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> m ()
append t n = runStream $ t $ foldMap return [n..n+appendValue]

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

{-# INLINE compose #-}
compose :: Monad m => (Stream m Int -> Stream m Int) -> Stream m Int -> m ()
compose f = transform . f . f . f . f

composeMapM           = compose (S.mapM return)
composeAllInFilters   = compose (S.filter (<= maxValue))
composeAllOutFilters  = compose (S.filter (> maxValue))
composeMapAllInFilter = compose (S.filter (<= maxValue) . fmap (subtract 1))

{-# INLINABLE composeScaling #-}
composeScaling :: Monad m => Int -> Stream m Int -> m ()
composeScaling m =
    case m of
        1 -> transform . f
        2 -> transform . f . f
        3 -> transform . f . f . f
        4 -> transform . f . f . f . f
        _ -> undefined
    where f = S.filter (<= maxValue)
