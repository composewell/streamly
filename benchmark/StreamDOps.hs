-- |
-- Module      : StreamDOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}

module StreamDOps where

-- import Prelude
       -- (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=),
        -- subtract, undefined, Maybe(..))
import Prelude
        (Monad, Int, (+), (.), return, (>), even, (<=),
         Maybe(..))

import qualified Streamly.Streams.StreamD as S

value, maxValue :: Int
value = 1000000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

-- {-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-
{-# INLINE takeWhileTrue #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE zip #-}
{-# INLINE concat #-}
{-# INLINE composeAllInFilters #-}
{-# INLINE composeAllOutFilters #-}
{-# INLINE composeMapAllInFilter #-}
-}
map, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll -- takeWhileTrue, dropAll, dropWhileTrue, zip,
    -- concat, composeAllInFilters, composeAllOutFilters,
    -- composeMapAllInFilter
    :: Monad m
    => Stream m Int -> m ()

{-
{-# INLINE composeMapM #-}
composeMapM :: S.MonadAsync m => Stream m Int -> m ()
-}

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE toNull #-}
{-# INLINE mapM #-}
toNull, mapM :: Monad m => Stream m Int -> m ()

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
        else (Just (cnt, cnt + 1))

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

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: Monad m => Int -> Stream m Int
sourceFromFoldable n = S.fromList [n..n+value]

{-# INLINE source #-}
source :: Monad m => Int -> Stream m Int
source n = sourceUnfoldrM n

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

toNull = runStream
toList = S.toList
foldl  = S.foldl' (+) 0
last   = S.last

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

-- scan          = transform . S.scanl' (+) 0
map           = transform . S.map (+1)
mapM          = transform . S.mapM return
filterEven    = transform . S.filter even
filterAllOut  = transform . S.filter (> maxValue)
filterAllIn   = transform . S.filter (<= maxValue)
takeOne       = transform . S.take 1
takeAll       = transform . S.take maxValue
{-
takeWhileTrue = transform . S.takeWhile (<= maxValue)
dropAll       = transform . S.drop maxValue
dropWhileTrue = transform . S.dropWhile (<= maxValue)

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip src       = transform $ (S.zipWith (,) src src)
concat _n     = return ()

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
    -}
