-- |
-- Module      : StreamKOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}

module StreamKOps where

import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined, Maybe(..), not)

import qualified Streamly.Streams.StreamK as S
import qualified Streamly.Streams.Prelude as S
import qualified Streamly.SVar as S

value, maxValue :: Int
value = 100000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE toNull #-}
{-# INLINE uncons #-}
{-# INLINE nullHeadTail #-}
{-# INLINE scan #-}
{-# INLINE map #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE zip #-}
{-# INLINE concat #-}
{-# INLINE composeAllInFilters #-}
{-# INLINE composeAllOutFilters #-}
{-# INLINE composeMapAllInFilter #-}
toNull, uncons, nullHeadTail, scan, map, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, dropAll, dropWhileTrue, zip,
    concat, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Stream m Int -> m ()

{-# INLINE composeMapM #-}
composeMapM :: S.MonadAsync m => Stream m Int -> m ()

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]
{-# INLINE foldl #-}
foldl :: Monad m => Stream m Int -> m Int
{-# INLINE last #-}
last :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE mapM #-}
mapM :: S.MonadAsync m => Stream m Int -> m ()

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
        else (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: S.MonadAsync m => Int -> Stream m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
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
sourceFoldMapWith n = S.foldMapWith (S.serial) S.yield [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: Monad m => Int -> Stream m Int
sourceFoldMapWithM n = S.foldMapWith (S.serial) (S.yieldM . return) [n..n+value]

{-# INLINE source #-}
source :: S.MonadAsync m => Int -> Stream m Int
source n = sourceUnfoldrM n

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
    r <- S.init s
    case r of
        Nothing -> return ()
        Just x -> S.runStream x

{-# INLINE tail #-}
tail :: (Monad m, S.IsStream t) => t m a -> m ()
tail s = do
    r <- S.tail s
    case r of
        Nothing -> return ()
        Just x -> tail x

-- | If the stream is not null get its head and tail and then do the same to
-- the tail.
nullHeadTail s = do
    r <- S.null s
    if not r
    then do
        _ <- S.head s
        t <- S.tail s
        case t of
            Nothing -> return ()
            Just x -> nullHeadTail x
    else return ()

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
