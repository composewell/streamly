-- |
-- Module      : BenchmarkOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE FlexibleContexts #-}

module LinearOps where

import Data.Maybe (fromJust)
import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=),
        subtract, undefined, Maybe(..), odd, Bool, not)

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

value, maxValue :: Int
value = 100000
maxValue = value + 1000

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
{-# INLINE nullHeadTail #-}
{-# INLINE scan #-}
{-# INLINE mapM_ #-}
{-# INLINE map #-}
{-# INLINE fmap #-}
{-# INLINE mapMaybe #-}
{-# INLINE filterEven #-}
{-# INLINE filterAllOut #-}
{-# INLINE filterAllIn #-}
{-# INLINE takeOne #-}
{-# INLINE takeAll #-}
{-# INLINE takeWhileTrue #-}
{-# INLINE takeWhileMTrue #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileMTrue #-}
{-# INLINE zip #-}
{-# INLINE zipM #-}
{-# INLINE concat #-}
{-# INLINE composeAllInFilters #-}
{-# INLINE composeAllOutFilters #-}
{-# INLINE composeMapAllInFilter #-}
uncons, nullHeadTail, scan, mapM_, map, fmap, mapMaybe, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, takeWhileMTrue, dropAll,
    dropWhileTrue, dropWhileMTrue, zip, zipM,
    concat, composeAllInFilters, composeAllOutFilters,
    composeMapAllInFilter
    :: Monad m
    => Stream m Int -> m ()

{-# INLINE composeMapM #-}
{-# INLINE zipAsync #-}
{-# INLINE zipAsyncM #-}
{-# INLINE mapMaybeM #-}
composeMapM, zipAsync, zipAsyncM, mapMaybeM :: S.MonadAsync m => Stream m Int -> m ()

{-# INLINE toList #-}
{-# INLINE foldr #-}
{-# INLINE foldrM #-}
toList, foldr, foldrM :: Monad m => Stream m Int -> m [Int]

{-# INLINE last #-}
{-# INLINE maximum #-}
{-# INLINE minimum #-}
last, minimum, maximum :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE foldl #-}
{-# INLINE length #-}
{-# INLINE sum #-}
{-# INLINE product #-}
foldl, length, sum, product :: Monad m => Stream m Int -> m Int

{-# INLINE all #-}
{-# INLINE any #-}
{-# INLINE elem #-}
{-# INLINE notElem #-}
elem, notElem, all, any :: Monad m => Stream m Int -> m Bool

{-# INLINE toNull #-}
toNull :: Monad m => (t m Int -> S.SerialT m Int) -> t m Int -> m ()

{-# INLINE mapM #-}
mapM :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> t m Int -> m ()

{-# INLINE sequence #-}
sequence :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> t m (m Int) -> m ()

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.SerialT m a

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> t m Int
source n = S.serially $ sourceUnfoldrM n
-- source n = S.serially $ sourceFromList n

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, S.IsStream t) => Int -> t m Int
sourceFromList n = S.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (S.MonadAsync m, S.IsStream t) => Int -> t m Int
sourceFromListM n = S.fromListM (Prelude.fmap return [n..n+value])

{-# INLINE sourceFromFoldable #-}
sourceFromFoldable :: S.IsStream t => Int -> t m Int
sourceFromFoldable n = S.fromFoldable [n..n+value]

{-# INLINE sourceFromFoldableM #-}
sourceFromFoldableM :: (S.IsStream t, S.MonadAsync m) => Int -> t m Int
sourceFromFoldableM n = S.fromFoldableM (Prelude.fmap return [n..n+value])

{-# INLINE sourceFoldMapWith #-}
sourceFoldMapWith :: (S.IsStream t, S.Semigroup (t m Int))
    => Int -> t m Int
sourceFoldMapWith n = S.foldMapWith (S.<>) S.yield [n..n+value]

{-# INLINE sourceFoldMapWithM #-}
sourceFoldMapWithM :: (S.IsStream t, Monad m, S.Semigroup (t m Int))
    => Int -> t m Int
sourceFoldMapWithM n = S.foldMapWith (S.<>) (S.yieldM . return) [n..n+value]

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> t m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> t m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMAction #-}
sourceUnfoldrMAction :: (S.IsStream t, S.MonadAsync m) => Int -> t m (m Int)
sourceUnfoldrMAction n = S.serially $ S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (return cnt, cnt + 1))

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

toNull t = runStream . t
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t
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
mapM_  = S.mapM_ (\_ -> return ())
toList = S.toList
foldr  = S.foldr (:) []
foldrM = S.foldrM (\a xs -> return (a : xs)) []
foldl  = S.foldl' (+) 0
last   = S.last
elem   = S.elem maxValue
notElem = S.notElem maxValue
length = S.length
all    = S.all (<= maxValue)
any    = S.any (> maxValue)
maximum = S.maximum
minimum = S.minimum
sum    = S.sum
product = S.product

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

scan          = transform . S.scanl' (+) 0
fmap          = transform . Prelude.fmap (+1)
map           = transform . S.map (+1)
mapM t        = transform . t . S.mapM return
mapMaybe      = transform . S.mapMaybe
    (\x -> if Prelude.odd x then Nothing else Just ())
mapMaybeM     = transform . S.mapMaybeM
    (\x -> if Prelude.odd x then (return Nothing) else return $ Just ())
sequence t    = transform . t . S.sequence
filterEven    = transform . S.filter even
filterAllOut  = transform . S.filter (> maxValue)
filterAllIn   = transform . S.filter (<= maxValue)
takeOne       = transform . S.take 1
takeAll       = transform . S.take maxValue
takeWhileTrue = transform . S.takeWhile (<= maxValue)
takeWhileMTrue = transform . S.takeWhileM (return . (<= maxValue))
dropAll       = transform . S.drop maxValue
dropWhileTrue = transform . S.dropWhile (<= maxValue)
dropWhileMTrue = transform . S.dropWhileM (return . (<= maxValue))

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

zip src       = do
    r <- S.tail src
    let src1 = fromJust r
    transform $ (S.zipWith (,) src src1)
zipM src      =  do
    r <- S.tail src
    let src1 = fromJust r
    transform $ (S.zipWithM (\a b -> return (a,b)) src src1)
zipAsync src  = do
    r <- S.tail src
    let src1 = fromJust r
    transform $ (S.zipAsyncWith (,) src src1)
zipAsyncM src = do
    r <- S.tail src
    let src1 = fromJust r
    transform $ (S.zipAsyncWithM (\a b -> return (a,b)) src src1)
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
composeMapAllInFilter =
    compose (S.filter (<= maxValue) . Prelude.fmap (subtract 1))

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
