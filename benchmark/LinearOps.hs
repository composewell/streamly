-- |
-- Module      : BenchmarkOps
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinearOps where

import Control.Monad (when)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromJust)
import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=), (==), (<=),
        subtract, undefined, Maybe(..), odd, Bool, not, (>>=), mapM_, curry,
        maxBound, div, IO)
import qualified Prelude as P
import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

import qualified Streamly          as S
import qualified Streamly.Prelude  as S

value, maxValue :: Int
#ifdef LINEAR_ASYNC
value = 10000
#else
value = 100000
#endif
maxValue = value + 1

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

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
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> t m Int
sourceUnfoldrM n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE sourceUnfoldrMN #-}
sourceUnfoldrMN :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrMN m n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + m
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
-- Pure stream generation
-------------------------------------------------------------------------------

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> S.SerialT Identity Int
sourceIsList n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> S.SerialT Identity P.Char
sourceIsString n = GHC.fromString (P.replicate (n + value) 'a')

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream = S.runStream

{-# INLINE toList #-}
{-# INLINE foldr #-}
{-# INLINE foldrM #-}
toList, foldr, foldrM :: Monad m => Stream m Int -> m [Int]

{-# INLINE last #-}
{-# INLINE maximum #-}
{-# INLINE minimum #-}
{-# INLINE find #-}
{-# INLINE findIndex #-}
{-# INLINE elemIndex #-}
{-# INLINE foldl1' #-}
{-# INLINE foldr1 #-}
last, minimum, maximum, find, findIndex, elemIndex, foldl1', foldr1
    :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE foldl' #-}
{-# INLINE length #-}
{-# INLINE sum #-}
{-# INLINE product #-}
foldl', length, sum, product :: Monad m => Stream m Int -> m Int

{-# INLINE all #-}
{-# INLINE any #-}
{-# INLINE and #-}
{-# INLINE or #-}
{-# INLINE elem #-}
{-# INLINE notElem #-}
elem, notElem, all, any, and, or :: Monad m => Stream m Int -> m Bool

{-# INLINE toNull #-}
toNull :: Monad m => (t m Int -> S.SerialT m Int) -> t m Int -> m ()
toNull t = runStream . t

{-# INLINE uncons #-}
uncons :: Monad m => Stream m Int -> m ()
uncons s = do
    r <- S.uncons s
    case r of
        Nothing -> return ()
        Just (_, t) -> uncons t

{-# INLINE init #-}
init :: Monad m => Stream m a -> m ()
init s = S.init s >>= Prelude.mapM_ S.runStream

{-# INLINE tail #-}
tail :: Monad m => Stream m a -> m ()
tail s = S.tail s >>= Prelude.mapM_ tail

{-# INLINE nullHeadTail #-}
nullHeadTail :: Monad m => Stream m Int -> m ()
nullHeadTail s = do
    r <- S.null s
    when (not r) $ do
        _ <- S.head s
        S.tail s >>= Prelude.mapM_ nullHeadTail

{-# INLINE mapM_ #-}
mapM_ :: Monad m => Stream m Int -> m ()
mapM_  = S.mapM_ (\_ -> return ())

toList = S.toList
foldr  = S.foldr (:) []
foldr1 = S.foldr1 (+)
foldrM = S.foldrM (\a xs -> return (a : xs)) []
foldl' = S.foldl' (+) 0
foldl1' = S.foldl1' (+)
last   = S.last
elem   = S.elem maxValue
notElem = S.notElem maxValue
length = S.length
all    = S.all (<= maxValue)
any    = S.any (> maxValue)
and    = S.and . S.map (<= maxValue)
or     = S.or . S.map (> maxValue)
find   = S.find (== maxValue)
findIndex = S.findIndex (== maxValue)
elemIndex = S.elemIndex maxValue
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

-- polymorphic stream version of composeN
{-# INLINE composeN' #-}
composeN'
    :: (S.IsStream t, Monad m)
    => Int -> (t m Int -> Stream m Int) -> t m Int -> m ()
composeN' n f =
    case n of
        1 -> transform . f
        2 -> transform . f . S.adapt . f
        3 -> transform . f . S.adapt . f . S.adapt . f
        4 -> transform . f . S.adapt . f . S.adapt . f . S.adapt . f
        _ -> undefined

{-# INLINE scan #-}
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
{-# INLINE dropOne #-}
{-# INLINE dropAll #-}
{-# INLINE dropWhileTrue #-}
{-# INLINE dropWhileMTrue #-}
{-# INLINE dropWhileFalse #-}
{-# INLINE findIndices #-}
{-# INLINE elemIndices #-}
scan, map, fmap, mapMaybe, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, takeWhileMTrue, dropOne,
    dropAll, dropWhileTrue, dropWhileMTrue, dropWhileFalse,
    findIndices, elemIndices
    :: Monad m
    => Int -> Stream m Int -> m ()

{-# INLINE mapMaybeM #-}
mapMaybeM :: S.MonadAsync m => Int -> Stream m Int -> m ()

{-# INLINE mapM #-}
mapM :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> Int -> t m Int -> m ()

{-# INLINE sequence #-}
sequence :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> t m (m Int) -> m ()

scan          n = composeN n $ S.scanl' (+) 0
fmap          n = composeN n $ Prelude.fmap (+1)
map           n = composeN n $ S.map (+1)
mapM t        n = composeN' n $ t . S.mapM return
mapMaybe      n = composeN n $ S.mapMaybe
    (\x -> if Prelude.odd x then Nothing else Just x)
mapMaybeM     n = composeN n $ S.mapMaybeM
    (\x -> if Prelude.odd x then return Nothing else return $ Just x)
sequence t    = transform . t . S.sequence
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
findIndices    n = composeN n $ S.findIndices (== maxValue)
elemIndices    n = composeN n $ S.elemIndices maxValue

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

iterateMapM            = iterateSource (S.mapM return) maxIters
iterateFilterEven      = iterateSource (S.filter even) maxIters
iterateTakeAll         = iterateSource (S.take maxValue) maxIters
iterateDropOne         = iterateSource (S.drop 1) maxIters
iterateDropWhileFalse  = iterateSource (S.dropWhile (> maxValue)) maxIters
iterateDropWhileTrue   = iterateSource (S.dropWhile (<= maxValue)) maxIters

-------------------------------------------------------------------------------
-- Zipping and concat
-------------------------------------------------------------------------------

{-# INLINE zip #-}
{-# INLINE zipM #-}
{-# INLINE concat #-}
zip, zipM, concat  :: Monad m => Stream m Int -> m ()

zip src       = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipWith (,) src src1)
zipM src      =  do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipWithM (curry return) src src1)

{-# INLINE zipAsync #-}
{-# INLINE zipAsyncM #-}
zipAsync, zipAsyncM :: S.MonadAsync m => Stream m Int -> m ()

zipAsync src  = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncWith (,) src src1)
zipAsyncM src = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncWithM (curry return) src src1)

concat _n     = return ()

{-# INLINE eqBy #-}
eqBy :: (Monad m, P.Eq a) => Stream m a -> m P.Bool
eqBy src = S.eqBy (==) src src

{-# INLINE cmpBy #-}
cmpBy :: (Monad m, P.Ord a) => Stream m a -> m P.Ordering
cmpBy src = S.cmpBy P.compare src src

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

-------------------------------------------------------------------------------
-- Pure stream operations
-------------------------------------------------------------------------------

{-# INLINE eqInstance #-}
eqInstance :: Stream Identity Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Identity Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE ordInstance #-}
ordInstance :: Stream Identity Int -> Bool
ordInstance src = src P.< src

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Identity Int -> Stream Identity Int
ordInstanceMin src = P.min src src

{-# INLINE showInstance #-}
showInstance :: Stream Identity Int -> P.String
showInstance src = P.showsPrec 0 src ""

{-# INLINE showInstanceList #-}
showInstanceList :: Stream Identity Int -> P.String
showInstanceList src = P.showsPrec 0 (GHC.toList src P.++ [2..value]) ""

{-# INLINE readInstance #-}
readInstance :: Stream Identity Int -> Stream Identity Int
readInstance src =
    let r = P.readsPrec 0 ("fromList [1"
                P.++ P.concat (P.replicate value ",1") P.++ "]")
    in case r of
        [(x,"")] -> src S.<> x
        _ -> P.error "readInstance: no parse"

{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Identity Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Identity Int -> Int
foldableSum = P.sum

{-# INLINE traversableMapM #-}
traversableMapM :: Stream Identity Int -> IO (Stream Identity Int)
traversableMapM = P.mapM return
