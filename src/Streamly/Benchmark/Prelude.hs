-- |
-- Module      : Streamly.Benchmark.Prelude
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : MIT
-- Maintainer  : harendra.kumar@gmail.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Streamly.Benchmark.Prelude where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Prelude
       (Monad, Int, (+), ($), (.), return, fmap, even, (>), (<=), (==), (>=),
        subtract, undefined, Maybe(..), odd, Bool, not, (>>=), mapM_, curry,
        maxBound, div, IO, compare, Double, fromIntegral, Integer, (<$>),
        (<*>), flip, (**), (/))
import qualified Prelude as P
import qualified Data.Foldable as F
import qualified GHC.Exts as GHC

#ifdef INSPECTION
import Test.Inspection

import qualified Streamly.Streams.StreamD as D
#endif

import qualified Streamly          as S hiding (foldMapWith, runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as Pipe

value, maxValue, value2 :: Int
#ifdef LINEAR_ASYNC
value = 10000
#else
value = 100000
#endif
maxValue = value + 1
value2 = P.round (P.fromIntegral value**(1/2::P.Double)) -- double nested loop

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

type Stream m a = S.SerialT m a

{-# INLINE source #-}
source :: (S.MonadAsync m, S.IsStream t) => Int -> t m Int
source n = sourceUnfoldrM n

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: (Monad m, S.IsStream t) => Int -> t m Int
sourceIntFromTo n = S.enumerateFromTo n (n + value)

{-# INLINE sourceIntFromThenTo #-}
sourceIntFromThenTo :: (Monad m, S.IsStream t) => Int -> t m Int
sourceIntFromThenTo n = S.enumerateFromThenTo n (n + 1) (n + value)

{-# INLINE sourceFracFromTo #-}
sourceFracFromTo :: (Monad m, S.IsStream t) => Int -> t m Double
sourceFracFromTo n =
    S.enumerateFromTo (fromIntegral n) (fromIntegral (n + value))

{-# INLINE sourceFracFromThenTo #-}
sourceFracFromThenTo :: (Monad m, S.IsStream t) => Int -> t m Double
sourceFracFromThenTo n = S.enumerateFromThenTo (fromIntegral n)
    (fromIntegral n + 1.0001) (fromIntegral (n + value))

{-# INLINE sourceIntegerFromStep #-}
sourceIntegerFromStep :: (Monad m, S.IsStream t) => Int -> t m Integer
sourceIntegerFromStep n =
    S.take value $ S.enumerateFromThen (fromIntegral n) (fromIntegral n + 1)

{-# INLINE sourceFromList #-}
sourceFromList :: (Monad m, S.IsStream t) => Int -> t m Int
sourceFromList n = S.fromList [n..n+value]

{-# INLINE sourceFromListM #-}
sourceFromListM :: (S.MonadAsync m, S.IsStream t) => Int -> t m Int
sourceFromListM n = S.fromListM (Prelude.fmap return [n..n+value])

{-# INLINE sourceFromIndices #-}
sourceFromIndices :: (Monad m, S.IsStream t) => Int -> t m Int
sourceFromIndices n = S.take value $ S.fromIndices (+ n)

{-# INLINE sourceFromIndicesM #-}
sourceFromIndicesM :: (S.MonadAsync m, S.IsStream t) => Int -> t m Int
sourceFromIndicesM n = S.take value $ S.fromIndicesM (Prelude.fmap return (+ n))

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

{-# INLINE sourceFoldMapM #-}
sourceFoldMapM :: (S.IsStream t, Monad m, P.Monoid (t m Int))
    => Int -> t m Int
sourceFoldMapM n = F.foldMap (S.yieldM . return) [n..n+value]

{-# INLINE sourceConcatMapId #-}
sourceConcatMapId :: (S.IsStream t, Monad m)
    => Int -> t m Int
sourceConcatMapId n =
    S.concatMap P.id $ S.fromFoldable $ P.map (S.yieldM . return) [n..n+value]

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: (Monad m, S.IsStream t) => Int -> t m Int
sourceUnfoldr n = S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceUnfoldrN #-}
sourceUnfoldrN :: (Monad m, S.IsStream t) => Int -> Int -> t m Int
sourceUnfoldrN upto start = S.unfoldr step start
    where
    step cnt =
        if cnt > start + upto
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
sourceUnfoldrMN upto start = S.unfoldrM step start
    where
    step cnt =
        if cnt > start + upto
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
runStream = S.drain

{-# INLINE toList #-}
toList :: Monad m => Stream m Int -> m [Int]

{-# INLINE head #-}
{-# INLINE last #-}
{-# INLINE maximum #-}
{-# INLINE minimum #-}
{-# INLINE find #-}
{-# INLINE findIndex #-}
{-# INLINE elemIndex #-}
{-# INLINE foldl1'Reduce #-}
head, last, minimum, maximum, find, findIndex, elemIndex, foldl1'Reduce
    :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE minimumBy #-}
{-# INLINE maximumBy #-}
minimumBy, maximumBy :: Monad m => Stream m Int -> m (Maybe Int)

{-# INLINE foldl'Reduce #-}
{-# INLINE foldl'ReduceMap #-}
{-# INLINE foldlM'Reduce #-}
{-# INLINE foldrMReduce #-}
{-# INLINE length #-}
{-# INLINE sum #-}
{-# INLINE product #-}
foldl'Reduce, foldl'ReduceMap, foldlM'Reduce, foldrMReduce, length, sum, product
    :: Monad m
    => Stream m Int -> m Int

{-# INLINE foldl'Build #-}
{-# INLINE foldlM'Build #-}
{-# INLINE foldrMBuild #-}
foldrMBuild, foldl'Build, foldlM'Build
    :: Monad m
    => Stream m Int -> m [Int]

{-# INLINE all #-}
{-# INLINE any #-}
{-# INLINE and #-}
{-# INLINE or #-}
{-# INLINE null #-}
{-# INLINE elem #-}
{-# INLINE notElem #-}
null, elem, notElem, all, any, and, or :: Monad m => Stream m Int -> m Bool

{-# INLINE toNull #-}
toNull :: Monad m => (t m a -> S.SerialT m a) -> t m a -> m ()
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
init s = S.init s >>= Prelude.mapM_ S.drain

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

{-# INLINE toListRev #-}
toListRev :: Monad m => Stream m Int -> m [Int]
toListRev = Internal.toListRev

foldrMBuild  = S.foldrM  (\x xs -> xs >>= return . (x :)) (return [])
foldl'Build = S.foldl' (flip (:)) []
foldlM'Build = S.foldlM' (\xs x -> return $ x : xs) []

foldrMReduce = S.foldrM (\x xs -> xs >>= return . (x +)) (return 0)
foldl'Reduce = S.foldl' (+) 0
foldl'ReduceMap = P.fmap (+1) . S.foldl' (+) 0
foldl1'Reduce = S.foldl1' (+)
foldlM'Reduce = S.foldlM' (\xs a -> return $ a + xs) 0

last   = S.last
null   = S.null
head   = S.head
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
minimumBy = S.minimumBy compare
maximumBy = S.maximumBy compare

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE transform #-}
transform :: Monad m => Stream m a -> m ()
transform = runStream

{-# INLINE composeN #-}
composeN
    :: MonadIO m
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
{-# INLINE scanl1' #-}
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
{-# INLINE insertBy #-}
{-# INLINE deleteBy #-}
{-# INLINE reverse #-}
{-# INLINE reverse' #-}
{-# INLINE foldrS #-}
{-# INLINE foldrSMap #-}
{-# INLINE foldrT #-}
{-# INLINE foldrTMap #-}
scan, scanl1', map, fmap, mapMaybe, filterEven, filterAllOut,
    filterAllIn, takeOne, takeAll, takeWhileTrue, takeWhileMTrue, dropOne,
    dropAll, dropWhileTrue, dropWhileMTrue, dropWhileFalse,
    findIndices, elemIndices, insertBy, deleteBy, reverse, reverse',
    foldrS, foldrSMap, foldrT, foldrTMap
    :: MonadIO m
    => Int -> Stream m Int -> m ()

{-# INLINE mapMaybeM #-}
{-# INLINE intersperse #-}
mapMaybeM, intersperse :: S.MonadAsync m => Int -> Stream m Int -> m ()

{-# INLINE mapM #-}
{-# INLINE map' #-}
{-# INLINE fmap' #-}
mapM, map' :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> Int -> t m Int -> m ()

fmap' :: (S.IsStream t, S.MonadAsync m, P.Functor (t m))
    => (t m Int -> S.SerialT m Int) -> Int -> t m Int -> m ()

{-# INLINE sequence #-}
sequence :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> t m (m Int) -> m ()

scan          n = composeN n $ S.scanl' (+) 0
scanl1'       n = composeN n $ S.scanl1' (+)
fmap          n = composeN n $ Prelude.fmap (+1)
fmap' t       n = composeN' n $ t . Prelude.fmap (+1)
map           n = composeN n $ S.map (+1)
map' t        n = composeN' n $ t . S.map (+1)
mapM t        n = composeN' n $ t . S.mapM return

{-# INLINE transformMapM #-}
{-# INLINE transformComposeMapM #-}
{-# INLINE transformTeeMapM #-}
{-# INLINE transformZipMapM #-}

transformMapM, transformComposeMapM, transformTeeMapM,
    transformZipMapM :: (S.IsStream t, S.MonadAsync m)
    => (t m Int -> S.SerialT m Int) -> Int -> t m Int -> m ()

transformMapM t n = composeN' n $ t . Internal.transform (Pipe.mapM return)
transformComposeMapM t n = composeN' n $ t . Internal.transform
    (Pipe.mapM (\x -> return (x + 1))
        `Pipe.compose` Pipe.mapM (\x -> return (x + 2)))
transformTeeMapM t n = composeN' n $ t . Internal.transform
    (Pipe.mapM (\x -> return (x + 1))
        `Pipe.tee` Pipe.mapM (\x -> return (x + 2)))
transformZipMapM t n = composeN' n $ t . Internal.transform
    (Pipe.zipWith (+) (Pipe.mapM (\x -> return (x + 1)))
        (Pipe.mapM (\x -> return (x + 2))))

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
intersperse    n = composeN n $ S.intersperse maxValue
insertBy       n = composeN n $ S.insertBy compare maxValue
deleteBy       n = composeN n $ S.deleteBy (>=) maxValue
reverse        n = composeN n $ S.reverse
reverse'       n = composeN n $ Internal.reverse'
foldrS         n = composeN n $ Internal.foldrS S.cons S.nil
foldrSMap      n = composeN n $ Internal.foldrS (\x xs -> x + 1 `S.cons` xs) S.nil
foldrT         n = composeN n $ Internal.foldrT S.cons S.nil
foldrTMap      n = composeN n $ Internal.foldrT (\x xs -> x + 1 `S.cons` xs) S.nil

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
{-# INLINE iterateScanl1 #-}
{-# INLINE iterateFilterEven #-}
{-# INLINE iterateTakeAll #-}
{-# INLINE iterateDropOne #-}
{-# INLINE iterateDropWhileFalse #-}
{-# INLINE iterateDropWhileTrue #-}
iterateMapM, iterateScan, iterateScanl1, iterateFilterEven, iterateTakeAll,
    iterateDropOne, iterateDropWhileFalse, iterateDropWhileTrue
    :: S.MonadAsync m
    => Int -> Stream m Int

-- this is quadratic
iterateScan            = iterateSource (S.scanl' (+) 0) (maxIters `div` 10)
-- so is this
iterateScanl1          = iterateSource (S.scanl1' (+)) (maxIters `div` 10)

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
zip :: Int -> Int -> IO ()
zip count n =
    S.drain $ S.zipWith (,)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zip
inspect $ 'zip `hasNoType` ''D.Step
#endif

{-# INLINE zipM #-}
zipM :: Int -> Int -> IO ()
zipM count n =
    S.drain $ S.zipWithM (curry return)
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'zipM
inspect $ 'zipM `hasNoType` ''D.Step
#endif

{-# INLINE mergeBy #-}
mergeBy :: Int -> Int -> IO ()
mergeBy count n =
    S.drain $ S.mergeBy P.compare
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mergeBy
inspect $ 'mergeBy `hasNoType` ''D.Step
#endif

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    S.drain $ S.serial
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    S.drain $ S.serial
        ((S.serial (sourceUnfoldrMN count n)
                   (sourceUnfoldrMN count (n + 1))))
        ((S.serial (sourceUnfoldrMN count (n+2))
                   (sourceUnfoldrMN count (n + 3))))

{-# INLINE append2 #-}
append2 :: Int -> Int -> IO ()
append2 count n =
    S.drain $ Internal.append
        (sourceUnfoldrMN count n)
        (sourceUnfoldrMN count (n + 1))

{-# INLINE append4 #-}
append4 :: Int -> Int -> IO ()
append4 count n =
    S.drain $ Internal.append
        ((Internal.append (sourceUnfoldrMN count n)
                          (sourceUnfoldrMN count (n + 1))))
        ((Internal.append (sourceUnfoldrMN count (n+2))
                          (sourceUnfoldrMN count (n + 3))))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append2
inspect $ 'append2 `hasNoType` ''D.AppendState
#endif

{-# INLINE wSerial2 #-}
wSerial2 :: Int -> IO ()
wSerial2 n = S.drain $ S.wSerial
    (sourceUnfoldrMN (value `div` 2) n)
    (sourceUnfoldrMN (value `div` 2) (n + 1))

{-# INLINE interleave2 #-}
interleave2 :: Int -> IO ()
interleave2 n = S.drain $ Internal.interleave
    (sourceUnfoldrMN (value `div` 2) n)
    (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interleave2
inspect $ 'interleave2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE roundRobin2 #-}
roundRobin2 :: Int -> IO ()
roundRobin2 n = S.drain $ Internal.roundrobin
    (sourceUnfoldrMN (value `div` 2) n)
    (sourceUnfoldrMN (value `div` 2) (n + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'roundRobin2
inspect $ 'roundRobin2 `hasNoType` ''D.InterleaveState
#endif

{-# INLINE isPrefixOf #-}
{-# INLINE isSubsequenceOf #-}
isPrefixOf, isSubsequenceOf :: Monad m => Stream m Int -> m Bool

isPrefixOf src = S.isPrefixOf src src
isSubsequenceOf src = S.isSubsequenceOf src src

{-# INLINE stripPrefix #-}
stripPrefix :: Monad m => Stream m Int -> m ()
stripPrefix src = do
    _ <- S.stripPrefix src src
    return ()

{-# INLINE zipAsync #-}
{-# INLINE zipAsyncM #-}
{-# INLINE zipAsyncAp #-}
zipAsync, zipAsyncAp, zipAsyncM :: S.MonadAsync m => Stream m Int -> m ()

zipAsync src  = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncWith (,) src src1)

zipAsyncM src = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncWithM (curry return) src src1)

zipAsyncAp src  = do
    r <- S.tail src
    let src1 = fromJust r
    transform (S.zipAsyncly $ (,) <$> S.serially src
                                  <*> S.serially src1)

{-# INLINE eqBy' #-}
eqBy' :: (Monad m, P.Eq a) => Stream m a -> m P.Bool
eqBy' src = S.eqBy (==) src src

{-# INLINE eqBy #-}
eqBy :: Int -> IO Bool
eqBy n = eqBy' (source n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqBy
inspect $ 'eqBy `hasNoType` ''D.Step
#endif


{-# INLINE eqByPure #-}
eqByPure :: Int -> Identity Bool
eqByPure n = eqBy' (sourceUnfoldr n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'eqByPure
inspect $ 'eqByPure `hasNoType` ''D.Step
#endif

{-# INLINE cmpBy' #-}
cmpBy' :: (Monad m, P.Ord a) => Stream m a -> m P.Ordering
cmpBy' src = S.cmpBy P.compare src src

{-# INLINE cmpBy #-}
cmpBy :: Int -> IO P.Ordering
cmpBy n = cmpBy' (source n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpBy
inspect $ 'cmpBy `hasNoType` ''D.Step
#endif

{-# INLINE cmpByPure #-}
cmpByPure :: Int -> Identity P.Ordering
cmpByPure n = cmpBy' (sourceUnfoldr n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'cmpByPure
inspect $ 'cmpByPure `hasNoType` ''D.Step
#endif

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrMN inner n)
        (sourceUnfoldrMN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
#endif

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrN inner n)
        (sourceUnfoldrN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapPure
#endif

{-# INLINE concatMapRepl4xN #-}
concatMapRepl4xN :: Int -> IO ()
concatMapRepl4xN n = S.drain $ S.concatMap (S.replicate 4)
                          (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl4xN
#endif

{-# INLINE concatUnfoldRepl4xN #-}
concatUnfoldRepl4xN :: Int -> IO ()
concatUnfoldRepl4xN n =
    S.drain $ S.concatUnfold
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRepl4xN
inspect $ 'concatUnfoldRepl4xN `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE concatMapWithSerial #-}
concatMapWithSerial :: Int -> Int -> Int -> IO ()
concatMapWithSerial outer inner n =
    S.drain $ S.concatMapWith S.serial
        (\_ -> sourceUnfoldrMN inner n)
        (sourceUnfoldrMN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithSerial
#endif

{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend outer inner n =
    S.drain $ S.concatMapWith Internal.append
        (\_ -> sourceUnfoldrMN inner n)
        (sourceUnfoldrMN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
#endif

{-# INLINE concatMapWithWSerial #-}
concatMapWithWSerial :: Int -> Int -> Int -> IO ()
concatMapWithWSerial outer inner n =
    S.drain $ S.concatMapWith S.wSerial
        (\_ -> sourceUnfoldrMN inner n)
        (sourceUnfoldrMN outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithWSerial
#endif

{-# INLINE concatUnfoldInterleaveRepl4xN #-}
concatUnfoldInterleaveRepl4xN :: Int -> IO ()
concatUnfoldInterleaveRepl4xN n =
    S.drain $ Internal.concatUnfoldInterleave
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldInterleaveRepl4xN
-- inspect $ 'concatUnfoldInterleaveRepl4xN `hasNoType` ''D.ConcatUnfoldInterleaveState
#endif

{-# INLINE concatUnfoldRoundrobinRepl4xN #-}
concatUnfoldRoundrobinRepl4xN :: Int -> IO ()
concatUnfoldRoundrobinRepl4xN n =
    S.drain $ Internal.concatUnfoldRoundrobin
        (UF.replicateM 4)
        (sourceUnfoldrMN (value `div` 4) n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatUnfoldRoundrobinRepl4xN
-- inspect $ 'concatUnfoldRoundrobinRepl4xN `hasNoType` ''D.ConcatUnfoldInterleaveState
#endif

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
{-# INLINE filterScanl1 #-}
{-# INLINE filterMap #-}
scanMap, dropMap, dropScan, takeDrop, takeScan, takeMap, filterDrop,
    filterTake, filterScan, filterScanl1, filterMap
    :: MonadIO m => Int -> Stream m Int -> m ()

scanMap    n = composeN n $ S.map (subtract 1) . S.scanl' (+) 0
dropMap    n = composeN n $ S.map (subtract 1) . S.drop 1
dropScan   n = composeN n $ S.scanl' (+) 0 . S.drop 1
takeDrop   n = composeN n $ S.drop 1 . S.take maxValue
takeScan   n = composeN n $ S.scanl' (+) 0 . S.take maxValue
takeMap    n = composeN n $ S.map (subtract 1) . S.take maxValue
filterDrop n = composeN n $ S.drop 1 . S.filter (<= maxValue)
filterTake n = composeN n $ S.take maxValue . S.filter (<= maxValue)
filterScan n = composeN n $ S.scanl' (+) 0 . S.filter (<= maxBound)
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)
filterMap  n = composeN n $ S.map (subtract 1) . S.filter (<= maxValue)

data Pair a b = Pair !a !b deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Monad m => Stream m Int -> m (Int, Int)
sumProductFold = S.foldl' (\(s,p) x -> (s + x, p P.* x)) (0,1)

{-# INLINE sumProductScan #-}
sumProductScan :: Monad m => Stream m Int -> m (Pair Int Int)
sumProductScan = S.foldl' (\(Pair _  p) (s0,x) -> Pair s0 (p P.* x)) (Pair 0 1)
    . S.scanl' (\(s,_) x -> (s + x,x)) (0,0)

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
showInstance src = P.show src

{-# INLINE showInstanceList #-}
showInstanceList :: [Int] -> P.String
showInstanceList src = P.show src

{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Identity Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

{-# INLINE readInstanceList #-}
readInstanceList :: P.String -> [Int]
readInstanceList str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

{-# INLINE pureFoldl' #-}
pureFoldl' :: Stream Identity Int -> Int
pureFoldl' = runIdentity . S.foldl' (+) 0

{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Identity Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Identity Int -> Int
foldableSum = P.sum

{-# INLINE traversableMapM #-}
traversableMapM :: Stream Identity Int -> IO (Stream Identity Int)
traversableMapM = P.mapM return
