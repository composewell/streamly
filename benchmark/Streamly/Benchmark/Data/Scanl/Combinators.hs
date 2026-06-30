-- |
-- Module      : Scanl.Combinators
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -fplugin-opt=Fusion.Plugin:verbose=2 #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Benchmarks for operations exported from Streamly.Internal.Data.Scanl.Combinators.
module Scanl.Combinators (benchmarks) where

import Data.Monoid (Sum(..))
import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Test.Tasty.Bench
import Prelude hiding (sum, product, mconcat, foldMap, unzip)

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Stream (Step(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream n f = randomRIO (1, 1) >>= f . sourceUnfoldrM n

{-# INLINE withPostscanl #-}
withPostscanl :: Int -> Scanl IO Int b -> IO ()
withPostscanl n s = withStream n $ Stream.fold FL.drain . Stream.postscanl s

{-# INLINE withPostscanlMap #-}
withPostscanlMap :: Int -> (Int -> a) -> Scanl IO a b -> IO ()
withPostscanlMap n f s =
    withStream n $ Stream.fold FL.drain . Stream.postscanl s . fmap f

{-# INLINE benchIO #-}
benchIO :: String -> (Int -> IO ()) -> Int -> Benchmark
benchIO name f value = bench name $ nfIO $ f value

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

-------------------------------------------------------------------------------
-- Semigroups and monoids
-------------------------------------------------------------------------------

{-# NOINLINE sconcat #-}
sconcat :: Int -> IO ()
sconcat n = withPostscanlMap n Sum (Scanl.sconcat (Sum 0))

#ifdef INSPECTION
inspect $ 'sconcat `hasNoType` ''Step
inspect $ 'sconcat `hasNoType` ''FL.Step
inspect $ 'sconcat `hasNoType` ''SPEC
#endif

{-# NOINLINE mconcat #-}
mconcat :: Int -> IO ()
mconcat n = withPostscanlMap n Sum Scanl.mconcat

#ifdef INSPECTION
inspect $ 'mconcat `hasNoType` ''Step
inspect $ 'mconcat `hasNoType` ''FL.Step
inspect $ 'mconcat `hasNoType` ''SPEC
#endif

{-# NOINLINE foldMap #-}
foldMap :: Int -> IO ()
foldMap n = withPostscanl n (Scanl.foldMap Sum)

#ifdef INSPECTION
inspect $ 'foldMap `hasNoType` ''Step
inspect $ 'foldMap `hasNoType` ''FL.Step
inspect $ 'foldMap `hasNoType` ''SPEC
#endif

{-# NOINLINE foldMapM #-}
foldMapM :: Int -> IO ()
foldMapM n = withPostscanl n (Scanl.foldMapM (return . Sum))

#ifdef INSPECTION
inspect $ 'foldMapM `hasNoType` ''Step
inspect $ 'foldMapM `hasNoType` ''FL.Step
inspect $ 'foldMapM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Reducers
-------------------------------------------------------------------------------

{-# NOINLINE drainMapM #-}
drainMapM :: Int -> IO ()
drainMapM n = withPostscanl n (Scanl.drainMapM return)

#ifdef INSPECTION
inspect $ 'drainMapM `hasNoType` ''Step
inspect $ 'drainMapM `hasNoType` ''FL.Step
inspect $ 'drainMapM `hasNoType` ''SPEC
#endif

{-# NOINLINE the #-}
the :: Int -> IO ()
the n = withPostscanlMap n (const (1 :: Int)) Scanl.the

#ifdef INSPECTION
inspect $ 'the `hasNoType` ''Step
inspect $ 'the `hasNoType` ''FL.Step
inspect $ 'the `hasNoType` ''SPEC
#endif

{-# NOINLINE mean #-}
mean :: Int -> IO ()
mean n = withPostscanlMap n (fromIntegral :: Int -> Double) Scanl.mean

#ifdef INSPECTION
inspect $ 'mean `hasNoType` ''Step
inspect $ 'mean `hasNoType` ''FL.Step
inspect $ 'mean `hasNoType` ''SPEC
#endif

{-# NOINLINE rollingHash #-}
rollingHash :: Int -> IO ()
rollingHash n = withPostscanl n Scanl.rollingHash

#ifdef INSPECTION
inspect $ 'rollingHash `hasNoType` ''Step
inspect $ 'rollingHash `hasNoType` ''FL.Step
inspect $ 'rollingHash `hasNoType` ''SPEC
#endif

{-# NOINLINE rollingHashWithSalt #-}
rollingHashWithSalt :: Int -> IO ()
rollingHashWithSalt n = withPostscanl n (Scanl.rollingHashWithSalt Scanl.defaultSalt)

#ifdef INSPECTION
inspect $ 'rollingHashWithSalt `hasNoType` ''Step
inspect $ 'rollingHashWithSalt `hasNoType` ''FL.Step
inspect $ 'rollingHashWithSalt `hasNoType` ''SPEC
#endif

{-# NOINLINE rollingHashFirstN #-}
rollingHashFirstN :: Int -> IO ()
rollingHashFirstN n = withPostscanl n (Scanl.rollingHashFirstN n)

#ifdef INSPECTION
inspect $ 'rollingHashFirstN `hasNoType` ''Step
inspect $ 'rollingHashFirstN `hasNoType` ''SPEC
#endif

{-# NOINLINE sum #-}
sum :: Int -> IO ()
sum n = withPostscanl n Scanl.sum

#ifdef INSPECTION
inspect $ 'sum `hasNoType` ''Step
inspect $ 'sum `hasNoType` ''FL.Step
inspect $ 'sum `hasNoType` ''SPEC
#endif

{-# NOINLINE product #-}
product :: Int -> IO ()
product n = withPostscanl n Scanl.product

#ifdef INSPECTION
inspect $ 'product `hasNoType` ''Step
inspect $ 'product `hasNoType` ''FL.Step
inspect $ 'product `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Scanners
-------------------------------------------------------------------------------

{-# NOINLINE indexingWith #-}
indexingWith :: Int -> IO ()
indexingWith n = withPostscanl n (Scanl.indexingWith 0 (+ 1))

#ifdef INSPECTION
inspect $ 'indexingWith `hasNoType` ''Step
inspect $ 'indexingWith `hasNoType` ''FL.Step
inspect $ 'indexingWith `hasNoType` ''SPEC
#endif

{-# NOINLINE indexing #-}
indexing :: Int -> IO ()
indexing n = withPostscanl n Scanl.indexing

#ifdef INSPECTION
inspect $ 'indexing `hasNoType` ''Step
inspect $ 'indexing `hasNoType` ''FL.Step
inspect $ 'indexing `hasNoType` ''SPEC
#endif

{-# NOINLINE indexingRev #-}
indexingRev :: Int -> IO ()
indexingRev n = withPostscanl n (Scanl.indexingRev n)

#ifdef INSPECTION
inspect $ 'indexingRev `hasNoType` ''Step
inspect $ 'indexingRev `hasNoType` ''FL.Step
inspect $ 'indexingRev `hasNoType` ''SPEC
#endif

{-# NOINLINE rollingMap #-}
rollingMap :: Int -> IO ()
rollingMap n = withPostscanl n (Scanl.rollingMap (\_ x -> x))

#ifdef INSPECTION
inspect $ 'rollingMap `hasNoType` ''Step
inspect $ 'rollingMap `hasNoType` ''FL.Step
inspect $ 'rollingMap `hasNoType` ''SPEC
#endif

{-# NOINLINE rollingMapM #-}
rollingMapM :: Int -> IO ()
rollingMapM n = withPostscanl n (Scanl.rollingMapM (\_ x -> return x))

#ifdef INSPECTION
inspect $ 'rollingMapM `hasNoType` ''Step
inspect $ 'rollingMapM `hasNoType` ''FL.Step
inspect $ 'rollingMapM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filters
-------------------------------------------------------------------------------

{-# NOINLINE deleteBy #-}
deleteBy :: Int -> IO ()
deleteBy n = withPostscanl n (Scanl.deleteBy (==) 0)

#ifdef INSPECTION
inspect $ 'deleteBy `hasNoType` ''Step
inspect $ 'deleteBy `hasNoType` ''FL.Step
inspect $ 'deleteBy `hasNoType` ''SPEC
#endif

{-# NOINLINE uniqBy #-}
uniqBy :: Int -> IO ()
uniqBy n = withPostscanl n (Scanl.uniqBy (==))

#ifdef INSPECTION
inspect $ 'uniqBy `hasNoType` ''Step
inspect $ 'uniqBy `hasNoType` ''FL.Step
inspect $ 'uniqBy `hasNoType` ''SPEC
#endif

{-# NOINLINE uniq #-}
uniq :: Int -> IO ()
uniq n = withPostscanl n Scanl.uniq

#ifdef INSPECTION
inspect $ 'uniq `hasNoType` ''Step
inspect $ 'uniq `hasNoType` ''FL.Step
inspect $ 'uniq `hasNoType` ''SPEC
#endif

{-# NOINLINE findIndices #-}
findIndices :: Int -> IO ()
findIndices n = withPostscanl n (Scanl.findIndices (== n))

#ifdef INSPECTION
inspect $ 'findIndices `hasNoType` ''Step
inspect $ 'findIndices `hasNoType` ''FL.Step
inspect $ 'findIndices `hasNoType` ''SPEC
#endif

{-# NOINLINE elemIndices #-}
elemIndices :: Int -> IO ()
elemIndices n = withPostscanl n (Scanl.elemIndices n)

#ifdef INSPECTION
inspect $ 'elemIndices `hasNoType` ''Step
inspect $ 'elemIndices `hasNoType` ''FL.Step
inspect $ 'elemIndices `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Multi-element scans
-------------------------------------------------------------------------------

{-# NOINLINE drainN #-}
drainN :: Int -> IO ()
drainN n = withPostscanl n (Scanl.drainN n)

#ifdef INSPECTION
inspect $ 'drainN `hasNoType` ''Step
inspect $ 'drainN `hasNoType` ''FL.Step
inspect $ 'drainN `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Trimmers
-------------------------------------------------------------------------------

{-# NOINLINE takingEndByM #-}
takingEndByM :: Int -> IO ()
takingEndByM n = withPostscanl n (Scanl.takingEndByM (return . (>= n)))

#ifdef INSPECTION
inspect $ 'takingEndByM `hasNoType` ''Step
inspect $ 'takingEndByM `hasNoType` ''FL.Step
inspect $ 'takingEndByM `hasNoType` ''SPEC
#endif

{-# NOINLINE takingEndBy #-}
takingEndBy :: Int -> IO ()
takingEndBy n = withPostscanl n (Scanl.takingEndBy (>= n))

#ifdef INSPECTION
inspect $ 'takingEndBy `hasNoType` ''Step
inspect $ 'takingEndBy `hasNoType` ''FL.Step
inspect $ 'takingEndBy `hasNoType` ''SPEC
#endif

{-# NOINLINE takingEndByM_ #-}
takingEndByM_ :: Int -> IO ()
takingEndByM_ n = withPostscanl n (Scanl.takingEndByM_ (return . (>= n)))

#ifdef INSPECTION
inspect $ 'takingEndByM_ `hasNoType` ''Step
inspect $ 'takingEndByM_ `hasNoType` ''FL.Step
inspect $ 'takingEndByM_ `hasNoType` ''SPEC
#endif

{-# NOINLINE takingEndBy_ #-}
takingEndBy_ :: Int -> IO ()
takingEndBy_ n = withPostscanl n (Scanl.takingEndBy_ (>= n))

#ifdef INSPECTION
inspect $ 'takingEndBy_ `hasNoType` ''Step
inspect $ 'takingEndBy_ `hasNoType` ''FL.Step
inspect $ 'takingEndBy_ `hasNoType` ''SPEC
#endif

{-# NOINLINE droppingWhileM #-}
droppingWhileM :: Int -> IO ()
droppingWhileM n = withPostscanl n (Scanl.droppingWhileM (return . (<= n)))

#ifdef INSPECTION
inspect $ 'droppingWhileM `hasNoType` ''Step
inspect $ 'droppingWhileM `hasNoType` ''FL.Step
inspect $ 'droppingWhileM `hasNoType` ''SPEC
#endif

{-# NOINLINE droppingWhile #-}
droppingWhile :: Int -> IO ()
droppingWhile n = withPostscanl n (Scanl.droppingWhile (<= n))

#ifdef INSPECTION
inspect $ 'droppingWhile `hasNoType` ''Step
inspect $ 'droppingWhile `hasNoType` ''FL.Step
inspect $ 'droppingWhile `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Scanning input
-------------------------------------------------------------------------------

{-# NOINLINE compose #-}
compose :: Int -> IO ()
compose n = withPostscanl n (Scanl.compose Scanl.sum Scanl.drain)

#ifdef INSPECTION
inspect $ 'compose `hasNoType` ''Step
inspect $ 'compose `hasNoType` ''SPEC
#endif

{-# NOINLINE composeMany #-}
composeMany :: Int -> IO ()
composeMany n = withPostscanl n (Scanl.composeMany (Scanl.take 2 Scanl.sum) Scanl.drain)

#ifdef INSPECTION
inspect $ 'composeMany `hasNoType` ''Step
inspect $ 'composeMany `hasNoType` ''SPEC
#endif

{-# NOINLINE pipe #-}
pipe :: Int -> IO ()
pipe n = withPostscanl n (Scanl.pipe (Pipe.mapM (\x -> return (x + 1))) Scanl.drain)

#ifdef INSPECTION
inspect $ 'pipe `hasNoType` ''Step
inspect $ 'pipe `hasNoType` ''FL.Step
inspect $ 'pipe `hasNoType` ''SPEC
#endif

{-# NOINLINE indexed #-}
indexed :: Int -> IO ()
indexed n = withPostscanl n (Scanl.indexed Scanl.length)

#ifdef INSPECTION
inspect $ 'indexed `hasNoType` ''Step
inspect $ 'indexed `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filtering input
-------------------------------------------------------------------------------

{-# NOINLINE mapMaybeM #-}
mapMaybeM :: Int -> IO ()
mapMaybeM n =
    withPostscanl n
        (Scanl.mapMaybeM
            (\x -> return (if even x then Just x else Nothing))
            Scanl.drain)

#ifdef INSPECTION
inspect $ 'mapMaybeM `hasNoType` ''Step
inspect $ 'mapMaybeM `hasNoType` ''FL.Step
inspect $ 'mapMaybeM `hasNoType` ''SPEC
#endif

{-# NOINLINE mapMaybe #-}
mapMaybe :: Int -> IO ()
mapMaybe n =
    withPostscanl n
        (Scanl.mapMaybe (\x -> if even x then Just x else Nothing) Scanl.drain)

#ifdef INSPECTION
inspect $ 'mapMaybe `hasNoType` ''Step
inspect $ 'mapMaybe `hasNoType` ''FL.Step
inspect $ 'mapMaybe `hasNoType` ''SPEC
#endif

{-# NOINLINE sampleFromthen #-}
sampleFromthen :: Int -> IO ()
sampleFromthen n = withPostscanl n (Scanl.sampleFromthen 0 2 Scanl.drain)

#ifdef INSPECTION
inspect $ 'sampleFromthen `hasNoType` ''Step
inspect $ 'sampleFromthen `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Parallel distribution
-------------------------------------------------------------------------------

{-# NOINLINE tee #-}
tee :: Int -> IO ()
tee n = withPostscanl n (Scanl.tee Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'tee `hasNoType` ''Step
inspect $ 'tee `hasNoType` ''FL.Step
inspect $ 'tee `hasNoType` ''SPEC
#endif

{-# NOINLINE distribute #-}
distribute :: Int -> IO ()
distribute n = withPostscanl n (Scanl.distribute [Scanl.sum, Scanl.length])

-------------------------------------------------------------------------------
-- Unzipping
-------------------------------------------------------------------------------

{-# NOINLINE unzip #-}
unzip :: Int -> IO ()
unzip n = withPostscanlMap n (\a -> (a, a)) (Scanl.unzip Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'unzip `hasNoType` ''Step
inspect $ 'unzip `hasNoType` ''FL.Step
inspect $ 'unzip `hasNoType` ''SPEC
#endif

{-# NOINLINE unzipWith #-}
unzipWith :: Int -> IO ()
unzipWith n = withPostscanl n (Scanl.unzipWith (\a -> (a, a)) Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'unzipWith `hasNoType` ''Step
inspect $ 'unzipWith `hasNoType` ''FL.Step
inspect $ 'unzipWith `hasNoType` ''SPEC
#endif

{-# NOINLINE unzipWithM #-}
unzipWithM :: Int -> IO ()
unzipWithM n =
    withPostscanl n (Scanl.unzipWithM (\a -> return (a, a)) Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'unzipWithM `hasNoType` ''Step
inspect $ 'unzipWithM `hasNoType` ''FL.Step
inspect $ 'unzipWithM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Partitioning
-------------------------------------------------------------------------------

{-# NOINLINE partitionByM #-}
partitionByM :: Int -> IO ()
partitionByM n =
    withPostscanl n (Scanl.partitionByM (return . oddEven) Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'partitionByM `hasNoType` ''Step
inspect $ 'partitionByM `hasNoType` ''FL.Step
inspect $ 'partitionByM `hasNoType` ''SPEC
#endif

{-# NOINLINE partitionBy #-}
partitionBy :: Int -> IO ()
partitionBy n = withPostscanl n (Scanl.partitionBy oddEven Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'partitionBy `hasNoType` ''Step
inspect $ 'partitionBy `hasNoType` ''FL.Step
inspect $ 'partitionBy `hasNoType` ''SPEC
#endif

{-# NOINLINE partition #-}
partition :: Int -> IO ()
partition n = withPostscanlMap n oddEven (Scanl.partition Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'partition `hasNoType` ''Step
inspect $ 'partition `hasNoType` ''FL.Step
inspect $ 'partition `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- O(n) heap: building structures
-------------------------------------------------------------------------------

{-# NOINLINE toListRev #-}
toListRev :: Int -> IO ()
toListRev n = withPostscanl n Scanl.toListRev

{-# NOINLINE toStream #-}
toStream :: Int -> IO ()
toStream n =
    withStream n
        $ Stream.fold FL.drain
        . Stream.postscanl (Scanl.toStream :: Scanl IO Int (Stream IO Int))

{-# NOINLINE toStreamRev #-}
toStreamRev :: Int -> IO ()
toStreamRev n =
    withStream n
        $ Stream.fold FL.drain
        . Stream.postscanl (Scanl.toStreamRev :: Scanl IO Int (Stream IO Int))

{-# NOINLINE topBy #-}
topBy :: Int -> IO ()
topBy n = withPostscanl n (Scanl.topBy compare 10)

{-# NOINLINE top #-}
top :: Int -> IO ()
top n = withPostscanl n (Scanl.top 10)

{-# NOINLINE bottomBy #-}
bottomBy :: Int -> IO ()
bottomBy n = withPostscanl n (Scanl.bottomBy compare 10)

{-# NOINLINE bottom #-}
bottom :: Int -> IO ()
bottom n = withPostscanl n (Scanl.bottom 10)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    fmap (SpaceO_1,)
        [ benchIO "sconcat" sconcat value
        , benchIO "mconcat" mconcat value
        , benchIO "foldMap" foldMap value
        , benchIO "foldMapM" foldMapM value
        , benchIO "drainMapM" drainMapM value
        , benchIO "the" the value
        , benchIO "mean" mean value
        , benchIO "rollingHash" rollingHash value
        , benchIO "rollingHashWithSalt" rollingHashWithSalt value
        , benchIO "rollingHashFirstN" rollingHashFirstN value
        , benchIO "sum" sum value
        , benchIO "product" product value
        , benchIO "indexingWith" indexingWith value
        , benchIO "indexing" indexing value
        , benchIO "indexingRev" indexingRev value
        , benchIO "rollingMap" rollingMap value
        , benchIO "rollingMapM" rollingMapM value
        , benchIO "deleteBy" deleteBy value
        , benchIO "uniqBy" uniqBy value
        , benchIO "uniq" uniq value
        , benchIO "findIndices" findIndices value
        , benchIO "elemIndices" elemIndices value
        , benchIO "drainN" drainN value
        , benchIO "takingEndByM" takingEndByM value
        , benchIO "takingEndBy" takingEndBy value
        , benchIO "takingEndByM_" takingEndByM_ value
        , benchIO "takingEndBy_" takingEndBy_ value
        , benchIO "droppingWhileM" droppingWhileM value
        , benchIO "droppingWhile" droppingWhile value
        , benchIO "compose (sum)" compose value
        , benchIO "composeMany (take 2 sum)" composeMany value
        , benchIO "pipe-mapM" pipe value
        , benchIO "indexed" indexed value
        , benchIO "mapMaybeM" mapMaybeM value
        , benchIO "mapMaybe" mapMaybe value
        , benchIO "sampleFromthen" sampleFromthen value
        , benchIO "tee (sum, length)" tee value
        , benchIO "distribute [sum, length]" distribute value
        , benchIO "unzip (sum, length)" unzip value
        , benchIO "unzipWith (sum, length)" unzipWith value
        , benchIO "unzipWithM (sum, length)" unzipWithM value
        , benchIO "partitionByM (sum, length)" partitionByM value
        , benchIO "partitionBy (sum, length)" partitionBy value
        , benchIO "partition (sum, length)" partition value
        ]
    ++ fmap (HeapO_n,)
        [ benchIO "toListRev" toListRev value
        , benchIO "toStream" toStream value
        , benchIO "toStreamRev" toStreamRev value
        , benchIO "topBy 10" topBy value
        , benchIO "top 10" top value
        , benchIO "bottomBy 10" bottomBy value
        , benchIO "bottom 10" bottom value
        ]
