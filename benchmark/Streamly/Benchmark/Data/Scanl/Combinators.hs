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

import GHC.Types (SPEC(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (Sum(..))
import Streamly.Data.MutArray (MutArray)
import Streamly.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Scanl (Scanl)
import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Fusion.Plugin.Types
import Scanl.Type (benchIO, withStream, withPostscanl, withPostscanlMap)
import Streamly.Benchmark.Common
import Test.Tasty.Bench
import Prelude hiding (sum, product, mconcat, foldMap, unzip)

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE oddEven #-}
oddEven :: Int -> Either Int Int
oddEven x = if odd x then Left x else Right x

-------------------------------------------------------------------------------
-- Semigroups and monoids
-------------------------------------------------------------------------------

{-# ANN sconcat (PermitPatternMatches [''Int]) #-}
{-# ANN sconcat (PermitConstructions [''()]) #-}
{-# ANN sconcat (PermitTypeClasses []) #-}
{-# NOINLINE sconcat #-}
sconcat :: Int -> Int -> IO ()
sconcat n = withPostscanlMap n Sum (Scanl.sconcat (Sum 0))

#ifdef INSPECTION
inspect $ 'sconcat `hasNoType` ''Step
inspect $ 'sconcat `hasNoType` ''FL.Step
inspect $ 'sconcat `hasNoType` ''SPEC
#endif

{-# ANN mconcat (PermitPatternMatches [''Int]) #-}
{-# ANN mconcat (PermitConstructions [''()]) #-}
{-# ANN mconcat (PermitTypeClasses []) #-}
{-# NOINLINE mconcat #-}
mconcat :: Int -> Int -> IO ()
mconcat n = withPostscanlMap n Sum Scanl.mconcat

#ifdef INSPECTION
inspect $ 'mconcat `hasNoType` ''Step
inspect $ 'mconcat `hasNoType` ''FL.Step
inspect $ 'mconcat `hasNoType` ''SPEC
#endif

{-# ANN foldMap (PermitPatternMatches [''Int]) #-}
{-# ANN foldMap (PermitConstructions [''()]) #-}
{-# ANN foldMap (PermitTypeClasses []) #-}
{-# NOINLINE foldMap #-}
foldMap :: Int -> Int -> IO ()
foldMap n = withPostscanl n (Scanl.foldMap Sum)

#ifdef INSPECTION
inspect $ 'foldMap `hasNoType` ''Step
inspect $ 'foldMap `hasNoType` ''FL.Step
inspect $ 'foldMap `hasNoType` ''SPEC
#endif

{-# ANN foldMapM (PermitPatternMatches [''Int]) #-}
{-# ANN foldMapM (PermitConstructions [''()]) #-}
{-# ANN foldMapM (PermitTypeClasses []) #-}
{-# NOINLINE foldMapM #-}
foldMapM :: Int -> Int -> IO ()
foldMapM n = withPostscanl n (Scanl.foldMapM (return . Sum))

#ifdef INSPECTION
inspect $ 'foldMapM `hasNoType` ''Step
inspect $ 'foldMapM `hasNoType` ''FL.Step
inspect $ 'foldMapM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Reducers
-------------------------------------------------------------------------------

{-# ANN drainMapM (PermitPatternMatches [''Int]) #-}
{-# ANN drainMapM (PermitConstructions [''()]) #-}
{-# ANN drainMapM (PermitTypeClasses []) #-}
{-# NOINLINE drainMapM #-}
drainMapM :: Int -> Int -> IO ()
drainMapM n = withPostscanl n (Scanl.drainMapM return)

#ifdef INSPECTION
inspect $ 'drainMapM `hasNoType` ''Step
inspect $ 'drainMapM `hasNoType` ''FL.Step
inspect $ 'drainMapM `hasNoType` ''SPEC
#endif

{-# ANN the (PermitPatternMatches [''Int]) #-}
{-# ANN the (PermitConstructions [''()]) #-}
{-# ANN the (PermitTypeClasses []) #-}
{-# NOINLINE the #-}
the :: Int -> Int -> IO ()
the n = withPostscanlMap n (const (1 :: Int)) Scanl.the

#ifdef INSPECTION
inspect $ 'the `hasNoType` ''Step
inspect $ 'the `hasNoType` ''FL.Step
inspect $ 'the `hasNoType` ''SPEC
#endif

{-# ANN mean (PermitPatternMatches [''Int]) #-}
{-# ANN mean (PermitConstructions [''()]) #-}
{-# ANN mean (PermitTypeClasses []) #-}
{-# NOINLINE mean #-}
mean :: Int -> Int -> IO ()
mean n = withPostscanlMap n (fromIntegral :: Int -> Double) Scanl.mean

#ifdef INSPECTION
inspect $ 'mean `hasNoType` ''Step
inspect $ 'mean `hasNoType` ''FL.Step
inspect $ 'mean `hasNoType` ''SPEC
#endif

{-# ANN rollingHash (PermitPatternMatches [''Int]) #-}
{-# ANN rollingHash (PermitConstructions [''()]) #-}
{-# ANN rollingHash (PermitTypeClasses []) #-}
{-# NOINLINE rollingHash #-}
rollingHash :: Int -> Int -> IO ()
rollingHash n = withPostscanl n Scanl.rollingHash

#ifdef INSPECTION
inspect $ 'rollingHash `hasNoType` ''Step
inspect $ 'rollingHash `hasNoType` ''FL.Step
inspect $ 'rollingHash `hasNoType` ''SPEC
#endif

{-# ANN rollingHashWithSalt (PermitPatternMatches [''Int]) #-}
{-# ANN rollingHashWithSalt (PermitConstructions [''()]) #-}
{-# ANN rollingHashWithSalt (PermitTypeClasses []) #-}
{-# NOINLINE rollingHashWithSalt #-}
rollingHashWithSalt :: Int -> Int -> IO ()
rollingHashWithSalt n = withPostscanl n (Scanl.rollingHashWithSalt Scanl.defaultSalt)

#ifdef INSPECTION
inspect $ 'rollingHashWithSalt `hasNoType` ''Step
inspect $ 'rollingHashWithSalt `hasNoType` ''FL.Step
inspect $ 'rollingHashWithSalt `hasNoType` ''SPEC
#endif

{-# ANN rollingHashFirstN (PermitPatternMatches [''Int]) #-}
{-# ANN rollingHashFirstN (PermitConstructions [''()]) #-}
{-# ANN rollingHashFirstN (PermitTypeClasses []) #-}
{-# NOINLINE rollingHashFirstN #-}
rollingHashFirstN :: Int -> Int -> IO ()
rollingHashFirstN n = withPostscanl n (Scanl.rollingHashFirstN n)

#ifdef INSPECTION
inspect $ 'rollingHashFirstN `hasNoType` ''Step
inspect $ 'rollingHashFirstN `hasNoType` ''SPEC
#endif

{-# ANN sum (PermitPatternMatches [''Int]) #-}
{-# ANN sum (PermitConstructions [''()]) #-}
{-# ANN sum (PermitTypeClasses []) #-}
{-# NOINLINE sum #-}
sum :: Int -> Int -> IO ()
sum n = withPostscanl n Scanl.sum

#ifdef INSPECTION
inspect $ 'sum `hasNoType` ''Step
inspect $ 'sum `hasNoType` ''FL.Step
inspect $ 'sum `hasNoType` ''SPEC
#endif

{-# ANN product (PermitPatternMatches [''Int]) #-}
{-# ANN product (PermitConstructions [''()]) #-}
{-# ANN product (PermitTypeClasses []) #-}
{-# NOINLINE product #-}
product :: Int -> Int -> IO ()
product n = withPostscanl n Scanl.product

#ifdef INSPECTION
inspect $ 'product `hasNoType` ''Step
inspect $ 'product `hasNoType` ''FL.Step
inspect $ 'product `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Scanners
-------------------------------------------------------------------------------

{-# ANN indexingWith (PermitPatternMatches [''Int]) #-}
{-# ANN indexingWith (PermitConstructions [''()]) #-}
{-# ANN indexingWith (PermitTypeClasses []) #-}
{-# NOINLINE indexingWith #-}
indexingWith :: Int -> Int -> IO ()
indexingWith n = withPostscanl n (Scanl.indexingWith 0 (+ 1))

#ifdef INSPECTION
inspect $ 'indexingWith `hasNoType` ''Step
inspect $ 'indexingWith `hasNoType` ''FL.Step
inspect $ 'indexingWith `hasNoType` ''SPEC
#endif

{-# ANN indexing (PermitPatternMatches [''Int]) #-}
{-# ANN indexing (PermitConstructions [''()]) #-}
{-# ANN indexing (PermitTypeClasses []) #-}
{-# NOINLINE indexing #-}
indexing :: Int -> Int -> IO ()
indexing n = withPostscanl n Scanl.indexing

#ifdef INSPECTION
inspect $ 'indexing `hasNoType` ''Step
inspect $ 'indexing `hasNoType` ''FL.Step
inspect $ 'indexing `hasNoType` ''SPEC
#endif

{-# ANN indexingRev (PermitPatternMatches [''Int]) #-}
{-# ANN indexingRev (PermitConstructions [''()]) #-}
{-# ANN indexingRev (PermitTypeClasses []) #-}
{-# NOINLINE indexingRev #-}
indexingRev :: Int -> Int -> IO ()
indexingRev n = withPostscanl n (Scanl.indexingRev n)

#ifdef INSPECTION
inspect $ 'indexingRev `hasNoType` ''Step
inspect $ 'indexingRev `hasNoType` ''FL.Step
inspect $ 'indexingRev `hasNoType` ''SPEC
#endif

{-# ANN rollingMap (PermitPatternMatches [''Int]) #-}
{-# ANN rollingMap (PermitConstructions [''()]) #-}
{-# ANN rollingMap (PermitTypeClasses []) #-}
{-# NOINLINE rollingMap #-}
rollingMap :: Int -> Int -> IO ()
rollingMap n = withPostscanl n (Scanl.rollingMap (\_ x -> x))

#ifdef INSPECTION
inspect $ 'rollingMap `hasNoType` ''Step
inspect $ 'rollingMap `hasNoType` ''FL.Step
inspect $ 'rollingMap `hasNoType` ''SPEC
#endif

{-# ANN rollingMapM (PermitPatternMatches [''Int]) #-}
{-# ANN rollingMapM (PermitConstructions [''()]) #-}
{-# ANN rollingMapM (PermitTypeClasses []) #-}
{-# NOINLINE rollingMapM #-}
rollingMapM :: Int -> Int -> IO ()
rollingMapM n = withPostscanl n (Scanl.rollingMapM (\_ x -> return x))

#ifdef INSPECTION
inspect $ 'rollingMapM `hasNoType` ''Step
inspect $ 'rollingMapM `hasNoType` ''FL.Step
inspect $ 'rollingMapM `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filters
-------------------------------------------------------------------------------

{-# ANN deleteBy (PermitPatternMatches [''Int]) #-}
{-# ANN deleteBy (PermitConstructions [''()]) #-}
{-# ANN deleteBy (PermitTypeClasses []) #-}
{-# NOINLINE deleteBy #-}
deleteBy :: Int -> Int -> IO ()
deleteBy n = withPostscanl n (Scanl.deleteBy (==) 0)

#ifdef INSPECTION
inspect $ 'deleteBy `hasNoType` ''Step
inspect $ 'deleteBy `hasNoType` ''FL.Step
inspect $ 'deleteBy `hasNoType` ''SPEC
#endif

{-# ANN uniqBy (PermitPatternMatches [''Int]) #-}
{-# ANN uniqBy (PermitConstructions [''()]) #-}
{-# ANN uniqBy (PermitTypeClasses []) #-}
{-# NOINLINE uniqBy #-}
uniqBy :: Int -> Int -> IO ()
uniqBy n = withPostscanl n (Scanl.uniqBy (==))

#ifdef INSPECTION
inspect $ 'uniqBy `hasNoType` ''Step
inspect $ 'uniqBy `hasNoType` ''FL.Step
inspect $ 'uniqBy `hasNoType` ''SPEC
#endif

{-# ANN uniq (PermitPatternMatches [''Int]) #-}
{-# ANN uniq (PermitConstructions [''()]) #-}
{-# ANN uniq (PermitTypeClasses []) #-}
{-# NOINLINE uniq #-}
uniq :: Int -> Int -> IO ()
uniq n = withPostscanl n Scanl.uniq

#ifdef INSPECTION
inspect $ 'uniq `hasNoType` ''Step
inspect $ 'uniq `hasNoType` ''FL.Step
inspect $ 'uniq `hasNoType` ''SPEC
#endif

{-# ANN findIndices (PermitPatternMatches [''Int]) #-}
{-# ANN findIndices (PermitConstructions [''()]) #-}
{-# ANN findIndices (PermitTypeClasses []) #-}
{-# NOINLINE findIndices #-}
findIndices :: Int -> Int -> IO ()
findIndices n = withPostscanl n (Scanl.findIndices (== n))

#ifdef INSPECTION
inspect $ 'findIndices `hasNoType` ''Step
inspect $ 'findIndices `hasNoType` ''FL.Step
inspect $ 'findIndices `hasNoType` ''SPEC
#endif

{-# ANN elemIndices (PermitPatternMatches [''Int]) #-}
{-# ANN elemIndices (PermitConstructions [''()]) #-}
{-# ANN elemIndices (PermitTypeClasses []) #-}
{-# NOINLINE elemIndices #-}
elemIndices :: Int -> Int -> IO ()
elemIndices n = withPostscanl n (Scanl.elemIndices n)

#ifdef INSPECTION
inspect $ 'elemIndices `hasNoType` ''Step
inspect $ 'elemIndices `hasNoType` ''FL.Step
inspect $ 'elemIndices `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Multi-element scans
-------------------------------------------------------------------------------

{-# ANN drainN (PermitPatternMatches [''Int]) #-}
{-# ANN drainN (PermitConstructions [''()]) #-}
{-# ANN drainN (PermitTypeClasses []) #-}
{-# NOINLINE drainN #-}
drainN :: Int -> Int -> IO ()
drainN n = withPostscanl n (Scanl.drainN n)

#ifdef INSPECTION
inspect $ 'drainN `hasNoType` ''Step
inspect $ 'drainN `hasNoType` ''FL.Step
inspect $ 'drainN `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Trimmers
-------------------------------------------------------------------------------

{-# ANN takingEndByM (PermitPatternMatches [''Int]) #-}
{-# ANN takingEndByM (PermitConstructions [''()]) #-}
{-# ANN takingEndByM (PermitTypeClasses []) #-}
{-# NOINLINE takingEndByM #-}
takingEndByM :: Int -> Int -> IO ()
takingEndByM n = withPostscanl n (Scanl.takingEndByM (return . (>= n)))

#ifdef INSPECTION
inspect $ 'takingEndByM `hasNoType` ''Step
inspect $ 'takingEndByM `hasNoType` ''FL.Step
inspect $ 'takingEndByM `hasNoType` ''SPEC
#endif

{-# ANN takingEndBy (PermitPatternMatches [''Int]) #-}
{-# ANN takingEndBy (PermitConstructions [''()]) #-}
{-# ANN takingEndBy (PermitTypeClasses []) #-}
{-# NOINLINE takingEndBy #-}
takingEndBy :: Int -> Int -> IO ()
takingEndBy n = withPostscanl n (Scanl.takingEndBy (>= n))

#ifdef INSPECTION
inspect $ 'takingEndBy `hasNoType` ''Step
inspect $ 'takingEndBy `hasNoType` ''FL.Step
inspect $ 'takingEndBy `hasNoType` ''SPEC
#endif

{-# ANN takingEndByM_ (PermitPatternMatches [''Int]) #-}
{-# ANN takingEndByM_ (PermitConstructions [''()]) #-}
{-# ANN takingEndByM_ (PermitTypeClasses []) #-}
{-# NOINLINE takingEndByM_ #-}
takingEndByM_ :: Int -> Int -> IO ()
takingEndByM_ n = withPostscanl n (Scanl.takingEndByM_ (return . (>= n)))

#ifdef INSPECTION
inspect $ 'takingEndByM_ `hasNoType` ''Step
inspect $ 'takingEndByM_ `hasNoType` ''FL.Step
inspect $ 'takingEndByM_ `hasNoType` ''SPEC
#endif

{-# ANN takingEndBy_ (PermitPatternMatches [''Int]) #-}
{-# ANN takingEndBy_ (PermitConstructions [''()]) #-}
{-# ANN takingEndBy_ (PermitTypeClasses []) #-}
{-# NOINLINE takingEndBy_ #-}
takingEndBy_ :: Int -> Int -> IO ()
takingEndBy_ n = withPostscanl n (Scanl.takingEndBy_ (>= n))

#ifdef INSPECTION
inspect $ 'takingEndBy_ `hasNoType` ''Step
inspect $ 'takingEndBy_ `hasNoType` ''FL.Step
inspect $ 'takingEndBy_ `hasNoType` ''SPEC
#endif

{-# ANN droppingWhileM (PermitPatternMatches [''Int]) #-}
{-# ANN droppingWhileM (PermitConstructions [''()]) #-}
{-# ANN droppingWhileM (PermitTypeClasses []) #-}
{-# NOINLINE droppingWhileM #-}
droppingWhileM :: Int -> Int -> IO ()
droppingWhileM n = withPostscanl n (Scanl.droppingWhileM (return . (<= n)))

#ifdef INSPECTION
inspect $ 'droppingWhileM `hasNoType` ''Step
inspect $ 'droppingWhileM `hasNoType` ''FL.Step
inspect $ 'droppingWhileM `hasNoType` ''SPEC
#endif

{-# ANN droppingWhile (PermitPatternMatches [''Int]) #-}
{-# ANN droppingWhile (PermitConstructions [''()]) #-}
{-# ANN droppingWhile (PermitTypeClasses []) #-}
{-# NOINLINE droppingWhile #-}
droppingWhile :: Int -> Int -> IO ()
droppingWhile n = withPostscanl n (Scanl.droppingWhile (<= n))

#ifdef INSPECTION
inspect $ 'droppingWhile `hasNoType` ''Step
inspect $ 'droppingWhile `hasNoType` ''FL.Step
inspect $ 'droppingWhile `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Scanning input
-------------------------------------------------------------------------------

{-# ANN compose (PermitPatternMatches [''Int]) #-}
{-# ANN compose (PermitConstructions [''()]) #-}
{-# ANN compose (PermitTypeClasses []) #-}
{-# NOINLINE compose #-}
compose :: Int -> Int -> IO ()
compose n = withPostscanl n (Scanl.compose Scanl.sum Scanl.drain)

#ifdef INSPECTION
inspect $ 'compose `hasNoType` ''Step
inspect $ 'compose `hasNoType` ''SPEC
#endif

{-# ANN composeMany (PermitPatternMatches [''Int]) #-}
{-# ANN composeMany (PermitConstructions [''()]) #-}
{-# ANN composeMany (PermitTypeClasses []) #-}
{-# NOINLINE composeMany #-}
composeMany :: Int -> Int -> IO ()
composeMany n = withPostscanl n (Scanl.composeMany (Scanl.take 2 Scanl.sum) Scanl.drain)

#ifdef INSPECTION
inspect $ 'composeMany `hasNoType` ''Step
inspect $ 'composeMany `hasNoType` ''SPEC
#endif

{-# ANN pipe (PermitPatternMatches [''Int]) #-}
{-# ANN pipe (PermitConstructions [''()]) #-}
{-# ANN pipe (PermitTypeClasses []) #-}
{-# NOINLINE pipe #-}
pipe :: Int -> Int -> IO ()
pipe n = withPostscanl n (Scanl.pipe (Pipe.mapM (\x -> return (x + 1))) Scanl.drain)

#ifdef INSPECTION
inspect $ 'pipe `hasNoType` ''Step
inspect $ 'pipe `hasNoType` ''FL.Step
inspect $ 'pipe `hasNoType` ''SPEC
#endif

{-# ANN indexed (PermitPatternMatches [''Int]) #-}
{-# ANN indexed (PermitConstructions [''()]) #-}
{-# ANN indexed (PermitTypeClasses []) #-}
{-# NOINLINE indexed #-}
indexed :: Int -> Int -> IO ()
indexed n = withPostscanl n (Scanl.indexed Scanl.length)

#ifdef INSPECTION
inspect $ 'indexed `hasNoType` ''Step
inspect $ 'indexed `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Filtering input
-------------------------------------------------------------------------------

{-# ANN mapMaybeM (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybeM (PermitConstructions [''()]) #-}
{-# ANN mapMaybeM (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybeM #-}
mapMaybeM :: Int -> Int -> IO ()
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

{-# ANN mapMaybe (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybe (PermitConstructions [''()]) #-}
{-# ANN mapMaybe (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybe #-}
mapMaybe :: Int -> Int -> IO ()
mapMaybe n =
    withPostscanl n
        (Scanl.mapMaybe (\x -> if even x then Just x else Nothing) Scanl.drain)

#ifdef INSPECTION
inspect $ 'mapMaybe `hasNoType` ''Step
inspect $ 'mapMaybe `hasNoType` ''FL.Step
inspect $ 'mapMaybe `hasNoType` ''SPEC
#endif

{-# ANN sampleFromthen (PermitPatternMatches [''Int]) #-}
{-# ANN sampleFromthen (PermitConstructions [''()]) #-}
{-# ANN sampleFromthen (PermitTypeClasses []) #-}
{-# NOINLINE sampleFromthen #-}
sampleFromthen :: Int -> Int -> IO ()
sampleFromthen n = withPostscanl n (Scanl.sampleFromthen 0 2 Scanl.drain)

#ifdef INSPECTION
inspect $ 'sampleFromthen `hasNoType` ''Step
inspect $ 'sampleFromthen `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Parallel distribution
-------------------------------------------------------------------------------

{-# ANN tee (PermitPatternMatches [''Int]) #-}
{-# ANN tee (PermitConstructions [''()]) #-}
{-# ANN tee (PermitTypeClasses []) #-}
{-# NOINLINE tee #-}
tee :: Int -> Int -> IO ()
tee n = withPostscanl n (Scanl.tee Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'tee `hasNoType` ''Step
inspect $ 'tee `hasNoType` ''FL.Step
inspect $ 'tee `hasNoType` ''SPEC
#endif

{-# ANN distribute (PermitPatternMatches [''Int]) #-}
{-# ANN distribute (PermitConstructions [''()]) #-}
{-# ANN distribute (PermitTypeClasses []) #-}
{-# NOINLINE distribute #-}
distribute :: Int -> Int -> IO ()
distribute n = withPostscanl n (Scanl.distribute [Scanl.sum, Scanl.length])

-------------------------------------------------------------------------------
-- Unzipping
-------------------------------------------------------------------------------

{-# ANN unzip (PermitPatternMatches [''Int]) #-}
{-# ANN unzip (PermitConstructions [''()]) #-}
{-# ANN unzip (PermitTypeClasses []) #-}
{-# NOINLINE unzip #-}
unzip :: Int -> Int -> IO ()
unzip n = withPostscanlMap n (\a -> (a, a)) (Scanl.unzip Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'unzip `hasNoType` ''Step
inspect $ 'unzip `hasNoType` ''FL.Step
inspect $ 'unzip `hasNoType` ''SPEC
#endif

{-# ANN unzipWith (PermitPatternMatches [''Int]) #-}
{-# ANN unzipWith (PermitConstructions [''()]) #-}
{-# ANN unzipWith (PermitTypeClasses []) #-}
{-# NOINLINE unzipWith #-}
unzipWith :: Int -> Int -> IO ()
unzipWith n = withPostscanl n (Scanl.unzipWith (\a -> (a, a)) Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'unzipWith `hasNoType` ''Step
inspect $ 'unzipWith `hasNoType` ''FL.Step
inspect $ 'unzipWith `hasNoType` ''SPEC
#endif

{-# ANN unzipWithM (PermitPatternMatches [''Int]) #-}
{-# ANN unzipWithM (PermitConstructions [''()]) #-}
{-# ANN unzipWithM (PermitTypeClasses []) #-}
{-# NOINLINE unzipWithM #-}
unzipWithM :: Int -> Int -> IO ()
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

{-# ANN partitionByM (PermitPatternMatches [''Int]) #-}
{-# ANN partitionByM (PermitConstructions [''()]) #-}
{-# ANN partitionByM (PermitTypeClasses []) #-}
{-# NOINLINE partitionByM #-}
partitionByM :: Int -> Int -> IO ()
partitionByM n =
    withPostscanl n (Scanl.partitionByM (return . oddEven) Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'partitionByM `hasNoType` ''Step
inspect $ 'partitionByM `hasNoType` ''FL.Step
inspect $ 'partitionByM `hasNoType` ''SPEC
#endif

{-# ANN partitionBy (PermitPatternMatches [''Int]) #-}
{-# ANN partitionBy (PermitConstructions [''()]) #-}
{-# ANN partitionBy (PermitTypeClasses []) #-}
{-# NOINLINE partitionBy #-}
partitionBy :: Int -> Int -> IO ()
partitionBy n = withPostscanl n (Scanl.partitionBy oddEven Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'partitionBy `hasNoType` ''Step
inspect $ 'partitionBy `hasNoType` ''FL.Step
inspect $ 'partitionBy `hasNoType` ''SPEC
#endif

{-# ANN partition (PermitPatternMatches [''Int]) #-}
{-# ANN partition (PermitConstructions [''()]) #-}
{-# ANN partition (PermitTypeClasses []) #-}
{-# NOINLINE partition #-}
partition :: Int -> Int -> IO ()
partition n = withPostscanlMap n oddEven (Scanl.partition Scanl.sum Scanl.length)

#ifdef INSPECTION
inspect $ 'partition `hasNoType` ''Step
inspect $ 'partition `hasNoType` ''FL.Step
inspect $ 'partition `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- O(n) heap: building structures
-------------------------------------------------------------------------------

{-# ANN toListRev (PermitPatternMatches [''SPEC,''Int,''Stream.ScanState]) #-}
{-# ANN toListRev (PermitConstructions [''SPEC,''Int,''Stream.ScanState,''(),''[]]) #-}
{-# ANN toListRev (PermitTypeClasses []) #-}
{-# NOINLINE toListRev #-}
toListRev :: Int -> Int -> IO ()
toListRev n = withPostscanl n Scanl.toListRev

{-# ANN toStream (PermitPatternMatches [''Int]) #-}
{-# ANN toStream (PermitConstructions [''Int,''[],''()]) #-}
{-# ANN toStream (PermitTypeClasses []) #-}
{-# NOINLINE toStream #-}
toStream :: Int -> Int -> IO ()
toStream n =
    withStream n
        $ Stream.fold FL.drain
        . Stream.postscanl (Scanl.toStream :: Scanl IO Int (Stream IO Int))

{-# ANN toStreamRev (PermitPatternMatches [''SPEC,''Int,''Stream.ScanState]) #-}
{-# ANN toStreamRev (PermitConstructions [''SPEC,''Int,''Stream.ScanState,''(),''[]]) #-}
{-# ANN toStreamRev (PermitTypeClasses []) #-}
{-# NOINLINE toStreamRev #-}
toStreamRev :: Int -> Int -> IO ()
toStreamRev n =
    withStream n
        $ Stream.fold FL.drain
        . Stream.postscanl (Scanl.toStreamRev :: Scanl IO Int (Stream IO Int))

{-# ANN topBy (PermitPatternMatches [''Int,''MutArray]) #-}
{-# ANN topBy (PermitConstructions [''MutArray,''Int,''()]) #-}
{-# ANN topBy (PermitTypeClasses [''MonadIO,''Unbox]) #-}
{-# NOINLINE topBy #-}
topBy :: Int -> Int -> IO ()
topBy n = withPostscanl n (Scanl.topBy compare 10)

{-# ANN top (PermitPatternMatches [''Int,''MutArray]) #-}
{-# ANN top (PermitConstructions [''MutArray,''Int,''()]) #-}
{-# ANN top (PermitTypeClasses [''MonadIO,''Unbox]) #-}
{-# NOINLINE top #-}
top :: Int -> Int -> IO ()
top n = withPostscanl n (Scanl.top 10)

{-# ANN bottomBy (PermitPatternMatches [''Int,''MutArray]) #-}
{-# ANN bottomBy (PermitConstructions [''MutArray,''Int,''()]) #-}
{-# ANN bottomBy (PermitTypeClasses [''MonadIO,''Unbox]) #-}
{-# NOINLINE bottomBy #-}
bottomBy :: Int -> Int -> IO ()
bottomBy n = withPostscanl n (Scanl.bottomBy compare 10)

{-# ANN bottom (PermitPatternMatches [''Int,''MutArray]) #-}
{-# ANN bottom (PermitConstructions [''MutArray,''Int,''()]) #-}
{-# ANN bottom (PermitTypeClasses [''MonadIO,''Unbox]) #-}
{-# NOINLINE bottom #-}
bottom :: Int -> Int -> IO ()
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
