-- |
-- Module      : Unfold.Type
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Unfold.Type (benchmarks) where

import Control.DeepSeq (NFData(..))
import Control.Exception (ErrorCall, try)
import qualified Data.Tuple as Tuple
import Streamly.Internal.Data.Unfold (Unfold)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF

import Test.Tasty.Bench hiding (env)
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
import Streamly.Benchmark.Common

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
import qualified Streamly.Internal.Data.Stream as S

import qualified Streamly.Internal.Data.Producer as Producer
#endif

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- generate numbers up to the argument value
{-# INLINE source #-}
source :: Monad m => Int -> Unfold m Int Int
source n = UF.supplySecond n UF.enumerateFromToIntegral

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE drainGeneration #-}
drainGeneration :: Monad m => Unfold m a b -> a -> m ()
drainGeneration = UF.fold FL.drain

{-# INLINE drainTransformation #-}
drainTransformation ::
       Monad m => Unfold m a b -> (Unfold m a b -> Unfold m c d) -> c -> m ()
drainTransformation unf f = drainGeneration (f unf)

{-# INLINE drainTransformationDefault #-}
drainTransformationDefault ::
       Monad m => Int -> (Unfold m Int Int -> Unfold m c d) -> c -> m ()
drainTransformationDefault to =
    drainTransformation (UF.supplySecond to UF.enumerateFromToIntegral)

{-# INLINE drainProduct #-}
drainProduct ::
       Monad m
    => Unfold m a b
    -> Unfold m c d
    -> (Unfold m a b -> Unfold m c d -> Unfold m e f)
    -> e
    -> m ()
drainProduct unf1 unf2 f = drainGeneration (f unf1 unf2)

{-# INLINE drainProductDefault #-}
drainProductDefault ::
       Monad m
    => Int
    -> (Unfold m Int Int -> Unfold m Int Int -> Unfold m e f)
    -> e
    -> m ()
drainProductDefault to = drainProduct src src

    where

    src = UF.supplySecond to UF.enumerateFromToIntegral

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

{-# INLINE lmap #-}
lmap :: Int -> Int -> IO ()
lmap size start =
    drainTransformationDefault (size + start) (UF.lmap (+ 1)) start

#ifdef INSPECTION
inspect $ 'lmap `hasNoType` ''S.Step
inspect $ 'lmap `hasNoType` ''FL.Step
inspect $ 'lmap `hasNoType` ''SPEC
#endif

{-# INLINE lmapM #-}
lmapM :: Int -> Int -> IO ()
lmapM size start =
    drainTransformationDefault (size + start) (UF.lmapM (return . (+) 1)) start

#ifdef INSPECTION
inspect $ 'lmapM `hasNoType` ''S.Step
inspect $ 'lmapM `hasNoType` ''FL.Step
inspect $ 'lmapM `hasNoType` ''SPEC
#endif

{-# INLINE both #-}
both :: Int -> Int -> IO ()
both size start =
    drainTransformationDefault (size + start) (UF.supply start) ()

#ifdef INSPECTION
inspect $ 'both `hasNoType` ''S.Step
inspect $ 'both `hasNoType` ''FL.Step
inspect $ 'both `hasNoType` ''SPEC
#endif

{-# INLINE first #-}
first :: Int -> Int -> IO ()
first size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.supplyFirst start)
        1

#ifdef INSPECTION
inspect $ 'first `hasNoType` ''S.Step
inspect $ 'first `hasNoType` ''FL.Step
inspect $ 'first `hasNoType` ''SPEC
#endif

{-# INLINE second #-}
second :: Int -> Int -> IO ()
second size =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.supplySecond 1)

#ifdef INSPECTION
inspect $ 'second `hasNoType` ''S.Step
inspect $ 'second `hasNoType` ''FL.Step
inspect $ 'second `hasNoType` ''SPEC
#endif

{-# INLINE consInput #-}
consInput :: Int -> Int -> IO ()
consInput size start =
    drainTransformationDefault (size + start) UF.consInput start

#ifdef INSPECTION
inspect $ 'consInput `hasNoType` ''S.Step
inspect $ 'consInput `hasNoType` ''FL.Step
inspect $ 'consInput `hasNoType` ''SPEC
inspect $ 'consInput `hasNoType` ''UF.ConsInputState
#endif

{-# INLINE consInputWith #-}
consInputWith :: Int -> Int -> IO ()
consInputWith size start =
    drainTransformationDefault (size + start) (UF.consInputWith (+1)) start

#ifdef INSPECTION
inspect $ 'consInputWith `hasNoType` ''S.Step
inspect $ 'consInputWith `hasNoType` ''FL.Step
inspect $ 'consInputWith `hasNoType` ''SPEC
inspect $ 'consInputWith `hasNoType` ''UF.ConsInputState
#endif

{-# INLINE swap #-}
swap :: Int -> Int -> IO ()
swap size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.lmap Tuple.swap)
        (1, start)

#ifdef INSPECTION
inspect $ 'swap `hasNoType` ''S.Step
inspect $ 'swap `hasNoType` ''FL.Step
inspect $ 'swap `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

-- 'functionM', 'function', 'identity' and 'fromEffect' generate a single
-- element per seed, so to process ~value elements we unfold them over an outer
-- source of value seeds.
{-# INLINE functionM #-}
functionM :: Int -> Int -> IO ()
functionM value start =
    drainGeneration
        (UF.unfoldEach (UF.functionM return) (source (start + value))) start

#ifdef INSPECTION
inspect $ 'functionM `hasNoType` ''S.Step
inspect $ 'functionM `hasNoType` ''FL.Step
inspect $ 'functionM `hasNoType` ''SPEC
inspect $ 'functionM `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE function #-}
function :: Int -> Int -> IO ()
function value start =
    drainGeneration
        (UF.unfoldEach (UF.function id) (source (start + value))) start

#ifdef INSPECTION
inspect $ 'function `hasNoType` ''S.Step
inspect $ 'function `hasNoType` ''FL.Step
inspect $ 'function `hasNoType` ''SPEC
inspect $ 'function `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE identity #-}
identity :: Int -> Int -> IO ()
identity value start =
    drainGeneration (UF.unfoldEach UF.identity (source (start + value))) start

#ifdef INSPECTION
inspect $ 'identity `hasNoType` ''S.Step
inspect $ 'identity `hasNoType` ''FL.Step
inspect $ 'identity `hasNoType` ''SPEC
inspect $ 'identity `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE fromEffect #-}
fromEffect :: Int -> Int -> IO ()
fromEffect value start =
    drainGeneration
        (UF.unfoldEach (UF.fromEffect (return start)) (source (start + value)))
        start

#ifdef INSPECTION
inspect $ 'fromEffect `hasNoType` ''S.Step
inspect $ 'fromEffect `hasNoType` ''FL.Step
inspect $ 'fromEffect `hasNoType` ''SPEC
inspect $ 'fromEffect `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE fromPure #-}
fromPure :: Int -> Int -> IO ()
fromPure value start =
    drainGeneration
        (UF.unfoldEach (UF.fromPure start) (source (start + value)))
        start

#ifdef INSPECTION
inspect $ 'fromPure `hasNoType` ''S.Step
inspect $ 'fromPure `hasNoType` ''FL.Step
inspect $ 'fromPure `hasNoType` ''SPEC
inspect $ 'fromPure `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE functionMaybeM #-}
functionMaybeM :: Int -> Int -> IO ()
functionMaybeM value start =
    drainGeneration
        (UF.unfoldEach (UF.functionMaybeM (return . Just)) (source (start + value)))
        start

#ifdef INSPECTION
inspect $ 'functionMaybeM `hasNoType` ''S.Step
inspect $ 'functionMaybeM `hasNoType` ''FL.Step
inspect $ 'functionMaybeM `hasNoType` ''SPEC
inspect $ 'functionMaybeM `hasNoType` ''Producer.ConcatState
#endif

-- 'fromTuple' generates two elements per seed, so unfold it over value/2 tuples
-- to emit and drain ~value elements.
{-# INLINE fromTuple #-}
fromTuple :: Int -> Int -> IO ()
fromTuple value start =
    let outer = UF.map (\i -> (i, i)) (source (start + value `div` 2))
     in drainGeneration (UF.unfoldEach UF.fromTuple outer) start

#ifdef INSPECTION
inspect $ 'fromTuple `hasNoType` ''S.Step
inspect $ 'fromTuple `hasNoType` ''FL.Step
inspect $ 'fromTuple `hasNoType` ''SPEC
inspect $ 'fromTuple `hasNoType` ''Producer.ConcatState
inspect $ 'fromTuple `hasNoType` ''Producer.TupleState
#endif

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Unfold m Int Int
sourceUnfoldrM size start = UF.unfoldrM step

    where

    step i =
        return
            $ if i < start + size
              then Just (i, i + 1)
              else Nothing

{-# INLINE unfoldrM #-}
unfoldrM :: Int -> Int -> IO ()
unfoldrM size start = drainGeneration (sourceUnfoldrM size start) start

#ifdef INSPECTION
inspect $ 'unfoldrM `hasNoType` ''S.Step
inspect $ 'unfoldrM `hasNoType` ''FL.Step
inspect $ 'unfoldrM `hasNoType` ''SPEC
#endif

{-# INLINE unfoldr #-}
unfoldr :: Int -> Int -> IO ()
unfoldr size start = drainGeneration (UF.unfoldr step) start
    where
    step i = if i < start + size then Just (i, i + 1) else Nothing

#ifdef INSPECTION
inspect $ 'unfoldr `hasNoType` ''S.Step
inspect $ 'unfoldr `hasNoType` ''FL.Step
inspect $ 'unfoldr `hasNoType` ''SPEC
#endif

{-# INLINE fromList #-}
fromList :: Int -> Int -> IO ()
fromList size start = drainGeneration UF.fromList [start .. start + size]

#ifdef INSPECTION
inspect $ 'fromList `hasNoType` ''S.Step
inspect $ 'fromList `hasNoType` ''FL.Step
inspect $ 'fromList `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE map #-}
map :: Int -> Int -> IO ()
map size start = drainTransformationDefault (size + start) (UF.map (+1)) start

#ifdef INSPECTION
inspect $ 'map `hasNoType` ''S.Step
inspect $ 'map `hasNoType` ''FL.Step
inspect $ 'map `hasNoType` ''SPEC
#endif

{-# INLINE mapM #-}
mapM :: Int -> Int -> IO ()
mapM size start =
    drainTransformationDefault (size + start) (UF.mapM (return . (+) 1)) start

#ifdef INSPECTION
inspect $ 'mapM `hasNoType` ''S.Step
inspect $ 'mapM `hasNoType` ''FL.Step
inspect $ 'mapM `hasNoType` ''SPEC
#endif

{-# INLINE mapM2 #-}
mapM2 :: Int -> Int -> IO ()
mapM2 size =
    drainTransformationDefault
        size
        (UF.mapM (\(a, b) -> return $ a + b) . UF.carryInput)

#ifdef INSPECTION
inspect $ 'mapM2 `hasNoType` ''S.Step
inspect $ 'mapM2 `hasNoType` ''FL.Step
inspect $ 'mapM2 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream filtering
-------------------------------------------------------------------------------

{-# INLINE takeWhileM #-}
takeWhileM :: Int -> Int -> IO ()
takeWhileM size start =
    drainTransformationDefault
        size
        (UF.takeWhileM (\b -> return (b <= size + start)))
        start

#ifdef INSPECTION
inspect $ 'takeWhileM `hasNoType` ''S.Step
inspect $ 'takeWhileM `hasNoType` ''FL.Step
inspect $ 'takeWhileM `hasNoType` ''SPEC
#endif

{-# INLINE takeWhile #-}
takeWhile :: Int -> Int -> IO ()
takeWhile size start =
    drainTransformationDefault
        size
        (UF.takeWhile (\b -> b <= size + start))
        start

#ifdef INSPECTION
inspect $ 'takeWhile `hasNoType` ''S.Step
inspect $ 'takeWhile `hasNoType` ''FL.Step
inspect $ 'takeWhile `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: Int -> Int -> IO ()
zipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) start

#ifdef INSPECTION
inspect $ 'zipWith `hasNoType` ''S.Step
inspect $ 'zipWith `hasNoType` ''FL.Step
inspect $ 'zipWith `hasNoType` ''SPEC
#endif

{-# INLINE zipWithM #-}
zipWithM :: Int -> Int -> IO ()
zipWithM size start =
    drainProductDefault
        (size + start)
        (UF.zipWithM (\a b -> return $ a + b))
        start

#ifdef INSPECTION
inspect $ 'zipWithM `hasNoType` ''S.Step
inspect $ 'zipWithM `hasNoType` ''FL.Step
inspect $ 'zipWithM `hasNoType` ''SPEC
#endif

{-# INLINE teeZipWith #-}
teeZipWith :: Int -> Int -> IO ()
teeZipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) start

#ifdef INSPECTION
inspect $ 'teeZipWith `hasNoType` ''S.Step
inspect $ 'teeZipWith `hasNoType` ''FL.Step
inspect $ 'teeZipWith `hasNoType` ''SPEC
#endif

{-# INLINE interleave #-}
interleave :: Int -> Int -> IO ()
interleave size start =
    drainProductDefault (size + start) UF.interleave (start, start)

#ifdef INSPECTION
inspect $ 'interleave `hasNoType` ''S.Step
inspect $ 'interleave `hasNoType` ''FL.Step
inspect $ 'interleave `hasNoType` ''SPEC
inspect $ 'interleave `hasNoType` ''Producer.InterleaveState
#endif

{-# INLINE zipArrowWithM #-}
zipArrowWithM :: Int -> Int -> IO ()
zipArrowWithM size start =
    drainProductDefault
        (size + start)
        (UF.zipArrowWithM (\a b -> return (a + b)))
        (start, start)

#ifdef INSPECTION
inspect $ 'zipArrowWithM `hasNoType` ''S.Step
inspect $ 'zipArrowWithM `hasNoType` ''FL.Step
inspect $ 'zipArrowWithM `hasNoType` ''SPEC
#endif

{-# INLINE zipArrowWith #-}
zipArrowWith :: Int -> Int -> IO ()
zipArrowWith size start =
    drainProductDefault (size + start) (UF.zipArrowWith (+)) (start, start)

#ifdef INSPECTION
inspect $ 'zipArrowWith `hasNoType` ''S.Step
inspect $ 'zipArrowWith `hasNoType` ''FL.Step
inspect $ 'zipArrowWith `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

nthRoot :: Double -> Int -> Int
nthRoot n value = round (fromIntegral value**(1/n))

{-# INLINE toNullAp #-}
toNullAp :: Int -> Int -> IO ()
toNullAp value start =
    let end = start + nthRoot 2 value
        s = source end
    -- in UF.fold ((+) <$> s <*> s) FL.drain start
    in UF.fold FL.drain (((+) `fmap` s) `UF.crossApply` s) start

#ifdef INSPECTION
inspect $ 'toNullAp `hasNoType` ''S.Step
inspect $ 'toNullAp `hasNoType` ''FL.Step
inspect $ 'toNullAp `hasNoType` ''SPEC
inspect $ 'toNullAp `hasNoType` ''Producer.CrossApplyState
#endif

{-# INLINE crossApplyFst #-}
crossApplyFst :: Int -> Int -> IO ()
crossApplyFst value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.crossApplyFst` s) start

#ifdef INSPECTION
inspect $ 'crossApplyFst `hasNoType` ''S.Step
inspect $ 'crossApplyFst `hasNoType` ''FL.Step
inspect $ 'crossApplyFst `hasNoType` ''SPEC
inspect $ 'crossApplyFst `hasNoType` ''Producer.CrossApplyFstState
#endif

{-# INLINE crossApplySnd #-}
crossApplySnd :: Int -> Int -> IO ()
crossApplySnd value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.crossApplySnd` s) start

#ifdef INSPECTION
inspect $ 'crossApplySnd `hasNoType` ''S.Step
inspect $ 'crossApplySnd `hasNoType` ''FL.Step
inspect $ 'crossApplySnd `hasNoType` ''SPEC
inspect $ 'crossApplySnd `hasNoType` ''Producer.CrossApplyState
#endif

{-# INLINE cross #-}
cross :: Int -> Int -> IO ()
cross value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.cross` s) start

#ifdef INSPECTION
inspect $ 'cross `hasNoType` ''S.Step
inspect $ 'cross `hasNoType` ''FL.Step
inspect $ 'cross `hasNoType` ''SPEC
inspect $ 'cross `hasNoType` ''Producer.CrossState
#endif

{-# INLINE fairCross #-}
fairCross :: Int -> Int -> IO ()
fairCross value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.fairCross` s) start

#ifdef INSPECTION
inspect $ 'fairCross `hasNoType` ''S.Step
inspect $ 'fairCross `hasNoType` ''FL.Step
inspect $ 'fairCross `hasNoType` ''SPEC
inspect $ 'fairCross `hasNoType` ''Producer.FairCrossState
#endif

{-# INLINE crossApply #-}
crossApply :: Int -> Int -> IO ()
crossApply value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.crossApply (UF.map (+) s) s) start

#ifdef INSPECTION
inspect $ 'crossApply `hasNoType` ''S.Step
inspect $ 'crossApply `hasNoType` ''FL.Step
inspect $ 'crossApply `hasNoType` ''SPEC
inspect $ 'crossApply `hasNoType` ''Producer.CrossApplyState
#endif

{-# INLINE crossWithM #-}
crossWithM :: Int -> Int -> IO ()
crossWithM value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.crossWithM (\a b -> return (a + b)) s s) start

#ifdef INSPECTION
inspect $ 'crossWithM `hasNoType` ''S.Step
inspect $ 'crossWithM `hasNoType` ''FL.Step
inspect $ 'crossWithM `hasNoType` ''SPEC
inspect $ 'crossWithM `hasNoType` ''Producer.CrossState
#endif

{-# INLINE crossWith #-}
crossWith :: Int -> Int -> IO ()
crossWith value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.crossWith (+) s s) start

#ifdef INSPECTION
inspect $ 'crossWith `hasNoType` ''S.Step
inspect $ 'crossWith `hasNoType` ''FL.Step
inspect $ 'crossWith `hasNoType` ''SPEC
inspect $ 'crossWith `hasNoType` ''Producer.CrossState
#endif

{-# INLINE fairCrossWithM #-}
fairCrossWithM :: Int -> Int -> IO ()
fairCrossWithM value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.fairCrossWithM (\a b -> return (a + b)) s s) start

#ifdef INSPECTION
inspect $ 'fairCrossWithM `hasNoType` ''S.Step
inspect $ 'fairCrossWithM `hasNoType` ''FL.Step
inspect $ 'fairCrossWithM `hasNoType` ''SPEC
inspect $ 'fairCrossWithM `hasNoType` ''Producer.FairCrossState
#endif

{-# INLINE fairCrossWith #-}
fairCrossWith :: Int -> Int -> IO ()
fairCrossWith value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.fairCrossWith (+) s s) start

#ifdef INSPECTION
inspect $ 'fairCrossWith `hasNoType` ''S.Step
inspect $ 'fairCrossWith `hasNoType` ''FL.Step
inspect $ 'fairCrossWith `hasNoType` ''SPEC
inspect $ 'fairCrossWith `hasNoType` ''Producer.FairCrossState
#endif

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- XXX to keep the benchmarks same as Stream we should use sourceUnfoldrM in
-- all of these, and other benchmarks too.
{-# INLINE concatMapM #-}
concatMapM :: Int -> Int -> Int -> IO ()
concatMapM inner outer start =
    drainGeneration (UF.concatMapM unfoldInGen unfoldOut) start

    where

    unfoldInGen i = return (UF.supplySecond (i + inner) UF.enumerateFromToIntegral)
    unfoldOut = UF.supplySecond (start + outer) UF.enumerateFromToIntegral

-- The 'bind'-based benchmarks use the Unfold monad ('UF.bind'), which is a
-- concatMap and does not fuse, so the 'Step' constructors remain.
#ifdef INSPECTION
-- inspect $ 'concatMapM `hasNoType` ''S.Step
inspect $ 'concatMapM `hasNoType` ''FL.Step
inspect $ 'concatMapM `hasNoType` ''SPEC
#endif

{-# INLINE toNull #-}
toNull :: Int -> Int -> IO ()
toNull value start =
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            return (x + y)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
                UF.fromPure (x + y)
     in UF.fold FL.drain u start

#ifdef INSPECTION
-- inspect $ 'toNull `hasNoType` ''S.Step
inspect $ 'toNull `hasNoType` ''FL.Step
inspect $ 'toNull `hasNoType` ''SPEC
#endif

{-# INLINE toNull3 #-}
toNull3 :: Int -> Int -> IO ()
toNull3 value start =
    let end = start + nthRoot 3 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            z <- src
            return (x + y + z)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            src `UF.bind` \z ->
                UF.fromPure (x + y + z)
     in UF.fold FL.drain u start

#ifdef INSPECTION
-- inspect $ 'toNull3 `hasNoType` ''S.Step
inspect $ 'toNull3 `hasNoType` ''FL.Step
inspect $ 'toNull3 `hasNoType` ''SPEC
#endif

{-# INLINE toNullConcatMap #-}
toNullConcatMap :: Int -> Int -> IO ()
toNullConcatMap value start =
    let end = start + nthRoot 2 value
        src = source end
        u = UF.concatMap (\x ->
            UF.concatMap (\y ->
                UF.fromPure (x + y)) src) src
     in UF.fold FL.drain u start

{-# INLINE toNull3ConcatMap #-}
toNull3ConcatMap :: Int -> Int -> IO ()
toNull3ConcatMap value start =
    let end = start + nthRoot 3 value
        src = source end
        u = UF.concatMap (\x ->
            UF.concatMap (\y ->
            UF.concatMap (\z ->
                UF.fromPure (x + y + z)) src) src) src
     in UF.fold FL.drain u start

{-# INLINE toList #-}
toList :: Int -> Int -> IO [Int]
toList value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            return (x + y)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
                UF.fromPure (x + y)
     in UF.fold FL.toList u start

#ifdef INSPECTION
-- inspect $ 'toList `hasNoType` ''S.Step
inspect $ 'toList `hasNoType` ''FL.Step
inspect $ 'toList `hasNoType` ''SPEC
#endif

{-# INLINE toListSome #-}
toListSome :: Int -> Int -> IO [Int]
toListSome value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
            return (x + y)
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
                UF.fromPure (x + y)
     in UF.fold FL.toList (UF.take 1000 u) start

#ifdef INSPECTION
-- inspect $ 'toListSome `hasNoType` ''S.Step
inspect $ 'toListSome `hasNoType` ''FL.Step
inspect $ 'toListSome `hasNoType` ''SPEC
#endif

{-# INLINE filterAllOut #-}
filterAllOut :: Int -> Int -> IO ()
filterAllOut value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s < 0
                then UF.fromPure s
                else UF.nilM (return . const ())
     in UF.fold FL.drain u start

#ifdef INSPECTION
-- inspect $ 'filterAllOut `hasNoType` ''S.Step
inspect $ 'filterAllOut `hasNoType` ''FL.Step
inspect $ 'filterAllOut `hasNoType` ''SPEC
#endif

{-# INLINE filterAllIn #-}
filterAllIn :: Int -> Int -> IO ()
filterAllIn value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s > 0
                then UF.fromPure s
                else UF.nilM (return . const ())
     in UF.fold FL.drain u start

#ifdef INSPECTION
-- inspect $ 'filterAllIn `hasNoType` ''S.Step
inspect $ 'filterAllIn `hasNoType` ''FL.Step
inspect $ 'filterAllIn `hasNoType` ''SPEC
#endif

{-# INLINE filterSome #-}
filterSome :: Int -> Int -> IO ()
filterSome value start = do
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s > 1100000
                then UF.fromPure s
                else UF.nilM (return . const ())
     in UF.fold FL.drain u start

#ifdef INSPECTION
-- inspect $ 'filterSome `hasNoType` ''S.Step
inspect $ 'filterSome `hasNoType` ''FL.Step
inspect $ 'filterSome `hasNoType` ''SPEC
#endif

{-# INLINE breakAfterSome #-}
breakAfterSome :: Int -> Int -> IO ()
breakAfterSome value start =
    let end = start + nthRoot 2 value
        src = source end
        {-
        u = do
            x <- src
            y <- src
        -}
        u = src `UF.bind` \x ->
            src `UF.bind` \y ->
            let s = x + y
             in if s > 1100000
                then error "break"
                else UF.fromPure s
     in do
        (_ :: Either ErrorCall ()) <- try $ UF.fold FL.drain u start
        return ()

#ifdef INSPECTION
-- inspect $ 'breakAfterSome `hasNoType` ''S.Step
inspect $ 'breakAfterSome `hasNoType` ''FL.Step
inspect $ 'breakAfterSome `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE unfoldEach #-}
unfoldEach :: Int -> Int -> Int -> IO ()
unfoldEach inner outer start = do
    UF.fold
        FL.drain
        (UF.unfoldEach (sourceUnfoldrM inner start) (sourceUnfoldrM outer start))
        start

#ifdef INSPECTION
inspect $ 'unfoldEach `hasNoType` ''S.Step
inspect $ 'unfoldEach `hasNoType` ''FL.Step
inspect $ 'unfoldEach `hasNoType` ''SPEC
inspect $ 'unfoldEach `hasNoType` ''Producer.ConcatState
#endif

-- NOTE: Inlining this blows up the heap requirement to 1 GB.
-- {-# INLINE unfoldEachInterleave #-}
unfoldEachInterleave :: Int -> Int -> Int -> IO ()
unfoldEachInterleave inner outer start = do
    UF.fold
        FL.drain
        (UF.unfoldEachInterleave
            (sourceUnfoldrM inner start) (sourceUnfoldrM outer start))
        start

-- 'unfoldEachInterleave' does not fuse: 'Step' and 'SPEC' are not eliminated.
#ifdef INSPECTION
-- inspect $ 'unfoldEachInterleave `hasNoType` ''S.Step
-- inspect $ 'unfoldEachInterleave `hasNoType` ''SPEC
inspect $ 'unfoldEachInterleave `hasNoType` ''FL.Step
#endif

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure inner outer start =
    drainGeneration (UF.concatMap unfoldInGen unfoldOut) start

    where

    unfoldInGen i = UF.supplySecond (i + inner) UF.enumerateFromToIntegral
    unfoldOut = UF.supplySecond (start + outer) UF.enumerateFromToIntegral

#ifdef INSPECTION
-- inspect $ 'concatMapPure `hasNoType` ''S.Step
inspect $ 'concatMapPure `hasNoType` ''FL.Step
inspect $ 'concatMapPure `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Basic Constructors
    [ (SpaceO_1, benchIO "unfoldrM" $ unfoldrM size)
    , (SpaceO_1, benchIO "unfoldr" $ unfoldr size)
    , (SpaceO_1, benchIO "functionM" $ functionM size)
    , (SpaceO_1, benchIO "function" $ function size)
    , (SpaceO_1, benchIO "functionMaybeM" $ functionMaybeM size)
    , (SpaceO_1, benchIO "identity" $ identity size)
    -- From Values
    , (SpaceO_1, benchIO "fromEffect" $ fromEffect size)
    , (SpaceO_1, benchIO "fromPure" $ fromPure size)
    -- From Containers
    , (SpaceO_1, benchIO "fromList" $ fromList size)
    , (SpaceO_1, benchIO "fromTuple" $ fromTuple size)
    -- Transformations
    , (SpaceO_1, benchIO "lmap" $ lmap size)
    , (SpaceO_1, benchIO "lmapM" $ lmapM size)
    , (SpaceO_1, benchIO "swap" $ swap size)
    , (SpaceO_1, benchIO "map" $ map size)
    , (SpaceO_1, benchIO "mapM" $ mapM size)
    , (SpaceO_1, benchIO "both" $ both size)
    , (SpaceO_1, benchIO "first" $ first size)
    , (SpaceO_1, benchIO "second" $ second size)
    , (SpaceO_1, benchIO "consInput" $ consInput size)
    , (SpaceO_1, benchIO "consInputWith" $ consInputWith size)
    -- Trimming
    , (SpaceO_1, benchIO "takeWhileM" $ takeWhileM size)
    , (SpaceO_1, benchIO "takeWhile" $ takeWhile size)
    -- Nesting
    , (SpaceO_1, benchIO "interleave" $ interleave size)
    , (SpaceO_1, benchIO "unfoldEach inner=outer=(sqrt Max)" $ unfoldEach sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach inner=1 outer=Max" $ unfoldEach 1 size)
    , (SpaceO_1, benchIO "unfoldEach inner=Max outer=1" $ unfoldEach size 1)
    , (SpaceO_1, benchIO "unfoldEachInterleave inner=outer=(sqrt Max)"
        $ unfoldEachInterleave sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEachInterleave inner=1 outer=Max"
        $ unfoldEachInterleave 1 size)
    , (SpaceO_1, benchIO "unfoldEachInterleave inner=Max outer=1"
        $ unfoldEachInterleave size 1)
    -- Applicative
    , (SpaceO_1, benchIO "crossApply outer=inner=(sqrt Max)" $ toNullAp size)
    , (SpaceO_1, benchIO "crossApply2 outer=inner=(sqrt Max)" $ crossApply size)
    , (SpaceO_1, benchIO "crossApplySnd outer=inner=(sqrt Max)" $ crossApplySnd size)
    , (SpaceO_1, benchIO "crossApplyFst outer=inner=(sqrt Max)" $ crossApplyFst size)
    , (SpaceO_1, benchIO "crossWithM outer=inner=(sqrt Max)" $ crossWithM size)
    , (SpaceO_1, benchIO "crossWith outer=inner=(sqrt Max)" $ crossWith size)
    , (SpaceO_1, benchIO "cross outer=inner=(sqrt Max)" $ cross size)
    , (SpaceO_1, benchIO "fairCrossWithM outer=inner=(sqrt Max)" $ fairCrossWithM size)
    , (SpaceO_1, benchIO "fairCrossWith outer=inner=(sqrt Max)" $ fairCrossWith size)
    , (SpaceO_1, benchIO "fairCross outer=inner=(sqrt Max)" $ fairCross size)
    -- Monad
    , (SpaceO_1, benchIO "concatMapM outer=inner=(sqrt Max)" $ concatMapM sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMapPure outer=inner=(sqrt Max)" $ concatMapPure sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap2" $ toNullConcatMap size)
    , (SpaceO_1, benchIO "concatMap3" $ toNull3ConcatMap size)
    , (SpaceO_1, benchIO "bind2" $ toNull size)
    , (SpaceO_1, benchIO "bind3" $ toNull3 size)
    , (SpaceO_1, benchIO "breakAfterSome2" $ breakAfterSome size)
    , (SpaceO_1, benchIO "filterAllOut2" $ filterAllOut size)
    , (SpaceO_1, benchIO "filterAllIn2" $ filterAllIn size)
    , (SpaceO_1, benchIO "filterSome2" $ filterSome size)
    , (SpaceO_n, benchIO "toList2" $ toList size)
    , (SpaceO_n, benchIO "toListSome2" $ toListSome size)
    -- zipWith
    , (SpaceO_1, benchIO "zipArrowWithM" $ zipArrowWithM size)
    , (SpaceO_1, benchIO "zipArrowWith" $ zipArrowWith size)
    , (SpaceO_1, benchIO "zipWithM" $ zipWithM size)
    , (SpaceO_1, benchIO "zipWith" $ zipWith size)
    , (SpaceO_1, benchIO "teeZipWith" $ teeZipWith size)
    -- Deprecated
    , (SpaceO_1, benchIO "mapM2" $ mapM2 size)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
