-- |
-- Module      : Unfold.Type
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unfold.Type (benchmarks) where

import GHC.Types (SPEC(..))
import Control.DeepSeq (NFData(..))
import Control.Exception (ErrorCall, try, Exception, SomeException)
import Data.Typeable (Typeable)
import Unsafe.Coerce (UnsafeEquality)
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import Streamly.Internal.Data.Unfold (Unfold)
import System.Random (randomRIO)

import qualified Data.Tuple as Tuple
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as UF

import Fusion.Plugin.Types
import Test.Tasty.Bench hiding (env)
import Prelude hiding
    (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
import Streamly.Benchmark.Common

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- generate numbers up to the argument value
{-# INLINE source #-}
source :: Monad m => Int -> Unfold m Int Int
source n = UF.supplySecond n UF.enumerateFromToNum

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
    drainTransformation (UF.supplySecond to UF.enumerateFromToNum)

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

    src = UF.supplySecond to UF.enumerateFromToNum

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

{-# ANN lmap (PermitPatternMatches [''Int]) #-}
{-# ANN lmap (PermitConstructions []) #-}
{-# ANN lmap (PermitTypeClasses []) #-}
{-# NOINLINE lmap #-}
lmap :: Int -> Int -> IO ()
lmap size start =
    drainTransformationDefault (size + start) (UF.lmap (+ 1)) start

{-# ANN lmapM (PermitPatternMatches [''Int]) #-}
{-# ANN lmapM (PermitConstructions []) #-}
{-# ANN lmapM (PermitTypeClasses []) #-}
{-# NOINLINE lmapM #-}
lmapM :: Int -> Int -> IO ()
lmapM size start =
    drainTransformationDefault (size + start) (UF.lmapM (return . (+) 1)) start

{-# ANN both (PermitPatternMatches [''Int]) #-}
{-# ANN both (PermitConstructions []) #-}
{-# ANN both (PermitTypeClasses []) #-}
{-# NOINLINE both #-}
both :: Int -> Int -> IO ()
both size start =
    drainTransformationDefault (size + start) (UF.supply start) ()

{-# ANN first (PermitPatternMatches [''Int]) #-}
{-# ANN first (PermitConstructions []) #-}
{-# ANN first (PermitTypeClasses []) #-}
{-# NOINLINE first #-}
first :: Int -> Int -> IO ()
first size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenNum)
        (UF.supplyFirst start)
        1

{-# ANN second (PermitPatternMatches [''Int]) #-}
{-# ANN second (PermitConstructions []) #-}
{-# ANN second (PermitTypeClasses []) #-}
{-# NOINLINE second #-}
second :: Int -> Int -> IO ()
second size =
    drainTransformation
        (UF.take size UF.enumerateFromThenNum)
        (UF.supplySecond 1)

{-# ANN consInput (PermitPatternMatches [''Int]) #-}
{-# ANN consInput (PermitConstructions []) #-}
{-# ANN consInput (PermitTypeClasses []) #-}
{-# NOINLINE consInput #-}
consInput :: Int -> Int -> IO ()
consInput size start =
    drainTransformationDefault (size + start) UF.consInput start

{-# ANN consInputWith (PermitPatternMatches [''Int]) #-}
{-# ANN consInputWith (PermitConstructions []) #-}
{-# ANN consInputWith (PermitTypeClasses []) #-}
{-# NOINLINE consInputWith #-}
consInputWith :: Int -> Int -> IO ()
consInputWith size start =
    drainTransformationDefault (size + start) (UF.consInputWith (+1)) start

{-# ANN swap (PermitPatternMatches [''Int]) #-}
{-# ANN swap (PermitConstructions []) #-}
{-# ANN swap (PermitTypeClasses []) #-}
{-# NOINLINE swap #-}
swap :: Int -> Int -> IO ()
swap size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenNum)
        (UF.lmap Tuple.swap)
        (1, start)

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

-- 'functionM', 'function', 'identity' and 'fromEffect' generate a single
-- element per seed, so to process ~value elements we unfold them over an outer
-- source of value seeds.
{-# ANN functionM (PermitPatternMatches [''Int]) #-}
{-# ANN functionM (PermitConstructions []) #-}
{-# ANN functionM (PermitTypeClasses []) #-}
{-# NOINLINE functionM #-}
functionM :: Int -> Int -> IO ()
functionM value start =
    drainGeneration
        (UF.unfoldEach (UF.functionM return) (source (start + value))) start

{-# ANN function (PermitPatternMatches [''Int]) #-}
{-# ANN function (PermitConstructions []) #-}
{-# ANN function (PermitTypeClasses []) #-}
{-# NOINLINE function #-}
function :: Int -> Int -> IO ()
function value start =
    drainGeneration
        (UF.unfoldEach (UF.function id) (source (start + value))) start

{-# ANN identity (PermitPatternMatches [''Int]) #-}
{-# ANN identity (PermitConstructions []) #-}
{-# ANN identity (PermitTypeClasses []) #-}
{-# NOINLINE identity #-}
identity :: Int -> Int -> IO ()
identity value start =
    drainGeneration (UF.unfoldEach UF.identity (source (start + value))) start

{-# ANN fromEffect (PermitPatternMatches [''Int]) #-}
{-# ANN fromEffect (PermitConstructions []) #-}
{-# ANN fromEffect (PermitTypeClasses []) #-}
{-# NOINLINE fromEffect #-}
fromEffect :: Int -> Int -> IO ()
fromEffect value start =
    drainGeneration
        (UF.unfoldEach (UF.fromEffect (return start)) (source (start + value)))
        start

{-# ANN fromPure (PermitPatternMatches [''Int]) #-}
{-# ANN fromPure (PermitConstructions []) #-}
{-# ANN fromPure (PermitTypeClasses []) #-}
{-# NOINLINE fromPure #-}
fromPure :: Int -> Int -> IO ()
fromPure value start =
    drainGeneration
        (UF.unfoldEach (UF.fromPure start) (source (start + value)))
        start

{-# ANN functionMaybeM (PermitPatternMatches [''Int]) #-}
{-# ANN functionMaybeM (PermitConstructions []) #-}
{-# ANN functionMaybeM (PermitTypeClasses []) #-}
{-# NOINLINE functionMaybeM #-}
functionMaybeM :: Int -> Int -> IO ()
functionMaybeM value start =
    drainGeneration
        (UF.unfoldEach
            (UF.functionMaybeM (return . Just)) (source (start + value)))
        start

-- 'fromTuple' generates two elements per seed, so unfold it over value/2 tuples
-- to emit and drain ~value elements.
{-# ANN fromTuple (PermitPatternMatches [''Int]) #-}
{-# ANN fromTuple (PermitConstructions []) #-}
{-# ANN fromTuple (PermitTypeClasses []) #-}
{-# NOINLINE fromTuple #-}
fromTuple :: Int -> Int -> IO ()
fromTuple value start =
    let outer = UF.map (\i -> (i, i)) (source (start + value `div` 2))
     in drainGeneration (UF.unfoldEach UF.fromTuple outer) start

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Unfold m Int Int
sourceUnfoldrM size start = UF.unfoldrM step

    where

    step i =
        return
            $ if i < start + size
              then Just (i, i + 1)
              else Nothing

{-# ANN unfoldrM (PermitPatternMatches []) #-}
{-# ANN unfoldrM (PermitConstructions []) #-}
{-# ANN unfoldrM (PermitTypeClasses []) #-}
{-# NOINLINE unfoldrM #-}
unfoldrM :: Int -> Int -> IO ()
unfoldrM size start = drainGeneration (sourceUnfoldrM size start) start

{-# ANN unfoldr (PermitPatternMatches []) #-}
{-# ANN unfoldr (PermitConstructions []) #-}
{-# ANN unfoldr (PermitTypeClasses []) #-}
{-# NOINLINE unfoldr #-}
unfoldr :: Int -> Int -> IO ()
unfoldr size start = drainGeneration (UF.unfoldr step) start
    where
    step i = if i < start + size then Just (i, i + 1) else Nothing

{-# ANN fromList (PermitPatternMatches [''[]]) #-}
{-# ANN fromList (PermitConstructions [''Int,''[]]) #-}
{-# ANN fromList (PermitTypeClasses []) #-}
{-# NOINLINE fromList #-}
fromList :: Int -> Int -> IO ()
fromList size start = drainGeneration UF.fromList [start .. start + size]

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# ANN map (PermitPatternMatches [''Int]) #-}
{-# ANN map (PermitConstructions []) #-}
{-# ANN map (PermitTypeClasses []) #-}
{-# NOINLINE map #-}
map :: Int -> Int -> IO ()
map size start = drainTransformationDefault (size + start) (UF.map (+1)) start

{-# ANN mapM (PermitPatternMatches [''Int]) #-}
{-# ANN mapM (PermitConstructions []) #-}
{-# ANN mapM (PermitTypeClasses []) #-}
{-# NOINLINE mapM #-}
mapM :: Int -> Int -> IO ()
mapM size start =
    drainTransformationDefault (size + start) (UF.mapM (return . (+) 1)) start

{-# ANN mapM_CarryInput (PermitPatternMatches [''Int]) #-}
{-# ANN mapM_CarryInput (PermitConstructions []) #-}
{-# ANN mapM_CarryInput (PermitTypeClasses []) #-}
{-# NOINLINE mapM_CarryInput #-}
mapM_CarryInput :: Int -> Int -> IO ()
mapM_CarryInput size =
    drainTransformationDefault
        size
        (UF.mapM (\(a, b) -> return $ a + b) . UF.carryInput)

-------------------------------------------------------------------------------
-- Stream filtering
-------------------------------------------------------------------------------

{-# ANN takeWhileM (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhileM (PermitConstructions []) #-}
{-# ANN takeWhileM (PermitTypeClasses []) #-}
{-# NOINLINE takeWhileM #-}
takeWhileM :: Int -> Int -> IO ()
takeWhileM size start =
    drainTransformationDefault
        size
        (UF.takeWhileM (\b -> return (b <= size + start)))
        start

{-# ANN takeWhile (PermitPatternMatches [''Int]) #-}
{-# ANN takeWhile (PermitConstructions []) #-}
{-# ANN takeWhile (PermitTypeClasses []) #-}
{-# NOINLINE takeWhile #-}
takeWhile :: Int -> Int -> IO ()
takeWhile size start =
    drainTransformationDefault
        size
        (UF.takeWhile (\b -> b <= size + start))
        start

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# ANN zipWith (PermitPatternMatches [''Int]) #-}
{-# ANN zipWith (PermitConstructions [''Int]) #-}
{-# ANN zipWith (PermitTypeClasses []) #-}
{-# NOINLINE zipWith #-}
zipWith :: Int -> Int -> IO ()
zipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) start

{-# ANN zipWithM (PermitPatternMatches [''Int]) #-}
{-# ANN zipWithM (PermitConstructions [''Int]) #-}
{-# ANN zipWithM (PermitTypeClasses []) #-}
{-# NOINLINE zipWithM #-}
zipWithM :: Int -> Int -> IO ()
zipWithM size start =
    drainProductDefault
        (size + start)
        (UF.zipWithM (\a b -> return $ a + b))
        start

{-# ANN interleave (PermitPatternMatches [''Int]) #-}
{-# ANN interleave (PermitConstructions []) #-}
{-# ANN interleave (PermitTypeClasses []) #-}
{-# NOINLINE interleave #-}
interleave :: Int -> Int -> IO ()
interleave size start =
    drainProductDefault (size + start) UF.interleave (start, start)

{-# ANN zipArrowWithM (PermitPatternMatches [''Int]) #-}
{-# ANN zipArrowWithM (PermitConstructions [''Int]) #-}
{-# ANN zipArrowWithM (PermitTypeClasses []) #-}
{-# NOINLINE zipArrowWithM #-}
zipArrowWithM :: Int -> Int -> IO ()
zipArrowWithM size start =
    drainProductDefault
        (size + start)
        (UF.zipArrowWithM (\a b -> return (a + b)))
        (start, start)

{-# ANN zipArrowWith (PermitPatternMatches [''Int]) #-}
{-# ANN zipArrowWith (PermitConstructions [''Int]) #-}
{-# ANN zipArrowWith (PermitTypeClasses []) #-}
{-# NOINLINE zipArrowWith #-}
zipArrowWith :: Int -> Int -> IO ()
zipArrowWith size start =
    drainProductDefault (size + start) (UF.zipArrowWith (+)) (start, start)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

nthRoot :: Double -> Int -> Int
nthRoot n value = round (fromIntegral value**(1/n))

{-# ANN ap_ApplicativeInstance_x2 (PermitPatternMatches [''Int]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitConstructions [''Int]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_x2 #-}
ap_ApplicativeInstance_x2 :: Int -> Int -> IO ()
ap_ApplicativeInstance_x2 value start =
    let end = start + nthRoot 2 value
        s = source end
    -- in UF.fold ((+) <$> s <*> s) FL.drain start
    in UF.fold FL.drain (((+) `fmap` s) `UF.crossApply` s) start

{-# ANN crossApplyFst (PermitPatternMatches [''Int]) #-}
{-# ANN crossApplyFst (PermitConstructions [''Int]) #-}
{-# ANN crossApplyFst (PermitTypeClasses []) #-}
{-# NOINLINE crossApplyFst #-}
crossApplyFst :: Int -> Int -> IO ()
crossApplyFst value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.crossApplyFst` s) start

{-# ANN crossApplySnd (PermitPatternMatches [''Int]) #-}
{-# ANN crossApplySnd (PermitConstructions [''Int]) #-}
{-# ANN crossApplySnd (PermitTypeClasses []) #-}
{-# NOINLINE crossApplySnd #-}
crossApplySnd :: Int -> Int -> IO ()
crossApplySnd value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.crossApplySnd` s) start

{-# ANN cross (PermitPatternMatches [''Int]) #-}
{-# ANN cross (PermitConstructions [''Int]) #-}
{-# ANN cross (PermitTypeClasses []) #-}
{-# NOINLINE cross #-}
cross :: Int -> Int -> IO ()
cross value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.cross` s) start

{-# ANN fairCross (PermitPatternMatches
    [''(,),''Int,''[],''Producer.EnumToState]) #-}
{-# ANN fairCross (PermitConstructions
    [''Int,''(,),''[],''Producer.EnumToState]) #-}
{-# ANN fairCross (PermitTypeClasses []) #-}
{-# NOINLINE fairCross #-}
fairCross :: Int -> Int -> IO ()
fairCross value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.fairCross` s) start

{-# ANN crossApply (PermitPatternMatches [''Int]) #-}
{-# ANN crossApply (PermitConstructions [''Int]) #-}
{-# ANN crossApply (PermitTypeClasses []) #-}
{-# NOINLINE crossApply #-}
crossApply :: Int -> Int -> IO ()
crossApply value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.crossApply (UF.map (+) s) s) start

{-# ANN crossWithM (PermitPatternMatches [''Int]) #-}
{-# ANN crossWithM (PermitConstructions [''Int]) #-}
{-# ANN crossWithM (PermitTypeClasses []) #-}
{-# NOINLINE crossWithM #-}
crossWithM :: Int -> Int -> IO ()
crossWithM value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.crossWithM (\a b -> return (a + b)) s s) start

{-# ANN crossWith (PermitPatternMatches [''Int]) #-}
{-# ANN crossWith (PermitConstructions [''Int]) #-}
{-# ANN crossWith (PermitTypeClasses []) #-}
{-# NOINLINE crossWith #-}
crossWith :: Int -> Int -> IO ()
crossWith value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.crossWith (+) s s) start

{-# ANN fairCrossWithM (PermitPatternMatches
    [''(,),''Int,''[],''Producer.EnumToState]) #-}
{-# ANN fairCrossWithM (PermitConstructions
    [''Int,''(,),''[],''Producer.EnumToState]) #-}
{-# ANN fairCrossWithM (PermitTypeClasses []) #-}
{-# NOINLINE fairCrossWithM #-}
fairCrossWithM :: Int -> Int -> IO ()
fairCrossWithM value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.fairCrossWithM (\a b -> return (a + b)) s s) start

{-# ANN fairCrossWith (PermitPatternMatches
    [''(,),''Int,''[],''Producer.EnumToState]) #-}
{-# ANN fairCrossWith (PermitConstructions
    [''Int,''(,),''[],''Producer.EnumToState]) #-}
{-# ANN fairCrossWith (PermitTypeClasses []) #-}
{-# NOINLINE fairCrossWith #-}
fairCrossWith :: Int -> Int -> IO ()
fairCrossWith value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.fairCrossWith (+) s s) start

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

-- XXX to keep the benchmarks same as Stream we should use sourceUnfoldrM in
-- all of these, and other benchmarks too.
{-# ANN concatMapM (PermitPatternMatches [''Int,''Producer.EnumToState]) #-}
{-# ANN concatMapM (PermitConstructions [''Int,''Producer.EnumToState]) #-}
{-# ANN concatMapM (PermitTypeClasses []) #-}
{-# NOINLINE concatMapM #-}
concatMapM :: Int -> Int -> Int -> IO ()
concatMapM inner outer start =
    drainGeneration (UF.concatMapM unfoldInGen unfoldOut) start

    where

    unfoldInGen i = return (UF.supplySecond (i + inner) UF.enumerateFromToNum)
    unfoldOut = UF.supplySecond (start + outer) UF.enumerateFromToNum

-- The 'bind'-based benchmarks use the Unfold monad ('UF.bind'), which is a
-- concatMap and does not fuse, so the 'Step' constructors remain.

{-# ANN bind_MonadInstance_x2 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_x2 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState
    ,''Producer.EnumToState,''Bool]) #-}
{-# ANN bind_MonadInstance_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_x2 #-}
bind_MonadInstance_x2 :: Int -> Int -> IO ()
bind_MonadInstance_x2 value start =
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

{-# ANN bind_MonadInstance_x3 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step,''Unfold]) #-}
{-# ANN bind_MonadInstance_x3 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState
    ,''Producer.EnumToState,''Unfold,''Bool]) #-}
{-# ANN bind_MonadInstance_x3 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_x3 #-}
bind_MonadInstance_x3 :: Int -> Int -> IO ()
bind_MonadInstance_x3 value start =
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

{-# ANN concatMap_x2 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step]) #-}
{-# ANN concatMap_x2 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState
    ,''Producer.EnumToState,''Bool]) #-}
{-# ANN concatMap_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_x2 #-}
concatMap_x2 :: Int -> Int -> IO ()
concatMap_x2 value start =
    let end = start + nthRoot 2 value
        src = source end
        u = UF.concatMap (\x ->
            UF.concatMap (\y ->
                UF.fromPure (x + y)) src) src
     in UF.fold FL.drain u start

{-# ANN concatMap_x3 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step,''Unfold]) #-}
{-# ANN concatMap_x3 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState
    ,''Producer.EnumToState,''Unfold,''Bool]) #-}
{-# ANN concatMap_x3 (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_x3 #-}
concatMap_x3 :: Int -> Int -> IO ()
concatMap_x3 value start =
    let end = start + nthRoot 3 value
        src = source end
        u = UF.concatMap (\x ->
            UF.concatMap (\y ->
            UF.concatMap (\z ->
                UF.fromPure (x + y + z)) src) src) src
     in UF.fold FL.drain u start

{-# ANN bind_MonadInstance_ToList_x2 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_ToList_x2 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState,''[]
    ,''Producer.EnumToState,''Bool]) #-}
{-# ANN bind_MonadInstance_ToList_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_ToList_x2 #-}
bind_MonadInstance_ToList_x2 :: Int -> Int -> IO [Int]
bind_MonadInstance_ToList_x2 value start = do
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

{-# ANN bind_MonadInstance_ToListSome_x2 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_ToListSome_x2 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState,''[]
    ,''Producer.EnumToState,''Bool]) #-}
{-# ANN bind_MonadInstance_ToListSome_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_ToListSome_x2 #-}
bind_MonadInstance_ToListSome_x2 :: Int -> Int -> IO [Int]
bind_MonadInstance_ToListSome_x2 value start = do
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

{-# ANN bind_MonadInstance_FilterAllOut_x2 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_x2 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState
    ,''Producer.EnumToState,''Bool]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllOut_x2 #-}
bind_MonadInstance_FilterAllOut_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllOut_x2 value start = do
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

{-# ANN bind_MonadInstance_FilterAllIn_x2 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_x2 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState
    ,''Producer.EnumToState,''Bool]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllIn_x2 #-}
bind_MonadInstance_FilterAllIn_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllIn_x2 value start = do
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

{-# ANN bind_MonadInstance_FilterSome_x2 (PermitPatternMatches
    [''Bool,''Int,''Producer.ConcatMapReaderState,''Producer.EnumToState
    ,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_FilterSome_x2 (PermitConstructions
    [''Int,''Stream.Step,''Producer.ConcatMapReaderState
    ,''Producer.EnumToState,''Bool]) #-}
{-# ANN bind_MonadInstance_FilterSome_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterSome_x2 #-}
bind_MonadInstance_FilterSome_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterSome_x2 value start = do
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

{-# ANN bind_MonadInstance_BreakAfterSome_x2 (PermitPatternMatches
    [''SomeException,''UnsafeEquality,''Bool]) #-}
{-# ANN bind_MonadInstance_BreakAfterSome_x2 (PermitConstructions
    [''Either,''Int,''SrcLoc,''CallStack]) #-}
{-# ANN bind_MonadInstance_BreakAfterSome_x2 (PermitTypeClasses
    [''Typeable,''IP,''Exception]) #-}
{-# NOINLINE bind_MonadInstance_BreakAfterSome_x2 #-}
bind_MonadInstance_BreakAfterSome_x2 :: Int -> Int -> IO ()
bind_MonadInstance_BreakAfterSome_x2 value start =
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

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# ANN unfoldEach (PermitPatternMatches [''Int]) #-}
{-# ANN unfoldEach (PermitConstructions []) #-}
{-# ANN unfoldEach (PermitTypeClasses []) #-}
{-# NOINLINE unfoldEach #-}
unfoldEach :: Int -> Int -> Int -> IO ()
unfoldEach inner outer start = do
    UF.fold
        FL.drain
        (UF.unfoldEach
            (sourceUnfoldrM inner start) (sourceUnfoldrM outer start))
        start

-- NOTE: Inlining this blows up the heap requirement to 1 GB.
{-# ANN unfoldEachInterleave (PermitPatternMatches
    [''Producer.InterleaveEachState,''Int,''IO,''[],''SPEC]) #-}
{-# ANN unfoldEachInterleave (PermitConstructions
    [''Producer.InterleaveEachState,''Int,''SrcLoc,''[],''CallStack,''SPEC]) #-}
{-# ANN unfoldEachInterleave (PermitTypeClasses [''IP]) #-}
{-# NOINLINE unfoldEachInterleave #-}
unfoldEachInterleave :: Int -> Int -> Int -> IO ()
unfoldEachInterleave inner outer start = do
    UF.fold
        FL.drain
        (UF.unfoldEachInterleave
            (sourceUnfoldrM inner start) (sourceUnfoldrM outer start))
        start

{-# ANN concatMap_Pure (PermitPatternMatches [''Int,''Producer.EnumToState]) #-}
{-# ANN concatMap_Pure (PermitConstructions [''Int,''Producer.EnumToState]) #-}
{-# ANN concatMap_Pure (PermitTypeClasses []) #-}
{-# NOINLINE concatMap_Pure #-}
concatMap_Pure :: Int -> Int -> Int -> IO ()
concatMap_Pure inner outer start =
    drainGeneration (UF.concatMap unfoldInGen unfoldOut) start

    where

    unfoldInGen i = UF.supplySecond (i + inner) UF.enumerateFromToNum
    unfoldOut = UF.supplySecond (start + outer) UF.enumerateFromToNum

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
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
    , (SpaceO_1, benchIO "unfoldEach (inner=outer=sqrt Max)" $
          unfoldEach sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEach (inner=1 outer=Max)" $ unfoldEach 1 size)
    , (SpaceO_1, benchIO "unfoldEach (inner=Max outer=1)" $ unfoldEach size 1)
    , (SpaceO_1, benchIO "unfoldEachInterleave (inner=outer=sqrt Max)"
        $ unfoldEachInterleave sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "unfoldEachInterleave (inner=1 outer=Max)"
        $ unfoldEachInterleave 1 size)
    , (SpaceO_1, benchIO "unfoldEachInterleave (inner=Max outer=1)"
        $ unfoldEachInterleave size 1)
    -- Applicative
    , (SpaceO_1, benchIO "ap_ApplicativeInstance_x2" $
          ap_ApplicativeInstance_x2 size)
    , (SpaceO_1, benchIO "crossApply" $ crossApply size)
    , (SpaceO_1, benchIO "crossApplySnd" $ crossApplySnd size)
    , (SpaceO_1, benchIO "crossApplyFst" $ crossApplyFst size)
    , (SpaceO_1, benchIO "crossWithM" $ crossWithM size)
    , (SpaceO_1, benchIO "crossWith" $ crossWith size)
    , (SpaceO_1, benchIO "cross" $ cross size)
    , (SpaceO_1, benchIO "fairCrossWithM" $ fairCrossWithM size)
    , (SpaceO_1, benchIO "fairCrossWith" $ fairCrossWith size)
    , (SpaceO_1, benchIO "fairCross" $ fairCross size)
    -- Monad
    , (SpaceO_1, benchIO "concatMapM (inner=outer=sqrt Max)" $
          concatMapM sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap_Pure (inner=outer=sqrt Max)" $
          concatMap_Pure sqrtVal sqrtVal)
    , (SpaceO_1, benchIO "concatMap_x2" $ concatMap_x2 size)
    , (SpaceO_1, benchIO "concatMap_x3" $ concatMap_x3 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_x2" $ bind_MonadInstance_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_x3" $ bind_MonadInstance_x3 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_BreakAfterSome_x2" $
          bind_MonadInstance_BreakAfterSome_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllOut_x2" $
          bind_MonadInstance_FilterAllOut_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllIn_x2" $
          bind_MonadInstance_FilterAllIn_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterSome_x2" $
          bind_MonadInstance_FilterSome_x2 size)
    , (SpaceO_n, benchIO "bind_MonadInstance_ToList_x2" $
          bind_MonadInstance_ToList_x2 size)
    , (SpaceO_n, benchIO "bind_MonadInstance_ToListSome_x2" $
          bind_MonadInstance_ToListSome_x2 size)
    -- zipWith
    , (SpaceO_1, benchIO "zipArrowWithM" $ zipArrowWithM size)
    , (SpaceO_1, benchIO "zipArrowWith" $ zipArrowWith size)
    , (SpaceO_1, benchIO "zipWithM" $ zipWithM size)
    , (SpaceO_1, benchIO "zipWith" $ zipWith size)
    -- Deprecated
    , (SpaceO_1, benchIO "mapM_CarryInput" $ mapM_CarryInput size)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)
