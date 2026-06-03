-- |
-- Module      : Streamly.Benchmark.Data.Fold
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

module Main (main) where

import Control.DeepSeq (NFData(..))
import Control.Exception (SomeException, ErrorCall, try)
import Data.Char (ord)
import qualified Data.Tuple as Tuple
import Data.Word (Word8)
import Streamly.Internal.Data.Unfold (Unfold)
import System.IO (Handle, hClose)
import System.Random (randomRIO)

import qualified Prelude
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.StreamK as K

import Test.Tasty.Bench hiding (env)
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Control.Monad.Catch (MonadCatch)
import Test.Inspection
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
drainGeneration unf seed = UF.fold FL.drain unf seed

{-# INLINE drainTransformation #-}
drainTransformation ::
       Monad m => Unfold m a b -> (Unfold m a b -> Unfold m c d) -> c -> m ()
drainTransformation unf f seed = drainGeneration (f unf) seed

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
drainProduct unf1 unf2 f seed = drainGeneration (f unf1 unf2) seed

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

{-# INLINE lmapM #-}
lmapM :: Int -> Int -> IO ()
lmapM size start =
    drainTransformationDefault (size + start) (UF.lmapM (return . (+) 1)) start

{-# INLINE both #-}
both :: Int -> Int -> IO ()
both size start =
    drainTransformationDefault (size + start) (UF.supply start) ()


{-# INLINE first #-}
first :: Int -> Int -> IO ()
first size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.supplyFirst start)
        1

{-# INLINE second #-}
second :: Int -> Int -> IO ()
second size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.supplySecond 1)
        start

{-# INLINE discardFirst #-}
discardFirst :: Int -> Int -> IO ()
discardFirst size start =
    drainTransformationDefault (size + start) UF.discardFirst (start, start)

{-# INLINE discardSecond #-}
discardSecond :: Int -> Int -> IO ()
discardSecond size start =
    drainTransformationDefault (size + start) UF.discardSecond (start, start)

{-# INLINE swap #-}
swap :: Int -> Int -> IO ()
swap size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.lmap Tuple.swap)
        (1, start)

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# INLINE fromStream #-}
fromStream :: Int -> Int -> IO ()
fromStream size start =
    drainGeneration UF.fromStream (S.replicate size start :: S.Stream IO Int)

-- XXX INVESTIGATE: Although the performance of this should be equivalant to
-- fromStream, this is considerably worse. More than 4x worse.
{-# INLINE fromStreamK #-}
fromStreamK :: Int -> Int -> IO ()
fromStreamK size start = drainGeneration UF.fromStreamK (K.replicate size start)

{-# INLINE fromStreamD #-}
fromStreamD :: Int -> Int -> IO ()
fromStreamD size start =
    drainGeneration UF.fromStreamD (S.replicate size start)

-- 'nilM' runs its action on the seed but yields no output, so unfold it over an
-- outer source of value seeds to run it ~value times.
{-# INLINE nilM #-}
nilM :: Int -> Int -> IO ()
nilM value start =
    drainGeneration (UF.unfoldEach (UF.nilM return) (source (start + value))) start

{-# INLINE consM #-}
consM :: Int -> Int -> IO ()
consM size start =
    drainTransformationDefault (size + start) (UF.consM return) start

-- 'functionM', 'function', 'identity' and 'fromEffect' generate a single
-- element per seed, so to process ~value elements we unfold them over an outer
-- source of value seeds.
{-# INLINE functionM #-}
functionM :: Int -> Int -> IO ()
functionM value start =
    drainGeneration
        (UF.unfoldEach (UF.functionM return) (source (start + value))) start

{-# INLINE function #-}
function :: Int -> Int -> IO ()
function value start =
    drainGeneration
        (UF.unfoldEach (UF.function id) (source (start + value))) start

{-# INLINE identity #-}
identity :: Int -> Int -> IO ()
identity value start =
    drainGeneration (UF.unfoldEach UF.identity (source (start + value))) start

{-# INLINE fromEffect #-}
fromEffect :: Int -> Int -> IO ()
fromEffect value start =
    drainGeneration
        (UF.unfoldEach (UF.fromEffect (return start)) (source (start + value)))
        start

-- 'fromTuple' generates two elements per seed, so unfold it over value/2 tuples
-- to emit and drain ~value elements.
{-# INLINE fromTuple #-}
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

{-# INLINE unfoldrM #-}
unfoldrM :: Int -> Int -> IO ()
unfoldrM size start = drainGeneration (sourceUnfoldrM size start) start

{-# INLINE fromList #-}
fromList :: Int -> Int -> IO ()
fromList size start = drainGeneration UF.fromList [start .. start + size]

{-# INLINE fromListM #-}
fromListM :: Int -> Int -> IO ()
fromListM size start =
    drainGeneration UF.fromListM (Prelude.map return [start .. start + size])

{-# INLINE _fromSVar #-}
_fromSVar :: Int -> Int -> m ()
_fromSVar = undefined

{-# INLINE _fromProducer #-}
_fromProducer :: Int -> Int -> m ()
_fromProducer = undefined

{-# INLINE replicateM #-}
replicateM :: Int -> Int -> IO ()
replicateM size start = drainGeneration UF.replicateM (size, return start)

{-# INLINE repeatM #-}
repeatM :: Int -> Int -> IO ()
repeatM size start = drainGeneration (UF.take size UF.repeatM) (return start)

{-# INLINE iterateM #-}
iterateM :: Int -> Int -> IO ()
iterateM size start =
    drainGeneration (UF.take size (UF.iterateM return)) (return start)

{-# INLINE fromIndicesM #-}
fromIndicesM :: Int -> Int -> IO ()
fromIndicesM size start =
    drainGeneration (UF.take size (UF.fromIndicesM return)) start

{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: Int -> Int -> IO ()
enumerateFromThenIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromThenIntegral) (start, 1)

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Int -> Int -> IO ()
enumerateFromToIntegral size start =
    drainGeneration
    ( UF.supplySecond
      (size + start)
      UF.enumerateFromToIntegral
    ) start

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: Int -> Int -> IO ()
enumerateFromIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromIntegral) start

{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: Int -> Int -> IO ()
enumerateFromStepNum size start =
    drainGeneration (UF.take size (UF.enumerateFromThenNum)) (start, 1)

{-# INLINE enumerateFromNum #-}
enumerateFromNum :: Int -> Int -> IO ()
enumerateFromNum size start = drainGeneration (UF.take size UF.enumerateFromNum) start

{-# INLINE enumerateFromToFractional #-}
enumerateFromToFractional :: Int -> Int -> IO ()
enumerateFromToFractional size start =
    let intToDouble x = (fromInteger (fromIntegral x)) :: Double
     in drainGeneration
            ( UF.supplySecond
              (intToDouble $ start + size)
              UF.enumerateFromToFractional
            )
            (intToDouble start)

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE postscan #-}
postscan :: Int -> Int -> IO ()
postscan size start =
    drainTransformationDefault (size + start) (UF.postscanl Scanl.sum) start

{-# INLINE map #-}
map :: Int -> Int -> IO ()
map size start = drainTransformationDefault (size + start) (UF.map (+1)) start

{-# INLINE mapM #-}
mapM :: Int -> Int -> IO ()
mapM size start =
    drainTransformationDefault (size + start) (UF.mapM (return . (+) 1)) start

{-# INLINE mapM2 #-}
mapM2 :: Int -> Int -> IO ()
mapM2 size start =
    drainTransformationDefault
        size
        (UF.mapM (\(a, b) -> return $ a + b) . UF.carryInput)
        start

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

{-# INLINE takeWhile #-}
takeWhile :: Int -> Int -> IO ()
takeWhile size start =
    drainTransformationDefault
        size
        (UF.takeWhile (\b -> b <= size + start))
        start

{-# INLINE take #-}
take :: Int -> Int -> IO ()
take size start = drainTransformationDefault (size + start) (UF.take size) start

{-# INLINE filter #-}
filter :: Int -> Int -> IO ()
filter size start =
    drainTransformationDefault (size + start) (UF.filter (\_ -> True)) start

{-# INLINE filterM #-}
filterM :: Int -> Int -> IO ()
filterM size start =
    drainTransformationDefault
        (size + start)
        (UF.filterM (\_ -> (return True)))
        start

-- Dropping one element from a large stream is dominated by generation, so
-- instead exercise 'drop' ~value/2 times: generate value/2 two-element streams
-- with 'fromTuple', 'drop' the first element of each, and flatten the rest.
{-# INLINE dropOne #-}
dropOne :: Int -> Int -> IO ()
dropOne value start =
    let outer = UF.map (\i -> (i, i)) (source (start + value `div` 2))
     in drainGeneration (UF.unfoldEach (UF.drop 1 UF.fromTuple) outer) start

{-# INLINE dropAll #-}
dropAll :: Int -> Int -> IO ()
dropAll size start =
    drainTransformationDefault (size + start) (UF.drop (size + 1)) start

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Int -> Int -> IO ()
dropWhileTrue size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return True))
        start

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Int -> Int -> IO ()
dropWhileFalse size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return False))
        start

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: Int -> Int -> IO ()
dropWhileMTrue size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return True))
        start

{-# INLINE dropWhileMFalse #-}
dropWhileMFalse :: Int -> Int -> IO ()
dropWhileMFalse size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return False))
        start

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: Int -> Int -> IO ()
zipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) start

{-# INLINE zipWithM #-}
zipWithM :: Int -> Int -> IO ()
zipWithM size start =
    drainProductDefault
        (size + start)
        (UF.zipWithM (\a b -> return $ a + b))
        start

{-# INLINE teeZipWith #-}
teeZipWith :: Int -> Int -> IO ()
teeZipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) start

{-# INLINE interleave #-}
interleave :: Int -> Int -> IO ()
interleave size start =
    drainProductDefault (size + start) UF.interleave (start, start)

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

{-# INLINE crossApplyFst #-}
crossApplyFst :: Int -> Int -> IO ()
crossApplyFst value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.crossApplyFst` s) start

{-# INLINE crossApplySnd #-}
crossApplySnd :: Int -> Int -> IO ()
crossApplySnd value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.crossApplySnd` s) start

{-# INLINE cross #-}
cross :: Int -> Int -> IO ()
cross value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.cross` s) start

{-# INLINE fairCross #-}
fairCross :: Int -> Int -> IO ()
fairCross value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (s `UF.fairCross` s) start

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

{-# INLINE unfoldEachInterleave #-}
unfoldEachInterleave :: Int -> Int -> Int -> IO ()
unfoldEachInterleave inner outer start = do
    UF.fold
        FL.drain
        (UF.unfoldEachInterleave
            (sourceUnfoldrM inner start) (sourceUnfoldrM outer start))
        start

-------------------------------------------------------------------------------
-- Inspection
-------------------------------------------------------------------------------

#ifdef INSPECTION
-- All benchmarks must fully fuse: no stream constructors (the 'Yield', 'Skip'
-- and 'Stop' of the 'Step' type) should remain in the optimized core.

-- input
inspect $ 'lmap `hasNoType` ''S.Step
inspect $ 'lmapM `hasNoType` ''S.Step
inspect $ 'both `hasNoType` ''S.Step
inspect $ 'first `hasNoType` ''S.Step
inspect $ 'second `hasNoType` ''S.Step
inspect $ 'discardFirst `hasNoType` ''S.Step
inspect $ 'discardSecond `hasNoType` ''S.Step
inspect $ 'swap `hasNoType` ''S.Step

-- generation
-- 'fromStream', 'fromStreamD' and 'consM' wrap an opaque stream/cons cell, so
-- the 'Step' is not eliminated.
-- inspect $ 'fromStream `hasNoType` ''S.Step
-- inspect $ 'fromStreamD `hasNoType` ''S.Step
-- inspect $ 'consM `hasNoType` ''S.Step
inspect $ 'fromStreamK `hasNoType` ''S.Step
inspect $ 'nilM `hasNoType` ''S.Step
inspect $ 'functionM `hasNoType` ''S.Step
inspect $ 'function `hasNoType` ''S.Step
inspect $ 'identity `hasNoType` ''S.Step
inspect $ 'fromEffect `hasNoType` ''S.Step
inspect $ 'fromTuple `hasNoType` ''S.Step
inspect $ 'unfoldrM `hasNoType` ''S.Step
inspect $ 'fromList `hasNoType` ''S.Step
inspect $ 'fromListM `hasNoType` ''S.Step
inspect $ 'replicateM `hasNoType` ''S.Step
inspect $ 'repeatM `hasNoType` ''S.Step
inspect $ 'iterateM `hasNoType` ''S.Step
inspect $ 'fromIndicesM `hasNoType` ''S.Step
inspect $ 'enumerateFromThenIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromToIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromStepNum `hasNoType` ''S.Step
inspect $ 'enumerateFromNum `hasNoType` ''S.Step
inspect $ 'enumerateFromToFractional `hasNoType` ''S.Step

-- transformation
inspect $ 'map `hasNoType` ''S.Step
inspect $ 'mapM `hasNoType` ''S.Step
inspect $ 'mapM2 `hasNoType` ''S.Step
inspect $ 'postscan `hasNoType` ''S.Step

-- filtering
inspect $ 'takeWhileM `hasNoType` ''S.Step
inspect $ 'takeWhile `hasNoType` ''S.Step
inspect $ 'take `hasNoType` ''S.Step
inspect $ 'filter `hasNoType` ''S.Step
inspect $ 'filterM `hasNoType` ''S.Step
inspect $ 'dropOne `hasNoType` ''S.Step
inspect $ 'dropAll `hasNoType` ''S.Step
inspect $ 'dropWhileTrue `hasNoType` ''S.Step
inspect $ 'dropWhileFalse `hasNoType` ''S.Step
inspect $ 'dropWhileMTrue `hasNoType` ''S.Step
inspect $ 'dropWhileMFalse `hasNoType` ''S.Step

-- zip
inspect $ 'zipWith `hasNoType` ''S.Step
inspect $ 'zipWithM `hasNoType` ''S.Step
inspect $ 'teeZipWith `hasNoType` ''S.Step
inspect $ 'interleave `hasNoType` ''S.Step

-- nested
inspect $ 'toNullAp `hasNoType` ''S.Step
inspect $ 'crossApplyFst `hasNoType` ''S.Step
inspect $ 'crossApplySnd `hasNoType` ''S.Step
inspect $ 'cross `hasNoType` ''S.Step
inspect $ 'fairCross `hasNoType` ''S.Step
inspect $ 'unfoldEach `hasNoType` ''S.Step
-- The 'bind'-based benchmarks use the Unfold monad ('UF.bind'), which is a
-- concatMap and does not fuse, so the 'Step' constructors remain. The same is
-- true for 'concatMapM' and 'unfoldEachInterleave'.
-- inspect $ 'concatMapM `hasNoType` ''S.Step
-- inspect $ 'toNull `hasNoType` ''S.Step
-- inspect $ 'toNull3 `hasNoType` ''S.Step
-- inspect $ 'toList `hasNoType` ''S.Step
-- inspect $ 'toListSome `hasNoType` ''S.Step
-- inspect $ 'filterAllOut `hasNoType` ''S.Step
-- inspect $ 'filterAllIn `hasNoType` ''S.Step
-- inspect $ 'filterSome `hasNoType` ''S.Step
-- inspect $ 'breakAfterSome `hasNoType` ''S.Step
-- inspect $ 'unfoldEachInterleave `hasNoType` ''S.Step
#endif

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_transformation_input :: Int -> [Benchmark]
o_1_space_transformation_input size =
    [ bgroup
          "transformation/input"
          [ benchIO "lmap" $ lmap size
          , benchIO "lmapM" $ lmapM size
          , benchIO "both" $ both size
          , benchIO "first" $ first size
          , benchIO "second" $ second size
          , benchIO "discardFirst" $ discardFirst size
          , benchIO "discardSecond" $ discardSecond size
          , benchIO "swap" $ swap size
          ]
    ]

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation size =
    [ bgroup
          "generation"
          [ benchIO "fromStream" $ fromStream size
          , benchIO "fromStreamK" $ fromStreamK size
          , benchIO "fromStreamD" $ fromStreamD size
          , benchIO "nilM" $ nilM size
          , benchIO "consM" $ consM size
          , benchIO "functionM" $ functionM size
          , benchIO "function" $ function size
          , benchIO "identity" $ identity size
          , benchIO "fromEffect" $ fromEffect size
          , benchIO "fromTuple" $ fromTuple size
          , benchIO "unfoldrM" $ unfoldrM size
          , benchIO "fromList" $ fromList size
          , benchIO "fromListM" $ fromListM size
          , benchIO "replicateM" $ replicateM size
          , benchIO "repeatM" $ repeatM size
          , benchIO "iterateM" $ iterateM size
          , benchIO "fromIndicesM" $ fromIndicesM size
          , benchIO "enumerateFromThenIntegral" $ enumerateFromThenIntegral size
          , benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral size
          , benchIO "enumerateFromIntegral" $ enumerateFromIntegral size
          , benchIO "enumerateFromStepNum" $ enumerateFromStepNum size
          , benchIO "enumerateFromNum" $ enumerateFromNum size
          , benchIO "enumerateFromToFractional" $ enumerateFromToFractional size
          ]
    ]

o_1_space_transformation :: Int -> [Benchmark]
o_1_space_transformation size =
    [ bgroup
          "transformation"
          [ benchIO "map" $ map size
          , benchIO "mapM" $ mapM size
          , benchIO "mapM2" $ mapM2 size
          , benchIO "postscan" $ postscan size
          ]
    ]

o_1_space_filtering :: Int -> [Benchmark]
o_1_space_filtering size =
    [ bgroup
          "filtering"
          [ benchIO "takeWhileM" $ takeWhileM size
          , benchIO "takeWhile" $ takeWhile size
          , benchIO "take" $ take size
          , benchIO "filter" $ filter size
          , benchIO "filterM" $ filterM size
          , benchIO "dropOne" $ dropOne size
          , benchIO "dropAll" $ dropAll size
          , benchIO "dropWhileTrue" $ dropWhileTrue size
          , benchIO "dropWhileFalse" $ dropWhileFalse size
          , benchIO "dropWhileMTrue" $ dropWhileMTrue size
          , benchIO "dropWhileMFalse" $ dropWhileMFalse size
          ]
    ]

o_1_space_zip :: Int -> [Benchmark]
o_1_space_zip size =
    [ bgroup
          "zip"
          [ benchIO "zipWithM" $ zipWithM size
          , benchIO "zipWith" $ zipWith size
          , benchIO "teeZipWith" $ teeZipWith size
          , benchIO "interleave" $ interleave size
          ]
    ]

lf :: Word8
lf = fromIntegral (ord '\n')

-- | Split on line feed.
foldManySepBy :: Handle -> IO Int
foldManySepBy =
    let u = UF.foldMany (FL.takeEndBy_ (== lf) FL.drain) FH.reader
     in UF.fold FL.length u

o_1_space_nested :: BenchEnv -> Int -> [Benchmark]
o_1_space_nested env size =
    [ bgroup
          "nested"
          [ benchIO "crossApply outer=inner=(sqrt Max)" $ toNullAp size
          , benchIO "crossApplyFst outer=inner=(sqrt Max)" $ crossApplyFst size
          , benchIO "crossApplySnd outer=inner=(sqrt Max)" $ crossApplySnd size
          , benchIO "cross outer=inner=(sqrt Max)" $ cross size
          , benchIO "fairCross outer=inner=(sqrt Max)" $ fairCross size

          , benchIO "concatMapM outer=inner=(sqrt Max)" $ concatMapM sqrtVal sqrtVal
          , benchIO "bind2" $ toNull size
          , benchIO "bind3" $ toNull3 size
          , benchIO "breakAfterSome2" $ breakAfterSome size
          , benchIO "filterAllOut2" $ filterAllOut size
          , benchIO "filterAllIn2" $ filterAllIn size
          , benchIO "filterSome2" $ filterSome size

          , benchIO "unfoldEach inner=outer=(sqrt Max)" $ unfoldEach sqrtVal sqrtVal
          , benchIO "unfoldEach inner=1 outer=Max" $ unfoldEach 1 size
          , benchIO "unfoldEach inner=Max outer=1" $ unfoldEach size 1
          , benchIO "unfoldEachInterleave inner=outer=(sqrt Max)"
            $ unfoldEachInterleave sqrtVal sqrtVal
          , benchIO "unfoldEachInterleave inner=1 outer=Max"
            $ unfoldEachInterleave 1 size
          , benchIO "unfoldEachInterleave inner=Max outer=1"
            $ unfoldEachInterleave size 1
          , mkBench "foldMany (Fold.takeEndBy_ (== lf) Fold.drain)" env
            $ \inh _ -> foldManySepBy inh
          ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)

o_n_space_nested :: Int -> [Benchmark]
o_n_space_nested size =
    [ bgroup
          "nested"
          [ benchIO "toList2" $ toList size
          , benchIO "toListSome2" $ toListSome size
          ]
    ]

-------------------------------------------------------------------------------
-- Unfold Exception Benchmarks
-------------------------------------------------------------------------------
-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionUnfold :: Handle -> Handle -> IO ()
readWriteOnExceptionUnfold inh devNull =
    let readEx = UF.onException (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'readWriteOnExceptionUnfold
-- inspect $ 'readWriteOnExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionUnfold :: Handle -> Handle -> IO ()
readWriteHandleExceptionUnfold inh devNull =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = UF.handle (UF.functionM handler) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
-- hasNoTypeClasses started failing in GHC 9.4.4
-- inspect $ hasNoTypeClasses 'readWriteHandleExceptionUnfold
-- inspect $ 'readWriteHandleExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteFinally_Unfold :: Handle -> Handle -> IO ()
readWriteFinally_Unfold inh devNull =
    let readEx = UF.finally_ (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readWriteFinally_Unfold [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readWriteFinally_Unfold
#endif
-- inspect $ 'readWriteFinally_Unfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteBracket_Unfold :: Handle -> Handle -> IO ()
readWriteBracket_Unfold inh devNull =
    let readEx = UF.bracket_ return (\_ -> hClose inh) FH.reader
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readWriteBracket_Unfold [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readWriteBracket_Unfold
#endif
-- inspect $ 'readWriteBracket_Unfold `hasNoType` ''S.Step
#endif

o_1_space_copy_read_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_read_exceptions env =
    [ bgroup "exceptions"
       [ mkBenchSmall "UF.onException" env $ \inh _ ->
           readWriteOnExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.handle" env $ \inh _ ->
           readWriteHandleExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.finally_" env $ \inh _ ->
           readWriteFinally_Unfold inh (nullH env)
       , mkBenchSmall "UF.bracket_" env $ \inh _ ->
           readWriteBracket_Unfold inh (nullH env)
        ]
    ]


-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env size =
        [ bgroup (o_1_space_prefix moduleName)
            $ Prelude.concat
                  [ o_1_space_transformation_input size
                  , o_1_space_generation size
                  , o_1_space_transformation size
                  , o_1_space_filtering size
                  , o_1_space_zip size
                  , o_1_space_nested env size
                  , o_1_space_copy_read_exceptions env
                  ]
        , bgroup (o_n_space_prefix moduleName)
            $ Prelude.concat [o_n_space_nested size]
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    lmapM value 0
    return ()
#endif
