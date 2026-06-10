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
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Control.Monad.Catch (MonadCatch)
import GHC.Types (SPEC(..))
import Test.Inspection

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
second size start =
    drainTransformation
        (UF.take size UF.enumerateFromThenIntegral)
        (UF.supplySecond 1)
        start

#ifdef INSPECTION
inspect $ 'second `hasNoType` ''S.Step
inspect $ 'second `hasNoType` ''FL.Step
inspect $ 'second `hasNoType` ''SPEC
#endif

{-# INLINE discardFirst #-}
discardFirst :: Int -> Int -> IO ()
discardFirst size start =
    drainTransformationDefault (size + start) UF.discardFirst (start, start)

#ifdef INSPECTION
inspect $ 'discardFirst `hasNoType` ''S.Step
inspect $ 'discardFirst `hasNoType` ''FL.Step
inspect $ 'discardFirst `hasNoType` ''SPEC
#endif

{-# INLINE discardSecond #-}
discardSecond :: Int -> Int -> IO ()
discardSecond size start =
    drainTransformationDefault (size + start) UF.discardSecond (start, start)

#ifdef INSPECTION
inspect $ 'discardSecond `hasNoType` ''S.Step
inspect $ 'discardSecond `hasNoType` ''FL.Step
inspect $ 'discardSecond `hasNoType` ''SPEC
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

{-# INLINE fromStream #-}
fromStream :: Int -> Int -> IO ()
fromStream size start =
    drainGeneration UF.fromStream (S.replicate size start :: S.Stream IO Int)

-- 'fromStream', 'fromStreamD' and 'consM' wrap an opaque stream/cons cell, so
-- the 'Step' is not eliminated.
#ifdef INSPECTION
-- inspect $ 'fromStream `hasNoType` ''S.Step
inspect $ 'fromStream `hasNoType` ''FL.Step
inspect $ 'fromStream `hasNoType` ''SPEC
#endif

-- XXX INVESTIGATE: Although the performance of this should be equivalant to
-- fromStream, this is considerably worse. More than 4x worse.
{-# INLINE fromStreamK #-}
fromStreamK :: Int -> Int -> IO ()
fromStreamK size start = drainGeneration UF.fromStreamK (K.replicate size start)

#ifdef INSPECTION
inspect $ 'fromStreamK `hasNoType` ''S.Step
inspect $ 'fromStreamK `hasNoType` ''FL.Step
inspect $ 'fromStreamK `hasNoType` ''SPEC
#endif

{-# INLINE fromStreamD #-}
fromStreamD :: Int -> Int -> IO ()
fromStreamD size start =
    drainGeneration UF.fromStreamD (S.replicate size start)

#ifdef INSPECTION
-- inspect $ 'fromStreamD `hasNoType` ''S.Step
inspect $ 'fromStreamD `hasNoType` ''FL.Step
inspect $ 'fromStreamD `hasNoType` ''SPEC
#endif

-- 'nilM' runs its action on the seed but yields no output, so unfold it over an
-- outer source of value seeds to run it ~value times.
{-# INLINE nilM #-}
nilM :: Int -> Int -> IO ()
nilM value start =
    drainGeneration (UF.unfoldEach (UF.nilM return) (source (start + value))) start

#ifdef INSPECTION
inspect $ 'nilM `hasNoType` ''S.Step
inspect $ 'nilM `hasNoType` ''FL.Step
inspect $ 'nilM `hasNoType` ''SPEC
inspect $ 'nilM `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE nil #-}
nil :: Int -> Int -> IO ()
nil value start =
    drainGeneration (UF.unfoldEach UF.nil (source (start + value))) start

#ifdef INSPECTION
inspect $ 'nil `hasNoType` ''S.Step
inspect $ 'nil `hasNoType` ''FL.Step
inspect $ 'nil `hasNoType` ''SPEC
inspect $ 'nil `hasNoType` ''Producer.ConcatState
#endif

{-# INLINE consM #-}
consM :: Int -> Int -> IO ()
consM size start =
    drainTransformationDefault (size + start) (UF.consM return) start

#ifdef INSPECTION
-- inspect $ 'consM `hasNoType` ''S.Step
inspect $ 'consM `hasNoType` ''FL.Step
inspect $ 'consM `hasNoType` ''SPEC
#endif

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

{-# INLINE fromListM #-}
fromListM :: Int -> Int -> IO ()
fromListM size start =
    drainGeneration UF.fromListM (Prelude.map return [start .. start + size])

#ifdef INSPECTION
inspect $ 'fromListM `hasNoType` ''S.Step
inspect $ 'fromListM `hasNoType` ''FL.Step
inspect $ 'fromListM `hasNoType` ''SPEC
#endif

{-# INLINE _fromSVar #-}
_fromSVar :: Int -> Int -> m ()
_fromSVar = undefined

{-# INLINE _fromProducer #-}
_fromProducer :: Int -> Int -> m ()
_fromProducer = undefined

{-# INLINE replicateM #-}
replicateM :: Int -> Int -> IO ()
replicateM size start = drainGeneration UF.replicateM (size, return start)

#ifdef INSPECTION
inspect $ 'replicateM `hasNoType` ''S.Step
inspect $ 'replicateM `hasNoType` ''FL.Step
inspect $ 'replicateM `hasNoType` ''SPEC
#endif

{-# INLINE repeatM #-}
repeatM :: Int -> Int -> IO ()
repeatM size start = drainGeneration (UF.take size UF.repeatM) (return start)

#ifdef INSPECTION
inspect $ 'repeatM `hasNoType` ''S.Step
inspect $ 'repeatM `hasNoType` ''FL.Step
inspect $ 'repeatM `hasNoType` ''SPEC
#endif

{-# INLINE repeat #-}
repeat :: Int -> Int -> IO ()
repeat size start = drainGeneration (UF.take size UF.repeat) start

#ifdef INSPECTION
inspect $ 'repeat `hasNoType` ''S.Step
inspect $ 'repeat `hasNoType` ''FL.Step
inspect $ 'repeat `hasNoType` ''SPEC
#endif

{-# INLINE iterateM #-}
iterateM :: Int -> Int -> IO ()
iterateM size start =
    drainGeneration (UF.take size (UF.iterateM return)) (return start)

#ifdef INSPECTION
inspect $ 'iterateM `hasNoType` ''S.Step
inspect $ 'iterateM `hasNoType` ''FL.Step
inspect $ 'iterateM `hasNoType` ''SPEC
#endif

{-# INLINE fromIndicesM #-}
fromIndicesM :: Int -> Int -> IO ()
fromIndicesM size start =
    drainGeneration (UF.take size (UF.fromIndicesM return)) start

#ifdef INSPECTION
inspect $ 'fromIndicesM `hasNoType` ''S.Step
inspect $ 'fromIndicesM `hasNoType` ''FL.Step
inspect $ 'fromIndicesM `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: Int -> Int -> IO ()
enumerateFromThenIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromThenIntegral) (start, 1)

#ifdef INSPECTION
inspect $ 'enumerateFromThenIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromThenIntegral `hasNoType` ''FL.Step
inspect $ 'enumerateFromThenIntegral `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Int -> Int -> IO ()
enumerateFromToIntegral size start =
    drainGeneration
    ( UF.supplySecond
      (size + start)
      UF.enumerateFromToIntegral
    ) start

#ifdef INSPECTION
inspect $ 'enumerateFromToIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromToIntegral `hasNoType` ''FL.Step
inspect $ 'enumerateFromToIntegral `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: Int -> Int -> IO ()
enumerateFromIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromIntegral) start

#ifdef INSPECTION
inspect $ 'enumerateFromIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromIntegral `hasNoType` ''FL.Step
inspect $ 'enumerateFromIntegral `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: Int -> Int -> IO ()
enumerateFromStepNum size start =
    drainGeneration (UF.take size (UF.enumerateFromThenNum)) (start, 1)

#ifdef INSPECTION
inspect $ 'enumerateFromStepNum `hasNoType` ''S.Step
inspect $ 'enumerateFromStepNum `hasNoType` ''FL.Step
inspect $ 'enumerateFromStepNum `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromNum #-}
enumerateFromNum :: Int -> Int -> IO ()
enumerateFromNum size start = drainGeneration (UF.take size UF.enumerateFromNum) start

#ifdef INSPECTION
inspect $ 'enumerateFromNum `hasNoType` ''S.Step
inspect $ 'enumerateFromNum `hasNoType` ''FL.Step
inspect $ 'enumerateFromNum `hasNoType` ''SPEC
#endif

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

#ifdef INSPECTION
inspect $ 'enumerateFromToFractional `hasNoType` ''S.Step
inspect $ 'enumerateFromToFractional `hasNoType` ''FL.Step
inspect $ 'enumerateFromToFractional `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE postscan #-}
postscan :: Int -> Int -> IO ()
postscan size start =
    drainTransformationDefault (size + start) (UF.postscanl Scanl.sum) start

#ifdef INSPECTION
inspect $ 'postscan `hasNoType` ''S.Step
inspect $ 'postscan `hasNoType` ''FL.Step
inspect $ 'postscan `hasNoType` ''SPEC
#endif

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
mapM2 size start =
    drainTransformationDefault
        size
        (UF.mapM (\(a, b) -> return $ a + b) . UF.carryInput)
        start

#ifdef INSPECTION
inspect $ 'mapM2 `hasNoType` ''S.Step
inspect $ 'mapM2 `hasNoType` ''FL.Step
inspect $ 'mapM2 `hasNoType` ''SPEC
#endif

{-# INLINE scanl #-}
scanl :: Int -> Int -> IO ()
scanl size start =
    drainTransformationDefault (size + start) (UF.scanl Scanl.sum) start

#ifdef INSPECTION
inspect $ 'scanl `hasNoType` ''S.Step
inspect $ 'scanl `hasNoType` ''FL.Step
inspect $ 'scanl `hasNoType` ''SPEC
#endif

{-# INLINE scanlMany #-}
scanlMany :: Int -> Int -> IO ()
scanlMany size start =
    drainTransformationDefault (size + start) (UF.scanlMany (Scanl.take 2 Scanl.sum)) start

#ifdef INSPECTION
inspect $ 'scanlMany `hasNoType` ''S.Step
inspect $ 'scanlMany `hasNoType` ''FL.Step
inspect $ 'scanlMany `hasNoType` ''SPEC
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

{-# INLINE take #-}
take :: Int -> Int -> IO ()
take size start = drainTransformationDefault (size + start) (UF.take size) start

#ifdef INSPECTION
inspect $ 'take `hasNoType` ''S.Step
inspect $ 'take `hasNoType` ''FL.Step
inspect $ 'take `hasNoType` ''SPEC
#endif

{-# INLINE filter #-}
filter :: Int -> Int -> IO ()
filter size start =
    drainTransformationDefault (size + start) (UF.filter (\_ -> True)) start

#ifdef INSPECTION
inspect $ 'filter `hasNoType` ''S.Step
inspect $ 'filter `hasNoType` ''FL.Step
inspect $ 'filter `hasNoType` ''SPEC
#endif

{-# INLINE filterM #-}
filterM :: Int -> Int -> IO ()
filterM size start =
    drainTransformationDefault
        (size + start)
        (UF.filterM (\_ -> (return True)))
        start

#ifdef INSPECTION
inspect $ 'filterM `hasNoType` ''S.Step
inspect $ 'filterM `hasNoType` ''FL.Step
inspect $ 'filterM `hasNoType` ''SPEC
#endif

-- Dropping one element from a large stream is dominated by generation, so
-- instead exercise 'drop' ~value/2 times: generate value/2 two-element streams
-- with 'fromTuple', 'drop' the first element of each, and flatten the rest.
{-# INLINE dropOne #-}
dropOne :: Int -> Int -> IO ()
dropOne value start =
    let outer = UF.map (\i -> (i, i)) (source (start + value `div` 2))
     in drainGeneration (UF.unfoldEach (UF.drop 1 UF.fromTuple) outer) start

#ifdef INSPECTION
inspect $ 'dropOne `hasNoType` ''S.Step
inspect $ 'dropOne `hasNoType` ''FL.Step
inspect $ 'dropOne `hasNoType` ''SPEC
inspect $ 'dropOne `hasNoType` ''Producer.ConcatState
inspect $ 'dropOne `hasNoType` ''Producer.TupleState
#endif

{-# INLINE dropAll #-}
dropAll :: Int -> Int -> IO ()
dropAll size start =
    drainTransformationDefault (size + start) (UF.drop (size + 1)) start

#ifdef INSPECTION
inspect $ 'dropAll `hasNoType` ''S.Step
inspect $ 'dropAll `hasNoType` ''FL.Step
inspect $ 'dropAll `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Int -> Int -> IO ()
dropWhileTrue size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return True))
        start

#ifdef INSPECTION
inspect $ 'dropWhileTrue `hasNoType` ''S.Step
inspect $ 'dropWhileTrue `hasNoType` ''FL.Step
inspect $ 'dropWhileTrue `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Int -> Int -> IO ()
dropWhileFalse size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return False))
        start

#ifdef INSPECTION
inspect $ 'dropWhileFalse `hasNoType` ''S.Step
inspect $ 'dropWhileFalse `hasNoType` ''FL.Step
inspect $ 'dropWhileFalse `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: Int -> Int -> IO ()
dropWhileMTrue size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return True))
        start

#ifdef INSPECTION
inspect $ 'dropWhileMTrue `hasNoType` ''S.Step
inspect $ 'dropWhileMTrue `hasNoType` ''FL.Step
inspect $ 'dropWhileMTrue `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileMFalse #-}
dropWhileMFalse :: Int -> Int -> IO ()
dropWhileMFalse size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return False))
        start

#ifdef INSPECTION
inspect $ 'dropWhileMFalse `hasNoType` ''S.Step
inspect $ 'dropWhileMFalse `hasNoType` ''FL.Step
inspect $ 'dropWhileMFalse `hasNoType` ''SPEC
#endif

{-# INLINE dropWhile #-}
dropWhile :: Int -> Int -> IO ()
dropWhile size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhile (\_ -> False))
        start

#ifdef INSPECTION
inspect $ 'dropWhile `hasNoType` ''S.Step
inspect $ 'dropWhile `hasNoType` ''FL.Step
inspect $ 'dropWhile `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybe #-}
mapMaybe :: Int -> Int -> IO ()
mapMaybe size start =
    drainTransformationDefault (size + start) (UF.mapMaybe Just) start

#ifdef INSPECTION
inspect $ 'mapMaybe `hasNoType` ''S.Step
inspect $ 'mapMaybe `hasNoType` ''FL.Step
inspect $ 'mapMaybe `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybeM #-}
mapMaybeM :: Int -> Int -> IO ()
mapMaybeM size start =
    drainTransformationDefault (size + start) (UF.mapMaybeM (return . Just)) start

#ifdef INSPECTION
inspect $ 'mapMaybeM `hasNoType` ''S.Step
inspect $ 'mapMaybeM `hasNoType` ''FL.Step
inspect $ 'mapMaybeM `hasNoType` ''SPEC
#endif

{-# INLINE catMaybes #-}
catMaybes :: Int -> Int -> IO ()
catMaybes size start =
    drainTransformationDefault (size + start) (UF.catMaybes . UF.map Just) start

#ifdef INSPECTION
inspect $ 'catMaybes `hasNoType` ''S.Step
inspect $ 'catMaybes `hasNoType` ''FL.Step
inspect $ 'catMaybes `hasNoType` ''SPEC
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

{-# INLINE eitherLeft #-}
eitherLeft :: Int -> Int -> IO ()
eitherLeft size start =
    drainGeneration
        (UF.either (source (size + start)) (source (size + start)))
        (Left start)

#ifdef INSPECTION
inspect $ 'eitherLeft `hasNoType` ''S.Step
inspect $ 'eitherLeft `hasNoType` ''FL.Step
inspect $ 'eitherLeft `hasNoType` ''SPEC
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

{-# INLINE zipRepeat #-}
zipRepeat :: Int -> Int -> IO ()
zipRepeat size start =
    drainGeneration (UF.zipRepeat (source (size + start))) (start, start)

#ifdef INSPECTION
inspect $ 'zipRepeat `hasNoType` ''S.Step
inspect $ 'zipRepeat `hasNoType` ''FL.Step
inspect $ 'zipRepeat `hasNoType` ''SPEC
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

{-# INLINE innerJoin #-}
innerJoin :: Int -> Int -> IO ()
innerJoin value start =
    let end = start + nthRoot 2 value
        s = source end
    in UF.fold FL.drain (UF.innerJoin (==) s s) start

#ifdef INSPECTION
inspect $ 'innerJoin `hasNoType` ''S.Step
inspect $ 'innerJoin `hasNoType` ''FL.Step
inspect $ 'innerJoin `hasNoType` ''SPEC
inspect $ 'innerJoin `hasNoType` ''Producer.CrossState
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

{-# INLINE unfoldEachInterleave #-}
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
-- Resource management
-------------------------------------------------------------------------------

{-# INLINE before #-}
before :: Int -> Int -> IO ()
before size start =
    drainTransformationDefault (size + start) (UF.before (\_ -> return ())) start

#ifdef INSPECTION
inspect $ 'before `hasNoType` ''S.Step
inspect $ 'before `hasNoType` ''FL.Step
inspect $ 'before `hasNoType` ''SPEC
#endif

{-# INLINE after_ #-}
after_ :: Int -> Int -> IO ()
after_ size start =
    drainTransformationDefault (size + start) (UF.after_ (\_ -> return ())) start

#ifdef INSPECTION
inspect $ 'after_ `hasNoType` ''S.Step
inspect $ 'after_ `hasNoType` ''FL.Step
inspect $ 'after_ `hasNoType` ''SPEC
#endif

{-# INLINE afterIO #-}
afterIO :: Int -> Int -> IO ()
afterIO size start =
    UF.fold FL.drain
        (UF.afterIO (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToIntegral))
        start

#ifdef INSPECTION
inspect $ 'afterIO `hasNoType` ''S.Step
inspect $ 'afterIO `hasNoType` ''FL.Step
inspect $ 'afterIO `hasNoType` ''SPEC
#endif

{-# INLINE finallyIO #-}
finallyIO :: Int -> Int -> IO ()
finallyIO size start =
    UF.fold FL.drain
        (UF.finallyIO (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToIntegral))
        start

-- 'finallyIO' and 'bracketIO' wrap the step function in exception handlers,
-- so 'Step' constructors are not eliminated.
#ifdef INSPECTION
-- inspect $ 'finallyIO `hasNoType` ''S.Step
inspect $ 'finallyIO `hasNoType` ''FL.Step
inspect $ 'finallyIO `hasNoType` ''SPEC
#endif

{-# INLINE bracketIO #-}
bracketIO :: Int -> Int -> IO ()
bracketIO size start =
    UF.fold FL.drain
        (UF.bracketIO return (\_ -> return ())
            (UF.supplySecond (size + start) UF.enumerateFromToIntegral))
        start

#ifdef INSPECTION
-- inspect $ 'bracketIO `hasNoType` ''S.Step
inspect $ 'bracketIO `hasNoType` ''FL.Step
inspect $ 'bracketIO `hasNoType` ''SPEC
#endif

lf :: Word8
lf = fromIntegral (ord '\n')

-- | Split on line feed.
foldManySepBy :: Handle -> IO Int
foldManySepBy =
    let u = UF.foldMany (FL.takeEndBy_ (== lf) FL.drain) FH.reader
     in UF.fold FL.length u

-- Handle-based fold splitting ('FoldMany' Fuse annotation is disabled so
-- 'Step' is not eliminated, but 'FL.Step' and 'SPEC' should still vanish.)
#ifdef INSPECTION
-- inspect $ 'foldManySepBy `hasNoType` ''S.Step
inspect $ 'foldManySepBy `hasNoType` ''FL.Step
inspect $ 'foldManySepBy `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_transformation_input :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_transformation_input size =
    [ (SpaceO_1, bgroup
          "transformation/input"
          [ benchIO "lmap" $ lmap size
          , benchIO "lmapM" $ lmapM size
          , benchIO "both" $ both size
          , benchIO "first" $ first size
          , benchIO "second" $ second size
          , benchIO "discardFirst" $ discardFirst size
          , benchIO "discardSecond" $ discardSecond size
          , benchIO "consInput" $ consInput size
          , benchIO "consInputWith" $ consInputWith size
          , benchIO "swap" $ swap size
          ])
    ]

o_1_space_generation :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_generation size =
    [ (SpaceO_1, bgroup
          "generation"
          [ benchIO "fromStream" $ fromStream size
          , benchIO "fromStreamK" $ fromStreamK size
          , benchIO "fromStreamD" $ fromStreamD size
          , benchIO "nilM" $ nilM size
          , benchIO "nil" $ nil size
          , benchIO "consM" $ consM size
          , benchIO "functionM" $ functionM size
          , benchIO "function" $ function size
          , benchIO "identity" $ identity size
          , benchIO "fromEffect" $ fromEffect size
          , benchIO "fromPure" $ fromPure size
          , benchIO "functionMaybeM" $ functionMaybeM size
          , benchIO "fromTuple" $ fromTuple size
          , benchIO "unfoldrM" $ unfoldrM size
          , benchIO "unfoldr" $ unfoldr size
          , benchIO "fromList" $ fromList size
          , benchIO "fromListM" $ fromListM size
          , benchIO "replicateM" $ replicateM size
          , benchIO "repeatM" $ repeatM size
          , benchIO "repeat" $ repeat size
          , benchIO "iterateM" $ iterateM size
          , benchIO "fromIndicesM" $ fromIndicesM size
          , benchIO "enumerateFromThenIntegral" $ enumerateFromThenIntegral size
          , benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral size
          , benchIO "enumerateFromIntegral" $ enumerateFromIntegral size
          , benchIO "enumerateFromStepNum" $ enumerateFromStepNum size
          , benchIO "enumerateFromNum" $ enumerateFromNum size
          , benchIO "enumerateFromToFractional" $ enumerateFromToFractional size
          ])
    ]

o_1_space_transformation :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_transformation size =
    [ (SpaceO_1, bgroup
          "transformation"
          [ benchIO "map" $ map size
          , benchIO "mapM" $ mapM size
          , benchIO "mapM2" $ mapM2 size
          , benchIO "postscan" $ postscan size
          , benchIO "scanl" $ scanl size
          , benchIO "scanlMany" $ scanlMany size
          ])
    ]

o_1_space_filtering :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_filtering size =
    [ (SpaceO_1, bgroup
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
          , benchIO "dropWhile" $ dropWhile size
          , benchIO "mapMaybe" $ mapMaybe size
          , benchIO "mapMaybeM" $ mapMaybeM size
          , benchIO "catMaybes" $ catMaybes size
          ])
    ]

o_1_space_zip :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_zip size =
    [ (SpaceO_1, bgroup
          "zip"
          [ benchIO "zipWithM" $ zipWithM size
          , benchIO "zipWith" $ zipWith size
          , benchIO "teeZipWith" $ teeZipWith size
          , benchIO "interleave" $ interleave size
          , benchIO "eitherLeft" $ eitherLeft size
          , benchIO "zipArrowWithM" $ zipArrowWithM size
          , benchIO "zipArrowWith" $ zipArrowWith size
          , benchIO "zipRepeat" $ zipRepeat size
          ])
    ]

o_1_space_nested :: BenchEnv -> Int -> [(SpaceComplexity, Benchmark)]
o_1_space_nested env size =
    [ (SpaceO_1, bgroup
          "nested"
          [ benchIO "crossApply outer=inner=(sqrt Max)" $ toNullAp size
          , benchIO "crossApplyFst outer=inner=(sqrt Max)" $ crossApplyFst size
          , benchIO "crossApplySnd outer=inner=(sqrt Max)" $ crossApplySnd size
          , benchIO "cross outer=inner=(sqrt Max)" $ cross size
          , benchIO "fairCross outer=inner=(sqrt Max)" $ fairCross size
          , benchIO "crossApply2 outer=inner=(sqrt Max)" $ crossApply size
          , benchIO "crossWithM outer=inner=(sqrt Max)" $ crossWithM size
          , benchIO "crossWith outer=inner=(sqrt Max)" $ crossWith size
          , benchIO "fairCrossWithM outer=inner=(sqrt Max)" $ fairCrossWithM size
          , benchIO "fairCrossWith outer=inner=(sqrt Max)" $ fairCrossWith size
          , benchIO "innerJoin outer=inner=(sqrt Max)" $ innerJoin size

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
          ])
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)

o_1_space_concat :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_concat size =
    [ (SpaceO_1, bgroup
          "concat"
          [ benchIO "concatMapM outer=inner=(sqrt Max)" $ concatMapM sqrtVal sqrtVal
          , benchIO "concatMapPure outer=inner=(sqrt Max)" $ concatMapPure sqrtVal sqrtVal
          , benchIO "bind2" $ toNull size
          , benchIO "bind3" $ toNull3 size
          , benchIO "concatMap2" $ toNullConcatMap size
          , benchIO "concatMap3" $ toNull3ConcatMap size
          , benchIO "breakAfterSome2" $ breakAfterSome size
          , benchIO "filterAllOut2" $ filterAllOut size
          , benchIO "filterAllIn2" $ filterAllIn size
          , benchIO "filterSome2" $ filterSome size
          ])
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double)

o_n_space_concat :: Int -> [(SpaceComplexity, Benchmark)]
o_n_space_concat size =
    [ (SpaceO_n, bgroup
          "concat"
          [ benchIO "toList2" $ toList size
          , benchIO "toListSome2" $ toListSome size
          ])
    ]

o_1_space_resource_management :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_resource_management size =
    [ (SpaceO_1, bgroup
          "resource-management"
          [ benchIO "before" $ before size
          , benchIO "after_" $ after_ size
          , benchIO "afterIO" $ afterIO size
          , benchIO "finallyIO" $ finallyIO size
          , benchIO "bracketIO" $ bracketIO size
          ])
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

o_1_space_copy_read_exceptions :: BenchEnv -> [(SpaceComplexity, Benchmark)]
o_1_space_copy_read_exceptions env =
    [ (SpaceO_1, bgroup "exceptions"
       [ mkBenchSmall "UF.onException" env $ \inh _ ->
           readWriteOnExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.handle" env $ \inh _ ->
           readWriteHandleExceptionUnfold inh (nullH env)
       , mkBenchSmall "UF.finally_" env $ \inh _ ->
           readWriteFinally_Unfold inh (nullH env)
       , mkBenchSmall "UF.bracket_" env $ \inh _ ->
           readWriteBracket_Unfold inh (nullH env)
        ])
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
        let allBenches = Prelude.concat
                  [ o_1_space_transformation_input size
                  , o_1_space_generation size
                  , o_1_space_transformation size
                  , o_1_space_filtering size
                  , o_1_space_zip size
                  , o_1_space_nested env size
                  , o_1_space_concat size
                  , o_1_space_resource_management size
                  , o_1_space_copy_read_exceptions env
                  , o_n_space_concat size
                  ]
            get x = Prelude.map snd $ Prelude.filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    lmapM value 0
    return ()
#endif
