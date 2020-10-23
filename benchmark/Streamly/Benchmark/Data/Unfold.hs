-- |
-- Module      : Streamly.Benchmark.Data.Fold
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Prelude
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Benchmark.Data.NestedUnfoldOps as Nested

import Streamly.Benchmark.Common
import Gauge
import Prelude hiding (concat, take, filter, zipWith, map, mapM, takeWhile)

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE drainGeneration #-}
drainGeneration :: Monad m => Unfold m a b -> a -> m ()
drainGeneration unf seed = UF.fold unf FL.drain seed

{-# INLINE drainTransformation #-}
drainTransformation ::
       Monad m => Unfold m a b -> (Unfold m a b -> Unfold m c d) -> c -> m ()
drainTransformation unf f seed = drainGeneration (f unf) seed

{-# INLINE drainTransformationDefault #-}
drainTransformationDefault ::
       Monad m => Int -> (Unfold m Int Int -> Unfold m c d) -> c -> m ()
drainTransformationDefault to =
    drainTransformation (UF.enumerateFromToIntegral to)

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

    src = UF.enumerateFromToIntegral to

-------------------------------------------------------------------------------
-- Operations on input
-------------------------------------------------------------------------------

{-# INLINE lmap #-}
lmap :: Monad m => Int -> Int -> m ()
lmap size start =
    drainTransformationDefault (size + start) (UF.lmap (+ 1)) start

{-# INLINE lmapM #-}
lmapM :: Monad m => Int -> Int -> m ()
lmapM size start =
    drainTransformationDefault (size + start) (UF.lmapM (return . (+) 1)) start

{-# INLINE supply #-}
supply :: Monad m => Int -> Int -> m ()
supply size start =
    drainTransformationDefault (size + start) (flip UF.supply start) undefined


{-# INLINE supplyFirst #-}
supplyFirst :: Monad m => Int -> Int -> m ()
supplyFirst size start =
    drainTransformation
        (UF.take size UF.enumerateFromStepIntegral)
        (flip UF.supplyFirst start)
        1

{-# INLINE supplySecond #-}
supplySecond :: Monad m => Int -> Int -> m ()
supplySecond size start =
    drainTransformation
        (UF.take size UF.enumerateFromStepIntegral)
        (flip UF.supplySecond 1)
        start

{-# INLINE discardFirst #-}
discardFirst :: Monad m => Int -> Int -> m ()
discardFirst size start =
    drainTransformationDefault (size + start) UF.discardFirst (start, start)

{-# INLINE discardSecond #-}
discardSecond :: Monad m => Int -> Int -> m ()
discardSecond size start =
    drainTransformationDefault (size + start) UF.discardSecond (start, start)

{-# INLINE swap #-}
swap :: Monad m => Int -> Int -> m ()
swap size start =
    drainTransformation
        (UF.take size UF.enumerateFromStepIntegral)
        UF.swap
        (1, start)

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# INLINE fromStream #-}
fromStream :: Int -> Int -> IO ()
fromStream size start =
    drainGeneration UF.fromStream (S.replicate size start :: S.SerialT IO Int)

-- XXX INVESTIGATE: Although the performance of this should be equivalant to
-- fromStream, this is considerably worse. More than 4x worse.
{-# INLINE fromStreamK #-}
fromStreamK :: Monad m => Int -> Int -> m ()
fromStreamK size start = drainGeneration UF.fromStreamK (K.replicate size start)

{-# INLINE fromStreamD #-}
fromStreamD :: Monad m => Int -> Int -> m ()
fromStreamD size start =
    drainGeneration UF.fromStreamD (D.replicate size start)

{-# INLINE _nilM #-}
_nilM :: Monad m => Int -> Int -> m ()
_nilM _ start = drainGeneration (UF.nilM return) start

{-# INLINE consM #-}
consM :: Monad m => Int -> Int -> m ()
consM size start =
    drainTransformationDefault (size + start) (UF.consM return) start

{-# INLINE _effect #-}
_effect :: Monad m => Int -> Int -> m ()
_effect _ start =
    drainGeneration (UF.effect (return start)) undefined

{-# INLINE _singletonM #-}
_singletonM :: Monad m => Int -> Int -> m ()
_singletonM _ start = drainGeneration (UF.singletonM return) start

{-# INLINE _singleton #-}
_singleton :: Monad m => Int -> Int -> m ()
_singleton _ start = drainGeneration (UF.singleton id) start

{-# INLINE _identity #-}
_identity :: Monad m => Int -> Int -> m ()
_identity _ start = drainGeneration UF.identity start

{-# INLINE _const #-}
_const :: Monad m => Int -> Int -> m ()
_const size start =
    drainGeneration (UF.take size (UF.const (return start))) undefined

{-# INLINE unfoldrM #-}
unfoldrM :: Monad m => Int -> Int -> m ()
unfoldrM size start = drainGeneration (UF.unfoldrM step) start

    where

    step i =
        return
            $ if i < start + size
              then Just (i, i + 1)
              else Nothing

{-# INLINE fromList #-}
fromList :: Monad m => Int -> Int -> m ()
fromList size start = drainGeneration UF.fromList [start .. start + size]

{-# INLINE fromListM #-}
fromListM :: Monad m => Int -> Int -> m ()
fromListM size start =
    drainGeneration UF.fromListM (Prelude.map return [start .. start + size])

{-# INLINE _fromSVar #-}
_fromSVar :: Int -> Int -> m ()
_fromSVar = undefined

{-# INLINE _fromProducer #-}
_fromProducer :: Int -> Int -> m ()
_fromProducer = undefined

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> m ()
replicateM size start = drainGeneration (UF.replicateM size) (return start)

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> m ()
repeatM size start = drainGeneration (UF.take size UF.repeatM) (return start)

{-# INLINE iterateM #-}
iterateM :: Monad m => Int -> Int -> m ()
iterateM size start =
    drainGeneration (UF.take size (UF.iterateM return)) (return start)

{-# INLINE fromIndicesM #-}
fromIndicesM :: Monad m => Int -> Int -> m ()
fromIndicesM size start =
    drainGeneration (UF.take size (UF.fromIndicesM return)) start

{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: Monad m => Int -> Int -> m ()
enumerateFromStepIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromStepIntegral) (start, 1)

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Monad m => Int -> Int -> m ()
enumerateFromToIntegral size start =
    drainGeneration (UF.enumerateFromToIntegral (size + start)) start

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: Monad m => Int -> Int -> m ()
enumerateFromIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromIntegral) start

{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: Monad m => Int -> Int -> m ()
enumerateFromStepNum size start =
    drainGeneration (UF.take size (UF.enumerateFromStepNum 1)) start

{-# INLINE numFrom #-}
numFrom :: Monad m => Int -> Int -> m ()
numFrom size start = drainGeneration (UF.take size UF.numFrom) start

{-# INLINE enumerateFromToFractional #-}
enumerateFromToFractional :: Monad m => Int -> Int -> m ()
enumerateFromToFractional size start =
    let intToDouble x = (fromInteger (fromIntegral x)) :: Double
     in drainGeneration
            (UF.enumerateFromToFractional (intToDouble $ start + size))
            (intToDouble start)

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE map #-}
map :: Monad m => Int -> Int -> m ()
map size start = drainTransformationDefault (size + start) (UF.map (+1)) start

{-# INLINE mapM #-}
mapM :: Monad m => Int -> Int -> m ()
mapM size start =
    drainTransformationDefault (size + start) (UF.mapM (return . (+) 1)) start

{-# INLINE mapMWithInput #-}
mapMWithInput :: Monad m => Int -> Int -> m ()
mapMWithInput size start =
    drainTransformationDefault
        size
        (UF.mapMWithInput (\a b -> return $ a + b))
        start

-------------------------------------------------------------------------------
-- Stream filtering
-------------------------------------------------------------------------------

{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => Int -> Int -> m ()
takeWhileM size start =
    drainTransformationDefault
        size
        (UF.takeWhileM (\b -> return (b <= size + start)))
        start

{-# INLINE takeWhile #-}
takeWhile :: Monad m => Int -> Int -> m ()
takeWhile size start =
    drainTransformationDefault
        size
        (UF.takeWhile (\b -> b <= size + start))
        start

{-# INLINE take #-}
take :: Monad m => Int -> Int -> m ()
take size start = drainTransformationDefault (size + start) (UF.take size) start

{-# INLINE filter #-}
filter :: Monad m => Int -> Int -> m ()
filter size start =
    drainTransformationDefault (size + start) (UF.filter (\_ -> True)) start

{-# INLINE filterM #-}
filterM :: Monad m => Int -> Int -> m ()
filterM size start =
    drainTransformationDefault
        (size + start)
        (UF.filterM (\_ -> (return True)))
        start

{-# INLINE _dropOne #-}
_dropOne :: Monad m => Int -> Int -> m ()
_dropOne size start =
    drainTransformationDefault (size + start) (UF.drop 1) start

{-# INLINE dropAll #-}
dropAll :: Monad m => Int -> Int -> m ()
dropAll size start =
    drainTransformationDefault (size + start) (UF.drop (size + 1)) start

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: Monad m => Int -> Int -> m ()
dropWhileTrue size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return True))
        start

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: Monad m => Int -> Int -> m ()
dropWhileFalse size start =
    drainTransformationDefault
        (size + start)
        (UF.dropWhileM (\_ -> return False))
        start

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: Monad m => Int -> Int -> m ()
dropWhileMTrue size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return True))
        start

{-# INLINE dropWhileMFalse #-}
dropWhileMFalse :: Monad m => Int -> Int -> m ()
dropWhileMFalse size start =
    drainTransformationDefault
        size
        (UF.dropWhileM (\_ -> return False))
        start

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: Monad m => Int -> Int -> m ()
zipWith size start =
    drainProductDefault (size + start) (UF.zipWith (+)) (start, start + 1)

{-# INLINE zipWithM #-}
zipWithM :: Monad m => Int -> Int -> m ()
zipWithM size start =
    drainProductDefault
        (size + start)
        (UF.zipWithM (\a b -> return $ a + b))
        (start, start + 1)

{-# INLINE teeZipWith #-}
teeZipWith :: Monad m => Int -> Int -> m ()
teeZipWith size start =
    drainProductDefault (size + start) (UF.teeZipWith (+)) start

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE concatMapM #-}
concatMapM :: Monad m => Int -> Int -> m ()
concatMapM size start =
    drainGeneration (UF.concatMapM unfoldInGen unfoldOut) start

    where

    sizeOuter = 100
    sizeInner = size `div` sizeOuter

    unfoldInGen i =
        return
            $ UF.lmap (\() -> undefined)
            $ UF.supply (UF.enumerateFromToIntegral (i + sizeInner)) i

    unfoldOut = UF.enumerateFromToIntegral (start + sizeOuter)

{-# INLINE _ap #-}
_ap :: Int -> Int -> m ()
_ap = undefined

{-# INLINE _apDiscardFst #-}
_apDiscardFst :: Int -> Int -> m ()
_apDiscardFst = undefined

{-# INLINE _apDiscardSnd #-}
_apDiscardSnd :: Int -> Int -> m ()
_apDiscardSnd = undefined

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
          , benchIO "supply" $ supply size
          , benchIO "supplyFirst" $ supplyFirst size
          , benchIO "supplySecond" $ supplySecond size
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
          , benchIO "fromStreamD (n/100)" $ fromStreamD (size `div` 100)
          -- Very small benchmarks, reporting in ns
          -- , benchIO "nilM" $ nilM size
          , benchIO "consM" $ consM size
          -- , benchIO "effect" $ effect size
          -- , benchIO "singletonM" $ singletonM size
          -- , benchIO "singleton" $ singleton size
          -- , benchIO "identity" $ identity size
          -- , benchIO "const" $ const size
          , benchIO "unfoldrM" $ unfoldrM size
          , benchIO "fromList" $ fromList size
          , benchIO "fromListM" $ fromListM size
          -- Unimplemented
          -- , benchIO "fromSVar" $ fromSVar size
          -- , benchIO "fromProducer" $ fromProducer size
          , benchIO "replicateM" $ replicateM size
          , benchIO "repeatM" $ repeatM size
          , benchIO "iterateM" $ iterateM size
          , benchIO "fromIndicesM" $ fromIndicesM size
          , benchIO "enumerateFromStepIntegral" $ enumerateFromStepIntegral size
          , benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral size
          , benchIO "enumerateFromIntegral" $ enumerateFromIntegral size
          , benchIO "enumerateFromStepNum" $ enumerateFromStepNum size
          , benchIO "numFrom" $ numFrom size
          , benchIO "enumerateFromToFractional" $ enumerateFromToFractional size
          ]
    ]

o_1_space_transformation :: Int -> [Benchmark]
o_1_space_transformation size =
    [ bgroup
          "transformation"
          [ benchIO "map" $ map size
          , benchIO "mapM" $ mapM size
          , benchIO "mapMWithInput" $ mapMWithInput size
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
          -- Very small benchmark, reporting in ns
          -- , benchIO "dropOne" $ dropOne size
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
          ]
    ]

o_1_space_nested :: Int -> [Benchmark]
o_1_space_nested size =
    [ bgroup
          "outer-product"
          [ benchIO "toNull" $ Nested.toNull size
          , benchIO "toNull3" $ Nested.toNull3 size
          , benchIO "concat" $ Nested.concat size
          , benchIO "breakAfterSome" $ Nested.breakAfterSome size
          , benchIO "filterAllOut" $ Nested.filterAllOut size
          , benchIO "filterAllIn" $ Nested.filterAllIn size
          , benchIO "filterSome" $ Nested.filterSome size
          , benchIO "concatMapM (100 x n/100)" $ concatMapM size
          -- Unimplemented
          -- , benchIO "ap" $ ap size
          -- , benchIO "apDiscardFst" $ apDiscardFst size
          -- , benchIO "apDiscardSnd" $ apDiscardSnd size
          ]
    ]

o_n_space_nested :: Int -> [Benchmark]
o_n_space_nested size =
    [ bgroup
          "outer-product"
          [ benchIO "toList" $ Nested.toList size
          , benchIO "toListSome" $ Nested.toListSome size
          ]
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (size, cfg, benches) <- parseCLIOpts defaultStreamSize
    size `seq` runMode (mode cfg) cfg benches (allBenchmarks size)

    where

    allBenchmarks size =
        [ bgroup (o_1_space_prefix moduleName)
            $ Prelude.concat
                  [ o_1_space_transformation_input size
                  , o_1_space_generation size
                  , o_1_space_transformation size
                  , o_1_space_filtering size
                  , o_1_space_zip size
                  , o_1_space_nested size
                  ]
        , bgroup (o_n_space_prefix moduleName)
            $ Prelude.concat [o_n_space_nested size]
        ]
