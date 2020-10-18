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
import qualified Streamly.Benchmark.Data.NestedUnfoldOps as Nested

import Streamly.Benchmark.Common
import Gauge
import Prelude hiding (concat, take)

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE drainUnfold #-}
drainUnfold :: Monad m => Unfold m a b -> a -> m ()
drainUnfold unf seed = UF.fold unf FL.drain seed

{-# INLINE drainGeneration #-}
drainGeneration :: Monad m => Unfold m a b -> a -> m ()
drainGeneration = drainUnfold

{-# INLINE drainTransformation #-}
drainTransformation ::
       Monad m => Unfold m a b -> (Unfold m a b -> Unfold m c d) -> c -> m ()
drainTransformation unf f seed = drainUnfold (f unf) seed

{-# INLINE drainTransformationDefault #-}
drainTransformationDefault ::
       Monad m => Int -> (Unfold m Int Int -> Unfold m c d) -> c -> m ()
drainTransformationDefault size =
    drainTransformation (UF.take size UF.enumerateFromIntegral)

{-# INLINE drainProduct #-}
drainProduct ::
       Monad m
    => Unfold m a b
    -> Unfold m c d
    -> (Unfold m a b -> Unfold m c d -> Unfold m e f)
    -> e
    -> m ()
drainProduct unf1 unf2 f seed = drainUnfold (f unf1 unf2) seed

{-# INLINE drainProductDefault #-}
drainProductDefault ::
       Monad m
    => Int
    -> (Unfold m Int Int -> Unfold m Int Int -> Unfold m e f)
    -> e
    -> m ()
drainProductDefault size = drainProduct src src

    where

    src = UF.take size UF.enumerateFromIntegral

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# INLINE fromListM #-}
fromListM :: Monad m => Int -> Int -> m ()
fromListM size start =
    drainGeneration UF.fromListM (Prelude.map return [start .. start + size])

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> m ()
replicateM size start = drainGeneration (UF.replicateM size) (return start)

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> m ()
repeatM size start = drainGeneration (UF.take size UF.repeatM) (return start)

{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: Monad m => Int -> Int -> m ()
enumerateFromStepIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromStepIntegral) (start, 1)

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Monad m => Int -> Int -> m ()
enumerateFromToIntegral size start =
    drainGeneration (UF.enumerateFromToIntegral (size + start)) start

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE take #-}
take :: Monad m => Int -> Int -> m ()
take size start = drainTransformationDefault size (UF.take size) start

{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => Int -> Int -> m ()
takeWhileM size start =
    drainTransformationDefault
        size
        (UF.takeWhileM (\b -> return (b <= size + start)))
        start

{-# INLINE _dropOne #-}
_dropOne :: Monad m => Int -> Int -> m ()
_dropOne size start =
    drainTransformationDefault size (UF.drop 1) start

{-# INLINE dropAll #-}
dropAll :: Monad m => Int -> Int -> m ()
dropAll size start =
    drainTransformationDefault size (UF.drop (size + 1)) start

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

{-# INLINE zipWithM #-}
zipWithM :: Monad m => Int -> Int -> m ()
zipWithM size start =
    drainProductDefault
        size
        (UF.zipWithM (\a b -> return $ a + b))
        (start, start + 1)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation size =
    [ bgroup "generation"
        [ benchIO "fromListM" $ fromListM size
        , benchIO "replicateM" $ replicateM size
        , benchIO "repeatM" $ repeatM size
        , benchIO "enumerateFromStepIntegral" $ enumerateFromStepIntegral size
        , benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral size
        ]
    ]

o_1_space_transformation :: Int -> [Benchmark]
o_1_space_transformation size =
    [ bgroup "transformation"
        [ benchIO "take" $ take size
        , benchIO "takeWhileM" $ takeWhileM size
        -- This will take nanoseconds, We need to fix the benchmark reporting to
        -- have microseconds as the minimum unit before uncommenting this.
        -- , benchIO "dropOne" $ dropOne size
        , benchIO "dropAll" $ dropAll size
        , benchIO "dropWhileMTrue" $ dropWhileMTrue size
        , benchIO "dropWhileMFalse" $ dropWhileMFalse size
        ]
    ]

o_1_space_combination :: Int -> [Benchmark]
o_1_space_combination size =
    [ bgroup "combination"
        [ benchIO "zipWithM" $ zipWithM size
        ]
    ]

o_1_space_nested :: Int -> [Benchmark]
o_1_space_nested size =
    [ bgroup "outer-product"
        [ benchIO "toNull" $ Nested.toNull size
        , benchIO "toNull3" $ Nested.toNull3 size
        , benchIO "concat" $ Nested.concat size
        , benchIO "breakAfterSome" $ Nested.breakAfterSome size
        , benchIO "filterAllOut" $ Nested.filterAllOut size
        , benchIO "filterAllIn" $ Nested.filterAllIn size
        , benchIO "filterSome" $ Nested.filterSome size
        ]
    ]

o_n_space_nested :: Int -> [Benchmark]
o_n_space_nested size =
    [ bgroup "outer-product"
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
                  [ o_1_space_generation size
                  , o_1_space_transformation size
                  , o_1_space_combination size
                  , o_1_space_nested size
                  ]
        , bgroup (o_n_space_prefix moduleName)
            $ Prelude.concat [o_n_space_nested size]
        ]
