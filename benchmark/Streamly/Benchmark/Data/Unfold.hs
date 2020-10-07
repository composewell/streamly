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

import qualified Prelude
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Streamly.Benchmark.Common
import Streamly.Benchmark.Data.NestedUnfoldOps
import Gauge
import Prelude hiding (concat, take)

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# INLINE fromListM #-}
fromListM :: Monad m => Int -> Int -> m ()
fromListM size start =
    S.drain
         $ S.unfold UF.fromListM (Prelude.map return [start .. start + size])

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> m ()
replicateM size start = S.drain $ S.unfold (UF.replicateM size) (return start)

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> m ()
repeatM size start = S.drain $ S.take size $ S.unfold UF.repeatM (return start)

{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: Monad m => Int -> Int -> m ()
enumerateFromStepIntegral size start = S.drain $
    S.take size $ S.unfold UF.enumerateFromStepIntegral (start, 1)

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Monad m => Int -> Int -> m ()
enumerateFromToIntegral size start =
    S.drain $ S.unfold (UF.enumerateFromToIntegral (size + start)) start

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE take #-}
take :: Monad m => Int -> Int -> m ()
take size start =
    S.drain
        $ S.unfold
              (UF.take size (UF.enumerateFromToIntegral (size + start)))
              start

{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => Int -> Int -> m ()
takeWhileM size start =
    S.drain
        $ S.unfold
            (UF.takeWhileM
                 (\b -> return (b <= size + start))
                 (UF.enumerateFromToIntegral (size + start)))
            start

{-# INLINE _dropOne #-}
_dropOne :: Monad m => Int -> Int -> m ()
_dropOne size start =
    S.drain
         $ S.unfold (UF.drop 1 (UF.enumerateFromToIntegral (size + start))) start

{-# INLINE dropAll #-}
dropAll :: Monad m => Int -> Int -> m ()
dropAll size start =
    S.drain
        $ S.unfold
            (UF.drop (size + 1) (UF.enumerateFromToIntegral (size + start)))
            start

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: Monad m => Int -> Int -> m ()
dropWhileMTrue size start =
    S.drain
        $ S.unfold
            (UF.dropWhileM
                 (\_ -> return True)
                 (UF.enumerateFromToIntegral (size + start)))
            start

{-# INLINE dropWhileMFalse #-}
dropWhileMFalse :: Monad m => Int -> Int -> m ()
dropWhileMFalse size start =
    S.drain
        $ S.unfold
            (UF.dropWhileM
                 (\_ -> return False)
                 (UF.enumerateFromToIntegral (size + start)))
            start

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# INLINE zipWithM #-}
zipWithM :: Monad m => Int -> Int -> m ()
zipWithM size start =
    S.drain
        $ S.unfold
            (UF.zipWithM
                 (\a b -> return $ a + b)
                 (UF.enumerateFromToIntegral (size + start))
                 (UF.enumerateFromToIntegral (size + start + 1)))
            (start, start + 1)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_serial_generation :: Int -> [Benchmark]
o_1_space_serial_generation size =
    [ bgroup "generation"
        [ benchIO "fromListM" $ fromListM size
        , benchIO "replicateM" $ replicateM size
        , benchIO "repeatM" $ repeatM size
        , benchIO "enumerateFromStepIntegral" $ enumerateFromStepIntegral size
        , benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral size
        ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation size =
    [ bgroup "transformation"
        [ benchIO "take" $ take size
        , benchIO "takeWhileM" $ takeWhileM size
        , benchIO "filterAllOut" $ filterAllOut size
        , benchIO "filterAllIn" $ filterAllIn size
        , benchIO "filterSome" $ filterSome size
        -- This will take nanoseconds, We need to fix the benchmark reporting to
        -- have microseconds as the minimum unit before uncommenting this.
        -- , benchIO "dropOne" $ dropOne size
        , benchIO "dropAll" $ dropAll size
        , benchIO "dropWhileMTrue" $ dropWhileMTrue size
        , benchIO "dropWhileMFalse" $ dropWhileMFalse size
        ]
    ]

o_1_space_serial_combination :: Int -> [Benchmark]
o_1_space_serial_combination size =
    [ bgroup "combination"
        [ benchIO "zipWithM" $ zipWithM size
        ]
    ]

o_1_space_serial_outerProduct :: Int -> [Benchmark]
o_1_space_serial_outerProduct size =
    [ bgroup "outer-product"
        [ benchIO "toNull" $ toNull size
        , benchIO "toNull3" $ toNull3 size
        , benchIO "concat" $ concat size
        , benchIO "breakAfterSome" $ breakAfterSome size
        ]
    ]

o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial size =
    [ bgroup "outer-product"
        [ benchIO "toList" $ toList size
        , benchIO "toListSome" $ toListSome size
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
                  [ o_1_space_serial_generation size
                  , o_1_space_serial_transformation size
                  , o_1_space_serial_combination size
                  , o_1_space_serial_outerProduct size
                  ]
        , bgroup (o_n_space_prefix moduleName)
            $ Prelude.concat [o_n_space_serial size]
        ]
