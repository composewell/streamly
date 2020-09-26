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
fromListM value n =
    S.drain $ S.unfold UF.fromListM (Prelude.map return [n .. n + value])

{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Int -> m ()
replicateM value n = S.drain $ S.unfold (UF.replicateM value) n

{-# INLINE repeatM #-}
repeatM :: Monad m => Int -> Int -> m ()
repeatM value n = S.drain $ S.take value $ S.unfold UF.repeatM n

{-# INLINE enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: Monad m => Int -> Int -> m ()
enumerateFromStepIntegral value n = S.drain $
    S.take value $ S.unfold UF.enumerateFromStepIntegral (n, 1)

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Monad m => Int -> Int -> m ()
enumerateFromToIntegral value n =
    S.drain $ S.unfold (UF.enumerateFromToIntegral (value + n)) n

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

-- XXX Investigate
{-# INLINE take #-}
take :: Monad m => Int -> Int -> m ()
take value n =
    S.drain
        $ S.unfold (UF.take value (UF.enumerateFromToIntegral (value + n))) n

{-# INLINE takeWhileM #-}
takeWhileM :: Monad m => Int -> Int -> m ()
takeWhileM value n =
    S.drain
        $ S.unfold
            (UF.takeWhileM
                 (\b -> return (b <= value + n))
                 (UF.enumerateFromToIntegral (value + n)))
            n

{-# INLINE dropOne #-}
dropOne :: Monad m => Int -> Int -> m ()
dropOne value n =
    S.drain $ S.unfold (UF.drop 1 (UF.enumerateFromToIntegral (value + n))) n

{-# INLINE dropAll #-}
dropAll :: Monad m => Int -> Int -> m ()
dropAll value n =
    S.drain
        $ S.unfold
            (UF.drop (value + 1) (UF.enumerateFromToIntegral (value + n)))
            n

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: Monad m => Int -> Int -> m ()
dropWhileMTrue value n =
    S.drain
        $ S.unfold
            (UF.dropWhileM
                 (\_ -> return True)
                 (UF.enumerateFromToIntegral (value + n)))
            n

{-# INLINE dropWhileMFalse #-}
dropWhileMFalse :: Monad m => Int -> Int -> m ()
dropWhileMFalse value n =
    S.drain
        $ S.unfold
            (UF.dropWhileM
                 (\_ -> return False)
                 (UF.enumerateFromToIntegral (value + n)))
            n

-------------------------------------------------------------------------------
-- Stream combination
-------------------------------------------------------------------------------

{-# INLINE zipWithM #-}
zipWithM :: Monad m => Int -> Int -> m ()
zipWithM value n =
    S.drain
        $ S.unfold
            (UF.zipWithM
                 (\a b -> return $ a + b)
                 (UF.enumerateFromToIntegral (value + n))
                 (UF.enumerateFromToIntegral (value + n + 1)))
            (n, n + 1)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unfold"

o_1_space_serial_generation :: Int -> [Benchmark]
o_1_space_serial_generation value =
    [ bgroup "generation"
        [ benchIO "fromListM" $ fromListM value
        , benchIO "replicateM" $ replicateM value
        , benchIO "repeatM" $ repeatM value
        , benchIO "enumerateFromStepIntegral" $ enumerateFromStepIntegral value
        , benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral value
        ]
    ]

o_1_space_serial_transformation :: Int -> [Benchmark]
o_1_space_serial_transformation value =
    [ bgroup "transformation"
        [ benchIO "take" $ take value
        , benchIO "takeWhileM" $ takeWhileM value
        , benchIO "filterAllOut" $ filterAllOut value
        , benchIO "filterAllIn" $ filterAllIn value
        , benchIO "filterSome" $ filterSome value
        , benchIO "dropOne" $ dropOne value
        , benchIO "dropAll" $ dropAll value
        , benchIO "dropWhileMTrue" $ dropWhileMTrue value
        , benchIO "dropWhileMFalse" $ dropWhileMFalse value
        ]
    ]


o_1_space_serial_combination :: Int -> [Benchmark]
o_1_space_serial_combination value =
    [ bgroup "combination"
        [ benchIO "zipWithM" $ zipWithM value
        ]
    ]


o_1_space_serial_outerProduct :: Int -> [Benchmark]
o_1_space_serial_outerProduct value =
    [ bgroup "outer-product"
        [ benchIO "toNull" $ toNull value
        , benchIO "toNull3" $ toNull3 value
        , benchIO "concat" $ concat value
        , benchIO "breakAfterSome" $ breakAfterSome value
        ]
    ]

o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial value =
    [ bgroup "outer-product"
        [ benchIO "toList" $ toList value
        , benchIO "toListSome" $ toListSome value
        ]
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName)
            $ Prelude.concat
                  [ o_1_space_serial_generation value
                  , o_1_space_serial_transformation value
                  , o_1_space_serial_combination value
                  , o_1_space_serial_outerProduct value
                  ]
        , bgroup (o_n_space_prefix moduleName)
            $ Prelude.concat [o_n_space_serial value]
        ]
