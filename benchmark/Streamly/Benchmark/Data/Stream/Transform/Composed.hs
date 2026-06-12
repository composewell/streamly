-- |
-- Module      : Stream.Reduce
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Transform.Composed (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
#endif

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Generics (Generic)
import Streamly.Internal.Data.Stream (Stream)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Scan as Scan
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Stream.Common hiding (benchIO)
import Stream.Type (benchIO, withRandomIntIO, withStream)
import Prelude hiding (tail)

-- Apply transformation g count times on a stream of length len
{-# INLINE iterateSource #-}
iterateSource ::
       MonadAsync m
    => (Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Int
    -> Stream m Int
iterateSource g count len n = f count (sourceUnfoldrM len n)

    where

    f (0 :: Int) stream = stream
    f i stream = f (i - 1) (g stream)

-------------------------------------------------------------------------------
-- Mixed Transformation
-------------------------------------------------------------------------------

{-# INLINE scanMap #-}
scanMap :: MonadIO m => Int -> Stream m Int -> m ()
scanMap n = composeN n $ fmap (subtract 1) . Common.scanl' (+) 0

{-# INLINE scanMap1 #-}
scanMap1 :: Int -> IO ()
scanMap1 value = withStream value (scanMap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanMap1
inspect $ 'scanMap1 `hasNoType` ''S.Step
inspect $ 'scanMap1 `hasNoType` ''S.ScanState
inspect $ 'scanMap1 `hasNoType` ''FL.Step
inspect $ 'scanMap1 `hasNoType` ''SPEC
#endif

{-# INLINE scanMap2 #-}
scanMap2 :: Int -> IO ()
scanMap2 value = withStream value (scanMap 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanMap2
inspect $ 'scanMap2 `hasNoType` ''S.Step
inspect $ 'scanMap2 `hasNoType` ''S.ScanState
inspect $ 'scanMap2 `hasNoType` ''FL.Step
inspect $ 'scanMap2 `hasNoType` ''SPEC
#endif

{-# INLINE scanMap4 #-}
scanMap4 :: Int -> IO ()
scanMap4 value = withStream value (scanMap 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanMap4
inspect $ 'scanMap4 `hasNoType` ''S.Step
inspect $ 'scanMap4 `hasNoType` ''S.ScanState
inspect $ 'scanMap4 `hasNoType` ''FL.Step
inspect $ 'scanMap4 `hasNoType` ''SPEC
#endif

{-# INLINE dropMap #-}
dropMap :: MonadIO m => Int -> Stream m Int -> m ()
dropMap n = composeN n $ fmap (subtract 1) . S.drop 1

{-# INLINE dropMap1 #-}
dropMap1 :: Int -> IO ()
dropMap1 value = withStream value (dropMap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropMap1
inspect $ 'dropMap1 `hasNoType` ''S.Step
inspect $ 'dropMap1 `hasNoType` ''FL.Step
inspect $ 'dropMap1 `hasNoType` ''SPEC
#endif

{-# INLINE dropMap2 #-}
dropMap2 :: Int -> IO ()
dropMap2 value = withStream value (dropMap 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropMap2
inspect $ 'dropMap2 `hasNoType` ''S.Step
inspect $ 'dropMap2 `hasNoType` ''FL.Step
inspect $ 'dropMap2 `hasNoType` ''SPEC
#endif

{-# INLINE dropMap4 #-}
dropMap4 :: Int -> IO ()
dropMap4 value = withStream value (dropMap 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropMap4
inspect $ 'dropMap4 `hasNoType` ''S.Step
inspect $ 'dropMap4 `hasNoType` ''FL.Step
inspect $ 'dropMap4 `hasNoType` ''SPEC
#endif

{-# INLINE dropScan #-}
dropScan :: MonadIO m => Int -> Stream m Int -> m ()
dropScan n = composeN n $ Common.scanl' (+) 0 . S.drop 1

{-# INLINE dropScan1 #-}
dropScan1 :: Int -> IO ()
dropScan1 value = withStream value (dropScan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropScan1
inspect $ 'dropScan1 `hasNoType` ''S.Step
inspect $ 'dropScan1 `hasNoType` ''S.ScanState
inspect $ 'dropScan1 `hasNoType` ''FL.Step
inspect $ 'dropScan1 `hasNoType` ''SPEC
#endif

{-# INLINE dropScan2 #-}
dropScan2 :: Int -> IO ()
dropScan2 value = withStream value (dropScan 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropScan2
inspect $ 'dropScan2 `hasNoType` ''S.Step
inspect $ 'dropScan2 `hasNoType` ''S.ScanState
inspect $ 'dropScan2 `hasNoType` ''FL.Step
inspect $ 'dropScan2 `hasNoType` ''SPEC
#endif

{-# INLINE dropScan4 #-}
dropScan4 :: Int -> IO ()
dropScan4 value = withStream value (dropScan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropScan4
inspect $ 'dropScan4 `hasNoType` ''S.Step
inspect $ 'dropScan4 `hasNoType` ''S.ScanState
inspect $ 'dropScan4 `hasNoType` ''FL.Step
inspect $ 'dropScan4 `hasNoType` ''SPEC
#endif

{-# INLINE takeDrop #-}
takeDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeDrop value n = composeN n $ S.drop 1 . S.take (value + 1)

{-# INLINE takeDrop1 #-}
takeDrop1 :: Int -> IO ()
takeDrop1 value = withStream value (takeDrop value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeDrop1
inspect $ 'takeDrop1 `hasNoType` ''S.Step
inspect $ 'takeDrop1 `hasNoType` ''FL.Step
inspect $ 'takeDrop1 `hasNoType` ''SPEC
#endif

{-# INLINE takeDrop2 #-}
takeDrop2 :: Int -> IO ()
takeDrop2 value = withStream value (takeDrop value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeDrop2
inspect $ 'takeDrop2 `hasNoType` ''S.Step
inspect $ 'takeDrop2 `hasNoType` ''FL.Step
inspect $ 'takeDrop2 `hasNoType` ''SPEC
#endif

{-# INLINE takeDrop4 #-}
takeDrop4 :: Int -> IO ()
takeDrop4 value = withStream value (takeDrop value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeDrop4
inspect $ 'takeDrop4 `hasNoType` ''S.Step
inspect $ 'takeDrop4 `hasNoType` ''FL.Step
inspect $ 'takeDrop4 `hasNoType` ''SPEC
#endif

{-# INLINE takeScan #-}
takeScan :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeScan value n = composeN n $ Common.scanl' (+) 0 . S.take (value + 1)

{-# INLINE takeScan1 #-}
takeScan1 :: Int -> IO ()
takeScan1 value = withStream value (takeScan value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeScan1
inspect $ 'takeScan1 `hasNoType` ''S.Step
inspect $ 'takeScan1 `hasNoType` ''S.ScanState
inspect $ 'takeScan1 `hasNoType` ''FL.Step
inspect $ 'takeScan1 `hasNoType` ''SPEC
#endif

{-# INLINE takeScan2 #-}
takeScan2 :: Int -> IO ()
takeScan2 value = withStream value (takeScan value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeScan2
inspect $ 'takeScan2 `hasNoType` ''S.Step
inspect $ 'takeScan2 `hasNoType` ''S.ScanState
inspect $ 'takeScan2 `hasNoType` ''FL.Step
inspect $ 'takeScan2 `hasNoType` ''SPEC
#endif

{-# INLINE takeScan4 #-}
takeScan4 :: Int -> IO ()
takeScan4 value = withStream value (takeScan value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeScan4
inspect $ 'takeScan4 `hasNoType` ''S.Step
inspect $ 'takeScan4 `hasNoType` ''S.ScanState
inspect $ 'takeScan4 `hasNoType` ''FL.Step
inspect $ 'takeScan4 `hasNoType` ''SPEC
#endif

{-# INLINE takeMap #-}
takeMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
takeMap value n = composeN n $ fmap (subtract 1) . S.take (value + 1)

{-# INLINE takeMap1 #-}
takeMap1 :: Int -> IO ()
takeMap1 value = withStream value (takeMap value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeMap1
inspect $ 'takeMap1 `hasNoType` ''S.Step
inspect $ 'takeMap1 `hasNoType` ''FL.Step
inspect $ 'takeMap1 `hasNoType` ''SPEC
#endif

{-# INLINE takeMap2 #-}
takeMap2 :: Int -> IO ()
takeMap2 value = withStream value (takeMap value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeMap2
inspect $ 'takeMap2 `hasNoType` ''S.Step
inspect $ 'takeMap2 `hasNoType` ''FL.Step
inspect $ 'takeMap2 `hasNoType` ''SPEC
#endif

{-# INLINE takeMap4 #-}
takeMap4 :: Int -> IO ()
takeMap4 value = withStream value (takeMap value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeMap4
inspect $ 'takeMap4 `hasNoType` ''S.Step
inspect $ 'takeMap4 `hasNoType` ''FL.Step
inspect $ 'takeMap4 `hasNoType` ''SPEC
#endif

{-# INLINE filterDrop #-}
filterDrop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterDrop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))

{-# INLINE filterDrop1 #-}
filterDrop1 :: Int -> IO ()
filterDrop1 value = withStream value (filterDrop value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterDrop1
inspect $ 'filterDrop1 `hasNoType` ''S.Step
inspect $ 'filterDrop1 `hasNoType` ''FL.Step
inspect $ 'filterDrop1 `hasNoType` ''SPEC
#endif

{-# INLINE filterDrop2 #-}
filterDrop2 :: Int -> IO ()
filterDrop2 value = withStream value (filterDrop value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterDrop2
inspect $ 'filterDrop2 `hasNoType` ''S.Step
inspect $ 'filterDrop2 `hasNoType` ''FL.Step
inspect $ 'filterDrop2 `hasNoType` ''SPEC
#endif

{-# INLINE filterDrop4 #-}
filterDrop4 :: Int -> IO ()
filterDrop4 value = withStream value (filterDrop value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterDrop4
inspect $ 'filterDrop4 `hasNoType` ''S.Step
inspect $ 'filterDrop4 `hasNoType` ''FL.Step
inspect $ 'filterDrop4 `hasNoType` ''SPEC
#endif

{-# INLINE filterTake #-}
filterTake :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterTake value n = composeN n $ S.take (value + 1) . S.filter (<= (value + 1))

{-# INLINE filterTake1 #-}
filterTake1 :: Int -> IO ()
filterTake1 value = withStream value (filterTake value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterTake1
inspect $ 'filterTake1 `hasNoType` ''S.Step
inspect $ 'filterTake1 `hasNoType` ''FL.Step
inspect $ 'filterTake1 `hasNoType` ''SPEC
#endif

{-# INLINE filterTake2 #-}
filterTake2 :: Int -> IO ()
filterTake2 value = withStream value (filterTake value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterTake2
inspect $ 'filterTake2 `hasNoType` ''S.Step
inspect $ 'filterTake2 `hasNoType` ''FL.Step
inspect $ 'filterTake2 `hasNoType` ''SPEC
#endif

{-# INLINE filterTake4 #-}
filterTake4 :: Int -> IO ()
filterTake4 value = withStream value (filterTake value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterTake4
inspect $ 'filterTake4 `hasNoType` ''S.Step
inspect $ 'filterTake4 `hasNoType` ''FL.Step
inspect $ 'filterTake4 `hasNoType` ''SPEC
#endif

{-# INLINE filterScan #-}
filterScan :: MonadIO m => Int -> Stream m Int -> m ()
filterScan n = composeN n $ Common.scanl' (+) 0 . S.filter (<= maxBound)

{-# INLINE filterScan1 #-}
filterScan1 :: Int -> IO ()
filterScan1 value = withStream value (filterScan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScan1
inspect $ 'filterScan1 `hasNoType` ''S.Step
inspect $ 'filterScan1 `hasNoType` ''S.ScanState
inspect $ 'filterScan1 `hasNoType` ''FL.Step
inspect $ 'filterScan1 `hasNoType` ''SPEC
#endif

{-# INLINE filterScan2 #-}
filterScan2 :: Int -> IO ()
filterScan2 value = withStream value (filterScan 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScan2
inspect $ 'filterScan2 `hasNoType` ''S.Step
inspect $ 'filterScan2 `hasNoType` ''S.ScanState
inspect $ 'filterScan2 `hasNoType` ''FL.Step
inspect $ 'filterScan2 `hasNoType` ''SPEC
#endif

{-# INLINE filterScan4 #-}
filterScan4 :: Int -> IO ()
filterScan4 value = withStream value (filterScan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScan4
inspect $ 'filterScan4 `hasNoType` ''S.Step
inspect $ 'filterScan4 `hasNoType` ''S.ScanState
inspect $ 'filterScan4 `hasNoType` ''FL.Step
inspect $ 'filterScan4 `hasNoType` ''SPEC
#endif

{-# INLINE filterScanl1 #-}
filterScanl1 :: MonadIO m => Int -> Stream m Int -> m ()
filterScanl1 n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)

{-# INLINE filterScanl12 #-}
filterScanl12 :: Int -> IO ()
filterScanl12 value = withStream value (filterScanl1 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScanl12
inspect $ 'filterScanl12 `hasNoType` ''S.Step
inspect $ 'filterScanl12 `hasNoType` ''S.ScanState
inspect $ 'filterScanl12 `hasNoType` ''FL.Step
inspect $ 'filterScanl12 `hasNoType` ''SPEC
#endif

{-# INLINE filterScanl14 #-}
filterScanl14 :: Int -> IO ()
filterScanl14 value = withStream value (filterScanl1 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScanl14
inspect $ 'filterScanl14 `hasNoType` ''S.Step
inspect $ 'filterScanl14 `hasNoType` ''S.ScanState
inspect $ 'filterScanl14 `hasNoType` ''FL.Step
inspect $ 'filterScanl14 `hasNoType` ''SPEC
#endif

{-# INLINE filterMap #-}
filterMap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMap value n = composeN n $ fmap (subtract 1) . S.filter (<= (value + 1))

{-# INLINE filterMap1 #-}
filterMap1 :: Int -> IO ()
filterMap1 value = withStream value (filterMap value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMap1
inspect $ 'filterMap1 `hasNoType` ''S.Step
inspect $ 'filterMap1 `hasNoType` ''FL.Step
inspect $ 'filterMap1 `hasNoType` ''SPEC
#endif

{-# INLINE filterMap2 #-}
filterMap2 :: Int -> IO ()
filterMap2 value = withStream value (filterMap value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMap2
inspect $ 'filterMap2 `hasNoType` ''S.Step
inspect $ 'filterMap2 `hasNoType` ''FL.Step
inspect $ 'filterMap2 `hasNoType` ''SPEC
#endif

{-# INLINE filterMap4 #-}
filterMap4 :: Int -> IO ()
filterMap4 value = withStream value (filterMap value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMap4
inspect $ 'filterMap4 `hasNoType` ''S.Step
inspect $ 'filterMap4 `hasNoType` ''FL.Step
inspect $ 'filterMap4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b =
    Pair !a !b
    deriving (Generic, NFData)

{-# INLINE sumProductFold #-}
sumProductFold :: Int -> IO (Pair Int Int)
sumProductFold value =
    withStream value $
        Common.foldl' (\(Pair s p) x -> Pair (s + x) (p * x)) (Pair 0 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sumProductFold
inspect $ 'sumProductFold `hasNoType` ''S.Step
inspect $ 'sumProductFold `hasNoType` ''FL.Step
inspect $ 'sumProductFold `hasNoType` ''SPEC
#endif

{-# INLINE sumProductScan #-}
sumProductScan :: Int -> IO (Pair Int Int)
sumProductScan value =
    withStream value $
        Common.foldl' (\(Pair _ p) (s0, x) -> Pair s0 (p * x)) (Pair 0 1) .
        Common.scanl' (\(s, _) x -> (s + x, x)) (0, 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sumProductScan
inspect $ 'sumProductScan `hasNoType` ''S.Step
inspect $ 'sumProductScan `hasNoType` ''S.ScanState
inspect $ 'sumProductScan `hasNoType` ''FL.Step
inspect $ 'sumProductScan `hasNoType` ''SPEC
#endif

{-# INLINE foldl'ReduceMap #-}
foldl'ReduceMap :: Int -> IO Int
foldl'ReduceMap value = withStream value $ fmap (+ 1) . Common.foldl' (+) 0

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'ReduceMap
inspect $ 'foldl'ReduceMap `hasNoType` ''S.Step
inspect $ 'foldl'ReduceMap `hasNoType` ''FL.Step
inspect $ 'foldl'ReduceMap `hasNoType` ''SPEC
#endif

o_1_space_transformations_mixed :: Int -> [Benchmark]
o_1_space_transformations_mixed value =
    -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
    -- library. If scan/fold followed by a map is efficient enough we may not
    -- need monolithic implementations of these.
    [ benchIO "scanl-map" $ scanMap1 value
    , benchIO "drop-map" $ dropMap1 value
    , benchIO "drop-scan" $ dropScan1 value
    , benchIO "take-drop" $ takeDrop1 value
    , benchIO "take-scan" $ takeScan1 value
    , benchIO "take-map" $ takeMap1 value
    , benchIO "filter-drop" $ filterDrop1 value
    , benchIO "filter-take" $ filterTake1 value
    , benchIO "filter-scan" $ filterScan1 value
    , benchIO "filter-map" $ filterMap1 value
    , benchIO "foldl-map" $ foldl'ReduceMap value
    , benchIO "sum-product-fold" $ sumProductFold value
    , benchIO "sum-product-scan" $ sumProductScan value
    ]

o_1_space_transformations_mixedX2 :: Int -> [Benchmark]
o_1_space_transformations_mixedX2 value =
    [ benchIO "scan-map x 2" $ scanMap2 value
    , benchIO "drop-map x 2" $ dropMap2 value
    , benchIO "drop-scan x 2" $ dropScan2 value
    , benchIO "take-drop x 2" $ takeDrop2 value
    , benchIO "take-scan x 2" $ takeScan2 value
    , benchIO "take-map x 2" $ takeMap2 value
    , benchIO "filter-drop x 2" $ filterDrop2 value
    , benchIO "filter-take x 2" $ filterTake2 value
    , benchIO "filter-scan x 2" $ filterScan2 value
    , benchIO "filter-scanl1 x 2" $ filterScanl12 value
    , benchIO "filter-map x 2" $ filterMap2 value
    ]

o_1_space_transformations_mixedX4 :: Int -> [Benchmark]
o_1_space_transformations_mixedX4 value =
    [ benchIO "scan-map x 4" $ scanMap4 value
    , benchIO "drop-map x 4" $ dropMap4 value
    , benchIO "drop-scan x 4" $ dropScan4 value
    , benchIO "take-drop x 4" $ takeDrop4 value
    , benchIO "take-scan x 4" $ takeScan4 value
    , benchIO "take-map x 4" $ takeMap4 value
    , benchIO "filter-drop x 4" $ filterDrop4 value
    , benchIO "filter-take x 4" $ filterTake4 value
    , benchIO "filter-scan x 4" $ filterScan4 value
    , benchIO "filter-scanl1 x 4" $ filterScanl14 value
    , benchIO "filter-map x 4" $ filterMap4 value
    ]

-------------------------------------------------------------------------------
-- Iterating a transformation over and over again
-------------------------------------------------------------------------------

-- this is quadratic
{-# INLINE iterateScan #-}
iterateScan :: Int -> Int -> IO ()
iterateScan value iterCount =
    withRandomIntIO $ Common.drain . iterateSource (Common.scanl' (+) 0) (value `div` iterCount) iterCount

-- this is quadratic
{-# INLINE iterateScanl1 #-}
iterateScanl1 :: Int -> Int -> IO ()
iterateScanl1 value iterCount =
    withRandomIntIO $ Common.drain . iterateSource (S.scanl1' (+)) (value `div` iterCount) iterCount

{-# INLINE iterateMapM #-}
iterateMapM :: Int -> Int -> IO ()
iterateMapM value iterCount =
    withRandomIntIO $ Common.drain . iterateSource (S.mapM return) (value `div` iterCount) iterCount

{-# INLINE iterateFilterEven #-}
iterateFilterEven :: Int -> Int -> IO ()
iterateFilterEven value iterCount =
    withRandomIntIO $ Common.drain . iterateSource (S.filter even) (value `div` iterCount) iterCount

{-# INLINE iterateTakeAll #-}
iterateTakeAll :: Int -> Int -> IO ()
iterateTakeAll value iterCount =
    withRandomIntIO
        $ Common.drain . iterateSource (S.take (value + 1)) (value `div` iterCount) iterCount

{-# INLINE iterateDropOne #-}
iterateDropOne :: Int -> Int -> IO ()
iterateDropOne value iterCount =
    withRandomIntIO $ Common.drain . iterateSource (S.drop 1) (value `div` iterCount) iterCount

{-# INLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: Int -> Int -> IO ()
iterateDropWhileTrue value iterCount =
    withRandomIntIO
        $ Common.drain . iterateSource (S.dropWhile (<= (value + 1))) (value `div` iterCount) iterCount

{-# INLINE iterateDropWhileFalse #-}
iterateDropWhileFalse :: Int -> Int -> IO ()
iterateDropWhileFalse value iterCount =
    withRandomIntIO
        $ Common.drain . iterateSource (S.dropWhile (> (value + 1))) (value `div` iterCount) iterCount

o_n_stack_iterated :: Int -> [Benchmark]
o_n_stack_iterated value =
    [ benchIO "iterated/mapM (n/10 x 10)" $ iterateMapM value 10
    , benchIO "iterated/scanl' (quadratic) (n/100 x 100)" $ iterateScan value 100
    , benchIO "iterated/scanl1' (n/10 x 10)" $ iterateScanl1 value 10
    , benchIO "iterated/filterEven (n/10 x 10)" $ iterateFilterEven value 10
    , benchIO "iterated/takeAll (n/10 x 10)" $ iterateTakeAll value 10
    , benchIO "iterated/dropOne (n/10 x 10)" $ iterateDropOne value 10
    , benchIO "iterated/dropWhileTrue (n/10 x 10)" $ iterateDropWhileTrue value 10
    , benchIO "iterated/dropWhileFalse (n/10 x 10)" $ iterateDropWhileFalse value 10
    ]

-------------------------------------------------------------------------------
-- Pipes
-------------------------------------------------------------------------------

{-# INLINE transformMapM #-}
transformMapM :: Monad m => Int -> Stream m Int -> m ()
transformMapM n = composeN n $ Stream.pipe (Pipe.mapM return)

{-# INLINE transformComposeMapM #-}
transformComposeMapM :: Monad m => Int -> Stream m Int -> m ()
transformComposeMapM n =
    composeN n $
    Stream.pipe
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.compose`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE transformTeeMapM #-}
transformTeeMapM :: Monad m => Int -> Stream m Int -> m ()
transformTeeMapM n =
    composeN n $
    Stream.pipe
        (Pipe.mapM (\x -> return (x + 1)) `Pipe.teeMerge`
         Pipe.mapM (\x -> return (x + 2)))

{-# INLINE scanMapM #-}
scanMapM :: Monad m => Int -> Stream m Int -> m ()
scanMapM n = composeN n $ Stream.scanr (Scan.functionM return)

{-# INLINE scanComposeMapM #-}
scanComposeMapM :: Monad m => Int -> Stream m Int -> m ()
scanComposeMapM n =
    composeN n $
    Stream.scanr
        (Scan.functionM (\x -> return (x + 1)) `Scan.compose`
         Scan.functionM (\x -> return (x + 2)))

{-# INLINE scanTeeMapM #-}
scanTeeMapM :: Monad m => Int -> Stream m Int -> m ()
scanTeeMapM n =
    composeN n $
    Stream.scanr
        (Scan.teeWith (+) (Scan.functionM (\x -> return (x + 1)))
         (Scan.functionM (\x -> return (x + 2))))

{-# INLINE pipeMapM #-}
pipeMapM :: Int -> IO ()
pipeMapM value = withStream value (transformMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeMapM
inspect $ 'pipeMapM `hasNoType` ''S.Step
inspect $ 'pipeMapM `hasNoType` ''S.PipeState
inspect $ 'pipeMapM `hasNoType` ''FL.Step
inspect $ 'pipeMapM `hasNoType` ''SPEC
#endif

{-# INLINE pipeCompose #-}
pipeCompose :: Int -> IO ()
pipeCompose value = withStream value (transformComposeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeCompose
inspect $ 'pipeCompose `hasNoType` ''S.Step
inspect $ 'pipeCompose `hasNoType` ''S.PipeState
inspect $ 'pipeCompose `hasNoType` ''FL.Step
inspect $ 'pipeCompose `hasNoType` ''SPEC
#endif

{-# INLINE pipeTee #-}
pipeTee :: Int -> IO ()
pipeTee value = withStream value (transformTeeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeTee
inspect $ 'pipeTee `hasNoType` ''S.Step
inspect $ 'pipeTee `hasNoType` ''S.PipeState
inspect $ 'pipeTee `hasNoType` ''FL.Step
inspect $ 'pipeTee `hasNoType` ''SPEC
#endif

{-# INLINE pipeMapMX4 #-}
pipeMapMX4 :: Int -> IO ()
pipeMapMX4 value = withStream value (transformMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeMapMX4
inspect $ 'pipeMapMX4 `hasNoType` ''S.Step
inspect $ 'pipeMapMX4 `hasNoType` ''S.PipeState
inspect $ 'pipeMapMX4 `hasNoType` ''FL.Step
inspect $ 'pipeMapMX4 `hasNoType` ''SPEC
#endif

{-# INLINE pipeComposeX4 #-}
pipeComposeX4 :: Int -> IO ()
pipeComposeX4 value = withStream value (transformComposeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeComposeX4
inspect $ 'pipeComposeX4 `hasNoType` ''S.Step
inspect $ 'pipeComposeX4 `hasNoType` ''S.PipeState
inspect $ 'pipeComposeX4 `hasNoType` ''FL.Step
inspect $ 'pipeComposeX4 `hasNoType` ''SPEC
#endif

{-# INLINE pipeTeeX4 #-}
pipeTeeX4 :: Int -> IO ()
pipeTeeX4 value = withStream value (transformTeeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'pipeTeeX4
inspect $ 'pipeTeeX4 `hasNoType` ''S.Step
inspect $ 'pipeTeeX4 `hasNoType` ''S.PipeState
inspect $ 'pipeTeeX4 `hasNoType` ''FL.Step
inspect $ 'pipeTeeX4 `hasNoType` ''SPEC
#endif

-- XXX these should move to Data.Pipe benchmarks
o_1_space_pipes :: Int -> [Benchmark]
o_1_space_pipes value =
    [ benchIO "pipe/mapM" $ pipeMapM value
    , benchIO "pipe/compose" $ pipeCompose value
    , benchIO "pipe/tee" $ pipeTee value
    -- XXX this take 1 GB memory to compile
    -- , benchIO "zip" $ pipeZip value
    ]

o_1_space_pipesX4 :: Int -> [Benchmark]
o_1_space_pipesX4 value =
    [ benchIO "pipe/mapM x 4" $ pipeMapMX4 value
    , benchIO "pipe/compose x 4" $ pipeComposeX4 value
    -- XXX requires @-fspec-constr-recursive=16@.
    , benchIO "pipe/tee x 4" $ pipeTeeX4 value
    -- XXX this take 1 GB memory to compile
    -- , benchIO "zip x 4" $ pipeZipX4 value
    ]

-------------------------------------------------------------------------------
-- Scans
-------------------------------------------------------------------------------

{-# INLINE scansMapM #-}
scansMapM :: Int -> IO ()
scansMapM value = withStream value (scanMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansMapM
inspect $ 'scansMapM `hasNoType` ''S.Step
inspect $ 'scansMapM `hasNoType` ''S.RunScanState
inspect $ 'scansMapM `hasNoType` ''FL.Step
inspect $ 'scansMapM `hasNoType` ''SPEC
#endif

{-# INLINE scansCompose #-}
scansCompose :: Int -> IO ()
scansCompose value = withStream value (scanComposeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansCompose
inspect $ 'scansCompose `hasNoType` ''S.Step
inspect $ 'scansCompose `hasNoType` ''S.RunScanState
inspect $ 'scansCompose `hasNoType` ''FL.Step
inspect $ 'scansCompose `hasNoType` ''SPEC
#endif

{-# INLINE scansTee #-}
scansTee :: Int -> IO ()
scansTee value = withStream value (scanTeeMapM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansTee
inspect $ 'scansTee `hasNoType` ''S.Step
inspect $ 'scansTee `hasNoType` ''S.RunScanState
inspect $ 'scansTee `hasNoType` ''FL.Step
inspect $ 'scansTee `hasNoType` ''SPEC
#endif

{-# INLINE scansMapMX4 #-}
scansMapMX4 :: Int -> IO ()
scansMapMX4 value = withStream value (scanMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansMapMX4
inspect $ 'scansMapMX4 `hasNoType` ''S.Step
inspect $ 'scansMapMX4 `hasNoType` ''S.RunScanState
inspect $ 'scansMapMX4 `hasNoType` ''FL.Step
inspect $ 'scansMapMX4 `hasNoType` ''SPEC
#endif

{-# INLINE scansComposeX4 #-}
scansComposeX4 :: Int -> IO ()
scansComposeX4 value = withStream value (scanComposeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansComposeX4
inspect $ 'scansComposeX4 `hasNoType` ''S.Step
inspect $ 'scansComposeX4 `hasNoType` ''S.RunScanState
inspect $ 'scansComposeX4 `hasNoType` ''FL.Step
inspect $ 'scansComposeX4 `hasNoType` ''SPEC
#endif

{-# INLINE scansTeeX4 #-}
scansTeeX4 :: Int -> IO ()
scansTeeX4 value = withStream value (scanTeeMapM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scansTeeX4
inspect $ 'scansTeeX4 `hasNoType` ''S.Step
inspect $ 'scansTeeX4 `hasNoType` ''S.RunScanState
inspect $ 'scansTeeX4 `hasNoType` ''FL.Step
inspect $ 'scansTeeX4 `hasNoType` ''SPEC
#endif

-- XXX These should move to the Data.Scan module
o_1_space_scans :: Int -> [Benchmark]
o_1_space_scans value =
    [ benchIO "scan/mapM" $ scansMapM value
    , benchIO "scan/compose" $ scansCompose value
    , benchIO "scan/tee" $ scansTee value
    ]

o_1_space_scansX4 :: Int -> [Benchmark]
o_1_space_scansX4 value =
    [ benchIO "scan/mapM x 4" $ scansMapMX4 value
    , benchIO "scan/compose x 4" $ scansComposeX4 value
    , benchIO "scan/tee x 4" $ scansTeeX4 value
    ]

-------------------------------------------------------------------------------
-- Composed transformations (scan + mapMaybe)
-------------------------------------------------------------------------------

{-# INLINE sieveScan #-}
sieveScan :: Monad m => Stream m Int -> Stream m Int
sieveScan =
      Stream.mapMaybe snd
    . Stream.scanl (Scanl.scanlM' (\(primes, _) n -> do
            return $
                let ps = takeWhile (\p -> p * p <= n) primes
                 in if all (\p -> n `mod` p /= 0) ps
                    then (primes ++ [n], Just n)
                    else (primes, Nothing)) (return ([2], Just 2)))

{-# INLINE naivePrimeSieve #-}
naivePrimeSieve :: Int -> IO Int
naivePrimeSieve value =
    withRandomIntIO $ \n ->
        Stream.fold FL.sum $ sieveScan $ Stream.enumerateFromTo 2 (value + n)

o_n_space_mapping :: Int -> [Benchmark]
o_n_space_mapping value =
    [ benchIO "naive prime sieve" $ naivePrimeSieve value
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    map (SpaceO_1,) (Prelude.concat
        [ o_1_space_transformations_mixed size
        , o_1_space_transformations_mixedX2 size
        , o_1_space_transformations_mixedX4 size

        -- pipes
        , o_1_space_pipes size
        , o_1_space_pipesX4 size

        -- scans
        , o_1_space_scans size
        , o_1_space_scansX4 size
        ])
    ++ map (StackO_n,) (o_n_stack_iterated size)
    ++ map (SpaceO_n,) (o_n_space_mapping size)
