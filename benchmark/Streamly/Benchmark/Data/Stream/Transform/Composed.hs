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
import Test.Inspection
#endif

import GHC.Types (SPEC(..))
import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Generics (Generic)
import Streamly.Internal.Data.Stream (Stream)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import qualified Streamly.Internal.Data.SVar.Type as SVar
import Stream.Common hiding (benchIO)
import Stream.Type (benchIO, withStream)
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

{-# ANN scanMap1 (PermitPatternMatches [''Int]) #-}
{-# ANN scanMap1 (PermitConstructions [''()]) #-}
{-# ANN scanMap1 (PermitTypeClasses []) #-}
{-# NOINLINE scanMap1 #-}
scanMap1 :: Int -> Int -> IO ()
scanMap1 value = withStream value (scanMap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanMap1
inspect $ 'scanMap1 `hasNoType` ''S.Step
inspect $ 'scanMap1 `hasNoType` ''S.ScanState
inspect $ 'scanMap1 `hasNoType` ''FL.Step
inspect $ 'scanMap1 `hasNoType` ''SPEC
#endif

{-# ANN scanMap2 (PermitPatternMatches [''Int]) #-}
{-# ANN scanMap2 (PermitConstructions [''()]) #-}
{-# ANN scanMap2 (PermitTypeClasses []) #-}
{-# NOINLINE scanMap2 #-}
scanMap2 :: Int -> Int -> IO ()
scanMap2 value = withStream value (scanMap 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanMap2
inspect $ 'scanMap2 `hasNoType` ''S.Step
inspect $ 'scanMap2 `hasNoType` ''S.ScanState
inspect $ 'scanMap2 `hasNoType` ''FL.Step
inspect $ 'scanMap2 `hasNoType` ''SPEC
#endif

{-# ANN scanMap4 (PermitPatternMatches [''Int]) #-}
{-# ANN scanMap4 (PermitConstructions [''()]) #-}
{-# ANN scanMap4 (PermitTypeClasses []) #-}
{-# NOINLINE scanMap4 #-}
scanMap4 :: Int -> Int -> IO ()
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

{-# ANN dropMap1 (PermitPatternMatches [''Int]) #-}
{-# ANN dropMap1 (PermitConstructions [''()]) #-}
{-# ANN dropMap1 (PermitTypeClasses []) #-}
{-# NOINLINE dropMap1 #-}
dropMap1 :: Int -> Int -> IO ()
dropMap1 value = withStream value (dropMap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropMap1
inspect $ 'dropMap1 `hasNoType` ''S.Step
inspect $ 'dropMap1 `hasNoType` ''FL.Step
inspect $ 'dropMap1 `hasNoType` ''SPEC
#endif

{-# ANN dropMap2 (PermitPatternMatches [''Int]) #-}
{-# ANN dropMap2 (PermitConstructions [''()]) #-}
{-# ANN dropMap2 (PermitTypeClasses []) #-}
{-# NOINLINE dropMap2 #-}
dropMap2 :: Int -> Int -> IO ()
dropMap2 value = withStream value (dropMap 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropMap2
inspect $ 'dropMap2 `hasNoType` ''S.Step
inspect $ 'dropMap2 `hasNoType` ''FL.Step
inspect $ 'dropMap2 `hasNoType` ''SPEC
#endif

{-# ANN dropMap4 (PermitPatternMatches [''Int]) #-}
{-# ANN dropMap4 (PermitConstructions [''()]) #-}
{-# ANN dropMap4 (PermitTypeClasses []) #-}
{-# NOINLINE dropMap4 #-}
dropMap4 :: Int -> Int -> IO ()
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

{-# ANN dropScan1 (PermitPatternMatches [''Int]) #-}
{-# ANN dropScan1 (PermitConstructions [''()]) #-}
{-# ANN dropScan1 (PermitTypeClasses []) #-}
{-# NOINLINE dropScan1 #-}
dropScan1 :: Int -> Int -> IO ()
dropScan1 value = withStream value (dropScan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropScan1
inspect $ 'dropScan1 `hasNoType` ''S.Step
inspect $ 'dropScan1 `hasNoType` ''S.ScanState
inspect $ 'dropScan1 `hasNoType` ''FL.Step
inspect $ 'dropScan1 `hasNoType` ''SPEC
#endif

{-# ANN dropScan2 (PermitPatternMatches [''Int]) #-}
{-# ANN dropScan2 (PermitConstructions [''()]) #-}
{-# ANN dropScan2 (PermitTypeClasses []) #-}
{-# NOINLINE dropScan2 #-}
dropScan2 :: Int -> Int -> IO ()
dropScan2 value = withStream value (dropScan 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropScan2
inspect $ 'dropScan2 `hasNoType` ''S.Step
inspect $ 'dropScan2 `hasNoType` ''S.ScanState
inspect $ 'dropScan2 `hasNoType` ''FL.Step
inspect $ 'dropScan2 `hasNoType` ''SPEC
#endif

{-# ANN dropScan4 (PermitPatternMatches [''Int]) #-}
{-# ANN dropScan4 (PermitConstructions [''()]) #-}
{-# ANN dropScan4 (PermitTypeClasses []) #-}
{-# NOINLINE dropScan4 #-}
dropScan4 :: Int -> Int -> IO ()
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

{-# ANN takeDrop1 (PermitPatternMatches [''Int]) #-}
{-# ANN takeDrop1 (PermitConstructions [''()]) #-}
{-# ANN takeDrop1 (PermitTypeClasses []) #-}
{-# NOINLINE takeDrop1 #-}
takeDrop1 :: Int -> Int -> IO ()
takeDrop1 value = withStream value (takeDrop value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeDrop1
inspect $ 'takeDrop1 `hasNoType` ''S.Step
inspect $ 'takeDrop1 `hasNoType` ''FL.Step
inspect $ 'takeDrop1 `hasNoType` ''SPEC
#endif

{-# ANN takeDrop2 (PermitPatternMatches [''Int]) #-}
{-# ANN takeDrop2 (PermitConstructions [''()]) #-}
{-# ANN takeDrop2 (PermitTypeClasses []) #-}
{-# NOINLINE takeDrop2 #-}
takeDrop2 :: Int -> Int -> IO ()
takeDrop2 value = withStream value (takeDrop value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeDrop2
inspect $ 'takeDrop2 `hasNoType` ''S.Step
inspect $ 'takeDrop2 `hasNoType` ''FL.Step
inspect $ 'takeDrop2 `hasNoType` ''SPEC
#endif

{-# ANN takeDrop4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeDrop4 (PermitConstructions [''()]) #-}
{-# ANN takeDrop4 (PermitTypeClasses []) #-}
{-# NOINLINE takeDrop4 #-}
takeDrop4 :: Int -> Int -> IO ()
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

{-# ANN takeScan1 (PermitPatternMatches [''Int]) #-}
{-# ANN takeScan1 (PermitConstructions [''()]) #-}
{-# ANN takeScan1 (PermitTypeClasses []) #-}
{-# NOINLINE takeScan1 #-}
takeScan1 :: Int -> Int -> IO ()
takeScan1 value = withStream value (takeScan value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeScan1
inspect $ 'takeScan1 `hasNoType` ''S.Step
inspect $ 'takeScan1 `hasNoType` ''S.ScanState
inspect $ 'takeScan1 `hasNoType` ''FL.Step
inspect $ 'takeScan1 `hasNoType` ''SPEC
#endif

{-# ANN takeScan2 (PermitPatternMatches [''Int]) #-}
{-# ANN takeScan2 (PermitConstructions [''()]) #-}
{-# ANN takeScan2 (PermitTypeClasses []) #-}
{-# NOINLINE takeScan2 #-}
takeScan2 :: Int -> Int -> IO ()
takeScan2 value = withStream value (takeScan value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeScan2
inspect $ 'takeScan2 `hasNoType` ''S.Step
inspect $ 'takeScan2 `hasNoType` ''S.ScanState
inspect $ 'takeScan2 `hasNoType` ''FL.Step
inspect $ 'takeScan2 `hasNoType` ''SPEC
#endif

{-# ANN takeScan4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeScan4 (PermitConstructions [''()]) #-}
{-# ANN takeScan4 (PermitTypeClasses []) #-}
{-# NOINLINE takeScan4 #-}
takeScan4 :: Int -> Int -> IO ()
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

{-# ANN takeMap1 (PermitPatternMatches [''Int]) #-}
{-# ANN takeMap1 (PermitConstructions [''()]) #-}
{-# ANN takeMap1 (PermitTypeClasses []) #-}
{-# NOINLINE takeMap1 #-}
takeMap1 :: Int -> Int -> IO ()
takeMap1 value = withStream value (takeMap value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeMap1
inspect $ 'takeMap1 `hasNoType` ''S.Step
inspect $ 'takeMap1 `hasNoType` ''FL.Step
inspect $ 'takeMap1 `hasNoType` ''SPEC
#endif

{-# ANN takeMap2 (PermitPatternMatches [''Int]) #-}
{-# ANN takeMap2 (PermitConstructions [''()]) #-}
{-# ANN takeMap2 (PermitTypeClasses []) #-}
{-# NOINLINE takeMap2 #-}
takeMap2 :: Int -> Int -> IO ()
takeMap2 value = withStream value (takeMap value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'takeMap2
inspect $ 'takeMap2 `hasNoType` ''S.Step
inspect $ 'takeMap2 `hasNoType` ''FL.Step
inspect $ 'takeMap2 `hasNoType` ''SPEC
#endif

{-# ANN takeMap4 (PermitPatternMatches [''Int]) #-}
{-# ANN takeMap4 (PermitConstructions [''()]) #-}
{-# ANN takeMap4 (PermitTypeClasses []) #-}
{-# NOINLINE takeMap4 #-}
takeMap4 :: Int -> Int -> IO ()
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

{-# ANN filterDrop1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterDrop1 (PermitConstructions [''()]) #-}
{-# ANN filterDrop1 (PermitTypeClasses []) #-}
{-# NOINLINE filterDrop1 #-}
filterDrop1 :: Int -> Int -> IO ()
filterDrop1 value = withStream value (filterDrop value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterDrop1
inspect $ 'filterDrop1 `hasNoType` ''S.Step
inspect $ 'filterDrop1 `hasNoType` ''FL.Step
inspect $ 'filterDrop1 `hasNoType` ''SPEC
#endif

{-# ANN filterDrop2 (PermitPatternMatches [''Int]) #-}
{-# ANN filterDrop2 (PermitConstructions [''()]) #-}
{-# ANN filterDrop2 (PermitTypeClasses []) #-}
{-# NOINLINE filterDrop2 #-}
filterDrop2 :: Int -> Int -> IO ()
filterDrop2 value = withStream value (filterDrop value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterDrop2
inspect $ 'filterDrop2 `hasNoType` ''S.Step
inspect $ 'filterDrop2 `hasNoType` ''FL.Step
inspect $ 'filterDrop2 `hasNoType` ''SPEC
#endif

{-# ANN filterDrop4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterDrop4 (PermitConstructions [''()]) #-}
{-# ANN filterDrop4 (PermitTypeClasses []) #-}
{-# NOINLINE filterDrop4 #-}
filterDrop4 :: Int -> Int -> IO ()
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

{-# ANN filterTake1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterTake1 (PermitConstructions [''()]) #-}
{-# ANN filterTake1 (PermitTypeClasses []) #-}
{-# NOINLINE filterTake1 #-}
filterTake1 :: Int -> Int -> IO ()
filterTake1 value = withStream value (filterTake value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterTake1
inspect $ 'filterTake1 `hasNoType` ''S.Step
inspect $ 'filterTake1 `hasNoType` ''FL.Step
inspect $ 'filterTake1 `hasNoType` ''SPEC
#endif

{-# ANN filterTake2 (PermitPatternMatches [''Int]) #-}
{-# ANN filterTake2 (PermitConstructions [''()]) #-}
{-# ANN filterTake2 (PermitTypeClasses []) #-}
{-# NOINLINE filterTake2 #-}
filterTake2 :: Int -> Int -> IO ()
filterTake2 value = withStream value (filterTake value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterTake2
inspect $ 'filterTake2 `hasNoType` ''S.Step
inspect $ 'filterTake2 `hasNoType` ''FL.Step
inspect $ 'filterTake2 `hasNoType` ''SPEC
#endif

{-# ANN filterTake4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterTake4 (PermitConstructions [''()]) #-}
{-# ANN filterTake4 (PermitTypeClasses []) #-}
{-# NOINLINE filterTake4 #-}
filterTake4 :: Int -> Int -> IO ()
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

{-# ANN filterScan1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterScan1 (PermitConstructions [''()]) #-}
{-# ANN filterScan1 (PermitTypeClasses []) #-}
{-# NOINLINE filterScan1 #-}
filterScan1 :: Int -> Int -> IO ()
filterScan1 value = withStream value (filterScan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScan1
inspect $ 'filterScan1 `hasNoType` ''S.Step
inspect $ 'filterScan1 `hasNoType` ''S.ScanState
inspect $ 'filterScan1 `hasNoType` ''FL.Step
inspect $ 'filterScan1 `hasNoType` ''SPEC
#endif

{-# ANN filterScan2 (PermitPatternMatches [''Int]) #-}
{-# ANN filterScan2 (PermitConstructions [''()]) #-}
{-# ANN filterScan2 (PermitTypeClasses []) #-}
{-# NOINLINE filterScan2 #-}
filterScan2 :: Int -> Int -> IO ()
filterScan2 value = withStream value (filterScan 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScan2
inspect $ 'filterScan2 `hasNoType` ''S.Step
inspect $ 'filterScan2 `hasNoType` ''S.ScanState
inspect $ 'filterScan2 `hasNoType` ''FL.Step
inspect $ 'filterScan2 `hasNoType` ''SPEC
#endif

{-# ANN filterScan4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterScan4 (PermitConstructions [''()]) #-}
{-# ANN filterScan4 (PermitTypeClasses []) #-}
{-# NOINLINE filterScan4 #-}
filterScan4 :: Int -> Int -> IO ()
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

{-# ANN filterScanl12 (PermitPatternMatches [''Int]) #-}
{-# ANN filterScanl12 (PermitConstructions [''()]) #-}
{-# ANN filterScanl12 (PermitTypeClasses []) #-}
{-# NOINLINE filterScanl12 #-}
filterScanl12 :: Int -> Int -> IO ()
filterScanl12 value = withStream value (filterScanl1 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterScanl12
inspect $ 'filterScanl12 `hasNoType` ''S.Step
inspect $ 'filterScanl12 `hasNoType` ''S.ScanState
inspect $ 'filterScanl12 `hasNoType` ''FL.Step
inspect $ 'filterScanl12 `hasNoType` ''SPEC
#endif

{-# ANN filterScanl14 (PermitPatternMatches [''Int]) #-}
{-# ANN filterScanl14 (PermitConstructions [''()]) #-}
{-# ANN filterScanl14 (PermitTypeClasses []) #-}
{-# NOINLINE filterScanl14 #-}
filterScanl14 :: Int -> Int -> IO ()
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

{-# ANN filterMap1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMap1 (PermitConstructions [''()]) #-}
{-# ANN filterMap1 (PermitTypeClasses []) #-}
{-# NOINLINE filterMap1 #-}
filterMap1 :: Int -> Int -> IO ()
filterMap1 value = withStream value (filterMap value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMap1
inspect $ 'filterMap1 `hasNoType` ''S.Step
inspect $ 'filterMap1 `hasNoType` ''FL.Step
inspect $ 'filterMap1 `hasNoType` ''SPEC
#endif

{-# ANN filterMap2 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMap2 (PermitConstructions [''()]) #-}
{-# ANN filterMap2 (PermitTypeClasses []) #-}
{-# NOINLINE filterMap2 #-}
filterMap2 :: Int -> Int -> IO ()
filterMap2 value = withStream value (filterMap value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMap2
inspect $ 'filterMap2 `hasNoType` ''S.Step
inspect $ 'filterMap2 `hasNoType` ''FL.Step
inspect $ 'filterMap2 `hasNoType` ''SPEC
#endif

{-# ANN filterMap4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMap4 (PermitConstructions [''()]) #-}
{-# ANN filterMap4 (PermitTypeClasses []) #-}
{-# NOINLINE filterMap4 #-}
filterMap4 :: Int -> Int -> IO ()
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

{-# ANN sumProductFold (PermitPatternMatches [''Int]) #-}
{-# ANN sumProductFold (PermitConstructions [''Pair,''Int]) #-}
{-# ANN sumProductFold (PermitTypeClasses []) #-}
{-# NOINLINE sumProductFold #-}
sumProductFold :: Int -> Int -> IO (Pair Int Int)
sumProductFold value =
    withStream value $
        Common.foldl' (\(Pair s p) x -> Pair (s + x) (p * x)) (Pair 0 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sumProductFold
inspect $ 'sumProductFold `hasNoType` ''S.Step
inspect $ 'sumProductFold `hasNoType` ''FL.Step
inspect $ 'sumProductFold `hasNoType` ''SPEC
#endif

{-# ANN sumProductScan (PermitPatternMatches [''Int]) #-}
{-# ANN sumProductScan (PermitConstructions [''Int,''Pair]) #-}
{-# ANN sumProductScan (PermitTypeClasses []) #-}
{-# NOINLINE sumProductScan #-}
sumProductScan :: Int -> Int -> IO (Pair Int Int)
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

{-# ANN foldl'ReduceMap (PermitPatternMatches [''Int]) #-}
{-# ANN foldl'ReduceMap (PermitConstructions [''Int]) #-}
{-# ANN foldl'ReduceMap (PermitTypeClasses []) #-}
{-# NOINLINE foldl'ReduceMap #-}
foldl'ReduceMap :: Int -> Int -> IO Int
foldl'ReduceMap value = withStream value $ fmap (+ 1) . Common.foldl' (+) 0

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'ReduceMap
inspect $ 'foldl'ReduceMap `hasNoType` ''S.Step
inspect $ 'foldl'ReduceMap `hasNoType` ''FL.Step
inspect $ 'foldl'ReduceMap `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Iterating a transformation over and over again
-------------------------------------------------------------------------------

-- this is quadratic
{-# ANN iterateScan (PermitPatternMatches [''Int,''Stream.Step,''Stream.ScanState,''Stream]) #-}
{-# ANN iterateScan (PermitConstructions [''Int,''Stream,''Stream.ScanState,''Stream.Step,''SVar.State,''Maybe,''(),''Bool]) #-}
{-# ANN iterateScan (PermitTypeClasses []) #-}
{-# NOINLINE iterateScan #-}
iterateScan :: Int -> Int -> Int -> IO ()
iterateScan value iterCount =
    Common.drain . iterateSource (Common.scanl' (+) 0) (value `div` iterCount) iterCount

-- this is quadratic
{-# ANN iterateScanl1 (PermitPatternMatches [''Maybe,''(,),''Int,''Stream.Step,''Stream]) #-}
{-# ANN iterateScanl1 (PermitConstructions [''Int,''Maybe,''Stream,''(,),''Stream.Step,''SVar.State,''(),''Bool]) #-}
{-# ANN iterateScanl1 (PermitTypeClasses []) #-}
{-# NOINLINE iterateScanl1 #-}
iterateScanl1 :: Int -> Int -> Int -> IO ()
iterateScanl1 value iterCount =
    Common.drain . iterateSource (S.scanl1' (+)) (value `div` iterCount) iterCount

{-# ANN iterateMapM (PermitPatternMatches [''Int,''Stream.Step,''Stream]) #-}
{-# ANN iterateMapM (PermitConstructions [''Int,''Stream,''SVar.State,''Maybe,''Stream.Step,''(),''Bool]) #-}
{-# ANN iterateMapM (PermitTypeClasses []) #-}
{-# NOINLINE iterateMapM #-}
iterateMapM :: Int -> Int -> Int -> IO ()
iterateMapM value iterCount =
    Common.drain . iterateSource (S.mapM return) (value `div` iterCount) iterCount

{-# ANN iterateFilterEven (PermitPatternMatches [''Int,''Stream.Step,''Stream]) #-}
{-# ANN iterateFilterEven (PermitConstructions [''Int,''Stream.Step,''Stream,''SVar.State,''Maybe,''(),''Bool]) #-}
{-# ANN iterateFilterEven (PermitTypeClasses []) #-}
{-# NOINLINE iterateFilterEven #-}
iterateFilterEven :: Int -> Int -> Int -> IO ()
iterateFilterEven value iterCount =
    Common.drain . iterateSource (S.filter even) (value `div` iterCount) iterCount

{-# ANN iterateTakeAll (PermitPatternMatches [''(,),''Int,''Stream.Step]) #-}
{-# ANN iterateTakeAll (PermitConstructions [''Int,''SVar.State,''Maybe,''Stream.Step,''(,),''(),''Bool]) #-}
{-# ANN iterateTakeAll (PermitTypeClasses []) #-}
{-# NOINLINE iterateTakeAll #-}
iterateTakeAll :: Int -> Int -> Int -> IO ()
iterateTakeAll value iterCount =
    Common.drain . iterateSource (S.take (value + 1)) (value `div` iterCount) iterCount

{-# ANN iterateDropOne (PermitPatternMatches [''Maybe,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN iterateDropOne (PermitConstructions [''Int,''Integer,''Maybe,''Stream,''(,),''Stream.Step,''SVar.State,''(),''Bool]) #-}
{-# ANN iterateDropOne (PermitTypeClasses [''Ord,''Num]) #-}
{-# NOINLINE iterateDropOne #-}
iterateDropOne :: Int -> Int -> Int -> IO ()
iterateDropOne value iterCount =
    Common.drain . iterateSource (S.drop 1) (value `div` iterCount) iterCount

{-# ANN iterateDropWhileTrue (PermitPatternMatches [''Int,''Stream.Step,''Stream.DropWhileState]) #-}
{-# ANN iterateDropWhileTrue (PermitConstructions [''Int,''SVar.State,''Maybe,''Stream.Step,''Stream.DropWhileState,''(),''Bool]) #-}
{-# ANN iterateDropWhileTrue (PermitTypeClasses []) #-}
{-# NOINLINE iterateDropWhileTrue #-}
iterateDropWhileTrue :: Int -> Int -> Int -> IO ()
iterateDropWhileTrue value iterCount =
    Common.drain . iterateSource (S.dropWhile (<= (value + 1))) (value `div` iterCount) iterCount

_iterateDropWhileFalse :: Int -> Int -> Int -> IO ()
_iterateDropWhileFalse value iterCount =
    Common.drain . iterateSource (S.dropWhile (> (value + 1))) (value `div` iterCount) iterCount

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: Applicative m =>
       (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n = iterateN g (Stream.fromPure n) count

{-
-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       Monad m
    => (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n
-}

{-# ANN iteratePlusBaseline (PermitPatternMatches [''Int]) #-}
{-# ANN iteratePlusBaseline (PermitConstructions [''Int]) #-}
{-# ANN iteratePlusBaseline (PermitTypeClasses []) #-}
{-# NOINLINE iteratePlusBaseline #-}
iteratePlusBaseline :: Int -> Int -> IO Int
iteratePlusBaseline value i0 =
    iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value

{-# ANN iterateSubMap (PermitPatternMatches [''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN iterateSubMap (PermitConstructions [''Int,''Stream.Step,''Stream,''SVar.State,''Maybe,''Bool]) #-}
{-# ANN iterateSubMap (PermitTypeClasses []) #-}
{-# NOINLINE iterateSubMap #-}
iterateSubMap :: Int -> Int -> IO ()
iterateSubMap value = drain . iterateSingleton (<$) value

{-# ANN iterateFmap (PermitPatternMatches [''Int,''Stream.Step,''Stream]) #-}
{-# ANN iterateFmap (PermitConstructions [''Int,''Stream.Step,''Stream,''SVar.State,''Maybe,''Bool]) #-}
{-# ANN iterateFmap (PermitTypeClasses []) #-}
{-# NOINLINE iterateFmap #-}
iterateFmap :: Int -> Int -> IO ()
iterateFmap value = drain . iterateSingleton (fmap . (+)) value

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

{-# ANN naivePrimeSieve (PermitPatternMatches [''Int,''[]]) #-}
{-# ANN naivePrimeSieve (PermitConstructions [''[],''Int]) #-}
{-# ANN naivePrimeSieve (PermitTypeClasses []) #-}
{-# NOINLINE naivePrimeSieve #-}
naivePrimeSieve :: Int -> Int -> IO Int
naivePrimeSieve value n =
    Stream.fold FL.sum $ sieveScan $ Stream.enumerateFromTo 2 (value + n)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
    -- library. If scan/fold followed by a map is efficient enough we may not
    -- need monolithic implementations of these.
    [ (SpaceO_1, benchIO "scanl-map" $ scanMap1 size)
    , (SpaceO_1, benchIO "drop-map" $ dropMap1 size)
    , (SpaceO_1, benchIO "drop-scan" $ dropScan1 size)
    , (SpaceO_1, benchIO "take-drop" $ takeDrop1 size)
    , (SpaceO_1, benchIO "take-scan" $ takeScan1 size)
    , (SpaceO_1, benchIO "take-map" $ takeMap1 size)
    , (SpaceO_1, benchIO "filter-drop" $ filterDrop1 size)
    , (SpaceO_1, benchIO "filter-take" $ filterTake1 size)
    , (SpaceO_1, benchIO "filter-scan" $ filterScan1 size)
    , (SpaceO_1, benchIO "filter-map" $ filterMap1 size)
    , (SpaceO_1, benchIO "foldl-map" $ foldl'ReduceMap size)
    , (SpaceO_1, benchIO "sum-product-fold" $ sumProductFold size)
    , (SpaceO_1, benchIO "sum-product-scan" $ sumProductScan size)
    , (SpaceO_1, benchIO "scan-map x 2" $ scanMap2 size)
    , (SpaceO_1, benchIO "drop-map x 2" $ dropMap2 size)
    , (SpaceO_1, benchIO "drop-scan x 2" $ dropScan2 size)
    , (SpaceO_1, benchIO "take-drop x 2" $ takeDrop2 size)
    , (SpaceO_1, benchIO "take-scan x 2" $ takeScan2 size)
    , (SpaceO_1, benchIO "take-map x 2" $ takeMap2 size)
    , (SpaceO_1, benchIO "filter-drop x 2" $ filterDrop2 size)
    , (SpaceO_1, benchIO "filter-take x 2" $ filterTake2 size)
    , (SpaceO_1, benchIO "filter-scan x 2" $ filterScan2 size)
    , (SpaceO_1, benchIO "filter-scanl1 x 2" $ filterScanl12 size)
    , (SpaceO_1, benchIO "filter-map x 2" $ filterMap2 size)
    , (SpaceO_1, benchIO "scan-map x 4" $ scanMap4 size)
    , (SpaceO_1, benchIO "drop-map x 4" $ dropMap4 size)
    , (SpaceO_1, benchIO "drop-scan x 4" $ dropScan4 size)
    , (SpaceO_1, benchIO "take-drop x 4" $ takeDrop4 size)
    , (SpaceO_1, benchIO "take-scan x 4" $ takeScan4 size)
    , (SpaceO_1, benchIO "take-map x 4" $ takeMap4 size)
    , (SpaceO_1, benchIO "filter-drop x 4" $ filterDrop4 size)
    , (SpaceO_1, benchIO "filter-take x 4" $ filterTake4 size)
    , (SpaceO_1, benchIO "filter-scan x 4" $ filterScan4 size)
    , (SpaceO_1, benchIO "filter-scanl1 x 4" $ filterScanl14 size)
    , (SpaceO_1, benchIO "filter-map x 4" $ filterMap4 size)

    , (StackO_n, benchIO "iterated/mapM (n/10 x 10)" $ iterateMapM size 10)
    , (StackO_n, benchIO "iterated/scanl' (quadratic) (n/100 x 100)" $ iterateScan size 100)
    , (StackO_n, benchIO "iterated/scanl1' (n/10 x 10)" $ iterateScanl1 size 10)
    , (StackO_n, benchIO "iterated/filterEven (n/10 x 10)" $ iterateFilterEven size 10)
    , (StackO_n, benchIO "iterated/takeAll (n/10 x 10)" $ iterateTakeAll size 10)
    , (StackO_n, benchIO "iterated/dropOne (n/10 x 10)" $ iterateDropOne size 10)
    , (StackO_n, benchIO "iterated/dropWhileTrue (n/10 x 10)" $ iterateDropWhileTrue size 10)
    -- XXX tasty-bench hangs on this sometimes
    -- , (StackO_n, benchIO "iterated/dropWhileFalse (n/10 x 10)" $ _iterateDropWhileFalse size 10)
    , (SpaceO_n, benchIO "iterated/(+) (n times) (baseline)" $ iteratePlusBaseline size)
    , (SpaceO_n, benchIO "iterated/(<$) (n times)" $ iterateSubMap size)
    , (SpaceO_n, benchIO "iterated/fmap (n times)" $ iterateFmap size)
    {-
    , benchIOSrc fromSerial "_(<$) (n times)" $
        _iterateSingleton (<$) value
    , benchIOSrc fromSerial "_fmap (n times)" $
        _iterateSingleton (fmap . (+)) value
    -}
    , (SpaceO_n, benchIO "naive prime sieve" $ naivePrimeSieve size)
    ]
