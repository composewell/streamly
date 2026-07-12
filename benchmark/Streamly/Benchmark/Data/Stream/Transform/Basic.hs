-- |
-- Module      : Stream.Transform
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Transform.Basic (benchmarks) where

#ifdef INSPECTION
import Test.Inspection
import qualified Streamly.Internal.Data.Producer as Producer
#endif

import GHC.Types (SPEC(..))
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Stream (Stream)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Tasty.Bench
import Stream.Common hiding (scanl', benchIO)
import Stream.Type (benchIO, withStream)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import qualified Streamly.Internal.Data.SVar.Type as SVar
import Streamly.Data.Array (Array)
import qualified Streamly.Internal.Data.MutArray as MutArray
import Streamly.Data.MutByteArray (MutByteArray)
import Prelude hiding (sequence, mapM, reverse)

-------------------------------------------------------------------------------
-- Pipelines (stream-to-stream transformations)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- one-to-one transformations
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- maps and scans
-------------------------------------------------------------------------------

{-# INLINE scanl' #-}
scanl' :: MonadIO m => Int -> Stream m Int -> m ()
scanl' n = composeN n $ Stream.scanl' (+) 0

{-# ANN scanl'1 (PermitPatternMatches [''Int]) #-}
{-# ANN scanl'1 (PermitConstructions [''()]) #-}
{-# ANN scanl'1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'1 #-}
scanl'1 :: Int -> Int -> IO ()
scanl'1 value = withStream value (scanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'1
inspect $ 'scanl'1 `hasNoType` ''Stream.Step
#endif

{-# ANN scanl'4 (PermitPatternMatches [''Int]) #-}
{-# ANN scanl'4 (PermitConstructions [''()]) #-}
{-# ANN scanl'4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'4 #-}
scanl'4 :: Int -> Int -> IO ()
scanl'4 value = withStream value (scanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'4
inspect $ 'scanl'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanlM' #-}
scanlM' :: MonadIO m => Int -> Stream m Int -> m ()
scanlM' n = composeN n $ Stream.scanlM' (\b a -> return $ b + a) (return 0)

{-# ANN scanlM'1 (PermitPatternMatches [''Int]) #-}
{-# ANN scanlM'1 (PermitConstructions [''()]) #-}
{-# ANN scanlM'1 (PermitTypeClasses []) #-}
{-# NOINLINE scanlM'1 #-}
scanlM'1 :: Int -> Int -> IO ()
scanlM'1 value = withStream value (scanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'1
inspect $ 'scanlM'1 `hasNoType` ''Stream.Step
#endif

{-# ANN scanlM'4 (PermitPatternMatches [''Int]) #-}
{-# ANN scanlM'4 (PermitConstructions [''()]) #-}
{-# ANN scanlM'4 (PermitTypeClasses []) #-}
{-# NOINLINE scanlM'4 #-}
scanlM'4 :: Int -> Int -> IO ()
scanlM'4 value = withStream value (scanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanlM'4
inspect $ 'scanlM'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1' n = composeN n $ Stream.scanl1' (+)

{-# ANN scanl1'1 (PermitPatternMatches [''Int]) #-}
{-# ANN scanl1'1 (PermitConstructions [''()]) #-}
{-# ANN scanl1'1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1'1 #-}
scanl1'1 :: Int -> Int -> IO ()
scanl1'1 value = withStream value (scanl1' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'1
inspect $ 'scanl1'1 `hasNoType` ''Stream.Step
#endif

{-# ANN scanl1'4 (PermitPatternMatches [''Int]) #-}
{-# ANN scanl1'4 (PermitConstructions [''()]) #-}
{-# ANN scanl1'4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1'4 #-}
scanl1'4 :: Int -> Int -> IO ()
scanl1'4 value = withStream value (scanl1' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1'4
inspect $ 'scanl1'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scanl1M' #-}
scanl1M' :: MonadIO m => Int -> Stream m Int -> m ()
scanl1M' n = composeN n $ Stream.scanl1M' (\b a -> return $ b + a)

{-# ANN scanl1M'1 (PermitPatternMatches [''Int]) #-}
{-# ANN scanl1M'1 (PermitConstructions [''()]) #-}
{-# ANN scanl1M'1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1M'1 #-}
scanl1M'1 :: Int -> Int -> IO ()
scanl1M'1 value = withStream value (scanl1M' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'1
inspect $ 'scanl1M'1 `hasNoType` ''Stream.Step
#endif

{-# ANN scanl1M'4 (PermitPatternMatches [''Int]) #-}
{-# ANN scanl1M'4 (PermitConstructions [''()]) #-}
{-# ANN scanl1M'4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl1M'4 #-}
scanl1M'4 :: Int -> Int -> IO ()
scanl1M'4 value = withStream value (scanl1M' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl1M'4
inspect $ 'scanl1M'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE scan #-}
scan :: MonadIO m => Int -> Stream m Int -> m ()
scan n = composeN n $ Stream.scanl Scanl.sum

{-# ANN scan1 (PermitPatternMatches [''Int]) #-}
{-# ANN scan1 (PermitConstructions [''()]) #-}
{-# ANN scan1 (PermitTypeClasses []) #-}
{-# NOINLINE scan1 #-}
scan1 :: Int -> Int -> IO ()
scan1 value = withStream value (scan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scan1
inspect $ 'scan1 `hasNoType` ''Stream.Step
inspect $ 'scan1 `hasNoType` ''Stream.ScanState
inspect $ 'scan1 `hasNoType` ''FL.Step
inspect $ 'scan1 `hasNoType` ''SPEC
#endif

{-# ANN scan4 (PermitPatternMatches [''Int]) #-}
{-# ANN scan4 (PermitConstructions [''()]) #-}
{-# ANN scan4 (PermitTypeClasses []) #-}
{-# NOINLINE scan4 #-}
scan4 :: Int -> Int -> IO ()
scan4 value = withStream value (scan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scan4
inspect $ 'scan4 `hasNoType` ''Stream.Step
inspect $ 'scan4 `hasNoType` ''Stream.ScanState
inspect $ 'scan4 `hasNoType` ''FL.Step
inspect $ 'scan4 `hasNoType` ''SPEC
#endif

{-# INLINE postscan #-}
postscan :: MonadIO m => Int -> Stream m Int -> m ()
postscan n = composeN n $ Stream.postscanl Scanl.sum

{-# ANN postscan1 (PermitPatternMatches [''Int]) #-}
{-# ANN postscan1 (PermitConstructions [''()]) #-}
{-# ANN postscan1 (PermitTypeClasses []) #-}
{-# NOINLINE postscan1 #-}
postscan1 :: Int -> Int -> IO ()
postscan1 value = withStream value (postscan 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscan1
inspect $ 'postscan1 `hasNoType` ''Stream.Step
inspect $ 'postscan1 `hasNoType` ''Stream.ScanState
inspect $ 'postscan1 `hasNoType` ''FL.Step
inspect $ 'postscan1 `hasNoType` ''SPEC
#endif

{-# ANN postscan4 (PermitPatternMatches [''Int]) #-}
{-# ANN postscan4 (PermitConstructions [''()]) #-}
{-# ANN postscan4 (PermitTypeClasses []) #-}
{-# NOINLINE postscan4 #-}
postscan4 :: Int -> Int -> IO ()
postscan4 value = withStream value (postscan 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscan4
inspect $ 'postscan4 `hasNoType` ''Stream.Step
inspect $ 'postscan4 `hasNoType` ''Stream.ScanState
inspect $ 'postscan4 `hasNoType` ''FL.Step
inspect $ 'postscan4 `hasNoType` ''SPEC
#endif

{-# INLINE postscanl' #-}
postscanl' :: MonadIO m => Int -> Stream m Int -> m ()
postscanl' n = composeN n $ Stream.postscanl' (+) 0

{-# ANN postscanl'1 (PermitPatternMatches [''Int]) #-}
{-# ANN postscanl'1 (PermitConstructions [''()]) #-}
{-# ANN postscanl'1 (PermitTypeClasses []) #-}
{-# NOINLINE postscanl'1 #-}
postscanl'1 :: Int -> Int -> IO ()
postscanl'1 value = withStream value (postscanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'1
inspect $ 'postscanl'1 `hasNoType` ''Stream.Step
#endif

{-# ANN postscanl'4 (PermitPatternMatches [''Maybe,''(,),''Int,''Stream.Step,''Stream]) #-}
{-# ANN postscanl'4 (PermitConstructions [''Int,''(,),''Maybe,''Stream.Step,''SVar.State,''Stream,''(),''Bool]) #-}
{-# ANN postscanl'4 (PermitTypeClasses []) #-}
{-# NOINLINE postscanl'4 #-}
postscanl'4 :: Int -> Int -> IO ()
postscanl'4 value = withStream value (postscanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanl'4
-- inspect $ 'postscanl'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE postscanlM' #-}
postscanlM' :: MonadIO m => Int -> Stream m Int -> m ()
postscanlM' n = composeN n $ Stream.postscanlM' (\b a -> return $ b + a) (return 0)

{-# ANN postscanlM'1 (PermitPatternMatches [''Int]) #-}
{-# ANN postscanlM'1 (PermitConstructions [''()]) #-}
{-# ANN postscanlM'1 (PermitTypeClasses []) #-}
{-# NOINLINE postscanlM'1 #-}
postscanlM'1 :: Int -> Int -> IO ()
postscanlM'1 value = withStream value (postscanlM' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'1
inspect $ 'postscanlM'1 `hasNoType` ''Stream.Step
#endif

{-# ANN postscanlM'4 (PermitPatternMatches [''Int]) #-}
{-# ANN postscanlM'4 (PermitConstructions [''()]) #-}
{-# ANN postscanlM'4 (PermitTypeClasses []) #-}
{-# NOINLINE postscanlM'4 #-}
postscanlM'4 :: Int -> Int -> IO ()
postscanlM'4 value = withStream value (postscanlM' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'postscanlM'4
inspect $ 'postscanlM'4 `hasNoType` ''Stream.Step
#endif

{-# INLINE sequence #-}
sequence :: MonadAsync m => Stream m (m Int) -> m ()
sequence = Common.drain . Stream.sequence

{-# ANN sequence1 (PermitPatternMatches [''Int]) #-}
{-# ANN sequence1 (PermitConstructions [''()]) #-}
{-# ANN sequence1 (PermitTypeClasses []) #-}
{-# NOINLINE sequence1 #-}
sequence1 :: Int -> Int -> IO ()
sequence1 value = sequence . sourceUnfoldrAction value

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'sequence1
inspect $ 'sequence1 `hasNoType` ''Stream.Step
inspect $ 'sequence1 `hasNoType` ''FL.Step
inspect $ 'sequence1 `hasNoType` ''SPEC
#endif

{-# INLINE tap #-}
tap :: MonadIO m => Int -> Stream m Int -> m ()
tap n = composeN n $ Stream.tap FL.sum

{-# ANN tap1 (PermitPatternMatches [''Int]) #-}
{-# ANN tap1 (PermitConstructions [''()]) #-}
{-# ANN tap1 (PermitTypeClasses []) #-}
{-# NOINLINE tap1 #-}
tap1 :: Int -> Int -> IO ()
tap1 value = withStream value (tap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'tap1
inspect $ 'tap1 `hasNoType` ''Stream.Step
inspect $ 'tap1 `hasNoType` ''Stream.TapState
inspect $ 'tap1 `hasNoType` ''FL.Step
inspect $ 'tap1 `hasNoType` ''SPEC
#endif

{-# INLINE _timestamped #-}
_timestamped :: MonadIO m => Stream m Int -> m ()
_timestamped = Stream.drain . Stream.timestamped
{-
{-# INLINE foldrT #-}
foldrT :: MonadIO m => Int -> Stream m Int -> m ()
foldrT n = composeN n (unCrossStream . Stream.foldrT cns (CrossStream Stream.nil))

    where cns x (CrossStream xs) = CrossStream (Stream.cons x xs)

{-# INLINE foldrTMap #-}
foldrTMap :: MonadIO m => Int -> Stream m Int -> m ()
foldrTMap n = composeN n $ Stream.foldrT (\x xs -> x + 1 `Stream.cons` xs) Stream.nil
-}

{-# INLINE trace #-}
trace :: MonadAsync m => Int -> Stream m Int -> m ()
trace n = composeN n $ Stream.trace return

{-# ANN trace4 (PermitPatternMatches [''Int]) #-}
{-# ANN trace4 (PermitConstructions [''()]) #-}
{-# ANN trace4 (PermitTypeClasses []) #-}
{-# NOINLINE trace4 #-}
trace4 :: Int -> Int -> IO ()
trace4 value = withStream value (trace 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'trace4
inspect $ 'trace4 `hasNoType` ''Stream.Step
inspect $ 'trace4 `hasNoType` ''FL.Step
inspect $ 'trace4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size reducing transformations (filtering)
-------------------------------------------------------------------------------

{-# INLINE filterEven #-}
filterEven :: MonadIO m => Int -> Stream m Int -> m ()
filterEven n = composeN n $ Stream.filter even

{-# ANN filterEven1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterEven1 (PermitConstructions [''()]) #-}
{-# ANN filterEven1 (PermitTypeClasses []) #-}
{-# NOINLINE filterEven1 #-}
filterEven1 :: Int -> Int -> IO ()
filterEven1 value = withStream value (filterEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterEven1
inspect $ 'filterEven1 `hasNoType` ''Stream.Step
inspect $ 'filterEven1 `hasNoType` ''FL.Step
inspect $ 'filterEven1 `hasNoType` ''SPEC
#endif

{-# ANN filterEven4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterEven4 (PermitConstructions [''()]) #-}
{-# ANN filterEven4 (PermitTypeClasses []) #-}
{-# NOINLINE filterEven4 #-}
filterEven4 :: Int -> Int -> IO ()
filterEven4 value = withStream value (filterEven 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterEven4
inspect $ 'filterEven4 `hasNoType` ''Stream.Step
inspect $ 'filterEven4 `hasNoType` ''FL.Step
inspect $ 'filterEven4 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllOut #-}
filterAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllOut value n = composeN n $ Stream.filter (> (value + 1))

{-# ANN filterAllOut1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterAllOut1 (PermitConstructions [''()]) #-}
{-# ANN filterAllOut1 (PermitTypeClasses []) #-}
{-# NOINLINE filterAllOut1 #-}
filterAllOut1 :: Int -> Int -> IO ()
filterAllOut1 value = withStream value (filterAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllOut1
inspect $ 'filterAllOut1 `hasNoType` ''Stream.Step
inspect $ 'filterAllOut1 `hasNoType` ''FL.Step
inspect $ 'filterAllOut1 `hasNoType` ''SPEC
#endif

{-# ANN filterAllOut4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterAllOut4 (PermitConstructions [''()]) #-}
{-# ANN filterAllOut4 (PermitTypeClasses []) #-}
{-# NOINLINE filterAllOut4 #-}
filterAllOut4 :: Int -> Int -> IO ()
filterAllOut4 value = withStream value (filterAllOut value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllOut4
inspect $ 'filterAllOut4 `hasNoType` ''Stream.Step
inspect $ 'filterAllOut4 `hasNoType` ''FL.Step
inspect $ 'filterAllOut4 `hasNoType` ''SPEC
#endif

{-# INLINE filterAllIn #-}
filterAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterAllIn value n = composeN n $ Stream.filter (<= (value + 1))

{-# ANN filterAllIn1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterAllIn1 (PermitConstructions [''()]) #-}
{-# ANN filterAllIn1 (PermitTypeClasses []) #-}
{-# NOINLINE filterAllIn1 #-}
filterAllIn1 :: Int -> Int -> IO ()
filterAllIn1 value = withStream value (filterAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllIn1
inspect $ 'filterAllIn1 `hasNoType` ''Stream.Step
inspect $ 'filterAllIn1 `hasNoType` ''FL.Step
inspect $ 'filterAllIn1 `hasNoType` ''SPEC
#endif

{-# ANN filterAllIn4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterAllIn4 (PermitConstructions [''()]) #-}
{-# ANN filterAllIn4 (PermitTypeClasses []) #-}
{-# NOINLINE filterAllIn4 #-}
filterAllIn4 :: Int -> Int -> IO ()
filterAllIn4 value = withStream value (filterAllIn value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterAllIn4
inspect $ 'filterAllIn4 `hasNoType` ''Stream.Step
inspect $ 'filterAllIn4 `hasNoType` ''FL.Step
inspect $ 'filterAllIn4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMEven #-}
filterMEven :: MonadIO m => Int -> Stream m Int -> m ()
filterMEven n = composeN n $ Stream.filterM (return . even)

{-# ANN filterMEven1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMEven1 (PermitConstructions [''()]) #-}
{-# ANN filterMEven1 (PermitTypeClasses []) #-}
{-# NOINLINE filterMEven1 #-}
filterMEven1 :: Int -> Int -> IO ()
filterMEven1 value = withStream value (filterMEven 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMEven1
inspect $ 'filterMEven1 `hasNoType` ''Stream.Step
inspect $ 'filterMEven1 `hasNoType` ''FL.Step
inspect $ 'filterMEven1 `hasNoType` ''SPEC
#endif

{-# ANN filterMEven4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMEven4 (PermitConstructions [''()]) #-}
{-# ANN filterMEven4 (PermitTypeClasses []) #-}
{-# NOINLINE filterMEven4 #-}
filterMEven4 :: Int -> Int -> IO ()
filterMEven4 value = withStream value (filterMEven 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMEven4
inspect $ 'filterMEven4 `hasNoType` ''Stream.Step
inspect $ 'filterMEven4 `hasNoType` ''FL.Step
inspect $ 'filterMEven4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllOut #-}
filterMAllOut :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllOut value n = composeN n $ Stream.filterM (\x -> return $ x > (value + 1))

{-# ANN filterMAllOut1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMAllOut1 (PermitConstructions [''()]) #-}
{-# ANN filterMAllOut1 (PermitTypeClasses []) #-}
{-# NOINLINE filterMAllOut1 #-}
filterMAllOut1 :: Int -> Int -> IO ()
filterMAllOut1 value = withStream value (filterMAllOut value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllOut1
inspect $ 'filterMAllOut1 `hasNoType` ''Stream.Step
inspect $ 'filterMAllOut1 `hasNoType` ''FL.Step
inspect $ 'filterMAllOut1 `hasNoType` ''SPEC
#endif

{-# ANN filterMAllOut4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMAllOut4 (PermitConstructions [''()]) #-}
{-# ANN filterMAllOut4 (PermitTypeClasses []) #-}
{-# NOINLINE filterMAllOut4 #-}
filterMAllOut4 :: Int -> Int -> IO ()
filterMAllOut4 value = withStream value (filterMAllOut value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllOut4
inspect $ 'filterMAllOut4 `hasNoType` ''Stream.Step
inspect $ 'filterMAllOut4 `hasNoType` ''FL.Step
inspect $ 'filterMAllOut4 `hasNoType` ''SPEC
#endif

{-# INLINE filterMAllIn #-}
filterMAllIn :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filterMAllIn value n = composeN n $ Stream.filterM (\x -> return $ x <= (value + 1))

{-# ANN filterMAllIn1 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMAllIn1 (PermitConstructions [''()]) #-}
{-# ANN filterMAllIn1 (PermitTypeClasses []) #-}
{-# NOINLINE filterMAllIn1 #-}
filterMAllIn1 :: Int -> Int -> IO ()
filterMAllIn1 value = withStream value (filterMAllIn value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllIn1
inspect $ 'filterMAllIn1 `hasNoType` ''Stream.Step
inspect $ 'filterMAllIn1 `hasNoType` ''FL.Step
inspect $ 'filterMAllIn1 `hasNoType` ''SPEC
#endif

{-# ANN filterMAllIn4 (PermitPatternMatches [''Int]) #-}
{-# ANN filterMAllIn4 (PermitConstructions [''()]) #-}
{-# ANN filterMAllIn4 (PermitTypeClasses []) #-}
{-# NOINLINE filterMAllIn4 #-}
filterMAllIn4 :: Int -> Int -> IO ()
filterMAllIn4 value = withStream value (filterMAllIn value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filterMAllIn4
inspect $ 'filterMAllIn4 `hasNoType` ''Stream.Step
inspect $ 'filterMAllIn4 `hasNoType` ''FL.Step
inspect $ 'filterMAllIn4 `hasNoType` ''SPEC
#endif

{-# INLINE dropOne #-}
dropOne :: MonadIO m => Int -> Stream m Int -> m ()
dropOne n = composeN n $ Stream.drop 1

{-# ANN dropOne1 (PermitPatternMatches [''Int]) #-}
{-# ANN dropOne1 (PermitConstructions [''()]) #-}
{-# ANN dropOne1 (PermitTypeClasses []) #-}
{-# NOINLINE dropOne1 #-}
dropOne1 :: Int -> Int -> IO ()
dropOne1 value = withStream value (dropOne 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropOne1
inspect $ 'dropOne1 `hasNoType` ''Stream.Step
inspect $ 'dropOne1 `hasNoType` ''FL.Step
inspect $ 'dropOne1 `hasNoType` ''SPEC
#endif

{-# ANN dropOne4 (PermitPatternMatches [''Int]) #-}
{-# ANN dropOne4 (PermitConstructions [''()]) #-}
{-# ANN dropOne4 (PermitTypeClasses []) #-}
{-# NOINLINE dropOne4 #-}
dropOne4 :: Int -> Int -> IO ()
dropOne4 value = withStream value (dropOne 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropOne4
inspect $ 'dropOne4 `hasNoType` ''Stream.Step
inspect $ 'dropOne4 `hasNoType` ''FL.Step
inspect $ 'dropOne4 `hasNoType` ''SPEC
#endif

{-# INLINE dropAll #-}
dropAll :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropAll value n = composeN n $ Stream.drop (value + 1)

{-# ANN dropAll1 (PermitPatternMatches [''Int]) #-}
{-# ANN dropAll1 (PermitConstructions [''()]) #-}
{-# ANN dropAll1 (PermitTypeClasses []) #-}
{-# NOINLINE dropAll1 #-}
dropAll1 :: Int -> Int -> IO ()
dropAll1 value = withStream value (dropAll value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropAll1
inspect $ 'dropAll1 `hasNoType` ''Stream.Step
inspect $ 'dropAll1 `hasNoType` ''FL.Step
inspect $ 'dropAll1 `hasNoType` ''SPEC
#endif

{-# ANN dropAll4 (PermitPatternMatches [''Int]) #-}
{-# ANN dropAll4 (PermitConstructions [''()]) #-}
{-# ANN dropAll4 (PermitTypeClasses []) #-}
{-# NOINLINE dropAll4 #-}
dropAll4 :: Int -> Int -> IO ()
dropAll4 value = withStream value (dropAll value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropAll4
inspect $ 'dropAll4 `hasNoType` ''Stream.Step
inspect $ 'dropAll4 `hasNoType` ''FL.Step
inspect $ 'dropAll4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileTrue #-}
dropWhileTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileTrue value n = composeN n $ Stream.dropWhile (<= (value + 1))

{-# ANN dropWhileTrue1 (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileTrue1 (PermitConstructions [''()]) #-}
{-# ANN dropWhileTrue1 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileTrue1 #-}
dropWhileTrue1 :: Int -> Int -> IO ()
dropWhileTrue1 value = withStream value (dropWhileTrue value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileTrue1
inspect $ 'dropWhileTrue1 `hasNoType` ''Stream.Step
inspect $ 'dropWhileTrue1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileTrue1 `hasNoType` ''FL.Step
inspect $ 'dropWhileTrue1 `hasNoType` ''SPEC
#endif

{-# ANN dropWhileTrue4 (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileTrue4 (PermitConstructions [''()]) #-}
{-# ANN dropWhileTrue4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileTrue4 #-}
dropWhileTrue4 :: Int -> Int -> IO ()
dropWhileTrue4 value = withStream value (dropWhileTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileTrue4
inspect $ 'dropWhileTrue4 `hasNoType` ''Stream.Step
inspect $ 'dropWhileTrue4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileTrue4 `hasNoType` ''FL.Step
inspect $ 'dropWhileTrue4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileMTrue #-}
dropWhileMTrue :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileMTrue value n = composeN n $ Stream.dropWhileM (return . (<= (value + 1)))

{-# ANN dropWhileMTrue4 (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileMTrue4 (PermitConstructions [''()]) #-}
{-# ANN dropWhileMTrue4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileMTrue4 #-}
dropWhileMTrue4 :: Int -> Int -> IO ()
dropWhileMTrue4 value = withStream value (dropWhileMTrue value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileMTrue4
inspect $ 'dropWhileMTrue4 `hasNoType` ''Stream.Step
inspect $ 'dropWhileMTrue4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileMTrue4 `hasNoType` ''FL.Step
inspect $ 'dropWhileMTrue4 `hasNoType` ''SPEC
#endif

{-# INLINE dropWhileFalse #-}
dropWhileFalse :: MonadIO m => Int -> Int -> Stream m Int -> m ()
dropWhileFalse value n = composeN n $ Stream.dropWhile (> (value + 1))

{-# ANN dropWhileFalse1 (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileFalse1 (PermitConstructions [''()]) #-}
{-# ANN dropWhileFalse1 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileFalse1 #-}
dropWhileFalse1 :: Int -> Int -> IO ()
dropWhileFalse1 value = withStream value (dropWhileFalse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileFalse1
inspect $ 'dropWhileFalse1 `hasNoType` ''Stream.Step
inspect $ 'dropWhileFalse1 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileFalse1 `hasNoType` ''FL.Step
inspect $ 'dropWhileFalse1 `hasNoType` ''SPEC
#endif

{-# ANN dropWhileFalse4 (PermitPatternMatches [''Int]) #-}
{-# ANN dropWhileFalse4 (PermitConstructions [''()]) #-}
{-# ANN dropWhileFalse4 (PermitTypeClasses []) #-}
{-# NOINLINE dropWhileFalse4 #-}
dropWhileFalse4 :: Int -> Int -> IO ()
dropWhileFalse4 value = withStream value (dropWhileFalse value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'dropWhileFalse4
inspect $ 'dropWhileFalse4 `hasNoType` ''Stream.Step
inspect $ 'dropWhileFalse4 `hasNoType` ''Stream.DropWhileState
inspect $ 'dropWhileFalse4 `hasNoType` ''FL.Step
inspect $ 'dropWhileFalse4 `hasNoType` ''SPEC
#endif

{-# INLINE findIndices #-}
findIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
findIndices value n = composeN n $ Stream.findIndices (== (value + 1))

{-# ANN findIndices1 (PermitPatternMatches [''Int]) #-}
{-# ANN findIndices1 (PermitConstructions [''()]) #-}
{-# ANN findIndices1 (PermitTypeClasses []) #-}
{-# NOINLINE findIndices1 #-}
findIndices1 :: Int -> Int -> IO ()
findIndices1 value = withStream value (findIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices1
inspect $ 'findIndices1 `hasNoType` ''Stream.Step
inspect $ 'findIndices1 `hasNoType` ''FL.Step
inspect $ 'findIndices1 `hasNoType` ''SPEC
#endif

{-# ANN findIndices4 (PermitPatternMatches [''Int]) #-}
{-# ANN findIndices4 (PermitConstructions [''()]) #-}
{-# ANN findIndices4 (PermitTypeClasses []) #-}
{-# NOINLINE findIndices4 #-}
findIndices4 :: Int -> Int -> IO ()
findIndices4 value = withStream value (findIndices value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndices4
inspect $ 'findIndices4 `hasNoType` ''Stream.Step
inspect $ 'findIndices4 `hasNoType` ''FL.Step
inspect $ 'findIndices4 `hasNoType` ''SPEC
#endif

{-# INLINE elemIndices #-}
elemIndices :: MonadIO m => Int -> Int -> Stream m Int -> m ()
elemIndices value n = composeN n $ Stream.elemIndices (value + 1)

{-# ANN elemIndices1 (PermitPatternMatches [''Int]) #-}
{-# ANN elemIndices1 (PermitConstructions [''()]) #-}
{-# ANN elemIndices1 (PermitTypeClasses []) #-}
{-# NOINLINE elemIndices1 #-}
elemIndices1 :: Int -> Int -> IO ()
elemIndices1 value = withStream value (elemIndices value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices1
inspect $ 'elemIndices1 `hasNoType` ''Stream.Step
inspect $ 'elemIndices1 `hasNoType` ''FL.Step
inspect $ 'elemIndices1 `hasNoType` ''SPEC
#endif

{-# ANN elemIndices4 (PermitPatternMatches [''Int]) #-}
{-# ANN elemIndices4 (PermitConstructions [''()]) #-}
{-# ANN elemIndices4 (PermitTypeClasses []) #-}
{-# NOINLINE elemIndices4 #-}
elemIndices4 :: Int -> Int -> IO ()
elemIndices4 value = withStream value (elemIndices value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndices4
inspect $ 'elemIndices4 `hasNoType` ''Stream.Step
inspect $ 'elemIndices4 `hasNoType` ''FL.Step
inspect $ 'elemIndices4 `hasNoType` ''SPEC
#endif

{-# ANN findIndex (PermitPatternMatches [''Int]) #-}
{-# ANN findIndex (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN findIndex (PermitTypeClasses []) #-}
{-# NOINLINE findIndex #-}
findIndex :: Int -> Int -> IO (Maybe Int)
findIndex value = withStream value (Stream.head . Stream.findIndices (== (value + 1)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'findIndex
inspect $ 'findIndex `hasNoType` ''Stream.Step
inspect $ 'findIndex `hasNoType` ''FL.Step
inspect $ 'findIndex `hasNoType` ''SPEC
#endif

{-# ANN elemIndex (PermitPatternMatches [''Int]) #-}
{-# ANN elemIndex (PermitConstructions [''Maybe,''Int]) #-}
{-# ANN elemIndex (PermitTypeClasses []) #-}
{-# NOINLINE elemIndex #-}
elemIndex :: Int -> Int -> IO (Maybe Int)
elemIndex value = withStream value (Stream.head . Stream.elemIndices (value + 1))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'elemIndex
inspect $ 'elemIndex `hasNoType` ''Stream.Step
inspect $ 'elemIndex `hasNoType` ''FL.Step
inspect $ 'elemIndex `hasNoType` ''SPEC
#endif

{-# INLINE deleteBy #-}
deleteBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
deleteBy value n = composeN n $ Stream.deleteBy (>=) (value + 1)

{-# ANN deleteBy1 (PermitPatternMatches [''Int]) #-}
{-# ANN deleteBy1 (PermitConstructions [''()]) #-}
{-# ANN deleteBy1 (PermitTypeClasses []) #-}
{-# NOINLINE deleteBy1 #-}
deleteBy1 :: Int -> Int -> IO ()
deleteBy1 value = withStream value (deleteBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'deleteBy1
inspect $ 'deleteBy1 `hasNoType` ''Stream.Step
inspect $ 'deleteBy1 `hasNoType` ''FL.Step
inspect $ 'deleteBy1 `hasNoType` ''SPEC
#endif

{-# ANN deleteBy4 (PermitPatternMatches [''Int]) #-}
{-# ANN deleteBy4 (PermitConstructions [''()]) #-}
{-# ANN deleteBy4 (PermitTypeClasses []) #-}
{-# NOINLINE deleteBy4 #-}
deleteBy4 :: Int -> Int -> IO ()
deleteBy4 value = withStream value (deleteBy value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'deleteBy4
inspect $ 'deleteBy4 `hasNoType` ''Stream.Step
inspect $ 'deleteBy4 `hasNoType` ''FL.Step
inspect $ 'deleteBy4 `hasNoType` ''SPEC
#endif

-- uniq . uniq == uniq, composeN 2 ~ composeN 1
{-# INLINE uniq #-}
uniq :: MonadIO m => Int -> Stream m Int -> m ()
uniq n = composeN n Stream.uniq

{-# ANN uniq1 (PermitPatternMatches [''Int]) #-}
{-# ANN uniq1 (PermitConstructions [''()]) #-}
{-# ANN uniq1 (PermitTypeClasses []) #-}
{-# NOINLINE uniq1 #-}
uniq1 :: Int -> Int -> IO ()
uniq1 value = withStream value (uniq 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uniq1
inspect $ 'uniq1 `hasNoType` ''Stream.Step
inspect $ 'uniq1 `hasNoType` ''FL.Step
inspect $ 'uniq1 `hasNoType` ''SPEC
#endif

{-# ANN uniq4 (PermitPatternMatches [''Int]) #-}
{-# ANN uniq4 (PermitConstructions [''()]) #-}
{-# ANN uniq4 (PermitTypeClasses []) #-}
{-# NOINLINE uniq4 #-}
uniq4 :: Int -> Int -> IO ()
uniq4 value = withStream value (uniq 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'uniq4
inspect $ 'uniq4 `hasNoType` ''Stream.Step
inspect $ 'uniq4 `hasNoType` ''FL.Step
inspect $ 'uniq4 `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybe #-}
mapMaybe :: MonadIO m => Int -> Stream m Int -> m ()
mapMaybe n =
    composeN n $
    Stream.mapMaybe
        (\x ->
             if odd x
             then Nothing
             else Just x)

{-# ANN mapMaybe1 (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybe1 (PermitConstructions [''()]) #-}
{-# ANN mapMaybe1 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybe1 #-}
mapMaybe1 :: Int -> Int -> IO ()
mapMaybe1 value = withStream value (mapMaybe 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybe1
inspect $ 'mapMaybe1 `hasNoType` ''Stream.Step
inspect $ 'mapMaybe1 `hasNoType` ''FL.Step
inspect $ 'mapMaybe1 `hasNoType` ''SPEC
#endif

{-# ANN mapMaybe4 (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybe4 (PermitConstructions [''()]) #-}
{-# ANN mapMaybe4 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybe4 #-}
mapMaybe4 :: Int -> Int -> IO ()
mapMaybe4 value = withStream value (mapMaybe 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybe4
inspect $ 'mapMaybe4 `hasNoType` ''Stream.Step
inspect $ 'mapMaybe4 `hasNoType` ''FL.Step
inspect $ 'mapMaybe4 `hasNoType` ''SPEC
#endif

{-# INLINE mapMaybeM #-}
mapMaybeM :: MonadAsync m => Int -> Stream m Int -> m ()
mapMaybeM n =
    composeN n $
    Stream.mapMaybeM
        (\x ->
             if odd x
             then return Nothing
             else return $ Just x)

{-# ANN mapMaybeM1 (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybeM1 (PermitConstructions [''()]) #-}
{-# ANN mapMaybeM1 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybeM1 #-}
mapMaybeM1 :: Int -> Int -> IO ()
mapMaybeM1 value = withStream value (mapMaybeM 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM1
inspect $ 'mapMaybeM1 `hasNoType` ''Stream.Step
inspect $ 'mapMaybeM1 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM1 `hasNoType` ''SPEC
#endif

{-# ANN mapMaybeM4 (PermitPatternMatches [''Int]) #-}
{-# ANN mapMaybeM4 (PermitConstructions [''()]) #-}
{-# ANN mapMaybeM4 (PermitTypeClasses []) #-}
{-# NOINLINE mapMaybeM4 #-}
mapMaybeM4 :: Int -> Int -> IO ()
mapMaybeM4 value = withStream value (mapMaybeM 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'mapMaybeM4
inspect $ 'mapMaybeM4 `hasNoType` ''Stream.Step
inspect $ 'mapMaybeM4 `hasNoType` ''FL.Step
inspect $ 'mapMaybeM4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size increasing transformations (insertions)
-------------------------------------------------------------------------------

{-# INLINE intersperse #-}
intersperse :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperse value n = composeN n $ Stream.intersperse (value + 1)

{-# ANN intersperse1 (PermitPatternMatches [''Int]) #-}
{-# ANN intersperse1 (PermitConstructions [''()]) #-}
{-# ANN intersperse1 (PermitTypeClasses []) #-}
{-# NOINLINE intersperse1 #-}
intersperse1 :: Int -> Int -> IO ()
intersperse1 value = withStream value (intersperse value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperse1
inspect $ 'intersperse1 `hasNoType` ''Stream.Step
inspect $ 'intersperse1 `hasNoType` ''Stream.LoopState
inspect $ 'intersperse1 `hasNoType` ''FL.Step
inspect $ 'intersperse1 `hasNoType` ''SPEC
#endif

{-# ANN intersperse4 (PermitPatternMatches [''Int,''Stream.LoopState,''SPEC]) #-}
{-# ANN intersperse4 (PermitConstructions [''Int,''Stream.LoopState,''(),''SPEC]) #-}
{-# ANN intersperse4 (PermitTypeClasses []) #-}
{-# NOINLINE intersperse4 #-}
intersperse4 :: Int -> Int -> IO ()
intersperse4 value = withStream value (intersperse value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperse4
inspect $ 'intersperse4 `hasNoType` ''Stream.Step
-- inspect $ 'intersperse4 `hasNoType` ''Stream.LoopState
inspect $ 'intersperse4 `hasNoType` ''FL.Step
-- inspect $ 'intersperse4 `hasNoType` ''SPEC
#endif

{-# INLINE intersperseM #-}
intersperseM :: MonadAsync m => Int -> Int -> Stream m Int -> m ()
intersperseM value n = composeN n $ Stream.intersperseM (return $ value + 1)

{-# ANN intersperseM1 (PermitPatternMatches [''Int]) #-}
{-# ANN intersperseM1 (PermitConstructions [''()]) #-}
{-# ANN intersperseM1 (PermitTypeClasses []) #-}
{-# NOINLINE intersperseM1 #-}
intersperseM1 :: Int -> Int -> IO ()
intersperseM1 value = withStream value (intersperseM value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intersperseM1
inspect $ 'intersperseM1 `hasNoType` ''Stream.Step
inspect $ 'intersperseM1 `hasNoType` ''Stream.LoopState
inspect $ 'intersperseM1 `hasNoType` ''FL.Step
inspect $ 'intersperseM1 `hasNoType` ''SPEC
#endif

{-# INLINE insertBy #-}
insertBy :: MonadIO m => Int -> Int -> Stream m Int -> m ()
insertBy value n = composeN n $ Stream.insertBy compare (value + 1)

{-# ANN insertBy1 (PermitPatternMatches [''Int]) #-}
{-# ANN insertBy1 (PermitConstructions [''()]) #-}
{-# ANN insertBy1 (PermitTypeClasses []) #-}
{-# NOINLINE insertBy1 #-}
insertBy1 :: Int -> Int -> IO ()
insertBy1 value = withStream value (insertBy value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'insertBy1
inspect $ 'insertBy1 `hasNoType` ''Stream.Step
inspect $ 'insertBy1 `hasNoType` ''FL.Step
inspect $ 'insertBy1 `hasNoType` ''SPEC
#endif

{-# ANN insertBy4 (PermitPatternMatches [''Int]) #-}
{-# ANN insertBy4 (PermitConstructions [''Int,''()]) #-}
{-# ANN insertBy4 (PermitTypeClasses []) #-}
{-# NOINLINE insertBy4 #-}
insertBy4 :: Int -> Int -> IO ()
insertBy4 value = withStream value (insertBy value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'insertBy4
inspect $ 'insertBy4 `hasNoType` ''Stream.Step
inspect $ 'insertBy4 `hasNoType` ''FL.Step
inspect $ 'insertBy4 `hasNoType` ''SPEC
#endif

{-# INLINE interposeSuffix #-}
interposeSuffix :: Monad m => Int -> Int -> Stream m Int -> m ()
interposeSuffix value n =
    composeN n $ Stream.unfoldEachSepBy (value + 1) Unfold.identity

{-# ANN interposeSuffix1 (PermitPatternMatches [''Int]) #-}
{-# ANN interposeSuffix1 (PermitConstructions [''()]) #-}
{-# ANN interposeSuffix1 (PermitTypeClasses []) #-}
{-# NOINLINE interposeSuffix1 #-}
interposeSuffix1 :: Int -> Int -> IO ()
interposeSuffix1 value = withStream value (interposeSuffix value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'interposeSuffix1
inspect $ 'interposeSuffix1 `hasNoType` ''Stream.Step
inspect $ 'interposeSuffix1 `hasNoType` ''Stream.InterposeState
inspect $ 'interposeSuffix1 `hasNoType` ''FL.Step
inspect $ 'interposeSuffix1 `hasNoType` ''SPEC
#endif

{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m => Int -> Int -> Stream m Int -> m ()
intercalateSuffix value n =
    composeN n $ Stream.unfoldEachSepBySeq (value + 1) Unfold.identity

{-# ANN intercalateSuffix1 (PermitPatternMatches [''Int]) #-}
{-# ANN intercalateSuffix1 (PermitConstructions [''()]) #-}
{-# ANN intercalateSuffix1 (PermitTypeClasses []) #-}
{-# NOINLINE intercalateSuffix1 #-}
intercalateSuffix1 :: Int -> Int -> IO ()
intercalateSuffix1 value = withStream value (intercalateSuffix value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'intercalateSuffix1
inspect $ 'intercalateSuffix1 `hasNoType` ''Stream.Step
inspect $ 'intercalateSuffix1 `hasNoType` ''Stream.LoopState
inspect $ 'intercalateSuffix1 `hasNoType` ''Producer.ConcatState
inspect $ 'intercalateSuffix1 `hasNoType` ''FL.Step
inspect $ 'intercalateSuffix1 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Indexing
-------------------------------------------------------------------------------

{-# INLINE indexed #-}
indexed :: MonadIO m => Int -> Stream m Int -> m ()
indexed n = composeN n (fmap snd . Stream.indexed)

{-# ANN indexed1 (PermitPatternMatches [''Int]) #-}
{-# ANN indexed1 (PermitConstructions [''()]) #-}
{-# ANN indexed1 (PermitTypeClasses []) #-}
{-# NOINLINE indexed1 #-}
indexed1 :: Int -> Int -> IO ()
indexed1 value = withStream value (indexed 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexed1
inspect $ 'indexed1 `hasNoType` ''Stream.Step
inspect $ 'indexed1 `hasNoType` ''FL.Step
inspect $ 'indexed1 `hasNoType` ''SPEC
#endif

{-# ANN indexed4 (PermitPatternMatches [''Int]) #-}
{-# ANN indexed4 (PermitConstructions [''()]) #-}
{-# ANN indexed4 (PermitTypeClasses []) #-}
{-# NOINLINE indexed4 #-}
indexed4 :: Int -> Int -> IO ()
indexed4 value = withStream value (indexed 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexed4
inspect $ 'indexed4 `hasNoType` ''Stream.Step
inspect $ 'indexed4 `hasNoType` ''FL.Step
inspect $ 'indexed4 `hasNoType` ''SPEC
#endif

{-# INLINE indexedR #-}
indexedR :: MonadIO m => Int -> Int -> Stream m Int -> m ()
indexedR value n = composeN n (fmap snd . Stream.indexedR value)

{-# ANN indexedR1 (PermitPatternMatches [''Int]) #-}
{-# ANN indexedR1 (PermitConstructions [''()]) #-}
{-# ANN indexedR1 (PermitTypeClasses []) #-}
{-# NOINLINE indexedR1 #-}
indexedR1 :: Int -> Int -> IO ()
indexedR1 value = withStream value (indexedR value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR1
inspect $ 'indexedR1 `hasNoType` ''Stream.Step
inspect $ 'indexedR1 `hasNoType` ''FL.Step
inspect $ 'indexedR1 `hasNoType` ''SPEC
#endif

{-# ANN indexedR4 (PermitPatternMatches [''Int]) #-}
{-# ANN indexedR4 (PermitConstructions [''()]) #-}
{-# ANN indexedR4 (PermitTypeClasses []) #-}
{-# NOINLINE indexedR4 #-}
indexedR4 :: Int -> Int -> IO ()
indexedR4 value = withStream value (indexedR value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'indexedR4
inspect $ 'indexedR4 `hasNoType` ''Stream.Step
inspect $ 'indexedR4 `hasNoType` ''FL.Step
inspect $ 'indexedR4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Size conserving transformations (reordering, buffering, etc.)
-------------------------------------------------------------------------------

{-# ANN reverse (PermitPatternMatches [''[],''Int,''SPEC]) #-}
{-# ANN reverse (PermitConstructions [''[],''Int,''(),''SPEC]) #-}
{-# ANN reverse (PermitTypeClasses []) #-}
{-# NOINLINE reverse #-}
reverse :: Int -> Int -> IO ()
reverse value = withStream value (composeN 1 Stream.reverse)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverse
inspect $ 'reverse `hasNoType` ''Stream.Step
inspect $ 'reverse `hasNoType` ''FL.Step
-- inspect $ 'reverse `hasNoType` ''SPEC
#endif

{-# ANN reverse' (PermitPatternMatches [''Int,''Array,''MutArray.GroupState,''MutByteArray,''SVar.State,''Stream.Step]) #-}
{-# ANN reverse' (PermitConstructions [''Array,''Maybe,''SVar.State,''MutArray.GroupState,''Int,''MutByteArray,''Stream.Step,''(),''Bool]) #-}
{-# ANN reverse' (PermitTypeClasses []) #-}
{-# NOINLINE reverse' #-}
reverse' :: Int -> Int -> IO ()
reverse' value = withStream value (composeN 1 Stream.reverseUnbox)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'reverse'
-- inspect $ 'reverse' `hasNoType` ''Stream.Step
#endif

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    [
    -- , benchIOSink value "foldrT" (foldrT 1)
    -- , benchIOSink value "foldrTMap" (foldrTMap 1)

    -- Mapping
      (SpaceO_1, benchIO "sequence" $ sequence1 size)
    , (SpaceO_1, benchIO "tap" $ tap1 size)
    -- XXX tasty-bench hangs benchmarking this
    -- , benchIOSink value "timestamped" _timestamped
    -- Scanning
    , (SpaceO_1, benchIO "scanl'" $ scanl'1 size)
    , (SpaceO_1, benchIO "scanl1'" $ scanl1'1 size)
    , (SpaceO_1, benchIO "scanlM'" $ scanlM'1 size)
    , (SpaceO_1, benchIO "scanl1M'" $ scanl1M'1 size)
    , (SpaceO_1, benchIO "postscanl'" $ postscanl'1 size)
    , (SpaceO_1, benchIO "postscanlM'" $ postscanlM'1 size)
    , (SpaceO_1, benchIO "scan" $ scan1 size)
    , (SpaceO_1, benchIO "postscan" $ postscan1 size)
    , (SpaceO_1, benchIO "trace x 4" $ trace4 size)

    , (SpaceO_1, benchIO "scanl' x 4" $ scanl'4 size)
    , (SpaceO_1, benchIO "scanl1' x 4" $ scanl1'4 size)
    , (SpaceO_1, benchIO "scanlM' x 4" $ scanlM'4 size)
    , (SpaceO_1, benchIO "scanl1M' x 4" $ scanl1M'4 size)
    , (SpaceO_1, benchIO "postscanl' x 4" $ postscanl'4 size)
    , (SpaceO_1, benchIO "postscanlM' x 4" $ postscanlM'4 size)
    , (SpaceO_1, benchIO "scan x 4" $ scan4 size)
    , (SpaceO_1, benchIO "postscan x 4" $ postscan4 size)
    , (SpaceO_1, benchIO "filter-even" $ filterEven1 size)
    , (SpaceO_1, benchIO "filter-all-out" $ filterAllOut1 size)
    , (SpaceO_1, benchIO "filter-all-in" $ filterAllIn1 size)

    , (SpaceO_1, benchIO "filterM-even" $ filterMEven1 size)
    , (SpaceO_1, benchIO "filterM-all-out" $ filterMAllOut1 size)
    , (SpaceO_1, benchIO "filterM-all-in" $ filterMAllIn1 size)

    , (SpaceO_1, benchIO "drop-one" $ dropOne1 size)
    , (SpaceO_1, benchIO "drop-all" $ dropAll1 size)
    , (SpaceO_1, benchIO "dropWhile-true" $ dropWhileTrue1 size)
 -- , (SpaceO_1, benchIO "dropWhileM-true" ...)
    , (SpaceO_1, benchIO "dropWhile-false" $ dropWhileFalse1 size)
    , (SpaceO_1, benchIO "deleteBy" $ deleteBy1 size)

    , (SpaceO_1, benchIO "uniq" $ uniq1 size)

    -- Map and filter
    , (SpaceO_1, benchIO "mapMaybe" $ mapMaybe1 size)
    , (SpaceO_1, benchIO "mapMaybeM" $ mapMaybeM1 size)

    -- Searching (stateful map and filter)
    , (SpaceO_1, benchIO "findIndices" $ findIndices1 size)
    , (SpaceO_1, benchIO "elemIndices" $ elemIndices1 size)
    , (SpaceO_1, benchIO "findIndex" $ findIndex size)
    , (SpaceO_1, benchIO "elemIndex" $ elemIndex size)
    , (SpaceO_1, benchIO "filter-even x 4" $ filterEven4 size)
    , (SpaceO_1, benchIO "filter-all-out x 4" $ filterAllOut4 size)
    , (SpaceO_1, benchIO "filter-all-in x 4" $ filterAllIn4 size)

    , (SpaceO_1, benchIO "filterM-even x 4" $ filterMEven4 size)
    , (SpaceO_1, benchIO "filterM-all-out x 4" $ filterMAllOut4 size)
    , (SpaceO_1, benchIO "filterM-all-in x 4" $ filterMAllIn4 size)

    , (SpaceO_1, benchIO "drop-one x 4" $ dropOne4 size)
    , (SpaceO_1, benchIO "drop-all x 4" $ dropAll4 size)
    , (SpaceO_1, benchIO "dropWhile-true x 4" $ dropWhileTrue4 size)
    , (SpaceO_1, benchIO "dropWhileM-true x 4" $ dropWhileMTrue4 size)
    -- XXX requires @-fspec-constr-recursive=12@.
    , (SpaceO_1, benchIO "dropWhile-false x 4" $ dropWhileFalse4 size)
    , (SpaceO_1, benchIO "deleteBy x 4" $ deleteBy4 size)

    , (SpaceO_1, benchIO "uniq x 4" $ uniq4 size)

    -- map and filter
    , (SpaceO_1, benchIO "mapMaybe x 4" $ mapMaybe4 size)
    , (SpaceO_1, benchIO "mapMaybeM x 4" $ mapMaybeM4 size)

    -- searching
    , (SpaceO_1, benchIO "findIndices x 4" $ findIndices4 size)
    , (SpaceO_1, benchIO "elemIndices x 4" $ elemIndices4 size)
    , (SpaceO_1, benchIO "intersperse" $ intersperse1 size)
    , (SpaceO_1, benchIO "intersperseM" $ intersperseM1 size)
    , (SpaceO_1, benchIO "insertBy" $ insertBy1 size)
    , (SpaceO_1, benchIO "interposeSuffix" $ interposeSuffix1 size)
    , (SpaceO_1, benchIO "intercalateSuffix" $ intercalateSuffix1 size)
    -- XXX requires @-fspec-constr-recursive=16@.
    , (SpaceO_1, benchIO "intersperse x 4" $ intersperse4 size)
    , (SpaceO_1, benchIO "insertBy x 4" $ insertBy4 size)
    , (SpaceO_1, benchIO "indexed" $ indexed1 size)
    , (SpaceO_1, benchIO "indexedR" $ indexedR1 size)
    , (SpaceO_1, benchIO "indexed x 4" $ indexed4 size)
    , (SpaceO_1, benchIO "indexedR x 4" $ indexedR4 size)
    -- Reversing a stream
    , (HeapO_n, benchIO "reverse" $ reverse size)
    , (HeapO_n, benchIO "reverse'" $ reverse' size)
    ]
