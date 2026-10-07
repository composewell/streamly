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
import GHC.Types (SPEC(..))
#endif

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Generics (Generic)
import Streamly.Internal.Data.Stream (Stream, Step, DropWhileState, ScanState)

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import Streamly.Internal.Data.SVar.Type (State)
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

{-# INLINE scanl'_fmap #-}
scanl'_fmap :: MonadIO m => Int -> Stream m Int -> m ()
scanl'_fmap n = composeN n $ fmap (subtract 1) . Common.scanl' (+) 0

{-# ANN scanl'_fmap_x1 (PermitPatternMatches []) #-}
{-# ANN scanl'_fmap_x1 (PermitConstructions []) #-}
{-# ANN scanl'_fmap_x1 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_fmap_x1 #-}
scanl'_fmap_x1 :: Int -> Int -> IO ()
scanl'_fmap_x1 value = withStream value (scanl'_fmap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'_fmap_x1
inspect $ 'scanl'_fmap_x1 `hasNoType` ''S.Step
inspect $ 'scanl'_fmap_x1 `hasNoType` ''S.ScanState
inspect $ 'scanl'_fmap_x1 `hasNoType` ''FL.Step
inspect $ 'scanl'_fmap_x1 `hasNoType` ''SPEC
#endif

{-# ANN scanl'_fmap_x2 (PermitPatternMatches []) #-}
{-# ANN scanl'_fmap_x2 (PermitConstructions []) #-}
{-# ANN scanl'_fmap_x2 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_fmap_x2 #-}
scanl'_fmap_x2 :: Int -> Int -> IO ()
scanl'_fmap_x2 value = withStream value (scanl'_fmap 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'_fmap_x2
inspect $ 'scanl'_fmap_x2 `hasNoType` ''S.Step
inspect $ 'scanl'_fmap_x2 `hasNoType` ''S.ScanState
inspect $ 'scanl'_fmap_x2 `hasNoType` ''FL.Step
inspect $ 'scanl'_fmap_x2 `hasNoType` ''SPEC
#endif

{-# ANN scanl'_fmap_x4 (PermitPatternMatches []) #-}
{-# ANN scanl'_fmap_x4 (PermitConstructions []) #-}
{-# ANN scanl'_fmap_x4 (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_fmap_x4 #-}
scanl'_fmap_x4 :: Int -> Int -> IO ()
scanl'_fmap_x4 value = withStream value (scanl'_fmap 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'_fmap_x4
inspect $ 'scanl'_fmap_x4 `hasNoType` ''S.Step
inspect $ 'scanl'_fmap_x4 `hasNoType` ''S.ScanState
inspect $ 'scanl'_fmap_x4 `hasNoType` ''FL.Step
inspect $ 'scanl'_fmap_x4 `hasNoType` ''SPEC
#endif

{-# INLINE drop_fmap #-}
drop_fmap :: MonadIO m => Int -> Stream m Int -> m ()
drop_fmap n = composeN n $ fmap (subtract 1) . S.drop 1

{-# ANN drop_fmap_x1 (PermitPatternMatches []) #-}
{-# ANN drop_fmap_x1 (PermitConstructions []) #-}
{-# ANN drop_fmap_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_fmap_x1 #-}
drop_fmap_x1 :: Int -> Int -> IO ()
drop_fmap_x1 value = withStream value (drop_fmap 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_fmap_x1
inspect $ 'drop_fmap_x1 `hasNoType` ''S.Step
inspect $ 'drop_fmap_x1 `hasNoType` ''FL.Step
inspect $ 'drop_fmap_x1 `hasNoType` ''SPEC
#endif

{-# ANN drop_fmap_x2 (PermitPatternMatches []) #-}
{-# ANN drop_fmap_x2 (PermitConstructions []) #-}
{-# ANN drop_fmap_x2 (PermitTypeClasses []) #-}
{-# NOINLINE drop_fmap_x2 #-}
drop_fmap_x2 :: Int -> Int -> IO ()
drop_fmap_x2 value = withStream value (drop_fmap 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_fmap_x2
inspect $ 'drop_fmap_x2 `hasNoType` ''S.Step
inspect $ 'drop_fmap_x2 `hasNoType` ''FL.Step
inspect $ 'drop_fmap_x2 `hasNoType` ''SPEC
#endif

{-# ANN drop_fmap_x4 (PermitPatternMatches []) #-}
{-# ANN drop_fmap_x4 (PermitConstructions []) #-}
{-# ANN drop_fmap_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_fmap_x4 #-}
drop_fmap_x4 :: Int -> Int -> IO ()
drop_fmap_x4 value = withStream value (drop_fmap 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_fmap_x4
inspect $ 'drop_fmap_x4 `hasNoType` ''S.Step
inspect $ 'drop_fmap_x4 `hasNoType` ''FL.Step
inspect $ 'drop_fmap_x4 `hasNoType` ''SPEC
#endif

{-# INLINE drop_scanl' #-}
drop_scanl' :: MonadIO m => Int -> Stream m Int -> m ()
drop_scanl' n = composeN n $ Common.scanl' (+) 0 . S.drop 1

{-# ANN drop_scanl'_x1 (PermitPatternMatches []) #-}
{-# ANN drop_scanl'_x1 (PermitConstructions []) #-}
{-# ANN drop_scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE drop_scanl'_x1 #-}
drop_scanl'_x1 :: Int -> Int -> IO ()
drop_scanl'_x1 value = withStream value (drop_scanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_scanl'_x1
inspect $ 'drop_scanl'_x1 `hasNoType` ''S.Step
inspect $ 'drop_scanl'_x1 `hasNoType` ''S.ScanState
inspect $ 'drop_scanl'_x1 `hasNoType` ''FL.Step
inspect $ 'drop_scanl'_x1 `hasNoType` ''SPEC
#endif

{-# ANN drop_scanl'_x2 (PermitPatternMatches []) #-}
{-# ANN drop_scanl'_x2 (PermitConstructions []) #-}
{-# ANN drop_scanl'_x2 (PermitTypeClasses []) #-}
{-# NOINLINE drop_scanl'_x2 #-}
drop_scanl'_x2 :: Int -> Int -> IO ()
drop_scanl'_x2 value = withStream value (drop_scanl' 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_scanl'_x2
inspect $ 'drop_scanl'_x2 `hasNoType` ''S.Step
inspect $ 'drop_scanl'_x2 `hasNoType` ''S.ScanState
inspect $ 'drop_scanl'_x2 `hasNoType` ''FL.Step
inspect $ 'drop_scanl'_x2 `hasNoType` ''SPEC
#endif

{-# ANN drop_scanl'_x4 (PermitPatternMatches []) #-}
{-# ANN drop_scanl'_x4 (PermitConstructions []) #-}
{-# ANN drop_scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE drop_scanl'_x4 #-}
drop_scanl'_x4 :: Int -> Int -> IO ()
drop_scanl'_x4 value = withStream value (drop_scanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'drop_scanl'_x4
inspect $ 'drop_scanl'_x4 `hasNoType` ''S.Step
inspect $ 'drop_scanl'_x4 `hasNoType` ''S.ScanState
inspect $ 'drop_scanl'_x4 `hasNoType` ''FL.Step
inspect $ 'drop_scanl'_x4 `hasNoType` ''SPEC
#endif

{-# INLINE take_drop #-}
take_drop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
take_drop value n = composeN n $ S.drop 1 . S.take (value + 1)

{-# ANN take_drop_x1 (PermitPatternMatches []) #-}
{-# ANN take_drop_x1 (PermitConstructions []) #-}
{-# ANN take_drop_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_drop_x1 #-}
take_drop_x1 :: Int -> Int -> IO ()
take_drop_x1 value = withStream value (take_drop value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_drop_x1
inspect $ 'take_drop_x1 `hasNoType` ''S.Step
inspect $ 'take_drop_x1 `hasNoType` ''FL.Step
inspect $ 'take_drop_x1 `hasNoType` ''SPEC
#endif

{-# ANN take_drop_x2 (PermitPatternMatches []) #-}
{-# ANN take_drop_x2 (PermitConstructions []) #-}
{-# ANN take_drop_x2 (PermitTypeClasses []) #-}
{-# NOINLINE take_drop_x2 #-}
take_drop_x2 :: Int -> Int -> IO ()
take_drop_x2 value = withStream value (take_drop value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_drop_x2
inspect $ 'take_drop_x2 `hasNoType` ''S.Step
inspect $ 'take_drop_x2 `hasNoType` ''FL.Step
inspect $ 'take_drop_x2 `hasNoType` ''SPEC
#endif

{-# ANN take_drop_x4 (PermitPatternMatches []) #-}
{-# ANN take_drop_x4 (PermitConstructions []) #-}
{-# ANN take_drop_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_drop_x4 #-}
take_drop_x4 :: Int -> Int -> IO ()
take_drop_x4 value = withStream value (take_drop value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_drop_x4
inspect $ 'take_drop_x4 `hasNoType` ''S.Step
inspect $ 'take_drop_x4 `hasNoType` ''FL.Step
inspect $ 'take_drop_x4 `hasNoType` ''SPEC
#endif

{-# INLINE take_scanl' #-}
take_scanl' :: MonadIO m => Int -> Int -> Stream m Int -> m ()
take_scanl' value n = composeN n $ Common.scanl' (+) 0 . S.take (value + 1)

{-# ANN take_scanl'_x1 (PermitPatternMatches []) #-}
{-# ANN take_scanl'_x1 (PermitConstructions []) #-}
{-# ANN take_scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_scanl'_x1 #-}
take_scanl'_x1 :: Int -> Int -> IO ()
take_scanl'_x1 value = withStream value (take_scanl' value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_scanl'_x1
inspect $ 'take_scanl'_x1 `hasNoType` ''S.Step
inspect $ 'take_scanl'_x1 `hasNoType` ''S.ScanState
inspect $ 'take_scanl'_x1 `hasNoType` ''FL.Step
inspect $ 'take_scanl'_x1 `hasNoType` ''SPEC
#endif

{-# ANN take_scanl'_x2 (PermitPatternMatches []) #-}
{-# ANN take_scanl'_x2 (PermitConstructions []) #-}
{-# ANN take_scanl'_x2 (PermitTypeClasses []) #-}
{-# NOINLINE take_scanl'_x2 #-}
take_scanl'_x2 :: Int -> Int -> IO ()
take_scanl'_x2 value = withStream value (take_scanl' value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_scanl'_x2
inspect $ 'take_scanl'_x2 `hasNoType` ''S.Step
inspect $ 'take_scanl'_x2 `hasNoType` ''S.ScanState
inspect $ 'take_scanl'_x2 `hasNoType` ''FL.Step
inspect $ 'take_scanl'_x2 `hasNoType` ''SPEC
#endif

{-# ANN take_scanl'_x4 (PermitPatternMatches []) #-}
{-# ANN take_scanl'_x4 (PermitConstructions []) #-}
{-# ANN take_scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_scanl'_x4 #-}
take_scanl'_x4 :: Int -> Int -> IO ()
take_scanl'_x4 value = withStream value (take_scanl' value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_scanl'_x4
inspect $ 'take_scanl'_x4 `hasNoType` ''S.Step
inspect $ 'take_scanl'_x4 `hasNoType` ''S.ScanState
inspect $ 'take_scanl'_x4 `hasNoType` ''FL.Step
inspect $ 'take_scanl'_x4 `hasNoType` ''SPEC
#endif

{-# INLINE take_fmap #-}
take_fmap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
take_fmap value n = composeN n $ fmap (subtract 1) . S.take (value + 1)

{-# ANN take_fmap_x1 (PermitPatternMatches []) #-}
{-# ANN take_fmap_x1 (PermitConstructions []) #-}
{-# ANN take_fmap_x1 (PermitTypeClasses []) #-}
{-# NOINLINE take_fmap_x1 #-}
take_fmap_x1 :: Int -> Int -> IO ()
take_fmap_x1 value = withStream value (take_fmap value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_fmap_x1
inspect $ 'take_fmap_x1 `hasNoType` ''S.Step
inspect $ 'take_fmap_x1 `hasNoType` ''FL.Step
inspect $ 'take_fmap_x1 `hasNoType` ''SPEC
#endif

{-# ANN take_fmap_x2 (PermitPatternMatches []) #-}
{-# ANN take_fmap_x2 (PermitConstructions []) #-}
{-# ANN take_fmap_x2 (PermitTypeClasses []) #-}
{-# NOINLINE take_fmap_x2 #-}
take_fmap_x2 :: Int -> Int -> IO ()
take_fmap_x2 value = withStream value (take_fmap value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_fmap_x2
inspect $ 'take_fmap_x2 `hasNoType` ''S.Step
inspect $ 'take_fmap_x2 `hasNoType` ''FL.Step
inspect $ 'take_fmap_x2 `hasNoType` ''SPEC
#endif

{-# ANN take_fmap_x4 (PermitPatternMatches []) #-}
{-# ANN take_fmap_x4 (PermitConstructions []) #-}
{-# ANN take_fmap_x4 (PermitTypeClasses []) #-}
{-# NOINLINE take_fmap_x4 #-}
take_fmap_x4 :: Int -> Int -> IO ()
take_fmap_x4 value = withStream value (take_fmap value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'take_fmap_x4
inspect $ 'take_fmap_x4 `hasNoType` ''S.Step
inspect $ 'take_fmap_x4 `hasNoType` ''FL.Step
inspect $ 'take_fmap_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filter_drop #-}
filter_drop :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filter_drop value n = composeN n $ S.drop 1 . S.filter (<= (value + 1))

{-# ANN filter_drop_x1 (PermitPatternMatches []) #-}
{-# ANN filter_drop_x1 (PermitConstructions []) #-}
{-# ANN filter_drop_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_drop_x1 #-}
filter_drop_x1 :: Int -> Int -> IO ()
filter_drop_x1 value = withStream value (filter_drop value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_drop_x1
inspect $ 'filter_drop_x1 `hasNoType` ''S.Step
inspect $ 'filter_drop_x1 `hasNoType` ''FL.Step
inspect $ 'filter_drop_x1 `hasNoType` ''SPEC
#endif

{-# ANN filter_drop_x2 (PermitPatternMatches []) #-}
{-# ANN filter_drop_x2 (PermitConstructions []) #-}
{-# ANN filter_drop_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_drop_x2 #-}
filter_drop_x2 :: Int -> Int -> IO ()
filter_drop_x2 value = withStream value (filter_drop value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_drop_x2
inspect $ 'filter_drop_x2 `hasNoType` ''S.Step
inspect $ 'filter_drop_x2 `hasNoType` ''FL.Step
inspect $ 'filter_drop_x2 `hasNoType` ''SPEC
#endif

{-# ANN filter_drop_x4 (PermitPatternMatches []) #-}
{-# ANN filter_drop_x4 (PermitConstructions []) #-}
{-# ANN filter_drop_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_drop_x4 #-}
filter_drop_x4 :: Int -> Int -> IO ()
filter_drop_x4 value = withStream value (filter_drop value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_drop_x4
inspect $ 'filter_drop_x4 `hasNoType` ''S.Step
inspect $ 'filter_drop_x4 `hasNoType` ''FL.Step
inspect $ 'filter_drop_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filter_take #-}
filter_take :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filter_take value n =
    composeN n $ S.take (value + 1) . S.filter (<= (value + 1))

{-# ANN filter_take_x1 (PermitPatternMatches []) #-}
{-# ANN filter_take_x1 (PermitConstructions []) #-}
{-# ANN filter_take_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_take_x1 #-}
filter_take_x1 :: Int -> Int -> IO ()
filter_take_x1 value = withStream value (filter_take value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_take_x1
inspect $ 'filter_take_x1 `hasNoType` ''S.Step
inspect $ 'filter_take_x1 `hasNoType` ''FL.Step
inspect $ 'filter_take_x1 `hasNoType` ''SPEC
#endif

{-# ANN filter_take_x2 (PermitPatternMatches []) #-}
{-# ANN filter_take_x2 (PermitConstructions []) #-}
{-# ANN filter_take_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_take_x2 #-}
filter_take_x2 :: Int -> Int -> IO ()
filter_take_x2 value = withStream value (filter_take value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_take_x2
inspect $ 'filter_take_x2 `hasNoType` ''S.Step
inspect $ 'filter_take_x2 `hasNoType` ''FL.Step
inspect $ 'filter_take_x2 `hasNoType` ''SPEC
#endif

{-# ANN filter_take_x4 (PermitPatternMatches []) #-}
{-# ANN filter_take_x4 (PermitConstructions []) #-}
{-# ANN filter_take_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_take_x4 #-}
filter_take_x4 :: Int -> Int -> IO ()
filter_take_x4 value = withStream value (filter_take value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_take_x4
inspect $ 'filter_take_x4 `hasNoType` ''S.Step
inspect $ 'filter_take_x4 `hasNoType` ''FL.Step
inspect $ 'filter_take_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filter_scanl' #-}
filter_scanl' :: MonadIO m => Int -> Stream m Int -> m ()
filter_scanl' n = composeN n $ Common.scanl' (+) 0 . S.filter (<= maxBound)

{-# ANN filter_scanl'_x1 (PermitPatternMatches []) #-}
{-# ANN filter_scanl'_x1 (PermitConstructions []) #-}
{-# ANN filter_scanl'_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl'_x1 #-}
filter_scanl'_x1 :: Int -> Int -> IO ()
filter_scanl'_x1 value = withStream value (filter_scanl' 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_scanl'_x1
inspect $ 'filter_scanl'_x1 `hasNoType` ''S.Step
inspect $ 'filter_scanl'_x1 `hasNoType` ''S.ScanState
inspect $ 'filter_scanl'_x1 `hasNoType` ''FL.Step
inspect $ 'filter_scanl'_x1 `hasNoType` ''SPEC
#endif

{-# ANN filter_scanl'_x2 (PermitPatternMatches []) #-}
{-# ANN filter_scanl'_x2 (PermitConstructions []) #-}
{-# ANN filter_scanl'_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl'_x2 #-}
filter_scanl'_x2 :: Int -> Int -> IO ()
filter_scanl'_x2 value = withStream value (filter_scanl' 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_scanl'_x2
inspect $ 'filter_scanl'_x2 `hasNoType` ''S.Step
inspect $ 'filter_scanl'_x2 `hasNoType` ''S.ScanState
inspect $ 'filter_scanl'_x2 `hasNoType` ''FL.Step
inspect $ 'filter_scanl'_x2 `hasNoType` ''SPEC
#endif

{-# ANN filter_scanl'_x4 (PermitPatternMatches []) #-}
{-# ANN filter_scanl'_x4 (PermitConstructions []) #-}
{-# ANN filter_scanl'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl'_x4 #-}
filter_scanl'_x4 :: Int -> Int -> IO ()
filter_scanl'_x4 value = withStream value (filter_scanl' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_scanl'_x4
inspect $ 'filter_scanl'_x4 `hasNoType` ''S.Step
inspect $ 'filter_scanl'_x4 `hasNoType` ''S.ScanState
inspect $ 'filter_scanl'_x4 `hasNoType` ''FL.Step
inspect $ 'filter_scanl'_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filter_scanl1' #-}
filter_scanl1' :: MonadIO m => Int -> Stream m Int -> m ()
filter_scanl1' n = composeN n $ S.scanl1' (+) . S.filter (<= maxBound)

{-# ANN filter_scanl1'_x2 (PermitPatternMatches []) #-}
{-# ANN filter_scanl1'_x2 (PermitConstructions []) #-}
{-# ANN filter_scanl1'_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl1'_x2 #-}
filter_scanl1'_x2 :: Int -> Int -> IO ()
filter_scanl1'_x2 value = withStream value (filter_scanl1' 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_scanl1'_x2
inspect $ 'filter_scanl1'_x2 `hasNoType` ''S.Step
inspect $ 'filter_scanl1'_x2 `hasNoType` ''S.ScanState
inspect $ 'filter_scanl1'_x2 `hasNoType` ''FL.Step
inspect $ 'filter_scanl1'_x2 `hasNoType` ''SPEC
#endif

{-# ANN filter_scanl1'_x4 (PermitPatternMatches []) #-}
{-# ANN filter_scanl1'_x4 (PermitConstructions []) #-}
{-# ANN filter_scanl1'_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_scanl1'_x4 #-}
filter_scanl1'_x4 :: Int -> Int -> IO ()
filter_scanl1'_x4 value = withStream value (filter_scanl1' 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_scanl1'_x4
inspect $ 'filter_scanl1'_x4 `hasNoType` ''S.Step
inspect $ 'filter_scanl1'_x4 `hasNoType` ''S.ScanState
inspect $ 'filter_scanl1'_x4 `hasNoType` ''FL.Step
inspect $ 'filter_scanl1'_x4 `hasNoType` ''SPEC
#endif

{-# INLINE filter_fmap #-}
filter_fmap :: MonadIO m => Int -> Int -> Stream m Int -> m ()
filter_fmap value n = composeN n $ fmap (subtract 1) . S.filter (<= (value + 1))

{-# ANN filter_fmap_x1 (PermitPatternMatches []) #-}
{-# ANN filter_fmap_x1 (PermitConstructions []) #-}
{-# ANN filter_fmap_x1 (PermitTypeClasses []) #-}
{-# NOINLINE filter_fmap_x1 #-}
filter_fmap_x1 :: Int -> Int -> IO ()
filter_fmap_x1 value = withStream value (filter_fmap value 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_fmap_x1
inspect $ 'filter_fmap_x1 `hasNoType` ''S.Step
inspect $ 'filter_fmap_x1 `hasNoType` ''FL.Step
inspect $ 'filter_fmap_x1 `hasNoType` ''SPEC
#endif

{-# ANN filter_fmap_x2 (PermitPatternMatches []) #-}
{-# ANN filter_fmap_x2 (PermitConstructions []) #-}
{-# ANN filter_fmap_x2 (PermitTypeClasses []) #-}
{-# NOINLINE filter_fmap_x2 #-}
filter_fmap_x2 :: Int -> Int -> IO ()
filter_fmap_x2 value = withStream value (filter_fmap value 2)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_fmap_x2
inspect $ 'filter_fmap_x2 `hasNoType` ''S.Step
inspect $ 'filter_fmap_x2 `hasNoType` ''FL.Step
inspect $ 'filter_fmap_x2 `hasNoType` ''SPEC
#endif

{-# ANN filter_fmap_x4 (PermitPatternMatches []) #-}
{-# ANN filter_fmap_x4 (PermitConstructions []) #-}
{-# ANN filter_fmap_x4 (PermitTypeClasses []) #-}
{-# NOINLINE filter_fmap_x4 #-}
filter_fmap_x4 :: Int -> Int -> IO ()
filter_fmap_x4 value = withStream value (filter_fmap value 4)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'filter_fmap_x4
inspect $ 'filter_fmap_x4 `hasNoType` ''S.Step
inspect $ 'filter_fmap_x4 `hasNoType` ''FL.Step
inspect $ 'filter_fmap_x4 `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Scan and fold
-------------------------------------------------------------------------------

data Pair a b =
    Pair !a !b
    deriving (Generic, NFData)

{-# ANN foldl'_SumProduct (PermitPatternMatches []) #-}
{-# ANN foldl'_SumProduct (PermitConstructions [''Pair,''Int]) #-}
{-# ANN foldl'_SumProduct (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_SumProduct #-}
foldl'_SumProduct :: Int -> Int -> IO (Pair Int Int)
foldl'_SumProduct value =
    withStream value $
        Common.foldl' (\(Pair s p) x -> Pair (s + x) (p * x)) (Pair 0 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'_SumProduct
inspect $ 'foldl'_SumProduct `hasNoType` ''S.Step
inspect $ 'foldl'_SumProduct `hasNoType` ''FL.Step
inspect $ 'foldl'_SumProduct `hasNoType` ''SPEC
#endif

{-# ANN scanl'_foldl'_SumProduct (PermitPatternMatches []) #-}
{-# ANN scanl'_foldl'_SumProduct (PermitConstructions [''Int,''Pair]) #-}
{-# ANN scanl'_foldl'_SumProduct (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_foldl'_SumProduct #-}
scanl'_foldl'_SumProduct :: Int -> Int -> IO (Pair Int Int)
scanl'_foldl'_SumProduct value =
    withStream value $
        Common.foldl' (\(Pair _ p) (s0, x) -> Pair s0 (p * x)) (Pair 0 1) .
        Common.scanl' (\(s, _) x -> (s + x, x)) (0, 0)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'scanl'_foldl'_SumProduct
inspect $ 'scanl'_foldl'_SumProduct `hasNoType` ''S.Step
inspect $ 'scanl'_foldl'_SumProduct `hasNoType` ''S.ScanState
inspect $ 'scanl'_foldl'_SumProduct `hasNoType` ''FL.Step
inspect $ 'scanl'_foldl'_SumProduct `hasNoType` ''SPEC
#endif

{-# ANN foldl'_fmap (PermitPatternMatches []) #-}
{-# ANN foldl'_fmap (PermitConstructions [''Int]) #-}
{-# ANN foldl'_fmap (PermitTypeClasses []) #-}
{-# NOINLINE foldl'_fmap #-}
foldl'_fmap :: Int -> Int -> IO Int
foldl'_fmap value = withStream value $ fmap (+ 1) . Common.foldl' (+) 0

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'foldl'_fmap
inspect $ 'foldl'_fmap `hasNoType` ''S.Step
inspect $ 'foldl'_fmap `hasNoType` ''FL.Step
inspect $ 'foldl'_fmap `hasNoType` ''SPEC
#endif

-------------------------------------------------------------------------------
-- Iterating a transformation over and over again
-------------------------------------------------------------------------------

-- this is quadratic
{-# ANN scanl'_Iterated (PermitPatternMatches
    [''Int,''Step,''ScanState,''Stream,''State]) #-}
{-# ANN scanl'_Iterated (PermitConstructions
    [''Int,''Stream,''ScanState,''Step,''State,''Maybe
    ,''(),''Bool]) #-}
{-# ANN scanl'_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE scanl'_Iterated #-}
scanl'_Iterated :: Int -> Int -> Int -> IO ()
scanl'_Iterated value iterCount =
    Common.drain . iterateSource (Common.scanl' (+) 0) (value `div` iterCount)
        iterCount

-- this is quadratic
{-# ANN scanl1'_Iterated (PermitPatternMatches
    [''Maybe,''(,),''Int,''Step,''Stream,''State]) #-}
{-# ANN scanl1'_Iterated (PermitConstructions
    [''Int,''Maybe,''Stream,''(,),''Step,''State,''(),''Bool]) #-}
{-# ANN scanl1'_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE scanl1'_Iterated #-}
scanl1'_Iterated :: Int -> Int -> Int -> IO ()
scanl1'_Iterated value iterCount =
    Common.drain . iterateSource (S.scanl1' (+)) (value `div` iterCount)
        iterCount

{-# ANN mapM_Iterated (PermitPatternMatches
    [''Int,''Step,''Stream,''State]) #-}
{-# ANN mapM_Iterated (PermitConstructions
    [''Int,''Stream,''State,''Maybe,''Step,''(),''Bool]) #-}
{-# ANN mapM_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE mapM_Iterated #-}
mapM_Iterated :: Int -> Int -> Int -> IO ()
mapM_Iterated value iterCount =
    Common.drain . iterateSource (S.mapM return) (value `div` iterCount)
        iterCount

{-# ANN filter_Even_Iterated (PermitPatternMatches
    [''Int,''Step,''Stream,''State]) #-}
{-# ANN filter_Even_Iterated (PermitConstructions
    [''Int,''Step,''Stream,''State,''Maybe,''(),''Bool]) #-}
{-# ANN filter_Even_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE filter_Even_Iterated #-}
filter_Even_Iterated :: Int -> Int -> Int -> IO ()
filter_Even_Iterated value iterCount =
    Common.drain . iterateSource (S.filter even) (value `div` iterCount)
        iterCount

{-# ANN take_All_Iterated (PermitPatternMatches
    [''(,),''Int,''Step,''State]) #-}
{-# ANN take_All_Iterated (PermitConstructions
    [''Int,''State,''Maybe,''Step,''(,),''(),''Bool]) #-}
{-# ANN take_All_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE take_All_Iterated #-}
take_All_Iterated :: Int -> Int -> Int -> IO ()
take_All_Iterated value iterCount =
    Common.drain . iterateSource (S.take (value + 1)) (value `div` iterCount)
        iterCount

{-# ANN drop_One_Iterated (PermitPatternMatches
    [''Maybe,''(,),''Bool,''Int,''Step,''Stream,''State]) #-}
{-# ANN drop_One_Iterated (PermitConstructions
    [''Int,''Integer,''Maybe,''Stream,''(,),''Step,''State
    ,''(),''Bool]) #-}
{-# ANN drop_One_Iterated (PermitTypeClasses [''Ord,''Num]) #-}
{-# NOINLINE drop_One_Iterated #-}
drop_One_Iterated :: Int -> Int -> Int -> IO ()
drop_One_Iterated value iterCount =
    Common.drain . iterateSource (S.drop 1) (value `div` iterCount) iterCount

{-# ANN dropWhile_True_Iterated (PermitPatternMatches
    [''Int,''Step,''DropWhileState,''State]) #-}
{-# ANN dropWhile_True_Iterated (PermitConstructions
    [''Int,''State,''Maybe,''Step,''DropWhileState,''()
    ,''Bool]) #-}
{-# ANN dropWhile_True_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE dropWhile_True_Iterated #-}
dropWhile_True_Iterated :: Int -> Int -> Int -> IO ()
dropWhile_True_Iterated value iterCount =
    Common.drain . iterateSource (S.dropWhile (<= (value + 1)))
        (value `div` iterCount) iterCount

_dropWhile_False_Iterated :: Int -> Int -> Int -> IO ()
_dropWhile_False_Iterated value iterCount =
    Common.drain . iterateSource (S.dropWhile (> (value + 1)))
        (value `div` iterCount) iterCount

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

{-# ANN ioAction_Iterated (PermitPatternMatches [''Int]) #-}
{-# ANN ioAction_Iterated (PermitConstructions [''Int]) #-}
{-# ANN ioAction_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE ioAction_Iterated #-}
ioAction_Iterated :: Int -> Int -> IO Int
ioAction_Iterated value i0 =
    iterateN (\i acc -> acc >>= \n -> return $ i + n) (return i0) value

{-# ANN submap_Iterated (PermitPatternMatches
    [''Bool,''Step,''Stream,''State]) #-}
{-# ANN submap_Iterated (PermitConstructions
    [''Int,''Step,''Stream,''State,''Maybe,''Bool]) #-}
{-# ANN submap_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE submap_Iterated #-}
submap_Iterated :: Int -> Int -> IO ()
submap_Iterated value = drain . iterateSingleton (<$) value

{-# ANN fmap_Iterated (PermitPatternMatches
    [''Int,''Step,''Stream,''State,''Bool]) #-}
{-# ANN fmap_Iterated (PermitConstructions
    [''Int,''Step,''Stream,''State,''Maybe,''Bool]) #-}
{-# ANN fmap_Iterated (PermitTypeClasses []) #-}
{-# NOINLINE fmap_Iterated #-}
fmap_Iterated :: Int -> Int -> IO ()
fmap_Iterated value = drain . iterateSingleton (fmap . (+)) value

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
-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- scanl-map and foldl-map are equivalent to the scan and fold in the foldl
    -- library. If scan/fold followed by a map is efficient enough we may not
    -- need monolithic implementations of these.
    [ (SpaceO_1, benchIO "scanl'_fmap_x1" $ scanl'_fmap_x1 size)
    , (SpaceO_1, benchIO "drop_fmap_x1" $ drop_fmap_x1 size)
    , (SpaceO_1, benchIO "drop_scanl'_x1" $ drop_scanl'_x1 size)
    , (SpaceO_1, benchIO "take_drop_x1" $ take_drop_x1 size)
    , (SpaceO_1, benchIO "take_scanl'_x1" $ take_scanl'_x1 size)
    , (SpaceO_1, benchIO "take_fmap_x1" $ take_fmap_x1 size)
    , (SpaceO_1, benchIO "filter_drop_x1" $ filter_drop_x1 size)
    , (SpaceO_1, benchIO "filter_take_x1" $ filter_take_x1 size)
    , (SpaceO_1, benchIO "filter_scanl'_x1" $ filter_scanl'_x1 size)
    , (SpaceO_1, benchIO "filter_fmap_x1" $ filter_fmap_x1 size)
    , (SpaceO_1, benchIO "foldl'_fmap" $ foldl'_fmap size)
    , (SpaceO_1, benchIO "foldl'_SumProduct" $ foldl'_SumProduct size)
    , (SpaceO_1, benchIO "scanl'_foldl'_SumProduct" $
          scanl'_foldl'_SumProduct size)
    , (SpaceO_1, benchIO "scanl'_fmap_x2" $ scanl'_fmap_x2 size)
    , (SpaceO_1, benchIO "drop_fmap_x2" $ drop_fmap_x2 size)
    , (SpaceO_1, benchIO "drop_scanl'_x2" $ drop_scanl'_x2 size)
    , (SpaceO_1, benchIO "take_drop_x2" $ take_drop_x2 size)
    , (SpaceO_1, benchIO "take_scanl'_x2" $ take_scanl'_x2 size)
    , (SpaceO_1, benchIO "take_fmap_x2" $ take_fmap_x2 size)
    , (SpaceO_1, benchIO "filter_drop_x2" $ filter_drop_x2 size)
    , (SpaceO_1, benchIO "filter_take_x2" $ filter_take_x2 size)
    , (SpaceO_1, benchIO "filter_scanl'_x2" $ filter_scanl'_x2 size)
    , (SpaceO_1, benchIO "filter_scanl1'_x2" $ filter_scanl1'_x2 size)
    , (SpaceO_1, benchIO "filter_fmap_x2" $ filter_fmap_x2 size)
    , (SpaceO_1, benchIO "scanl'_fmap_x4" $ scanl'_fmap_x4 size)
    , (SpaceO_1, benchIO "drop_fmap_x4" $ drop_fmap_x4 size)
    , (SpaceO_1, benchIO "drop_scanl'_x4" $ drop_scanl'_x4 size)
    , (SpaceO_1, benchIO "take_drop_x4" $ take_drop_x4 size)
    , (SpaceO_1, benchIO "take_scanl'_x4" $ take_scanl'_x4 size)
    , (SpaceO_1, benchIO "take_fmap_x4" $ take_fmap_x4 size)
    , (SpaceO_1, benchIO "filter_drop_x4" $ filter_drop_x4 size)
    , (SpaceO_1, benchIO "filter_take_x4" $ filter_take_x4 size)
    , (SpaceO_1, benchIO "filter_scanl'_x4" $ filter_scanl'_x4 size)
    , (SpaceO_1, benchIO "filter_scanl1'_x4" $ filter_scanl1'_x4 size)
    , (SpaceO_1, benchIO "filter_fmap_x4" $ filter_fmap_x4 size)

    , (StackO_n, benchIO "mapM_Iterated" $ mapM_Iterated size 10)
    , (StackO_n, benchIO "scanl'_Iterated" $ scanl'_Iterated size 100)
    , (StackO_n, benchIO "scanl1'_Iterated" $ scanl1'_Iterated size 10)
    , (StackO_n, benchIO "filter_Even_Iterated" $ filter_Even_Iterated size 10)
    , (StackO_n, benchIO "take_All_Iterated" $ take_All_Iterated size 10)
    , (StackO_n, benchIO "drop_One_Iterated" $ drop_One_Iterated size 10)
    , (StackO_n, benchIO "dropWhile_True_Iterated" $
          dropWhile_True_Iterated size 10)
    -- XXX tasty-bench hangs on this sometimes
    -- , (StackO_n, benchIO "dropWhile_False_Iterated" $
    --       _dropWhile_False_Iterated size 10)
    , (SpaceO_n, benchIO "ioAction_Iterated (plain IO for baseline)" $
          ioAction_Iterated size)
    , (SpaceO_n, benchIO "submap_Iterated (<$)" $ submap_Iterated size)
    , (SpaceO_n, benchIO "fmap_Iterated" $ fmap_Iterated size)
    {-
    , benchIOSrc fromSerial "_(<$) (n times)" $
        _iterateSingleton (<$) value
    , benchIOSrc fromSerial "_fmap (n times)" $
        _iterateSingleton (fmap . (+)) value
    -}
    , (SpaceO_n, benchIO "naivePrimeSieve" $ naivePrimeSieve size)
    ]
