-- |
-- Module      : Array.Type
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Array.Type
    (
      typeCommonBenchmarks
    , benchIO
    , withRandomIntIO
    , withArray
    , withStream
    ) where

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Array as A

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)
import qualified Stream.Common as P

import Prelude as P

#if __GLASGOW_HASKELL__ >= 810
type Arr :: Type -> Type
#endif
type Arr = A.Array

instance NFData (A.Array a) where
    {-# INLINE rnf #-}
    rnf _ = ()

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: Int -> IO (Arr Int)
sourceIntFromToFromList value = withRandomIntIO $ \n ->
    P.return $ A.fromListN value [n..n + value]

#include "Streamly/Benchmark/Data/Array/TypeCommon.hs"
