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

-- Imported for the types named in the fusion-plugin annotations in the
-- included Array/TypeCommon.hs.
import qualified Streamly.Internal.Data.MutArray as MutArray
import Streamly.Internal.Data.MutByteArray (PinnedState)
import GHC.Classes (IP)
import GHC.Stack (CallStack, SrcLoc)
import Streamly.Data.MutByteArray (Unbox)
import Unsafe.Coerce (UnsafeEquality)

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)
import Fusion.Plugin.Types
import qualified Stream.Common as P

import Prelude as P

#if __GLASGOW_HASKELL__ >= 810
type Arr :: Type -> Type
#endif
type Arr = A.Array

instance NFData (A.Array a) where
    {-# INLINE rnf #-}
    rnf _ = ()

{-# ANN fromListN (PermitPatternMatches [''[], ''Int, ''IO]) #-}
{-# ANN fromListN (PermitConstructions [''A.Array, ''[], ''Int]) #-}
{-# ANN fromListN (PermitTypeClasses []) #-}
{-# NOINLINE fromListN #-}
fromListN :: Int -> Int -> IO (Arr Int)
fromListN value n =
    P.return $ A.fromListN value [n..n + value]

-- Selects the unboxed-array variant of the fusion-plugin annotations in the
-- shared include below.
#define ARRAY_UNBOXED
#include "Streamly/Benchmark/Data/Array/TypeCommon.hs"
