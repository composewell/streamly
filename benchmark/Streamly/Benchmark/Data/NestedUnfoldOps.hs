-- |
-- Module      : NestedUnfoldOps
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com

module Streamly.Benchmark.Data.NestedUnfoldOps where

import Control.Monad.IO.Class (MonadIO (..))
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Fold as FL

-- n * (n + 1) / 2 == linearCount
concatCount :: Int -> Int
concatCount linearCount =
    round (((1 + 8 * fromIntegral linearCount)**(1/2::Double) - 1) / 2)

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-- generate numbers up to the argument value
{-# INLINE source #-}
source :: Monad m => Int -> Unfold m Int Int
source n = UF.enumerateFromToIntegral n

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

{-# INLINE concat #-}
concat :: MonadIO m => Int -> Int -> m ()
concat linearCount start = do
    let end = start + concatCount linearCount
    UF.fold
        (UF.concat (source end) (source end))
        FL.drain start
