{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Data.Fold.Prelude
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- All Fold related combinators including the streamly-core
-- "Streamly.Data.Fold" module, concurrency, unordered container operations.
--
module Streamly.Data.Fold.Prelude
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * "Streamly.Data.Fold"
    -- | All "Streamly.Data.Fold" combinators are re-exported via this
    -- module. For more pre-release combinators also see
    -- "Streamly.Internal.Data.Fold" module.
      module Streamly.Data.Fold
    -- * Concurrent Operations
    -- ** Configuration
    , Config
    , maxBuffer
    , boundThreads
    , inspect

    -- ** Combinators
    , parBuffered
    , parTee
    , parDistribute
    , parDistributeScan
    , parPartition
    , parDemuxScan
    , parUnzip

    -- * Container Related
    , toHashMapIO

    -- ** Deprecated
    , parEval
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Streamly.Data.Fold
import Streamly.Internal.Data.Fold (toContainerIO)
import Streamly.Internal.Data.Fold.Prelude
import Streamly.Internal.Data.IsMap.HashMap ()

#include "DocTestDataFold.hs"

-- | Split the input stream based on a hashable component of the key field and
-- fold each split using the given fold. Useful for map/reduce, bucketizing
-- the input in different bins or for generating histograms.
--
-- Consider a stream of key value pairs:
--
-- >>> input = Stream.fromList [("k1",1),("k1",1.1),("k2",2), ("k2",2.2)]
--
-- Classify each key to a different hash bin and fold the bins:
--
-- >>> classify = Fold.toHashMapIO fst (Fold.lmap snd Fold.toList)
-- >>> sortOn fst . HM.toList <$> Stream.fold classify input :: IO [(String, [Double])]
-- [("k1",[1.0,1.1]),("k2",[2.0,2.2])]
--
-- /Pre-release/
--
{-# INLINE toHashMapIO #-}
toHashMapIO :: (MonadIO m, Hashable k
#if __GLASGOW_HASKELL__ == 810
    , Eq k
#endif
    ) =>
    (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
toHashMapIO = toContainerIO
