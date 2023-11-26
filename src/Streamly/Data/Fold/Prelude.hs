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
    , parEval

    -- * Container Related
    , toHashMapIO
    )
where

import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Streamly.Data.Fold
import Streamly.Internal.Data.Fold (toContainerIO)
import Streamly.Internal.Data.Fold.Concurrent
import Streamly.Internal.Data.IsMap.HashMap ()

-- | Split the input stream based on a hashable component of the key field and
-- fold each split using the given fold. Useful for map/reduce, bucketizing
-- the input in different bins or for generating histograms.
--
-- >>> import Data.HashMap.Strict (HashMap, fromList)
-- >>> import qualified Streamly.Data.Fold.Prelude as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
--
-- Consider a stream of key value pairs:
--
-- >>> input = Stream.fromList [("k1",1),("k1",1.1),("k2",2), ("k2",2.2)]
--
-- Classify each key to a different hash bin and fold the bins:
--
-- >>> classify = Fold.toHashMapIO fst (Fold.lmap snd Fold.toList)
-- >>> Stream.fold classify input :: IO (HashMap String [Double])
-- fromList [("k2",[2.0,2.2]),("k1",[1.0,1.1])]
--
-- /Pre-release/
--
{-# INLINE toHashMapIO #-}
toHashMapIO :: (MonadIO m, Hashable k, Ord k) =>
    (a -> k) -> Fold m a b -> Fold m a (HashMap k b)
toHashMapIO = toContainerIO
