{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Concurrent
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
--
module Streamly.Internal.Data.Stream.Zip.Concurrent
    (
      ZipConcurrent (..)
    )
where

import Streamly.Internal.Data.Stream.StreamD (Stream)
import Streamly.Internal.Data.Stream.Concurrent (MonadAsync, parZipWith)

import qualified Streamly.Internal.Data.Stream.StreamD as Stream (repeat)
import Prelude hiding (map, repeat, zipWith)

-- $setup
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
--

newtype ZipConcurrent m a = ZipConcurrent {getZipConcurrent :: Stream m a}
      deriving (Functor)

-- | An IO stream whose applicative instance zips streams concurrently. Note
-- that it uses the default concurrency options.
--
-- >>> s = ZipConcurrent $ Stream.fromList [1, 2, 3]
-- >>> x = (,,) <$> s <*> s <*> s
-- >>> Stream.fold Fold.toList (getZipConcurrent x)
-- [(1,1,1),(2,2,2),(3,3,3)]
--
-- @since 0.9.0

instance MonadAsync m => Applicative (ZipConcurrent m) where
    pure = ZipConcurrent . Stream.repeat

    {-# INLINE (<*>) #-}
    ZipConcurrent m1 <*> ZipConcurrent m2 =
        ZipConcurrent $ parZipWith id id m1 m2
