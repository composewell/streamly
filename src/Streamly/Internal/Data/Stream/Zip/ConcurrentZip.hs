
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.ConcurrentZip
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Prelude as Stream
--
module Streamly.Internal.Data.Stream.Zip.ConcurrentZip
    (
      ConcurrentZipM (..)
    , ConcurrentZip
    , consMConcurrentZip
    )
where

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Concurrent (MonadAsync, zipWith)

import qualified Streamly.Internal.Data.Stream as Stream (consM, repeat)

import Prelude hiding (map, repeat, zipWith, errorWithoutStackTrace)

#include "Instances.hs"

-- $setup
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import Control.Concurrent (threadDelay)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

newtype ConcurrentZipM m a = ConcurrentZipM {getStream :: Stream m a}
        deriving (Semigroup, Monoid)

-- | An IO stream whose applicative instance zips streams concurrently.
--
-- >>> s = ConcurrentZipM $ Stream.fromList [1, 2, 3]
-- >>> x = (,,) <$> s <*> s <*> s
-- >>> Stream.fold Fold.toList (getStream x)
-- [(1,1,1),(2,2,2),(3,3,3)]
--
-- @since 0.9.0
type ConcurrentZip = ConcurrentZipM IO

consMConcurrentZip :: Monad m =>
    m a -> ConcurrentZipM m a -> ConcurrentZipM m a
consMConcurrentZip m (ConcurrentZipM r) = ConcurrentZipM $ Stream.consM m r

instance Monad m => Functor (ConcurrentZipM m) where
    {-# INLINE fmap #-}
    fmap f (ConcurrentZipM m) =
        ConcurrentZipM $ fmap f m

instance MonadAsync m => Applicative (ConcurrentZipM m) where
    pure = ConcurrentZipM . Stream.repeat

    {-# INLINE (<*>) #-}
    ConcurrentZipM m1 <*> ConcurrentZipM m2 = ConcurrentZipM $ zipWith id m1 m2
