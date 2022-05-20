-- |
-- Module      : Streamly.Internal.Data.Stream.Serial.Transform
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Serial.Transform
(
  filter
, foldFilter
, map
, mapM
)
where

import Streamly.Internal.Data.Fold.Type (Fold)

import qualified Streamly.Internal.Data.Stream.StreamD.Transform as D
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

import Prelude hiding (map, mapM, repeat, filter)

import Streamly.Internal.Data.Stream.Serial.Type

#include "Instances.hs"
#include "inline.hs"

--
-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Data.Function ((&))
-- >>> import Streamly.Prelude ((|$))
-- >>> import Prelude hiding ( filter, drop, dropWhile, take, takeWhile, foldr, map, mapM, sequence, reverse, foldr1 , scanl, scanl1)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
--
-- >>> hSetBuffering stdout LineBuffering

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

-- |
-- @
-- mapM f = sequence . map f
-- @
--
-- Apply a monadic function to each element of the stream and replace it with
-- the output of the resulting action.
--
-- @
-- >>> drain $ Stream.mapM putStr $ Stream.fromList ["a", "b", "c"]
-- abc
--
-- >>> :{
--    drain $ Stream.replicateM 10 (return 1)
--      & (fromSerial . Stream.mapM (\x -> threadDelay 1000000 >> print x))
-- :}
-- 1
-- ...
-- 1
--
-- > drain $ Stream.replicateM 10 (return 1)
--  & (fromAsync . Stream.mapM (\x -> threadDelay 1000000 >> print x))
-- @
--
-- /Concurrent (do not use with 'fromParallel' on infinite streams)/
--
-- /Pre-release/
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> SerialT m a -> SerialT m b
mapM f (SerialT m) = SerialT $ D.toStreamK $ D.mapM f $ D.fromStreamK m

-- |
-- @
-- map = fmap
-- @
--
-- Same as 'fmap'.
--
-- @
-- > S.toList $ S.map (+1) $ S.fromList [1,2,3]
-- [2,3,4]
-- @
--
-- /Pre-release/
--
{-# INLINE map #-}
map :: Monad m => (a -> b) -> SerialT m a -> SerialT m b
map f = mapM (return . f)

-- | Include only those elements that pass a predicate.
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> SerialT m a -> SerialT m a
filter p = fromStreamD . D.filter p . toStreamD

-- | Use a filtering fold on a stream.
--
-- > Stream.sum $ Stream.foldFilter (Fold.satisfy (> 5)) $ Stream.fromList [1..10]
-- 40
--
-- /Pre-release/
--
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> SerialT m a -> SerialT m b
foldFilter p = fromStreamD . D.foldFilter p . toStreamD
