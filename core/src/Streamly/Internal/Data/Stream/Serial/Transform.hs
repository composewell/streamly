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
-- @since 0.4.0
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
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> SerialT m a -> SerialT m b
foldFilter p = fromStreamD . D.foldFilter p . toStreamD
