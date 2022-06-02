-- |
-- Module      : Streamly.Internal.Data.Stream.Transform
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Transform
    (
      filter
    , foldFilter
    , map
    , mapM
    )
where

import Streamly.Internal.Data.Fold.Type (Fold)
import Streamly.Internal.Data.Stream.Type (Stream)

import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Transform as StreamD

import Prelude hiding (map, mapM, filter)

--
-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Transformation by Mapping
------------------------------------------------------------------------------

-- |
-- >> mapM f = Stream.sequence . Stream.map f
--
-- Apply a monadic function to each element of the stream and replace it with
-- the output of the resulting action.
--
-- >>> s = Stream.unfold Unfold.fromList ["a", "b", "c"]
-- >>> Stream.fold Fold.drain $ Stream.mapM putStr s
-- abc
--
-- /Pre-release/
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f = Stream.fromStreamD . StreamD.mapM f . Stream.toStreamD

-- |
-- >>> map = fmap
--
-- Same as 'fmap'.
--
-- >>> s = Stream.unfold Unfold.fromList [1,2,3]
-- >>> Stream.fold Fold.toList $ Stream.map (+1) s
-- [2,3,4]
--
-- /Pre-release/
--
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f = mapM (return . f)

-- | Include only those elements that pass a predicate.
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter p = Stream.fromStreamD . StreamD.filter p . Stream.toStreamD

-- | Use a filtering fold on a stream.
--
-- >>> :{
--    Stream.fold Fold.sum
--  $ Stream.foldFilter (Fold.maybe (\x -> if x > 5 then Just x else Nothing))
--  $ Stream.unfold Unfold.fromList [1..10]
--  :}
--40
--
-- /Pre-release/
--
{-# INLINE foldFilter #-}
foldFilter :: Monad m => Fold m a (Maybe b) -> Stream m a -> Stream m b
foldFilter p = Stream.fromStreamD . StreamD.foldFilter p . Stream.toStreamD
