{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Common
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low level functions using StreamK as the intermediate stream type. These
-- functions are used in other stream modules to implement their instances.
--
module Streamly.Internal.Data.Stream.Common
    (
    -- * Conversion operations
      fromList
    , toList

    -- * Fold operations
    , foldr
    , foldl'
    , fold

    -- * Zip style operations
    , eqBy
    , cmpBy
    )
where

#include "inline.hs"

import Streamly.Data.Fold (Fold)

import qualified Streamly.Data.StreamK as K
import qualified Streamly.Internal.Data.Stream as D

import Prelude hiding (Foldable(..), repeat)

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromList = 'Prelude.foldr' 'K.cons' 'K.nil'
-- @
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'K.fromFoldable' for serial streams.
--
{-# INLINE_EARLY fromList #-}
fromList :: Monad m => [a] -> K.StreamK m a
fromList = D.toStreamK . D.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. D.toStreamK (D.fromList a) = K.fromFoldable a #-}

-- | Convert a stream into a list in the underlying monad.
--
{-# INLINE toList #-}
toList :: Monad m => K.StreamK m a -> m [a]
toList m = D.toList $ D.fromStreamK m

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> K.StreamK m a -> m b
foldrM step acc m = D.foldrM step acc $ D.fromStreamK m

{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> K.StreamK m a -> m b
foldr f z = foldrM (\a b -> f a <$> b) (return z)

-- | Strict left associative fold.
--
{-# INLINE foldl' #-}
foldl' ::
    Monad m => (b -> a -> b) -> b -> K.StreamK m a -> m b
foldl' step begin m = D.foldl' step begin $ D.fromStreamK m


{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> K.StreamK m a -> m b
fold fld m = D.fold fld $ D.fromStreamK m

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
{-# INLINE eqBy #-}
eqBy :: Monad m =>
    (a -> b -> Bool) -> K.StreamK m a -> K.StreamK m b -> m Bool
eqBy f m1 m2 = D.eqBy f (D.fromStreamK m1) (D.fromStreamK m2)

-- | Compare two streams
--
{-# INLINE cmpBy #-}
cmpBy
    :: Monad m
    => (a -> b -> Ordering) -> K.StreamK m a -> K.StreamK m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (D.fromStreamK m1) (D.fromStreamK m2)
