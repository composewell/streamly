{-# LANGUAGE UndecidableInstances #-}
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

import Streamly.Internal.Data.Fold.Type (Fold (..))

#ifdef USE_STREAMK_ONLY
import qualified Streamly.Internal.Data.Stream.StreamK as S
import qualified Streamly.Internal.Data.StreamK.Type as S
#else
import qualified Streamly.Internal.Data.StreamD.Type as S
#endif

import qualified Streamly.Internal.Data.StreamK.Type as K
import qualified Streamly.Internal.Data.StreamD.Type as D

import Prelude hiding (foldr, repeat)

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
fromList :: Monad m => [a] -> K.Stream m a
fromList = S.toStreamK . S.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. S.toStreamK (S.fromList a) = K.fromFoldable a #-}

-- | Convert a stream into a list in the underlying monad.
--
{-# INLINE toList #-}
toList :: Monad m => K.Stream m a -> m [a]
toList m = S.toList $ S.fromStreamK m

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> K.Stream m a -> m b
foldrM step acc m = S.foldrM step acc $ S.fromStreamK m

{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> K.Stream m a -> m b
foldr f z = foldrM (\a b -> f a <$> b) (return z)

-- | Strict left associative fold.
--
{-# INLINE foldl' #-}
foldl' ::
    Monad m => (b -> a -> b) -> b -> K.Stream m a -> m b
foldl' step begin m = S.foldl' step begin $ S.fromStreamK m


{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> K.Stream m a -> m b
fold fld m = S.fold fld $ S.fromStreamK m

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
{-# INLINE eqBy #-}
eqBy :: Monad m =>
    (a -> b -> Bool) -> K.Stream m a -> K.Stream m b -> m Bool
eqBy f m1 m2 = D.eqBy f (D.fromStreamK m1) (D.fromStreamK m2)

-- | Compare two streams
--
{-# INLINE cmpBy #-}
cmpBy
    :: Monad m
    => (a -> b -> Ordering) -> K.Stream m a -> K.Stream m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (D.fromStreamK m1) (D.fromStreamK m2)
