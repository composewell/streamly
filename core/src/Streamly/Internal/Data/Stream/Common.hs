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
    -- * Running Effects
      drain

    -- * Conversion operations
    , fromList
    , toList

    -- * Fold operations
    , foldrM
    , foldrMx
    , foldr

    , foldlx'
    , foldlMx'
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
import qualified Streamly.Internal.Data.Stream.StreamK.Type as S
#else
import qualified Streamly.Internal.Data.Stream.StreamD.Type as S
#endif

import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

import Prelude hiding (foldr, repeat)

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

{-# INLINE_EARLY drain #-}
drain :: Monad m => K.Stream m a -> m ()
drain m = D.drain $ D.fromStreamK m
{-# RULES "drain fallback to CPS" [1]
    forall a. D.drain (D.fromStreamK a) = K.drain a #-}

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
-- @since 0.4.0
{-# INLINE_EARLY fromList #-}
fromList :: Monad m => [a] -> K.Stream m a
fromList = S.toStreamK . S.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. S.toStreamK (S.fromList a) = K.fromFoldable a #-}

-- | Convert a stream into a list in the underlying monad.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: Monad m => K.Stream m a -> m [a]
toList m = S.toList $ S.fromStreamK m

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> K.Stream m a -> m b
foldrM step acc m = S.foldrM step acc $ S.fromStreamK m

{-# INLINE foldrMx #-}
foldrMx :: Monad m
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> K.Stream m a -> m b
foldrMx step final project m = D.foldrMx step final project $ D.fromStreamK m

{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> K.Stream m a -> m b
foldr f z = foldrM (\a b -> f a <$> b) (return z)

-- | Like 'foldlx'', but with a monadic step function.
--
-- @since 0.7.0
{-# INLINE foldlMx' #-}
foldlMx' ::
    Monad m
    => (x -> a -> m x) -> m x -> (x -> m b) -> K.Stream m a -> m b
foldlMx' step begin done m = S.foldlMx' step begin done $ S.fromStreamK m

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.7.0
{-# INLINE foldlx' #-}
foldlx' ::
    Monad m => (x -> a -> x) -> x -> (x -> b) -> K.Stream m a -> m b
foldlx' step begin done m = S.foldlx' step begin done $ S.fromStreamK m

-- | Strict left associative fold.
--
-- @since 0.2.0
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
-- @since 0.5.3
{-# INLINE eqBy #-}
eqBy :: Monad m =>
    (a -> b -> Bool) -> K.Stream m a -> K.Stream m b -> m Bool
eqBy f m1 m2 = D.eqBy f (D.fromStreamK m1) (D.fromStreamK m2)

-- | Compare two streams
--
-- @since 0.5.3
{-# INLINE cmpBy #-}
cmpBy
    :: Monad m
    => (a -> b -> Ordering) -> K.Stream m a -> K.Stream m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (D.fromStreamK m1) (D.fromStreamK m2)
