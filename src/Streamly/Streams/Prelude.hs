{-# LANGUAGE CPP                       #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

#include "inline.hs"

-- |
-- Module      : Streamly.Streams.Prelude
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.Prelude
    (
    -- * Stream Conversion
      fromStreamS
    , toStreamS

    -- * Conversion operations
    , fromList
    , toList

    -- * Fold operations
    , foldrM
    , foldr
    , foldl'

    -- * Zip style operations
    , eqBy
    , cmpBy

    -- * Fold Utilities
    , foldWith
    , foldMapWith
    , forEachWith
    )
where

import Prelude hiding (foldr)
import qualified Prelude

#ifdef USE_STREAMK_ONLY
import qualified Streamly.Streams.StreamK as S
#else
import qualified Streamly.Streams.StreamD as S
#endif

import Streamly.Streams.StreamK (IsStream(..))
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.StreamD as D

------------------------------------------------------------------------------
-- Conversion to and from direct style stream
------------------------------------------------------------------------------

-- These definitions are dependent on what is imported as S
{-# INLINE fromStreamS #-}
fromStreamS :: (IsStream t, Monad m) => S.Stream m a -> t m a
fromStreamS = fromStream . S.toStreamK

{-# INLINE toStreamS #-}
toStreamS :: (IsStream t, Monad m) => t m a -> S.Stream m a
toStreamS = S.fromStreamK . toStream

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
fromList :: (Monad m, IsStream t) => [a] -> t m a
fromList = fromStreamS . S.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. S.toStreamK (S.fromList a) = K.fromFoldable a #-}

-- | Convert a stream into a list in the underlying monad.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: (Monad m, IsStream t) => t m a -> m [a]
toList m = S.toList $ toStreamS m

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM :: (Monad m, IsStream t) => (a -> b -> m b) -> b -> t m a -> m b
foldrM step acc m = S.foldrM step acc $ toStreamS m

{-# INLINE foldr #-}
foldr :: (Monad m, IsStream t) => (a -> b -> b) -> b -> t m a -> m b
foldr f = foldrM (\a b -> return (f a b))

-- | Strict left associative fold.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' :: (Monad m, IsStream t) => (b -> a -> b) -> b -> t m a -> m b
foldl' step begin m = S.foldl' step begin $ toStreamS m

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
-- @since 0.5.3
{-# INLINE eqBy #-}
eqBy :: (IsStream t, Monad m) => (a -> b -> Bool) -> t m a -> t m b -> m Bool
eqBy f m1 m2 = D.eqBy f (D.toStreamD m1) (D.toStreamD m2)

-- | Compare two streams
--
-- @since 0.5.3
{-# INLINE cmpBy #-}
cmpBy
    :: (IsStream t, Monad m)
    => (a -> b -> Ordering) -> t m a -> t m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (D.toStreamD m1) (D.toStreamD m2)

------------------------------------------------------------------------------
-- Fold Utilities
------------------------------------------------------------------------------

-- | A variant of 'Data.Foldable.fold' that allows you to fold a 'Foldable'
-- container of streams using the specified stream sum operation.
--
-- @foldWith 'async' $ map return [1..3]@
--
-- @since 0.1.0
{-# INLINABLE foldWith #-}
foldWith :: (IsStream t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
foldWith f = Prelude.foldr f K.nil

-- | A variant of 'foldMap' that allows you to map a monadic streaming action
-- on a 'Foldable' container and then fold it using the specified stream sum
-- operation.
--
-- @foldMapWith 'async' return [1..3]@
--
-- @since 0.1.0
{-# INLINABLE foldMapWith #-}
foldMapWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
foldMapWith f g = Prelude.foldr (f . g) K.nil

-- | Like 'foldMapWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
--
-- @since 0.1.0
{-# INLINABLE forEachWith #-}
forEachWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
forEachWith f xs g = Prelude.foldr (f . g) K.nil xs
